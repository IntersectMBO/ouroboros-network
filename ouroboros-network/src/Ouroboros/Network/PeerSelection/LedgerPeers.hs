{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Ouroboros.Network.PeerSelection.LedgerPeers
  ( DomainAccessPoint (..)
  , IP.IP (..)
  , LedgerPeers (..)
  , getLedgerPeers
  , RelayAccessPoint (..)
  , PoolStake (..)
  , AccPoolStake (..)
  , TraceLedgerPeers (..)
  , NumberOfPeers (..)
  , LedgerPeersKind (..)
  , StakeMapOverSource (..)
    -- * Ledger Peers specific functions
  , accPoolStake
  , accBigPoolStakeMap
  , bigLedgerPeerQuota
  , stakeMapWithSlotOverSource
    -- * DNS based provider for ledger root peers
  , WithLedgerPeersArgs (..)
  , withLedgerPeers
    -- Re-exports for testing purposes
  , module Ouroboros.Network.PeerSelection.LedgerPeers.Type
  , module Ouroboros.Network.PeerSelection.LedgerPeers.Common
    -- * Internal only exported for testing purposes
  , resolveLedgerPeers
  ) where

import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer, traceWith)
import Data.IP qualified as IP
import Data.List as List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio
import System.Random

import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Data.Word (Word16, Word64)
import Network.DNS qualified as DNS
import Ouroboros.Network.PeerSelection.LedgerPeers.Common
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.LedgerPeers.Utils (accBigPoolStake,
           bigLedgerPeerQuota, reRelativeStake)
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers
           (resolveLedgerPeers)

-- | Internal API to deal with 'UseLedgerAfter' configuration
-- option
--
-- Receiving the 'LedgerPeersConsensusInterface' we are able to compute a
-- function that given a 'SlotNo' will give us 'LedgerPeers' according to the
-- following invariants:
--
-- * 'BeforeSlot' is returned iff the latest slot is before the 'slotNo';
-- * 'LedgerPeers lsj peers' is returned iff the latest slot is after the
--   'slotNo'.
--
getLedgerPeers
  :: MonadSTM m
  => LedgerPeersConsensusInterface m
  -> AfterSlot
  -> STM m LedgerPeers
getLedgerPeers (LedgerPeersConsensusInterface lpGetLatestSlot
                                              lpGetLedgerStateJudgement
                                              lpGetLedgerPeers)
               ulp = do
  wOrigin <- lpGetLatestSlot
  case (wOrigin, ulp) of
    (_         , Always) -> ledgerPeers
    (At curSlot, After slot)
      | curSlot >= slot -> ledgerPeers
    _ -> pure BeforeSlot
  where
    ledgerPeers = LedgerPeers
              <$> lpGetLedgerStateJudgement
              <*> lpGetLedgerPeers

-- | Convert a list of pools with stake to a Map keyed on the accumulated stake.
-- Consensus provides a list of pairs of relative stake and corresponding relays for all usable
-- registered pools.
-- By creating a Map keyed on the `AccPoolStake` that is the sum of the pool's relative stake and
-- the stake of all preceding pools we can support weighted random selection in
-- O(log n) time by taking advantage of Map.lookupGE (returns the smallest key greater or equal
-- to the provided value).
--
accPoolStake :: [(PoolStake, NonEmpty RelayAccessPoint)]
             -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
accPoolStake =
      Map.fromList
    . List.foldl' fn []
    . reRelativeStake AllLedgerPeers
  where
    fn :: [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))]
       -> (PoolStake, NonEmpty RelayAccessPoint)
       -> [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))]
    fn [] (s, rs) =
        [(AccPoolStake (unPoolStake s), (s, rs))]
    fn ps (s, !rs) =
        let accst = AccPoolStake (unPoolStake s)
            as = fst $ head ps
            !acc = as + accst in
        (acc, (s, rs)) : ps

-- | Take the result of 'accBigPoolStake' and turn it into
--
accBigPoolStakeMap :: [(PoolStake, NonEmpty RelayAccessPoint)]
                   -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
accBigPoolStakeMap = Map.fromAscList      -- the input list is ordered by `AccPoolStake`, thus we
                                          -- can use `fromAscList`
                     . accBigPoolStake

-- | Try to pick n random peers using stake distribution.
--
pickPeers :: forall m. Monad m
          => StdGen
          -> Tracer m TraceLedgerPeers
          -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
          -- ^ all ledger peers
          -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
          -- ^ big ledger peers
          -> NumberOfPeers
          -> LedgerPeersKind
          -> m (StdGen, [RelayAccessPoint])

pickPeers inRng _ pools _bigPools _ _ | Map.null pools = return (inRng, [])

-- pick big ledger peers using ledger stake distribution
pickPeers inRng tracer _pools bigPools (NumberOfPeers cnt) BigLedgerPeers =
    go inRng cnt []
  where
    go :: StdGen -> Word16 -> [RelayAccessPoint] -> m (StdGen, [RelayAccessPoint])
    go rng 0 picked = return (rng, picked)
    go rng n picked =
        let (r :: Word64, rng') = random rng
            d = maxBound :: Word64
            -- x is the random accumulated stake capped by `bigLedgerPeerQuota`.
            -- We use it to select random big ledger peer according to their
            -- stake distribution.
            x = fromIntegral r % fromIntegral d * (unAccPoolStake bigLedgerPeerQuota)
        in case Map.lookupGE (AccPoolStake x) bigPools of
             -- XXX We failed pick a peer. Shouldn't this be an error?
             Nothing -> go rng' (n - 1) picked
             Just (ackStake, (stake, relays)) -> do
                 let (ix, rng'') = randomR (0, NonEmpty.length relays - 1) rng'
                     relay = relays NonEmpty.!! ix
                 traceWith tracer $ PickedBigLedgerPeer relay ackStake stake
                 go rng'' (n - 1) (relay : picked)

-- pick ledger peers (not necessarily big ones) using square root of the stake
-- distribution
pickPeers inRng tracer pools _bigPools (NumberOfPeers cnt) AllLedgerPeers = go inRng cnt []
  where
    go :: StdGen -> Word16 -> [RelayAccessPoint] -> m (StdGen, [RelayAccessPoint])
    go rng 0 picked = return (rng, picked)
    go rng n picked =
        let (r :: Word64, rng') = random rng
            d = maxBound :: Word64
            x = fromIntegral r % fromIntegral d in
        case Map.lookupGE (AccPoolStake x) pools of
             -- XXX We failed pick a peer. Shouldn't this be an error?
             Nothing -> go rng' (n - 1) picked
             Just (ackStake, (stake, relays)) -> do
                 let (ix, rng'') = randomR (0, NonEmpty.length relays - 1) rng'
                     relay = relays NonEmpty.!! ix
                 traceWith tracer $ PickedLedgerPeer relay ackStake stake
                 go rng'' (n - 1) (relay : picked)


-- | Peer list life time decides how often previous ledger peers should be
-- reused.  If the ledger peer map is empty we use 'short_PEER_LIST_LIFE_TIME'
-- otherwise we use 'long_PEER_LIST_LIFE_TIME'
--
short_PEER_LIST_LIFE_TIME :: DiffTime
short_PEER_LIST_LIFE_TIME = 30

-- | Long peer list lift time, close to 30minutes but not exactly
--
long_PEER_LIST_LIFE_TIME :: DiffTime
long_PEER_LIST_LIFE_TIME = 1847 -- a prime number!

-- | Run the LedgerPeers worker thread.
--
ledgerPeersThread :: forall m peerAddr resolver exception.
                     ( MonadAsync m
                     , MonadMonotonicTime m
                     , MonadThrow m
                     , Exception exception
                     , Ord peerAddr
                     )
                  => PeerActionsDNS peerAddr resolver exception m
                  -> WithLedgerPeersArgs m
                  -- blocking request for ledger peers
                  -> STM m (NumberOfPeers, LedgerPeersKind)
                  -- response with ledger peers
                  -> (Maybe (Set peerAddr, DiffTime) -> STM m ())
                  -> m Void
ledgerPeersThread PeerActionsDNS {
                    paToPeerAddr,
                    paDnsActions,
                    paDnsSemaphore }
                  WithLedgerPeersArgs {
                    wlpRng,
                    wlpConsensusInterface,
                    wlpTracer,
                    wlpGetUseLedgerPeers,
                    wlpGetLedgerPeerSnapshot }
                  getReq
                  putResp = do
    go wlpRng (Time 0) Map.empty Map.empty 0
  where
    go :: StdGen
       -> Time
       -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
       -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
       -> SlotNo
       -> m Void
    go rng oldTs peerMap bigPeerMap lastSnapshotSlot = do
        traceWith wlpTracer WaitingOnRequest
        -- wait until next request of ledger peers
        ((numRequested, ledgerPeersKind), useLedgerPeers) <- atomically $
          (,) <$> getReq <*> wlpGetUseLedgerPeers
        traceWith wlpTracer (TraceUseLedgerPeers useLedgerPeers)

        let peerListLifeTime = if Map.null peerMap && isLedgerPeersEnabled useLedgerPeers
                                  then short_PEER_LIST_LIFE_TIME
                                  else long_PEER_LIST_LIFE_TIME

        traceWith wlpTracer $ RequestForPeers numRequested
        !now <- getMonotonicTime
        let age = diffTime now oldTs
        (peerMap', bigPeerMap', lastSnapshotSlot', ts) <-
          if age > peerListLifeTime
             then case useLedgerPeers of
               DontUseLedgerPeers -> do
                 traceWith wlpTracer DisabledLedgerPeers
                 return (Map.empty, Map.empty, 0, now)
               UseLedgerPeers ula -> do
                 (ledgerSlotNo, ledgerPools, peerSnapshot) <-
                   atomically ((,,) <$> lpGetLatestSlot wlpConsensusInterface
                                    <*> getLedgerPeers wlpConsensusInterface ula
                                    <*> wlpGetLedgerPeerSnapshot)

                 let (peersStakeMap, bigPeersStakeMap, lastSnapshotSlot'') =
                       stakeMapWithSlotOverSource StakeMapOverSource {
                                                    ledgerSlotNo, ledgerPools, peerSnapshot,
                                                    lastSnapshotSlot, peerMap, bigPeerMap, ula}
                 when (lastSnapshotSlot'' /= 0) $ traceWith wlpTracer UsingBigLedgerPeerSnapshot

                 traceWith wlpTracer $ FetchingNewLedgerState (Map.size peersStakeMap) (Map.size bigPeersStakeMap)
                 return (peersStakeMap, bigPeersStakeMap, lastSnapshotSlot'', now)
             else do
               traceWith wlpTracer $ ReusingLedgerState (Map.size peerMap) age
               return (peerMap, bigPeerMap, lastSnapshotSlot, oldTs)

        if all Map.null [peerMap', bigPeerMap']
           then do
               when (isLedgerPeersEnabled useLedgerPeers) $
                   traceWith wlpTracer FallingBackToPublicRootPeers
               atomically $ putResp Nothing
               go rng ts peerMap' bigPeerMap' lastSnapshotSlot'
           else do
               let ttl = 5 -- TTL, used as re-request interval by the governor.

               (rng', !pickedPeers) <- pickPeers rng wlpTracer peerMap' bigPeerMap' numRequested ledgerPeersKind
               case ledgerPeersKind of
                 BigLedgerPeers -> do
                   let numBigLedgerPeers = Map.size bigPeerMap'
                   when (getNumberOfPeers numRequested
                           > fromIntegral numBigLedgerPeers) $
                     traceWith wlpTracer (NotEnoughBigLedgerPeers numRequested numBigLedgerPeers)
                   traceWith wlpTracer (PickedBigLedgerPeers numRequested pickedPeers)
                 AllLedgerPeers -> do
                   let numLedgerPeers = Map.size peerMap'
                   when (getNumberOfPeers numRequested
                           > fromIntegral numLedgerPeers) $
                     traceWith wlpTracer (NotEnoughLedgerPeers numRequested numLedgerPeers)
                   traceWith wlpTracer (PickedLedgerPeers numRequested pickedPeers)


               let (plainAddrs, domains) =
                     List.foldl' partitionPeer (Set.empty, []) pickedPeers

               -- NOTE: we don't set `resolveConcurrent` because
               -- of https://github.com/kazu-yamamoto/dns/issues/174
               domainAddrs <- resolveLedgerPeers wlpTracer
                                                 paToPeerAddr
                                                 paDnsSemaphore
                                                 DNS.defaultResolvConf
                                                 paDnsActions
                                                 domains

               let (rng'', rngDomain) = split rng'
                   pickedAddrs =
                     snd $ List.foldl' pickDomainAddrs
                                  (rngDomain, plainAddrs)
                                  domainAddrs

               atomically $ putResp $ Just (pickedAddrs, ttl)
               go rng'' ts peerMap' bigPeerMap' lastSnapshotSlot'

    -- Randomly pick one of the addresses returned in the DNS result.
    pickDomainAddrs :: (StdGen, Set peerAddr)
                    -> Set peerAddr
                    -> (StdGen, Set peerAddr)
    pickDomainAddrs (rng,  pickedAddrs) addrs | Set.null addrs = (rng, pickedAddrs)
    pickDomainAddrs (rng, !pickedAddrs) addrs =
        let (ix, rng')   = randomR (0, Set.size addrs - 1) rng
            !pickedAddr  = Set.elemAt ix addrs
            pickedAddrs' = Set.insert pickedAddr pickedAddrs
        in (rng', pickedAddrs')


    -- Divide the picked peers form the ledger into addresses we can use
    -- directly and domain names that we need to resolve.
    partitionPeer :: (Set peerAddr, [DomainAccessPoint])
                  -> RelayAccessPoint
                  -> (Set peerAddr, [DomainAccessPoint])
    partitionPeer (addrs, domains) (RelayDomainAccessPoint domain) =
      (addrs, domain : domains)
    partitionPeer (!addrs, domains) (RelayAccessAddress ip port) =
      let !addr  = paToPeerAddr ip port
          addrs' = Set.insert addr addrs
       in (addrs', domains)


-- | Arguments record to stakeMapWithSlotOverSource function
--
data StakeMapOverSource = StakeMapOverSource {
  ledgerSlotNo     :: WithOrigin SlotNo,
  ledgerPools      :: LedgerPeers,
  peerSnapshot     :: Maybe LedgerPeerSnapshot,
  lastSnapshotSlot :: SlotNo,
  peerMap          :: Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint),
  bigPeerMap       :: Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint),
  ula              :: AfterSlot }
  deriving Show

-- | Build up a stake map to sample ledger peers from. The SlotNo, if different from 0,
-- indicates that the maps are the stake pools from the snapshot taken from the particular
-- slot number.
--
stakeMapWithSlotOverSource :: StakeMapOverSource
                           -> (Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint),
                               Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint),
                               SlotNo)
stakeMapWithSlotOverSource StakeMapOverSource {
                             ledgerSlotNo,
                             ledgerPools,
                             peerSnapshot,
                             lastSnapshotSlot,
                             peerMap,
                             bigPeerMap,
                             ula } =
  case (ledgerSlotNo, ledgerPools, peerSnapshot) of
    (At t, LedgerPeers _ ledgerPools', Just (LedgerPeerSnapshot (At t', accLedgerPools)))
      | t' >= t -> -- we cache the peers from the snapshot to avoid unnecessary work
        if t' == lastSnapshotSlot
          then (peerMap, bigPeerMap, t')
          else (accPoolStake (map snd accLedgerPools), Map.fromAscList accLedgerPools, t')
      | otherwise -> (accPoolStake ledgerPools', accBigPoolStakeMap ledgerPools', 0)

    (_, LedgerPeers _ ledgerPools', Nothing) -> (accPoolStake ledgerPools', accBigPoolStakeMap ledgerPools', 0)

    (_, _, Just (LedgerPeerSnapshot (At t', accLedgerPools)))
      | After slot <- ula, t' >= slot -> do
        if t' == lastSnapshotSlot
          then (peerMap, bigPeerMap, t')
          else (accPoolStake (map snd accLedgerPools), Map.fromAscList accLedgerPools, t')

    _otherwise -> (Map.empty, Map.empty, 0)

-- | Argument record for withLedgerPeers
--
data WithLedgerPeersArgs m = WithLedgerPeersArgs {
  wlpRng                   :: StdGen,
  -- ^ Random generator for picking ledger peers
  wlpConsensusInterface    :: LedgerPeersConsensusInterface m,
  wlpTracer                :: Tracer m TraceLedgerPeers,
  -- ^ Get Ledger Peers comes from here
  wlpGetUseLedgerPeers     :: STM m UseLedgerPeers,
  -- ^ Get Use Ledger After value
  wlpGetLedgerPeerSnapshot :: STM m (Maybe LedgerPeerSnapshot)
  -- ^ Get ledger peer snapshot from file read by node
  }

-- | For a LedgerPeers worker thread and submit request and receive responses.
--
withLedgerPeers :: forall peerAddr resolver exception m a.
                   ( MonadAsync m
                   , MonadThrow m
                   , MonadMonotonicTime m
                   , Exception exception
                   , Ord peerAddr
                   )
                => PeerActionsDNS peerAddr resolver exception m
                -> WithLedgerPeersArgs m
                -> ((NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peerAddr, DiffTime)))
                     -> Async m Void
                     -> m a )
                -> m a
withLedgerPeers peerActionsDNS
                ledgerPeerArgs
                k = do
    reqVar  <- newEmptyTMVarIO
    respVar <- newEmptyTMVarIO
    let getRequest  = takeTMVar reqVar
        putResponse = putTMVar  respVar
        request :: NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peerAddr, DiffTime))
        request = \numberOfPeers ledgerPeersKind -> do
          atomically $ putTMVar reqVar (numberOfPeers, ledgerPeersKind)
          atomically $ takeTMVar respVar
    withAsync
      (ledgerPeersThread peerActionsDNS ledgerPeerArgs getRequest putResponse)
      $ \ thread -> k request thread
