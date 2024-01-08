{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Network.PeerSelection.LedgerPeers
  ( DomainAccessPoint (..)
  , IP.IP (..)
  , LedgerPeersConsensusInterface (..)
  , LedgerStateJudgement (..)
  , LedgerPeers (..)
  , getLedgerPeers
  , RelayAccessPoint (..)
  , PoolStake (..)
  , AccPoolStake (..)
  , TraceLedgerPeers (..)
  , NumberOfPeers (..)
  , LedgerPeersKind (..)
    -- * Ledger Peers specific functions
  , accPoolStake
  , accBigPoolStake
  , bigLedgerPeerQuota
    -- * DNS based provider for ledger root peers
  , withLedgerPeers
    -- * Re-exports
  , module Ouroboros.Network.PeerSelection.LedgerPeers.Type
  , module Ouroboros.Network.PeerSelection.LedgerPeers.Common
    -- * Internal only exported for testing purposes
  , resolveLedgerPeers
  ) where

import           Control.Exception (assert)
import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer (Tracer, traceWith)
import           Data.Bifunctor (first)
import qualified Data.IP as IP
import           Data.List (foldl', sortOn)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))
import           Data.Ratio
import           System.Random

import           Cardano.Slotting.Slot (SlotNo)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Common
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)
import           Data.Word (Word16, Word64)

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow


import qualified Network.DNS as DNS
import qualified Network.Socket as Socket

import           Ouroboros.Network.PeerSelection.RelayAccessPoint
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
                     (DNSActions)
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
                     (DNSSemaphore)
import           Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers

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
  -> SlotNo
  -> STM m LedgerPeers
getLedgerPeers (LedgerPeersConsensusInterface lpGetLatestSlot
                                              lpGetLedgerStateJudgement
                                              lpGetLedgerPeers)
               slot = do
  curSlot <- lpGetLatestSlot
  if curSlot < slot
     then pure BeforeSlot
     else LedgerPeers <$> lpGetLedgerStateJudgement
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
    . foldl' fn []
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

-- | The total accumulated stake of big ledger peers.
--
bigLedgerPeerQuota :: AccPoolStake
bigLedgerPeerQuota = 0.9

-- | Convert a list of pools with stake to a Map keyed on the accumulated stake
-- which only contains big ledger peers, e.g. largest ledger peers which
-- cumulatively control 90% of stake.
--
accBigPoolStake :: [(PoolStake, NonEmpty RelayAccessPoint)]
                -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
accBigPoolStake =
      Map.fromAscList -- the input list is ordered by `AccPoolStake`, thus we
                      -- can use `fromAscList`
    . takeWhilePrev (\(acc, _) -> acc <= bigLedgerPeerQuota)
    . go 0
    . sortOn (Down . fst)
    . reRelativeStake BigLedgerPeers
  where
    takeWhilePrev :: (a -> Bool) -> [a] -> [a]
    takeWhilePrev f as =
        fmap snd
      . takeWhile (\(a, _) -> maybe True f a)
      $ zip (Nothing : (Just <$> as)) as

    -- natural fold
    go :: AccPoolStake
       -> [(PoolStake, NonEmpty RelayAccessPoint)]
       -> [(AccPoolStake, (PoolStake, NonEmpty RelayAccessPoint))]
    go _acc [] = []
    go !acc (a@(s, _) : as) =
      let acc' = acc + AccPoolStake (unPoolStake s)
      in (acc', a) : go acc' as

-- | Not all stake pools have valid \/ usable relay information. This means that
-- we need to recalculate the relative stake for each pool.
--
reRelativeStake :: LedgerPeersKind
                -> [(PoolStake, NonEmpty RelayAccessPoint)]
                -> [(PoolStake, NonEmpty RelayAccessPoint)]
reRelativeStake ledgerPeersKind pl =
    let pl'   = first adjustment <$> pl
        total = foldl' (+) 0 (fst <$> pl')
        pl''  = first (/ total) <$> pl'
    in
    assert (let total' = sum $ map fst pl''
            in total == 0 || (total' > (PoolStake $ 999999 % 1000000) &&
                  total' < (PoolStake $ 1000001 % 1000000))
           )
    pl''
  where
    adjustment :: PoolStake -> PoolStake
    adjustment =
      case ledgerPeersKind of
        AllLedgerPeers ->
          -- We do loose some precision in the conversion. However we care about
          -- precision in the order of 1 block per year and for that a Double is
          -- good enough.
          PoolStake . toRational . sqrt @Double . fromRational . unPoolStake
        BigLedgerPeers ->
          id


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
                  => StdGen
                  -> DNSSemaphore m
                  -> (IP.IP -> Socket.PortNumber -> peerAddr)
                  -> Tracer m TraceLedgerPeers
                  -> STM m UseLedgerAfter
                  -> LedgerPeersConsensusInterface m
                  -> DNSActions resolver exception m
                  -> STM m (NumberOfPeers, LedgerPeersKind)
                  -- ^ a blocking action which receives next request for more
                  -- ledger peers
                  -> (Maybe (Set peerAddr, DiffTime) -> STM m ())
                  -> m Void
ledgerPeersThread inRng dnsSemaphore toPeerAddr tracer readUseLedgerAfter
                  ledgerPeersConsensusInterface dnsActions
                  getReq putRsp = do
    go inRng (Time 0) Map.empty Map.empty
  where
    go :: StdGen
       -> Time
       -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
       -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
       -> m Void
    go rng oldTs peerMap bigPeerMap = do
        useLedgerAfter <- atomically readUseLedgerAfter
        traceWith tracer (TraceUseLedgerAfter useLedgerAfter)

        let peerListLifeTime =
              if Map.null peerMap && isLedgerPeersEnabled useLedgerAfter
                 then short_PEER_LIST_LIFE_TIME
                 else long_PEER_LIST_LIFE_TIME

        traceWith tracer WaitingOnRequest
        -- wait until next request of ledger peers
        (numRequested, ledgerPeersKind) <- atomically getReq
        traceWith tracer $ RequestForPeers numRequested
        !now <- getMonotonicTime
        let age = diffTime now oldTs
        (peerMap', bigPeerMap', ts) <- if age > peerListLifeTime
                             then
                                 case useLedgerAfter of
                                   DontUseLedger -> do
                                     traceWith tracer DisabledLedgerPeers
                                     return (Map.empty, Map.empty, now)
                                   UseLedgerAfter slot -> do
                                     peers <- (\case
                                                BeforeSlot          -> []
                                                LedgerPeers _ peers -> peers
                                              )
                                          <$> atomically (getLedgerPeers ledgerPeersConsensusInterface slot)
                                     let peers' = accPoolStake peers
                                         bigPeers = accBigPoolStake peers
                                     traceWith tracer $ FetchingNewLedgerState (Map.size peers') (Map.size bigPeers)
                                     return (peers', bigPeers, now)
                             else do
                                 traceWith tracer $ ReusingLedgerState (Map.size peerMap) age
                                 return (peerMap, bigPeerMap, oldTs)
        if Map.null peerMap'
           then do
               when (isLedgerPeersEnabled useLedgerAfter) $
                   traceWith tracer FallingBackToPublicRootPeers
               atomically $ putRsp Nothing
               go rng ts peerMap' bigPeerMap'
           else do
               let ttl = 5 -- TTL, used as re-request interval by the governor.

               (rng', !pickedPeers) <- pickPeers rng tracer peerMap' bigPeerMap' numRequested ledgerPeersKind
               case ledgerPeersKind of
                 BigLedgerPeers -> do
                   let numBigLedgerPeers = Map.size bigPeerMap'
                   when (getNumberOfPeers numRequested
                           > fromIntegral numBigLedgerPeers) $
                     traceWith tracer (NotEnoughBigLedgerPeers numRequested numBigLedgerPeers)
                   traceWith tracer (PickedBigLedgerPeers numRequested pickedPeers)
                 AllLedgerPeers -> do
                   let numLedgerPeers = Map.size peerMap'
                   when (getNumberOfPeers numRequested
                           > fromIntegral numLedgerPeers) $
                     traceWith tracer (NotEnoughLedgerPeers numRequested numLedgerPeers)
                   traceWith tracer (PickedLedgerPeers numRequested pickedPeers)


               let (plainAddrs, domains) =
                     foldl' partitionPeer (Set.empty, []) pickedPeers

               -- NOTE: we don't set `resolveConcurrent` because
               -- of https://github.com/kazu-yamamoto/dns/issues/174
               domainAddrs <- resolveLedgerPeers tracer
                                                 toPeerAddr
                                                 dnsSemaphore
                                                 DNS.defaultResolvConf
                                                 dnsActions
                                                 domains

               let (rng'', rngDomain) = split rng'
                   pickedAddrs =
                     snd $ foldl' pickDomainAddrs
                                  (rngDomain, plainAddrs)
                                  domainAddrs

               atomically $ putRsp $ Just (pickedAddrs, ttl)
               go rng'' ts peerMap' bigPeerMap'

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
      let !addr  = toPeerAddr ip port
          addrs' = Set.insert addr addrs
       in (addrs', domains)

-- | For a LedgerPeers worker thread and submit request and receive responses.
--
withLedgerPeers :: forall peerAddr resolver exception m a.
                   ( MonadAsync m
                   , MonadThrow m
                   , MonadMonotonicTime m
                   , Exception exception
                   , Ord peerAddr
                   )
                => StdGen
                -> DNSSemaphore m
                -> (IP.IP -> Socket.PortNumber -> peerAddr)
                -> Tracer m TraceLedgerPeers
                -> STM m UseLedgerAfter
                -> LedgerPeersConsensusInterface m
                -> DNSActions resolver exception m
                -> ( (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peerAddr, DiffTime)))
                     -> Async m Void
                     -> m a )
                -> m a
withLedgerPeers inRng dnsSemaphore toPeerAddr tracer readUseLedgerAfter interface dnsActions k = do
    reqVar  <- newEmptyTMVarIO
    respVar <- newEmptyTMVarIO
    let getRequest  = takeTMVar reqVar
        putResponse = putTMVar  respVar
        request :: NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peerAddr, DiffTime))
        request = \numberOfPeers ledgerPeersKind -> do
          atomically $ putTMVar reqVar (numberOfPeers, ledgerPeersKind)
          atomically $ takeTMVar respVar
    withAsync
      ( ledgerPeersThread inRng dnsSemaphore toPeerAddr tracer readUseLedgerAfter
                          interface dnsActions getRequest putResponse )
      $ \ thread -> k request thread
