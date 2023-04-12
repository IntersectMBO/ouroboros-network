{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Network.PeerSelection.LedgerPeers
  ( DomainAccessPoint (..)
  , IP.IP (..)
  , LedgerPeersConsensusInterface (..)
  , RelayAccessPoint (..)
  , PoolStake (..)
  , AccPoolStake (..)
  , TraceLedgerPeers (..)
  , NumberOfPeers (..)
  , accPoolStake
  , withLedgerPeers
  , UseLedgerAfter (..)
  , IsLedgerPeer (..)
  , Socket.PortNumber
  ) where


import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (assert)
import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer, traceWith)
import qualified Data.IP as IP
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ratio
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)
import           Data.Word
import qualified Network.Socket as Socket
import           System.Random

import           Cardano.Slotting.Slot (SlotNo)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type
                     (AccPoolStake (..), PoolStake (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DomainAccessPoint (..), RelayAccessPoint (..))
import           Text.Printf

-- | Only use the ledger after the given slot number.
data UseLedgerAfter = DontUseLedger | UseLedgerAfter SlotNo deriving (Eq, Show)

isLedgerPeersEnabled :: UseLedgerAfter -> Bool
isLedgerPeersEnabled DontUseLedger = False
isLedgerPeersEnabled _             = True

-- | Identifies a peer as coming from ledger or not
data IsLedgerPeer = IsLedgerPeer | IsNotLedgerPeer
  deriving (Eq, Show)

newtype NumberOfPeers = NumberOfPeers Word16 deriving Show

newtype LedgerPeersConsensusInterface m = LedgerPeersConsensusInterface {
      lpGetPeers :: SlotNo -> STM m (Maybe [(PoolStake, NonEmpty RelayAccessPoint)])
    }

-- | Trace LedgerPeers events.
data TraceLedgerPeers =
      PickedPeer RelayAccessPoint AccPoolStake PoolStake
      -- ^ Trace for a peer picked with accumulated and relative stake of its pool.
    | PickedPeers NumberOfPeers [RelayAccessPoint]
      -- ^ Trace for the number of peers we wanted to pick and the list of peers picked.
    | FetchingNewLedgerState Int
      -- ^ Trace for fetching a new list of peers from the ledger. Int is the number of peers
      -- returned.
    | DisabledLedgerPeers
      -- ^ Trace for when getting peers from the ledger is disabled, that is DontUseLedger.
    | TraceUseLedgerAfter UseLedgerAfter
      -- ^ Trace UseLedgerAfter value
    | WaitingOnRequest
    | RequestForPeers NumberOfPeers
    | ReusingLedgerState Int DiffTime
    | FallingBackToBootstrapPeers


instance Show TraceLedgerPeers where
    show (PickedPeer addr ackStake stake) =
        printf "PickedPeer %s ack stake %s ( %.04f) relative stake %s ( %.04f )"
            (show addr)
            (show $ unAccPoolStake ackStake)
            (fromRational (unAccPoolStake ackStake) :: Double)
            (show $ unPoolStake stake)
            (fromRational (unPoolStake stake) :: Double)
    show (PickedPeers (NumberOfPeers n) peers) =
        printf "PickedPeers %d %s" n (show peers)
    show (FetchingNewLedgerState cnt) =
        printf "Fetching new ledgerstate, %d registered pools"
            cnt
    show (TraceUseLedgerAfter ula) =
        printf "UseLedgerAfter state %s"
            (show ula)
    show WaitingOnRequest = "WaitingOnRequest"
    show (RequestForPeers (NumberOfPeers cnt)) = printf "RequestForPeers %d" cnt
    show (ReusingLedgerState cnt age) =
        printf "ReusingLedgerState %d peers age %s"
          cnt
          (show age)
    show FallingBackToBootstrapPeers = "Falling back to bootstrap peers"
    show DisabledLedgerPeers = "LedgerPeers is disabled"


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
accPoolStake pl =
    let pl' = reRelativeStake pl
        ackList = foldl' fn [] pl' in
    Map.fromList ackList
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

-- | Not all stake pools have valid \/ usable relay information. This means that we need to
-- recalculate the relative stake for each pool.
--
-- The relative stake is scaled by the square root in order to increase the number
-- of down stream peers smaller pools are likely to get.
-- https://en.wikipedia.org/wiki/Penrose_method
--
reRelativeStake :: [(PoolStake, NonEmpty RelayAccessPoint)]
                -> [(PoolStake, NonEmpty RelayAccessPoint)]
reRelativeStake pl =
    let total = foldl' (+) 0 $ map (adjustment . fst) pl
        pl' = map  (\(s, rls) -> (adjustment s / total, rls)) pl
        total' = sum $ map fst pl' in
    assert (total == 0 || (total' > (PoolStake $ 999999 % 1000000) &&
            total' < (PoolStake $ 1000001 % 1000000))) pl'

  where
    -- We do loose some precisioun in the conversion. However we care about precision
    -- in the order of 1 block per year and for that a Double is good enough.
    adjustment :: PoolStake -> PoolStake
    adjustment (PoolStake s) =
      let d = fromRational s ::Double in
      PoolStake $ toRational $ sqrt d


-- | Try to pick n random peers.
pickPeers :: forall m. Monad m
          => StdGen
          -> Tracer m TraceLedgerPeers
          -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
          -> NumberOfPeers
          -> m (StdGen, [RelayAccessPoint])
pickPeers inRng _ pools _ | Map.null pools = return (inRng, [])
pickPeers inRng tracer pools (NumberOfPeers cnt) = go inRng cnt []
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
                 traceWith tracer $ PickedPeer relay ackStake stake
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
ledgerPeersThread :: forall m peerAddr.
                     ( MonadAsync m
                     , MonadTime m
                     , Ord peerAddr
                     )
                  => StdGen
                  -> (IP.IP -> Socket.PortNumber -> peerAddr)
                  -> Tracer m TraceLedgerPeers
                  -> STM m UseLedgerAfter
                  -> LedgerPeersConsensusInterface m
                  -> ([DomainAccessPoint] -> m (Map DomainAccessPoint (Set peerAddr)))
                  -> STM m NumberOfPeers
                  -- ^ a blocking action which receives next request for more
                  -- ledger peers
                  -> (Maybe (Set peerAddr, DiffTime) -> STM m ())
                  -> m Void
ledgerPeersThread inRng toPeerAddr tracer readUseLedgerAfter LedgerPeersConsensusInterface{..} doResolve
                  getReq putRsp =
    go inRng (Time 0) Map.empty
  where
    go :: StdGen -> Time -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
       -> m Void
    go rng oldTs peerMap = do
        useLedgerAfter <- atomically readUseLedgerAfter
        traceWith tracer (TraceUseLedgerAfter useLedgerAfter)

        let peerListLifeTime = if Map.null peerMap && isLedgerPeersEnabled useLedgerAfter
                                  then short_PEER_LIST_LIFE_TIME
                                  else long_PEER_LIST_LIFE_TIME

        traceWith tracer WaitingOnRequest
        -- wait until next request of ledger peers
        numRequested <- atomically getReq
        traceWith tracer $ RequestForPeers numRequested
        !now <- getMonotonicTime
        let age = diffTime now oldTs
        (peerMap', ts) <- if age > peerListLifeTime
                             then
                                 case useLedgerAfter of
                                   DontUseLedger -> do
                                     traceWith tracer DisabledLedgerPeers
                                     return (Map.empty, now)
                                   UseLedgerAfter slot -> do
                                     peers_m <- atomically $ lpGetPeers slot
                                     let peers = maybe Map.empty accPoolStake peers_m
                                     traceWith tracer $ FetchingNewLedgerState $ Map.size peers
                                     return (peers, now)

                             else do
                                 traceWith tracer $ ReusingLedgerState (Map.size peerMap) age
                                 return (peerMap, oldTs)

        if Map.null peerMap'
           then do
               when (isLedgerPeersEnabled useLedgerAfter) $
                   traceWith tracer FallingBackToBootstrapPeers
               atomically $ putRsp Nothing
               go rng ts peerMap'
           else do
               let ttl = 5 -- TTL, used as re-request interval by the governor.

               (rng', !pickedPeers) <- pickPeers rng tracer peerMap' numRequested
               traceWith tracer $ PickedPeers numRequested pickedPeers

               let (plainAddrs, domains) = foldl' partitionPeer (Set.empty, []) pickedPeers

               domainAddrs <- doResolve domains

               let (rng'', rngDomain) = split rng'
                   pickedAddrs = snd $ foldl' pickDomainAddrs (rngDomain, plainAddrs)
                                                       domainAddrs

               atomically $ putRsp $ Just (pickedAddrs, ttl)
               go rng'' ts peerMap'

    -- Randomly pick one of the addresses returned in the DNS result.
    pickDomainAddrs :: (StdGen, Set peerAddr)
                    -> Set peerAddr
                    -> (StdGen, Set peerAddr)
    pickDomainAddrs (rng, pickedAddrs) addrs | Set.null addrs = (rng, pickedAddrs)
    pickDomainAddrs (rng, pickedAddrs) addrs =
        let (ix, rng') = randomR (0, Set.size addrs - 1) rng
            !pickedAddr = Set.elemAt ix addrs in
        (rng', Set.insert pickedAddr pickedAddrs)


    -- Divide the picked peers form the ledger into addresses we can use directly and
    -- domain names that we need to resolve.
    partitionPeer :: (Set peerAddr, [DomainAccessPoint])
                  -> RelayAccessPoint
                  -> (Set peerAddr, [DomainAccessPoint])
    partitionPeer (addrs, domains) (RelayDomainAccessPoint domain) = (addrs, domain : domains)
    partitionPeer (addrs, domains) (RelayAccessAddress ip port) =
        let !addr = toPeerAddr ip port in
        (Set.insert addr addrs, domains)


-- | For a LedgerPeers worker thread and submit request and receive responses.
--
withLedgerPeers :: forall peerAddr m a.
                   ( MonadAsync m
                   , MonadTime m
                   , Ord peerAddr
                   )
                => StdGen
                -> (IP.IP -> Socket.PortNumber -> peerAddr)
                -> Tracer m TraceLedgerPeers
                -> STM m UseLedgerAfter
                -> LedgerPeersConsensusInterface m
                -> ([DomainAccessPoint] -> m (Map DomainAccessPoint (Set peerAddr)))
                -> ( (NumberOfPeers -> m (Maybe (Set peerAddr, DiffTime)))
                     -> Async m Void
                     -> m a )
                -> m a
withLedgerPeers inRng toPeerAddr tracer readUseLedgerAfter interface doResolve k = do
    reqVar  <- newEmptyTMVarIO
    respVar <- newEmptyTMVarIO
    let getRequest  = takeTMVar reqVar
        putResponse = putTMVar  respVar
        request :: NumberOfPeers -> m (Maybe (Set peerAddr, DiffTime))
        request = \numberOfPeers -> do
          atomically $ putTMVar reqVar numberOfPeers
          atomically $ takeTMVar respVar
    withAsync
      ( ledgerPeersThread inRng toPeerAddr tracer readUseLedgerAfter
                          interface doResolve
                          getRequest putResponse )
      $ \ thread -> k request thread

