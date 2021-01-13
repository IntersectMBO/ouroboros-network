{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Network.PeerSelection.LedgerPeers (
    DomainAddress (..),
    IP.IP (..),
    LedgerPeersConsensusInterface (..),
    RelayAddress (..),
    PoolStake (..),
    AccPoolStake (..),
    TraceLedgerPeers (..),
    NumberOfPeers (..),
    pickPeers,
    accPoolStake,
    runLedgerPeers,
    UseLedgerAfter (..),

    Socket.PortNumber
    ) where


import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer, traceWith)
import qualified Data.IP as IP
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ratio
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Word
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Network.Socket (SockAddr)
import           System.Random

import           Cardano.Slotting.Slot (SlotNo)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DomainAddress (..))

import           Text.Printf

-- | Only use the ledger after the given slot number.
data UseLedgerAfter = DontUseLedger | UseLedgerAfter SlotNo deriving (Eq, Show)

isLedgerPeersEnabled :: UseLedgerAfter -> Bool
isLedgerPeersEnabled DontUseLedger = False
isLedgerPeersEnabled _             = True

newtype NumberOfPeers = NumberOfPeers Word16 deriving Show

newtype LedgerPeersConsensusInterface m = LedgerPeersConsensusInterface {
      lpGetPeers :: SlotNo -> STM m (Maybe [(PoolStake, NonEmpty RelayAddress)])
    }

-- | Trace LedgerPeers events.
data TraceLedgerPeers =
      PickedPeer !RelayAddress !AccPoolStake ! PoolStake
      -- ^ Trace for a peer picked with accumulated and relative stake of its pool.
    | PickedPeers !NumberOfPeers ![RelayAddress]
      -- ^ Trace for the number of peers we wanted to pick and the list of peers picked.
    | FetchingNewLedgerState !Int
      -- ^ Trace for fetching a new list of peers from the ledger. Int is the number of peers
      -- returned.
    | WaitingOnRequest
    | RequestForPeers !NumberOfPeers
    | ReusingLedgerState !Int !DiffTime
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
    show WaitingOnRequest = "WaitingOnRequest"
    show (RequestForPeers (NumberOfPeers cnt)) = printf "RequestForPeers %d" cnt
    show (ReusingLedgerState cnt age) =
        printf "ReusingLedgerState %d peers age %s"
          cnt
          (show age)
    show FallingBackToBootstrapPeers = "Falling back to bootstrap peers"

-- | A relay can have either an IP address and a port number or
-- a domain with a port number
data RelayAddress = RelayDomain DomainAddress
                  | RelayAddress IP.IP Socket.PortNumber
                  deriving (Show, Eq, Ord)

-- | The relative stake of a stakepool in relation to the total amount staked.
-- A value in the [0, 1] range.
--
newtype PoolStake = PoolStake { unPoolStake :: Rational }
  deriving (Eq, Fractional, Num, Ord, Show)

-- | The accumulated relative stake of a stake pool, like PoolStake but it also includes the
-- relative stake of all preceding pools. A value in the range [0, 1].
--
newtype AccPoolStake = AccPoolStake { unAccPoolStake :: Rational }
    deriving (Eq, Num, Ord)

-- | Convert a list of pools with stake to a Map keyed on the accumulated stake.
-- Consensus provides a list of pairs of relative stake and corresponding relays for all usable
-- registered pools.
-- By creating a Map keyed on the `AccPoolStake` that is the sum of the pool's relative stake and
-- the stake of all preceding pools we can support weighted random selection in
-- O(log n) time by taking advantage of Map.lookupGE (returns the smallest key greater or equal
-- to the provided value).
--
accPoolStake :: [(PoolStake, NonEmpty RelayAddress)]
             -> Map AccPoolStake (PoolStake, NonEmpty RelayAddress)
accPoolStake pl =
    let pl' = reRelativeStake pl
        ackList = foldl' fn [] pl' in
    Map.fromList ackList
  where
    fn :: [(AccPoolStake, (PoolStake, NonEmpty RelayAddress))]
       -> (PoolStake, NonEmpty RelayAddress)
       -> [(AccPoolStake, (PoolStake, NonEmpty RelayAddress))]
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
reRelativeStake :: [(PoolStake, NonEmpty RelayAddress)]
                -> [(PoolStake, NonEmpty RelayAddress)]
reRelativeStake pl =
    let total = sum $ map fst pl in
    map (\(s, rls) -> (s / total, rls)) pl

-- | Try to pick n random peers.
pickPeers :: forall m. Monad m
          => StdGen
          -> Tracer m TraceLedgerPeers
          -> Map AccPoolStake (PoolStake, NonEmpty RelayAddress)
          -> NumberOfPeers
          -> m (StdGen, [RelayAddress])
pickPeers inRng _ pools _ | Map.null pools = return (inRng, [])
pickPeers inRng tracer pools (NumberOfPeers cnt) = go inRng cnt []
  where
    go :: StdGen -> Word16 -> [RelayAddress] -> m (StdGen, [RelayAddress])
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


-- | Run the LedgerPeers worker thread.
runLedgerPeers :: forall m.
                      ( MonadAsync m
                      , MonadTime m
                      )
               => StdGen
               -> Tracer m TraceLedgerPeers
               -> UseLedgerAfter
               -> LedgerPeersConsensusInterface m
               -> ([DomainAddress] -> m (Map DomainAddress (Set SockAddr)))
               -> STM m NumberOfPeers
               -> (Maybe (Set SockAddr, DiffTime) -> STM m ())
               -> m Void
runLedgerPeers inRng tracer useLedgerAfter LedgerPeersConsensusInterface{..} doResolve
               getReq putRsp = do
    go inRng (Time 0) Map.empty
  where
    go :: StdGen -> Time -> Map AccPoolStake (PoolStake, NonEmpty RelayAddress) -> m Void
    go rng oldTs peerMap = do
        let peerListLifeTime = if Map.null peerMap && isLedgerPeersEnabled useLedgerAfter
                                  then 30
                                  else 1847 -- Close to but not exactly 30min.

        traceWith tracer WaitingOnRequest
        numRequested <- atomically getReq
        traceWith tracer $ RequestForPeers numRequested
        !now <- getMonotonicTime
        let age = diffTime now oldTs
        (peerMap', ts) <- if age > peerListLifeTime
                             then
                                 case useLedgerAfter of
                                   DontUseLedger -> do
                                     traceWith tracer $ FetchingNewLedgerState 0
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
               traceWith tracer FallingBackToBootstrapPeers
               atomically $ putRsp Nothing
               go rng ts peerMap'
           else do
               let ttl = 5 -- TTL, used as re-request interval by the governor.

               (rng', !pickedPeers) <- pickPeers rng tracer peerMap' numRequested
               traceWith tracer $ PickedPeers numRequested pickedPeers

               let (plainAddrs, domains) = foldl' splitPeers (Set.empty, []) pickedPeers

               domainAddrs <- doResolve domains

               let (rng'', rngDomain) = split rng'
                   pickedAddrs = snd $ foldl' pickDomainAddrs (rngDomain, plainAddrs)
                                                       domainAddrs

               atomically $ putRsp $ Just (pickedAddrs, ttl)
               go rng'' ts peerMap'

    -- Randomly pick one of the addresses returned in the DNS result.
    pickDomainAddrs :: (StdGen, Set SockAddr)
                    -> Set SockAddr
                    -> (StdGen, Set SockAddr)
    pickDomainAddrs (rng, pickedAddrs) addrs | Set.null addrs = (rng, pickedAddrs)
    pickDomainAddrs (rng, pickedAddrs) addrs =
        let (ix, rng') = randomR (0, Set.size addrs - 1) rng
            !pickedAddr = Set.elemAt ix addrs in
        (rng', Set.insert pickedAddr pickedAddrs)


    -- Divide the picked peers form the ledger into addresses we can use directly and
    -- domain names that we need to resolve.
    splitPeers :: (Set SockAddr, [DomainAddress])
               -> RelayAddress
               -> (Set SockAddr, [DomainAddress])
    splitPeers (addrs, domains) (RelayDomain domain) = (addrs, domain : domains)
    splitPeers (addrs, domains) (RelayAddress ip port) =
        let !addr = IP.toSockAddr (ip, port) in
        (Set.insert addr addrs, domains)
