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
    pickPeers,
    accPoolStake,

    Socket.PortNumber
    ) where


import           Control.Monad.Class.MonadSTM
import           Control.Tracer (Tracer, traceWith)
import qualified Data.IP as IP
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Ratio
import           Data.Word
import qualified Network.Socket as Socket
import           System.Random

import           Cardano.Slotting.Slot (SlotNo)
import           Ouroboros.Network.PeerSelection.RootPeersDNS (DomainAddress (..))

import           Text.Printf

newtype LedgerPeersConsensusInterface m = LedgerPeersConsensusInterface {
      lpGetPeers :: SlotNo -> STM m (Maybe [(PoolStake, NonEmpty RelayAddress)])
    }

-- | Trace LedgerPeers events.
data TraceLedgerPeers =
      PickedPeer !RelayAddress !AccPoolStake ! PoolStake
      -- ^ Trace for a peer picked with accumulated and relative stake of its pool.
    | PickedPeers !Word16 ![RelayAddress]
      -- ^ Trace for the number of peers we wanted to pick and the list of peers picked.
    | FetchingNewLedgerState !Int
      -- ^ Trace for fetching a new list of peers from the ledger. Int is the number of peers
      -- returned.


instance Show TraceLedgerPeers where
    show (PickedPeer addr ackStake stake) =
        printf "PickedPeer %s ack stake %s ( %.04f) relative stake %s ( %.04f )"
            (show addr)
            (show $ unAccPoolStake ackStake)
            (fromRational (unAccPoolStake ackStake) :: Double)
            (show $ unPoolStake stake)
            (fromRational (unPoolStake stake) :: Double)
    show (PickedPeers n peers) =
        printf "PickedPeers %d %s" n (show peers)
    show (FetchingNewLedgerState cnt) =
        printf "Fetching new ledgerstate, %d registered pools"
            cnt


data RelayAddress = RelayAddressDomain DomainAddress
                  | RelayAddressAddr IP.IP Socket.PortNumber
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
          -> Word16
          -> m (StdGen, [RelayAddress])
pickPeers inRng _ pools _ | Map.null pools = return (inRng, [])
pickPeers inRng tracer pools cnt = go inRng cnt []
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
