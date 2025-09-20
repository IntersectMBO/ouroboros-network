{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Network.PeerSelection.LedgerPeers.Utils
  ( bigLedgerPeerQuota
  , accumulateBigLedgerStake
  , recomputeRelativeStake
  , AccPoolStake (..)
  , PoolStake (..)
  , RelayAccessPoint (..)
  ) where

import Control.Exception (assert)
import Data.Bifunctor (first)
import Data.List as List (foldl', sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (Down (..))
import Data.Ratio ((%))

import Ouroboros.Network.PeerSelection.LedgerPeers.Type

-- | Big ledger peers are those ledger peers, which when sorted down by their
-- relative stake, in the aggregate hold 90% of the total stake in the network.
--
bigLedgerPeerQuota :: AccPoolStake
bigLedgerPeerQuota = 0.9

-- | Sort ascendingly a given list of pools with stake,
-- and tag each one with cumulative stake, with a cutoff
-- at 'bigLedgerPeerQuota'
--
accumulateBigLedgerStake
  :: forall relayAccessPoint.
     [(PoolStake, NonEmpty relayAccessPoint)]
  -> [(AccPoolStake, (PoolStake, NonEmpty relayAccessPoint))]
accumulateBigLedgerStake =
    takeWhilePrev (\(acc, _) -> acc <= bigLedgerPeerQuota)
    . go 0
    . sortOn (Down . fst)
    . recomputeRelativeStake BigLedgerPeers
  where
    takeWhilePrev :: (a -> Bool) -> [a] -> [a]
    takeWhilePrev f as =
        fmap snd
      . takeWhile (\(a, _) -> maybe True f a)
      $ zip (Nothing : (Just <$> as)) as

    -- natural fold
    go :: AccPoolStake
       -> [(PoolStake, NonEmpty relayAccessPoint)]
       -> [(AccPoolStake, (PoolStake, NonEmpty relayAccessPoint))]
    go _acc [] = []
    go !acc (a@(s, _) : as) =
      let acc' = acc + AccPoolStake (unPoolStake s)
      in (acc', a) : go acc' as

-- | Not all stake pools have valid \/ usable relay information. This means that
-- we need to recalculate the relative stake for each pool.
--
recomputeRelativeStake
  :: LedgerPeersKind
  -> [(PoolStake, NonEmpty relayAccessPoint)]
  -> [(PoolStake, NonEmpty relayAccessPoint)]
recomputeRelativeStake ledgerPeersKind pl =
    let pl'   = first adjustment <$> pl
        total = List.foldl' (+) 0 (fst <$> pl')
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
