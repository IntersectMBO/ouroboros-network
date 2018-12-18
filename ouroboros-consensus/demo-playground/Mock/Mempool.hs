{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Mock.Mempool (
    Mempool(..)
  , mempoolToList
  , mempoolInsert
  , mempoolRemove
  , collect
  , consistent
  ) where

import           Control.Monad.Except
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup, (<>))
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.HList

{-------------------------------------------------------------------------------
  Mempool
-------------------------------------------------------------------------------}

newtype Mempool tx = Mempool { mempoolToMap :: Map (Hash ShortHash tx) tx }
  deriving (HasUtxo, Condense, Semigroup, Monoid)

mempoolToList :: Mempool tx -> [tx]
mempoolToList = Map.elems . mempoolToMap

mempoolRemove :: Set (Hash ShortHash tx) -> Mempool tx -> Mempool tx
mempoolRemove toRemove (Mempool m) = Mempool (m `Map.withoutKeys` toRemove)

mempoolInsert :: Serialise tx => tx -> Mempool tx -> Mempool tx
mempoolInsert tx (Mempool txs) = Mempool (Map.insert (hash tx) tx txs)

{-------------------------------------------------------------------------------
  Abstract away from specific form of transactions
-------------------------------------------------------------------------------}

consistent :: (HasUtxo tx, Monad m)
           => Utxo
           -> Mempool tx
           -> tx
           -> ExceptT InvalidInputs m ()
consistent chainUtxo mempool tx = void $ updateUtxo tx curUtxo
  where
    curUtxo :: Utxo
    curUtxo =
        let Right u = runExcept $ utxo (mempool :* Nil)
        in chainUtxo <> u

-- | Collect the transactions from the mempool that can now be confirmed.
collect :: forall tx. HasUtxo tx
        => Utxo
        -> Mempool tx
        -> Map (Hash ShortHash tx) tx
collect chainUtxo =
    Map.filter (\tx -> txIns tx `Set.isSubsetOf` Map.keysSet chainUtxo) . mempoolToMap
