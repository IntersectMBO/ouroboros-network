{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Ouroboros.Consensus.UTxO.Mempool (
    Mempool(..)
  , mempoolToList
  , mempoolInsert
  , consistent
  , collect
  ) where

import           Control.Monad.Except
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Infra.Util (Condense)
import           Ouroboros.Consensus.UTxO.Mock (HasUtxo)
import           Ouroboros.Network.Chain (Chain)
import           Ouroboros.Network.Serialise (Serialise)

{-------------------------------------------------------------------------------
  Mempool
-------------------------------------------------------------------------------}

newtype Mempool tx = Mempool { mempoolToSet :: Set tx }
  deriving (HasUtxo, Condense)

mempoolToList :: Mempool tx -> [tx]
mempoolToList = Set.toList . mempoolToSet

mempoolInsert :: Ord tx => tx -> Mempool tx -> Mempool tx
mempoolInsert tx (Mempool txs) = Mempool (Set.insert tx txs)

{-------------------------------------------------------------------------------
  Abstract away from specific form of transactions
-------------------------------------------------------------------------------}

class ( Show      tx
      , Ord       tx
      , Serialise tx
      , Condense  tx
      , HasUtxo   tx
      , Show     (Inconsistent tx)
      , Condense (Inconsistent tx)
      ) => Transaction tx where
  type Inconsistent tx :: *

  consistent :: Monad m => Chain tx -> Mempool tx -> tx -> ExceptT (Inconsistent tx) m ()
  collect    ::            Chain tx -> Mempool tx -> (Set tx, Mempool tx)
