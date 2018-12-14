{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Mock.Mempool (
    Mempool(..)
  , mempoolToList
  , mempoolInsert
  , Transaction(..)
  ) where

import           Control.Monad.Except
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup, (<>))
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Ledger.Mock (HasUtxo, Tx, TxIn, Utxo,
                     txIns, updateUtxo, utxo)
import           Ouroboros.Consensus.Util (Condense (..))
import           Ouroboros.Consensus.Util.HList
import           Ouroboros.Network.Serialise (Serialise)

{-------------------------------------------------------------------------------
  Mempool
-------------------------------------------------------------------------------}

newtype Mempool tx = Mempool { mempoolToSet :: Set tx }
  deriving (HasUtxo, Condense, Semigroup, Monoid)

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

  consistent :: Monad m => Utxo -> Mempool tx -> tx -> ExceptT (Inconsistent tx) m ()
  collect    ::            Utxo -> Mempool tx -> Set tx -> (Set tx, Mempool tx)



{-------------------------------------------------------------------------------
  Very simple model for transactions
-------------------------------------------------------------------------------}

data InconsistentTx =
    InputNotAvailable Utxo TxIn
  deriving (Show)

instance Condense InconsistentTx where
  condense (InputNotAvailable _ inp) = condense inp

instance Transaction Tx where
  type Inconsistent Tx = InconsistentTx

  consistent chainUtxo mempool tx =
      forM_ (txIns tx) $ \inp ->
        unless (inp `Map.member` curUtxo) $
          throwError $ InputNotAvailable curUtxo inp
    where
      curUtxo :: Utxo
      curUtxo = chainUtxo <> utxo (mempool :* Nil)

  collect chainUtxo mempool alreadyConfirmed =
      go chainUtxo Set.empty Set.empty (mempoolToList mempool)
    where
      go :: Utxo    -- ^ Accumulator: UTxO
         -> Set Tx  -- ^ Accumulator: valid
         -> Set Tx  -- ^ Accumulator: invalid
         -> [Tx]
         -> (Set Tx, Mempool Tx)
      go _ accValid accInvalid [] = (accValid, Mempool accInvalid)
      go accUtxo accValid accInvalid (tx:txs)
        | tx `Set.member` alreadyConfirmed =
            go accUtxo accValid accInvalid txs
        | all (`Map.member` accUtxo) (txIns tx) =
            go (updateUtxo tx accUtxo) (Set.insert tx accValid) accInvalid txs
        | otherwise =
            go accUtxo accValid (Set.insert tx accInvalid) txs
