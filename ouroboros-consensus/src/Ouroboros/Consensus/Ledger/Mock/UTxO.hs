{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Mock.UTxO (
    -- * Basic definitions
    Tx(..)
  , mkTx
  , TxId
  , TxIn
  , TxOut
  , Addr
  , Utxo
    -- * Computing UTxO
  , InvalidInputs(..)
  , HasUtxo(..)
  , utxo
    -- * Genesis
  , genesisTx
  , genesisUtxo
  ) where

import           Codec.Serialise (Serialise (..))
import           Control.DeepSeq (NFData (..), force)
import           Control.Monad.Except
import           Data.Either (fromRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.Hash
import           Cardano.Prelude (NoUnexpectedThunks, UseIsNormalForm (..))

import           Ouroboros.Network.MockChain.Chain (Chain, toOldestFirst)

import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Consensus.Ledger.Mock.Address

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

data Tx = UnsafeTx (Set TxIn) [TxOut]
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)
  deriving NoUnexpectedThunks via UseIsNormalForm Tx

mkTx :: Set TxIn -> [TxOut] -> Tx
mkTx ins outs = force tx
  where
    tx = UnsafeTx ins outs

instance ToCBOR Tx where
  toCBOR = encode

instance Condense Tx where
  condense (UnsafeTx ins outs) = condense (ins, outs)

type TxId  = Hash ShortHash Tx
type TxIn  = (TxId, Int)
type TxOut = (Addr, Int)
type Utxo  = Map TxIn TxOut

{-------------------------------------------------------------------------------
  Computing UTxO
-------------------------------------------------------------------------------}

newtype InvalidInputs = InvalidInputs (Set TxIn)
  deriving stock    (Generic)
  deriving newtype  (Show, Condense)
  deriving anyclass (Serialise)

class HasUtxo a where
  txIns      :: a -> Set TxIn
  txOuts     :: a -> Utxo
  confirmed  :: a -> Set TxId
  updateUtxo :: Monad m => a -> Utxo -> ExceptT InvalidInputs m Utxo

utxo :: (Monad m, HasUtxo a) => a -> ExceptT InvalidInputs m Utxo
utxo a = updateUtxo a Map.empty

{-------------------------------------------------------------------------------
  HasUtxo instances
-------------------------------------------------------------------------------}

instance HasUtxo Tx where
  txIns     (UnsafeTx ins _outs) = ins
  txOuts tx@(UnsafeTx _ins outs) =
      Map.fromList $ map aux (zip [0..] outs)
    where
      aux :: (Int, TxOut) -> (TxIn, TxOut)
      aux (ix, out) = ((hash tx, ix), out)

  confirmed       = Set.singleton . hash
  updateUtxo tx u =
      let notInUtxo = txIns tx Set.\\ (Map.keysSet u)
      in case Set.null notInUtxo of
           True  -> return $ (u `Map.union` txOuts tx) `Map.withoutKeys` txIns tx
           False -> throwError $ InvalidInputs notInUtxo

instance HasUtxo a => HasUtxo [a] where
  txIns      = foldr (Set.union . txIns)     Set.empty
  txOuts     = foldr (Map.union . txOuts)    Map.empty
  confirmed  = foldr (Set.union . confirmed) Set.empty
  updateUtxo = repeatedlyM updateUtxo

instance HasUtxo a => HasUtxo (Chain a) where
  txIns      = txIns      . toOldestFirst
  txOuts     = txOuts     . toOldestFirst
  updateUtxo = updateUtxo . toOldestFirst
  confirmed  = confirmed  . toOldestFirst

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

-- | Transaction giving initial stake to the nodes
genesisTx :: AddrDist -> Tx
genesisTx addrDist = mkTx mempty [(addr, 1000) | addr <- Map.keys addrDist]

genesisUtxo :: AddrDist -> Utxo
genesisUtxo addrDist =
    fromRight (error "genesisLedger: invalid genesis tx") $
      runExcept (utxo (genesisTx addrDist))
