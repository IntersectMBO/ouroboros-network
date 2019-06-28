{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Mock.UTxO (
    -- * Basic definitions
    Tx(..)
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

import           Codec.Serialise (Serialise(..))
import           Control.Monad.Except
import           Data.Either (fromRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR(..))
import           Cardano.Crypto.Hash

import           Ouroboros.Network.Chain (Chain, toOldestFirst)

import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.HList (All, HList)
import qualified Ouroboros.Consensus.Util.HList as HList
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Consensus.Ledger.Mock.Address

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

data Tx = Tx (Set TxIn) [TxOut]
  deriving (Show, Eq, Ord, Generic, Serialise)

instance ToCBOR Tx where
  toCBOR = encode

instance Condense Tx where
  condense (Tx ins outs) = condense (ins, outs)

type TxId  = Hash ShortHash Tx
type TxIn  = (TxId, Int)
type TxOut = (Addr, Int)
type Utxo  = Map TxIn TxOut

{-------------------------------------------------------------------------------
  Computing UTxO
-------------------------------------------------------------------------------}

newtype InvalidInputs = InvalidInputs (Set TxIn)
  deriving newtype (Show, Condense)

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
  txIns     (Tx ins _outs) = ins
  txOuts tx@(Tx _ins outs) =
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

instance All HasUtxo as => HasUtxo (HList as) where
  txIns      = HList.foldr (Proxy @HasUtxo) (Set.union . txIns)     Set.empty
  txOuts     = HList.foldr (Proxy @HasUtxo) (Map.union . txOuts)    Map.empty
  confirmed  = HList.foldr (Proxy @HasUtxo) (Set.union . confirmed) Set.empty
  updateUtxo = HList.repeatedlyM (Proxy @HasUtxo) updateUtxo

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
genesisTx addrDist = Tx mempty [(addr, 1000) | addr <- Map.keys addrDist]

genesisUtxo :: AddrDist -> Utxo
genesisUtxo addrDist =
    fromRight (error "genesisLedger: invalid genesis tx") $
      runExcept (utxo (genesisTx addrDist))
