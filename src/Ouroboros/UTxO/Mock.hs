{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.UTxO.Mock (
    -- * Basic definitions
    Tx(..)
  , TxIn
  , TxOut
  , Addr
  , Utxo
    -- * Compute UTxO
  , HasUtxo(..)
  , utxo
  ) where

import           Codec.Serialise
import           Data.Map          (Map)
import qualified Data.Map.Strict   as Map
import           Data.Proxy
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           GHC.Generics      (Generic)

import           Infra.Crypto.Hash
import           Infra.Util
import           Infra.Util.HList  (All, HList)
import qualified Infra.Util.HList  as HList


{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

data Tx = Tx (Set TxIn) [TxOut]
  deriving (Show, Eq, Ord, Generic)

instance Condense Tx where
  condense (Tx ins outs) = condense (ins, outs)

type TxIn  = (Hash Tx, Int)
type TxOut = (Addr, Int)
type Addr  = String
type Utxo  = Map TxIn TxOut

{-------------------------------------------------------------------------------
  Computing UTxO
-------------------------------------------------------------------------------}

class HasUtxo a where
  txIns      :: a -> Set TxIn
  txOuts     :: a -> Utxo
  confirmed  :: a -> Set Tx
  updateUtxo :: a -> Utxo -> Utxo

utxo :: HasUtxo a => a -> Utxo
utxo a = updateUtxo a Map.empty

instance HasUtxo Tx where
  txIns     (Tx ins _outs) = ins
  txOuts tx@(Tx _ins outs) =
      Map.fromList $ map aux (zip [0..] outs)
    where
      aux :: (Int, TxOut) -> (TxIn, TxOut)
      aux (ix, out) = ((hash tx, ix), out)

  confirmed       = Set.singleton
  updateUtxo tx u = u `Map.union` txOuts tx

instance HasUtxo a => HasUtxo (Set a) where
  txIns           = txIns     . Set.toList
  txOuts          = txOuts    . Set.toList
  confirmed       = confirmed . Set.toList
  updateUtxo as u = (u `Map.union` txOuts as) `Map.withoutKeys` txIns as

instance HasUtxo a => HasUtxo [a] where
  txIns      = foldr (Set.union . txIns)     Set.empty
  txOuts     = foldr (Map.union . txOuts)    Map.empty
  confirmed  = foldr (Set.union . confirmed) Set.empty
  updateUtxo = repeatedly updateUtxo

instance All HasUtxo as => HasUtxo (HList as) where
  txIns      = HList.foldr (Proxy @HasUtxo) (Set.union . txIns)     Set.empty
  txOuts     = HList.foldr (Proxy @HasUtxo) (Map.union . txOuts)    Map.empty
  confirmed  = HList.foldr (Proxy @HasUtxo) (Set.union . confirmed) Set.empty
  updateUtxo = HList.repeatedly (Proxy @HasUtxo) updateUtxo

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise Tx
