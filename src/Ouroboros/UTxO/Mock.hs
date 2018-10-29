{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Ouroboros.UTxO.Mock (
    -- * Basic definitions
    PreTx(..)
  , Tx(..)
  , mkTx
  , TxIn
  , TxOut
  , Addr
  , Utxo
    -- * Compute UTxO
  , HasUtxo(..)
  , utxo
  ) where

import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Proxy
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           GHC.Generics                (Generic)

import           Ouroboros.Infra.Crypto.Mock
import           Ouroboros.Infra.Util
import           Ouroboros.Infra.Util.HList  (All, HList)
import qualified Ouroboros.Infra.Util.HList  as HList

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

data PreTx = PreTx (Set TxIn) [TxOut]
  deriving (Show, Eq, Ord, Generic)

instance Condense PreTx where
  condense (PreTx ins outs) = condense (ins, outs)

newtype Tx = Tx (Hashable Tx PreTx)
  deriving (Show, Eq, Ord, Generic, Decorates PreTx, Condense)

mkTx :: Hash Tx -> PreTx -> Tx
mkTx = decorateWith

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
  txIns     (undecorate -> PreTx ins _outs) = ins
  txOuts tx@(undecorate -> PreTx _ins outs) =
      Map.fromList $ map aux (zip [0..] outs)
    where
      aux :: (Int, TxOut) -> (TxIn, TxOut)
      aux (ix, out) = ((decoration tx, ix), out)

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
