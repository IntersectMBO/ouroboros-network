{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Mock.UTxO (
    -- * Basic definitions
    Tx(Tx)
  , TxId
  , TxIn
  , TxOut
  , Addr
  , Amount
  , Ix
  , Utxo
    -- * Computing UTxO
  , UtxoError(..)
  , HasUtxo(..)
    -- * Genesis
  , genesisTx
  , genesisUtxo
  ) where

import           Codec.Serialise (Serialise (..))
import           Control.DeepSeq (NFData (..), force)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor (($>))
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

pattern Tx :: Set TxIn -> [TxOut] -> Tx
pattern Tx ins outs <- UnsafeTx ins outs where
  Tx ins outs = force $ UnsafeTx ins outs

{-# COMPLETE Tx #-}

instance ToCBOR Tx where
  toCBOR = encode

instance Condense Tx where
  condense (Tx ins outs) = condense (ins, outs)

type Ix     = Word
type Amount = Word
type TxId   = Hash ShortHash Tx
type TxIn   = (TxId, Ix)
type TxOut  = (Addr, Amount)
type Utxo   = Map TxIn TxOut

{-------------------------------------------------------------------------------
  Computing UTxO
-------------------------------------------------------------------------------}

data UtxoError
  = MissingInput TxIn
  | InputOutputMismatch
      Amount  -- ^ Input
      Amount  -- ^ Output
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (Serialise, NoUnexpectedThunks)

instance Condense UtxoError where
  condense = show

class HasUtxo a where
  txIns      :: a -> Set TxIn
  txOuts     :: a -> Utxo
  confirmed  :: a -> Set TxId
  updateUtxo :: a -> Utxo -> Except UtxoError Utxo

{-------------------------------------------------------------------------------
  HasUtxo instances
-------------------------------------------------------------------------------}

instance HasUtxo Tx where
  txIns     (Tx ins _outs) = ins
  txOuts tx@(Tx _ins outs) =
      Map.fromList $ zipWith aux [0..] outs
    where
      aux :: Ix -> TxOut -> (TxIn, TxOut)
      aux ix out = ((hash tx, ix), out)

  confirmed       = Set.singleton . hash
  updateUtxo tx = execStateT $ do
    -- Remove all inputs from the Utxo and calculate the sum of all the input
    -- amounts
    inputAmount <- fmap sum $ forM (Set.toList (txIns tx)) $ \txIn -> do
      u <- get
      case Map.updateLookupWithKey (\_ _ -> Nothing) txIn u of
        (Nothing,              _)  -> throwError $ MissingInput txIn
        (Just (_addr, amount), u') -> put u' $> amount

    -- Check that the sum of the inputs is equal to the sum of the outputs
    let outputAmount = sum $ map snd $ Map.elems $ txOuts tx
    when (inputAmount /= outputAmount) $
      throwError $ InputOutputMismatch inputAmount outputAmount

    -- Add the outputs to the Utxo
    modify (`Map.union` txOuts tx)

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
genesisTx addrDist = Tx mempty [(addr, 1000) | addr <- Map.keys addrDist]

genesisUtxo :: AddrDist -> Utxo
genesisUtxo addrDist = txOuts (genesisTx addrDist)
