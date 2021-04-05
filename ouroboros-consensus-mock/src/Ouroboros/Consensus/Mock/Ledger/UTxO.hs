{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mock.Ledger.UTxO (
    -- * Basic definitions
    Addr
  , Amount
  , Expiry (..)
  , Ix
  , Tx (Tx)
  , TxId
  , TxIn
  , TxOut
  , Utxo
    -- * Computing UTxO
  , HasMockTxs (..)
  , UtxoError (..)
  , confirmed
  , txIns
  , txOuts
  , updateUtxo
    -- * Genesis
  , genesisTx
  , genesisUtxo
  ) where

import           Codec.Serialise (Serialise (..))
import           Control.DeepSeq (NFData (..), force, rwhnf)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor (($>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class (InspectHeap (..), NoThunks)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.Hash

import           Ouroboros.Network.MockChain.Chain (Chain, toOldestFirst)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util (repeatedlyM)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Consensus.Mock.Ledger.Address

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

data Expiry
  = DoNotExpire
  | ExpireAtOnsetOf !SlotNo
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks)

instance NFData Expiry where rnf = rwhnf

instance Condense Expiry where
  condense = show

data Tx = UnsafeTx Expiry (Set TxIn) [TxOut]
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NFData)
  deriving NoThunks via InspectHeap Tx

pattern Tx :: Expiry -> Set TxIn -> [TxOut] -> Tx
pattern Tx expiry ins outs <- UnsafeTx expiry ins outs where
  Tx expiry ins outs = force $ UnsafeTx expiry ins outs

{-# COMPLETE Tx #-}

instance ToCBOR Tx where
  toCBOR = encode

instance Condense Tx where
  condense (Tx expiry ins outs) = condense (expiry, ins, outs)

type Ix     = Word
type Amount = Word
type TxId   = Hash MD5 Tx
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
  deriving anyclass (Serialise, NoThunks)

instance Condense UtxoError where
  condense = show

class HasMockTxs a where
  -- | The transactions in the order they are to be applied
  --
  getMockTxs :: a -> [Tx]

instance HasMockTxs Tx where
  getMockTxs = (:[])

instance HasMockTxs a => HasMockTxs [a] where
  getMockTxs = concatMap getMockTxs

instance HasMockTxs a => HasMockTxs (Chain a) where
  getMockTxs = getMockTxs . toOldestFirst

txIns :: HasMockTxs a => a -> Set TxIn
txIns = Set.unions . map each . getMockTxs
  where
    each (Tx _expiry ins _outs) = ins

txOuts :: HasMockTxs a => a -> Utxo
txOuts = Map.unions . map each . getMockTxs
  where
    each tx@(Tx _expiry _ins outs) =
        Map.fromList $ zipWith aux [0..] outs
      where
        aux :: Ix -> TxOut -> (TxIn, TxOut)
        aux ix out = ((hashWithSerialiser toCBOR tx, ix), out)

confirmed :: HasMockTxs a => a -> Set TxId
confirmed = Set.fromList . map (hashWithSerialiser toCBOR) . getMockTxs

updateUtxo :: HasMockTxs a => a -> Utxo -> Except UtxoError Utxo
updateUtxo = repeatedlyM each . getMockTxs
  where
    each tx = execStateT $ do
        -- Remove all inputs from the Utxo and calculate the sum of all the
        -- input amounts
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

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

-- | Transaction giving initial stake to the nodes
genesisTx :: AddrDist -> Tx
genesisTx addrDist =
    Tx DoNotExpire mempty [(addr, 1000) | addr <- Map.keys addrDist]

genesisUtxo :: AddrDist -> Utxo
genesisUtxo addrDist = txOuts (genesisTx addrDist)
