{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shelley mempool integration
module Ouroboros.Consensus.Shelley.Ledger.Mempool (
    ApplyTx (..)
  , SL.ApplyTxError (..)
  , GenTx (..)
  , TxId (..)
  , mkShelleyTx
  ) where

import           Control.Monad.Except (Except)
import           Data.Foldable (toList)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..),
                     FullByteString (..), Annotator (..))
import           Cardano.Prelude (NoUnexpectedThunks (..), UseIsNormalForm (..))

import           Ouroboros.Network.Block (unwrapCBORinCBOR, wrapCBORinCBOR)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.Condense

import qualified Shelley.Spec.Ledger.API as SL
import           Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Shelley.Protocol


type ShelleyTxId c = SL.TxId c

instance TPraosCrypto c => ApplyTx (ShelleyBlock c) where

  data GenTx (ShelleyBlock c) = ShelleyTx !(ShelleyTxId c) !(SL.Tx c)
    deriving (Eq, Generic)
    -- TODO
    deriving (NoUnexpectedThunks) via UseIsNormalForm (GenTx (ShelleyBlock c))

  type ApplyTxErr (ShelleyBlock c) = SL.ApplyTxError c

  -- TODO why does that function live in LedgerState? It looks like a crazy
  -- function.
  txSize (ShelleyTx _ tx) = fromIntegral $ SL.txsize tx

  txInvariant = const True

  applyTx = applyShelleyTx

  -- TODO actual reapplication:
  -- https://github.com/input-output-hk/cardano-ledger-specs/issues/1304
  reapplyTx = applyShelleyTx

mkShelleyTx :: Crypto c => SL.Tx c -> GenTx (ShelleyBlock c)
mkShelleyTx tx = ShelleyTx (SL.txid (SL._body tx)) tx

instance Crypto c => HasTxId (GenTx (ShelleyBlock c)) where

  newtype TxId (GenTx (ShelleyBlock c)) = ShelleyTxId (ShelleyTxId c)
    deriving newtype (Eq, Ord, FromCBOR, ToCBOR)
    deriving (NoUnexpectedThunks) via UseIsNormalForm (TxId (GenTx (ShelleyBlock c)))

  txId (ShelleyTx i _) = ShelleyTxId i

instance Crypto c => HasTxs (ShelleyBlock c) where
  extractTxs =
        map mkShelleyTx
      . txSeqToList
      . SL.bbody
      . shelleyBlockRaw
    where
      txSeqToList :: TxSeq c -> [SL.Tx c]
      txSeqToList (TxSeq s) = toList s

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Crypto c => ToCBOR (GenTx (ShelleyBlock c)) where
  -- No need to encode the 'TxId', it's just a hash of the 'SL.TxBody' inside
  -- 'SL.Tx', so it can be recomputed.
  toCBOR (ShelleyTx _txid tx) = wrapCBORinCBOR toCBOR tx

instance Crypto c => FromCBOR (GenTx (ShelleyBlock c)) where
  fromCBOR = fmap mkShelleyTx $ unwrapCBORinCBOR
    $ (. Full) . runAnnotator <$> fromCBOR

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Crypto c => Condense (GenTx (ShelleyBlock c)) where
  condense (ShelleyTx _ tx ) = show tx

instance Condense (GenTxId (ShelleyBlock c)) where
  condense (ShelleyTxId i) = "txid: " <> show i

instance Crypto c => Show (GenTx (ShelleyBlock c)) where
  show = condense

instance Show (GenTxId (ShelleyBlock c)) where
  show = condense

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

applyShelleyTx
  :: TPraosCrypto c
  => LedgerConfig (ShelleyBlock c)
  -> GenTx (ShelleyBlock c)
  -> TickedLedgerState (ShelleyBlock c)
  -> Except (ApplyTxErr (ShelleyBlock c)) (TickedLedgerState (ShelleyBlock c))
applyShelleyTx globals (ShelleyTx _ tx) (TickedLedgerState slot st) =
    (\state -> TickedLedgerState slot $ st { shelleyState = state }) <$>
       SL.overShelleyState
        (SL.applyTxs globals mempoolEnv (Seq.singleton tx))
        shelleyState
  where
    mempoolEnv = SL.mkMempoolEnv shelleyState slot

    ShelleyLedgerState { shelleyState } = st
