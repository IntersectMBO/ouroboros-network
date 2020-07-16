{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shelley mempool integration
module Ouroboros.Consensus.Shelley.Ledger.Mempool (
    SL.ApplyTxError (..)
  , GenTx (..)
  , TxId (..)
  , mkShelleyTx
  ) where

import           Control.Monad.Except (Except)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Foldable (toList)
import qualified Data.Sequence as Seq
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (Annotator (..), FromCBOR (..),
                     FullByteString (..), ToCBOR (..))
import           Cardano.Prelude (NoUnexpectedThunks (..), UseIsNormalForm (..))

import           Ouroboros.Network.Block (unwrapCBORinCBOR, wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense

import qualified Shelley.Spec.Ledger.API as SL
import           Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Shelley.Protocol


type ShelleyTxId c = SL.TxId c

data instance GenTx (ShelleyBlock c) = ShelleyTx !(ShelleyTxId c) !(SL.Tx c)
  deriving stock    (Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

instance Typeable c => ShowProxy (GenTx (ShelleyBlock c)) where

type instance ApplyTxErr (ShelleyBlock c) = SL.ApplyTxError c

-- orphaned instance
instance Typeable c => ShowProxy (SL.ApplyTxError c) where

instance TPraosCrypto c => LedgerSupportsMempool (ShelleyBlock c) where
  txInvariant = const True

  applyTx = applyShelleyTx

  -- TODO actual reapplication:
  -- https://github.com/input-output-hk/cardano-ledger-specs/issues/1304
  reapplyTx = applyShelleyTx

  maxTxCapacity (TickedShelleyLedgerState _ _ shelleyState) =
      fromIntegral maxBlockBodySize
    where
      SL.PParams { _maxBBSize = maxBlockBodySize } = getPParams shelleyState

  txInBlockSize (ShelleyTx _ tx) =
    fromIntegral . Lazy.length . SL.txFullBytes $ tx

mkShelleyTx :: Crypto c => SL.Tx c -> GenTx (ShelleyBlock c)
mkShelleyTx tx = ShelleyTx (SL.txid (SL._body tx)) tx

newtype instance TxId (GenTx (ShelleyBlock c)) = ShelleyTxId (ShelleyTxId c)
  deriving newtype (Eq, Ord, FromCBOR, ToCBOR)
  deriving (NoUnexpectedThunks) via UseIsNormalForm (TxId (GenTx (ShelleyBlock c)))

instance Typeable c => ShowProxy (TxId (GenTx (ShelleyBlock c))) where

instance Crypto c => HasTxId (GenTx (ShelleyBlock c)) where
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
  -> SlotNo
  -> GenTx (ShelleyBlock c)
  -> TickedLedgerState (ShelleyBlock c)
  -> Except (ApplyTxErr (ShelleyBlock c)) (TickedLedgerState (ShelleyBlock c))
applyShelleyTx cfg slot (ShelleyTx _ tx) st =
    (\state -> st { tickedShelleyState = state }) <$>
       SL.overShelleyState
        (SL.applyTxs globals mempoolEnv (Seq.singleton tx))
        (tickedShelleyState st)
  where
    globals    = shelleyLedgerGlobals cfg
    mempoolEnv = SL.mkMempoolEnv (tickedShelleyState st) slot
