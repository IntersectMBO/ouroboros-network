{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Shelley mempool integration
module Ouroboros.Consensus.Ledger.Shelley.Mempool
  ( ApplyTx (..),
    GenTx (..),
    GenTxId (..)
  )
where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Ledger.Shelley.API
import           Cardano.Prelude (NoUnexpectedThunks (..), UseIsNormalForm (..))
import           Cardano.Slotting.Slot
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Monad.Except (runExcept)
import           Data.Either (fromRight)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Shelley.Block
import           Ouroboros.Consensus.Ledger.Shelley.Ledger
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Util.Condense
import qualified TxData as TxData

type Tx = TxData.Tx TPraosStandardCrypto

type TxId = TxData.TxId TPraosStandardCrypto

instance ApplyTx ShelleyBlock where

  data GenTx ShelleyBlock
    = ShelleyTx !TxId !Tx
    deriving (Eq, Generic)
    deriving (NoUnexpectedThunks) via UseIsNormalForm (GenTx ShelleyBlock)

  newtype GenTxId ShelleyBlock
    = ShelleyTxId TxId
    deriving (Eq, Ord)
    deriving newtype (FromCBOR, ToCBOR)

  type ApplyTxErr ShelleyBlock = ApplyTxError TPraosStandardCrypto

  txId (ShelleyTx i _) = ShelleyTxId i

  txSize (ShelleyTx _ _tx) = 2000 -- TODO

  txInvariant = const True

  applyTx (ShelleyLedgerConfig globals) (ShelleyTx _ tx) (TickedLedgerState ledgerState) =
    (\ss' -> TickedLedgerState $ ledgerState {shelleyLedgerState = ss'})
      <$> overShelleyState
        ( applyTxs
            globals
            (mkMempoolEnv ls slot)
            (Seq.singleton tx)
        )
        ls
    where
      ls = shelleyLedgerState ledgerState
      slot = case ledgerTipSlot ledgerState of
        Origin -> genesisSlotNo
        At s   -> succ s

  -- TODO At present we have no short-cuts to skip things like crypto checks.
  reapplyTx = applyTx

  reapplyTxSameState cfg tx ls =
    fromRight err
      . runExcept
      $ applyTx cfg tx ls
    where
      err = error "reapply TX with same state failed"

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ToCBOR (GenTx ShelleyBlock) where
  toCBOR (ShelleyTx txi tx) = mconcat
    [ CBOR.encodeListLen 2
    , toCBOR txi
    , toCBOR tx
    ]
instance FromCBOR (GenTx ShelleyBlock) where
  fromCBOR = do
    CBOR.decodeListLenOf 2
    ShelleyTx
      <$> fromCBOR
      <*> fromCBOR

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Condense (GenTx ShelleyBlock) where
  condense (ShelleyTx _ tx ) = show tx

instance Condense (GenTxId ShelleyBlock) where
  condense (ShelleyTxId i) = "txid: " <> show i

instance Show (GenTx ShelleyBlock) where
  show = condense

instance Show (GenTxId ShelleyBlock) where
  show = condense
