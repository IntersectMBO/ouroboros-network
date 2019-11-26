{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Byron mempool integration
module Ouroboros.Consensus.Ledger.Byron.Mempool (
    -- * Mempool integration
    GenTx(..)
  , GenTxId(..)
    -- * Serialisation
  , encodeByronGenTx
  , decodeByronGenTx
  , encodeByronGenTxId
  , decodeByronGenTxId
  , encodeByronApplyTxError
  , decodeByronApplyTxError
    -- * Low-level API (primarily for testing)
  , toMempoolPayload
  , fromMempoolPayload
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Binary (ByteSpan, DecoderError (..), FromCBOR (..),
                     ToCBOR (..), enforceSize, fromCBOR, serialize, slice,
                     toCBOR, unsafeDeserialize)
import           Cardano.Crypto (hashDecoded)
import           Cardano.Prelude (NoUnexpectedThunks (..), UseIsNormalForm (..),
                     cborError)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.MempoolPayload as CC
import qualified Cardano.Chain.Update.Proposal as Update
import qualified Cardano.Chain.Update.Vote as Update
import qualified Cardano.Chain.UTxO as Utxo
import qualified Cardano.Chain.ValidationMode as CC

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron.Aux
import           Ouroboros.Consensus.Ledger.Byron.Block
import           Ouroboros.Consensus.Ledger.Byron.Ledger
import           Ouroboros.Consensus.Ledger.Byron.Orphans ()
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

instance ApplyTx ByronBlock where
  -- | Generalized transactions in Byron
  --
  -- This is effectively the same as 'CC.AMempoolPayload' but we cache the
  -- transaction ID (a hash).
  data GenTx ByronBlock
    = ByronTx             !Utxo.TxId                !(Utxo.ATxAux             ByteString)
    | ByronDlg            !Delegation.CertificateId !(Delegation.ACertificate ByteString)
    | ByronUpdateProposal !Update.UpId              !(Update.AProposal        ByteString)
    | ByronUpdateVote     !Update.VoteId            !(Update.AVote            ByteString)
    deriving (Eq, Generic)
    deriving NoUnexpectedThunks via UseIsNormalForm (GenTx ByronBlock)

  data GenTxId ByronBlock
    = ByronTxId             !Utxo.TxId
    | ByronDlgId            !Delegation.CertificateId
    | ByronUpdateProposalId !Update.UpId
    | ByronUpdateVoteId     !Update.VoteId
    deriving (Eq, Ord)

  txId (ByronTx             i _) = ByronTxId             i
  txId (ByronDlg            i _) = ByronDlgId            i
  txId (ByronUpdateProposal i _) = ByronUpdateProposalId i
  txId (ByronUpdateVote     i _) = ByronUpdateVoteId     i

  txSize tx =
        1 {- encodeListLen -}
      + 1 {- tag -}
      + (fromIntegral . Strict.length $ mempoolPayloadRecoverBytes tx')
    where
      tx' = toMempoolPayload tx

  -- Check that the annotation is the canonical encoding. This is currently
  -- enforced by 'decodeByronGenTx', see its docstring for more context.
  txInvariant tx =
      mempoolPayloadRecoverBytes tx' == mempoolPayloadReencode tx'
    where
      tx' = toMempoolPayload tx

  type ApplyTxErr ByronBlock = ApplyMempoolPayloadErr

  applyTx = applyByronGenTx validationMode
    where
      validationMode = CC.ValidationMode CC.BlockValidation Utxo.TxValidation

  reapplyTx = applyByronGenTx validationMode
    where
      validationMode = CC.ValidationMode CC.NoBlockValidation Utxo.TxValidationNoCrypto

  reapplyTxSameState cfg tx st =
      validationErrorImpossible $
        applyByronGenTx validationMode cfg tx st
    where
      validationMode = CC.ValidationMode CC.NoBlockValidation Utxo.NoTxValidation

{-------------------------------------------------------------------------------
  Conversion to and from 'AMempoolPayload'
-------------------------------------------------------------------------------}

toMempoolPayload :: GenTx ByronBlock -> CC.AMempoolPayload ByteString
toMempoolPayload = go
  where
    -- Just extract the payload @p@
    go :: GenTx ByronBlock -> CC.AMempoolPayload ByteString
    go (ByronTx             _ p) = CC.MempoolTx             p
    go (ByronDlg            _ p) = CC.MempoolDlg            p
    go (ByronUpdateProposal _ p) = CC.MempoolUpdateProposal p
    go (ByronUpdateVote     _ p) = CC.MempoolUpdateVote     p

fromMempoolPayload :: CC.AMempoolPayload ByteString -> GenTx ByronBlock
fromMempoolPayload = go
  where
    -- Bundle the payload @p@ with its ID
    go :: CC.AMempoolPayload ByteString -> GenTx ByronBlock
    go (CC.MempoolTx             p) = ByronTx             (idTx   p) p
    go (CC.MempoolDlg            p) = ByronDlg            (idDlg  p) p
    go (CC.MempoolUpdateProposal p) = ByronUpdateProposal (idProp p) p
    go (CC.MempoolUpdateVote     p) = ByronUpdateVote     (idVote p) p

    idTx   = hashDecoded . Utxo.aTaTx -- TODO (cardano-ledger#581)
    idDlg  = Delegation.recoverCertificateId
    idProp = Update.recoverUpId
    idVote = Update.recoverVoteId

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Condense (GenTx ByronBlock) where
  condense = condense . toMempoolPayload

instance Condense (GenTxId ByronBlock) where
  condense (ByronTxId             i) = "txid: "             <> condense i
  condense (ByronDlgId            i) = "dlgid: "            <> condense i
  condense (ByronUpdateProposalId i) = "updateproposalid: " <> condense i
  condense (ByronUpdateVoteId     i) = "updatevoteid: "     <> condense i

instance Show (GenTx ByronBlock) where
  show = condense

instance Show (GenTxId ByronBlock) where
  show = condense

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

applyByronGenTx :: CC.ValidationMode
                -> LedgerConfig ByronBlock
                -> GenTx ByronBlock
                -> TickedLedgerState ByronBlock
                -> Except (ApplyTxErr ByronBlock) (TickedLedgerState ByronBlock)
applyByronGenTx validationMode cfg genTx (TickedLedgerState st) =
    (\state -> TickedLedgerState $ st {byronLedgerState = state}) <$>
      applyMempoolPayload
        validationMode
        (unByronLedgerConfig cfg)
        (toMempoolPayload genTx)
        (byronLedgerState st)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronGenTx :: GenTx ByronBlock -> Encoding
encodeByronGenTx genTx = toCBOR (toMempoolPayload genTx)

-- | The 'ByteString' annotation will be the canonical encoding.
--
-- While the new implementation does not care about canonical encodings, the
-- old one does. When a generalised transaction arrives that is not in its
-- canonical encoding (only the 'CC.UTxO.ATxAux' of the 'ByronTx' can be
-- produced by nodes that are not under our control), the old implementation
-- will reject it. Therefore, we need to reject them too. See #905.
--
-- We use the ledger to check for canonical encodings: the ledger will check
-- whether the signed hash of the transaction (in the case of a
-- 'CC.UTxO.ATxAux', the transaction witness) matches the annotated
-- bytestring. Is therefore __important__ that the annotated bytestring be the
-- /canonical/ encoding, not the /original, possibly non-canonical/ encoding.
decodeByronGenTx :: Decoder s (GenTx ByronBlock)
decodeByronGenTx = fromMempoolPayload . canonicalise <$> fromCBOR
  where
    -- Fill in the 'ByteString' annotation with a canonical encoding of the
    -- 'GenTx'. We must reserialise the deserialised 'GenTx' to be sure we
    -- have the canonical one. We don't have access to the original
    -- 'ByteString' anyway, so having to reserialise here gives us a
    -- 'ByteString' we can use.
    canonicalise :: CC.AMempoolPayload ByteSpan
                 -> CC.AMempoolPayload ByteString
    canonicalise mp = Lazy.toStrict . slice canonicalBytes <$> mp'
      where
        canonicalBytes = serialize (void mp)
        -- 'unsafeDeserialize' cannot fail, since we just 'serialize'd it.
        -- Note that we cannot reuse @mp@, as its 'ByteSpan' might differ from
        -- the canonical encoding's 'ByteSpan'.
        mp'            = unsafeDeserialize canonicalBytes

encodeByronGenTxId :: GenTxId ByronBlock -> Encoding
encodeByronGenTxId genTxId = mconcat [
      CBOR.encodeListLen 2
    , case genTxId of
        ByronTxId             i -> toCBOR (0 :: Word8) <> toCBOR i
        ByronDlgId            i -> toCBOR (1 :: Word8) <> toCBOR i
        ByronUpdateProposalId i -> toCBOR (2 :: Word8) <> toCBOR i
        ByronUpdateVoteId     i -> toCBOR (3 :: Word8) <> toCBOR i
    ]

decodeByronGenTxId :: Decoder s (GenTxId ByronBlock)
decodeByronGenTxId = do
    enforceSize "GenTxId (ByronBlock cfg)" 2
    CBOR.decodeWord8 >>= \case
      0   -> ByronTxId             <$> fromCBOR
      1   -> ByronDlgId            <$> fromCBOR
      2   -> ByronUpdateProposalId <$> fromCBOR
      3   -> ByronUpdateVoteId     <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "GenTxId (ByronBlock cfg)" tag

encodeByronApplyTxError :: ApplyTxErr ByronBlock -> Encoding
encodeByronApplyTxError = toCBOR

decodeByronApplyTxError :: Decoder s (ApplyTxErr ByronBlock)
decodeByronApplyTxError = fromCBOR
