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

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Byron mempool integration
module Ouroboros.Consensus.Byron.Ledger.Mempool (
    -- * Mempool integration
    GenTx (..)
  , TxId (..)
    -- * Transaction IDs
  , byronIdDlg
  , byronIdProp
  , byronIdTx
  , byronIdVote
    -- * Serialisation
  , decodeByronApplyTxError
  , decodeByronGenTx
  , decodeByronGenTxId
  , encodeByronApplyTxError
  , encodeByronGenTx
  , encodeByronGenTxId
    -- * Low-level API (primarily for testing)
  , fromMempoolPayload
  , toMempoolPayload
    -- * Auxiliary functions
  , countByronGenTxs
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Maybe (maybeToList)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

import           Cardano.Binary (ByteSpan, DecoderError (..), FromCBOR (..),
                     ToCBOR (..), enforceSize, fromCBOR, serialize, slice,
                     toCBOR, unsafeDeserialize)
import           Cardano.Crypto (hashDecoded)
import           Cardano.Prelude (cborError)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.MempoolPayload as CC
import qualified Cardano.Chain.UTxO as Utxo
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.ValidationMode as CC

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Conversions (toByronSlotNo)
import           Ouroboros.Consensus.Byron.Ledger.Ledger
import           Ouroboros.Consensus.Byron.Ledger.Orphans ()
import           Ouroboros.Consensus.Byron.Ledger.Serialisation
                     (byronBlockEncodingOverhead)

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

-- | Generalized transactions in Byron
--
-- This is effectively the same as 'CC.AMempoolPayload' but we cache the
-- transaction ID (a hash).
data instance GenTx ByronBlock
  = ByronTx             !Utxo.TxId                !(Utxo.ATxAux             ByteString)
  | ByronDlg            !Delegation.CertificateId !(Delegation.ACertificate ByteString)
  | ByronUpdateProposal !Update.UpId              !(Update.AProposal        ByteString)
  | ByronUpdateVote     !Update.VoteId            !(Update.AVote            ByteString)
  deriving (Eq, Generic)
  deriving NoThunks via InspectHeapNamed "GenTx ByronBlock" (GenTx ByronBlock)

instance ShowProxy (GenTx ByronBlock) where

type instance ApplyTxErr ByronBlock = CC.ApplyMempoolPayloadErr

-- orphaned instance
instance ShowProxy CC.ApplyMempoolPayloadErr where

instance LedgerSupportsMempool ByronBlock where
  -- Check that the annotation is the canonical encoding. This is currently
  -- enforced by 'decodeByronGenTx', see its docstring for more context.
  txInvariant tx =
      CC.mempoolPayloadRecoverBytes tx' == CC.mempoolPayloadReencode tx'
    where
      tx' = toMempoolPayload tx

  applyTx = applyByronGenTx validationMode
    where
      validationMode = CC.ValidationMode CC.BlockValidation Utxo.TxValidation

  reapplyTx = applyByronGenTx validationMode
    where
      validationMode = CC.ValidationMode CC.NoBlockValidation Utxo.TxValidationNoCrypto

  maxTxCapacity st =
    CC.getMaxBlockSize (tickedByronLedgerState st) - byronBlockEncodingOverhead

  txInBlockSize =
      fromIntegral
    . Strict.length
    . CC.mempoolPayloadRecoverBytes
    . toMempoolPayload

data instance TxId (GenTx ByronBlock)
  = ByronTxId             !Utxo.TxId
  | ByronDlgId            !Delegation.CertificateId
  | ByronUpdateProposalId !Update.UpId
  | ByronUpdateVoteId     !Update.VoteId
  deriving (Eq, Ord)
  deriving NoThunks via InspectHeapNamed "TxId (GenTx ByronBlock)" (TxId (GenTx ByronBlock))

instance ShowProxy (TxId (GenTx ByronBlock)) where

instance HasTxId (GenTx ByronBlock) where
  txId (ByronTx             i _) = ByronTxId             i
  txId (ByronDlg            i _) = ByronDlgId            i
  txId (ByronUpdateProposal i _) = ByronUpdateProposalId i
  txId (ByronUpdateVote     i _) = ByronUpdateVoteId     i

instance HasTxs ByronBlock where
  extractTxs blk = case byronBlockRaw blk of
    -- EBBs don't contain transactions
    CC.ABOBBoundary _ebb    -> []
    CC.ABOBBlock regularBlk -> fromMempoolPayload <$>
        maybeToList proposal <> votes <> dlgs <> txs
      where
        body = CC.blockBody regularBlk

        txs      = CC.MempoolTx             <$> Utxo.aUnTxPayload      (CC.bodyTxPayload     body)
        proposal = CC.MempoolUpdateProposal <$> Update.payloadProposal (CC.bodyUpdatePayload body)
        votes    = CC.MempoolUpdateVote     <$> Update.payloadVotes    (CC.bodyUpdatePayload body)
        dlgs     = CC.MempoolDlg            <$> Delegation.getPayload  (CC.bodyDlgPayload    body)

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
    go (CC.MempoolTx             p) = ByronTx             (byronIdTx   p) p
    go (CC.MempoolDlg            p) = ByronDlg            (byronIdDlg  p) p
    go (CC.MempoolUpdateProposal p) = ByronUpdateProposal (byronIdProp p) p
    go (CC.MempoolUpdateVote     p) = ByronUpdateVote     (byronIdVote p) p

{-------------------------------------------------------------------------------
  Auxiliary: transaction IDs
-------------------------------------------------------------------------------}

-- TODO: move to cardano-ledger-byron (cardano-ledger-byron#581)
byronIdTx :: Utxo.ATxAux ByteString -> Utxo.TxId
byronIdTx = hashDecoded . Utxo.aTaTx

byronIdDlg :: Delegation.ACertificate ByteString -> Delegation.CertificateId
byronIdDlg = Delegation.recoverCertificateId

byronIdProp :: Update.AProposal ByteString -> Update.UpId
byronIdProp = Update.recoverUpId

byronIdVote :: Update.AVote ByteString -> Update.VoteId
byronIdVote = Update.recoverVoteId

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Condense (GenTx ByronBlock) where
  condense = condense . toMempoolPayload

instance Condense (GenTxId ByronBlock) where
  condense (ByronTxId             i) = condense i
  condense (ByronDlgId            i) = condense i
  condense (ByronUpdateProposalId i) = condense i
  condense (ByronUpdateVoteId     i) = condense i

instance Show (GenTx ByronBlock) where
  show = condense

instance Show (GenTxId ByronBlock) where
  show = condense

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

applyByronGenTx :: CC.ValidationMode
                -> LedgerConfig ByronBlock
                -> SlotNo
                -> GenTx ByronBlock
                -> TickedLedgerState ByronBlock
                -> Except (ApplyTxErr ByronBlock) (TickedLedgerState ByronBlock)
applyByronGenTx validationMode cfg slot genTx st =
    (\state -> st {tickedByronLedgerState = state}) <$>
      CC.applyMempoolPayload
        validationMode
        cfg
        (toByronSlotNo slot)
        (toMempoolPayload genTx)
        (tickedByronLedgerState st)

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

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Count all (generalized) transactions in the block
countByronGenTxs :: ByronBlock -> Word64
countByronGenTxs = fromIntegral . length . extractTxs
