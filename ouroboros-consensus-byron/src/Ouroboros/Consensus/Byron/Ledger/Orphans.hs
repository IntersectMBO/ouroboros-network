{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Byron.Ledger.Orphans (

  ) where

import           Codec.Serialise (Serialise, decode, encode)
import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Data.Coerce
import           Data.Text (unpack)
import           Formatting
import           NoThunks.Class (InspectHeap (..), NoThunks)

import qualified Cardano.Binary
import           Cardano.Crypto (shortHashF)
import qualified Cardano.Crypto

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Delegation as CC
import qualified Cardano.Chain.MempoolPayload as CC
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Chain.Update as CC

import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Serialise
-------------------------------------------------------------------------------}

instance Serialise CC.ChainValidationState where
  encode = Cardano.Binary.toCBOR
  decode = Cardano.Binary.fromCBOR

instance Serialise CC.KeyHash where
  encode = Cardano.Binary.toCBOR
  decode = Cardano.Binary.fromCBOR

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Condense CC.HeaderHash where
  condense = formatToString CC.headerHashF

instance Condense (CC.ABlock ByteString) where
  condense = unpack
           . sformat build
           . CC.txpTxs
           . CC.bodyTxPayload
           . CC.blockBody

instance Condense (CC.AHeader ByteString) where
  condense hdr = mconcat [
        "( hash: "         <> unpack condensedHash
      , ", previousHash: " <> unpack condensedPrevHash
      , ", slot: "         <> unpack condensedSlot
      , ", issuer: "       <> condense issuer
      , ", delegate: "     <> condense delegate
      , ")"
      ]
    where
      psigCert = CC.delegationCertificate $ CC.headerSignature hdr
      issuer   = CC.issuerVK   psigCert
      delegate = CC.delegateVK psigCert
      hdrHash  = CC.headerHashAnnotated hdr

      condensedHash     = sformat CC.headerHashF $ hdrHash
      condensedPrevHash = sformat CC.headerHashF $ CC.headerPrevHash hdr
      condensedSlot     = sformat build $
                            Cardano.Binary.unAnnotated (CC.aHeaderSlot hdr)

instance Condense (CC.ABoundaryBlock ByteString) where
  condense = condense . CC.boundaryHeader

instance Condense (CC.ABlockOrBoundary ByteString) where
  condense (CC.ABOBBlock blk) = mconcat [
        "( header: " <> condense (CC.blockHeader blk)
      , ", body: "   <> condense blk
      , ")"
      ]
  condense (CC.ABOBBoundary ebb) =
      condense ebb

instance Condense (CC.ABoundaryHeader ByteString) where
  condense hdr = mconcat [
        "( ebb: "          <> condense (CC.boundaryEpoch hdr)
      , ", hash: "         <> condensedHash
      , ", previousHash: " <> condensedPrevHash
      , ")"
      ]
    where
      condensedHash =
            unpack
          . sformat CC.headerHashF
          . coerce
          . Cardano.Crypto.hashDecoded . fmap CC.wrapBoundaryBytes
          $ hdr

      condensedPrevHash =
          unpack $ case CC.boundaryPrevHash hdr of
            Left _  -> "Genesis"
            Right h -> sformat CC.headerHashF h

instance Condense CC.TxId where
  condense hash = "txid:" <> unpack (sformat shortHashF hash)

instance Condense CC.UpId where
  condense hash = "upid:" <> unpack (sformat shortHashF hash)

instance Condense CC.CertificateId where
  condense hash = "certificateid: " <> unpack (sformat shortHashF hash)

instance Condense CC.VoteId where
  condense hash = "voteid: " <> unpack (sformat shortHashF hash)

instance Condense (CC.AMempoolPayload a) where
    condense (CC.MempoolTx tx) =
      "tx: " <> unpack (sformat build (void tx))
    condense (CC.MempoolDlg cert) =
      "dlg: " <> unpack (sformat build (void cert))
    condense (CC.MempoolUpdateProposal p) =
      "updateproposal: " <> unpack (sformat build (void p))
    condense (CC.MempoolUpdateVote vote) =
      "updatevote: " <> unpack (sformat build (void vote))

instance Condense Cardano.Crypto.VerificationKey where
  condense = unpack . sformat build

{-------------------------------------------------------------------------------
  NoThunks
-------------------------------------------------------------------------------}

-- TODO <https://github.com/input-output-hk/cardano-ledger-byron/issues/685>
--
-- Cardano.Chain.Delegation.Validation.Registration.TooLarge is not exported,
-- but occurs somewhere in CC.ChainValidationError, so we use
-- 'InspectHeap' instead of deriving one using Generics.
deriving via InspectHeap CC.ChainValidationError
  instance NoThunks CC.ChainValidationError
