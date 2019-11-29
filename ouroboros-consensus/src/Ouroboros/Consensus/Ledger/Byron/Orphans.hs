{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Ledger.Byron.Orphans () where

import           Codec.Serialise (Serialise, decode, encode)
import           Data.Text (unpack)
import           Formatting

import qualified Cardano.Binary
import           Cardano.Crypto (shortHashF)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Delegation as CC
import qualified Cardano.Chain.MempoolPayload as CC
import qualified Cardano.Chain.Update as CC
import qualified Cardano.Chain.UTxO as CC

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

instance Condense CC.Block where
  condense = unpack
           . sformat build
           . CC.txpTxs
           . CC.bodyTxPayload
           . CC.blockBody

instance Condense CC.Header where
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
      hdrHash  = CC.hashHeader hdr

      condensedHash     = sformat CC.headerHashF $ hdrHash
      condensedPrevHash = sformat CC.headerHashF $ CC.headerPrevHash hdr
      condensedSlot     = sformat build $ CC.headerSlot hdr

instance Condense CC.BoundaryBlock where
  condense = condense . CC.boundaryHeader

instance Condense CC.BlockOrBoundary where
  condense (CC.BOBBlock blk) = mconcat [
        "( header: " <> condense (CC.blockHeader blk)
      , ", body: "   <> condense blk
      , ")"
      ]
  condense (CC.BOBBoundary ebb) =
      condense ebb

instance Condense CC.BoundaryHeader where
  condense hdr = mconcat [
        "( ebb: true"
      , ", hash: "         <> condensedHash
      , ", previousHash: " <> condensedPrevHash
      , ")"
      ]
    where
      condensedHash =
            unpack
          . sformat CC.headerHashF
          . CC.boundaryHeaderHashAnnotated
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

instance Condense CC.MempoolPayload where
    condense (CC.MempoolTx tx) =
      "tx: " <> unpack (sformat build tx)
    condense (CC.MempoolDlg cert) =
      "dlg: " <> unpack (sformat build cert)
    condense (CC.MempoolUpdateProposal p) =
      "updateproposal: " <> unpack (sformat build p)
    condense (CC.MempoolUpdateVote vote) =
      "updatevote: " <> unpack (sformat build vote)
