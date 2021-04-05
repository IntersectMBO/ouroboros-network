{-# LANGUAGE NamedFieldPuns #-}
module Ouroboros.Consensus.Byron.Ledger.Integrity (
    verifyBlockIntegrity
  , verifyHeaderIntegrity
  , verifyHeaderSignature
  ) where

import           Data.Either (isRight)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Crypto.DSIGN.Class as CC.Crypto

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.PBFT

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.PBFT ()

-- | Verify whether a header matches its signature.
--
-- Note that we cannot check this for an EBB, as an EBB contains no signature.
-- This function will always return 'True' for an EBB.
verifyHeaderSignature :: BlockConfig ByronBlock -> Header ByronBlock -> Bool
verifyHeaderSignature cfg hdr =
    case validateView cfg hdr of
      PBftValidateBoundary{} ->
        -- EBB, no signature to check
        True
      PBftValidateRegular fields signed contextDSIGN ->
        let PBftFields { pbftIssuer, pbftSignature } = fields
        in isRight $ CC.Crypto.verifySignedDSIGN
             contextDSIGN
             pbftIssuer
             signed
             pbftSignature

-- | Verify whether a header is not corrupted.
--
-- The difference with 'verifyHeaderSignature' is that this function also
-- checks the integrity of the 'CC.headerProtocolMagicId' field, which is the
-- only field of a regular header that is not signed.
--
-- Note that we cannot check this for an EBB, as an EBB contains no signature.
-- This function will always return 'True' for an EBB.
verifyHeaderIntegrity :: BlockConfig ByronBlock -> Header ByronBlock -> Bool
verifyHeaderIntegrity cfg hdr =
    verifyHeaderSignature cfg hdr &&
    -- @CC.headerProtocolMagicId@ is the only field of a regular header that
    -- is not signed, so check it manually.
    case byronHeaderRaw hdr of
        CC.ABOBBlockHdr h    -> CC.headerProtocolMagicId h == protocolMagicId
        -- EBB, we can't check it
        CC.ABOBBoundaryHdr _ -> True
  where
    protocolMagicId = byronProtocolMagicId cfg

-- | Verifies whether the block is not corrupted by checking its signature and
-- witnesses.
--
-- This function will always return 'True' for an EBB, as we cannot check
-- anything for an EBB.
verifyBlockIntegrity :: BlockConfig ByronBlock -> ByronBlock -> Bool
verifyBlockIntegrity cfg blk =
    verifyHeaderIntegrity cfg hdr &&
    blockMatchesHeader        hdr blk
  where
    hdr = getHeader blk
