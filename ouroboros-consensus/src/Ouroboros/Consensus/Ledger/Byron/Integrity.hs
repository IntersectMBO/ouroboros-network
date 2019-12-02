{-# LANGUAGE NamedFieldPuns #-}
module Ouroboros.Consensus.Ledger.Byron.Integrity
  ( verifyBlockMatchesHeader
  , verifyHeaderSignature
  , verifyHeaderIntegrity
  , verifyBlockIntegrity
  ) where

import           Data.Either (isRight)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Crypto.DSIGN.Class as CC.Crypto

import           Ouroboros.Consensus.Block (getHeader)

import           Ouroboros.Consensus.Ledger.Byron.Auxiliary
import           Ouroboros.Consensus.Ledger.Byron.Block
import           Ouroboros.Consensus.Ledger.Byron.ContainsGenesis
import           Ouroboros.Consensus.Ledger.Byron.PBFT
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT

-- | Check if a block matches its header
--
-- Note that we cannot check this for an EBB, as the EBB header doesn't store
-- a hash of the EBB body.
verifyBlockMatchesHeader :: Header ByronBlock -> ByronBlock -> Bool
verifyBlockMatchesHeader hdr blk =
    abobMatchesBody (byronHeaderRaw hdr) (byronBlockRaw blk)

-- | Verify whether a header matches its signature.
--
-- Note that we cannot check this for an EBB, as an EBB contains no signature.
-- This function will always return 'True' for an EBB.
verifyHeaderSignature
  :: NodeConfig ByronConsensusProtocol -> Header ByronBlock -> Bool
verifyHeaderSignature cfg@PBftNodeConfig { pbftExtConfig } hdr =
    case headerPBftFields pbftExtConfig hdr of
      -- EBB, no signature to check
      Nothing -> True
      Just (PBftFields { pbftIssuer, pbftGenKey, pbftSignature }, signed) ->
        isRight $ CC.Crypto.verifySignedDSIGN
          (constructContextDSIGN cfg pbftExtConfig pbftGenKey)
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
verifyHeaderIntegrity
  :: NodeConfig ByronConsensusProtocol -> Header ByronBlock -> Bool
verifyHeaderIntegrity cfg hdr =
    verifyHeaderSignature cfg hdr &&
    -- @CC.headerProtocolMagicId@ is the only field of a regular header that
    -- is not signed, so check it manually.
    case byronHeaderRaw hdr of
        ABOBBlockHdr h    -> CC.headerProtocolMagicId h == protocolMagicId
        -- EBB, we can't check it
        ABOBBoundaryHdr _ -> True
  where
    protocolMagicId = CC.Genesis.configProtocolMagicId (getGenesisConfig cfg)

-- | Verifies whether the block is not corrupted by checking its signature and
-- witnesses.
--
-- This function will always return 'True' for an EBB, as we cannot check
-- anything for an EBB.
verifyBlockIntegrity :: NodeConfig ByronConsensusProtocol -> ByronBlock -> Bool
verifyBlockIntegrity cfg blk =
    verifyHeaderIntegrity    cfg hdr &&
    verifyBlockMatchesHeader     hdr blk
  where
    hdr = getHeader blk
