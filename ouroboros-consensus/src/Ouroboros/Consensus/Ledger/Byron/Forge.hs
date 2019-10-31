{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Ledger.Byron.Forge (
    forgeBlock
  , forgeBlockOrEBB
    -- * For testing purposes
  , forgeGenesisEBB
  ) where

import           Control.Monad (void)
import           Crypto.Random (MonadRandom)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Foldable (foldl')
import           Data.Reflection (give)

import           Cardano.Binary (Annotated (..), reAnnotate)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Ssc as CC.Ssc
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.UTxO as CC.UTxO
import qualified Cardano.Crypto as Crypto
import           Cardano.Crypto.DSIGN

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.ContainsGenesis
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.WithEBBs

import           Ouroboros.Consensus.Ledger.Byron.Config

forgeBlockOrEBB
  :: forall m.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
     )
  => NodeConfig ByronEBBNodeConfig
  -> SlotNo                          -- ^ Current slot
  -> BlockNo                         -- ^ Current block number
  -> ChainHash ByronBlockOrEBB       -- ^ Previous hash
  -> [GenTx ByronBlockOrEBB]         -- ^ Txs to add in the block
  -> PBftIsLeader PBftCardanoCrypto  -- ^ Leader proof ('IsLeader')
  -> m ByronBlockOrEBB
forgeBlockOrEBB cfg curSlot curNo prevHash txs isLeader = case prevHash of
  GenesisHash -> return $ forgeGenesisEBB cfg curSlot
  BlockHash _ -> forgeBlock cfg curSlot curNo prevHash txs isLeader

forgeGenesisEBB
  :: NodeConfig ByronEBBNodeConfig
  -> SlotNo
  -> ByronBlockOrEBB
forgeGenesisEBB (WithEBBNodeConfig cfg) curSlot =
        mkByronBlockOrEBB pbftEpochSlots
      . CC.Block.ABOBBoundary
      . annotateBoundary protocolMagicId
      $ boundaryBlock
  where
    protocolMagicId = CC.Genesis.configProtocolMagicId (genesisConfig cfg)
    ByronConfig { pbftGenesisHash
                , pbftEpochSlots
                } = pbftExtConfig cfg

    boundaryBlock :: CC.Block.ABoundaryBlock ()
    boundaryBlock =
      CC.Block.ABoundaryBlock {
        CC.Block.boundaryBlockLength = 0 -- Since this is a demo and we
                                         -- ignore the length, set this to 0
      , CC.Block.boundaryHeader
      , CC.Block.boundaryBody        = CC.Block.ABoundaryBody ()
      , CC.Block.boundaryAnnotation  = ()
      }

    boundaryHeader :: CC.Block.ABoundaryHeader ()
    boundaryHeader = CC.Block.mkABoundaryHeader
      (Left pbftGenesisHash)
      epoch
      (CC.Common.ChainDifficulty 0)
      ()
      where
        CC.Slot.EpochNumber epoch =
            CC.Slot.epochNo
          . CC.Slot.fromSlotNumber pbftEpochSlots
          $ coerce curSlot

-- | Internal helper data type for 'forgeBlock' used to accumulate the
-- different kinds of block payloads that can be found in a given collection
-- of Byron 'GenTx's.
--
-- n.b. This data type is not to be exposed from this module.
data BlockPayloads = BlockPayloads
  { bpTxs        :: ![CC.UTxO.TxAux]
  , bpDlgCerts   :: ![CC.Delegation.Certificate]
  , bpUpVotes    :: ![CC.Update.Vote]
  , bpUpProposal :: !(Maybe CC.Update.Proposal)
    -- ^ 'Just' if there is at least one 'CC.Update.Proposal' in a list of
    -- Byron 'GenTx's and 'Nothing' if there are none. It is worth noting that
    -- if we encounter multiple 'CC.Update.Proposal's in a collection of
    -- 'GenTx's, this value will be that of the last 'CC.Update.Proposal'
    -- encountered.
  }

initBlockPayloads :: BlockPayloads
initBlockPayloads = BlockPayloads
  { bpTxs        = []
  , bpDlgCerts   = []
  , bpUpVotes    = []
  , bpUpProposal = Nothing
  }

forgeBlock
  :: forall m.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
     )
  => NodeConfig ByronEBBNodeConfig
  -> SlotNo                            -- ^ Current slot
  -> BlockNo                           -- ^ Current block number
  -> ChainHash ByronBlockOrEBB         -- ^ Previous hash
  -> [GenTx ByronBlockOrEBB]           -- ^ Txs to add in the block
  -> PBftIsLeader PBftCardanoCrypto    -- ^ Leader proof ('IsLeader')
  -> m ByronBlockOrEBB
forgeBlock (WithEBBNodeConfig cfg) curSlot curNo prevHash txs isLeader = do
    ouroborosPayload <-
      give (VerKeyCardanoDSIGN headerGenesisKey) $
      give protocolMagicId $
      forgePBftFields cfg isLeader (reAnnotate $ Annotated toSign ())
    return $ forge ouroborosPayload
  where
    -- TODO: Might be sufficient to add 'ConfigContainsGenesis' constraint.
    ByronConfig
      { pbftGenesisHash
      , pbftEpochSlots
      , pbftProtocolVersion
      , pbftSoftwareVersion
      , pbftProtocolMagic
      } = pbftExtConfig cfg

    protocolMagicId = CC.Genesis.configProtocolMagicId (genesisConfig cfg)

    blockPayloads :: BlockPayloads
    blockPayloads = foldl' extendBlockPayloads initBlockPayloads txs

    txPayload :: CC.UTxO.TxPayload
    txPayload = CC.UTxO.mkTxPayload (bpTxs blockPayloads)

    dlgPayload :: CC.Delegation.Payload
    dlgPayload = CC.Delegation.unsafePayload (bpDlgCerts blockPayloads)

    updatePayload :: CC.Update.Payload
    updatePayload = CC.Update.payload (bpUpProposal blockPayloads)
                                      (bpUpVotes blockPayloads)

    extendBlockPayloads :: BlockPayloads
                        -> GenTx ByronBlockOrEBB
                        -> BlockPayloads
    extendBlockPayloads bp@BlockPayloads{bpTxs, bpDlgCerts, bpUpVotes} genTx =
      -- TODO: We should try to use 'recoverProof' (and other variants of
      -- 'recoverBytes') here as opposed to throwing away the serializations
      -- (the 'ByteString' annotations) with 'void' as we're currently doing.
      case genTx of
        ByronTx             _ tx   -> bp { bpTxs        = void tx : bpTxs }
        ByronDlg            _ cert -> bp { bpDlgCerts   = void cert : bpDlgCerts }
        -- TODO: We should throw an error if we encounter multiple
        -- 'ByronUpdateProposal's (i.e. if 'bpUpProposal' 'isJust').
        -- This is because we should only be provided with a maximum of one
        -- 'ByronUpdateProposal' to include in a block payload.
        ByronUpdateProposal _ prop -> bp { bpUpProposal = Just (void prop) }
        ByronUpdateVote     _ vote -> bp { bpUpVotes    = void vote : bpUpVotes }

    body :: CC.Block.Body
    body = CC.Block.ABody {
          CC.Block.bodyTxPayload     = txPayload
        , CC.Block.bodySscPayload    = CC.Ssc.SscPayload
        , CC.Block.bodyDlgPayload    = dlgPayload
        , CC.Block.bodyUpdatePayload = updatePayload
        }

    proof :: CC.Block.Proof
    proof = CC.Block.mkProof body

    prevHeaderHash :: CC.Block.HeaderHash
    prevHeaderHash = case prevHash of
      GenesisHash             -> CC.Block.genesisHeaderHash pbftGenesisHash
      BlockHash (ByronHash h) -> h

    epochAndSlotCount :: CC.Slot.EpochAndSlotCount
    epochAndSlotCount = CC.Slot.fromSlotNumber pbftEpochSlots $ coerce curSlot

    toSign :: CC.Block.ToSign
    toSign = CC.Block.ToSign {
          CC.Block.tsHeaderHash      = prevHeaderHash
        , CC.Block.tsSlot            = epochAndSlotCount
        , CC.Block.tsDifficulty      = coerce curNo
        , CC.Block.tsBodyProof       = proof
        , CC.Block.tsProtocolVersion = pbftProtocolVersion
        , CC.Block.tsSoftwareVersion = pbftSoftwareVersion
        }

    headerGenesisKey :: Crypto.VerificationKey
    VerKeyCardanoDSIGN headerGenesisKey = give protocolMagicId $
      dlgCertGenVerKey $ pbftDlgCert isLeader

    dlgCertificate :: CC.Delegation.Certificate
    dlgCertificate = pbftDlgCert isLeader

    forge :: PBftFields PBftCardanoCrypto (Annotated CC.Block.ToSign ByteString)
          -> ByronBlockOrEBB
    forge ouroborosPayload =
       annotateByronBlock pbftEpochSlots block
      where
        block :: CC.Block.Block
        block = CC.Block.ABlock {
              CC.Block.blockHeader     = header
            , CC.Block.blockBody       = body
            , CC.Block.blockAnnotation = ()
            }

        headerSignature :: CC.Block.BlockSignature
        headerSignature = CC.Block.ABlockSignature dlgCertificate (coerce sig)
          where
            sig :: Crypto.Signature CC.Block.ToSign
            SignedDSIGN (SigCardanoDSIGN sig) = pbftSignature ouroborosPayload

        header :: CC.Block.Header
        header = CC.Block.AHeader {
              CC.Block.aHeaderProtocolMagicId = ann (Crypto.getProtocolMagicId pbftProtocolMagic)
            , CC.Block.aHeaderPrevHash        = ann prevHeaderHash
            , CC.Block.aHeaderSlot            = ann (coerce curSlot)
            , CC.Block.aHeaderDifficulty      = ann (coerce curNo)
            , CC.Block.headerProtocolVersion  = pbftProtocolVersion
            , CC.Block.headerSoftwareVersion  = pbftSoftwareVersion
            , CC.Block.aHeaderProof           = ann proof
            , CC.Block.headerGenesisKey       = headerGenesisKey
            , CC.Block.headerSignature        = headerSignature
            , CC.Block.headerAnnotation       = ()
            , CC.Block.headerExtraAnnotation  = ()
            }

        ann :: b -> Annotated b ()
        ann b = Annotated b ()
