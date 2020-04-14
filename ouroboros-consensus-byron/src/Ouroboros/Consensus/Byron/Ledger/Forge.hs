{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.Byron.Ledger.Forge (
    forgeByronBlock
  , forgeRegularBlock
  , byronBlockEncodingOverhead
    -- * For testing purposes
  , forgeEBB
  ) where

import           Control.Monad (void)
import           Crypto.Random (MonadRandom)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Word (Word32)
import           GHC.Stack

import           Cardano.Binary (Annotated (..), reAnnotate)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Byron.API as CC
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

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.PBFT

import           Ouroboros.Consensus.Byron.Crypto.DSIGN
import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.Mempool
import           Ouroboros.Consensus.Byron.Ledger.PBFT
import           Ouroboros.Consensus.Byron.Protocol

forgeByronBlock
  :: forall m. (MonadRandom m, HasCallStack)
  => TopLevelConfig ByronBlock
  -> Update m (NodeState ByronBlock)
  -> BlockNo                         -- ^ Current block number
  -> TickedLedgerState ByronBlock    -- ^ Current ledger
  -> [GenTx ByronBlock]              -- ^ Txs to add in the block
  -> PBftIsLeader PBftByronCrypto  -- ^ Leader proof ('IsLeader')
  -> m ByronBlock
forgeByronBlock = forgeRegularBlock

forgeEBB
  :: TopLevelConfig ByronBlock
  -> SlotNo                          -- ^ Current slot
  -> BlockNo                         -- ^ Current block number
  -> ChainHash ByronBlock            -- ^ Previous hash
  -> ByronBlock
forgeEBB cfg curSlot curNo prevHash =
        mkByronBlock (byronEpochSlots byronConfig)
      . CC.Block.ABOBBoundary
      . CC.reAnnotateBoundary (byronProtocolMagicId byronConfig)
      $ boundaryBlock
  where
    byronConfig :: BlockConfig ByronBlock
    byronConfig = configBlock cfg

    prevHeaderHash :: Either CC.Genesis.GenesisHash CC.Block.HeaderHash
    prevHeaderHash = case prevHash of
      GenesisHash             -> Left  (byronGenesisHash byronConfig)
      BlockHash (ByronHash h) -> Right h

    boundaryBlock :: CC.Block.ABoundaryBlock ()
    boundaryBlock =
      CC.Block.ABoundaryBlock {
        CC.Block.boundaryBlockLength = 0 -- Used only in testing anyway
      , CC.Block.boundaryHeader
      , CC.Block.boundaryBody        = CC.Block.ABoundaryBody ()
      , CC.Block.boundaryAnnotation  = ()
      }

    boundaryHeader :: CC.Block.ABoundaryHeader ()
    boundaryHeader = CC.Block.mkABoundaryHeader
      prevHeaderHash
      epoch
      (coerce curNo)
      ()
      where
        CC.Slot.EpochNumber epoch =
            CC.Slot.epochNo
          . CC.Slot.fromSlotNumber (byronEpochSlots byronConfig)
          $ coerce curSlot

-- | Internal helper data type for 'forgeRegularBlock' used to accumulate the
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

forgeRegularBlock
  :: forall m. (MonadRandom m, HasCallStack)
  => TopLevelConfig ByronBlock
  -> Update m (NodeState ByronBlock)
  -> BlockNo                           -- ^ Current block number
  -> TickedLedgerState ByronBlock      -- ^ Current ledger
  -> [GenTx ByronBlock]                -- ^ Txs to add in the block
  -> PBftIsLeader PBftByronCrypto    -- ^ Leader proof ('IsLeader')
  -> m ByronBlock
forgeRegularBlock cfg _updateState curNo tickedLedger txs isLeader = do
    ouroborosPayload <-
      forgePBftFields
        (mkByronContextDSIGN (configBlock cfg))
        isLeader
        (reAnnotate $ Annotated toSign ())
    return $ forge ouroborosPayload
  where
    curSlot :: SlotNo
    curSlot = tickedSlotNo tickedLedger

    byronConfig :: BlockConfig ByronBlock
    byronConfig = configBlock cfg

    blockPayloads :: BlockPayloads
    blockPayloads = foldr extendBlockPayloads initBlockPayloads txs

    txPayload :: CC.UTxO.TxPayload
    txPayload = CC.UTxO.mkTxPayload (bpTxs blockPayloads)

    dlgPayload :: CC.Delegation.Payload
    dlgPayload = CC.Delegation.unsafePayload (bpDlgCerts blockPayloads)

    updatePayload :: CC.Update.Payload
    updatePayload = CC.Update.payload (bpUpProposal blockPayloads)
                                      (bpUpVotes blockPayloads)

    extendBlockPayloads :: GenTx ByronBlock
                        -> BlockPayloads
                        -> BlockPayloads
    extendBlockPayloads genTx bp@BlockPayloads{bpTxs, bpDlgCerts, bpUpVotes} =
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
    prevHeaderHash = case ledgerTipHash (tickedLedgerState tickedLedger) of
      GenesisHash             -> error
        "the first block on the Byron chain must be an EBB"
      BlockHash (ByronHash h) -> h

    epochAndSlotCount :: CC.Slot.EpochAndSlotCount
    epochAndSlotCount = CC.Slot.fromSlotNumber (byronEpochSlots byronConfig) $
                          coerce curSlot

    toSign :: CC.Block.ToSign
    toSign = CC.Block.ToSign {
          CC.Block.tsHeaderHash      = prevHeaderHash
        , CC.Block.tsSlot            = epochAndSlotCount
        , CC.Block.tsDifficulty      = coerce curNo
        , CC.Block.tsBodyProof       = proof
        , CC.Block.tsProtocolVersion = byronProtocolVersion byronConfig
        , CC.Block.tsSoftwareVersion = byronSoftwareVersion byronConfig
        }

    headerGenesisKey :: Crypto.VerificationKey
    VerKeyByronDSIGN headerGenesisKey = dlgCertGenVerKey $ pbftDlgCert isLeader

    dlgCertificate :: CC.Delegation.Certificate
    dlgCertificate = pbftDlgCert isLeader

    forge :: PBftFields PBftByronCrypto (Annotated CC.Block.ToSign ByteString)
          -> ByronBlock
    forge ouroborosPayload =
       annotateByronBlock (byronEpochSlots byronConfig) block
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
            SignedDSIGN (SigByronDSIGN sig) = pbftSignature ouroborosPayload

        header :: CC.Block.Header
        header = CC.Block.AHeader {
              CC.Block.aHeaderProtocolMagicId = ann (Crypto.getProtocolMagicId (byronProtocolMagic byronConfig))
            , CC.Block.aHeaderPrevHash        = ann prevHeaderHash
            , CC.Block.aHeaderSlot            = ann (coerce curSlot)
            , CC.Block.aHeaderDifficulty      = ann (coerce curNo)
            , CC.Block.headerProtocolVersion  = byronProtocolVersion byronConfig
            , CC.Block.headerSoftwareVersion  = byronSoftwareVersion byronConfig
            , CC.Block.aHeaderProof           = ann proof
            , CC.Block.headerGenesisKey       = headerGenesisKey
            , CC.Block.headerSignature        = headerSignature
            , CC.Block.headerAnnotation       = ()
            , CC.Block.headerExtraAnnotation  = ()
            }

        ann :: b -> Annotated b ()
        ann b = Annotated b ()

-- | The Byron block encoding overhead size in bytes.
--
-- This encompasses the overhead in bytes for everything that is encoded
-- within a Byron block, excluding the actual generalized transactions
-- (transactions, delegation certificates, update votes, and update
-- proposals).
byronBlockEncodingOverhead :: Word32
byronBlockEncodingOverhead =
    blockHeaderOverhead + blockBodyOverhead + safetyMargin
  where
    -- The maximum block header size.
    blockHeaderOverhead = 650

    -- The block body overhead excluding the actual generalized transactions.
    blockBodyOverhead = 1 {- ABody: encodeListLen 4 -}
                      + 2 {- TxPayload: list -}
                      + 1 {- SscPayload: encodeListLen 2 -}
                      + 1 {- SscPayload: Word8 -}
                      + 1 {- SscPayload: mempty :: Set () -}
                      + 2 {- Delegation.Payload: list -}
                      + 1 {- Update.Payload: encodeListLen 2 -}
                      + 1 {- Update.Payload: Maybe AProposal -}
                      + 2 {- Update.Payload: list of AVote -}

    -- Just for safety.
    safetyMargin = 1024
