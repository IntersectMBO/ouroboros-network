{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Demo.Ledger.Byron.Forge (
    forgeBlock
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad (void)
import           Crypto.Random (MonadRandom)
import           Data.Coerce (coerce)
import           Data.Either (lefts, rights)
import           Data.Foldable (find)
import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import           Data.Reflection (Given (..))
import           Debug.Trace (trace)

import           Cardano.Binary (Annotated (..), toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Ssc as CC.Ssc
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.UTxO as CC.UTxO
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Update

import           Ouroboros.Consensus.Demo.Ledger.Byron.Config

forgeBlock
  :: forall m cfg.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
       -- TODO: This 'Given' constraint should not be needed (present in config)
     , Given Crypto.ProtocolMagicId
     )
  => NodeConfig ByronExtNodeConfig
  -> ExtLedgerState (ByronBlock cfg)
                                  -- ^ Current ledger state
  -> SlotNo                       -- ^ Current slot
  -> BlockNo                      -- ^ Current block number
  -> ChainHash (ByronBlock cfg)   -- ^ Previous hash
  -> [GenTx (ByronBlock cfg)]     -- ^ Txs to add in the block
  -> [USSArgs]                    -- ^ Update system stimulus args
  -> ()                           -- ^ Leader proof ('IsLeader')
  -> m (ByronBlock ByronDemoConfig)
forgeBlock cfg els curSlot curNo prevHash txs ussargs () = do
    ouroborosPayload <- trace ("forging @ slot " <> show curSlot) $ forgePBftFields (encNodeConfigP cfg) toCBOR toSign
    return $ forge ouroborosPayload
  where
    -- TODO: If we reconsider 'ByronDemoConfig', we can probably move this whole
    -- function to the real Byron module (instead of the demo). None of the
    -- fields here are demo specific.
    --
    -- TODO: Might be sufficient to add 'ConfigContainsGenesis' constraint.
    ByronDemoConfig { pbftGenesisHash
                    , pbftEpochSlots
                    , pbftProtocolVersion
                    , pbftSoftwareVersion
                    , pbftGenesisDlg
                    , pbftProtocolMagic
                    , pbftGenesisConfig
                    } = encNodeConfigExt cfg

    PBftNodeConfig {..}                = encNodeConfigP   cfg
    ByronLedgerState {..}              = ledgerState els
    CC.Block.ChainValidationState {..} = blsCurrent

    usStimuli = promoteUSSArgs cvsUpdateState <$> ussargs
    votes     = lefts usStimuli
    mProposal = case rights usStimuli of
                  []  -> Nothing
                  [p] -> Just p
                  _   -> error "XXX: unhandled -- multiple pending proposals for block."

    completeProposalBody :: CC.UPI.State -> USSArgs -> CC.Update.ProposalBody
    completeProposalBody state (ProposeSoftware (MProposalBody{softwareVersion=Just ver, metadata})) =
      CC.Update.ProposalBody (CC.UPI.adoptedProtocolVersion state) emptyPPU ver metadata
      -- XXX: export 'empty' in cardano-ledger
      where emptyPPU = CC.Update.ProtocolParametersUpdate
                       Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    completeProposalBody _ (ProposeProtocol (MProposalBody{protocolVersion=Just ver, protocolParametersUpdate=Just params})) =
      CC.Update.ProposalBody ver params softVerMemptyish mempty
      where softVerMemptyish =
              CC.Update.SoftwareVersion (CC.Update.ApplicationName "") 0

    completeProposalBody _ _ = error "Invariant failed: non-proposal in completeProposalBody."

    safeSigner = Crypto.noPassSafeSigner key
      where SignKeyCardanoDSIGN key = pbftSignKey

    promoteUSSArgs :: CC.UPI.State -> USSArgs -> Either CC.Update.Vote CC.Update.Proposal
    promoteUSSArgs _ (SubmitVote upid forAgainst) = Left $
      CC.Update.mkVoteSafe
      (CC.Genesis.configProtocolMagicId pbftGenesisConfig)
      safeSigner
      upid
      forAgainst
    promoteUSSArgs state ussa = Right $
      CC.Update.signProposal
      (CC.Genesis.configProtocolMagicId pbftGenesisConfig)
      (completeProposalBody state ussa)
      safeSigner

    txPayload :: CC.UTxO.TxPayload
    txPayload = CC.UTxO.mkTxPayload (map (void . unByronTx) txs)

    body :: CC.Block.Body
    body = CC.Block.ABody {
          CC.Block.bodyTxPayload     = txPayload
        , CC.Block.bodySscPayload    = CC.Ssc.SscPayload
        , CC.Block.bodyDlgPayload    = CC.Delegation.UnsafeAPayload [] ()
        , CC.Block.bodyUpdatePayload = CC.Update.APayload mProposal votes ()
        }

    proof :: CC.Block.Proof
    proof = CC.Block.mkProof body

    prevHeaderHash :: CC.Block.HeaderHash
    prevHeaderHash = case prevHash of
      GenesisHash -> CC.Block.genesisHeaderHash pbftGenesisHash
      BlockHash h -> h

    slotId :: CC.Slot.SlotId
    slotId = CC.Slot.unflattenSlotId pbftEpochSlots $ coerce curSlot

    toSign :: CC.Block.ToSign
    toSign = CC.Block.ToSign {
          CC.Block.tsHeaderHash      = prevHeaderHash
        , CC.Block.tsSlot            = slotId
        , CC.Block.tsDifficulty      = coerce curNo
        , CC.Block.tsBodyProof       = proof
        , CC.Block.tsProtocolVersion = pbftProtocolVersion
        , CC.Block.tsSoftwareVersion = pbftSoftwareVersion
        }

    forge :: PBftFields PBftCardanoCrypto CC.Block.ToSign
          -> ByronBlock ByronDemoConfig
    forge ouroborosPayload =
        annotateByronBlock pbftEpochSlots block
      where
        block :: CC.Block.Block
        block = CC.Block.ABlock {
              CC.Block.blockHeader     = header
            , CC.Block.blockBody       = body
            , CC.Block.blockAnnotation = ()
            }

        headerGenesisKey :: Crypto.VerificationKey
        dlgCertificate :: CC.Delegation.Certificate
        (headerGenesisKey, dlgCertificate) = case findDelegate of
            Just x  -> x
            Nothing -> error "Issuer is not a valid genesis key delegate."
          where
            dlgMap = CC.Genesis.unGenesisDelegation pbftGenesisDlg
            VerKeyCardanoDSIGN issuer = pbftIssuer ouroborosPayload
            findDelegate = fmap (\crt -> (Crypto.pskIssuerVK crt, crt))
                         . find (\crt -> Crypto.pskDelegateVK crt == issuer)
                         $ Map.elems dlgMap

        headerSignature :: CC.Block.BlockSignature
        headerSignature = CC.Block.BlockSignature
                        $ Crypto.AProxySignature dlgCertificate (coerce sig)
          where
            sig :: Crypto.Signature Encoding
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
