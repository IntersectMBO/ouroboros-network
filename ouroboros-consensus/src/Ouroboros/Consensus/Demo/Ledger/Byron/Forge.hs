{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Demo.Ledger.Byron.Forge (
    forgeBlock
  ) where

import           Control.Monad (void)
import           Crypto.Random (MonadRandom)
import           Data.Coerce (coerce)
import           Data.Foldable (find)
import qualified Data.Map.Strict as Map
import           Data.Reflection (Given (..), give)

import           Cardano.Binary (Annotated (..), toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Ssc as CC.Ssc
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.UTxO as CC.UTxO
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT

import           Ouroboros.Consensus.Demo.Ledger.Byron.Config

forgeBlock
  :: forall m cfg.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
       -- TODO: This 'Given' constraint should not be needed (present in config)
     , Given Crypto.ProtocolMagicId
     )
  => NodeConfig ByronExtNodeConfig
  -> SlotNo                       -- ^ Current slot
  -> BlockNo                      -- ^ Current block number
  -> ChainHash (ByronBlock cfg)   -- ^ Previous hash
  -> [GenTx (ByronBlock cfg)]     -- ^ Txs to add in the block
  -> ()                           -- ^ Leader proof ('IsLeader')
  -> m (ByronBlock ByronDemoConfig)
forgeBlock cfg curSlot curNo prevHash txs () = do
    ouroborosPayload <- give (VerKeyCardanoDSIGN headerGenesisKey)
      $ forgePBftFields (encNodeConfigP cfg) toCBOR toSign
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
                    } = encNodeConfigExt cfg

    txPayload :: CC.UTxO.TxPayload
    txPayload = CC.UTxO.mkTxPayload (map (void . unByronTx) txs)

    body :: CC.Block.Body
    body = CC.Block.ABody {
          CC.Block.bodyTxPayload     = txPayload
        , CC.Block.bodySscPayload    = CC.Ssc.SscPayload
        , CC.Block.bodyDlgPayload    = CC.Delegation.UnsafeAPayload [] ()
        , CC.Block.bodyUpdatePayload = CC.Update.APayload Nothing [] ()
        }

    proof :: CC.Block.Proof
    proof = CC.Block.mkProof body

    prevHeaderHash :: CC.Block.HeaderHash
    prevHeaderHash = case prevHash of
      GenesisHash -> CC.Block.genesisHeaderHash pbftGenesisHash
      BlockHash h -> h

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
    dlgCertificate :: CC.Delegation.Certificate
    (headerGenesisKey, dlgCertificate) = case findDelegate of
        Just x  -> x
        Nothing -> error "Issuer is not a valid genesis key delegate."
      where
        dlgMap = CC.Genesis.unGenesisDelegation pbftGenesisDlg
        VerKeyCardanoDSIGN issuer = pbftVerKey $ encNodeConfigP cfg
        findDelegate = fmap (\crt -> (CC.Delegation.issuerVK crt, crt))
                      . find (\crt -> CC.Delegation.delegateVK crt == issuer)
                      $ Map.elems dlgMap

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
