{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Ledger.Byron.Forge (
    forgeBlock
  , forgeBlockOrEBB
  ) where

import           Control.Monad (void)
import           Crypto.Random (MonadRandom)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Foldable (find)
import qualified Data.Map.Strict as Map
import           Data.Reflection (Given (..), give)

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
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.WithEBBs

import           Ouroboros.Consensus.Ledger.Byron.Config

forgeBlockOrEBB
  :: forall m cfg.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
       -- TODO: This 'Given' constraint should not be needed (present in config)
     , Given Crypto.ProtocolMagicId
     )
  => NodeConfig ByronEBBExtNodeConfig
  -> SlotNo                       -- ^ Current slot
  -> BlockNo                      -- ^ Current block number
  -> ChainHash (ByronBlockOrEBB cfg)   -- ^ Previous hash
  -> [GenTx (ByronBlockOrEBB cfg)]     -- ^ Txs to add in the block
  -> PBftIsLeader PBftCardanoCrypto    -- ^ Leader proof ('IsLeader')
  -> m (ByronBlockOrEBB ByronConfig)
forgeBlockOrEBB cfg curSlot curNo prevHash txs isLeader = case prevHash of
  GenesisHash -> forgeGenesisEBB cfg curSlot
  BlockHash _ -> forgeBlock cfg curSlot curNo prevHash txs isLeader

forgeGenesisEBB
  :: forall m.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
       -- TODO: This 'Given' constraint should not be needed (present in config)
     , Given Crypto.ProtocolMagicId
     )
  => NodeConfig ByronEBBExtNodeConfig
  -> SlotNo
  -> m (ByronBlockOrEBB ByronConfig)
forgeGenesisEBB (WithEBBNodeConfig cfg) curSlot = do
    return
      . ByronBlockOrEBB
      . CC.Block.ABOBBoundary
      . annotateBoundary given
      $ boundaryData
  where
    boundaryData = CC.Block.BoundaryValidationData
                   boundaryLength
                   (Left pbftGenesisHash)
                   epoch
                   (CC.Common.ChainDifficulty 0)
                   ()
                   ()
    boundaryLength = 0 -- Since this is a demo and we ignore the length, set this
                       -- to 0
    ByronConfig { pbftGenesisHash
                    , pbftEpochSlots
                    } = encNodeConfigExt cfg
    CC.Slot.EpochNumber epoch = CC.Slot.epochNo
                                . CC.Slot.fromSlotNumber pbftEpochSlots
                                $ coerce curSlot

forgeBlock
  :: forall m cfg.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
       -- TODO: This 'Given' constraint should not be needed (present in config)
     , Given Crypto.ProtocolMagicId
     )
  => NodeConfig ByronEBBExtNodeConfig
  -> SlotNo                       -- ^ Current slot
  -> BlockNo                      -- ^ Current block number
  -> ChainHash (ByronBlockOrEBB cfg)   -- ^ Previous hash
  -> [GenTx (ByronBlockOrEBB cfg)]     -- ^ Txs to add in the block
  -> PBftIsLeader PBftCardanoCrypto    -- ^ Leader proof ('IsLeader')
  -> m (ByronBlockOrEBB ByronConfig)
forgeBlock (WithEBBNodeConfig cfg) curSlot curNo prevHash txs isLeader = do
    ouroborosPayload <- give (VerKeyCardanoDSIGN headerGenesisKey)
      $ forgePBftFields isLeader (reAnnotate $ Annotated toSign ())
    return $ forge ouroborosPayload
  where
    -- TODO: Might be sufficient to add 'ConfigContainsGenesis' constraint.
    ByronConfig
      { pbftGenesisHash
      , pbftEpochSlots
      , pbftProtocolVersion
      , pbftSoftwareVersion
      , pbftGenesisDlg
      , pbftProtocolMagic
      } = encNodeConfigExt cfg

    txPayload :: CC.UTxO.TxPayload
    txPayload = CC.UTxO.mkTxPayload (map (void . byronTx) txs)

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
    VerKeyCardanoDSIGN headerGenesisKey = pbftGenVerKey isLeader

    dlgCertificate :: CC.Delegation.Certificate
    dlgCertificate = case findDelegate of
        Just x  -> x
        Nothing -> error "Issuer is not a valid genesis key delegate."
      where
        dlgMap = CC.Genesis.unGenesisDelegation pbftGenesisDlg
        VerKeyCardanoDSIGN delegate = pbftVerKey isLeader
        findDelegate = find (\crt -> CC.Delegation.delegateVK crt == delegate
                                   && CC.Delegation.issuerVK crt == headerGenesisKey
                             )
                      $ Map.elems dlgMap

    forge :: PBftFields PBftCardanoCrypto (Annotated CC.Block.ToSign ByteString)
          -> ByronBlockOrEBB ByronConfig
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
