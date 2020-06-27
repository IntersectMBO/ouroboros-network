{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronDual.Node (
    protocolInfoDualByron
  ) where

import           Control.Monad
import           Data.Either (fromRight)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)

import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.Delegation as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import qualified Byron.Spec.Ledger.UTxO as Spec

import qualified Test.Cardano.Chain.Elaboration.Block as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Delegation as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Keys as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Update as Spec.Test
import qualified Test.Cardano.Chain.UTxO.Model as Spec.Test

import qualified Cardano.Chain.Block as Impl
import qualified Cardano.Chain.Genesis as Impl
import qualified Cardano.Chain.Update as Impl
import qualified Cardano.Chain.Update.Validation.Interface as Impl
import qualified Cardano.Chain.UTxO as Impl

import           Ouroboros.Consensus.HeaderValidation

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node
import           Ouroboros.Consensus.Byron.Protocol

import           Ouroboros.Consensus.ByronSpec.Ledger
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Genesis as Genesis

import           Ouroboros.Consensus.ByronDual.Ledger
import           Ouroboros.Consensus.ByronDual.Node.Serialisation ()

{-------------------------------------------------------------------------------
  ProtocolInfo

  Partly modelled after 'applyTrace' in "Test.Cardano.Chain.Block.Model".
-------------------------------------------------------------------------------}

protocolInfoDualByron :: forall m. Monad m
                      => ByronSpecGenesis
                      -> PBftParams
                      -> Maybe CoreNodeId -- ^ Are we a core node?
                      -> ProtocolInfo m DualByronBlock
protocolInfoDualByron abstractGenesis@ByronSpecGenesis{..} params mLeader =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            configConsensus = PBftConfig {
                pbftParams = params
              }
          , configIndep  = ()
          , configLedger = DualLedgerConfig {
                dualLedgerConfigMain = concreteGenesis
              , dualLedgerConfigAux  = abstractConfig
              }
          , configBlock = DualBlockConfig {
                dualBlockConfigMain = concreteConfig
              , dualBlockConfigAux  = ByronSpecBlockConfig
              }
          , configCodec = DualCodecConfig {
                dualCodecConfigMain = mkByronCodecConfig concreteGenesis
              }
          }
      , pInfoInitLedger = ExtLedgerState {
             ledgerState = DualLedgerState {
                 dualLedgerStateMain   = initConcreteState
               , dualLedgerStateAux    = initAbstractState
               , dualLedgerStateBridge = initBridge
               }
           , headerState = genesisHeaderState S.empty
           }
      , pInfoLeaderCreds = mkCreds <$> mLeader
      }
  where
    initUtxo :: Impl.UTxO
    txIdMap  :: Map Spec.TxId Impl.TxId
    (initUtxo, txIdMap) = Spec.Test.elaborateInitialUTxO byronSpecGenesisInitUtxo

    mkCreds :: CoreNodeId
            -> (PBftIsLeader PBftByronCrypto
               , MaintainForgeState m DualByronBlock
               )
    mkCreds nid = (pbftIsLeader nid, defaultMaintainForgeState)

    -- 'Spec.Test.abEnvToCfg' ignores the UTxO, because the Byron genesis
    -- data doesn't contain a UTxO, but only a 'UTxOConfiguration'.
    --
    -- It also ignores the slot length (the Byron spec does not talk about
    -- slot lengths at all) so we have to set this ourselves.
    concreteGenesis :: Impl.Config
    concreteGenesis = translated {
          Impl.configGenesisData = configGenesisData {
              Impl.gdProtocolParameters = protocolParameters {
                   Impl.ppSlotDuration = byronSpecGenesisSlotLength
                }
            }
        }
      where
        translated = Spec.Test.abEnvToCfg $ Genesis.toChainEnv abstractGenesis
        configGenesisData  = Impl.configGenesisData translated
        protocolParameters = Impl.gdProtocolParameters configGenesisData

    initAbstractState :: LedgerState ByronSpecBlock
    initConcreteState :: LedgerState ByronBlock

    initAbstractState = initByronSpecLedgerState abstractGenesis
    initConcreteState = initByronLedgerState     concreteGenesis (Just initUtxo)

    abstractConfig :: LedgerConfig ByronSpecBlock
    concreteConfig :: BlockConfig ByronBlock

    abstractConfig = abstractGenesis
    concreteConfig = mkByronConfig
                       concreteGenesis
                       protocolVersion
                       softwareVersion
      where
        -- TODO: Take (spec) protocol version and (spec) software version
        -- as arguments instead, and then translate /those/ to Impl types.
        -- <https://github.com/input-output-hk/ouroboros-network/issues/1495>
        protocolVersion :: Impl.ProtocolVersion
        protocolVersion =
            Impl.adoptedProtocolVersion $
              Impl.cvsUpdateState (byronLedgerState initConcreteState)

        -- The spec has a TODO about this; we just copy what 'elaborate' does
        -- (Test.Cardano.Chain.Elaboration.Block)
        softwareVersion :: Impl.SoftwareVersion
        softwareVersion =
            Spec.Test.elaborateSoftwareVersion $
              Spec.SwVer (Spec.ApName "") (Spec.ApVer 0)

    initBridge :: DualByronBridge
    initBridge = initByronSpecBridge abstractGenesis txIdMap

    pbftIsLeader :: CoreNodeId -> PBftIsLeader PBftByronCrypto
    pbftIsLeader nid = mkPBftIsLeader $
        fromRight (error "pbftIsLeader: failed to construct credentials") $
          mkPBftLeaderCredentials
            concreteGenesis
            (Spec.Test.vKeyToSKey vkey)
            (Spec.Test.elaborateDCert
               (Impl.configProtocolMagicId concreteGenesis)
               abstractDCert)
      where
        -- PBFT constructs the core node ID by the implicit ordering of
        -- the hashes of the verification keys in the genesis config. Here
        -- we go the other way, looking up this hash, and then using our
        -- translation map to find the corresponding abstract key.
        --
        -- TODO: We should be able to use keys that are /not/ in genesis
        -- (so that we can start the node with new delegated keys that aren't
        -- present in the genesis config).
        -- <https://github.com/input-output-hk/ouroboros-network/issues/1495>
        keyHash :: PBftVerKeyHash PBftByronCrypto
        keyHash = fromMaybe
                    (error $ "mkCredentials: invalid " ++ show nid)
                    (nodeIdToGenesisKey concreteGenesis nid)

        vkey :: Spec.VKey
        vkey = bridgeToSpecKey initBridge keyHash

        abstractDCert :: Spec.DCert
        abstractDCert = Spec.Test.rcDCert
                          vkey
                          byronSpecGenesisSecurityParam
                          (byronSpecLedgerState initAbstractState)

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance RunNode DualByronBlock where
  -- Just like Byron, we need to start with an EBB
  nodeInitChainDB cfg chainDB = do
      empty <- InitChainDB.checkEmpty chainDB
      when empty $ InitChainDB.addBlock chainDB genesisEBB
    where
      genesisEBB :: DualByronBlock
      genesisEBB = DualBlock {
            dualBlockMain   = byronEBB
          , dualBlockAux    = Nothing
          , dualBlockBridge = mempty
          }

      byronEBB :: ByronBlock
      byronEBB = forgeEBB
                   (dualTopLevelConfigMain cfg)
                   (SlotNo 0)
                   (BlockNo 0)
                   GenesisHash

  -- Node config is a consensus concern, determined by the main block only
  nodeImmDbChunkInfo  = nodeImmDbChunkInfo  . dualTopLevelConfigMain

  -- For now the size of the block is just an estimate, and so we just reuse
  -- the estimate from the concrete header.
  nodeBlockFetchSize = nodeBlockFetchSize . dualHeaderMain

  -- We don't really care too much about data loss or malicious behaviour for
  -- the dual ledger tests, so integrity and match checks can just use the
  -- concrete implementation
  nodeCheckIntegrity cfg = nodeCheckIntegrity (dualTopLevelConfigMain cfg) . dualBlockMain

  nodeGetBinaryBlockInfo = dualBinaryBlockInfo nodeGetBinaryBlockInfo
