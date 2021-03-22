{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronDual.Node (protocolInfoDualByron) where

import           Data.Either (fromRight)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)

import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.Delegation as Spec
import qualified Byron.Spec.Ledger.UTxO as Spec
import qualified Byron.Spec.Ledger.Update as Spec

import qualified Test.Cardano.Chain.Elaboration.Block as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Delegation as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Keys as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Update as Spec.Test
import qualified Test.Cardano.Chain.UTxO.Model as Spec.Test

import qualified Cardano.Chain.Block as Impl
import qualified Cardano.Chain.Genesis as Impl
import qualified Cardano.Chain.UTxO as Impl
import qualified Cardano.Chain.Update as Impl
import qualified Cardano.Chain.Update.Validation.Interface as Impl

import           Ouroboros.Consensus.HeaderValidation

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB (..))
import           Ouroboros.Consensus.Util ((.....:), (.:))

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node
import           Ouroboros.Consensus.Byron.Protocol

import           Ouroboros.Consensus.ByronSpec.Ledger
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Genesis as Genesis

import           Ouroboros.Consensus.ByronDual.Ledger
import           Ouroboros.Consensus.ByronDual.Node.Serialisation ()

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

dualByronBlockForging
  :: Monad m
  => ByronLeaderCredentials
  -> BlockForging m DualByronBlock
dualByronBlockForging creds = BlockForging {
      forgeLabel       = forgeLabel
    , canBeLeader      = canBeLeader
    , updateForgeState = \cfg ->
        fmap castForgeStateUpdateInfo .: updateForgeState (dualTopLevelConfigMain cfg)
    , checkCanForge    = checkCanForge . dualTopLevelConfigMain
    , forgeBlock       = return .....: forgeDualByronBlock
    }
  where
    BlockForging {..} = byronBlockForging creds

{-------------------------------------------------------------------------------
  ProtocolInfo

  Partly modelled after 'applyTrace' in "Test.Cardano.Chain.Block.Model".
-------------------------------------------------------------------------------}

protocolInfoDualByron :: forall m. Monad m
                      => ByronSpecGenesis
                      -> PBftParams
                      -> [CoreNodeId] -- ^ Are we a core node?
                      -> ProtocolInfo m DualByronBlock
protocolInfoDualByron abstractGenesis@ByronSpecGenesis{..} params credss =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = PBftConfig {
                pbftParams = params
              }
          , topLevelConfigLedger = DualLedgerConfig {
                dualLedgerConfigMain = concreteGenesis
              , dualLedgerConfigAux  = abstractConfig
              }
          , topLevelConfigBlock = DualBlockConfig {
                dualBlockConfigMain = concreteConfig
              , dualBlockConfigAux  = ByronSpecBlockConfig
              }
          , topLevelConfigCodec = DualCodecConfig {
                dualCodecConfigMain = mkByronCodecConfig concreteGenesis
              , dualCodecConfigAux  = ByronSpecCodecConfig
              }
          , topLevelConfigStorage = DualStorageConfig {
                dualStorageConfigMain = ByronStorageConfig concreteConfig
              , dualStorageConfigAux  = ByronSpecStorageConfig
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
      , pInfoBlockForging =
           return $ dualByronBlockForging . byronLeaderCredentials <$> credss
      }
  where
    initUtxo :: Impl.UTxO
    txIdMap  :: Map Spec.TxId Impl.TxId
    (initUtxo, txIdMap) = Spec.Test.elaborateInitialUTxO byronSpecGenesisInitUtxo

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

    byronLeaderCredentials :: CoreNodeId -> ByronLeaderCredentials
    byronLeaderCredentials nid =
        fromRight (error "byronLeaderCredentials: failed to construct credentials") $
          mkByronLeaderCredentials
            concreteGenesis
            (Spec.Test.vKeyToSKey vkey)
            (Spec.Test.elaborateDCert
               (Impl.configProtocolMagicId concreteGenesis)
               abstractDCert)
            "byronLeaderCredentials"
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
  NodeInitStorage instance
-------------------------------------------------------------------------------}

instance NodeInitStorage DualByronBlock where
  -- Just like Byron, we need to start with an EBB
  nodeInitChainDB cfg InitChainDB { getCurrentLedger, addBlock } = do
      tip <- ledgerTipPoint (Proxy @DualByronBlock) <$> getCurrentLedger
      case tip of
        BlockPoint {} -> return ()
        GenesisPoint  -> addBlock genesisEBB
    where
      genesisEBB :: DualByronBlock
      genesisEBB = DualBlock {
            dualBlockMain   = byronEBB
          , dualBlockAux    = Nothing
          , dualBlockBridge = mempty
          }

      byronEBB :: ByronBlock
      byronEBB = forgeEBB
                   (getByronBlockConfig (dualStorageConfigMain cfg))
                   (SlotNo 0)
                   (BlockNo 0)
                   GenesisHash

  -- Node config is a consensus concern, determined by the main block only
  nodeImmutableDbChunkInfo = nodeImmutableDbChunkInfo . dualStorageConfigMain

  -- We don't really care too much about data loss or malicious behaviour for
  -- the dual ledger tests, so integrity and match checks can just use the
  -- concrete implementation
  nodeCheckIntegrity cfg = nodeCheckIntegrity (dualStorageConfigMain cfg) . dualBlockMain

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance BlockSupportsMetrics DualByronBlock where
  isSelfIssued = isSelfIssuedConstUnknown

instance RunNode DualByronBlock
