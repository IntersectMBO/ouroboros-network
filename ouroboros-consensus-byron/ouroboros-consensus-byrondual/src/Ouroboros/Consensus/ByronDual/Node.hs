{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronDual.Node (
    protocolInfoDualByron
  ) where

import           Data.Either (fromRight)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.Proxy

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
import           Cardano.Chain.Slotting (EpochSlots)
import qualified Cardano.Chain.Update as Impl
import qualified Cardano.Chain.Update.Validation.Interface as Impl
import qualified Cardano.Chain.UTxO as Impl

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.HeaderValidation

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node hiding (extractEpochSlots)
import qualified Ouroboros.Consensus.Byron.Node as Byron
import           Ouroboros.Consensus.Byron.Protocol

import           Ouroboros.Consensus.ByronSpec.Ledger
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Genesis as Genesis

import           Ouroboros.Consensus.ByronDual.Ledger

{-------------------------------------------------------------------------------
  ProtocolInfo

  Partly modelled after 'applyTrace' in "Test.Cardano.Chain.Block.Model".
-------------------------------------------------------------------------------}

protocolInfoDualByron :: ByronSpecGenesis
                      -> PBftParams
                      -> Maybe CoreNodeId -- ^ Are we a core node?
                      -> ProtocolInfo DualByronBlock
protocolInfoDualByron abstractGenesis@ByronSpecGenesis{..} params mLeader =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            configConsensus = PBftConfig {
                pbftParams    = params
              , pbftIsLeader  = case mLeader of
                                  Nothing  -> PBftIsNotALeader
                                  Just nid -> PBftIsALeader $ pbftIsLeader nid
              }
          , configLedger = DualLedgerConfig {
                dualLedgerConfigMain = concreteGenesis
              , dualLedgerConfigAux  = abstractConfig
              }
          , configBlock = DualBlockConfig {
                dualBlockConfigMain = concreteConfig
              , dualBlockConfigAux  = ByronSpecBlockConfig
              }
          }
      , pInfoInitState =
          ()
      , pInfoInitLedger = ExtLedgerState {
             ledgerState = DualLedgerState {
                 dualLedgerStateMain   = initConcreteState
               , dualLedgerStateAux    = initAbstractState
               , dualLedgerStateBridge = initBridge
               }
           , headerState = genesisHeaderState S.empty
           }
      }
  where
    initUtxo :: Impl.UTxO
    txIdMap  :: Map Spec.TxId Impl.TxId
    (initUtxo, txIdMap) = Spec.Test.elaborateInitialUTxO byronSpecGenesisInitUtxo

    -- 'Spec.Test.abEnvToCfg' ignores the UTxO, because the Byron genesis
    -- data doesn't contain a UTxO, but only a 'UTxOConfiguration'.
    concreteGenesis :: Impl.Config
    concreteGenesis = Spec.Test.abEnvToCfg $ Genesis.toChainEnv abstractGenesis

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
    pbftIsLeader nid = pbftLeaderOrNot $
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

pb :: Proxy ByronBlock
pb = Proxy

instance HasNetworkProtocolVersion DualByronBlock where
  type NodeToNodeVersion   DualByronBlock = NodeToNodeVersion   ByronBlock
  type NodeToClientVersion DualByronBlock = NodeToClientVersion ByronBlock

  supportedNodeToNodeVersions   _ = supportedNodeToNodeVersions   pb
  supportedNodeToClientVersions _ = supportedNodeToClientVersions pb
  mostRecentNodeToNodeVersion   _ = mostRecentNodeToNodeVersion   pb
  mostRecentNodeToClientVersion _ = mostRecentNodeToClientVersion pb
  nodeToNodeProtocolVersion     _ = nodeToNodeProtocolVersion     pb
  nodeToClientProtocolVersion   _ = nodeToClientProtocolVersion   pb

instance RunNode DualByronBlock where
  nodeForgeBlock = forgeDualByronBlock

  -- Just like Byron, we need to start with an EBB
  nodeInitChainDB cfg chainDB = do
      tip <- atomically $ ChainDB.getTipPoint chainDB
      case tip of
        BlockPoint {} -> return () -- Chain is not empty
        GenesisPoint  -> ChainDB.addBlock_ chainDB genesisEBB
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
  nodeImmDbChunkInfo  = \_p -> nodeImmDbChunkInfo  pb . dualTopLevelConfigMain
  nodeStartTime       = \_p -> nodeStartTime       pb . dualTopLevelConfigMain
  nodeNetworkMagic    = \_p -> nodeNetworkMagic    pb . dualTopLevelConfigMain
  nodeProtocolMagicId = \_p -> nodeProtocolMagicId pb . dualTopLevelConfigMain

  -- The max block size we set to the max block size of the /concrete/ block
  -- (Correspondingly, 'txSize' for the Byron spec returns 0)
  nodeMaxBlockSize          = nodeMaxBlockSize          . dualLedgerStateMain
  nodeBlockEncodingOverhead = nodeBlockEncodingOverhead . dualLedgerStateMain

  -- Envelope
  nodeHashInfo      = \_p -> nodeHashInfo      pb
  nodeEncodeTipInfo = \_p -> nodeEncodeTipInfo pb
  nodeDecodeTipInfo = \_p -> nodeDecodeTipInfo pb

  -- We can look at the concrete header to see if this is an EBB
  nodeIsEBB = nodeIsEBB . dualHeaderMain

  -- For now the size of the block is just an estimate, and so we just reuse
  -- the estimate from the concrete header.
  nodeBlockFetchSize = nodeBlockFetchSize . dualHeaderMain

  -- We don't really care too much about data loss or malicious behaviour for
  -- the dual ledger tests, so integrity and match checks can just use the
  -- concrete implementation
  nodeBlockMatchesHeader hdr = nodeBlockMatchesHeader (dualHeaderMain         hdr) . dualBlockMain
  nodeCheckIntegrity     cfg = nodeCheckIntegrity     (dualTopLevelConfigMain cfg) . dualBlockMain

  -- The header is just the concrete header, so we can just reuse the Byron def
  nodeAddHeaderEnvelope = \_ -> nodeAddHeaderEnvelope pb

  nodeExceptionIsFatal  = \_ -> nodeExceptionIsFatal pb

  -- Encoders
  nodeEncodeBlockWithInfo  = const $ encodeDualBlockWithInfo encodeByronBlockWithInfo
  nodeEncodeHeader         = \cfg version ->
                                  nodeEncodeHeader
                                    (dualBlockConfigMain cfg)
                                    (castSerialisationVersion version)
                                . dualHeaderMain
  nodeEncodeWrappedHeader  = \cfg version ->
                                  nodeEncodeWrappedHeader
                                    (dualBlockConfigMain cfg)
                                    (castSerialisationAcrossNetwork version)
                                . dualWrappedMain
  nodeEncodeLedgerState    = encodeDualLedgerState encodeByronLedgerState
  nodeEncodeApplyTxError   = const $ encodeDualGenTxErr encodeByronApplyTxError
  nodeEncodeHeaderHash     = const $ encodeByronHeaderHash
  nodeEncodeGenTx          = encodeDualGenTx   encodeByronGenTx
  nodeEncodeGenTxId        = encodeDualGenTxId encodeByronGenTxId
  nodeEncodeConsensusState = \_proxy _cfg -> encodeByronConsensusState
  nodeEncodeQuery          = \case {}
  nodeEncodeResult         = \case {}

  -- Decoders
  nodeDecodeBlock          = decodeDualBlock  . decodeByronBlock   . extractEpochSlots
  nodeDecodeHeader         = \cfg version ->
                               (DualHeader .) <$>
                                 nodeDecodeHeader
                                   (dualBlockConfigMain cfg)
                                   (castSerialisationVersion version)
  nodeDecodeWrappedHeader  = \cfg version ->
                               rewrapMain <$>
                                 nodeDecodeWrappedHeader
                                   (dualBlockConfigMain cfg)
                                   (castSerialisationAcrossNetwork version)
  nodeDecodeGenTx          = decodeDualGenTx   decodeByronGenTx
  nodeDecodeGenTxId        = decodeDualGenTxId decodeByronGenTxId
  nodeDecodeHeaderHash     = const $ decodeByronHeaderHash
  nodeDecodeLedgerState    = decodeDualLedgerState decodeByronLedgerState
  nodeDecodeApplyTxError   = const $ decodeDualGenTxErr    decodeByronApplyTxError
  nodeDecodeConsensusState = \_proxy cfg ->
                                let k = configSecurityParam cfg
                                in decodeByronConsensusState k
  nodeDecodeQuery          = error "DualByron.nodeDecodeQuery"
  nodeDecodeResult         = \case {}

extractEpochSlots :: BlockConfig DualByronBlock -> EpochSlots
extractEpochSlots = Byron.extractEpochSlots . dualBlockConfigMain

{-------------------------------------------------------------------------------
  The headers for DualByronBlock and ByronBlock are identical, so we can
  safely cast the serialised forms.
-------------------------------------------------------------------------------}

dualWrappedMain :: Serialised (Header DualByronBlock)
                -> Serialised (Header ByronBlock)
dualWrappedMain (Serialised bs) = Serialised bs

rewrapMain :: Serialised (Header ByronBlock)
           -> Serialised (Header DualByronBlock)
rewrapMain (Serialised bs) = Serialised bs
