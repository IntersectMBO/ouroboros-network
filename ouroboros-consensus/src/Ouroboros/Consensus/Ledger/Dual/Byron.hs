{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Dual.Byron (
    DualByronBlock
  , DualByronConsensusProtocol
  , DualByronConfig
  , DualByronBridge
  , forgeDualByronBlock
  , protocolInfoDualByron
  ) where

import           Codec.Serialise
import           Crypto.Random (MonadRandom)
import           Data.Either (fromRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Crypto.DSIGN.Class

import qualified Ledger.Core as Spec
import qualified Ledger.Delegation as Spec
import qualified Ledger.Update as Spec

import qualified Test.Cardano.Chain.Elaboration.Block as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Delegation as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Keys as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Update as Spec.Test

import qualified Cardano.Chain.Block as Impl
import qualified Cardano.Chain.Genesis as Impl
import qualified Cardano.Chain.Update as Impl
import qualified Cardano.Chain.Update.Validation.Interface as Impl

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.ByronSpec
import qualified Ouroboros.Consensus.Ledger.ByronSpec.Genesis as Genesis
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS

{-------------------------------------------------------------------------------
  Shorthand
-------------------------------------------------------------------------------}

type DualByronBlock             = DualBlock         ByronBlock ByronSpecBlock
type DualByronConsensusProtocol = DualBlockProtocol ByronBlock ByronSpecBlock
type DualByronConfig            = DualConfig        ByronBlock ByronSpecBlock
type DualByronBridge            = BridgeState       ByronBlock ByronSpecBlock

{-------------------------------------------------------------------------------
  Bridge
-------------------------------------------------------------------------------}

data ByronSpecBridge = ByronSpecBridge {
      -- | Map between keys
      --
      -- Some observations:
      --
      -- * The abstract chain environment contains a set of allowed delegators
      --   (of type @Set VKeyGenesis@), which gets translated to
      --   'gdGenesisKeyHashes' (of type @Set Common.KeyHash@) in the concrete
      --   genesis config.
      --
      -- * During the translation from abstract blocks to concrete blocks, the
      --   'VKey' of the block is translated to a concrete 'SigningKey' (as well
      --   as a 'VerificationKey') in 'elaborateKeyPair'.
      --
      -- * Although this translation is deterministic, it doesn't have an
      --   easily definable inverse. For this reason, we maintain an opposite
      --   mapping as part of the ledger state.
      toSpecKeys :: Map (PBftVerKeyHash PBftCardanoCrypto) Spec.VKey
    }
  deriving (Show, Eq, Generic, Serialise)

instance Bridge ByronBlock ByronSpecBlock where
  type ExtraNodeConfig ByronBlock = ByronConfig
  type BridgeState ByronBlock ByronSpecBlock = ByronSpecBridge

  updateBridgeWithBlock _ = id -- TODO
  updateBridgeWithTx    _ = id -- TODO

{-------------------------------------------------------------------------------
  Bridge initialization
-------------------------------------------------------------------------------}

initByronSpecBridge :: ByronSpecGenesis -> ByronSpecBridge
initByronSpecBridge ByronSpecGenesis{..} = ByronSpecBridge {
      toSpecKeys = Map.fromList $ map mapKey $
                     Set.toList byronSpecGenesisDelegators
    }
  where
    -- The abstract spec maps the allowed delegators to themselves initially
    mapKey :: Spec.VKeyGenesis -> (PBftVerKeyHash PBftCardanoCrypto, Spec.VKey)
    mapKey (Spec.VKeyGenesis vkey) = (
          hashVerKey $ VerKeyCardanoDSIGN (Spec.Test.elaborateVKey vkey)
        , vkey
        )

{-------------------------------------------------------------------------------
  Using the bridge
-------------------------------------------------------------------------------}

-- | Translate issuer key
--
-- We get a proof from PBFT that we are the leader, including a signing key (of
-- type 'SigningKey'). In order to produce the corresponding abstract block, we
-- need a 'VKey'.
toSpecKey :: DualByronBridge -> PBftVerKeyHash PBftCardanoCrypto -> Spec.VKey
toSpecKey ByronSpecBridge{..} keyHash =
    case Map.lookup keyHash toSpecKeys of
      Just vkey -> vkey
      Nothing   -> error $ "toSpecKey: unknown key " ++ show keyHash

{-------------------------------------------------------------------------------
  Block forging
-------------------------------------------------------------------------------}

forgeDualByronBlock
  :: forall m.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
     , HasCallStack
     )
  => NodeConfig DualByronConsensusProtocol
  -> SlotNo                          -- ^ Current slot
  -> BlockNo                         -- ^ Current block number
  -> ExtLedgerState DualByronBlock   -- ^ Ledger
  -> [GenTx DualByronBlock]          -- ^ Txs to add in the block
  -> PBftIsLeader PBftCardanoCrypto  -- ^ Leader proof ('IsLeader')
  -> m DualByronBlock
forgeDualByronBlock cfg curSlotNo curBlockNo extLedger txs isLeader = do
    main <- forgeByronBlock
              (dualNodeConfigMain cfg)
              curSlotNo
              curBlockNo
              (dualExtLedgerStateMain extLedger)
              (map dualGenTxMain txs)
              isLeader

    let aux :: ByronSpecBlock
        aux = forgeByronSpecBlock
                curSlotNo
                curBlockNo
                (dualLedgerStateAux $ ledgerState extLedger)
                (map dualGenTxAux txs)
                (toSpecKey
                   (dualLedgerStateBridge $ ledgerState extLedger)
                   (hashVerKey . deriveVerKeyDSIGN . pbftSignKey $ isLeader))

    return DualBlock {
        dualBlockMain = main
      , dualBlockAux  = Just aux
      }

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

protocolInfoDualByron :: ByronSpecGenesis
                      -> PBftParams
                      -> Maybe CoreNodeId -- ^ Are we a core node?
                      -> ProtocolInfo DualByronBlock
protocolInfoDualByron abstractGenesis@ByronSpecGenesis{..} params mLeader =
    ProtocolInfo {
        pInfoConfig = PBftNodeConfig {
            pbftParams    = params
          , pbftIsLeader  = case mLeader of
                              Nothing  -> PBftIsNotALeader
                              Just nid -> PBftIsALeader $ pbftIsLeader nid
          , pbftExtConfig = DualConfig {
                                dualConfigMain = concreteConfig
                              , dualConfigAux  = abstractConfig
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
           , ouroborosChainState = CS.empty
           }
      }
  where
    concreteGenesis :: Impl.Config
    concreteGenesis = Spec.Test.abEnvToCfg $ Genesis.toChainEnv abstractGenesis

    initAbstractState :: LedgerState ByronSpecBlock
    initConcreteState :: LedgerState ByronBlock

    initAbstractState = initByronSpecLedgerState abstractGenesis
    initConcreteState = initByronLedgerState     concreteGenesis

    abstractConfig :: LedgerConfig ByronSpecBlock
    concreteConfig :: ExtraNodeConfig ByronBlock

    abstractConfig = ByronSpecLedgerConfig abstractGenesis
    concreteConfig = byronConfig
                       concreteGenesis
                       protocolVersion
                       softwareVersion
      where
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
    initBridge = initByronSpecBridge abstractGenesis

    pbftIsLeader :: CoreNodeId -> PBftIsLeader PBftCardanoCrypto
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
        keyHash :: PBftVerKeyHash PBftCardanoCrypto
        keyHash = fromMaybe
                    (error $ "mkCredentials: invalid " ++ show nid)
                    (nodeIdToGenesisKey concreteGenesis nid)

        vkey :: Spec.VKey
        vkey = toSpecKey initBridge keyHash

        abstractDCert :: Spec.DCert
        abstractDCert = Spec.Test.rcDCert
                          vkey
                          byronSpecGenesisSecurityParam
                          (byronSpecLedgerState initAbstractState)
