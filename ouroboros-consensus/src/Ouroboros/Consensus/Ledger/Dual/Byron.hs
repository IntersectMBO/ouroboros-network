{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Dual.Byron (
    -- * Shorthand
    DualByronBlock
  , DualByronProtocol
  , DualByronConfig
  , DualByronBridge
    -- * Bridge
  , ByronSpecBridge(..)
  , SpecToImplIds(..)
  , specToImplTxWit
  , bridgeTransactionIds
    -- * Block forging
  , forgeDualByronBlock
    -- * Setup
  , protocolInfoDualByron
  ) where

import           Codec.Serialise
import           Crypto.Random (MonadRandom)
import           Data.ByteString (ByteString)
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
import qualified Ledger.UTxO as Spec

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

type DualByronBlock    = DualBlock         ByronBlock ByronSpecBlock
type DualByronProtocol = DualBlockProtocol ByronBlock ByronSpecBlock
type DualByronConfig   = DualConfig        ByronBlock ByronSpecBlock
type DualByronBridge   = BridgeLedger      ByronBlock ByronSpecBlock

{-------------------------------------------------------------------------------
  Map transaction Ids (part of the bridge)
-------------------------------------------------------------------------------}

newtype SpecToImplIds = SpecToImplIds {
      getSpecToImplIds :: Spec.Test.AbstractToConcreteIdMaps
    }
  deriving (Show, Eq, Generic, Serialise)

instance Semigroup SpecToImplIds where
  SpecToImplIds a <> SpecToImplIds b =
      SpecToImplIds $ Spec.Test.AbstractToConcreteIdMaps {
          transactionIds = combine Spec.Test.transactionIds
        , proposalIds    = combine Spec.Test.proposalIds
        }
    where
      combine :: Semigroup x => (Spec.Test.AbstractToConcreteIdMaps -> x) -> x
      combine f = f a <> f b

instance Monoid SpecToImplIds where
  mempty = SpecToImplIds Spec.Test.AbstractToConcreteIdMaps {
        transactionIds = mempty
      , proposalIds    = mempty
      }

-- | Construct singleton 'SpecToImplIds' for a transaction
specToImplTxWit :: Spec.TxWits -> Impl.ATxAux ByteString -> SpecToImplIds
specToImplTxWit spec impl = SpecToImplIds $ Spec.Test.AbstractToConcreteIdMaps {
      transactionIds = Map.singleton (specTxId spec) (byronIdTx impl)
    , proposalIds    = Map.empty
    }
  where
    specTxId :: Spec.TxWits -> Spec.TxId
    specTxId = Spec.txid . Spec.body

{-------------------------------------------------------------------------------
  Bridge
-------------------------------------------------------------------------------}

-- | Bridge the gap between the Byron implementation and specification
--
-- The relation between the Byron implementation and specification for the
-- /linear/ case is tested in the Byron implementation itself, specifically
-- in 'ts_prop_generatedChainsAreValidated'. The main goal of the consensus
-- DualPBFT tests is to lift these tests to the general consensus setting,
-- where time is not linear but branching.
--
-- In the linear case, the tests maintain some state linking the spec and
-- the implementation. In the consensus case, this state cannot be maintained
-- like this, and so it has to become part of transactions, blocks, and the
-- ledger state itself.
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

      -- | Mapping between abstract and concrete Ids
      --
      -- We need to maintain this mapping so that we can use the abstract state
      -- generators and then elaborate to concrete values.
    , toImplIds  :: SpecToImplIds
    }
  deriving (Show, Eq, Generic, Serialise)

instance Bridge ByronBlock ByronSpecBlock where
  type ExtraNodeConfig ByronBlock = ByronConfig
  type BridgeLedger ByronBlock ByronSpecBlock = ByronSpecBridge
  type BridgeBlock  ByronBlock ByronSpecBlock = SpecToImplIds
  type BridgeTx     ByronBlock ByronSpecBlock = SpecToImplIds

  -- TODO: Once we generate delegation certificates,
  -- we should update 'toSpecKeys' also,

  updateBridgeWithBlock block bridge = bridge {
        toImplIds = toImplIds bridge <> dualBlockBridge block
      }

  updateBridgeWithTx genTx bridge = bridge {
        toImplIds = toImplIds bridge <> dualGenTxBridge genTx
      }

{-------------------------------------------------------------------------------
  Bridge initialization
-------------------------------------------------------------------------------}

initByronSpecBridge :: ByronSpecGenesis
                    -> Map Spec.TxId Impl.TxId
                    -- ^ Mapping for the transaction in the initial UTxO
                    -> ByronSpecBridge
initByronSpecBridge ByronSpecGenesis{..} txIdMap = ByronSpecBridge {
      toSpecKeys = Map.fromList $ map mapKey $
                     Set.toList byronSpecGenesisDelegators
    , toImplIds  = SpecToImplIds Spec.Test.AbstractToConcreteIdMaps {
                       transactionIds = txIdMap
                     , proposalIds    = Map.empty
                     }
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
bridgeToSpecKey :: DualByronBridge
                -> PBftVerKeyHash PBftCardanoCrypto -> Spec.VKey
bridgeToSpecKey ByronSpecBridge{..} keyHash =
    case Map.lookup keyHash toSpecKeys of
      Just vkey -> vkey
      Nothing   -> error $ "toSpecKey: unknown key " ++ show keyHash

bridgeTransactionIds :: DualByronBridge -> Map Spec.TxId Impl.TxId
bridgeTransactionIds = Spec.Test.transactionIds
                     . getSpecToImplIds
                     . toImplIds

{-------------------------------------------------------------------------------
  Block forging
-------------------------------------------------------------------------------}

forgeDualByronBlock
  :: forall m.
     ( HasNodeState_ () m  -- @()@ is the @NodeState@ of PBFT
     , MonadRandom m
     , HasCallStack
     )
  => NodeConfig DualByronProtocol
  -> SlotNo                          -- ^ Current slot
  -> BlockNo                         -- ^ Current block number
  -> ExtLedgerState DualByronBlock   -- ^ Ledger
  -> [GenTx DualByronBlock]          -- ^ Txs to add in the block
  -> PBftIsLeader PBftCardanoCrypto  -- ^ Leader proof ('IsLeader')
  -> m DualByronBlock
forgeDualByronBlock cfg curSlotNo curBlockNo extLedger txs isLeader = do
    -- NOTE: We do not /elaborate/ the real Byron block from the spec one, but
    -- instead we /forge/ it. This is important, because we want to test that
    -- codepath. This does mean that we do not get any kind of "bridge" between
    -- the two blocks (which we would have gotten if we would have elaborated
    -- the block instead). Fortunately, this is okay, since the bridge for the
    -- block can be computed from the bridge information of all of the txs.

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
                (bridgeToSpecKey
                   (dualLedgerStateBridge $ ledgerState extLedger)
                   (hashVerKey . deriveVerKeyDSIGN . pbftSignKey $ isLeader))

    return DualBlock {
        dualBlockMain   = main
      , dualBlockAux    = Just aux
      , dualBlockBridge = mconcat $ map dualGenTxBridge txs
      }

{-------------------------------------------------------------------------------
  Setup

  Partly modelled after 'applyTrace' in "Test.Cardano.Chain.Block.Model".
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
    concreteConfig :: ExtraNodeConfig ByronBlock

    abstractConfig = ByronSpecLedgerConfig abstractGenesis
    concreteConfig = byronConfig
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
        --
        -- TODO: We should be able to use keys that are /not/ in genesis
        -- (so that we can start the node with new delegated keys that aren't
        -- present in the genesis config).
        -- <https://github.com/input-output-hk/ouroboros-network/issues/1495>
        keyHash :: PBftVerKeyHash PBftCardanoCrypto
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
