{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.ByronDual.Ledger (
    -- * Shorthand
    DualByronBlock
  , DualByronBridge
    -- * Bridge
  , ByronSpecBridge (..)
  , SpecToImplIds (..)
  , bridgeToSpecKey
  , bridgeTransactionIds
  , initByronSpecBridge
  , specToImplTx
    -- * Block forging
  , forgeDualByronBlock
  ) where

import           Codec.Serialise
import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Crypto.DSIGN.Class

import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.UTxO as Spec

import qualified Test.Cardano.Chain.Elaboration.Block as Spec.Test
import qualified Test.Cardano.Chain.Elaboration.Keys as Spec.Test

import qualified Cardano.Chain.UTxO as Impl

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Dual
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Protocol.PBFT

import           Ouroboros.Consensus.Byron.Crypto.DSIGN
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Protocol

import           Ouroboros.Consensus.ByronSpec.Ledger

{-------------------------------------------------------------------------------
  Shorthand
-------------------------------------------------------------------------------}

type DualByronBlock  = DualBlock    ByronBlock ByronSpecBlock
type DualByronBridge = BridgeLedger ByronBlock ByronSpecBlock

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
specToImplTx :: Spec.Tx -> Impl.ATxAux ByteString -> SpecToImplIds
specToImplTx spec impl = SpecToImplIds $ Spec.Test.AbstractToConcreteIdMaps {
      transactionIds = Map.singleton (specTxId spec) (byronIdTx impl)
    , proposalIds    = Map.empty
    }
  where
    specTxId :: Spec.Tx -> Spec.TxId
    specTxId = Spec.txid . Spec.body

{-------------------------------------------------------------------------------
  Bridge
-------------------------------------------------------------------------------}

-- | Bridge the gap between the Byron implementation and specification
--
-- The relation between the Byron implementation and specification for the
-- /linear/ case is tested in the Byron implementation itself, specifically
-- in 'ts_prop_generatedChainsAreValidated'. The main goal of the consensus
-- DualByron tests is to lift these tests to the general consensus setting,
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
      toSpecKeys :: Map (PBftVerKeyHash PBftByronCrypto) Spec.VKey

      -- | Mapping between abstract and concrete Ids
      --
      -- We need to maintain this mapping so that we can use the abstract state
      -- generators and then elaborate to concrete values.
    , toImplIds  :: SpecToImplIds
    }
  deriving (Show, Eq, Generic, Serialise)

instance Bridge ByronBlock ByronSpecBlock where
  type BridgeLedger ByronBlock ByronSpecBlock = ByronSpecBridge
  type BridgeBlock  ByronBlock ByronSpecBlock = SpecToImplIds
  type BridgeTx     ByronBlock ByronSpecBlock = SpecToImplIds

  -- TODO: Once we generate delegation certificates,
  -- we should update 'toSpecKeys' also,

  updateBridgeWithBlock block bridge = bridge {
        toImplIds = toImplIds bridge <> dualBlockBridge block
      }

  updateBridgeWithTx genTx bridge = bridge {
        toImplIds = toImplIds bridge <> vDualGenTxBridge genTx
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
    mapKey :: Spec.VKeyGenesis -> (PBftVerKeyHash PBftByronCrypto, Spec.VKey)
    mapKey (Spec.VKeyGenesis vkey) = (
          hashVerKey $ VerKeyByronDSIGN (Spec.Test.elaborateVKey vkey)
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
                -> PBftVerKeyHash PBftByronCrypto -> Spec.VKey
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
  :: HasCallStack
  => TopLevelConfig DualByronBlock
  -> BlockNo                              -- ^ Current block number
  -> SlotNo                               -- ^ Current slot number
  -> TickedLedgerState DualByronBlock     -- ^ Ledger
  -> [Validated (GenTx DualByronBlock)]   -- ^ Txs to add in the block
  -> PBftIsLeader PBftByronCrypto         -- ^ Leader proof ('IsLeader')
  -> DualByronBlock
forgeDualByronBlock cfg curBlockNo curSlotNo tickedLedger vtxs isLeader =
    -- NOTE: We do not /elaborate/ the real Byron block from the spec one, but
    -- instead we /forge/ it. This is important, because we want to test that
    -- codepath. This does mean that we do not get any kind of "bridge" between
    -- the two blocks (which we would have gotten if we would have elaborated
    -- the block instead). Fortunately, this is okay, since the bridge for the
    -- block can be computed from the bridge information of all of the txs.
    DualBlock {
        dualBlockMain   = main
      , dualBlockAux    = Just aux
      , dualBlockBridge = mconcat $ map vDualGenTxBridge vtxs
      }
  where
    main :: ByronBlock
    main = forgeByronBlock
             (dualTopLevelConfigMain cfg)
             TxLimits.noOverrides
             curBlockNo
             curSlotNo
             (tickedDualLedgerStateMain tickedLedger)
             (map vDualGenTxMain vtxs)
             isLeader

    aux :: ByronSpecBlock
    aux = forgeByronSpecBlock
            curBlockNo
            curSlotNo
            (tickedDualLedgerStateAux tickedLedger)
            (map vDualGenTxAux vtxs)
            (bridgeToSpecKey
               (tickedDualLedgerStateBridge tickedLedger)
               (hashVerKey . deriveVerKeyDSIGN . pbftIsLeaderSignKey $ isLeader))
