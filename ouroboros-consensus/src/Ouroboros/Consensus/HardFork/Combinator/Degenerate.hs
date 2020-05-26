{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.HardFork.Combinator.Degenerate (
    DegenFork(..)
    -- * Type families
  , Header(..)
  , BlockConfig(..)
  , LedgerState(..)
  , GenTx(..)
  , TxId(..)
  , CodecConfig(..)
    -- * Test support
  , projCfg
  ) where

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Except
import           Data.FingerTree.Strict (Measured (..))
import           Data.Function (on)
import           Data.Proxy
import           Data.Type.Equality
import           Data.Void

import           Ouroboros.Network.Block
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB

import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query ()
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import           Ouroboros.Consensus.HardFork.Combinator.Unary

-- | Degenerate hard fork with a single era
--
-- NOTE: It is important to realize that in general
--
-- > HardForkBlock '[b]
--
-- and
--
-- > DegenFork b
--
-- may behave differently. Crucially, they might have
--
-- * different serialization formats, where the former uses a serialization
--   format that is forward-compatible with hard fork transitions, whereas
--   the latter may well not be
-- * related to the previous point, it will have its own network protocol
--   versioning
--
-- The main use of 'DegenFork' is for testing, and as evidence that all
-- type class instances that are required for the hard fork are present.
newtype DegenFork b = DBlk {
      unDBlk :: HardForkBlock '[b]
    }
  deriving (Eq, Show)

type instance BlockProtocol (DegenFork b) = BlockProtocol (HardForkBlock '[b])

{-------------------------------------------------------------------------------
  Data family instances
-------------------------------------------------------------------------------}

instance SingleEraBlock b => GetHeader (DegenFork b) where
  newtype Header (DegenFork b) = DHdr {
        unDHdr :: Header (HardForkBlock '[b])
      }
    deriving (Show, NoUnexpectedThunks)

  getHeader (DBlk b) = DHdr (getHeader b)

newtype instance CodecConfig (DegenFork b) = DCCfg {
      unDCCfg :: CodecConfig (HardForkBlock '[b])
    }

newtype instance BlockConfig (DegenFork b) = DBCfg {
      unDBCfg :: BlockConfig (HardForkBlock '[b])
    }
  deriving (NoUnexpectedThunks)

newtype instance LedgerState (DegenFork b) = DLgr {
      unDLgr :: LedgerState (HardForkBlock '[b])
    }
  deriving (Eq, Show, NoUnexpectedThunks)

instance SingleEraBlock b => BlockHasCodecConfig (DegenFork b) where
  getCodecConfig = DCCfg . getCodecConfig . unDBCfg

{-------------------------------------------------------------------------------
  Forward HasHeader instances
-------------------------------------------------------------------------------}

type instance HeaderHash (DegenFork b) = HeaderHash (HardForkBlock '[b])

instance SingleEraBlock b => StandardHash (DegenFork b)

instance SingleEraBlock b => Measured BlockMeasure (DegenFork b) where
  measure = blockMeasure

instance SingleEraBlock b => HasHeader (DegenFork b) where
    blockHash      =            blockHash     . unDBlk
    blockPrevHash  = castHash . blockPrevHash . unDBlk
    blockSlot      =            blockSlot     . unDBlk
    blockNo        =            blockNo       . unDBlk

    blockInvariant = const True

instance SingleEraBlock b => HasHeader (Header (DegenFork b)) where
  blockHash      =            blockHash     . unDHdr
  blockPrevHash  = castHash . blockPrevHash . unDHdr
  blockSlot      =            blockSlot     . unDHdr
  blockNo        =            blockNo       . unDHdr
  blockInvariant = const True

{-------------------------------------------------------------------------------
  Forward 'HardForkBlock' instances
-------------------------------------------------------------------------------}

type instance LedgerCfg (LedgerState (DegenFork b)) = LedgerCfg (LedgerState (HardForkBlock '[b]))

instance SingleEraBlock b => IsLedger (LedgerState (DegenFork b)) where
  type LedgerErr (LedgerState (DegenFork b)) = LedgerErr (LedgerState (HardForkBlock '[b]))

  applyChainTick cfg slot (DLgr lgr) = DLgr <$> applyChainTick cfg slot lgr

instance SingleEraBlock b => ApplyBlock (LedgerState (DegenFork b)) (DegenFork b) where
  applyLedgerBlock cfg (DBlk b) (Ticked slot (DLgr lgr)) =
    DLgr <$> applyLedgerBlock cfg b (Ticked slot lgr)
  reapplyLedgerBlock cfg (DBlk b) (Ticked slot (DLgr lgr)) =
    DLgr $ reapplyLedgerBlock cfg b (Ticked slot lgr)
  ledgerTipPoint (DLgr l) =
    (castPoint :: Point (HardForkBlock '[b]) -> Point (DegenFork b)) $ ledgerTipPoint l

instance SingleEraBlock b => UpdateLedger (DegenFork b)

instance SingleEraBlock b => HasHardForkHistory (DegenFork b) where
  type HardForkIndices (DegenFork b) = '[b]

  hardForkSummary start cfg (DLgr lgr) = hardForkSummary start cfg lgr

instance SingleEraBlock b => HasAnnTip (DegenFork b) where
  type TipInfo (DegenFork b) = TipInfo (HardForkBlock '[b])

  tipInfoHash _ = tipInfoHash (Proxy @(HardForkBlock '[b]))
  getTipInfo (DHdr hdr) = getTipInfo hdr

instance SingleEraBlock b => BasicEnvelopeValidation (DegenFork b) where
  expectedFirstBlockNo  _ = expectedFirstBlockNo  (Proxy @b)
  minimumPossibleSlotNo _ = minimumPossibleSlotNo (Proxy @b)
  expectedNextBlockNo   _ = expectedNextBlockNo   (Proxy @b) `on` projTipInfo
  minimumNextSlotNo     _ = minimumNextSlotNo     (Proxy @b) `on` projTipInfo

instance SingleEraBlock b => ValidateEnvelope (DegenFork b) where
  type OtherHeaderEnvelopeError (DegenFork b) = OtherHeaderEnvelopeError (HardForkBlock '[b])

  additionalEnvelopeChecks cfg view (DHdr hdr) =
      withExcept injEnvelopeErr $
        additionalEnvelopeChecks
          (projCfg cfg)
          (projLedgerView (Proxy @b) <$> view)
          (projHeader hdr)

instance SingleEraBlock b => BlockSupportsProtocol (DegenFork b) where
  validateView (DBCfg cfg) (DHdr hdr) = validateView cfg hdr
  selectView   (DBCfg cfg) (DHdr hdr) = selectView   cfg hdr

instance SingleEraBlock b => LedgerSupportsProtocol (DegenFork b) where
  protocolLedgerView   cfg (DLgr lgr) = protocolLedgerView   cfg lgr
  ledgerViewForecastAt cfg (DLgr lgr) = ledgerViewForecastAt cfg lgr

instance SingleEraBlock b => ApplyTx (DegenFork b) where
  newtype GenTx (DegenFork b) = DTx {
        unDTx :: GenTx (HardForkBlock '[b])
      }
    deriving (Show, NoUnexpectedThunks)

  type ApplyTxErr (DegenFork b) = ApplyTxErr (HardForkBlock '[b])

  txInvariant = txInvariant . unDTx

  applyTx cfg (DTx tx) (Ticked slot (DLgr lgr)) =
    fmap DLgr <$> applyTx cfg tx (Ticked slot lgr)
  reapplyTx cfg (DTx tx) (Ticked slot (DLgr lgr)) =
    fmap DLgr <$> reapplyTx cfg tx (Ticked slot lgr)

instance SingleEraBlock b => HasTxId (GenTx (DegenFork b)) where
  newtype TxId (GenTx (DegenFork b)) = DTxId {
        unDTxId :: TxId (GenTx (HardForkBlock '[b]))
      }
    deriving (Show, Eq, Ord, NoUnexpectedThunks)

  txId (DTx tx) = DTxId (txId tx)

instance SingleEraBlock b => ShowQuery (Query (DegenFork b)) where
  showResult (DQry qry) = showResult qry

instance SingleEraBlock b => QueryLedger (DegenFork b) where
  newtype Query (DegenFork b) result = DQry {
        unDQry :: Query (HardForkBlock '[b]) result
      }
    deriving (Show)

  answerQuery cfg (DQry qry) (DLgr lgr) = answerQuery cfg qry lgr
  eqQuery (DQry qry1) (DQry qry2) = eqQuery qry1 qry2

instance SingleEraBlock b => CanForge (DegenFork b) where
  type ForgeState (DegenFork b) = ForgeState (HardForkBlock '[b])

  forgeBlock cfg upd block (Ticked slot (DLgr lgr)) txs proof =
      (DBlk . injBlock) <$>
        forgeBlock
          (projCfg cfg)
          (projUpdateForgeState upd)
          block
          (Ticked slot (projLedgerState lgr))
          (map (projGenTx . unDTx) txs)
          (projIsLeader proof)

instance HasTxs b => HasTxs (DegenFork b) where
  extractTxs = map DTx . extractTxs . unDBlk

{-------------------------------------------------------------------------------
  RunNode instance

  As discussed in the module header, for this we delegate to @b@, rather than
  to @HardForkBlock '[b]@
-------------------------------------------------------------------------------}

projCfg :: SingleEraBlock b => TopLevelConfig (DegenFork b) -> TopLevelConfig b
projCfg = projTopLevelConfig . castTopLevelConfig

instance HasNetworkProtocolVersion b => HasNetworkProtocolVersion (DegenFork b) where
  type NodeToNodeVersion   (DegenFork b) = NodeToNodeVersion   b
  type NodeToClientVersion (DegenFork b) = NodeToClientVersion b

  -- | Enumerate all supported node-to-node versions
  supportedNodeToNodeVersions   _ = supportedNodeToNodeVersions   (Proxy @b)
  supportedNodeToClientVersions _ = supportedNodeToClientVersions (Proxy @b)
  mostRecentNodeToNodeVersion   _ = mostRecentNodeToNodeVersion   (Proxy @b)
  mostRecentNodeToClientVersion _ = mostRecentNodeToClientVersion (Proxy @b)
  nodeToNodeProtocolVersion     _ = nodeToNodeProtocolVersion     (Proxy @b)
  nodeToClientProtocolVersion   _ = nodeToClientProtocolVersion   (Proxy @b)

instance (SingleEraBlock b, FromRawHash b, RunNode b) => RunNode (DegenFork b) where
  nodeBlockMatchesHeader (DHdr hdr) (DBlk blk) = nodeBlockMatchesHeader (projHeader hdr) (projBlock blk)
  nodeBlockFetchSize     (DHdr hdr)            = nodeBlockFetchSize     (projHeader hdr)
  nodeIsEBB              (DHdr hdr)            = nodeIsEBB              (projHeader hdr)

  nodeImmDbChunkInfo  cfg = nodeImmDbChunkInfo  (projCfg cfg)
  nodeStartTime       cfg = nodeStartTime       (projCfg cfg)
  nodeNetworkMagic    cfg = nodeNetworkMagic    (projCfg cfg)
  nodeProtocolMagicId cfg = nodeProtocolMagicId (projCfg cfg)

  nodeHashInfo          _ = injHashInfo (nodeHashInfo (Proxy @b))
  nodeAddHeaderEnvelope _ = nodeAddHeaderEnvelope (Proxy @b)
  nodeExceptionIsFatal  _ = nodeExceptionIsFatal  (Proxy @b)

  nodeInitChainDB cfg initDB =
      nodeInitChainDB
        (projCfg cfg)
        (projInitChainDB (InitChainDB.cast initDB))

  nodeMaxBlockSize          (DLgr lgr) = nodeMaxBlockSize          (projLedgerState lgr)
  nodeBlockEncodingOverhead (DLgr lgr) = nodeBlockEncodingOverhead (projLedgerState lgr)
  nodeMaxTxSize             (DLgr lgr) = nodeMaxTxSize             (projLedgerState lgr)

  nodeCheckIntegrity cfg (DBlk blk) = nodeCheckIntegrity (projCfg cfg) (projBlock blk)

  nodeTxInBlockSize (DTx tx) = nodeTxInBlockSize (projGenTx tx)

  -- Encoders

  nodeEncodeBlockWithInfo (DCCfg cfg) (DBlk blk) =
      nodeEncodeBlockWithInfo (projCodecConfig cfg) (projBlock blk)
  nodeEncodeBlock (DCCfg cfg) (DBlk blk) =
      nodeEncodeBlock (projCodecConfig cfg) (projBlock blk)
  nodeEncodeHeader (DCCfg cfg) version (DHdr hdr) =
      nodeEncodeHeader (projCodecConfig cfg) (castSerialisationVersion version) (projHeader hdr)
  nodeEncodeWrappedHeader (DCCfg cfg) version (Serialised hdr) =
      nodeEncodeWrappedHeader (projCodecConfig cfg) (castSerialisationAcrossNetwork version) (Serialised hdr)
  nodeEncodeGenTx (DTx tx) =
      nodeEncodeGenTx (projGenTx tx)
  nodeEncodeGenTxId (DTxId tid) =
      nodeEncodeGenTxId (projGenTxId tid)
  nodeEncodeHeaderHash _ hash =
      nodeEncodeHeaderHash (Proxy @b) (projHeaderHash hash)
  nodeEncodeLedgerState cfg (DLgr lgr) =
      nodeEncodeLedgerState (projCfg cfg) (projLedgerState lgr)
  nodeEncodeConsensusState cfg st =
      nodeEncodeConsensusState (projCfg cfg) (projConsensusState st)
  nodeEncodeApplyTxError _ err =
      nodeEncodeApplyTxError (Proxy @b) (projApplyTxErr err)
  nodeEncodeAnnTip _ tip =
      nodeEncodeAnnTip (Proxy @b) (projAnnTip (castAnnTip tip))
  nodeEncodeQuery (DQry qry) =
      projQuery qry $ \_pf qry' -> nodeEncodeQuery qry'
  nodeEncodeResult (DQry qry) mResult =
      projQuery qry $ \Refl qry' ->
        case mResult of
          Right result -> nodeEncodeResult qry' result
          Left  err    -> absurd $ mismatchOneEra err

  -- Decoders

  nodeDecodeBlock (DCCfg cfg) =
      (\f -> DBlk . injBlock . f) <$>
        nodeDecodeBlock (projCodecConfig cfg)
  nodeDecodeHeader (DCCfg cfg) version =
      (\f -> DHdr . injHeader . f) <$>
        nodeDecodeHeader (projCodecConfig cfg) (castSerialisationVersion version)
  nodeDecodeWrappedHeader (DCCfg cfg) version =
      (\(Serialised hdr) -> Serialised hdr) <$>
        nodeDecodeWrappedHeader (projCodecConfig cfg) (castSerialisationAcrossNetwork version)
  nodeDecodeGenTx =
      (DTx . injGenTx) <$>
        nodeDecodeGenTx
  nodeDecodeGenTxId =
      (DTxId . injGenTxId) <$>
        nodeDecodeGenTxId
  nodeDecodeHeaderHash _ =
      injHeaderHash <$>
        nodeDecodeHeaderHash (Proxy @b)
  nodeDecodeLedgerState cfg =
      (DLgr . injLedgerState (nodeStartTime cfg)) <$>
        nodeDecodeLedgerState cfg'
    where
      cfg' = projCfg cfg
  nodeDecodeConsensusState cfg =
      injConsensusState (nodeStartTime cfg) <$>
        nodeDecodeConsensusState cfg'
    where
      cfg' = projCfg cfg
  nodeDecodeApplyTxError _ =
      injApplyTxErr <$>
        nodeDecodeApplyTxError (Proxy @b)
  nodeDecodeAnnTip _ =
      (castAnnTip . injAnnTip) <$>
        nodeDecodeAnnTip (Proxy @b)
  nodeDecodeQuery =
      (\(Some qry) -> Some (DQry $ injQuery qry)) <$>
        nodeDecodeQuery
  nodeDecodeResult (DQry qry) =
      projQuery qry $ \Refl qry' ->
        Right <$> nodeDecodeResult qry'
