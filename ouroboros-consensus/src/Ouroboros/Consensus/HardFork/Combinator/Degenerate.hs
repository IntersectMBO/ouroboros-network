{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.HardFork.Combinator.Degenerate (
    DegenFork(..)
  , DegenForkProtocol
    -- * Type families
  , Header(..)
  , BlockConfig(..)
  , ConsensusConfig(..)
  , LedgerState(..)
  , GenTx(..)
  , TxId(..)
  , CodecConfig(..)
  , NestedCtxt_(..)
    -- * Newtype wrappers
  , DegenForkChainDepState(..)
  , DegenForkHeaderHash(..)
  , DegenForkApplyTxErr(..)
    -- * Test support
  , projCfg
  ) where

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.Type.Equality
import           Data.Typeable
import           Data.Void

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query ()
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Node ()
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

{-------------------------------------------------------------------------------
  Data family instances
-------------------------------------------------------------------------------}

instance NoHardForks b => GetHeader (DegenFork b) where
  newtype Header (DegenFork b) = DHdr {
        unDHdr :: Header (HardForkBlock '[b])
      }
    deriving (Show, NoUnexpectedThunks)

  getHeader (DBlk b) = DHdr (getHeader b)

  blockMatchesHeader (DHdr hdr) (DBlk blk) =
      blockMatchesHeader (project hdr) (project' (Proxy @(I b)) blk)

  headerIsEBB (DHdr hdr) = headerIsEBB (project hdr)

newtype instance BlockConfig (DegenFork b) = DBCfg {
      unDBCfg :: BlockConfig (HardForkBlock '[b])
    }
  deriving (NoUnexpectedThunks)

newtype instance CodecConfig (DegenFork b) = DCCfg {
      unDCCfg :: CodecConfig (HardForkBlock '[b])
    }
  deriving (NoUnexpectedThunks)

newtype instance ConsensusConfig (DegenForkProtocol b) = DConCfg {
      unDConCfg :: ConsensusConfig (HardForkProtocol '[b])
    }
  deriving (NoUnexpectedThunks)

newtype instance LedgerState (DegenFork b) = DLgr {
      unDLgr :: LedgerState (HardForkBlock '[b])
    }
  deriving (Eq, Show, NoUnexpectedThunks)

instance ConfigSupportsNode b => ConfigSupportsNode (DegenFork b) where
  getSystemStart     = getSystemStart     . unDBCfg
  getNetworkMagic    = getNetworkMagic    . unDBCfg
  getProtocolMagicId = getProtocolMagicId . unDBCfg

{-------------------------------------------------------------------------------
  Forward HasHeader instances
-------------------------------------------------------------------------------}

newtype DegenForkHeaderHash b = DHash {
      unDHash :: HeaderHash (HardForkBlock '[b])
    }
  deriving (Eq, Ord, Show, Typeable, NoUnexpectedThunks, Serialise)

type instance HeaderHash (DegenFork b) = DegenForkHeaderHash b

instance SingleEraBlock b => StandardHash (DegenFork b)

instance NoHardForks b => Measured BlockMeasure (DegenFork b) where
  measure = blockMeasure

instance NoHardForks b => HasHeader (DegenFork b) where
  getHeaderFields = getBlockHeaderFields

instance NoHardForks b => HasHeader (Header (DegenFork b)) where
  getHeaderFields (DHdr hdr) = HeaderFields {
        headerFieldHash    = DHash headerFieldHash
      , headerFieldSlot    = headerFieldSlot
      , headerFieldBlockNo = headerFieldBlockNo
      }
    where
      HeaderFields{..} = getHeaderFields hdr

instance NoHardForks b => GetPrevHash (DegenFork b) where
  headerPrevHash = castHash . headerPrevHash . unDHdr

{-------------------------------------------------------------------------------
  Forward the 'ConsensusProtocol' instance
-------------------------------------------------------------------------------}

data DegenForkProtocol b

type instance BlockProtocol (DegenFork b) = DegenForkProtocol b

newtype DegenForkChainDepState b = DCSt {
      unDCSt :: ChainDepState (HardForkProtocol '[b])
    }
deriving instance SingleEraBlock b => Eq                 (DegenForkChainDepState b)
deriving instance SingleEraBlock b => Show               (DegenForkChainDepState b)
deriving instance SingleEraBlock b => NoUnexpectedThunks (DegenForkChainDepState b)

instance SingleEraBlock b => ChainSelection (DegenForkProtocol b) where
  type ChainSelConfig (DegenForkProtocol b) = ChainSelConfig (HardForkProtocol '[b])
  type SelectView     (DegenForkProtocol b) = SelectView     (HardForkProtocol '[b])
  preferCandidate   _ = preferCandidate   (Proxy @(HardForkProtocol '[b]))
  compareCandidates _ = compareCandidates (Proxy @(HardForkProtocol '[b]))

instance SingleEraBlock b => HasChainIndepState (DegenForkProtocol b) where
  type ChainIndepStateConfig (DegenForkProtocol b) = ChainIndepStateConfig (HardForkProtocol '[b])
  type ChainIndepState       (DegenForkProtocol b) = ChainIndepState       (HardForkProtocol '[b])

  -- Operations on the chain independent state
  updateChainIndepState _ = updateChainIndepState (Proxy @(HardForkProtocol '[b]))

instance SingleEraBlock b => ConsensusProtocol (DegenForkProtocol b) where
  -- The reason for introducing a separate 'DegenForkProtocol' instead of:
  --
  -- > type instance BlockProtocol (DegenFork b) = BlockProtocol (HardForkBlock '[b])
  --
  -- is that we need to wrap the 'ChainDepState' in a newtype so that we can
  -- define non-orphan serialisation instances for it. The orphan instances
  -- would be /bad orphans/, i.e., for @HardForkChainDepState '[b]@.
  type ChainDepState (DegenForkProtocol b) = DegenForkChainDepState b
  type ValidationErr (DegenForkProtocol b) = ValidationErr   (HardForkProtocol '[b])
  type LedgerView    (DegenForkProtocol b) = LedgerView      (HardForkProtocol '[b])
  type CanBeLeader   (DegenForkProtocol b) = CanBeLeader     (HardForkProtocol '[b])
  type CannotLead    (DegenForkProtocol b) = CannotLead      (HardForkProtocol '[b])
  type IsLeader      (DegenForkProtocol b) = IsLeader        (HardForkProtocol '[b])
  type ValidateView  (DegenForkProtocol b) = ValidateView    (HardForkProtocol '[b])

  -- Operations on the state
  checkIsLeader (DConCfg cfg) canBeLeader tickedLedgerView chainIndepState (DCSt chainDepState) =
    castLeaderCheck <$>
      checkIsLeader cfg canBeLeader tickedLedgerView chainIndepState chainDepState
  updateChainDepState (DConCfg cfg) tickedLedgerView valView (DCSt chainDepState) =
    DCSt <$> updateChainDepState cfg tickedLedgerView valView chainDepState
  rewindChainDepState _ secParam pt (DCSt chainDepState) =
    DCSt <$>
      rewindChainDepState
        (Proxy @(HardForkProtocol '[b]))
        secParam
        pt
        chainDepState

  -- Straight-forward extensions
  protocolSecurityParam = protocolSecurityParam . unDConCfg

  -- Extract 'ChainSelConfig'
  chainSelConfig = chainSelConfig . unDConCfg

{-------------------------------------------------------------------------------
  Forward 'HardForkBlock' instances
-------------------------------------------------------------------------------}

type instance LedgerCfg (LedgerState (DegenFork b)) = LedgerCfg (LedgerState (HardForkBlock '[b]))

instance SingleEraBlock b => IsLedger (LedgerState (DegenFork b)) where
  type LedgerErr (LedgerState (DegenFork b)) = LedgerErr (LedgerState (HardForkBlock '[b]))

  applyChainTick cfg slot (DLgr lgr) = DLgr <$> applyChainTick cfg slot lgr

  ledgerTipPoint (DLgr l) =
    (castPoint :: Point (LedgerState (HardForkBlock '[b])) -> Point (LedgerState (DegenFork b))) $
    ledgerTipPoint l

instance NoHardForks b => ApplyBlock (LedgerState (DegenFork b)) (DegenFork b) where
  applyLedgerBlock cfg (DBlk b) (Ticked slot (DLgr lgr)) =
    DLgr <$> applyLedgerBlock cfg b (Ticked slot lgr)
  reapplyLedgerBlock cfg (DBlk b) (Ticked slot (DLgr lgr)) =
    DLgr $ reapplyLedgerBlock cfg b (Ticked slot lgr)

instance NoHardForks b => UpdateLedger (DegenFork b)

instance SingleEraBlock b => HasHardForkHistory (DegenFork b) where
  type HardForkIndices (DegenFork b) = '[b]

  hardForkSummary cfg (DLgr lgr) = hardForkSummary cfg lgr

instance SingleEraBlock b => HasAnnTip (DegenFork b) where
  type TipInfo (DegenFork b) = TipInfo (HardForkBlock '[b])

  tipInfoHash _ = DHash . tipInfoHash (Proxy @(HardForkBlock '[b]))
  getTipInfo (DHdr hdr) = getTipInfo hdr

instance NoHardForks b => BasicEnvelopeValidation (DegenFork b) where
  expectedFirstBlockNo  _ = expectedFirstBlockNo  (Proxy @(HardForkBlock '[b]))
  minimumPossibleSlotNo _ = minimumPossibleSlotNo (Proxy @(HardForkBlock '[b]))
  expectedNextBlockNo   _ = expectedNextBlockNo   (Proxy @(HardForkBlock '[b]))
  minimumNextSlotNo     _ = minimumNextSlotNo     (Proxy @(HardForkBlock '[b]))

instance NoHardForks b => ValidateEnvelope (DegenFork b) where
  type OtherHeaderEnvelopeError (DegenFork b) = OtherHeaderEnvelopeError (HardForkBlock '[b])

  additionalEnvelopeChecks cfg view (DHdr hdr) =
      withExcept (inject' (Proxy @(WrapEnvelopeErr b))) $
        additionalEnvelopeChecks
          (projCfg cfg)
          (project' (Proxy @(WrapLedgerView b)) <$> view)
          (project hdr)

instance NoHardForks b => BlockSupportsProtocol (DegenFork b) where
  validateView (DBCfg cfg) (DHdr hdr) = validateView cfg hdr
  selectView   (DBCfg cfg) (DHdr hdr) = selectView   cfg hdr

instance NoHardForks b => LedgerSupportsProtocol (DegenFork b) where
  protocolLedgerView   cfg (DLgr lgr) = protocolLedgerView   cfg lgr
  ledgerViewForecastAt cfg (DLgr lgr) = ledgerViewForecastAt cfg lgr

newtype DegenForkApplyTxErr b = DApplyTxErr {
      unDApplyTxErr :: ApplyTxErr (HardForkBlock '[b])
    }
  deriving (Show)

instance NoHardForks b => LedgerSupportsMempool (DegenFork b) where
  newtype GenTx (DegenFork b) = DTx {
        unDTx :: GenTx (HardForkBlock '[b])
      }
    deriving (Show, NoUnexpectedThunks)

  type ApplyTxErr (DegenFork b) = DegenForkApplyTxErr b

  txInvariant = txInvariant . unDTx

  applyTx cfg (DTx tx) (Ticked slot (DLgr lgr)) =
    withExcept DApplyTxErr $
      fmap DLgr <$> applyTx cfg tx (Ticked slot lgr)
  reapplyTx cfg (DTx tx) (Ticked slot (DLgr lgr)) =
    withExcept DApplyTxErr $
      fmap DLgr <$> reapplyTx cfg tx (Ticked slot lgr)

  maxTxCapacity (Ticked slot (DLgr lgr)) =
    maxTxCapacity (Ticked slot (project lgr))

  txInBlockSize (DTx tx) = txInBlockSize (project tx)

instance SingleEraBlock b => HasTxId (GenTx (DegenFork b)) where
  newtype TxId (GenTx (DegenFork b)) = DTxId {
        unDTxId :: TxId (GenTx (HardForkBlock '[b]))
      }
    deriving (Show, Eq, Ord, NoUnexpectedThunks)

  txId (DTx tx) = DTxId (txId tx)

instance SingleEraBlock b => ShowQuery (Query (DegenFork b)) where
  showResult (DQry qry) = showResult qry

instance NoHardForks b => QueryLedger (DegenFork b) where
  newtype Query (DegenFork b) result = DQry {
        unDQry :: Query (HardForkBlock '[b]) result
      }
    deriving (Show)

  answerQuery cfg (DQry qry) (DLgr lgr) = answerQuery cfg qry lgr
  eqQuery (DQry qry1) (DQry qry2) = eqQuery qry1 qry2

instance NoHardForks b => CommonProtocolParams (DegenFork b) where
  maxHeaderSize (DLgr lgr) = maxHeaderSize (project lgr)
  maxTxSize     (DLgr lgr) = maxTxSize     (project lgr)

instance NoHardForks b => CanForge (DegenFork b) where
  type ExtraForgeState (DegenFork b) = ExtraForgeState (HardForkBlock '[b])

  forgeBlock cfg forgeState block (Ticked slot (DLgr lgr)) txs proof =
      DBlk . inject' (Proxy @(I b)) $
        forgeBlock
          (projCfg cfg)
          (project (castForgeState forgeState))
          block
          (Ticked slot (project lgr))
          (map (project . unDTx) txs)
          (project' (Proxy @(WrapIsLeader b)) proof)

instance HasTxs b => HasTxs (DegenFork b) where
  extractTxs = map DTx . extractTxs . unDBlk

instance SingleEraBlock b => ConvertRawHash (DegenFork b) where
  toRawHash   _ =         toRawHash   (Proxy @(HardForkBlock '[b])) . unDHash
  fromRawHash _ = DHash . fromRawHash (Proxy @(HardForkBlock '[b]))
  hashSize    _ =          hashSize   (Proxy @(HardForkBlock '[b]))

{-------------------------------------------------------------------------------
  Serialisation instances

  As discussed in the module header, for this we delegate to @b@, rather than
  to @HardForkBlock '[b]@

  -- TODO go through @HardForkBlock '[b]@
-------------------------------------------------------------------------------}

-- Disk

instance (SerialiseDiskConstraints b, NoHardForks b) => ImmDbSerialiseConstraints (DegenFork b)
instance (SerialiseDiskConstraints b, NoHardForks b) => LgrDbSerialiseConstraints (DegenFork b)
instance (SerialiseDiskConstraints b, NoHardForks b) => VolDbSerialiseConstraints (DegenFork b)
instance (SerialiseDiskConstraints b, NoHardForks b) => SerialiseDiskConstraints  (DegenFork b)

defaultEncodeDisk
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , EncodeDisk b (f b)
     )
  => Proxy (f b) -> CodecConfig (DegenFork b) -> x -> Encoding
defaultEncodeDisk p (DCCfg ccfg) x =
    encodeDisk (project ccfg) (project' p x :: f b)

defaultDecodeDisk
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , DecodeDisk b (f b)
     )
  => Proxy (f b) -> CodecConfig (DegenFork b) -> forall s. Decoder s x
defaultDecodeDisk _ (DCCfg ccfg) =
    coerce . inject <$> decodeDisk @b @(f b) (project ccfg)

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDisk (DegenFork b) (DegenFork b) where
  encodeDisk = defaultEncodeDisk (Proxy @(I b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDisk (DegenFork b) (Lazy.ByteString -> DegenFork b) where
  decodeDisk = defaultDecodeDisk (Proxy @(Lazy.ByteString -> b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDisk (DegenFork b) (DegenForkChainDepState b) where
  encodeDisk = defaultEncodeDisk (Proxy @(WrapChainDepState b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDisk (DegenFork b) (DegenForkChainDepState b) where
  decodeDisk = defaultDecodeDisk (Proxy @(WrapChainDepState b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDisk (DegenFork b) (LedgerState (DegenFork b)) where
  encodeDisk = defaultEncodeDisk (Proxy @(LedgerState b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDisk (DegenFork b) (LedgerState (DegenFork b)) where
  decodeDisk = defaultDecodeDisk (Proxy @(LedgerState b))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDisk (DegenFork b) (AnnTip (DegenFork b)) where
  encodeDisk cfg =
        defaultEncodeDisk (Proxy @(AnnTip b)) cfg
      . (castAnnTip :: AnnTip (DegenFork b) -> AnnTip (HardForkBlock '[b]))

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDisk (DegenFork b) (AnnTip (DegenFork b)) where
  decodeDisk =
      fmap (castAnnTip :: AnnTip (HardForkBlock '[b]) -> AnnTip (DegenFork b))
    . defaultDecodeDisk (Proxy @(AnnTip b))

-- NodeToNode

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNodeConstraints (DegenFork b)

defaultEncodeNodeToNode
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , SerialiseNodeToNode b (f b)
     )
  => Proxy (f b)
  -> CodecConfig (DegenFork b) -> BlockNodeToNodeVersion (DegenFork b)
  -> x -> Encoding
defaultEncodeNodeToNode p (DCCfg ccfg) version x =
    encodeNodeToNode (project ccfg) version (project' p x :: f b)

defaultDecodeNodeToNode
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , SerialiseNodeToNode b (f b)
     )
  => Proxy (f b)
  -> CodecConfig (DegenFork b) -> BlockNodeToNodeVersion (DegenFork b)
  -> forall s. Decoder s x
defaultDecodeNodeToNode _ (DCCfg ccfg) version =
    coerce . inject <$> decodeNodeToNode @b @(f b) (project ccfg) version

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (DegenFork b) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(I b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(I b))

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (Header (DegenFork b)) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(Header b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(Header b))

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (Serialised (DegenFork b)) where
  encodeNodeToNode (DCCfg ccfg) version (Serialised bytes) =
      encodeNodeToNode
        (project ccfg)
        version
        (Serialised bytes :: Serialised b)
  decodeNodeToNode (DCCfg ccfg) version =
      (\(Serialised bytes) -> Serialised bytes) <$>
        decodeNodeToNode @b @(Serialised b) (project ccfg) version

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (SerialisedHeader (DegenFork b)) where
  encodeNodeToNode (DCCfg ccfg) version serialisedHeader =
      encodeNodeToNode
        (project ccfg)
        version
        (project $ castSerialisedHeader unDCtxt serialisedHeader)
  decodeNodeToNode (DCCfg ccfg) version =
      (castSerialisedHeader DCtxt . inject) <$>
        decodeNodeToNode (project ccfg) version

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (GenTx (DegenFork b)) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(GenTx b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(GenTx b))

instance (SerialiseNodeToNodeConstraints b, NoHardForks b)
       => SerialiseNodeToNode (DegenFork b) (GenTxId (DegenFork b)) where
  encodeNodeToNode = defaultEncodeNodeToNode (Proxy @(WrapGenTxId b))
  decodeNodeToNode = defaultDecodeNodeToNode (Proxy @(WrapGenTxId b))

-- NodeToClient

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClientConstraints (DegenFork b)

defaultEncodeNodeToClient
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , SerialiseNodeToClient b (f b)
     )
  => Proxy (f b)
  -> CodecConfig (DegenFork b) -> BlockNodeToClientVersion (DegenFork b)
  -> x -> Encoding
defaultEncodeNodeToClient p (DCCfg ccfg) version x =
    encodeNodeToClient (project ccfg) version (project' p x :: f b)

defaultDecodeNodeToClient
  :: forall x f b.
     ( Coercible x (f (HardForkBlock '[b]))
     , Isomorphic f
     , NoHardForks b
     , SerialiseNodeToClient b (f b)
     )
  => Proxy (f b)
  -> CodecConfig (DegenFork b) -> BlockNodeToClientVersion (DegenFork b)
  -> forall s. Decoder s x
defaultDecodeNodeToClient _ (DCCfg ccfg) version =
    coerce . inject <$> decodeNodeToClient @b @(f b) (project ccfg) version

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (DegenFork b) where
  encodeNodeToClient = defaultEncodeNodeToClient (Proxy @(I b))
  decodeNodeToClient = defaultDecodeNodeToClient (Proxy @(I b))

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (Serialised (DegenFork b)) where
  encodeNodeToClient (DCCfg ccfg) version (Serialised bytes) =
      encodeNodeToClient
        (project ccfg)
        version
        (Serialised bytes :: Serialised b)
  decodeNodeToClient (DCCfg ccfg) version =
      (\(Serialised bytes) -> Serialised bytes) <$>
        decodeNodeToClient @b @(Serialised b) (project ccfg) version

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (GenTx (DegenFork b)) where
  encodeNodeToClient = defaultEncodeNodeToClient (Proxy @(GenTx b))
  decodeNodeToClient = defaultDecodeNodeToClient (Proxy @(GenTx b))

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (DegenForkApplyTxErr b) where
  encodeNodeToClient = defaultEncodeNodeToClient (Proxy @(WrapApplyTxErr b))
  decodeNodeToClient = defaultDecodeNodeToClient (Proxy @(WrapApplyTxErr b))

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseNodeToClient (DegenFork b) (SomeBlock Query (DegenFork b)) where
  encodeNodeToClient (DCCfg ccfg) version (SomeBlock (DQry qry)) =
      projQuery qry $ \_pf qry' ->
        encodeNodeToClient
          (project ccfg)
          version
          (SomeBlock qry')
  decodeNodeToClient (DCCfg ccfg) version =
      (\(SomeBlock qry) -> SomeBlock (DQry $ injQuery qry)) <$>
        decodeNodeToClient @b @(SomeBlock Query b) (project ccfg) version

instance (SerialiseNodeToClientConstraints b, NoHardForks b)
       => SerialiseResult (DegenFork b) (Query (DegenFork b)) where
  encodeResult (DCCfg ccfg) version (DQry qry) mResult =
      projQuery qry $ \Refl qry' ->
        case mResult of
          Right result -> encodeResult (project ccfg) version qry' result
          Left  err    -> absurd $ mismatchOneEra err
  decodeResult (DCCfg ccfg) version (DQry qry) =
      projQuery qry $ \Refl qry' ->
        Right <$> decodeResult (project ccfg) version qry'

{-------------------------------------------------------------------------------
  Nested contents
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (DegenFork b) f a where
  DCtxt :: NestedCtxt_ (HardForkBlock '[b]) f a
        -> NestedCtxt_ (DegenFork b)        f a

unDCtxt
  :: NestedCtxt_ (DegenFork b)        f a
  -> NestedCtxt_ (HardForkBlock '[b]) f a
unDCtxt (DCtxt ctxt) = ctxt

deriving instance SingleEraBlock b => Show (NestedCtxt_ (DegenFork b) Header a)

instance SingleEraBlock b => SameDepIndex (NestedCtxt_ (DegenFork b) Header) where
  sameDepIndex (DCtxt ctxt1) (DCtxt ctxt2) =
      sameDepIndex ctxt1 ctxt2

instance SingleEraBlock b => HasNestedContent Header (DegenFork b) where
  unnest hdr = case unnest (unDHdr hdr) of
      DepPair ctxt a -> DepPair (mapNestedCtxt DCtxt ctxt) a
  nest (DepPair ctxt a) =
      DHdr $ nest (DepPair (mapNestedCtxt unDCtxt ctxt) a)

instance NoHardForks b => ReconstructNestedCtxt Header (DegenFork b) where
  reconstructPrefixLen _ =
      reconstructPrefixLen (Proxy @(Header b))

  reconstructNestedCtxt _ prefix blockSize =
      mapSomeNestedCtxt DCtxt . inject $
        reconstructNestedCtxt (Proxy @(Header b)) prefix blockSize

instance (SerialiseDiskConstraints b, NoHardForks b)
      => EncodeDiskDep (NestedCtxt Header) (DegenFork b) where
  encodeDiskDep (DCCfg ccfg) =
      encodeDiskDep (project ccfg) . projNestedCtxt . mapNestedCtxt unDCtxt

instance (SerialiseDiskConstraints b, NoHardForks b)
      => DecodeDiskDep (NestedCtxt Header) (DegenFork b) where
  decodeDiskDep (DCCfg ccfg) =
      decodeDiskDep (project ccfg) . projNestedCtxt . mapNestedCtxt unDCtxt

{-------------------------------------------------------------------------------
  RunNode instance

  As discussed in the module header, for this we delegate to @b@, rather than
  to @HardForkBlock '[b]@
-------------------------------------------------------------------------------}

projCfg :: NoHardForks b => TopLevelConfig (DegenFork b) -> TopLevelConfig b
projCfg = project . castTopLevelConfig

instance HasNetworkProtocolVersion b => HasNetworkProtocolVersion (DegenFork b) where
  type BlockNodeToNodeVersion   (DegenFork b) = BlockNodeToNodeVersion   b
  type BlockNodeToClientVersion (DegenFork b) = BlockNodeToClientVersion b

instance TranslateNetworkProtocolVersion b => TranslateNetworkProtocolVersion (DegenFork b) where
  supportedNodeToNodeVersions     _ = supportedNodeToNodeVersions     (Proxy @b)
  supportedNodeToClientVersions   _ = supportedNodeToClientVersions   (Proxy @b)
  mostRecentSupportedNodeToNode   _ = mostRecentSupportedNodeToNode   (Proxy @b)
  mostRecentSupportedNodeToClient _ = mostRecentSupportedNodeToClient (Proxy @b)
  nodeToNodeProtocolVersion       _ = nodeToNodeProtocolVersion       (Proxy @b)
  nodeToClientProtocolVersion     _ = nodeToClientProtocolVersion     (Proxy @b)

instance (NoHardForks b, RunNode b) => RunNode (DegenFork b) where
  nodeBlockFetchSize (DHdr hdr) = nodeBlockFetchSize (project hdr)

  nodeImmDbChunkInfo cfg = nodeImmDbChunkInfo (projCfg cfg)

  nodeGetBinaryBlockInfo (DBlk blk) =
      nodeGetBinaryBlockInfo (project' (Proxy @(I b)) blk :: b)

  nodeInitChainDB cfg initDB =
      nodeInitChainDB
        (projCfg cfg)
        (project (InitChainDB.cast initDB))

  nodeCheckIntegrity cfg (DBlk blk) =
      nodeCheckIntegrity (projCfg cfg) (project' (Proxy @(I b)) blk)
