{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- | Minimal instantiation of the consensus layer to be able to run the ChainDB
module Test.Util.TestBlock (
    -- * Blocks
    BlockConfig (..)
  , BlockQuery (..)
  , CodecConfig (..)
  , Header (..)
  , LedgerTables (..)
  , StorageConfig (..)
  , TestBlockError (..)
  , TestBlockWith (tbPayload, tbValid)
  , TestHash (TestHash)
  , Validity (..)
  , firstBlockWithPayload
  , forkBlock
  , modifyFork
  , successorBlockWithPayload
  , testHashFromList
  , unTestHash
    -- ** Test block without payload
  , TestBlock
  , firstBlock
  , successorBlock
    -- ** Payload semantics
  , PayloadSemantics (..)
    -- * LedgerState
  , LedgerState (TestLedger, payloadDependentState, lastAppliedPoint)
  , Ticked1 (TickedTestLedger)
  , getTickedTestLedger
    -- * Chain
  , BlockChain (..)
  , blockChain
  , chainToBlocks
    -- * Tree
  , BlockTree (..)
  , blockTree
  , treePreferredChain
  , treeToBlocks
  , treeToChains
    -- * Ledger infrastructure
  , singleNodeTestConfig
  , singleNodeTestConfigWith
  , singleNodeTestConfigWithK
  , testInitExtLedger
  , testInitExtLedgerWithState
  , testInitLedger
  , testInitLedgerWithState
    -- * Support for tests
  , Permutation (..)
  , permute
  ) where

import           Codec.Serialise (Serialise (..), serialise)
import           Control.DeepSeq (force)
import           Control.Monad (guard, replicateM, replicateM_)
import           Control.Monad.Except (throwError)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (for_)
import           Data.Functor.Identity (Identity (..))
import           Data.Int
import           Data.Kind (Type)
import           Data.List (transpose)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import           Data.TreeDiff (ToExpr)
import           Data.Typeable (Typeable)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import qualified System.Random as R
import           Test.QuickCheck hiding (Result)

import           Cardano.Crypto.DSIGN

import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.MockChainSel
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as InMemory
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util (ShowProxy (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Test.Util.Orphans.SignableRepresentation ()
import           Test.Util.Orphans.ToExpr ()

{-------------------------------------------------------------------------------
  Test infrastructure: test block
-------------------------------------------------------------------------------}

-- The hash represents a path through a tree (of forks) as a list of
-- 'Word64's: @[0, 1, 0]@ means we forked off at the first block.
--
-- The following tree is just a small subset of the actual tree, which allows
-- multiple forks at /each/ node:
--
-- > G--A--B--C
-- > \  \--B"-C"
-- >  \-A'-B'-C'
--
-- Some examples:
--
-- > []      = G = Genesis
-- > [0]     = A
-- > [1]     = A'
-- > [0,0]   = B
-- > [0,0,0] = C
-- > [0,1]   = B"
-- > [0,1,0] = C"
-- > [1,0]   = B'
-- > [1,0,0] = C'
-- > ...
--
-- Since the empty list represents Genesis, which does not correspond to an
-- actual block, we use a NonEmpty list.
--
-- As it is easier to prepend to and access the front of a (NonEmpty) list, we
-- store the list in reverse order, e.g., @C'@ is stored as @[0,0,1]@, but we
-- print ('Show' and 'Condense') it as @[1,0,0]@.
--
-- The predecessor (parent in the tree) can be obtained by dropping the last
-- (in the printed representation) element in the list (or the head in the
-- in-memory representation).
--
-- The 'BlockNo' of the corresponding block is just the length of the list.
newtype TestHash = UnsafeTestHash {
      unTestHash :: NonEmpty Word64
    }
  deriving stock    (Generic)
  deriving newtype  (Eq, Ord, Serialise, ToExpr)
  deriving anyclass (NoThunks)

pattern TestHash :: NonEmpty Word64 -> TestHash
pattern TestHash path <- UnsafeTestHash path where
  TestHash path = UnsafeTestHash (force path)

{-# COMPLETE TestHash #-}

testHashFromList :: [Word64] -> TestHash
testHashFromList = TestHash . NE.fromList . reverse

instance Show TestHash where
  show (TestHash h) = "(testHashFromList " <> show (reverse (NE.toList h)) <> ")"

instance Condense TestHash where
  condense = condense . reverse . NE.toList . unTestHash

data Validity = Valid | Invalid
  deriving stock    (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

instance InMemory.ReadsKeySets Identity (LedgerState TestBlock) where
  readDb (InMemory.RewoundTableKeySets seqNo NoTestLedgerTables) =
      pure $ InMemory.UnforwardedReadSets seqNo NoTestLedgerTables NoTestLedgerTables


-- | Test block parametrized on the payload type
--
-- For blocks without payload see the 'TestBlock' type alias.
--
-- By defining a 'PayloadSemantics' it is possible to obtain an 'ApplyBlock'
-- instance. See the former class for more details.
--
data TestBlockWith ptype = TestBlockWith {
      tbHash    :: TestHash
    , tbSlot    :: SlotNo
      -- ^ We store a separate 'Block.SlotNo', as slots can have gaps between
      -- them, unlike block numbers.
      --
      -- Note that when generating a 'TestBlock', you must make sure that
      -- blocks with the same 'TestHash' have the same slot number.
    , tbValid   :: Validity
      -- ^ Note that when generating a 'TestBlock', you must make sure that
      -- blocks with the same 'TestHash' have the same value for 'tbValid'.
    , tbPayload :: ptype
    }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

-- | Create the first block in the given fork, @[fork]@, with the given payload.
-- The 'SlotNo' will be 1.
firstBlockWithPayload :: Word64 -> ptype -> TestBlockWith ptype
firstBlockWithPayload forkNo payload = TestBlockWith
    { tbHash    = TestHash (forkNo NE.:| [])
    , tbSlot    = 1
    , tbValid   = Valid
    , tbPayload = payload
    }

-- | Create the successor of the given block without forking: @b -> b ++ [0]@ (in
-- the printed representation) The 'SlotNo' is increased by 1.
--
-- In Zipper parlance, this corresponds to going down in a tree.
successorBlockWithPayload ::
  TestHash -> SlotNo -> ptype -> TestBlockWith ptype
successorBlockWithPayload hash slot payload = TestBlockWith
    { tbHash    = TestHash (NE.cons 0 (unTestHash hash))
    , tbSlot    = succ slot
    , tbValid   = Valid
    , tbPayload = payload
    }

instance ShowProxy TestBlock where

newtype instance Header (TestBlockWith ptype) =
    TestHeader { testHeader :: TestBlockWith ptype }
  deriving stock (Eq, Show)
  deriving newtype (NoThunks, Serialise)

instance Typeable ptype => ShowProxy (Header (TestBlockWith ptype)) where

instance (Typeable ptype, Eq ptype) => HasHeader (Header (TestBlockWith ptype)) where
  getHeaderFields (TestHeader TestBlockWith{..}) = HeaderFields {
        headerFieldHash    = tbHash
      , headerFieldSlot    = tbSlot
      , headerFieldBlockNo = fromIntegral . NE.length . unTestHash $ tbHash
      }

instance (Typeable ptype, Eq ptype) => GetHeader (TestBlockWith ptype) where
  getHeader = TestHeader
  blockMatchesHeader (TestHeader blk') blk = blk == blk'
  headerIsEBB = const Nothing

type instance HeaderHash (TestBlockWith ptype) = TestHash

instance (Typeable ptype, Eq ptype) => HasHeader (TestBlockWith ptype) where
  getHeaderFields = getBlockHeaderFields

instance (Typeable ptype, Eq ptype) => GetPrevHash (TestBlockWith ptype) where
  headerPrevHash (TestHeader b) =
      case NE.nonEmpty . NE.tail . unTestHash . tbHash $ b of
        Nothing       -> GenesisHash
        Just prevHash -> BlockHash (TestHash prevHash)

instance StandardHash (TestBlockWith ptype)

instance (Typeable ptype, Eq ptype) => Condense (TestBlockWith ptype) where
  condense b = mconcat [
        "(H:"
      , condense (blockHash b)
      , ",S:"
      , condense (blockSlot b)
      , ",B:"
      , condense (unBlockNo (blockNo b))
      , ")"
      ]

instance (Typeable ptype, Eq ptype) => Condense (Header (TestBlockWith ptype)) where
  condense = condense . testHeader

instance Condense (ChainHash (TestBlockWith ptype)) where
  condense GenesisHash   = "genesis"
  condense (BlockHash h) = show h

data instance BlockConfig (TestBlockWith ptype) = TestBlockConfig {
      -- | Number of core nodes
      --
      -- We need this in order to compute the 'ValidateView', which must
      -- conjure up a validation key out of thin air
      testBlockNumCoreNodes :: !NumCoreNodes
    }
  deriving (Show, Generic, NoThunks)

instance HasNetworkProtocolVersion (TestBlockWith ptype) where
  -- Use defaults

instance ConfigSupportsNode (TestBlockWith ptype) where
  getSystemStart = const (SystemStart dummyDate)
    where
      --  This doesn't matter much
      dummyDate = UTCTime (fromGregorian 2019 8 13) 0

  getNetworkMagic = const (NetworkMagic 42)

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

class ( Typeable ptype
      , Eq       ptype
      , NoThunks ptype

      , Eq        (PayloadDependentState ptype EmptyMK)
      , Eq        (PayloadDependentState ptype DiffMK)
      , Eq        (PayloadDependentState ptype ValuesMK)

      , forall mk'. Show (PayloadDependentState ptype (ApplyMapKind' mk'))

      , forall mk. Generic   (PayloadDependentState ptype mk)
      ,            Serialise (PayloadDependentState ptype EmptyMK)

      , NoThunks  (PayloadDependentState ptype EmptyMK)
      , NoThunks  (PayloadDependentState ptype ValuesMK)
      , NoThunks  (PayloadDependentState ptype SeqDiffMK)
      , NoThunks  (PayloadDependentState ptype DiffMK)

      , TickedTableStuff     (LedgerState (TestBlockWith ptype))
      , StowableLedgerTables (LedgerState (TestBlockWith ptype))
      , ShowLedgerState      (LedgerState (TestBlockWith ptype))
      , SufficientSerializationForAnyBackingStore (LedgerState (TestBlockWith ptype))

      , NoThunks (LedgerTables (LedgerState (TestBlockWith ptype)) SeqDiffMK)
      , NoThunks (LedgerTables (LedgerState (TestBlockWith ptype)) ValuesMK)

      , Eq        (PayloadDependentError ptype)
      , Show      (PayloadDependentError ptype)
      , Generic   (PayloadDependentError ptype)
      , ToExpr    (PayloadDependentError ptype)
      , Serialise (PayloadDependentError ptype)
      , NoThunks  (PayloadDependentError ptype)

      , NoThunks (CodecConfig (TestBlockWith ptype))

      , NoThunks (StorageConfig (TestBlockWith ptype))
      ) => PayloadSemantics ptype where

  data PayloadDependentState ptype (mk :: MapKind) :: Type

  type PayloadDependentError ptype :: Type

  applyPayload ::
       PayloadDependentState ptype ValuesMK
    -> ptype
    -> Either (PayloadDependentError ptype) (PayloadDependentState ptype TrackingMK)

  -- | This function is used to implement the 'getBlockKeySets' function of the
  -- 'ApplyBlock' class. Thus we assume that the payload contains all the
  -- information needed to determine which keys should be retrieved from the
  -- backing store to apply a 'TestBlockWith'.
  getPayloadKeySets :: ptype -> LedgerTables (LedgerState (TestBlockWith ptype)) KeysMK

instance PayloadSemantics () where
  data PayloadDependentState () mk = EmptyPLDS
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Serialise, NoThunks)

  type PayloadDependentError () = ()

  applyPayload _ _ = Right EmptyPLDS

  getPayloadKeySets = const NoTestLedgerTables

{-------------------------------------------------------------------------------
  NestedCtxt
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (TestBlockWith ptype) f a where
  CtxtTestBlock :: NestedCtxt_ (TestBlockWith ptype) f (f (TestBlockWith ptype))

deriving instance Show (NestedCtxt_ (TestBlockWith ptype) f a)

instance TrivialDependency (NestedCtxt_ (TestBlockWith ptype) f) where
  type TrivialIndex (NestedCtxt_ (TestBlockWith ptype) f) = f (TestBlockWith ptype)
  hasSingleIndex CtxtTestBlock CtxtTestBlock = Refl
  indexIsTrivial = CtxtTestBlock

instance SameDepIndex (NestedCtxt_ (TestBlockWith ptype) f)
instance HasNestedContent f (TestBlockWith ptype)

{-------------------------------------------------------------------------------
  Test infrastructure: ledger state
-------------------------------------------------------------------------------}

type instance BlockProtocol (TestBlockWith ptype) = Bft BftMockCrypto

type instance Signed (Header (TestBlockWith ptype)) = ()
instance SignedHeader (Header (TestBlockWith ptype)) where
  headerSigned _ = ()

data TestBlockError ptype =
    -- | The hashes don't line up
    InvalidHash
      (ChainHash (TestBlockWith ptype))  -- ^ Expected hash
      (ChainHash (TestBlockWith ptype))  -- ^ Invalid hash

    -- | The block itself is invalid
  | InvalidBlock
  | InvalidPayload (PayloadDependentError ptype)

deriving stock instance Eq (PayloadDependentError ptype) => Eq (TestBlockError ptype)
deriving stock instance Show (PayloadDependentError ptype) => Show (TestBlockError ptype)
deriving stock instance Generic (TestBlockError ptype)

deriving anyclass instance
  ( Typeable ptype
  , Generic (PayloadDependentError ptype)
  , NoThunks (PayloadDependentError ptype)) => NoThunks (TestBlockError ptype)

instance ( Typeable ptype
         , Eq       ptype
         , NoThunks ptype
         , NoThunks (CodecConfig (TestBlockWith ptype))
         , NoThunks (StorageConfig (TestBlockWith ptype))
         ) => BlockSupportsProtocol (TestBlockWith ptype) where
  validateView TestBlockConfig{..} =
      bftValidateView bftFields
    where
      NumCoreNodes numCore = testBlockNumCoreNodes

      bftFields :: Header (TestBlockWith ptype) -> BftFields BftMockCrypto ()
      bftFields (TestHeader tb) = BftFields {
            bftSignature = SignedDSIGN $ mockSign () (signKey (tbSlot tb))
          }

      -- We don't want /our/ signing key, but rather the signing key of the
      -- node that produced the block
      signKey :: SlotNo -> SignKeyDSIGN MockDSIGN
      signKey (SlotNo n) = SignKeyMockDSIGN $ n `mod` numCore

type instance LedgerCfg (LedgerState TestBlock) = HardFork.EraParams

instance PayloadSemantics ptype => ShowLedgerState (LedgerState (TestBlockWith ptype)) where
  showsLedgerState _sing = shows

instance TableStuff (LedgerState TestBlock) where
  data LedgerTables (LedgerState TestBlock) mk = NoTestLedgerTables
    deriving stock    (Generic, Eq, Show)
    deriving anyclass (NoThunks)

  projectLedgerTables _                  = NoTestLedgerTables
  withLedgerTables st NoTestLedgerTables = convertMapKind st

  pureLedgerTables     _                                                          = NoTestLedgerTables
  mapLedgerTables      _                                       NoTestLedgerTables = NoTestLedgerTables
  traverseLedgerTables _                                       NoTestLedgerTables = pure NoTestLedgerTables
  zipLedgerTables      _                    NoTestLedgerTables NoTestLedgerTables = NoTestLedgerTables
  zipLedgerTables2     _ NoTestLedgerTables NoTestLedgerTables NoTestLedgerTables = NoTestLedgerTables
  zipLedgerTablesA     _                    NoTestLedgerTables NoTestLedgerTables = pure NoTestLedgerTables
  zipLedgerTables2A    _ NoTestLedgerTables NoTestLedgerTables NoTestLedgerTables = pure NoTestLedgerTables
  foldLedgerTables     _                                       NoTestLedgerTables = mempty
  foldLedgerTables2    _                    NoTestLedgerTables NoTestLedgerTables = mempty
  namesLedgerTables                                                               = NoTestLedgerTables

instance SufficientSerializationForAnyBackingStore (LedgerState TestBlock) where
    codecLedgerTables = NoTestLedgerTables

instance TickedTableStuff (LedgerState TestBlock) where
  projectLedgerTablesTicked _                         = NoTestLedgerTables
  withLedgerTablesTicked (TickedTestLedger st) tables =
      TickedTestLedger $ withLedgerTables st tables

instance ShowLedgerState (LedgerTables (LedgerState TestBlock)) where
  showsLedgerState _sing = shows

instance StowableLedgerTables (LedgerState TestBlock) where
  stowLedgerTables   (TestLedger p EmptyPLDS) = TestLedger p EmptyPLDS
  unstowLedgerTables (TestLedger p EmptyPLDS) = TestLedger p EmptyPLDS

instance PayloadSemantics ptype
         => ApplyBlock (LedgerState (TestBlockWith ptype)) (TestBlockWith ptype) where
  applyBlockLedgerResult _ tb@TestBlockWith{..} (TickedTestLedger TestLedger{..})
    | blockPrevHash tb /= pointHash lastAppliedPoint
    = throwError $ InvalidHash (pointHash lastAppliedPoint) (blockPrevHash tb)
    | tbValid == Invalid
    = throwError $ InvalidBlock
    | otherwise
    = case applyPayload payloadDependentState tbPayload of
        Left err  -> throwError $ InvalidPayload err
        Right st' -> return     $ pureLedgerResult
                                $ forgetLedgerTablesValues
                                $ TestLedger {
                                    lastAppliedPoint      = Chain.blockPoint tb
                                  , payloadDependentState = st'
                                  }

  reapplyBlockLedgerResult _ tb@TestBlockWith{..} (TickedTestLedger TestLedger{..}) =
    case applyPayload payloadDependentState tbPayload of
        Left err  -> error $ "Found an error when reapplying a block: " ++ show err
        Right st' ->              pureLedgerResult
                                $ forgetLedgerTablesValues
                                $ TestLedger {
                                    lastAppliedPoint      = Chain.blockPoint tb
                                  , payloadDependentState = st'
                                  }


  getBlockKeySets = getPayloadKeySets . tbPayload

data instance LedgerState (TestBlockWith ptype) mk =
    TestLedger {
        -- | The ledger state simply consists of the last applied block
        lastAppliedPoint      :: Point (TestBlockWith ptype)
        -- | State that depends on the application of the block payload to the
        -- state.
      , payloadDependentState :: PayloadDependentState ptype mk
      }

deriving stock instance PayloadSemantics ptype
  => Show (LedgerState (TestBlockWith ptype) (ApplyMapKind' mk))

deriving stock instance Eq (PayloadDependentState ptype EmptyMK)
  => Eq (LedgerState (TestBlockWith ptype) EmptyMK)
deriving stock instance Eq (PayloadDependentState ptype ValuesMK)
  => Eq (LedgerState (TestBlockWith ptype) ValuesMK)
deriving stock instance Eq (PayloadDependentState ptype DiffMK)
  => Eq (LedgerState (TestBlockWith ptype) DiffMK)

deriving stock instance Generic (LedgerState (TestBlockWith ptype) mk)

deriving anyclass instance PayloadSemantics ptype =>
  Serialise (LedgerState (TestBlockWith ptype) EmptyMK)
deriving anyclass instance NoThunks (PayloadDependentState ptype mk) =>
  NoThunks  (LedgerState (TestBlockWith ptype) mk)

instance InMemory (LedgerState (TestBlockWith ())) where
  convertMapKind TestLedger {lastAppliedPoint} = TestLedger lastAppliedPoint EmptyPLDS

instance InMemory (LedgerTables (LedgerState TestBlock)) where
  convertMapKind NoTestLedgerTables = NoTestLedgerTables

testInitLedgerWithState ::
  PayloadDependentState ptype mk -> LedgerState (TestBlockWith ptype) mk
testInitLedgerWithState = TestLedger GenesisPoint

-- Ticking has no effect
newtype instance Ticked1 (LedgerState (TestBlockWith ptype)) mk = TickedTestLedger {
      getTickedTestLedger :: LedgerState (TestBlockWith ptype) mk
    } deriving Generic

testInitExtLedgerWithState ::
  PayloadDependentState ptype mk -> ExtLedgerState (TestBlockWith ptype) mk
testInitExtLedgerWithState st = ExtLedgerState {
      ledgerState = testInitLedgerWithState st
    , headerState = genesisHeaderState ()
    }

type instance LedgerCfg (LedgerState (TestBlockWith ptype)) = HardFork.EraParams

instance GetTip (LedgerState (TestBlockWith ptype) mk) where
  getTip = castPoint . lastAppliedPoint

instance GetTip (Ticked1 (LedgerState (TestBlockWith ptype)) mk) where
  getTip = castPoint . lastAppliedPoint . getTickedTestLedger

instance PayloadSemantics ptype => IsLedger (LedgerState (TestBlockWith ptype)) where
  type LedgerErr (LedgerState (TestBlockWith ptype)) = TestBlockError ptype

  type AuxLedgerEvent (LedgerState (TestBlockWith ptype)) =
    VoidLedgerEvent (LedgerState (TestBlockWith ptype))

  applyChainTickLedgerResult _ _ = pureLedgerResult
                                 . TickedTestLedger
                                 . noNewTickingDiffs

instance PayloadSemantics ptype => UpdateLedger (TestBlockWith ptype)

instance InspectLedger (TestBlockWith ptype) where
  -- Defaults are fine

instance (Typeable ptype, PayloadSemantics ptype) => HasAnnTip (TestBlockWith ptype) where
  -- Use defaults

instance (Typeable ptype, PayloadSemantics ptype) => BasicEnvelopeValidation (TestBlockWith ptype) where
  -- The block number of a test block is derived from the length of the hash
  expectedFirstBlockNo _ = BlockNo 1

instance (Typeable ptype, PayloadSemantics ptype) => ValidateEnvelope (TestBlockWith ptype) where
  -- Use defaults

instance (Typeable ptype, PayloadSemantics ptype) => LedgerSupportsProtocol (TestBlockWith ptype) where
  protocolLedgerView   _ _  = TickedTrivial
  ledgerViewForecastAt _    = trivialForecast

singleNodeTestConfigWith ::
     CodecConfig (TestBlockWith ptype)
  -> StorageConfig (TestBlockWith ptype)
  -> SecurityParam -> TopLevelConfig (TestBlockWith ptype)
singleNodeTestConfigWith codecConfig storageConfig k = TopLevelConfig {
      topLevelConfigProtocol = BftConfig {
          bftParams  = BftParams { bftSecurityParam = k
                                 , bftNumNodes      = numCoreNodes
                                 }
        , bftSignKey = SignKeyMockDSIGN 0
        , bftVerKeys = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
        }
    , topLevelConfigLedger  = eraParams
    , topLevelConfigBlock   = TestBlockConfig numCoreNodes
    , topLevelConfigCodec   = codecConfig
    , topLevelConfigStorage = storageConfig
    }
  where
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes 1

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams k slotLength


{-------------------------------------------------------------------------------
  Test blocks without payload
-------------------------------------------------------------------------------}

-- | Block without payload
type TestBlock = TestBlockWith ()

-- | The 'TestBlock' does not need any codec config
data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | The 'TestBlock' does not need any storage config
data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

instance HasHardForkHistory TestBlock where
  type HardForkIndices TestBlock = '[TestBlock]
  hardForkSummary = neverForksHardForkSummary id

data instance BlockQuery TestBlock fp result where
  QueryLedgerTip :: BlockQuery TestBlock SmallL (Point TestBlock)

deriving instance Show (BlockQuery TestBlock fp result)

instance SmallQuery (BlockQuery TestBlock) where
  proveSmallQuery k = \case
    QueryLedgerTip -> k

instance QueryLedger TestBlock where
  answerBlockQuery _cfg QueryLedgerTip (ExtLedgerState TestLedger { lastAppliedPoint } _) =
    lastAppliedPoint

instance EqQuery (BlockQuery TestBlock) where
  eqQuery QueryLedgerTip QueryLedgerTip = Just Refl

instance ShowQuery (BlockQuery TestBlock) where
  showResult QueryLedgerTip = show

instance IsQuery (BlockQuery TestBlock) where

testInitLedger :: LedgerState TestBlock ValuesMK
testInitLedger = testInitLedgerWithState EmptyPLDS

testInitExtLedger :: ExtLedgerState TestBlock ValuesMK
testInitExtLedger = testInitExtLedgerWithState EmptyPLDS

-- | Trivial test configuration with a single core node
singleNodeTestConfig :: TopLevelConfig TestBlock
singleNodeTestConfig = singleNodeTestConfigWithK (SecurityParam 4)

singleNodeTestConfigWithK :: SecurityParam -> TopLevelConfig TestBlock
singleNodeTestConfigWithK = singleNodeTestConfigWith TestBlockCodecConfig TestBlockStorageConfig

{-------------------------------------------------------------------------------
  Chain of blocks (without payload)
-------------------------------------------------------------------------------}

newtype BlockChain = BlockChain Word64
  deriving (Show)

blockChain :: BlockChain -> Chain TestBlock
blockChain = Chain.fromOldestFirst . chainToBlocks

chainToBlocks :: BlockChain -> [TestBlock]
chainToBlocks (BlockChain c) =
    take (fromIntegral c) $ iterate successorBlock (firstBlock 0)

instance Arbitrary BlockChain where
  arbitrary = BlockChain <$> choose (0, 30)
  shrink (BlockChain c) = BlockChain <$> shrink c

-- | See 'firstBlockWithPayload'.
firstBlock :: Word64 -> TestBlock
firstBlock forkNo = firstBlockWithPayload forkNo ()

-- | See 'successorBlockWithPayload'.
successorBlock :: TestBlock -> TestBlock
successorBlock TestBlockWith{tbHash, tbSlot} = successorBlockWithPayload tbHash tbSlot ()

-- Modify the (last) fork number of the given block:
-- @g@ -> @[.., f]@ -> @[.., g f]@
-- The 'SlotNo' is left unchanged.
modifyFork :: (Word64 -> Word64) -> TestBlock -> TestBlock
modifyFork g tb@TestBlockWith{ tbHash = UnsafeTestHash (f NE.:| h) } = tb
    { tbHash = let !gf = g f in UnsafeTestHash (gf NE.:| h)
    }

-- Increase the fork number of the given block:
-- @[.., f]@ -> @[.., f+1]@
-- The 'SlotNo' is left unchanged.
--
-- In Zipper parlance, this corresponds to going right in a tree.
forkBlock :: TestBlock -> TestBlock
forkBlock = modifyFork succ

{-------------------------------------------------------------------------------
  Tree of blocks (without payload)
-------------------------------------------------------------------------------}

newtype BlockTree = BlockTree (Tree ())

blockTree :: BlockTree -> Tree TestBlock
blockTree (BlockTree t) = go (firstBlock 0) t
  where
    go :: TestBlock -> Tree () -> Tree TestBlock
    go b (Node () ts) = Node b (zipWith go bs ts)
      where
        -- The first child of a node is the sucessor of b ("go down"), each
        -- subsequent child is a "fork" ("go right")
        bs = iterate forkBlock (successorBlock b)

treeToBlocks :: BlockTree -> [TestBlock]
treeToBlocks = Tree.flatten . blockTree

treeToChains :: BlockTree -> [Chain TestBlock]
treeToChains = map Chain.fromOldestFirst . allPaths . blockTree

treePreferredChain :: BlockTree -> Chain TestBlock
treePreferredChain =
      fromMaybe Genesis
    . selectUnvalidatedChain
        (Proxy @(BlockProtocol TestBlock))
        blockNo
        Genesis
    . treeToChains

instance Show BlockTree where
  show (BlockTree t) = Tree.drawTree (fmap show t)

instance Arbitrary BlockTree where
  arbitrary = sized $ \n ->
      BlockTree <$> mkTree 0.2 (replicate (max 1 n) ())
  shrink (BlockTree t) =
      BlockTree <$> shrinkTree t

{-------------------------------------------------------------------------------
  Generic auxiliary
-------------------------------------------------------------------------------}

-- | Construct random binary tree from given set of elements
mkTree :: forall a.
          Double -- ^ Likelyhood of branching at any point
       -> [a] -> Gen (Tree a)
mkTree threshold = go
  where
    go :: [a] -> Gen (Tree a)
    go []     = error "go: no elements"
    go [a]    = return $ Node a []
    go (a:as) = do n <- choose (0, 1)
                   if n >= threshold || null right
                     then (\t   -> Node a [t])    <$> go as
                     else (\l r -> Node a [l, r]) <$> go left <*> go right
      where
        (left, right) = split as

-- | Shrink tree (without shrinking any elements)
shrinkTree :: Tree a -> [Tree a]
shrinkTree (Node a ts) = map (Node a) (shrinkList shrinkTree ts)
                         -- Also try shrinking all subtrees at once
                      ++ map (Node a) (transpose (map shrinkTree ts))


-- | Split list into two
--
-- > split [1..6] == ([1,3,5],[2,4,6])
-- > take 5 (fst (split [1..])) == [1,3,5,7,9]
-- > take 5 (snd (split [1..])) == [2,4,6,8,10]
split :: [a] -> ([a], [a])
split []     = ([], [])
split (a:as) = let (xs, ys) = split as in (a:ys, xs)

-- | All paths through a tree
allPaths :: Tree a -> [[a]]
allPaths t = [] : nonEmptyPaths t

nonEmptyPaths :: Tree a -> [[a]]
nonEmptyPaths (Node a ts) = [a] : map (a:) (concatMap nonEmptyPaths ts)

{-------------------------------------------------------------------------------
  Test auxiliary
-------------------------------------------------------------------------------}

newtype Permutation = Permutation Int
  deriving (Show)

instance Arbitrary Permutation where
  arbitrary = Permutation . cast <$> arbitrary
    where
      -- Use the generator for 'Int64' (rather than 'Int') as it is not biased
      -- towards small values
      cast :: Int64 -> Int
      cast = fromIntegral

  -- Doesn't make sense to shrink PRNG seed
  shrink _ = []

permute :: Permutation -> [a] -> [a]
permute (Permutation n) = go (R.mkStdGen n)
  where
    go :: R.StdGen -> [a] -> [a]
    go _ [] = []
    go g as = let (i, g')           = R.randomR (0, length as - 1) g
                  (before, a:after) = splitAt i as
              in a : go g' (before ++ after)

{-------------------------------------------------------------------------------
  Additional serialisation instances
-------------------------------------------------------------------------------}

instance Serialise (AnnTip (TestBlockWith ptype)) where
  encode = defaultEncodeAnnTip encode
  decode = defaultDecodeAnnTip decode

instance PayloadSemantics ptype => Serialise (ExtLedgerState (TestBlockWith ptype) EmptyMK) where
  encode = encodeExtLedgerState encode encode encode
  decode = decodeExtLedgerState decode decode decode

instance Serialise (RealPoint (TestBlockWith ptype)) where
  encode = encodeRealPoint encode
  decode = decodeRealPoint decode

-- 'ConvertRawHash' expects a constant-size hash. As a compromise, we allow to
-- encode hashes with a block length of up to 100.
instance ConvertRawHash (TestBlockWith ptype) where
  -- 8 + 100 * 8: size of the list, and its elements, one Word64 each
  hashSize _ = 808
  toRawHash _ (TestHash h)
      | len > 100 = error "hash too long"
      | otherwise      = BL.toStrict . Put.runPut $ do
          Put.putWord64le (fromIntegral len)
          for_ h Put.putWord64le
          replicateM_ (100 - len) $ Put.putWord64le 0
    where
      len = length h
  fromRawHash _ bs = flip Get.runGet (BL.fromStrict bs) $ do
      len <- fromIntegral <$> Get.getWord64le
      (NE.nonEmpty -> Just h, rs) <-
        splitAt len <$> replicateM 100 Get.getWord64le
      guard $ all (0 ==) rs
      pure $ TestHash h

instance Serialise ptype => EncodeDisk (TestBlockWith ptype) (TestBlockWith ptype)
instance Serialise ptype => DecodeDisk (TestBlockWith ptype) (BL.ByteString -> TestBlockWith ptype) where
  decodeDisk _ = const <$> decode

instance Serialise ptype => EncodeDisk (TestBlockWith ptype) (Header (TestBlockWith ptype))
instance Serialise ptype => DecodeDisk (TestBlockWith ptype) (BL.ByteString -> Header (TestBlockWith ptype)) where
  decodeDisk _ = const <$> decode

instance EncodeDisk (TestBlockWith ptype) (AnnTip (TestBlockWith ptype))
instance DecodeDisk (TestBlockWith ptype) (AnnTip (TestBlockWith ptype))

instance ReconstructNestedCtxt Header (TestBlockWith ptype)

instance PayloadSemantics ptype => EncodeDisk (TestBlockWith ptype) (LedgerState (TestBlockWith ptype) EmptyMK)
instance PayloadSemantics ptype => DecodeDisk (TestBlockWith ptype) (LedgerState (TestBlockWith ptype) EmptyMK)

instance Serialise ptype => EncodeDiskDep (NestedCtxt Header) (TestBlockWith ptype)
instance Serialise ptype => DecodeDiskDep (NestedCtxt Header) (TestBlockWith ptype)

-- ChainDepState (BlockProtocol (TestBlockWith ptype)) ~ ()
instance EncodeDisk (TestBlockWith ptype) ()
instance DecodeDisk (TestBlockWith ptype) ()

-- Header (TestBlockWith ptype) is a newtype around TestBlockWith ptype
instance Serialise ptype => HasBinaryBlockInfo (TestBlockWith ptype) where
  getBinaryBlockInfo blk = BinaryBlockInfo {
        headerOffset = 0
      , headerSize   = fromIntegral . BL.length . serialise $ blk
      }

instance (Serialise ptype, PayloadSemantics ptype) => SerialiseDiskConstraints (TestBlockWith ptype)
