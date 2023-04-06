{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- | On-disk ledger DB tests.
--
-- This is a model based test. The commands here are
--
-- * Get the current ledger state
-- * Push a block, or switch to a fork
-- * Write a snapshot to disk
-- * Restore the ledger DB from the snapshots on disk
-- * Model disk corruption
--
-- The model here is satisfyingly simple: just a map from blocks to their
-- corresponding ledger state.
--
module Test.Ouroboros.Storage.LedgerDB.OnDisk (
    showLabelledExamples
  , tests
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Slotting.Slot as WithOrigin
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import           Control.Monad.Except (Except, runExcept)
import           Control.Monad.State (StateT (..))
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import           Control.Tracer (nullTracer)
import qualified Control.Tracer as Trace
import           Data.Bifunctor
import           Data.Foldable (toList)
import           Data.Functor.Classes
import qualified Data.List as L
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Diff.Strict.Internal as DS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.TreeDiff.Class (genericToExpr)
import           Data.TreeDiff.Expr (Expr (App))
import           Data.Word
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB hiding (maxRollback, push,
                     switch, tip)
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Impl as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB as LMDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.DiffSeq as DS
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
                     (KeySetsReader, PointNotFound (..), getLedgerTablesFor,
                     readKeySets)
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.Block (Point (Point))
import           Ouroboros.Network.Point (Block (Block))
import           Prelude hiding (elem)
import qualified System.Directory as Dir
import           System.FS.API
import           System.FS.API.Types
import qualified System.FS.IO as FSIO
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.STM
import           System.Info (os)
import qualified System.IO.Temp as Temp
import           System.Random (getStdRandom, randomR)
import           Test.Ouroboros.Storage.LedgerDB.InMemory ()
import           Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary ()
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import qualified Test.QuickCheck.Random as QC
import           Test.StateMachine hiding (showLabelledExamples)
import qualified Test.StateMachine.Labelling as C
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck
import           Test.Util.Range
import           Test.Util.TestBlock hiding (TestBlock, TestBlockCodecConfig,
                     TestBlockStorageConfig)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "OnDisk"
  ([ testProperty "LedgerSimple-InMem" $
     prop_sequential 100000 (inMemDbEnv) uniform
   ] <>
   [ testProperty "LedgerSimple-LMDB" $
     prop_sequential 2000 (lmdbDbEnv testLMDBLimits) lmdbCustom
   | os /= "mingw32"  -- FIXME: Should re-enable at some point, see #4022
   ]
  )
  where
    uniform = CmdDistribution
        { freqCurrent   = 1
        , freqPush      = 1
        , freqSwitch    = 1
        , freqSnap      = 1
        , freqFlush     = 1
        , freqRestore   = 1
        , freqCorrupt   = 1
        , freqDrop      = 1
        , freqGetTables = 1
        }

    -- NOTE: For the LMDB backend we chose this distribution, which reduces the
    -- probability of generating commands that require re-creating an LMDB
    -- database. We have investigated reusing the same LMDB database across all
    -- tests, but this leads to more complex code and dependencies between
    -- tests. In particular, if we would have chosen this path we would have had
    -- to introduce modifications in the system under test only to accommodate
    -- the database sharing functionality needed for tests.
    lmdbCustom = CmdDistribution
        { freqCurrent   = 10
        , freqPush      = 10
        , freqSwitch    = 1
        , freqSnap      = 1
        , freqFlush     = 5
        , freqRestore   = 1
        , freqCorrupt   = 1
        , freqDrop      = 1
        , freqGetTables = 1
        }

testLMDBLimits :: LMDB.LMDBLimits
testLMDBLimits = LMDB.LMDBLimits
  { -- 100 MiB should be more than sufficient for the tests we're running here.
    -- If the database were to grow beyond 100 Mebibytes, resulting in a test
    -- error, then something in the LMDB backing store or tests has changed and
    -- we should reconsider this value.
    LMDB.lmdbMapSize = 100 * 1024 * 1024
    -- 3 internal databases: 1 for the settings, 1 for the state, and 1 for the
    -- ledger tables.
  , LMDB.lmdbMaxDatabases = 3
  , LMDB.lmdbMaxReaders = 16
  }

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}

type TestBlock = TestBlockWith Tx

-- | Mock of a UTxO transaction where exactly one (transaction) input is
-- consumed and exactly one output is produced.
--
data Tx = Tx {
    -- | Input that the transaction consumes.
    consumed :: Token
    -- | Ouptupt that the transaction produces.
  , produced :: (Token, TValue)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

-- | A token is an identifier for the values produced and consumed by the
-- 'TestBlock' transactions.
--
-- This is analogous to @TxId@: it's how we identify what's in the table. It's
-- also analogous to @TxIn@, since we trivially only have one output per 'Tx'.
newtype Token = Token { unToken :: Point TestBlock }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr, QC.Arbitrary)

instance QC.Arbitrary (Point TestBlock) where
  arbitrary = do
    slot <- SlotNo <$> QC.arbitrary
    hash <- TestHash . fromJust . nonEmpty . QC.getNonEmpty <$> QC.arbitrary
    pure $ Point $ WithOrigin.At $ Block slot hash

-- | Unit of value associated with the output produced by a transaction.
--
-- This is analogous to @TxOut@: it's what the table maps 'Token's to.
newtype TValue = TValue (WithOrigin SlotNo)
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr)

{-------------------------------------------------------------------------------
  A ledger semantics for TestBlock
-------------------------------------------------------------------------------}

data TxErr
  = TokenWasAlreadyCreated Token
  | TokenDoesNotExist      Token
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, Serialise, ToExpr)

instance PayloadSemantics Tx where
  data PayloadDependentState Tx mk =
    UTxTok { utxtoktables :: LedgerTables (LedgerState TestBlock) mk
             -- | All the tokens that ever existed. We use this to
             -- make sure a token is not created more than once. See
             -- the definition of 'applyPayload' in the
             -- 'PayloadSemantics' of 'Tx'.
           , utxhist      :: Set Token
           }
    deriving stock    (Generic)

  type PayloadDependentError Tx = TxErr

  -- We need to exercise the HD backend. This requires that we store key-values
  -- ledger tables and the block application semantics satisfy:
  --
  -- * a key is deleted at most once
  -- * a key is inserted at most once
  --
  applyPayload st Tx{consumed, produced} =
      fmap track $ delete consumed st >>= uncurry insert produced
    where
      insert ::
           Token
        -> TValue
        -> PayloadDependentState Tx ValuesMK
        -> Either TxErr (PayloadDependentState Tx ValuesMK)
      insert tok val st'@UTxTok{utxtoktables, utxhist} =
          if tok `Set.member` utxhist
          then Left  $ TokenWasAlreadyCreated tok
          else Right $ st' { utxtoktables = Map.insert tok val `onValues` utxtoktables
                           , utxhist      = Set.insert tok utxhist
                           }
      delete ::
           Token
        -> PayloadDependentState Tx ValuesMK
        -> Either TxErr (PayloadDependentState Tx ValuesMK)
      delete tok st'@UTxTok{utxtoktables} =
          if Map.member tok `queryKeys` utxtoktables
          then Right $ st' { utxtoktables = Map.delete tok `onValues` utxtoktables
                           }
          else Left  $ TokenDoesNotExist tok

      track :: PayloadDependentState Tx ValuesMK -> PayloadDependentState Tx TrackingMK
      track stAfter =
          stAfter { utxtoktables =
                      TokenToTValue $ rawCalculateDifference utxtokBefore utxtokAfter
                  }
        where
          utxtokBefore = testUtxtokTable $ utxtoktables st
          utxtokAfter  = testUtxtokTable $ utxtoktables stAfter

  getPayloadKeySets Tx{consumed} =
    TokenToTValue $ KeysMK $ Set.singleton consumed

deriving instance Eq (LedgerTables (LedgerState TestBlock) mk) => Eq (PayloadDependentState Tx mk)
deriving instance NoThunks (LedgerTables (LedgerState TestBlock) mk) => NoThunks (PayloadDependentState Tx mk)
deriving instance Show (LedgerTables (LedgerState TestBlock) mk) => Show (PayloadDependentState Tx mk)
deriving instance Serialise (LedgerTables (LedgerState TestBlock) mk) => Serialise (PayloadDependentState Tx mk)

onValues ::
     (Map Token TValue -> Map Token TValue)
  -> LedgerTables (LedgerState TestBlock) ValuesMK
  -> LedgerTables (LedgerState TestBlock) ValuesMK
onValues f TokenToTValue {testUtxtokTable} = TokenToTValue $ updateMap testUtxtokTable
  where
    updateMap :: ValuesMK Token TValue -> ValuesMK Token TValue
    updateMap (ValuesMK utxovals) =
      ValuesMK $ f utxovals

queryKeys ::
     (Map Token TValue -> a)
  -> LedgerTables (LedgerState TestBlock) ValuesMK
  -> a
queryKeys f (TokenToTValue (ValuesMK utxovals)) = f utxovals

{-------------------------------------------------------------------------------
  Instances required for HD storage of ledger state tables
-------------------------------------------------------------------------------}

instance HasLedgerTables (LedgerState TestBlock) where
  newtype LedgerTables (LedgerState TestBlock) mk =
    TokenToTValue { testUtxtokTable :: mk Token TValue }
    deriving stock (Generic)

  projectLedgerTables st       = utxtoktables $ payloadDependentState st
  withLedgerTables    st table = st { payloadDependentState =
                                        (payloadDependentState st) {utxtoktables = table}
                                    }

  pureLedgerTables = TokenToTValue

  mapLedgerTables      f                                     (TokenToTValue x) = TokenToTValue    (f x)
  traverseLedgerTables f                                     (TokenToTValue x) = TokenToTValue <$> f x
  zipLedgerTables      f                   (TokenToTValue x) (TokenToTValue y) = TokenToTValue    (f x y)
  zipLedgerTables3     f (TokenToTValue x) (TokenToTValue y) (TokenToTValue z) = TokenToTValue    (f x y z)
  zipLedgerTablesA     f                   (TokenToTValue x) (TokenToTValue y) = TokenToTValue <$> f x y
  zipLedgerTables3A    f (TokenToTValue x) (TokenToTValue y) (TokenToTValue z) = TokenToTValue <$> f x y z
  foldLedgerTables     f                                     (TokenToTValue x) =                   f x
  foldLedgerTables2    f                   (TokenToTValue x) (TokenToTValue y) =                   f x y
  namesLedgerTables                                                            = TokenToTValue $ NameMK "testblocktables"

deriving instance Eq (mk Token TValue) => Eq (LedgerTables (LedgerState TestBlock) mk)
deriving anyclass instance NoThunks (mk Token TValue) => NoThunks (LedgerTables (LedgerState TestBlock) mk)
deriving instance Show (mk Token TValue) => Show (LedgerTables (LedgerState TestBlock) mk)

instance CanSerializeLedgerTables (LedgerState TestBlock) where
  codecLedgerTables = TokenToTValue $ CodecMK toCBOR toCBOR fromCBOR fromCBOR

instance Serialise (LedgerTables (LedgerState TestBlock) EmptyMK) where
  encode (TokenToTValue (_ :: EmptyMK Token TValue))
         = CBOR.encodeNull
  decode = TokenToTValue EmptyMK <$ CBOR.decodeNull

instance ToCBOR Token where
  toCBOR (Token pt) = S.encode pt

instance FromCBOR Token where
  fromCBOR = fmap Token S.decode

instance ToCBOR TValue where
  toCBOR (TValue v) = S.encode v

instance FromCBOR TValue where
  fromCBOR = fmap TValue S.decode

instance HasTickedLedgerTables (LedgerState TestBlock) where
  projectLedgerTablesTicked (TickedTestLedger st)        = projectLedgerTables st
  withLedgerTablesTicked    (TickedTestLedger st) tables =
    TickedTestLedger $ withLedgerTables st tables

instance CanStowLedgerTables (LedgerState TestBlock) where
  stowLedgerTables     = stowErr "stowLedgerTables"
  unstowLedgerTables   = stowErr "unstowLedgerTables"

stowErr :: String -> a
stowErr fname = error $ "Function " <> fname <> " should not be used in these tests."

deriving anyclass instance ToExpr v => ToExpr (DS.DiffEntry v)
deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.Diff k v)
deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.RootMeasure k v)
deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.InternalMeasure k v)
deriving anyclass instance (ToExpr k, ToExpr v) => ToExpr (DS.Element k v)
deriving anyclass instance ToExpr DS.Length
deriving anyclass instance ToExpr DS.SlotNoUB
deriving anyclass instance ToExpr DS.SlotNoLB
deriving anyclass instance ToExpr (mk Token TValue) => ToExpr (LedgerTables (LedgerState TestBlock) mk)
deriving instance ToExpr (LedgerTables (LedgerState TestBlock) mk) => ToExpr (PayloadDependentState Tx mk)

deriving newtype instance ToExpr (ValuesMK Token TValue)

instance ToExpr v => ToExpr (DS.DiffHistory v) where
  toExpr h = App "DiffHistory" [genericToExpr . toList . DS.getDiffHistory $ h]

instance ToExpr v => ToExpr (DS.NEDiffHistory v) where
  toExpr h = App "NEDiffHistory" [genericToExpr . toList . DS.getNEDiffHistory $ h]

instance ToExpr (ExtLedgerState TestBlock ValuesMK) where
  toExpr = genericToExpr

instance ToExpr (LedgerState (TestBlockWith Tx) ValuesMK) where
  toExpr = genericToExpr

{-------------------------------------------------------------------------------
  TestBlock generation

  When we added support for storing parts of the ledger state on disk we needed
  to exercise this new functionality. Therefore, we modified this test so that
  the ledger state associated to the test block contained tables (key-value
  maps) to be stored on disk. This ledger state needs to follow an evolution
  pattern similar to the UTxO one (see the 'PayloadSemantics' instance for more
  details). As a result, block application might fail on a given payload.

  The tests in this module assume that no invalid blocks are generated. Thus we
  have to satisfy this assumption in the block generators. To keep the
  generators simple, eg independent on the ledger state, we follow this strategy
  to block generation:

  - The block payload consist of a single transaction:
      - input: Point
      - output: (Point, SlotNo)
  - The ledger state is a map from Point to SlotNo.
  - We start always in an initial state in which 'GenesisPoint' maps to slot 0.
  - When we generate a block for point p, the payload of the block will be:
      - input: point p - 1
      - ouptput: (point p, slot of point p)


  A consequence of adopting the strategy above is that the initial state is
  coupled to the generator's semantics.
 -------------------------------------------------------------------------------}

initialTestLedgerState :: PayloadDependentState Tx ValuesMK
initialTestLedgerState = UTxTok {
    utxtoktables =   TokenToTValue
                   $ ValuesMK
                   $ Map.singleton initialToken (pointTValue initialToken)
  , utxhist      = Set.singleton initialToken

  }
  where
    initialToken = Token GenesisPoint

-- | Get the token value associated to a given token. This is coupled to the
-- generators semantics.
pointTValue :: Token -> TValue
pointTValue = TValue . pointSlot . unToken

genBlocks ::
     Word64
  -> Point TestBlock
  -> [TestBlock]
genBlocks n pt0 = take (fromIntegral n) (go pt0)
  where
    go pt = let b = genBlock pt in b : go (blockPoint b)

genBlock ::
     Point TestBlock -> TestBlock
genBlock pt =
  mkBlockFrom pt Tx { consumed = Token pt
                    , produced = ( Token pt', TValue (pointSlot pt'))
                    }
  where
    mkBlockFrom :: Point (TestBlockWith ptype) -> ptype -> TestBlockWith ptype
    mkBlockFrom GenesisPoint           = firstBlockWithPayload 0
    mkBlockFrom (BlockPoint slot hash) = successorBlockWithPayload hash slot

    pt' :: Point (TestBlockWith Tx)
    pt' = castPoint (blockPoint dummyBlk)
      where
        -- This could be the new block itself; we merely wanted to avoid the loop.
        dummyBlk :: TestBlockWith ()
        dummyBlk = mkBlockFrom (castPoint pt) ()

genBlockFromLedgerState :: ExtLedgerState TestBlock mk -> Gen TestBlock
genBlockFromLedgerState = pure . genBlock . lastAppliedPoint . ledgerState

extLedgerDbConfig :: SecurityParam -> LedgerDbCfg (ExtLedgerState TestBlock)
extLedgerDbConfig secParam = LedgerDbCfg {
      ledgerDbCfgSecParam = secParam
    , ledgerDbCfg         = ExtLedgerCfg $ singleNodeTestConfigWith TestBlockCodecConfig TestBlockStorageConfig secParam
    }


-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

data Corruption =
    -- | Delete the snapshot entirely
    Delete

    -- | Truncate the file
    --
    -- This is just a simple way to cause a deserialisation error
  | Truncate
  deriving (Show, Eq, Generic, ToExpr)

data Cmd ss =
    -- | Get the current ledger state
    Current

    -- | Push a block
  | Push TestBlock

    -- | Switch to a fork
  | Switch Word64 [TestBlock]

    -- | Take a snapshot (write to disk)
  | Snap

    -- | Flush the DbChangelog in the model.
    --
    -- Note that as the Mock doesn't have the notion of "flushed differences", it is a no-op on the mock.
  | Flush

    -- | Restore the DB from disk, then return it along with the init log
  | Restore

    -- | Corrupt a previously taken snapshot
  | Corrupt Corruption ss

    -- | Corruption of the chain
    --
    -- Chain corruption, no matter what form, always results in truncation. We
    -- model this as the number of blocks that got truncated from the end of the
    -- chain.
    --
    -- NOTE: Since this is modelling /disk/ corruption, and it is the
    -- responsibility of the 'ChainDB' to /notice/ disk corruption (by
    -- catching the appropriate exceptions), we assume that the ledger state
    -- will immediately be re-initialized after a 'Truncate' (which is precisely
    -- what the 'ChainDB' would do, after first doing recovery on the
    -- underlying 'LedgerDB'). This is important because otherwise the model
    -- would diverge from the real thing.
    --
    -- Since 'Drop' therefore implies a 'Restore', we return the new ledger.
  | Drop Word64

  | GetTablesAtFor (Point TestBlock) (LedgerTables (ExtLedgerState TestBlock) KeysMK)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Success ss =
    Unit ()
  | MaybeErr (Either (ExtValidationError TestBlock) ())
  | Ledger (ExtLedgerState TestBlock EmptyMK)
  | Snapped (Maybe (ss, RealPoint TestBlock))
  | Restored (MockInitLog ss, ExtLedgerState TestBlock EmptyMK)
  | Tables (Either (PointNotFound TestBlock) (LedgerTables (ExtLedgerState TestBlock) ValuesMK))
  | Flushed
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Currently we don't have any error responses
newtype Resp ss = Resp (Success ss)
  deriving (Show, Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Pure model
-------------------------------------------------------------------------------}

-- | The mock ledger records the blocks and ledger values (new to old)
--
-- TODO: we need the values in the ledger state of the mock since we do not
-- store the values anywhere else in the mock.
type MockLedger = [(TestBlock, ExtLedgerState TestBlock ValuesMK)]

-- | We use the slot number of the ledger state as the snapshot number
--
-- We only keep track of this to be able to give more meaningful statistics
-- about generated tests. The mock implementation doesn't actually " take "
-- any snapshots (instead it stores the ledger state at each point).
newtype MockSnap = MockSnap Word64
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToExpr)

-- | State of all snapshots on disk
--
-- In addition to the state of the snapshot we also record the tip of the chain
-- at the time we took the snapshot; this is important for 'mockMaxRollback'.
type MockSnaps = Map MockSnap (RealPoint TestBlock, SnapState)

-- | Mock implementation
--
-- The mock implementation simply records the ledger at every point.
-- We store the chain most recent first.
data Mock = Mock {
      -- | Current ledger
      mockLedger             :: MockLedger

      -- | Current state the snapshots
    , mockSnaps              :: MockSnaps

      -- | Point at which an immutable tip in the sequence of ledger states was
      -- flushed to disk
      --
      -- The latest flushed point does not necessarily correspond to the point
      -- at which the immutable tip is. As blocks are pushed onto the LedgerDB,
      -- the immutable tip will move further ahead from the latest flushed
      -- point. See the example below:
      --
      -- p0--------------------p_f-----------------p_i----------p_t
      -- ^                      ^                   ^            ^
      -- Genesis point  latest flushed point     Immutable tip   Tip
      --
      -- A 'Flush' command will make @p_f@ and @p_i@ equal.
      --
      -- The value of this field is affected by a 'Flush' command, but also by
      -- 'Restore' and 'Drop' commands. The last two affect the current chain,
      -- and therefore they also affect the point of the most recent known
      -- flush. See the implementation of 'runMock' for additional details.
    , mockLatestFlushedPoint :: Point TestBlock

      -- | The oldest (tail) block in the real DB at the most recent restore
      --
      -- This puts a limit on how far we can roll back.
      -- See also 'applyMockLog', 'mockMaxRollback'.
    , mockRestore            :: Point TestBlock

      -- | Security parameter
      --
      -- We need the security parameter only to compute which snapshots the real
      -- implementation would take, so that we can accurately predict how far
      -- the real implementation can roll back.
    , mockSecParam           :: SecurityParam
    }
  deriving (Show, Generic, ToExpr)

data SnapState = SnapOk | SnapCorrupted
  deriving (Show, Eq, Generic, ToExpr)

mockInit :: SecurityParam -> Mock
mockInit secParam = Mock
  { mockLedger             = []
  , mockSnaps              = Map.empty
  , mockLatestFlushedPoint = GenesisPoint
  , mockRestore            = GenesisPoint
  , mockSecParam           = secParam
  }

mockChainLength :: Mock -> Word64
mockChainLength Mock{..} = fromIntegral (length mockLedger)

mockRollback :: Word64 -> Mock -> Mock
mockRollback n mock@Mock{..} = mock {
      mockLedger = drop (fromIntegral n) mockLedger
    }

mockUpdateLedger :: StateT MockLedger (Except (ExtValidationError TestBlock)) a
                 -> Mock -> (Either (ExtValidationError TestBlock) a, Mock)
mockUpdateLedger f mock =
    case runExcept (runStateT f (mockLedger mock)) of
      Left  err          -> (Left err, mock)
      Right (a, ledger') -> (Right a, mock { mockLedger = ledger' })

mockRecentSnap :: Mock -> Maybe SnapState
mockRecentSnap Mock{..} = snd . snd <$> Map.lookupMax mockSnaps

{-------------------------------------------------------------------------------
  Modelling restoration

  Although the mock implementation itself is not affected by disk failures
  (in fact, the concept makes no sense, since we don't store anything on disk),
  we /do/ need to be able to accurately predict how the real DB will be
  initialized (from which snapshot); this is important, because this dictates
  how far the real DB can roll back.
-------------------------------------------------------------------------------}

data MockInitLog ss =
    MockFromGenesis
  | MockFromSnapshot    ss (RealPoint TestBlock)
  | MockReadFailure     ss                       (MockInitLog ss)
  | MockTooRecent       ss (RealPoint TestBlock) (MockInitLog ss)
  | MockGenesisSnapshot ss                       (MockInitLog ss)
  deriving (Show, Eq, Functor, Foldable, Traversable)

fromInitLog :: InitLog TestBlock -> MockInitLog DiskSnapshot
fromInitLog  InitFromGenesis          = MockFromGenesis
fromInitLog (InitFromSnapshot ss tip) = MockFromSnapshot ss tip
fromInitLog (InitFailure ss err log') =
    case err of
      InitFailureRead _err     -> MockReadFailure     ss     (fromInitLog log')
      InitFailureTooRecent tip -> MockTooRecent       ss tip (fromInitLog log')
      InitFailureGenesis       -> MockGenesisSnapshot ss     (fromInitLog log')

mockInitLog :: Mock -> MockInitLog MockSnap
mockInitLog Mock{..} = go (Map.toDescList mockSnaps)
  where
    go :: [(MockSnap, (RealPoint TestBlock, SnapState))] -> MockInitLog MockSnap
    go []                          = MockFromGenesis
    go ((snap, (pt, state)):snaps) =
        case state of
          SnapCorrupted ->
            -- If it's truncated, it will skip it
            MockReadFailure snap $ go snaps
          SnapOk ->
            if onChain pt
              then MockFromSnapshot snap pt
              else MockTooRecent    snap pt $ go snaps

    onChain :: RealPoint TestBlock -> Bool
    onChain pt = any (\(b, _) -> blockRealPoint b == pt) mockLedger

applyMockLog :: MockInitLog MockSnap -> Mock -> Mock
applyMockLog = go
  where
    -- NOTE: we do not alter the mockLedger when applying the MockLog. When the
    -- SUT restores, it streams all the blocks from the dbState to initLedgerDB.
    -- No matter which snapshot we use when restoring, once we apply all the
    -- blocks after the snapshot, we should end up with the same LedgerDB we had
    -- after restoration. That is why the mock does not care about updating
    -- mockLedger.
    go :: MockInitLog MockSnap -> Mock -> Mock
    go  MockFromGenesis                mock = mock { mockRestore = GenesisPoint         }
    go (MockFromSnapshot    _  tip)    mock = mock { mockRestore = realPointToPoint tip }
    go (MockReadFailure     ss   log') mock = go log' $ deleteSnap ss mock
    go (MockTooRecent       ss _ log') mock = go log' $ deleteSnap ss mock
    go (MockGenesisSnapshot ss   log') mock = go log' $ deleteSnap ss mock

    deleteSnap :: MockSnap -> Mock -> Mock
    deleteSnap ss mock = mock {
        mockSnaps = Map.alter delete ss (mockSnaps mock)
      }
      where
        delete ::
            Maybe (RealPoint TestBlock, SnapState)
         -> Maybe (RealPoint TestBlock, SnapState)
        delete Nothing  = error "setIsDeleted: impossible"
        delete (Just _) = Nothing

-- | Compute theoretical maximum rollback
--
-- The actual maximum rollback will be restricted by the ledger DB params.
mockMaxRollback :: Mock -> Word64
mockMaxRollback Mock{..} = go mockLedger
  where
    go :: MockLedger -> Word64
    go ((b, _l):bs)
      | blockPoint b == mockRestore = 0
      | otherwise                   = 1 + go bs
    go []                           = 0

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

runMock :: Cmd MockSnap -> Mock -> (Resp MockSnap, Mock)
runMock cmd initMock =
    first Resp $ go cmd initMock
  where
    cfg :: LedgerDbCfg (ExtLedgerState TestBlock)
    cfg = extLedgerDbConfig (mockSecParam initMock)

    go :: Cmd MockSnap -> Mock -> (Success MockSnap, Mock)
    go Current       mock = (Ledger (forgetLedgerTables $ cur (mockLedger mock)), mock)
    go (Push b)      mock = first MaybeErr $ mockUpdateLedger (push b)      mock
    go (Switch n bs) mock = first MaybeErr $ mockUpdateLedger (switch n bs) mock
    go Restore       mock = (Restored (initLog, forgetLedgerTables $ cur (mockLedger mock'')), mock'')
      -- When we restore to the most recent valid snapshot, the point at which a
      -- immutable tip was flushed to disk becomes the tip of the restoraton
      -- point, since any flushes that occured after this point are now invalid
      -- because we restored a snapshot.
      --
      -- TODO: This depends on the SUT not flushing during restoration. At the
      -- moment we flush every 100 blocks, and the chains we generate are far
      -- below that (about less than 40 elements). At the moment we do not model
      -- the flushing during restoration aspect here.
      where
        initLog = mockInitLog mock
        mock'   = applyMockLog initLog mock
        mock''  = mock'  { mockLatestFlushedPoint = mockRestore mock' }
    go Flush         mock =
      (Flushed, mock { mockLatestFlushedPoint = immutableTipSlot mock })
      -- When we flush, we might encounter the following scenarios:
      --
      -- 1. We do not have an immutable tip because we have less than @k@ blocks
      -- in the chain.
      --
      -- 2. We have less than @k@ blocks from the immutable tip, which will
      -- happen only after a restoration. In that case we leave the latest
      -- flushed point unaltered.
      --
      -- 2. We have more than @k@ blocks, in which case we set the latest
      -- flushed point to the immutable tip.
    go Snap          mock = case mbSnapshot of
        Just pt
          | let mockSnap = MockSnap (unSlotNo (realPointSlot pt))
          , Map.notMember mockSnap (mockSnaps mock)
          -> ( Snapped (Just (mockSnap, pt))
             , mock {
                   mockSnaps =
                     Map.insert mockSnap (pt, SnapOk) (mockSnaps mock)
                 }
             )
        _otherwise
          -- No snapshot to take or one already exists
          -> (Snapped Nothing, mock)
      where
        -- | The snapshot that the real implementation will possibly write to
        -- disk.
        --
        -- 1. We will write the snapshot of the latest flushed ledger state.
        --
        --    In the example below, if the latest flushed state was B this is
        --    what we will write to disk, unless there is already a snapshot of
        --    B.
        --
        --    > A -> B -> C -> D -> E
        --
        -- 2. In case flushed never ocurred, we look at the snapshot from which
        --    we restored ('mockRestore').
        --
        --    a. When that corresponds to the genesis ledger state, we don't
        --       write a snapshot to disk.
        --
        --    b. Otherwise, we write 'mockRestore' to disk. Note that we later
        --       check whether that snapshots still exists on disk, in which
        --       case we wouldn't write it to disk again.
        mbSnapshot :: Maybe (RealPoint TestBlock)
        mbSnapshot = case pointToWithOriginRealPoint $ mockLatestFlushedPoint mock of
            NotOrigin pt -> Just pt                  -- 1
            Origin       -> case pointToWithOriginRealPoint (mockRestore mock) of
                            Origin       -> Nothing  -- 2a
                            NotOrigin pt -> Just pt  -- 2b
    go (Corrupt c ss) mock = (
          Unit ()
        , mock { mockSnaps = Map.alter corrupt ss (mockSnaps mock) }
        )
      where
        corrupt :: Maybe (RealPoint TestBlock, SnapState)
                -> Maybe (RealPoint TestBlock, SnapState)
        corrupt Nothing         = error "corrupt: impossible"
        corrupt (Just (ref, _)) = case c of
          Delete   -> Nothing
          Truncate -> Just (ref, SnapCorrupted)
    go (Drop n) mock =
        go Restore $ mock {
            mockLedger = drop (fromIntegral n) (mockLedger mock)
          }

    go (GetTablesAtFor pt keys) mock = (,mock) $
      if pointSlot pt < pointSlot (immutableTipSlot mock)
      then Tables $ Left $ PointNotFound pt
      else
        case L.find (\(_, st) -> castPoint (getTip st) == pt) (mockLedger mock) of
          Nothing      -> Tables $ Left $ PointNotFound pt
          Just (_, st) -> Tables $ Right $ restrictValues st keys

    push :: TestBlock -> StateT MockLedger (Except (ExtValidationError TestBlock)) ()
    push b = do
        ls <- State.get
        l' <- State.lift $ tickThenApply (ledgerDbCfg cfg) b (cur ls)
        State.put ((b, applyLedgerTablesDiffs (cur ls) l'):ls)

    switch :: Word64
           -> [TestBlock]
           -> StateT MockLedger (Except (ExtValidationError TestBlock)) ()
    switch n bs = do
        State.modify $ drop (fromIntegral n)
        mapM_ push bs

    cur :: MockLedger -> ExtLedgerState TestBlock ValuesMK
    cur []         = testInitExtLedgerWithState initialTestLedgerState
    cur ((_, l):_) = l

{-------------------------------------------------------------------------------
  Standalone instantiation of the ledger DB
-------------------------------------------------------------------------------}

-- | Arguments required by 'StandaloneDB'
data DbEnv m = DbEnv {
      dbHasFS                :: !(SomeHasFS m)
    , dbBackingStoreSelector :: !(BackingStoreSelector m)
    , dbTracer               :: !(Trace.Tracer m HD.BackingStoreTrace)
    , dbCleanup              :: !(m ())
    }

-- | Standalone ledger DB
--
-- Under normal circumstances the ledger DB is maintained by the 'ChainDB',
-- and supported by the 'ChainDB'. In order to test it stand-alone we need to
-- mock these components.
data StandaloneDB m = DB {
      -- | Arguments
      dbEnv         :: DbEnv m
    , dbSecParam    :: !SecurityParam

      -- | Block storage
      --
      -- We can think of this as mocking the volatile DB. Blocks can be
      -- added to this without updating the rest of the state.
    , dbBlocks      :: StrictTVar m (Map (RealPoint TestBlock) TestBlock)

      -- | Current chain and corresponding ledger state
      --
      -- We can think of this as mocking the ChainDB, which must keep
      -- track of a current chain and keep the ledger DB in sync with it.
      --
      -- Invariant: all references @r@ here must be present in 'dbBlocks'.
    , dbState       :: StrictTVar m ([RealPoint TestBlock], LedgerDB' TestBlock)

      -- | Resolve blocks
    , dbResolve     :: ResolveBlock m TestBlock

      -- | LedgerDB config
    , dbLedgerDbCfg :: LedgerDbCfg (ExtLedgerState TestBlock)

    , dbBackingStore :: StrictTVar m (HD.LedgerBackingStore m (ExtLedgerState TestBlock))

      -- | needed for restore TODO does this really belong here?
    , sdbBackingStoreSelector :: BackingStoreSelector m

    , dbRegistry :: ResourceRegistry m
    }

immutableTipSlot :: Mock -> Point TestBlock
immutableTipSlot mock =
  case drop k (mockLedger mock) of
    []           -> mockLatestFlushedPoint mock
    (blk, _st):_ -> max (mockLatestFlushedPoint mock) (blockPoint blk)
  where
    k :: Int
    k = fromIntegral $ maxRollbacks $ mockSecParam mock

initStandaloneDB ::
     forall m. (Trans.MonadIO m, IOLike m)
  => DbEnv m
  -> ResourceRegistry m
  -> SecurityParam
  -> m (StandaloneDB m)
initStandaloneDB dbEnv@DbEnv{..} dbRegistry dbSecParam = do
    dbBlocks <- uncheckedNewTVarM Map.empty
    dbState  <- uncheckedNewTVarM (initChain, initDB)
    let bsi = newBackingStoreInitialiser dbTracer dbBackingStoreSelector
    dbBackingStore <- uncheckedNewTVarM . HD.LedgerBackingStore
                      =<< bsi
                             dbHasFS
                             (HD.InitFromValues
                              Origin
                              initTables -- TODO we could consider adapting the test generator to generate an initial ledger with non-empty tables.
                             )
    let dbResolve :: ResolveBlock m TestBlock
        dbResolve r = atomically $ getBlock r <$> readTVar dbBlocks

        dbLedgerDbCfg :: LedgerDbCfg (ExtLedgerState TestBlock)
        dbLedgerDbCfg = extLedgerDbConfig dbSecParam

    return DB{..}
  where
    initChain :: [RealPoint TestBlock]
    initChain = []

    initDB     :: LedgerDB' TestBlock
    initTables :: LedgerTables (ExtLedgerState TestBlock) ValuesMK
    (initDB, initTables) =
        ( mkWithAnchor
            (forgetLedgerTables initialState)
        , projectLedgerTables initialState
        )
      where
        initialState = testInitExtLedgerWithState initialTestLedgerState

    getBlock ::
         RealPoint TestBlock
      -> Map (RealPoint TestBlock) TestBlock
      -> TestBlock
    getBlock = Map.findWithDefault (error blockNotFound)

    blockNotFound :: String
    blockNotFound = concat [
          "dbConf: "
        , "invariant violation: "
        , "block in dbChain not in dbBlocks, "
        , "or LedgerDB not re-initialized after chain truncation"
        ]
    sdbBackingStoreSelector = dbBackingStoreSelector

dbStreamAPI :: forall m. IOLike m => StandaloneDB m -> StreamAPI m TestBlock TestBlock
dbStreamAPI DB{..} = StreamAPI {..}
  where
    streamAfter ::
         Point TestBlock
      -> (Either (RealPoint TestBlock) (m (NextItem TestBlock)) -> m a)
      -> m a
    streamAfter tip k = do
        pts <- atomically $ reverse . fst <$> readTVar dbState
        case tip' of
          NotOrigin pt
            | pt `L.notElem` pts
            -> k $ Left pt
          _otherwise
            -> do toStream <- uncheckedNewTVarM (blocksToStream tip' pts)
                  k (Right (getNext toStream))
     where
       tip' = pointToWithOriginRealPoint tip

    -- Blocks to stream
    --
    -- Precondition: tip must be on the current chain
    blocksToStream ::
         WithOrigin (RealPoint TestBlock)
      -> [RealPoint TestBlock] -> [RealPoint TestBlock]
    blocksToStream Origin        = id
    blocksToStream (NotOrigin r) = tail . dropWhile (/= r)

    getNext :: StrictTVar m [RealPoint TestBlock] -> m (NextItem TestBlock)
    getNext toStream = do
        mr <- atomically $ do
                rs <- readTVar toStream
                case rs of
                  []    -> return Nothing
                  r:rs' -> writeTVar toStream rs' >> return (Just r)
        case mr of
          Nothing -> return NoMoreItems
          Just r  -> do mb <- atomically $ Map.lookup r <$> readTVar dbBlocks
                        case mb of
                          Just b  -> return $ NextItem b
                          Nothing -> error blockNotFound

    blockNotFound :: String
    blockNotFound = concat [
          "dbStreamAPI: "
        , "invariant violation: "
        , "block in dbChain not present in dbBlocks"
        ]

runDB ::
     forall m. IOLike m
  => StandaloneDB m -> Cmd DiskSnapshot -> m (Resp DiskSnapshot)
runDB standalone@DB{..} cmd =
    case dbEnv of
      DbEnv{dbHasFS} -> Resp <$> go dbHasFS cmd
  where
    streamAPI = dbStreamAPI standalone

    annLedgerErr' ::
         AnnLedgerError (ExtLedgerState TestBlock) TestBlock
      -> ExtValidationError TestBlock
    annLedgerErr' = annLedgerErr

    reader :: KeySetsReader m (ExtLedgerState TestBlock)
    reader rewoundTableKeySets = do
      backingStore <- readTVarIO dbBackingStore
      readKeySets backingStore rewoundTableKeySets

    go :: SomeHasFS m -> Cmd DiskSnapshot -> m (Success DiskSnapshot)
    go _ Current =
        atomically $ (Ledger . current . snd) <$> readTVar dbState
    go _ (Push b) = do
        atomically $ modifyTVar dbBlocks $
          uncurry Map.insert (refValPair b)
        upd (push b) $ \db ->
          fmap (first annLedgerErr') $
            defaultThrowLedgerErrors $
              LedgerDB.push
                dbLedgerDbCfg
                (ApplyVal b)
                (Trans.lift . reader)
                db
    go _ (Switch n bs) = do
        atomically $ modifyTVar dbBlocks $
          repeatedly (uncurry Map.insert) (map refValPair bs)
        upd (switch n bs) $ \db ->
          fmap (bimap annLedgerErr' ignoreExceedRollback) $
            defaultResolveWithErrors dbResolve $
              LedgerDB.switch
                dbLedgerDbCfg
                n
                (const $ pure ())
                (map ApplyVal bs)
                (Trans.lift . Trans.lift . reader)
                db

    go _ Flush = do
        (toFlush, bs) <- atomically $ do
          (_, db) <- readTVar dbState
          bs <- readTVar dbBackingStore
          let (toFlush, db') = flush (DbChangelog.FlushAllImmutable dbSecParam) db
          modifyTVar dbState (\(rs, _) -> (rs, db'))
          pure (toFlush, bs)
        DbChangelog.flushIntoBackingStore bs toFlush
        pure Flushed
    go hasFS Snap = do
        (bs, anc) <- atomically $ do
          bs <- readTVar dbBackingStore
          (_, anc) <- readTVar dbState
          pure (bs, lastFlushedState anc)
        Snapped <$>
          takeSnapshot
            nullTracer
            hasFS
            bs
            S.encode
            anc
    go hasFS Restore = do
        HD.LedgerBackingStore old_db <- atomically . readTVar $ dbBackingStore
        HD.bsClose old_db
        (initLog, db, _replayed, backingStore) <-
          initialize
            nullTracer
            -- Todo(jdral): Consider using @traceMaybe@. This would require
            -- the function to be added to the IOHK-fork of @contra-variant@.
            (Trace.Tracer $ \case
               BackingStoreEvent e -> Trace.runTracer (dbTracer dbEnv) e
               _           -> pure ())
            hasFS
            dbRegistry
            S.decode
            S.decode
            dbLedgerDbCfg
            (defaultDiskPolicy (SecurityParam 100) DefaultSnapshotInterval)
            (return (testInitExtLedgerWithState initialTestLedgerState))
            streamAPI
            sdbBackingStoreSelector

        atomically $ do
          modifyTVar dbState (\(rs, _) -> (rs, db))
          writeTVar dbBackingStore backingStore
        return $ Restored (fromInitLog initLog, current db)
    go hasFS (Corrupt c ss) =
        catch
          (case c of
             Delete   -> Unit <$> deleteSnapshot   hasFS ss
             Truncate -> Unit <$> truncateSnapshot hasFS ss)
          (\(_ :: FsError) -> return $ Unit()) -- ignore any errors during corruption
    go hasFS (Drop n) = do
        -- During recovery the ChainDB would ask the ChainDB to recover
        -- and pick a new current chain; only once that is done would it
        -- compute a new ledger state. During this process the ChainDB
        -- would effectively be closed.
        atomically $ do
            (rs, _db) <- readTVar dbState
            writeTVar dbState (drop (fromIntegral n) rs, error "ledger DB not initialized")
        go hasFS Restore
    go hasFS g@(GetTablesAtFor pt keys) = do
        (bstore, lgrDb) <- atomically $ do
          (_, db :: LedgerDB' TestBlock) <- readTVar dbState
          bstore <- readTVar dbBackingStore
          pure (bstore, db)
        case rollback pt lgrDb of
          Nothing -> pure $ Tables $ Left $ PointNotFound pt
          Just l  -> do
            eValues <- getLedgerTablesFor l keys (readKeySets bstore)
            case eValues of
              Right v -> pure $ Tables $ Right v
              Left _  -> go hasFS g

    push ::
         TestBlock
      -> [RealPoint TestBlock] -> [RealPoint TestBlock]
    push b = (blockRealPoint b:)

    switch ::
         Word64
      -> [TestBlock]
      -> [RealPoint TestBlock] -> [RealPoint TestBlock]
    switch 0 bs = (reverse (map blockRealPoint bs) ++)
    switch n bs = switch 0 bs . drop (fromIntegral n)

    -- We don't currently test the case where the LedgerDB cannot support
    -- the full rollback range. See also
    -- <https://github.com/input-output-hk/ouroboros-network/issues/1025>
    ignoreExceedRollback :: Either ExceededRollback a -> a
    ignoreExceedRollback (Left  _) = error "unexpected ExceededRollback"
    ignoreExceedRollback (Right a) = a

    upd :: ( [RealPoint TestBlock] -> [RealPoint TestBlock] )
        -> (   LedgerDB' TestBlock
            -> m (Either (ExtValidationError TestBlock) (LedgerDB' TestBlock))
           )
        -> m (Success DiskSnapshot)
    upd f g = do
        -- We cannot run the whole thing in a transaction, since computing the
        -- new value of the ledger DB may require reading from the chain DB
        (rs, db) <- atomically $ readTVar dbState
        mDB'     <- g db
        case mDB' of
          Left  e   -> return $ MaybeErr (Left e)
          Right db' -> do atomically $ writeTVar dbState (f rs, db')
                          return $ MaybeErr (Right ())

    truncateSnapshot :: SomeHasFS m -> DiskSnapshot -> m ()
    truncateSnapshot (SomeHasFS hasFS@HasFS{..}) ss = do
        withFile hasFS (snapshotToStatePath ss) (AppendMode AllowExisting) $ \h ->
          hTruncate h 0
        withFile hasFS (snapshotToTablesPath ss) (AppendMode AllowExisting) $ \h ->
          hTruncate h 0

    refValPair :: TestBlock -> (RealPoint TestBlock, TestBlock)
    refValPair b = (blockRealPoint b, b)

{-------------------------------------------------------------------------------
  References
-------------------------------------------------------------------------------}

newtype At f r = At (f (Reference DiskSnapshot r))
type    f :@ r = At f r

deriving instance Show (f (Reference DiskSnapshot r)) => Show (At f r)

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

type SnapRefs r = [(Reference DiskSnapshot r, MockSnap)]

(!) :: Eq r => [(r, a)] -> r -> a
env ! r = fromJust (lookup r env)

data Model r = Model {
    modelMock  :: Mock
  , modelSnaps :: SnapRefs r
  }
  deriving (Generic)

deriving instance Show1 r => Show (Model r)

initModel :: SecurityParam -> Model r
initModel secParam = Model (mockInit secParam) []

toMock :: (Functor f, Eq1 r) => Model r -> f :@ r -> f MockSnap
toMock m (At fr) = (modelSnaps m !) <$> fr

step :: Eq1 r => Model r -> Cmd :@ r -> (Resp MockSnap, Mock)
step m cmd = runMock (toMock m cmd) (modelMock m)

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event r = Event {
      eventBefore   :: Model    r
    , eventCmd      :: Cmd   :@ r
    , eventResp     :: Resp  :@ r
    , eventAfter    :: Model    r
    , eventMockResp :: Resp  MockSnap
    }

deriving instance Show1 r => Show (Event r)

lockstep :: Eq1 r
         => Model    r
         -> Cmd   :@ r
         -> Resp  :@ r
         -> Event    r
lockstep m@(Model _ hs) cmd (At resp) = Event {
      eventBefore   = m
    , eventCmd      = cmd
    , eventResp     = At resp
    , eventAfter    = Model mock' (hs' <> hs) -- new references override old!
    , eventMockResp = resp'
    }
  where
    (resp', mock') = step m cmd
    hs' = zip (toList resp) (toList resp')

execCmd :: Model Symbolic
        -> QSM.Command (At Cmd) (At Resp)
        -> Event Symbolic
execCmd model (QSM.Command cmd resp _vars) = lockstep model cmd resp

execCmds :: SecurityParam
         -> QSM.Commands (At Cmd) (At Resp)
         -> [Event Symbolic]
execCmds secParam = \(QSM.Commands cs) -> go (initModel secParam) cs
  where
    go :: Model Symbolic
       -> [QSM.Command (At Cmd) (At Resp)]
       -> [Event Symbolic]
    go _ []     = []
    go m (c:cs) = e : go (eventAfter e) cs
      where
        e = execCmd m c

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

-- | Generated commands distribution frequency used by 'generator'.
--
-- The values in this type determine what is passed to QuickCheck's 'frequency'.
data CmdDistribution = CmdDistribution
    { freqCurrent   :: Int
    , freqPush      :: Int
    , freqSwitch    :: Int
    , freqSnap      :: Int
    , freqFlush     :: Int
    , freqRestore   :: Int
    , freqCorrupt   :: Int
    , freqDrop      :: Int
    , freqGetTables :: Int
    }
    deriving (Generic, Show, Eq)

generator ::
     CmdDistribution
  -> SecurityParam
  -> Model Symbolic
  -> Maybe (Gen (Cmd :@ Symbolic))
generator cd secParam (Model mock hs) =
  Just $ QC.frequency $ concat [
      withoutRef
    , if null possibleCorruptions
        then []
        else [(freqCorrupt cd, (At . uncurry Corrupt) <$> QC.elements possibleCorruptions)]
    ]
  where
    withoutRef :: [(Int, Gen (Cmd :@ Symbolic))]
    withoutRef = fmap (bimap ($ cd) (fmap At)) [
          (freqCurrent,   pure Current)
        , (freqPush,      Push <$> genBlockFromLedgerState (mockCurrent mock))
        , (freqSwitch,    genSwitchCmd)
        , (freqSnap,      pure Snap)
        , (freqFlush,     pure Flush)
        , (freqRestore,   pure Restore)
        , (freqDrop,      Drop <$> QC.choose (0, mockChainLength mock))
        , (freqGetTables, genGetTables)
        ]
      where
        genGetTables :: Gen (Cmd ss)
        genGetTables =
          let randomGetTablesAtFor = do
                pt <- QC.arbitrary
                keys <- ExtLedgerStateTables
                      . TokenToTValue
                      . KeysMK
                    <$> QC.arbitrary
                pure $ GetTablesAtFor pt keys
          in
          if null (mockLedger mock)
            then randomGetTablesAtFor
            else do
              QC.frequency
               [ (5, do
                     -- existing point, subset of keys
                     (blk, st) <- QC.elements (mockLedger mock)
                     keys <- traverseLedgerTables f (projectLedgerTables st)
                     pure $ GetTablesAtFor (blockPoint blk) keys)
               , (1, do
                     -- existing point, random keys
                     (blk, _) <- QC.elements (mockLedger mock)
                     keys <- ExtLedgerStateTables
                          .  TokenToTValue
                          .  KeysMK
                         <$> QC.arbitrary
                     pure $ GetTablesAtFor (blockPoint blk) keys)
               , (1, randomGetTablesAtFor)
               ]
          where
            f :: Ord k => ValuesMK k v -> Gen (KeysMK k v)
            f (ValuesMK vals) = do
              requested <- QC.sublistOf (Set.toList $ Map.keysSet vals)
              pure $ KeysMK $ Set.fromList requested

        mockCurrent :: Mock -> ExtLedgerState TestBlock ValuesMK
        mockCurrent Mock{..} =
          case mockLedger of
            []       -> testInitExtLedgerWithState initialTestLedgerState
            (_, l):_ -> l

        genSwitchCmd :: Gen (Cmd ss)
        genSwitchCmd = do
          let maxRollback = minimum [
                  mockMaxRollback mock
                , maxRollbacks secParam
                ]
          numRollback  <- QC.choose (0, maxRollback)
          numNewBlocks <- QC.choose (numRollback, numRollback + 2)
          let
            afterRollback = mockRollback numRollback mock
            blocks        = genBlocks
                              numNewBlocks
                              (lastAppliedPoint . ledgerState . mockCurrent $ afterRollback)
          return $ Switch numRollback blocks

    possibleCorruptions :: [(Corruption, Reference DiskSnapshot Symbolic)]
    possibleCorruptions = concatMap aux hs
      where
        aux :: (Reference DiskSnapshot Symbolic, MockSnap)
            -> [(Corruption, Reference DiskSnapshot Symbolic)]
        aux (diskSnap, mockSnap) =
            case Map.lookup mockSnap (mockSnaps mock) of
              Just (_tip, state) ->
                map (, diskSnap) $ possibleCorruptionsInState state
              Nothing ->
                -- The snapshot has already been deleted
                []
          where
            possibleCorruptionsInState :: SnapState -> [Corruption]
            possibleCorruptionsInState SnapOk        = [Delete, Truncate]
            possibleCorruptionsInState SnapCorrupted = [Delete]

shrinker :: Model Symbolic -> Cmd :@ Symbolic -> [Cmd :@ Symbolic]
shrinker _ (At cmd) =
    case cmd of
      Current      -> []
      Push _b      -> []
      Snap         -> []
      Flush        -> []
      Restore      -> []
      GetTablesAtFor{} -> []
      Switch 0 [b] -> [At $ Push b]
      Switch n bs  -> if length bs > fromIntegral n
                        then [At $ Switch n (init bs)]
                        else []
      -- an absent snapshot is easier than a corrupted one
      Corrupt c ss -> case c of
                        Truncate -> [At $ Corrupt Delete ss]
                        Delete   -> []
      Drop n       -> At . Drop <$> QC.shrink n

{-------------------------------------------------------------------------------
  Additional type class instances required by QSM
-------------------------------------------------------------------------------}

instance CommandNames (At Cmd) where
  cmdName (At Current{})        = "Current"
  cmdName (At Push{})           = "Push"
  cmdName (At Switch{})         = "Switch"
  cmdName (At Snap{})           = "Snap"
  cmdName (At Flush{})          = "Flush"
  cmdName (At Restore{})        = "Restore"
  cmdName (At Corrupt{})        = "Corrupt"
  cmdName (At Drop{})           = "Drop"
  cmdName (At GetTablesAtFor{}) = "GetTables"

  cmdNames _ = [
      "Current"
    , "Push"
    , "Switch"
    , "Snap"
    , "Flush"
    , "Restore"
    , "Corrupt"
    , "Drop"
    , "GetTables"
    ]

instance Functor f => Rank2.Functor (At f) where
  fmap = \f (At x) -> At $ fmap (lift f) x
    where
      lift :: (r x -> r' x) -> QSM.Reference x r -> QSM.Reference x r'
      lift f (QSM.Reference x) = QSM.Reference (f x)

instance Foldable f => Rank2.Foldable (At f) where
  foldMap = \f (At x) -> foldMap (lift f) x
    where
      lift :: (r x -> m) -> QSM.Reference x r -> m
      lift f (QSM.Reference x) = f x

instance Traversable t => Rank2.Traversable (At t) where
  traverse = \f (At x) -> At <$> traverse (lift f) x
    where
      lift :: Functor f
           => (r x -> f (r' x)) -> QSM.Reference x r -> f (QSM.Reference x r')
      lift f (QSM.Reference x) = QSM.Reference <$> f x

instance ToExpr (Model Concrete)
instance ToExpr SecurityParam
instance ToExpr DiskSnapshot

{-------------------------------------------------------------------------------
  Final state machine
-------------------------------------------------------------------------------}

semantics :: IOLike m
          => StandaloneDB m
          -> Cmd :@ Concrete -> m (Resp :@ Concrete)
semantics db (At cmd) = (At . fmap reference) <$> runDB db (concrete <$> cmd)

transition :: Eq1 r
           => Model    r
           -> Cmd   :@ r
           -> Resp  :@ r
           -> Model    r
transition m cmd = eventAfter . lockstep m cmd

postcondition :: Model    Concrete
              -> Cmd   :@ Concrete
              -> Resp  :@ Concrete
              -> Logic
postcondition m cmd r = toMock (eventAfter e) r .== eventMockResp e
  where
    e = lockstep m cmd r

precondition :: Model Symbolic -> Cmd :@ Symbolic -> Logic
precondition (Model mock hs) (At c) =
        forall (toList c) (`member` map fst hs)
    .&& validCmd c
  where
    -- Maximum rollback might decrease if shrinking removed blocks
    validCmd :: Cmd ss -> Logic
    validCmd (Switch n _) = n .<= mockMaxRollback mock
    validCmd _otherwise   = Top

symbolicResp :: Model           Symbolic
             -> Cmd          :@ Symbolic
             -> GenSym (Resp :@ Symbolic)
symbolicResp m c = At <$> traverse (const genSym) resp
  where
    (resp, _mock') = step m c

sm :: IOLike m
   => SecurityParam
   -> CmdDistribution
   -> StandaloneDB m
   -> StateMachine Model (At Cmd) m (At Resp)
sm secParam cd db = StateMachine {
      initModel     = initModel secParam
    , transition    = transition
    , precondition  = precondition
    , postcondition = postcondition
    , invariant     = Nothing
    , generator     = generator cd secParam
    , shrinker      = shrinker
    , semantics     = semantics db
    , mock          = symbolicResp
    , cleanup       = \_ -> do
        HD.LedgerBackingStore bs <- atomically . readTVar $ dbBackingStore db
        HD.bsClose bs
        dbCleanup $ dbEnv db
    }

prop_sequential ::
     Int
     -- ^ Number of tests to run
  -> IO (DbEnv IO)
  -> CmdDistribution
  -> SecurityParam
  -> QC.Property
prop_sequential maxSuccess mkDbEnv cd secParam = QC.withMaxSuccess maxSuccess $
  forAllCommands (sm secParam cd dbUnused) Nothing $ \cmds ->
    QC.monadicIO $ do
      rr <- QC.run unsafeNewRegistry
      QC.run mkDbEnv >>= \e -> propCmds e cd secParam rr cmds

mkDbTracer :: Trace.Tracer IO HD.BackingStoreTrace
mkDbTracer = show `Trace.contramap` Trace.stdoutTracer

inMemDbEnv :: Trans.MonadIO m
  => m (DbEnv IO)
inMemDbEnv = Trans.liftIO $ do
  fs <- uncheckedNewTVarM MockFS.empty
  let
    dbHasFS = SomeHasFS $ simHasFS fs
    dbBackingStoreSelector = InMemoryBackingStore
    dbCleanup = pure ()
    dbTracer = mkDbTracer
  pure DbEnv{..}

lmdbDbEnv :: Trans.MonadIO m
  => LMDB.LMDBLimits
  -> m (DbEnv IO)
lmdbDbEnv limits = do
  (dbHasFS, dbCleanup) <- Trans.liftIO $ do
      systmpdir <- Dir.getTemporaryDirectory
      tmpdir <- Temp.createTempDirectory systmpdir "init_standalone_db"
      pure (SomeHasFS $ FSIO.ioHasFS $ MountPoint tmpdir, Dir.removeDirectoryRecursive tmpdir)
  let
    dbBackingStoreSelector = LMDBBackingStore limits
    dbTracer = mkDbTracer
  pure DbEnv{..}

-- Ideally we'd like to use @IOSim s@ instead of IO, but unfortunately
-- QSM requires monads that implement MonadIO.
propCmds ::
     DbEnv IO
  -> CmdDistribution
  -> SecurityParam
  -> ResourceRegistry IO
  -> QSM.Commands (At Cmd) (At Resp)
  -> QC.PropertyM IO ()
propCmds dbEnv cd secParam rr cmds = do
    db <- QC.run $ initStandaloneDB dbEnv rr secParam
    let sm' = sm secParam cd db
    (hist, _model, res) <- runCommands sm' cmds
    prettyCommands sm' hist
      $ checkCommandNames cmds
      $ res QC.=== Ok

dbUnused :: StandaloneDB IO
dbUnused = error "DB unused during command generation"

{-------------------------------------------------------------------------------
  Event labelling

  TODO: We need at least a label for restore-after-corruption
-------------------------------------------------------------------------------}

data Tag =
    -- | Restore
    --
    -- We record the length of the chain at the time of the restore (to the
    -- closest power of two) as well as the state of the most recent snapshot,
    -- if any has been created.
    --
    -- We will look for the /maximum/ chain length in each case.
    TagRestore (Maybe SnapState) RangeK

    -- | Tag rollback
    --
    -- We record the rollback length
  | TagMaxRollback RangeK

    -- | Tag chain truncation
    --
    -- We record how many blocks were dropped
  | TagMaxDrop RangeK
  deriving (Show, Eq)

type EventPred = C.Predicate (Event Symbolic) Tag

tagEvents :: SecurityParam -> [Event Symbolic] -> [Tag]
tagEvents k = C.classify [
      tagMaxRollback
    , tagMaxDrop
    , tagRestore Nothing
    , tagRestore (Just SnapOk)
    , tagRestore (Just SnapCorrupted)
    ]
  where
    tagMaxRollback :: EventPred
    tagMaxRollback =
        fmap (TagMaxRollback . rangeK k) $ C.maximum $ \ev ->
          case eventCmd ev of
            At (Switch n _) -> Just n
            _otherwise      -> Nothing

    tagMaxDrop :: EventPred
    tagMaxDrop =
        fmap (TagMaxDrop . rangeK k) $ C.maximum $ \ev ->
          case eventCmd ev of
            At (Drop n) -> Just n
            _otherwise  -> Nothing

    tagRestore :: Maybe SnapState -> EventPred
    tagRestore mST =
        fmap (TagRestore mST . rangeK k) $ C.maximum $ \ev ->
          let mock = modelMock (eventBefore ev) in
          case eventCmd ev of
            At Restore | mockRecentSnap mock == mST -> Just (mockChainLength mock)
            _otherwise                              -> Nothing

{-------------------------------------------------------------------------------
  Inspecting the labelling function
-------------------------------------------------------------------------------}

showLabelledExamples ::
     SecurityParam
  -> CmdDistribution
  -> Maybe Int
  -> (Tag -> Bool) -- ^ Which tag are we interested in?
  -> IO ()
showLabelledExamples secParam cd mReplay relevant = do
    replaySeed <- case mReplay of
                    Nothing   -> getStdRandom $ randomR (1, 999999)
                    Just seed -> return seed

    putStrLn $ "Using replaySeed " ++ show replaySeed

    let args = QC.stdArgs {
            QC.maxSuccess = 10000
          , QC.replay     = Just (QC.mkQCGen replaySeed, 0)
          }

    QC.labelledExamplesWith args $
      forAllCommands (sm secParam cd dbUnused) Nothing $ \cmds ->
        repeatedly QC.collect (run cmds) $
          QC.property True
  where
    run :: QSM.Commands (At Cmd) (At Resp) -> [Tag]
    run = filter relevant . tagEvents secParam . execCmds secParam
