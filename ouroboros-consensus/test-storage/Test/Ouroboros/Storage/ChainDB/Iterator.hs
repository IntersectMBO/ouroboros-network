{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Storage.ChainDB.Iterator
  ( tests
  ) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Codec.Serialise (decode, encode, serialiseIncremental)
import           Control.Monad.Except
import           Control.Tracer
import qualified Data.ByteString.Lazy as Lazy
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)

import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     HeaderHash, SlotNo (..), blockPoint)
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.ChainDB.API (Iterator (..), IteratorId (..),
                     IteratorResult (..), StreamFrom (..), StreamTo (..),
                     UnknownRange, deserialiseIterator)
import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB, getPointAtTip,
                     mkImmDB)
import           Ouroboros.Storage.ChainDB.Impl.Iterator (IteratorEnv (..),
                     newIterator)
import           Ouroboros.Storage.ChainDB.Impl.Types (TraceIteratorEvent (..))
import           Ouroboros.Storage.ChainDB.Impl.VolDB (VolDB, mkVolDB)
import           Ouroboros.Storage.Common (BinaryInfo (..), EpochSize)
import           Ouroboros.Storage.EpochInfo (fixedSizeEpochInfo)
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import qualified Ouroboros.Storage.VolatileDB as VolDB

import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock
import           Test.Util.Tracer (recordingTracerTVar)

import qualified Test.Ouroboros.Storage.ImmutableDB.Mock as ImmDB (openDBMock)
import qualified Test.Ouroboros.Storage.VolatileDB.Mock as VolDB (openDBMock)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Iterator"
    [ testProperty "#773 bug in example 1"  prop_773_bug
    , testProperty "#773 correct example 2" prop_773_working
    ]

-- These tests focus on the implementation of the ChainDB iterators, which are
-- used to stream blocks from the ChainDB. A few things make this code
-- complex:
--
-- * We need to be able to stream from both the ImmutableDB and the
--   VolatileDB.
-- * While streaming, blocks might be copied from the VolatileDB to the
--   ImmutableDB.
-- * While streaming, blocks might be garbage-collected from the VolatileDB.
--   These blocks might have been copied to the ImmutableDB or not.
--
-- The copying and garbage collection will happen in the background,
-- /concurrently/ with the streaming, so we have to be careful about race
-- conditions. For these reasons, we provide separate tests for the ChainDb
-- iterators.
--
-- To avoid the complexity of a whole ChainDB and to have explicit control of
-- the copying and garbage collection, we set up a mock 'IteratorEnv' record
-- containing (amongst others) a mock ImmutableDB and a mock VolatileDB that
-- can be manipulated directly, instead of relying on the background threads
-- to manipulate them for us.

-- TODO (#766):
-- - Write a generator for TestSetup and a model implementation (reuse
--   ChainDB.Model) to turn this into a property test.
-- - Instead of simply reading all blocks, use:
--   > data Action = IterNext .. | CopyToImmDB .. | GCFromVolDB ..
--   And write a generator for it.
-- - Run multiple @Action@s in parallel

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

-- All blocks on the same chain
a, b, c, d, e :: TestBlock
a = mkBlk [0]
b = mkBlk [0,0]
c = mkBlk [0,0,0]
d = mkBlk [0,0,0,0]
e = mkBlk [0,0,0,0,0]

-- | Requested stream = A -> C
--
--              ImmDB              VolDB
-- Hash    A -> B -> C -> D        C, D
--
-- Bug: we find a partial path [B]->C in the VolDB. Now the 'ForkTooOld'
-- condition is triggered because the tip of the ImmDB is not B but D.
--
-- For more details, see:
-- https://github.com/input-output-hk/ouroboros-network/pull/773#issuecomment-513128004
prop_773_bug :: Property
prop_773_bug = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [a, b, c, d]
      , volatile  = [c, d]
      }
    (StreamFromInclusive (blockPoint a))
    (StreamToInclusive   (blockPoint c))
    (Right (map Right [a, b, c]))

-- | Requested stream = A -> E
--
--              ImmDB              VolDB
-- Hash    A -> B -> C -> D        C   D   E
--
-- This was/is handled correctly in @streamFromBoth@.
prop_773_working :: Property
prop_773_working = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [a, b, c, d]
      , volatile  = [c, d, e]
      }
    (StreamFromInclusive (blockPoint a))
    (StreamToInclusive   (blockPoint e))
    (Right (map Right [a, b, c, d, e]))

-- | The general property test
prop_general_test
  :: TestSetup
  -> StreamFrom TestBlock
  -> StreamTo   TestBlock
  -> IterRes
  -> Property
prop_general_test setup from to expected =
    counterexample (testSetupInfo setup) $
    case (actual, expected) of
      (Left actualErr, Left expectedErr)         -> actualErr === expectedErr
      (Left actualErr, Right expectedStream)     -> failure $
        "Got " <> show actualErr <> "\nbut expected " <> ppStream expectedStream
      (Right actualStream, Left expectedErr)     -> failure $
        "Got " <> ppStream actualStream <> "\nbut expected " <> show expectedErr
      (Right actualStream, Right expectedStream)
        | actualStream == expectedStream
        -> property True
        | otherwise
        -> failure $ "Got " <> ppStream actualStream <> "\nbut expected " <>
            ppStream expectedStream
  where
    (_trace, actual) = runIterator setup from to
    failure msg = counterexample msg False

    ppStream :: [Either (HeaderHash TestBlock) TestBlock] -> String
    ppStream = intercalate " :> " . map ppEBBOrBlock

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

-- | The initial contents of the ImmutableDB and the VolatileDB.
--
-- Note that the iterator implementation does not rely on the current
-- in-memory chain.
data TestSetup = TestSetup
  { immutable :: Chain TestBlock
  , volatile  :: [TestBlock]
  }

mkBlk :: [Word64] -> TestBlock
mkBlk h = TestBlock
    { tbHash  = testHashFromList h
    , tbSlot  = SlotNo $ fromIntegral $ 2 * length h
    , tbValid = True
    }

-- | Human-friendly string description of the 'TestSetup' that can be used
-- when printing a failing test.
testSetupInfo :: TestSetup -> String
testSetupInfo TestSetup { immutable, volatile } = mconcat
    [ "Immutable: "
    , intercalate " :> " (map ppBlock (Chain.toOldestFirst immutable))
    , "\n"
    , "Volatile:  "
    , intercalate ", " (map ppBlock volatile)
    ]

ppEBBOrBlock :: Either (HeaderHash TestBlock) TestBlock -> String
ppEBBOrBlock (Left  ebbHash) = "EBB " <> condense ebbHash
ppEBBOrBlock (Right blk)     = ppBlock blk

ppBlock :: TestBlock -> String
ppBlock = condense . blockHash

{-------------------------------------------------------------------------------
  Running an iterator test
-------------------------------------------------------------------------------}

type IterRes = Either (UnknownRange TestBlock)
                      [Either (HeaderHash TestBlock) TestBlock]
                      -- Left:  EBB hash
                      -- Right: regular block

-- | Open an iterator with the given bounds on the given 'TestSetup'. Return a
-- trace of the 'TraceIteratorEvent's produced and the result of the iterator
-- itself.
runIterator
  :: TestSetup
  -> StreamFrom TestBlock
  -> StreamTo   TestBlock
  -> ([TraceIteratorEvent TestBlock], IterRes)
runIterator setup from to = runSimOrThrow $ withRegistry $ \r -> do
    (tracer, getTrace) <- recordingTracerTVar
    itEnv <- initIteratorEnv setup tracer
    res <- runExceptT $ do
      it <- ExceptT $ newIterator itEnv ($ itEnv) r from to
      lift $ consume (deserialiseIterator it)
    trace <- getTrace
    return (trace, res)
  where
    consume :: Monad m
            => Iterator m TestBlock
            -> m [Either TestHash TestBlock]
    consume it = iteratorNext it >>= \case
      IteratorResult blk -> (Right blk :) <$> consume it
      IteratorBlockGCed hash -> do
        iteratorClose it
        return [Left hash]
      IteratorExhausted -> do
        iteratorClose it
        return []

{-------------------------------------------------------------------------------
  Setting up a mock IteratorEnv
-------------------------------------------------------------------------------}

initIteratorEnv
  :: forall m. IOLike m
  => TestSetup
  -> Tracer m (TraceIteratorEvent TestBlock)
  -> m (IteratorEnv m TestBlock)
initIteratorEnv TestSetup { immutable, volatile } tracer = do
    iters      <- uncheckedNewTVarM Map.empty
    nextIterId <- uncheckedNewTVarM $ IteratorId 0
    volDB      <- openVolDB volatile
    immDB      <- openImmDB immutable
    return IteratorEnv
      { itImmDB          = immDB
      , itVolDB          = volDB
      , itGetImmDBTip    = getPointAtTip immDB
      , itIterators      = iters
      , itNextIteratorId = nextIterId
      , itTracer         = tracer
      }
  where
    -- | Open a mock VolatileDB and add the given blocks
    openVolDB :: [TestBlock] -> m (VolDB m TestBlock)
    openVolDB blocks = do
        (_volDBModel, volDB) <- VolDB.openDBMock EH.throwSTM 1
        forM_ blocks $ \block ->
          VolDB.putBlock volDB (blockInfo block) (serialiseIncremental block)
        return $ mkVolDB volDB (const <$> decode) (const <$> decode)
          (addDummyBinaryInfo . encode) isEBB addHdrEnv
          EH.monadCatch EH.throwSTM
      where
        isEBB = const IsNotEBB

    blockInfo :: TestBlock -> VolDB.BlockInfo (HeaderHash TestBlock)
    blockInfo tb = VolDB.BlockInfo
      { VolDB.bbid          = blockHash tb
      , VolDB.bslot         = blockSlot tb
      , VolDB.bpreBid       = case blockPrevHash tb of
          GenesisHash -> Origin
          BlockHash h -> At h
      , VolDB.bisEBB        = IsNotEBB
      , VolDB.bheaderOffset = 0
      , VolDB.bheaderSize   = 0
      }

    epochSize :: EpochSize
    epochSize = 10

    addHdrEnv :: IsEBB -> Lazy.ByteString -> Lazy.ByteString
    addHdrEnv = const id

    -- | Open a mock ImmutableDB and add the given chain of blocks
    openImmDB :: Chain TestBlock -> m (ImmDB m TestBlock)
    openImmDB chain = do
        (_immDBModel, immDB) <- ImmDB.openDBMock EH.monadCatch (const epochSize)
        forM_ (Chain.toOldestFirst chain) $ \block ->
          ImmDB.appendBlock immDB
            (blockSlot block) (blockHash block)
            (addDummyBinaryInfo (serialiseIncremental block))
        return $ mkImmDB immDB (const <$> decode) (const <$> decode)
          (addDummyBinaryInfo . encode) epochInfo isEBB addHdrEnv EH.monadCatch
      where
        epochInfo = fixedSizeEpochInfo epochSize
        isEBB     = const Nothing

addDummyBinaryInfo :: blk -> BinaryInfo blk
addDummyBinaryInfo blob = BinaryInfo
  { binaryBlob   = blob
  , headerOffset = 0
  , headerSize   = 0
  }
