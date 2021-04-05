{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Test.Ouroboros.Storage.ChainDB.Iterator (tests) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Monad.Except
import           Control.Tracer
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map

import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     Iterator (..), IteratorResult (..), StreamFrom (..),
                     StreamTo (..), UnknownRange (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator
                     (IteratorEnv (..), newIterator)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (IteratorKey (..), TraceIteratorEvent (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.VolatileDB (VolatileDB)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

import           Test.Util.Orphans.IOLike ()
import           Test.Util.Tracer (recordingTracerTVar)

import qualified Test.Ouroboros.Storage.ImmutableDB.Mock as ImmutableDB
                     (openDBMock)
import           Test.Ouroboros.Storage.TestBlock
import qualified Test.Ouroboros.Storage.VolatileDB.Mock as VolatileDB
                     (openDBMock)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Iterator"
    [ testProperty "#773 bug in example 1"  prop_773_bug
    , testProperty "#773 correct example 2" prop_773_working
    , testProperty "#1435 case 1" prop_1435_case1
    , testProperty "#1435 case 2" prop_1435_case2
    , testProperty "#1435 case 3" prop_1435_case3
    , testProperty "#1435 case 4" prop_1435_case4
    , testProperty "#1435 case 5" prop_1435_case5
    , testProperty "#1435 case 6" prop_1435_case6
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
--   > data Action = IterNext .. | CopyToImmutableDB .. | GCFromVolatileDB ..
--   And write a generator for it.
-- - Run multiple @Action@s in parallel

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

-- All blocks on the same chain
a, b, c, d, e :: TestBlock
a = firstBlock    0 TestBody { tbForkNo = 0, tbIsValid = True }
b = mkNextBlock a 1 TestBody { tbForkNo = 0, tbIsValid = True }
c = mkNextBlock b 2 TestBody { tbForkNo = 0, tbIsValid = True }
d = mkNextBlock c 3 TestBody { tbForkNo = 0, tbIsValid = True }
e = mkNextBlock d 4 TestBody { tbForkNo = 0, tbIsValid = True }

-- | Requested stream = A -> C
--
--           ImmutableDB        VolatileDB
-- Hash    A -> B -> C -> D        C, D
--
-- Bug: we find a partial path [B]->C in the VolatileDB. Now the 'ForkTooOld'
-- condition is triggered because the tip of the ImmutableDB is not B but D.
--
-- For more details, see:
-- https://github.com/input-output-hk/ouroboros-network/pull/773#issuecomment-513128004
prop_773_bug :: Property
prop_773_bug = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [a, b, c, d]
      , volatile  = [c, d]
      }
    (StreamFromInclusive (blockRealPoint a))
    (StreamToInclusive   (blockRealPoint c))
    (Right (map Right [a, b, c]))

-- | Requested stream = A -> E
--
--           ImmutableDB           VolatileDB
-- Hash    A -> B -> C -> D        C   D   E
--
-- This was/is handled correctly in @streamFromBoth@.
prop_773_working :: Property
prop_773_working = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [a, b, c, d]
      , volatile  = [c, d, e]
      }
    (StreamFromInclusive (blockRealPoint a))
    (StreamToInclusive   (blockRealPoint e))
    (Right (map Right [a, b, c, d, e]))

-- | Requested stream = B' -> B' where EBB, B, and B' are all blocks in the
-- same slot, and B' is not part of the current chain nor ChainDB.
--
--      ImmutableDB      VolatileDB
-- Hash  EBB -> B
--
prop_1435_case1 :: Property
prop_1435_case1 = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [ebb, b]
      , volatile  = []
      }
    (StreamFromInclusive (blockRealPoint b'))
    (StreamToInclusive   (blockRealPoint b'))
    (Left (ForkTooOld (StreamFromInclusive (blockRealPoint b'))))
  where
    canContainEBB = const True
    ebb = firstEBB    canContainEBB       TestBody { tbForkNo = 0, tbIsValid = True }
    b   = mkNextBlock               ebb 0 TestBody { tbForkNo = 0, tbIsValid = True }
    b'  = mkNextBlock               ebb 0 TestBody { tbForkNo = 1, tbIsValid = True }

-- | Requested stream = EBB' -> EBB' where EBB, B, and EBB' are all blocks in
-- the same slot, and EBB' is not part of the current chain nor ChainDB.
--
--      ImmutableDB      VolatileDB
-- Hash  EBB -> B
--
prop_1435_case2 :: Property
prop_1435_case2 = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [ebb, b]
      , volatile  = []
      }
    (StreamFromInclusive (blockRealPoint ebb'))
    (StreamToInclusive   (blockRealPoint ebb'))
    (Left (ForkTooOld (StreamFromInclusive (blockRealPoint ebb'))))
  where
    canContainEBB = const True
    ebb  = firstEBB    canContainEBB       TestBody { tbForkNo = 0, tbIsValid = True }
    b    = mkNextBlock               ebb 0 TestBody { tbForkNo = 0, tbIsValid = True }
    ebb' = firstEBB    canContainEBB       TestBody { tbForkNo = 1, tbIsValid = True }

-- | Requested stream = EBB -> EBB where EBB and B are all blocks in the same
-- slot.
--
--      ImmutableDB      VolatileDB
-- Hash  EBB -> B
--
prop_1435_case3 :: Property
prop_1435_case3 = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [ebb, b]
      , volatile  = []
      }
    (StreamFromInclusive (blockRealPoint ebb))
    (StreamToInclusive   (blockRealPoint ebb))
    (Right (map Right [ebb]))
  where
    canContainEBB = const True
    ebb  = firstEBB    canContainEBB       TestBody { tbForkNo = 0, tbIsValid = True }
    b    = mkNextBlock               ebb 0 TestBody { tbForkNo = 0, tbIsValid = True }

-- | Requested stream = EBB -> EBB where EBB and B are all blocks in the same
-- slot.
--
--       ImmutableDB      VolatileDB
-- Hash     EBB               B
--
prop_1435_case4 :: Property
prop_1435_case4 = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [ebb]
      , volatile  = [b]
      }
    (StreamFromInclusive (blockRealPoint ebb))
    (StreamToInclusive   (blockRealPoint ebb))
    (Right (map Right [ebb]))
  where
    canContainEBB = const True
    ebb  = firstEBB    canContainEBB       TestBody { tbForkNo = 0, tbIsValid = True }
    b    = mkNextBlock               ebb 0 TestBody { tbForkNo = 0, tbIsValid = True }

-- | Requested stream = EBB -> EBB where EBB and B' are all blocks in the same
-- slot, and B' is not part of the current chain nor ChainDB.
--
--       ImmutableDB      VolatileDB
-- Hash     EBB
--
prop_1435_case5 :: Property
prop_1435_case5 = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [ebb]
      , volatile  = []
      }
    (StreamFromInclusive (blockRealPoint b'))
    (StreamToInclusive   (blockRealPoint b'))
    (Left (ForkTooOld (StreamFromInclusive (blockRealPoint b'))))
  where
    canContainEBB = const True
    ebb  = firstEBB    canContainEBB       TestBody { tbForkNo = 0, tbIsValid = True }
    b'   = mkNextBlock               ebb 0 TestBody { tbForkNo = 1, tbIsValid = True }

-- | Requested stream = EBB' -> EBB' where EBB and EBB' are all blocks in the
-- same slot, and EBB' is not part of the current chain nor ChainDB.
--
--       ImmutableDB      VolatileDB
-- Hash     EBB
--
prop_1435_case6 :: Property
prop_1435_case6 = prop_general_test
    TestSetup
      { immutable = Chain.fromOldestFirst [ebb]
      , volatile  = []
      }
    (StreamFromInclusive (blockRealPoint ebb'))
    (StreamToInclusive   (blockRealPoint ebb'))
    (Left (ForkTooOld (StreamFromInclusive (blockRealPoint ebb'))))
  where
    canContainEBB = const True
    ebb  = firstEBB canContainEBB TestBody { tbForkNo = 0, tbIsValid = True }
    ebb' = firstEBB canContainEBB TestBody { tbForkNo = 1, tbIsValid = True }

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

    ppStream :: [Either (RealPoint TestBlock) TestBlock] -> String
    ppStream = intercalate " :> " . map ppGCedOrBlock

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

ppGCedOrBlock :: Either (RealPoint TestBlock) TestBlock -> String
ppGCedOrBlock (Left  gcedPt) = "GCed: " <> condense gcedPt
ppGCedOrBlock (Right blk)    = ppBlock blk

ppBlock :: TestBlock -> String
ppBlock = condense

{-------------------------------------------------------------------------------
  Running an iterator test
-------------------------------------------------------------------------------}

type IterRes = Either (UnknownRange TestBlock)
                      [Either (RealPoint TestBlock) TestBlock]
                      -- Left:  point of garbage collected block
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
      it <- ExceptT $ newIterator itEnv ($ itEnv) r GetBlock from to
      lift $ consume it
    trace <- getTrace
    return (trace, res)
  where
    consume :: Monad m
            => Iterator m TestBlock TestBlock
            -> m [Either (RealPoint TestBlock) TestBlock]
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
    iters       <- uncheckedNewTVarM Map.empty
    nextIterKey <- uncheckedNewTVarM $ IteratorKey 0
    volatileDB  <- openVolatileDB volatile
    immutableDB <- openImmutableDB immutable
    return IteratorEnv
      { itImmutableDB     = immutableDB
      , itVolatileDB      = volatileDB
      , itIterators       = iters
      , itNextIteratorKey = nextIterKey
      , itTracer          = tracer
      }
  where
    -- | Open a mock VolatileDB and add the given blocks
    openVolatileDB :: [TestBlock] -> m (VolatileDB m TestBlock)
    openVolatileDB blocks = do
        (_volatileDBModel, volatileDB) <-
          VolatileDB.openDBMock
            (VolatileDB.mkBlocksPerFile 1)
            TestBlockCodecConfig
        forM_ blocks $ VolatileDB.putBlock volatileDB
        return volatileDB

    epochSize :: EpochSize
    epochSize = 10

    -- | Open a mock ImmutableDB and add the given chain of blocks
    openImmutableDB :: Chain TestBlock -> m (ImmutableDB m TestBlock)
    openImmutableDB chain = do
        (_immutableDBModel, immutableDB) <-
            ImmutableDB.openDBMock
              chunkInfo
              TestBlockCodecConfig
        mapM_ (ImmutableDB.appendBlock immutableDB) (Chain.toOldestFirst chain)
        return immutableDB
      where
        chunkInfo = ImmutableDB.simpleChunkInfo epochSize
