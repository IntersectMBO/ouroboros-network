{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Ouroboros.Storage.ChainDB.AddBlock
  ( tests
  ) where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Exception (throw)
import           Control.Monad (void)
import qualified Data.ByteString.Lazy as Lazy
import           Data.List (permutations, transpose)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Monad.IOSim

import           Control.Tracer

import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (chunks)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.ChainDB (ChainDbArgs (..),
                     TraceAddBlockEvent (..), addBlock, closeDB, openDB,
                     toChain)
import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common (EpochSize (..))
import           Ouroboros.Storage.EpochInfo (fixedSizeEpochInfo)
import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..),
                     ValidationPolicy (ValidateAllEpochs))
import           Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import           Ouroboros.Storage.LedgerDB.InMemory (ledgerDbDefaultParams)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.NoUnexpectedThunks ()
import           Test.Util.SOP
import           Test.Util.TestBlock

import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import           Test.Ouroboros.Storage.ChainDB.StateMachine ()

tests :: TestTree
tests = testGroup "AddBlock"
    [ testProperty "addBlock multiple threads" prop_addBlock_multiple_threads
    ]


-- | Add a bunch of blocks from multiple threads and check whether the
-- resulting chain is correct.
--
-- Why not use the parallel state machine tests for this? If one thread adds a
-- block that fits onto the current block and the other thread adds a
-- different block that also fits onto the current block, then the outcome
-- depends on scheduling: either outcome is fine, but they will be different.
-- quickcheck-state-machine expects the outcome to be deterministic.
--
-- Hence this test: don't check whether the resulting chain is equal to the
-- model chain, but check whether they are equally preferable (candidates). In
-- fact, it is more complicated than that. Say @k = 2@ and we first add the
-- following blocks in the given order: @A@, @B@, @C@, @A'@, @B'@, @C'@, @D'@,
-- then the current chain will end with @C@, even though the fork ending with
-- @D'@ would be longer. The reason is for this is that we would have to roll
-- back more @k@ blocks to switch to this fork. Now imagine that the last four
-- blocks in the list above get added before the first three, then the current
-- chain would have ended with @D'@. So the order in which blocks are added
-- really matters.
--
-- We take the following approach to check whether the resulting chain is
-- correct:
--
-- * We take the list of blocks to add
-- * For each permutation of this list:
--   - We add all the blocks in the permuted list to the model
--   - We check whether the current chain of the model is equally preferable
--     to the resulting chain. If so, the test passes.
-- * If none of the permutations result in an equally preferable chain, the
--   test fails.
--
-- TODO test with multiple protocols
-- TODO test with different thread schedulings for the simulator
prop_addBlock_multiple_threads :: BlocksPerThread -> Property
prop_addBlock_multiple_threads bpt =
  -- TODO coverage checking
    tabulate "Event" (map constrName trace) $
    counterexample ("Actual chain: " <> condense actualChain) $
    counterexample
      "No interleaving of adding blocks found that results in the same chain" $
    any (equallyPreferable actualChain) (map modelAddBlocks (permutations blks))
  where
    blks = blocks bpt

    hashInfo = testHashInfo (map tbHash blks)

    actualChain :: Chain TestBlock
    trace       :: [TraceAddBlockEvent TestBlock]
    (actualChain, trace) = run $ do
        -- Open the DB
        fsVars <- (,,)
          <$> uncheckedNewTVarM Mock.empty
          <*> uncheckedNewTVarM Mock.empty
          <*> uncheckedNewTVarM Mock.empty
        withRegistry $ \registry -> do
          let args = mkArgs cfg initLedger dynamicTracer registry hashInfo fsVars
          db <- openDB args
          -- Add blocks concurrently
          mapConcurrently_ (mapM_ (addBlock db)) $ blocksPerThread bpt
          -- Obtain the actual chain
          chain <- toChain db
          -- Close
          closeDB db
          return chain

    -- The current chain after all the given blocks were added to a fresh
    -- model.
    modelAddBlocks :: [TestBlock] -> Chain TestBlock
    modelAddBlocks theBlks =
        Model.currentChain $
        Model.addBlocks cfg theBlks $
        -- Make sure no blocks are "from the future"
        Model.advanceCurSlot cfg maxBound $
        Model.empty initLedger

    equallyPreferable :: Chain TestBlock -> Chain TestBlock -> Bool
    equallyPreferable chain1 chain2 =
      compareCandidates cfg (Chain.toAnchoredFragment chain1) (Chain.toAnchoredFragment chain2) == EQ

    cfg :: NodeConfig (BlockProtocol TestBlock)
    cfg = singleNodeTestConfig

    initLedger :: ExtLedgerState TestBlock
    initLedger = testInitExtLedger

    dynamicTracer :: Tracer (SimM s) (ChainDB.TraceEvent TestBlock)
    dynamicTracer = Tracer $ \case
      ChainDB.TraceAddBlockEvent ev -> traceM ev
      _                             -> return ()

    run :: (forall s. SimM s a) -> (a, [TraceAddBlockEvent TestBlock])
    run m = (res, evs)
      where
      tr  = runSimTrace m
      res = either throw id $ traceResult True tr
      evs = selectTraceEventsDynamic tr

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mapConcurrently_ :: forall m a. IOLike m => (a -> m ()) -> [a] -> m ()
mapConcurrently_ f = go
  where
    go :: [a] -> m ()
    go []     = return ()
    go (x:xs) =
      withAsync (f  x)  $ \y  ->
      withAsync (go xs) $ \ys ->
        void $ waitBoth y ys

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

newtype Threads = Threads { unThreads :: Int }
    deriving (Eq, Show)

instance Arbitrary Threads where
  arbitrary = Threads <$> choose (2, 10)
  shrink (Threads n) = [Threads n' | n' <- [2..n-1]]


data BlocksPerThread = BlocksPerThread
  { _blockTree   :: !BlockTree
  , _permutation :: !Permutation
  , _threads     :: !Threads
  }

blocks :: BlocksPerThread -> [TestBlock]
blocks BlocksPerThread{..} =
    permute _permutation $ treeToBlocks _blockTree

blocksPerThread :: BlocksPerThread -> [[TestBlock]]
blocksPerThread bpt@BlocksPerThread{..} =
    transpose $ chunks (unThreads _threads) $ blocks bpt

instance Show BlocksPerThread where
  show bpt = unlines $ zipWith
    (\(i :: Int) blks -> "thread " <> show i <> ": " <> condense blks)
    [0..]
    (blocksPerThread bpt)

instance Arbitrary BlocksPerThread where
  arbitrary = BlocksPerThread
     -- Limit the number of blocks to 10, because the number of interleavings
     -- grows factorially.
    <$> scale (min 10) arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink (BlocksPerThread bt p t) =
    -- No need to shrink permutations
    [BlocksPerThread bt  p t' | t'  <- shrink t]  <>
    [BlocksPerThread bt' p t  | bt' <- shrink bt]


{-------------------------------------------------------------------------------
  ChainDB args
-------------------------------------------------------------------------------}

fixedEpochSize :: EpochSize
fixedEpochSize = 10

mkArgs :: IOLike m
       => NodeConfig (BlockProtocol TestBlock)
       -> ExtLedgerState TestBlock
       -> Tracer m (ChainDB.TraceEvent TestBlock)
       -> ResourceRegistry m
       -> HashInfo TestHash
       -> (StrictTVar m MockFS, StrictTVar m MockFS, StrictTVar m MockFS)
          -- ^ ImmutableDB, VolatileDB, LedgerDB
       -> ChainDbArgs m TestBlock
mkArgs cfg initLedger tracer registry hashInfo
       (immDbFsVar, volDbFsVar, lgrDbFsVar) = ChainDbArgs
    { -- Decoders
      cdbDecodeHash       = decode
    , cdbDecodeBlock      = const <$> decode
    , cdbDecodeHeader     = const <$> decode
    , cdbDecodeLedger     = decode
    , cdbDecodeChainState = decode

      -- Encoders
    , cdbEncodeHash       = encode
    , cdbEncodeBlock      = addDummyBinaryInfo . encode
    , cdbEncodeHeader     = encode
    , cdbEncodeLedger     = encode
    , cdbEncodeChainState = encode

      -- Error handling
    , cdbErrImmDb         = EH.monadCatch
    , cdbErrVolDb         = EH.monadCatch
    , cdbErrVolDbSTM      = EH.throwSTM

      -- HasFS instances
    , cdbHasFSImmDb       = simHasFS EH.monadCatch immDbFsVar
    , cdbHasFSVolDb       = simHasFS EH.monadCatch volDbFsVar
    , cdbHasFSLgrDB       = simHasFS EH.monadCatch lgrDbFsVar

      -- Policy
    , cdbValidation       = ValidateAllEpochs
    , cdbBlocksPerFile    = 4
    , cdbParamsLgrDB      = ledgerDbDefaultParams (protocolSecurityParam cfg)
    , cdbDiskPolicy       = defaultDiskPolicy (protocolSecurityParam cfg) 10000

      -- Integration
    , cdbNodeConfig       = cfg
    , cdbEpochInfo        = fixedSizeEpochInfo fixedEpochSize
    , cdbHashInfo         = hashInfo
    , cdbIsEBB            = const Nothing
    , cdbCheckIntegrity   = const True
    , cdbBlockchainTime   = fixedBlockchainTime maxBound
    , cdbGenesis          = return initLedger
    , cdbAddHdrEnv        = const id

    -- Misc
    , cdbTracer           = tracer
    , cdbTraceLedger      = nullTracer
    , cdbRegistry         = registry
    , cdbGcDelay          = 0
    }
  where
    addDummyBinaryInfo :: CBOR.Encoding -> BinaryInfo CBOR.Encoding
    addDummyBinaryInfo blob = BinaryInfo
      { binaryBlob   = blob
      -- The serialised @Header TestBlock@ is the same as the serialised
      -- @TestBlock@
      , headerOffset = 0
      , headerSize   = fromIntegral $ Lazy.length (CBOR.toLazyByteString blob)
      }
