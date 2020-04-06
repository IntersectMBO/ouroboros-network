{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Ouroboros.Storage.ImmutableDB (tests) where

import qualified Codec.Serialise as S
import           Control.Monad (void)
import           Control.Tracer (nullTracer)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block (getHeader)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.ImmutableDB.Parser
                     (chunkFileParser)

import qualified Test.Ouroboros.Storage.ImmutableDB.Primary as Primary
import qualified Test.Ouroboros.Storage.ImmutableDB.StateMachine as StateMachine
import           Test.Ouroboros.Storage.TestBlock
import           Test.Ouroboros.Storage.Util

{------------------------------------------------------------------------------
  The list of all tests
------------------------------------------------------------------------------}

tests :: HasCallStack => TestTree
tests = testGroup "ImmutableDB"
    [ testCase     "ReadFutureSlotError equivalence" test_ReadFutureSlotErrorEquivalence
    , testCase     "openDB with empty index files"   test_openDBEmptyIndexFilesEquivalence
    , testCase     "closeDB is idempotent"           test_closeDBIdempotentEquivalence
    , Primary.tests
    , StateMachine.tests
    ]

fixedChunkInfo :: ChunkInfo
fixedChunkInfo = simpleChunkInfo 10

type Hash = TestHeaderHash

-- Shorthand
openTestDB :: (HasCallStack, IOLike m, Eq h)
           => ResourceRegistry m
           -> HasFS m h
           -> m (ImmutableDB Hash m)
openTestDB registry hasFS =
    fst <$> openDBInternal ImmutableDbArgs
      { registry
      , hasFS
      , chunkInfo   = fixedChunkInfo
      , hashInfo    = testHashInfo
      , tracer      = nullTracer
      , cacheConfig = Index.CacheConfig 2 60
      , valPol      = ValidateMostRecentChunk
      , parser
      }
  where
    parser = chunkFileParser hasFS (const <$> S.decode) isEBB getBinaryInfo
      testBlockIsValid
    isEBB  = testHeaderEpochNoIfEBB fixedChunkInfo . getHeader
    getBinaryInfo = void . testBlockToBinaryInfo

-- Shorthand
withTestDB :: (HasCallStack, IOLike m, Eq h)
           => HasFS m h
           -> (ImmutableDB Hash m -> m a)
           -> m a
withTestDB hasFS k = withRegistry $ \registry ->
    bracket (openTestDB registry hasFS) closeDB k

{------------------------------------------------------------------------------
  Equivalence tests between IO and MockFS
------------------------------------------------------------------------------}

test_ReadFutureSlotErrorEquivalence :: HasCallStack => Assertion
test_ReadFutureSlotErrorEquivalence =
    apiEquivalenceImmDB (expectUserError isReadFutureSlotError) $ \hasFS ->
      withTestDB hasFS $ \db -> do
        _ <- getBlockComponent db GetBlock 0
        return ()
  where
    isReadFutureSlotError ReadFutureSlotError {} = True
    isReadFutureSlotError _                      = False

test_openDBEmptyIndexFilesEquivalence :: Assertion
test_openDBEmptyIndexFilesEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= Origin)) $ \hasFS@HasFS{..} -> do
      -- Create empty index files
      h1 <- hOpen (mkFsPath ["00000.epoch"]) (WriteMode MustBeNew)
      h2 <- hOpen (mkFsPath ["00000.primary"]) (WriteMode MustBeNew)
      h3 <- hOpen (mkFsPath ["00000.secondary"]) (WriteMode MustBeNew)
      hClose h1
      hClose h2
      hClose h3

      withTestDB hasFS getTip

test_closeDBIdempotentEquivalence :: Assertion
test_closeDBIdempotentEquivalence = withRegistry $ \registry ->
    apiEquivalenceImmDB (expectImmDBResult (@?= ())) $ \hasFS -> do
      db <- openTestDB registry hasFS
      closeDB db
      closeDB db
