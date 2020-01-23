{-# LANGUAGE RecordWildCards #-}
module Test.Ouroboros.Storage.ImmutableDB (tests) where

import qualified Codec.Serialise as S
import           Control.Monad (void)
import           Control.Tracer (nullTracer)
import           Data.Binary (get, put)
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (..))
import           Data.Maybe (fromJust)

import           Control.Monad.Class.MonadThrow (bracket)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Block (getHeader)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

import           Ouroboros.Storage.ImmutableDB
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary (PrimaryIndex)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Storage.ImmutableDB.Impl.Validation
                     (ShouldBeFinalised (..), reconstructPrimaryIndex)
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Parser (epochFileParser)

import qualified Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CumulEpochSizes
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
    , testProperty "reconstructPrimaryIndex"         prop_reconstructPrimaryIndex
    , Primary.tests
    , StateMachine.tests
    , CumulEpochSizes.tests
    ]

fixedEpochSize :: EpochSize
fixedEpochSize = 10

type Hash = TestHeaderHash

-- Shorthand
openTestDB :: (HasCallStack, IOLike m)
           => ResourceRegistry m
           -> HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> m (ImmutableDB Hash m)
openTestDB registry hasFS err = fst <$> openDBInternal
    registry
    hasFS
    err
    (fixedSizeEpochInfo fixedEpochSize)
    testHashInfo
    ValidateMostRecentEpoch
    parser
    nullTracer
    (Index.CacheConfig 2 60)
  where
    parser = epochFileParser hasFS (const <$> S.decode) isEBB getBinaryInfo
      testBlockIsValid
    isEBB  = testHeaderEpochNoIfEBB fixedEpochSize . getHeader
    getBinaryInfo = void . testBlockToBinaryInfo

-- Shorthand
withTestDB :: (HasCallStack, IOLike m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> (ImmutableDB Hash m -> m a)
           -> m a
withTestDB hasFS err k = withRegistry $ \registry ->
    bracket (openTestDB registry hasFS err) closeDB k

{------------------------------------------------------------------------------
  Equivalence tests between IO and MockFS
------------------------------------------------------------------------------}

test_ReadFutureSlotErrorEquivalence :: HasCallStack => Assertion
test_ReadFutureSlotErrorEquivalence =
    apiEquivalenceImmDB (expectUserError isReadFutureSlotError) $ \hasFS err ->
      withTestDB hasFS err $ \db -> do
        _ <- getBlockComponent db GetBlock 0
        return ()
  where
    isReadFutureSlotError ReadFutureSlotError {} = True
    isReadFutureSlotError _                      = False

test_openDBEmptyIndexFilesEquivalence :: Assertion
test_openDBEmptyIndexFilesEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= TipGen)) $ \hasFS@HasFS{..} err -> do
      -- Create empty index files
      h1 <- hOpen (mkFsPath ["00000.epoch"]) (WriteMode MustBeNew)
      h2 <- hOpen (mkFsPath ["00000.primary"]) (WriteMode MustBeNew)
      h3 <- hOpen (mkFsPath ["00000.secondary"]) (WriteMode MustBeNew)
      hClose h1
      hClose h2
      hClose h3

      withTestDB hasFS err getTip

test_closeDBIdempotentEquivalence :: Assertion
test_closeDBIdempotentEquivalence = withRegistry $ \registry ->
    apiEquivalenceImmDB (expectImmDBResult (@?= ())) $ \hasFS err -> do
      db <- openTestDB registry hasFS err
      closeDB db
      closeDB db

{------------------------------------------------------------------------------
  reconstructPrimaryIndex
------------------------------------------------------------------------------}

prop_reconstructPrimaryIndex :: PrimaryIndex -> Property
prop_reconstructPrimaryIndex primaryIndex =
    counterexample ("blocksOrEBBs: " <> show blockOrEBBs)    $
    counterexample ("primaryIndex': " <> show primaryIndex') $
    reconstructedPrimaryIndex === primaryIndex'
  where
    reconstructedPrimaryIndex = runIdentity $
      reconstructPrimaryIndex epochInfo hashInfo ShouldNotBeFinalised
                              blockOrEBBs

    -- Remove empty trailing slots because we don't reconstruct them
    primaryIndex' = case Primary.lastFilledSlot primaryIndex of
      Just slot -> Primary.truncateToSlot slot primaryIndex
      -- Index is empty, use the minimal empty index without any trailing
      -- slots
      Nothing   -> fromJust $ Primary.mk [0]

    blockOrEBBs :: [BlockOrEBB]
    blockOrEBBs =
      [ if relSlot == 0 then EBB 0 else Block (coerce relSlot - 1)
      | relSlot <- Primary.filledSlots primaryIndex]

    -- Use maxBound as epoch size so that we can easily map from SlotNo to
    -- RelativeSlot and vice versa.
    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo (EpochSize maxBound)

    -- Only 'hashSize' is used. Note that 32 matches the hard-coded value in
    -- the 'PrimaryIndex' generator we use.
    hashInfo :: HashInfo ()
    hashInfo = HashInfo
      { hashSize = 32
      , getHash  = get
      , putHash  = put
      }
