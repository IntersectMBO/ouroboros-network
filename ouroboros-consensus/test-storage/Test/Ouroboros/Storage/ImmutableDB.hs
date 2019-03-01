{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
module Test.Ouroboros.Storage.ImmutableDB (tests) where

import qualified Codec.Serialise as S
import           Control.Monad (forM_, void)
import           Control.Monad.Class.MonadSTM (MonadSTM)
import           Control.Monad.Class.MonadThrow (MonadCatch)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust, isNothing, maybeToList)
import           Data.Word (Word64)

import qualified System.IO as IO

import qualified Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CumulEpochSizes
import qualified Test.Ouroboros.Storage.ImmutableDB.StateMachine as StateMachine
import           Test.Ouroboros.Storage.ImmutableDB.TestBlock hiding (tests)
import qualified Test.Ouroboros.Storage.ImmutableDB.TestBlock as TestBlock
import           Test.Ouroboros.Storage.Util

import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, run)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..))
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import qualified Ouroboros.Storage.FS.Sim.STM as Sim
import           Ouroboros.Storage.ImmutableDB
import           Ouroboros.Storage.ImmutableDB.CumulEpochSizes
                     (RelativeSlot (..))
import           Ouroboros.Storage.ImmutableDB.Index
import           Ouroboros.Storage.ImmutableDB.Util (cborEpochFileParser',
                     reconstructSlotOffsets)
import           Ouroboros.Storage.Util (decodeIndexEntryAt)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  The list of all tests
------------------------------------------------------------------------------}

tests :: HasCallStack => TestTree
tests = testGroup "ImmutableDB"
    [ testCase     "AppendToSlotInThePastError equivalence" test_AppendToSlotInThePastErrorEquivalence
    , testCase     "ReadFutureSlotError equivalence" test_ReadFutureSlotErrorEquivalence
    , testCase     "Starting a new epoch pads the previous epoch's index" test_startNewEpochPadsTheIndexFile
    , testCase     "openDB with an empty index file" test_openDBEmptyIndexFileEquivalence
    , testCase     "Reopen the database" test_reopenDBEquivalence
    , testCase     "closeDB is idempotent" test_closeDBIdempotentEquivalence
    , testCase     "appendBinaryBlob after closeDB throws a ClosedDBError" test_closeDBAppendBinaryBlobEquivalence
    , testGroup "Index"
      [ testProperty "Slotoffsets/Index roundtrip " prop_indexToSlotOffsets_indexFromSlotOffsets
      , testProperty "isFilledSlot iff in filledSlots" prop_filledSlots_isFilledSlot
      , testProperty "writeIndex/loadIndex roundtrip" prop_writeIndex_loadIndex
      , testProperty "writeSlotOffsets/loadIndex/indexToSlotOffsets roundtrip" prop_writeSlotOffsets_loadIndex_indexToSlotOffsets
      ]
    , testCase     "reconstructSlotOffsets" test_reconstructSlotOffsets
    , testCase     "reconstructSlotOffsets empty slots" test_reconstructSlotOffsets_empty_slots
    , testCase     "cborEpochFileParser" test_cborEpochFileParser
    , TestBlock.tests
    , StateMachine.tests
    , CumulEpochSizes.tests
    ]


fixedEpochSize :: EpochSize
fixedEpochSize = 10

fixedGetEpochSize :: Monad m => Epoch -> m EpochSize
fixedGetEpochSize _ = return fixedEpochSize


-- Shorthand
openTestDB :: (HasCallStack, MonadSTM m, MonadCatch m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> m (ImmutableDB m, Maybe Slot)
openTestDB hasFS err =
    openDB hasFS err fixedGetEpochSize ValidateMostRecentEpoch parser
  where
    parser = TestBlock.testBlockEpochFileParser' hasFS

-- Shorthand
withTestDB :: (HasCallStack, MonadSTM m, MonadCatch m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> (ImmutableDB m -> m a)
           -> m a
withTestDB hasFS err f = withDB (openTestDB hasFS err) (\db _ -> f db)

{------------------------------------------------------------------------------
  Equivalence tests between IO and MockFS
------------------------------------------------------------------------------}

-- Trying to append to a slot \"in the past\" should be an error
test_AppendToSlotInThePastErrorEquivalence :: HasCallStack => Assertion
test_AppendToSlotInThePastErrorEquivalence =
    apiEquivalenceImmDB (expectUserError isAppendToSlotInThePastError) $ \hasFS err ->
      withTestDB hasFS err $ \db -> do
        appendBinaryBlob db 3 "test"
        appendBinaryBlob db 2 "haskell"
  where
    isAppendToSlotInThePastError AppendToSlotInThePastError {} = True
    isAppendToSlotInThePastError _                             = False

test_ReadFutureSlotErrorEquivalence :: HasCallStack => Assertion
test_ReadFutureSlotErrorEquivalence =
    apiEquivalenceImmDB (expectUserError isReadFutureSlotError) $ \hasFS err ->
      withTestDB hasFS err $ \db -> do
        _ <- getBinaryBlob db 0
        return ()
  where
    isReadFutureSlotError ReadFutureSlotError {} = True
    isReadFutureSlotError _                      = False

test_openDBEmptyIndexFileEquivalence :: Assertion
test_openDBEmptyIndexFileEquivalence =
    apiEquivalenceImmDB (expectUnexpectedError isInvalidFileError) $ \hasFS@HasFS{..} err -> do
      -- Create an empty index file
      h1 <- hOpen ["epoch-000.dat"] IO.WriteMode
      h2 <- hOpen ["index-000.dat"] IO.WriteMode
      hClose h1
      hClose h2

      withTestDB hasFS err $ \_db ->
        return ()
  where
    isInvalidFileError InvalidFileError {} = True
    isInvalidFileError _                   = False

test_reopenDBEquivalence :: Assertion
test_reopenDBEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= 6)) $ \hasFS err -> do
      withTestDB hasFS err $ \db ->
        appendBinaryBlob db 5 (testBlockToBuilder (TestBlock 5))
      withTestDB hasFS err $ \db ->
        getNextSlot db

test_closeDBIdempotentEquivalence :: Assertion
test_closeDBIdempotentEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= ())) $ \hasFS err -> do
      db <- fst <$> openTestDB hasFS err
      closeDB db
      closeDB db


test_closeDBAppendBinaryBlobEquivalence :: Assertion
test_closeDBAppendBinaryBlobEquivalence =
    apiEquivalenceImmDB (expectUserError isClosedDBError) $ \hasFS err -> do
      db <- fst <$> openTestDB hasFS err
      closeDB db
      appendBinaryBlob db 0 "foo"
  where
    isClosedDBError ClosedDBError {} = True
    isClosedDBError _                = False

{------------------------------------------------------------------------------
  Testing the index file format
------------------------------------------------------------------------------}

getIndexContents :: MockFS -> FsPath -> [Word64]
getIndexContents fs f =
  case FS.index f (Mock.mockFiles fs) of
    Left e           -> fail $ show e
    Right (Folder _) -> fail "Index file is a folder"
    Right (File bs)  -> byteStringToWord64s bs

byteStringToWord64s :: ByteString -> [Word64]
byteStringToWord64s bs = go 0
  where
    len = BS.length bs
    go i | i + 8 <= len = decodeIndexEntryAt i bs : go (i + 8)
         | otherwise    = []

test_startNewEpochPadsTheIndexFile :: Assertion
test_startNewEpochPadsTheIndexFile = withMockFS tryImmDB assrt $ \hasFS err ->
    withTestDB hasFS err $ \db -> do
      appendBinaryBlob db 0 "a"
      appendBinaryBlob db 1 "bravo"
      appendBinaryBlob db 4 "haskell"
      appendBinaryBlob db 21 "c"
  where
    assrt (Left _)        = assertFailure "Unexpected error"
    assrt (Right (_, fs)) = do
      getIndexContents fs ["index-000.dat"] @?= [0, 1, 6, 6, 6, 13, 13, 13, 13, 13, 13]
      getIndexContents fs ["index-001.dat"] @?= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      getIndexContents fs ["index-002.dat"] @?= []

{------------------------------------------------------------------------------
  Index
------------------------------------------------------------------------------}

newtype SlotOffsets = SlotOffsets { getSlotOffsets :: NonEmpty SlotOffset }
  deriving (Show)

instance Arbitrary SlotOffsets where
  arbitrary = SlotOffsets . NE.reverse . (0 NE.:|) <$> orderedList
  shrink (SlotOffsets offsets) =
    [ SlotOffsets offsets'
    | offsetList <- shrink $ NE.toList offsets
    , offsets' <- maybeToList $ NE.nonEmpty offsetList ]

instance Arbitrary Index where
  arbitrary = indexFromSlotOffsets . getSlotOffsets <$> arbitrary
  shrink index =
    [ indexFromSlotOffsets $ getSlotOffsets offsets'
    | offsets' <- shrink (SlotOffsets (indexToSlotOffsets index)) ]

prop_indexToSlotOffsets_indexFromSlotOffsets :: SlotOffsets -> Property
prop_indexToSlotOffsets_indexFromSlotOffsets (SlotOffsets offsets) =
  indexToSlotOffsets (indexFromSlotOffsets offsets) === offsets

prop_filledSlots_isFilledSlot :: SlotOffsets -> Property
prop_filledSlots_isFilledSlot (SlotOffsets offsets) = conjoin
    [ isFilledSlot idx slot === (slot `elem` filledSlots idx)
    | slot <- slots ]
  where
    slots :: [RelativeSlot]
    slots | totalSlots == 0 = []
          | otherwise       = map coerce [0..indexSlots idx-1]
    totalSlots = indexSlots idx
    idx = indexFromSlotOffsets offsets

prop_writeIndex_loadIndex :: Index -> Property
prop_writeIndex_loadIndex index =
    monadicIO $ run $ runS prop
  where
    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      writeIndex hasFS epoch index
      (index', mbJunk) <- loadIndex hasFS epoch
      return $ index === index' .&&. isNothing mbJunk

    runS :: (HasFS IO Mock.Handle -> IO Property) -> IO Property
    runS m = do
        r <- tryFS (Sim.runSimFS EH.exceptions Mock.empty m)
        case r of
          Left  e      -> fail (prettyFsError e)
          Right (p, _) -> return p

prop_writeSlotOffsets_loadIndex_indexToSlotOffsets :: SlotOffsets -> Property
prop_writeSlotOffsets_loadIndex_indexToSlotOffsets (SlotOffsets offsets) =
    monadicIO $ run $ runS prop
  where
    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      writeSlotOffsets hasFS epoch offsets
      (index, mbJunk) <- loadIndex hasFS epoch
      return $ indexToSlotOffsets index === offsets .&&. isNothing mbJunk

    runS :: (HasFS IO Mock.Handle -> IO Property) -> IO Property
    runS m = do
        r <- tryFS (Sim.runSimFS EH.exceptions Mock.empty m)
        case r of
          Left  e      -> fail (prettyFsError e)
          Right (p, _) -> return p


{------------------------------------------------------------------------------
  reconstructSlotOffsets
------------------------------------------------------------------------------}

test_reconstructSlotOffsets :: Assertion
test_reconstructSlotOffsets = reconstructSlotOffsets input @?= output
  where
    input  = [(0, (10, 0)), (10, (10, 1)), (20, (5, 2)), (25, (10, 3))]
    output = NE.fromList [35, 25, 20, 10, 0]


test_reconstructSlotOffsets_empty_slots :: Assertion
test_reconstructSlotOffsets_empty_slots = reconstructSlotOffsets input @?= output
  where
    input  = [(0, (10, 2)), (10, (10, 4))]
    output = NE.fromList [20, 10, 10, 0, 0, 0]



{------------------------------------------------------------------------------
  cborEpochFileParser
------------------------------------------------------------------------------}

test_cborEpochFileParser :: Assertion
test_cborEpochFileParser = fmap fst $ Sim.runSimFS err Mock.empty $ \hasFS -> do
    let HasFS{..} = hasFS

    withFile hasFS fp IO.AppendMode $ \h -> do
      forM_ blocks $ \block ->
        hPut h (S.serialiseIncremental block)
      void $ hPut h "trailingjunk"

    (offsetsAndSizesAndBlocks', mbErr) <-
      runEpochFileParser (cborEpochFileParser' hasFS) fp

    offsetsAndSizesAndBlocks' @?= offsetsAndSizesAndBlocks
    assertBool "Expected an error" (isJust mbErr)
  where
    blocks :: [ByteString]
    blocks = map (snd . snd) offsetsAndSizesAndBlocks

    offsetsAndSizesAndBlocks :: [(SlotOffset, (Word, ByteString))]
    offsetsAndSizesAndBlocks =
      [ (0, (4, "foo"))
        -- Note that a "foo" is encoded as "Cfoo", hence the size of 4
      , (4, (4, "bar"))
      , (8, (4, "baz"))
      ]

    fp = ["test"]

    err = EH.exceptions
