{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
module Test.Ouroboros.Storage.ImmutableDB (tests) where

import qualified Codec.Serialise as S
import           Control.Tracer (nullTracer)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (maybeToList)
import           Data.Word (Word64)

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

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..))
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS
import           Ouroboros.Storage.FS.Sim.MockFS (HandleMock, MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import qualified Ouroboros.Storage.FS.Sim.STM as Sim
import           Ouroboros.Storage.ImmutableDB
import           Ouroboros.Storage.ImmutableDB.Index
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.SlotOffsets (SlotOffsets)
import qualified Ouroboros.Storage.ImmutableDB.SlotOffsets as SlotOffsets
import           Ouroboros.Storage.ImmutableDB.Util (tryImmDB)
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
      , testProperty "slotOffsetsFromList/slotOffsetsToList roundtrip" prop_slotOffsetsFromList_slotOffsetsToList
      ]
    , testCase     "reconstructSlotOffsets" test_reconstructSlotOffsets
    , testCase     "reconstructSlotOffsets empty slots" test_reconstructSlotOffsets_empty_slots
    , TestBlock.tests
    , StateMachine.tests
    , CumulEpochSizes.tests
    ]


fixedEpochSize :: EpochSize
fixedEpochSize = 10

type Hash = TestBlock

-- Shorthand
openTestDB :: (HasCallStack, IOLike m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> m (ImmutableDB Hash m)
openTestDB hasFS err =
    openDB S.decode S.encode hasFS err (fixedSizeEpochInfo fixedEpochSize) ValidateMostRecentEpoch parser nullTracer
  where
    parser = TestBlock.testBlockEpochFileParser' hasFS

-- Shorthand
withTestDB :: (HasCallStack, IOLike m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> (ImmutableDB Hash m -> m a)
           -> m a
withTestDB hasFS err = withDB (openTestDB hasFS err)

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
    apiEquivalenceImmDB (expectImmDBResult (@?= TipGen)) $ \hasFS@HasFS{..} err -> do
      -- Create an empty index file
      h1 <- hOpen (mkFsPath ["epoch-000.dat"]) (WriteMode MustBeNew)
      h2 <- hOpen (mkFsPath ["index-000.dat"]) (WriteMode MustBeNew)
      hClose h1
      hClose h2

      withTestDB hasFS err getTip

test_reopenDBEquivalence :: Assertion
test_reopenDBEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= Tip (Block 5))) $ \hasFS err -> do
      withTestDB hasFS err $ \db ->
        appendBinaryBlob db 5 (testBlockToBuilder (TestBlock 5))
      withTestDB hasFS err $ \db ->
        getTip db

test_closeDBIdempotentEquivalence :: Assertion
test_closeDBIdempotentEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= ())) $ \hasFS err -> do
      db <- openTestDB hasFS err
      closeDB db
      closeDB db


test_closeDBAppendBinaryBlobEquivalence :: Assertion
test_closeDBAppendBinaryBlobEquivalence =
    apiEquivalenceImmDB (expectUserError isClosedDBError) $ \hasFS err -> do
      db <- openTestDB hasFS err
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

-- Note: cannot handle a hash after the offsets. Works fine if the hash is
-- Nothing.
byteStringToWord64s :: ByteString -> [Word64]
byteStringToWord64s bs = go 0
  where
    len = BS.length bs
    go i | i + 8 <= len = decodeIndexEntryAt i bs : go (i + 8)
         | otherwise    = []

test_startNewEpochPadsTheIndexFile :: Assertion
test_startNewEpochPadsTheIndexFile = withMockFS try assrt $ \hasFS err ->
    withTestDB hasFS err $ \db -> do
      appendBinaryBlob db 0 "a"
      appendBinaryBlob db 1 "bravo"
      appendBinaryBlob db 4 "haskell"
      -- Skip epoch 1, now in epoch 2
      appendBinaryBlob db 21 "c"
  where
    try = tryImmDB EH.exceptions EH.exceptions

    assrt (Left _)        = assertFailure "Unexpected error"
    assrt (Right (_, fs)) = do
      getIndexContents fs (mkFsPath ["index-000.dat"]) @?= [0, 0, 1, 6, 6, 6, 13, 13, 13, 13, 13, 13]
      getIndexContents fs (mkFsPath ["index-001.dat"]) @?= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      getIndexContents fs (mkFsPath ["index-002.dat"]) @?= []

{------------------------------------------------------------------------------
  Index
------------------------------------------------------------------------------}

-- Return True when the EBB has a size > 0 according to its corresponding
-- offset (2nd offset, starting from the end)
hasEBB :: SlotOffsets -> Bool
hasEBB offsets = go $ SlotOffsets.toList offsets
  where
    go []     = error $ "Offsets must be non-empty"
    go [0]    = False
    go [x]    = error $ "Offsets should end with 0, not with " <> show x
    go [0, 0] = False
    go [_, 0] = True
    go (_:xs) = go xs

instance Arbitrary SlotOffsets where
  arbitrary = do
    offsetsList <- (0 NE.:|) <$> orderedList
    emptyEBB <- arbitrary
    -- Flip a coin to determine whether we will generate an empty EBB slot (=
    -- no EBB) by making the size of the first slot 0 (by setting the second
    -- offset to 0). If the first element in the generated @orderedList@ is
    -- already 0, then the EBB will be empty regardless the value of
    -- @emptyEBB@.
    let allOffsets | emptyEBB  = 0 NE.<| offsetsList
                   | otherwise = offsetsList
    return $ SlotOffsets.fromNonEmptyList $ NE.reverse allOffsets

  shrink offsets =
    [ offsets'
    | offsetsList <- shrink $ SlotOffsets.toList offsets
    , offsets'    <- maybeToList $ SlotOffsets.fromNonEmptyList <$> NE.nonEmpty offsetsList ]

-- A non-empty string
newtype TestHash = TestHash String
    deriving (Show, Eq, S.Serialise)

instance Arbitrary TestHash where
  arbitrary = TestHash <$> elements ["a", "b", "c"]

instance Arbitrary (Index TestHash) where
  arbitrary = do
    offsets <- arbitrary
    ebbHash <- if hasEBB offsets
      then CurrentEBB <$> arbitrary
      else return NoCurrentEBB
    return $ indexFromSlotOffsets offsets ebbHash

  shrink index =
    [ indexFromSlotOffsets slotOffsets' ebbHash'
    | slotOffsets' <- shrink (indexToSlotOffsets index)
    , let ebbHash' | hasEBB slotOffsets' = CurrentEBB (TestHash "a")
                   | otherwise           = NoCurrentEBB
    ]

prop_indexToSlotOffsets_indexFromSlotOffsets :: SlotOffsets -> Property
prop_indexToSlotOffsets_indexFromSlotOffsets offsets =
  indexToSlotOffsets (indexFromSlotOffsets offsets NoCurrentEBB) === offsets

prop_filledSlots_isFilledSlot :: SlotOffsets -> Property
prop_filledSlots_isFilledSlot offsets = conjoin
    [ isFilledSlot idx slot === (slot `elem` filledSlots idx)
    | slot <- slots ]
  where
    slots :: [RelativeSlot]
    slots | totalSlots == 0 = []
          | otherwise       = map coerce [0..indexSlots idx-1]
    totalSlots = indexSlots idx
    idx = indexFromSlotOffsets offsets NoCurrentEBB

prop_writeIndex_loadIndex :: Index TestHash -> Property
prop_writeIndex_loadIndex index =
    monadicIO $ run $ runS prop
  where
    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      writeIndex S.encode hasFS epoch index
      let epochSize = indexSlots index
      index' <- loadIndex S.decode hasFS EH.exceptions epoch epochSize
      return $ index === index'

    runS :: (HasFS IO HandleMock -> IO Property) -> IO Property
    runS m = do
        r <- tryFS (Sim.runSimFS EH.exceptions Mock.empty m)
        case r of
          Left  e      -> fail (prettyFsError e)
          Right (p, _) -> return p

prop_writeSlotOffsets_loadIndex_indexToSlotOffsets :: SlotOffsets
                                                   -> Property
prop_writeSlotOffsets_loadIndex_indexToSlotOffsets offsets =
    monadicIO $ run $ runS prop
  where
    ebbHash | hasEBB offsets = CurrentEBB (TestHash "a")
            | otherwise      = NoCurrentEBB

    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      SlotOffsets.write S.encode hasFS epoch offsets ebbHash
      let epochSize = fromIntegral (length (SlotOffsets.toList offsets) - 1)
      index :: Index TestHash <- loadIndex S.decode hasFS EH.exceptions epoch epochSize
      return $ indexToSlotOffsets index === offsets

    runS :: (HasFS IO HandleMock -> IO Property) -> IO Property
    runS m = do
        r <- tryFS (Sim.runSimFS EH.exceptions Mock.empty m)
        case r of
          Left  e      -> fail (prettyFsError e)
          Right (p, _) -> return p

prop_slotOffsetsFromList_slotOffsetsToList :: SlotOffsets -> Property
prop_slotOffsetsFromList_slotOffsetsToList sos =
    SlotOffsets.fromList (SlotOffsets.toList sos) === sos

{------------------------------------------------------------------------------
  reconstructSlotOffsets
------------------------------------------------------------------------------}

test_reconstructSlotOffsets :: Assertion
test_reconstructSlotOffsets = SlotOffsets.reconstruct input @?= output
  where
    input  = [(0, (10, 0)), (10, (10, 1)), (20, (5, 2)), (25, (10, 3))]
    output = SlotOffsets.fromList [35, 25, 20, 10, 0]


test_reconstructSlotOffsets_empty_slots :: Assertion
test_reconstructSlotOffsets_empty_slots = SlotOffsets.reconstruct input @?= output
  where
    input  = [(0, (10, 2)), (10, (10, 4))]
    output = SlotOffsets.fromList [20, 10, 10, 0, 0, 0]
