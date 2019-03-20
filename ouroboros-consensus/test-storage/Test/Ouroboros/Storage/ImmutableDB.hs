{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
module Test.Ouroboros.Storage.ImmutableDB (tests) where

import qualified Codec.Serialise as S
import           Control.Monad (forM_, void)
import           Control.Monad.Class.MonadSTM (MonadSTM)
import           Control.Monad.Class.MonadThrow (MonadCatch)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
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

fixedGetEpochSize :: Monad m => EpochNo -> m EpochSize
fixedGetEpochSize _ = return fixedEpochSize

type Hash = TestBlock

-- Shorthand
openTestDB :: (HasCallStack, MonadSTM m, MonadCatch m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> m (ImmutableDB Hash m)
openTestDB hasFS err =
    openDB S.decode S.encode hasFS err fixedGetEpochSize ValidateMostRecentEpoch parser
  where
    parser = TestBlock.testBlockEpochFileParser' hasFS

-- Shorthand
withTestDB :: (HasCallStack, MonadSTM m, MonadCatch m)
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
    apiEquivalenceImmDB (expectImmDBResult (@?= TipGenesis)) $ \hasFS@HasFS{..} err -> do
      -- Create an empty index file
      h1 <- hOpen ["epoch-000.dat"] IO.WriteMode
      h2 <- hOpen ["index-000.dat"] IO.WriteMode
      hClose h1
      hClose h2

      withTestDB hasFS err getTip

test_reopenDBEquivalence :: Assertion
test_reopenDBEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= TipBlock 5)) $ \hasFS err -> do
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
test_startNewEpochPadsTheIndexFile = withMockFS tryImmDB assrt $ \hasFS err ->
    withTestDB hasFS err $ \db -> do
      appendBinaryBlob db 0 "a"
      appendBinaryBlob db 1 "bravo"
      appendBinaryBlob db 4 "haskell"
      -- Skip epoch 1, now in epoch 2
      appendBinaryBlob db 21 "c"
  where
    assrt (Left _)        = assertFailure "Unexpected error"
    assrt (Right (_, fs)) = do
      getIndexContents fs ["index-000.dat"] @?= [0, 0, 1, 6, 6, 6, 13, 13, 13, 13, 13, 13]
      getIndexContents fs ["index-001.dat"] @?= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
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

instance Arbitrary (Index String) where
  arbitrary = indexFromSlotOffsets
    <$> (getSlotOffsets <$> arbitrary)
    <*> frequency [ (3, return Nothing)
                  , (1, Just <$> elements ["a", "b", "c"])]

  shrink index =
    [ indexFromSlotOffsets (getSlotOffsets offsets') (getEBBHash index)
    | offsets' <- shrink (SlotOffsets (indexToSlotOffsets index)) ] <>
    [ indexFromSlotOffsets (indexToSlotOffsets index) ebbHash'
    | ebbHash' <- shrink (getEBBHash index) ]

prop_indexToSlotOffsets_indexFromSlotOffsets :: SlotOffsets -> Property
prop_indexToSlotOffsets_indexFromSlotOffsets (SlotOffsets offsets) =
  indexToSlotOffsets (indexFromSlotOffsets offsets Nothing) === offsets

prop_filledSlots_isFilledSlot :: SlotOffsets -> Property
prop_filledSlots_isFilledSlot (SlotOffsets offsets) = conjoin
    [ isFilledSlot idx slot === (slot `elem` filledSlots idx)
    | slot <- slots ]
  where
    slots :: [RelativeSlot]
    slots | totalSlots == 0 = []
          | otherwise       = map coerce [0..indexSlots idx-1]
    totalSlots = indexSlots idx
    idx = indexFromSlotOffsets offsets Nothing

prop_writeIndex_loadIndex :: Index String -> Property
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

    runS :: (HasFS IO Mock.Handle -> IO Property) -> IO Property
    runS m = do
        r <- tryFS (Sim.runSimFS EH.exceptions Mock.empty m)
        case r of
          Left  e      -> fail (prettyFsError e)
          Right (p, _) -> return p

prop_writeSlotOffsets_loadIndex_indexToSlotOffsets :: Maybe String
                                                   -> SlotOffsets
                                                   -> Property
prop_writeSlotOffsets_loadIndex_indexToSlotOffsets ebbHash (SlotOffsets offsets) =
    monadicIO $ run $ runS prop
  where
    epoch = 0

    prop :: HasFS IO h -> IO Property
    prop hasFS = do
      writeSlotOffsets S.encode hasFS epoch offsets ebbHash
      let epochSize = fromIntegral (NE.length offsets - 1)
      index :: Index String <- loadIndex S.decode hasFS EH.exceptions epoch epochSize
      return $ indexToSlotOffsets index === offsets

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
test_cborEpochFileParser = forM_ ["junk", ""] $ \junk -> runFS $ \hasFS -> do
    -- Test once with junk at the end and once without
    let HasFS{..} = hasFS

    withFile hasFS fp IO.AppendMode $ \h -> do
      forM_ blocks $ \block ->
        hPut h (S.serialiseIncremental block)
      void $ hPut h (BS.string8 junk)

    (offsetsAndSizesAndBlocks', ebbHash, mbErr) <-
      runEpochFileParser (cborEpochFileParser' hasFS S.decode getEBBHash) fp

    ebbHash @?= Just "ebb"
    offsetsAndSizesAndBlocks' @?= offsetsAndSizesAndBlocks

    if null junk
      then assertBool "Unexpected error"  (isNothing mbErr)
      else assertBool "Expected an error" (isJust    mbErr)
  where
    blocks :: [ByteString]
    blocks = map (snd . snd) offsetsAndSizesAndBlocks

    offsetsAndSizesAndBlocks :: [(SlotOffset, (Word, ByteString))]
    offsetsAndSizesAndBlocks =
      [ (0, (4, "ebb"))
        -- Note that "ebb" is encoded as "Cebb", hence the size of 4
      , (4, (4, "bar"))
      , (8, (4, "baz"))
      ]

    fp = ["test"]

    getEBBHash :: ByteString -> Maybe String
    getEBBHash "ebb" = Just "ebb"
    getEBBHash _     = Nothing

    err = EH.exceptions

    runFS :: (HasFS IO Mock.Handle -> IO a) -> IO a
    runFS = fmap fst . Sim.runSimFS err Mock.empty
