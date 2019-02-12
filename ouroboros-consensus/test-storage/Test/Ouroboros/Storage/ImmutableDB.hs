{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
module Test.Ouroboros.Storage.ImmutableDB (tests) where

import           Control.Monad (void)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Class.MonadSTM (MonadSTM)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Function (on)
import           Data.List (groupBy, nubBy, sortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Word (Word64)

import qualified System.IO as IO

import           Test.Ouroboros.Storage.ImmutableDB.Sim (demoScript)
import qualified Test.Ouroboros.Storage.ImmutableDB.TestBlock as TestBlock
import qualified Test.Ouroboros.Storage.ImmutableDB.StateMachine as StateMachine
import qualified Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CumulEpochSizes
import           Test.Ouroboros.Storage.Util
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, pick, run)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..))
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (SimFS)
import qualified Ouroboros.Storage.FS.Sim.STM as Sim
import           Ouroboros.Storage.ImmutableDB
import           Ouroboros.Storage.ImmutableDB.Index
import           Ouroboros.Storage.Util (decodeIndexEntryAt)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  The list of all tests
------------------------------------------------------------------------------}

tests :: HasCallStack => TestTree
tests = testGroup "ImmutableDB"
    [ testCase     "What you store is what you get" test_appendAndGet
    , testProperty "append/get roundtrip" prop_appendAndGetRoundtrip
    , testProperty "Appending and getting to/from different slots" prop_appendAndGet
    , testCase     "AppendToSlotInThePastError equivalence" test_AppendToSlotInThePastErrorEquivalence
    , testCase     "ReadFutureSlotError equivalence" test_ReadFutureSlotErrorEquivalence
    , testCase     "SlotGreaterThanEpochSizeError equivalence" test_SlotGreaterThanEpochSizeErrorEquivalence
      -- demoScript
    , testCase     "demoScript equivalence" test_demoSimEquivalence
    , testCase     "Check index file layout" test_index_layout
    , testCase     "Starting a new epoch pads the previous epoch's index" test_startNewEpochPadsTheIndexFile
    , testCase     "Opening a DB with a missing epoch size gives an error" test_openDBMissingEpochSizeErrorEquivalence
    , testCase     "openDB with an empty index file" test_openDBEmptyIndexFileEquivalence
    , testCase     "Reopen the database" test_reopenDBEquivalence
    , testCase     "closeDB is idempotent" test_closeDBIdempotentEquivalence
    , testCase     "appendBinaryBlob after closeDB throws a ClosedDBError" test_closeDBAppendBinaryBlobEquivalence
    , testGroup "Iterators"
      [ testCase     "Basic test case of an iterator" test_iterator_basics
      , testProperty "Random iterators" prop_iterator
      , testProperty "Slotoffsets/Index roundtrip " prop_indexToSlotOffsets_indexFromSlotOffsets
      , testProperty "isFilledSlot iff in filledSlots" prop_filledSlots_isFilledSlot
      , testProperty "writeIndex/loadIndex roundtrip" prop_writeIndex_loadIndex
      , testProperty "writeSlotOffsets/loadIndex/indexToSlotOffsets roundtrip" prop_writeSlotOffsets_loadIndex_indexToSlotOffsets
      ]
    , TestBlock.tests
    , StateMachine.tests
    , CumulEpochSizes.tests
    ]

-- Shorthand
withTestDB :: (HasCallStack, MonadSTM m, MonadMask m)
           => HasFS m
           -> ErrorHandling ImmutableDBError m
           -> Map Epoch EpochSize
           -> (ImmutableDB m -> m a)
           -> m a
withTestDB hasFS err epochSizes f =
    withDB (openDB hasFS err ["test"] 0 epochSizes NoValidation) (\db _ -> f db)

withSimTestDB :: Map Epoch EpochSize
              -> (ImmutableDB (SimFS IO) -> SimFS IO a)
              -> SimFS IO a
withSimTestDB = withTestDB
                  (Sim.simHasFS     EH.exceptions)
                  (Sim.liftErrSimFS EH.exceptions)

{------------------------------------------------------------------------------
  Builder
------------------------------------------------------------------------------}

-- For the custom 'Show' and 'Arbitrary' instances
--
-- A builder of a non-empty bytestring.
newtype Builder = Builder { getBuilder :: BS.Builder }

instance Show Builder where
    show = show . BS.toLazyByteString . getBuilder

instance Arbitrary Builder where
  arbitrary = do
    str <- (getASCIIString <$> arbitrary) `suchThat` (not . null)
    return $ Builder $ BS.byteString $ C8.pack str
  shrink (Builder b) =
    [ Builder b'
    | let s = ASCIIString $ LC8.unpack $ BS.toLazyByteString b
    , s' <- getASCIIString <$> shrink s
    , not (null s')
    , let b' = BS.byteString $ C8.pack s' ]

builderToBS :: Builder -> ByteString
builderToBS = BL.toStrict . BS.toLazyByteString . getBuilder

{------------------------------------------------------------------------------
  Appending a blob and getting a blob
------------------------------------------------------------------------------}

-- TODO the StateMachine tests will supersede these tests

data AppendAndGet = AppendAndGet
  { _append :: RelativeSlot
  , _get    :: EpochSlot
  }

instance Show AppendAndGet where
  show (AppendAndGet (RelativeSlot aSlot)
                     (EpochSlot gEpoch (RelativeSlot gSlot))) =
    "Append: " <> show (1 :: Epoch, aSlot) <> ", Get: " <> show (gEpoch, gSlot)

instance Arbitrary AppendAndGet where
  arbitrary = do
    _append <- elements [0..12]
    _get    <- EpochSlot <$> elements [0..1] <*> elements [0..12]
    return $ AppendAndGet _append _get

-- Instead of testing all combinations of getting/appending from/to a
-- present/missing slot from a past/current/future epoch, generate these cases
prop_appendAndGet :: AppendAndGet -> Property
prop_appendAndGet (AppendAndGet append get) = label labelToApply $
    monadicIO $ do
      r <- run $ tryImmDB $ Sim.runSimFS script Mock.empty
      assertion r
  where
    script = withSimTestDB (M.singleton 0 epochSize) $ \db -> do
        appendBinaryBlob db 5 "first"
        _ <- startNewEpoch db epochSize
        appendBinaryBlob db 3 "second"
        appendBinaryBlob db append "haskell"
        getBinaryBlob db get

    epochSize = 10
    maxSlot = RelativeSlot (epochSize - 1)
    -- Where we appended the first blob: "first"
    firstAppend = EpochSlot 0 5
    -- Where we appended the second blob: "second"
    secondAppend = EpochSlot 1 3
    -- The next expected append to the DB
    nextAppend = EpochSlot 1 4

    (labelToApply, assertion)
      | append < _relativeSlot nextAppend
      = ("append to a slot in the past",
         \case
            Left (UserError (AppendToSlotInThePastError {}) _) -> return ()
            r -> fail ("Expected AppendToSlotInThePastError, got: " ++ show r))
      | append > maxSlot
      = ("append past the last slot of the epoch",
         \case
            Left (UserError (SlotGreaterThanEpochSizeError {}) _) -> return ()
            r -> fail ("Expected SlotGreaterThanEpochSizeError, got: " ++ show r))
      | get > EpochSlot 1 append
      = ("get a blob from a slot in the future",
         \case
            Left (UserError (ReadFutureSlotError {}) _) -> return ()
            r -> fail ("Expected ReadFutureSlotError, got: " ++ show r))
      | _relativeSlot get > maxSlot
      = ("get past the last slot of the epoch",
         \case
            Left (UserError (SlotGreaterThanEpochSizeError {}) _) -> return ()
            r -> fail ("Expected SlotGreaterThanEpochSizeError, got: " ++ show r))
      | get == firstAppend
      = ("get the first blob from the first epoch",
         \case
            Right (Just b, _) | b == "first" -> return ()
            r -> fail ("Expected Just \"first\", got: " ++ show r))
      | get == secondAppend
      = ("get the second blob from the second epoch",
         \case
            Right (Just b, _) | b == "second" -> return ()
            r -> fail ("Expected Just \"second\", got: " ++ show r))
      | get == EpochSlot 1 append
      = ("get the append blob from the second epoch",
         \case
            Right (Just b, _) | b == "haskell" -> return ()
            r -> fail ("Expected Just \"haskell\", got: " ++ show r))
      | otherwise
      = ("get a missing slot from an epoch",
         \case
            Right (Nothing, _) -> return ()
            r -> fail ("Expected Nothing, got: " ++ show r))

test_appendAndGet :: Assertion
test_appendAndGet = withMockFS tryImmDB (expectDBResult ((@?= Just "haskell") . fst)) $ \hasFS err ->
    withTestDB hasFS err (M.singleton 0 10) $ \db -> do
      appendBinaryBlob db 0 "haskell"
      getBinaryBlob db (EpochSlot 0 0)

prop_appendAndGetRoundtrip :: Property
prop_appendAndGetRoundtrip = monadicIO $ do
    input <- pick arbitrary
    run $ apiEquivalenceDB (expectDBResult (@?= Just (builderToBS input))) $ \hasFS err ->
      withTestDB hasFS err (M.singleton 0 10) $ \db -> do
        appendBinaryBlob db 0 (getBuilder input)
        getBinaryBlob db (EpochSlot 0 0)

{------------------------------------------------------------------------------
  Equivalence tests between IOFS(E) and SimFS(E)
------------------------------------------------------------------------------}

test_demoSimEquivalence :: HasCallStack => Assertion
test_demoSimEquivalence = apiEquivalenceDB (expectDBResult (@?= blobs)) $ \hasFS err ->
    demoScript (openDB hasFS err ["test"])
  where
    blobs = map Just ["haskell", "nice", "cardano", "test"]

-- Trying to append to a slot \"in the past\" should be an error, both in Sim
-- and IO.
test_AppendToSlotInThePastErrorEquivalence :: HasCallStack => Assertion
test_AppendToSlotInThePastErrorEquivalence =
    apiEquivalenceDB (expectUserError isAppendToSlotInThePastError) $ \hasFS err ->
      withTestDB hasFS err (M.singleton 0 10) $ \db -> do
        appendBinaryBlob db 3 "test"
        appendBinaryBlob db 2 "haskell"
  where
    isAppendToSlotInThePastError AppendToSlotInThePastError {} = True
    isAppendToSlotInThePastError _                             = False

test_ReadFutureSlotErrorEquivalence :: HasCallStack => Assertion
test_ReadFutureSlotErrorEquivalence =
    apiEquivalenceDB (expectUserError isReadFutureSlotError) $ \hasFS err ->
      withTestDB hasFS err (M.singleton 0 10) $ \db -> do
        _ <- getBinaryBlob db (EpochSlot 0 0)
        return ()
  where
    isReadFutureSlotError ReadFutureSlotError {} = True
    isReadFutureSlotError _                      = False

test_SlotGreaterThanEpochSizeErrorEquivalence :: HasCallStack => Assertion
test_SlotGreaterThanEpochSizeErrorEquivalence =
    apiEquivalenceDB (expectUserError isSlotGreaterThanEpochSizeError) $ \hasFS err ->
      withTestDB hasFS err (M.singleton 0 10) $ \db ->
        appendBinaryBlob db 11 "test"
  where
    isSlotGreaterThanEpochSizeError SlotGreaterThanEpochSizeError {} = True
    isSlotGreaterThanEpochSizeError _                                = False

test_openDBMissingEpochSizeErrorEquivalence :: Assertion
test_openDBMissingEpochSizeErrorEquivalence =
    apiEquivalenceDB (expectUserError isMissingEpochSizeError) $ \hasFS err -> do
      withTestDB hasFS err (M.singleton 0 10) $ \db -> do
          appendBinaryBlob db 0 "test"
          _ <- startNewEpoch db 10
          appendBinaryBlob db 0 "haskell"
        -- The second withDB should fail.
      withTestDB hasFS err (M.singleton 1 10) $ \_ -> return ()
  where
    isMissingEpochSizeError MissingEpochSizeError {} = True
    isMissingEpochSizeError _                        = False

test_openDBEmptyIndexFileEquivalence :: Assertion
test_openDBEmptyIndexFileEquivalence =
    apiEquivalenceDB (expectUnexpectedError isInvalidFileError) $ \hasFS@HasFS{..} err -> do
      createDirectoryIfMissing True ["test"]
      -- Create an empty index file
      h1 <- hOpen ["test", "epoch-000.dat"] IO.WriteMode
      h2 <- hOpen ["test", "index-000.dat"] IO.WriteMode
      hClose h1
      hClose h2

      withTestDB hasFS err (M.singleton 0 5) $ \_db ->
          return ()
  where
    isInvalidFileError InvalidFileError {} = True
    isInvalidFileError _                   = False

test_reopenDBEquivalence :: Assertion
test_reopenDBEquivalence =
    apiEquivalenceDB (expectDBResult (@?= EpochSlot 0 6)) $ \hasFS err -> do
      withTestDB hasFS err (M.singleton 0 10) $ \db ->
        appendBinaryBlob db 5 "a"
      withTestDB hasFS err (M.singleton 0 10) $ \db ->
        getNextEpochSlot db

test_closeDBIdempotentEquivalence :: Assertion
test_closeDBIdempotentEquivalence =
    apiEquivalenceDB (expectDBResult (@?= ())) $ \hasFS err -> do
      db <- fst <$> openDB hasFS err ["test"] 0 (M.singleton 0 10) NoValidation
      closeDB db
      closeDB db

test_closeDBAppendBinaryBlobEquivalence :: Assertion
test_closeDBAppendBinaryBlobEquivalence =
    apiEquivalenceDB (expectUserError isClosedDBError) $ \hasFS err -> do
      db <- fst <$> openDB hasFS err ["test"] 0 (M.singleton 0 10) NoValidation
      closeDB db
      appendBinaryBlob db 0 "foo"
  where
    isClosedDBError ClosedDBError {} = True
    isClosedDBError _                = False

-- TODO Property test that reopens the DB at random places -> add to q-s-m?

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

-- TODO replace unit tests with property tests
--
-- For example, say we have written "a" to relative slot 0, "bravo" to
-- relative slot 1, and "haskell" to slot 4. We get the following index file:
--
-- > |o0|o1|o2|o3|o4|o5|
-- > | 0| 1| 6| 6| 6|13|
test_index_layout :: Assertion
test_index_layout = withMockFS tryImmDB assrt $ \hasFS err ->
    withTestDB hasFS err (M.singleton 0 10) $ \db -> do
       appendBinaryBlob db 0 "a"
       appendBinaryBlob db 1 "bravo"
       appendBinaryBlob db 4 "haskell"
  where
    assrt (Left _)        = assertFailure "Unexpected error"
    assrt (Right (_, fs)) =
      getIndexContents fs ["test", "index-000.dat"] @?= [0, 1, 6, 6, 6, 13]

test_startNewEpochPadsTheIndexFile :: Assertion
test_startNewEpochPadsTheIndexFile = withMockFS tryImmDB assrt $ \hasFS err ->
    withTestDB hasFS err (M.singleton 0 10) $ \db -> do
      appendBinaryBlob db 0 "a"
      appendBinaryBlob db 1 "bravo"
      appendBinaryBlob db 4 "haskell"
      _ <- startNewEpoch db 5 -- Now in epoch 1
      _ <- startNewEpoch db 10 -- Now in epoch 2
      appendBinaryBlob db 1 "c"
  where
    assrt (Left _)        = assertFailure "Unexpected error"
    assrt (Right (_, fs)) = do
      getIndexContents fs ["test", "index-000.dat"] @?= [0, 1, 6, 6, 6, 13, 13, 13, 13, 13, 13]
      getIndexContents fs ["test", "index-001.dat"] @?= [0, 0, 0, 0, 0, 0]
      getIndexContents fs ["test", "index-002.dat"] @?= [0, 0, 1]

{------------------------------------------------------------------------------
  Testing iterators
------------------------------------------------------------------------------}

-- TODO the StateMachine tests will supersede these tests

test_iterator_basics :: Assertion
test_iterator_basics = apiEquivalenceDB
      (expectDBResult (@?= ["a", "b", "c", "d", "e", "f", "g"])) $ \hasFS err ->
    withTestDB hasFS err (M.singleton 0 10) $ \db -> do
      appendBinaryBlob db 0 "a"
      appendBinaryBlob db 1 "b"
      appendBinaryBlob db 2 "c"
      appendBinaryBlob db 7 "d"
      _ <- startNewEpoch db 5 -- Now in epoch 1
      appendBinaryBlob db 0 "e"
      appendBinaryBlob db 2 "f"
      _ <- startNewEpoch db 5 -- Now in epoch 2
      appendBinaryBlob db 3 "g"
      _ <- startNewEpoch db 5 -- Now in epoch 3
      _ <- startNewEpoch db 5 -- Now in epoch 4
      appendBinaryBlob db 4 "not included"
      withIterator db (Just (EpochSlot 0 0)) (Just (EpochSlot 4 2)) iteratorToList

data DBAction
  = StartNewEpoch
  | AppendBinaryBlob RelativeSlot Builder
  deriving (Show)

newtype DBActions = DBActions [DBAction]
  deriving (Show)

-- Fixed size for each epoch: 10
fixedEpochSize :: EpochSize
fixedEpochSize = 10

instance Arbitrary DBAction where
  arbitrary = frequency
    [ (1, return StartNewEpoch)
    , (3, AppendBinaryBlob
          <$> (RelativeSlot <$> choose (0, fixedEpochSize - 1))
          <*> arbitrary)
    ]
  shrink (AppendBinaryBlob slot b) =
    [AppendBinaryBlob slot b' | b' <- shrink b]
  shrink _ = []

-- Makes sure that appends are monotonically increasing per epoch
mkMonotonicallyIncreasing :: [DBAction] -> [DBAction]
mkMonotonicallyIncreasing =
    concatMap (nubBy ((==) `on` getSlot) . sortOn getSlot) . groupBy byAction
  where
    byAction StartNewEpoch StartNewEpoch                 = True
    byAction (AppendBinaryBlob {}) (AppendBinaryBlob {}) = True
    byAction _ _                                         = False
    getSlot (AppendBinaryBlob slot _) = slot
    getSlot StartNewEpoch             = 0

instance Arbitrary DBActions where
  arbitrary = DBActions . mkMonotonicallyIncreasing <$> listOf arbitrary
  shrink (DBActions actions) =
    DBActions . mkMonotonicallyIncreasing <$> shrinkList shrink actions

executeDBAction :: MonadSTM m => ImmutableDB m -> DBAction -> m ()
executeDBAction db action = case action of
    StartNewEpoch              -> void $ startNewEpoch db fixedEpochSize
    AppendBinaryBlob slot blob -> appendBinaryBlob db slot (getBuilder blob)

executeDBActions :: MonadSTM m => ImmutableDB m -> DBActions -> m ()
executeDBActions db (DBActions actions) =
    mapM_ (executeDBAction db) actions

expectedIteratorContents :: DBActions -> [(EpochSlot, Builder)]
expectedIteratorContents (DBActions actions) = go 0 actions
  where
    go _     [] = []
    go epoch (StartNewEpoch:actions') = go (epoch + 1) actions'
    go epoch (AppendBinaryBlob slot blob:actions') =
      (EpochSlot epoch slot, blob) : go epoch actions'

contentsRange :: EpochSlot -> EpochSlot
              -> [(EpochSlot, Builder)] -> [(EpochSlot, Builder)]
contentsRange start end =
  takeWhile ((<= end) . fst) . dropWhile ((< start) . fst)

contentsLastAppended :: [(EpochSlot, Builder)] -> Maybe EpochSlot
contentsLastAppended [] = Nothing
contentsLastAppended xs = Just $ maximum $ map fst xs

data IteratorContentsAndRange = IteratorContentsAndRange
  { _dbActions :: DBActions
  , _start     :: EpochSlot
  , _end       :: EpochSlot
  } deriving (Show)

-- Generate an EpochSlot that is among the non-empty list given ones, but that
-- may also be outside outside the given range or exceed 'fixedEpochSize'
genSlot :: [EpochSlot] -> Gen EpochSlot
genSlot slots = frequency
  [ (1, EpochSlot <$> arbitrary <*> (RelativeSlot <$> arbitrary))
  , (2, elements slots)
  ]

instance Arbitrary IteratorContentsAndRange where
  arbitrary = do
    _dbActions <- arbitrary `suchThat`
                  \as -> not (null (expectedIteratorContents as))
    let slots = map fst $ expectedIteratorContents _dbActions
    (_start, _end) <- frequency
      [ -- start > end
        (1, ((,) <$> genSlot slots <*> genSlot slots)
            `suchThat` (\(start, end) -> start > end))
        -- start <= end
      , (4, ((,) <$> genSlot slots <*> genSlot slots)
            `suchThat` (\(start, end) -> start <= end))
      ]
    return IteratorContentsAndRange {..}
  shrink itcar@(IteratorContentsAndRange {..}) =
    [ itcar { _dbActions = _dbActions' }
    | _dbActions' <- shrink _dbActions ]

prop_iterator :: IteratorContentsAndRange -> Property
prop_iterator IteratorContentsAndRange {..} = monadicIO $ do
    let contents      = contentsRange _start _end (expectedIteratorContents _dbActions)
        expectedBlobs = map (builderToBS . snd) contents
        -- Default to 0, 0 when there are no blobs
        lastAppended  = fromMaybe (EpochSlot 0 0) $ contentsLastAppended contents
        script        = withSimTestDB (M.singleton 0 fixedEpochSize) $ \db -> do
                          executeDBActions db _dbActions
                          withIterator db (Just _start) (Just _end) iteratorToList
    r <- run $ tryImmDB $ Sim.runSimFS script Mock.empty
    case r of
      Left (UserError (SlotGreaterThanEpochSizeError {}) _)
        |  getRelativeSlot (_relativeSlot _start) >= fixedEpochSize
        || getRelativeSlot (_relativeSlot _end) >= fixedEpochSize
        -> return ()
      Left (UserError (InvalidIteratorRangeError {}) _)
        | _start > _end
        -> return ()
      Left (UserError (ReadFutureSlotError {}) _)
        | _start > lastAppended || _end > lastAppended
        -> return ()
      Left e -> fail $ prettyImmutableDBError e
      Right (actualBlobs, _) | actualBlobs /= expectedBlobs
        -> fail ("Expected: " <> show expectedBlobs <> " but got: " <>
                 show actualBlobs)
      Right _ -> return ()

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
    slots | totalSlots == 0 = []
          | otherwise       = map RelativeSlot [0..indexSlots idx-1]
    totalSlots = indexSlots idx
    idx = indexFromSlotOffsets offsets

prop_writeIndex_loadIndex :: Index -> Property
prop_writeIndex_loadIndex index =
    monadicIO $ run $ runS prop
  where
    dbFolder = []
    epoch = 0

    prop :: SimFS IO Property
    prop = do
      writeIndex hasFS dbFolder epoch index
      index' <- loadIndex hasFS dbFolder epoch
      return $ index === index'

    hasFS :: HasFS (SimFS IO)
    hasFS = Sim.simHasFS EH.exceptions

    runS :: SimFS IO Property -> IO Property
    runS m = do
        r <- tryFS (Sim.runSimFS m Mock.empty)
        case r of
          Left  e      -> fail (prettyFsError e)
          Right (p, _) -> return p

prop_writeSlotOffsets_loadIndex_indexToSlotOffsets :: SlotOffsets -> Property
prop_writeSlotOffsets_loadIndex_indexToSlotOffsets (SlotOffsets offsets) =
    monadicIO $ run $ runS prop
  where
    dbFolder = []
    epoch = 0

    prop :: SimFS IO Property
    prop = do
      writeSlotOffsets hasFS dbFolder epoch offsets
      index <- loadIndex hasFS dbFolder epoch
      return $ indexToSlotOffsets index === offsets

    hasFS :: HasFS (SimFS IO)
    hasFS = Sim.simHasFS EH.exceptions

    runS :: SimFS IO Property -> IO Property
    runS m = do
        r <- tryFS (Sim.runSimFS m Mock.empty)
        case r of
          Left  e      -> fail (prettyFsError e)
          Right (p, _) -> return p
