{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Block used for the state machine tests
module Test.Ouroboros.Storage.ImmutableDB.TestBlock
  ( TestBlock(..)
  , testBlockRepeat
  , testBlockSize
  , testBlockToBuilder
  , binaryEpochFileParser
  , testBlockEpochFileParser'

  , FileCorruption(..)
  , corruptFile
  , Corruptions
  , generateCorruptions
  , shrinkCorruptions

  , tests
  ) where

import           Codec.Serialise (Serialise, decode, serialiseIncremental)
import           Control.Monad (forM, replicateM, void, when)
import           Control.Monad.Class.MonadThrow

import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Functor (($>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust, maybeToList)
import           Data.Word (Word64)

import           GHC.Generics (Generic)

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.StateMachine.Utils as QSM
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API (HasFS (..), hPut, withFile)
import           Ouroboros.Storage.FS.API.Types
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (runSimFS)
import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.ImmutableDB.Util (cborEpochFileParser,
                     readAll)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr)

import           Test.Util.Orphans.Arbitrary ()

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}


data TestBlock
  = TestBlock { tbSlot :: SlotNo }
  | TestEBB   { tbSlot :: SlotNo }
  deriving (Generic, Eq, Ord)

instance Show TestBlock where
    show (TestBlock slot) = "(TestBlock " <> show (unSlotNo slot) <> ")"
    show (TestEBB   slot) = "(TestEBB "   <> show (unSlotNo slot) <> ")"

-- | Only generates 'TestBlock's, no 'TestEBB's.
instance Arbitrary TestBlock where
  arbitrary = TestBlock <$> arbitrary
  shrink    = genericShrink

testBlockIsEBB :: TestBlock -> Bool
testBlockIsEBB TestBlock {} = False
testBlockIsEBB TestEBB   {} = True

-- | The binary representation of 'TestBlock' consists of repeating its 'SlotNo'
-- @n@ times, where @n = 'testBlockSize'@.
testBlockRepeat :: Word64
testBlockRepeat = 9

-- | The number of bytes the binary representation 'TestBlock' takes up.
testBlockSize :: Word64
testBlockSize = 8 + testBlockRepeat * 8

-- | The encoding is as follows:
--
-- First a tag, either the bytestring @"bbbbbbbb"@ for a 'TestBlock' or the
-- bytestring @"ebbebbeb"@ for a 'TestEBB'. Then the encoding of the @slot@ as
-- a 'Word', repeated 9 times. This means that:
--
-- * We know the size of each block (no delimiters needed)
-- * We know the slot of the block
-- * We know when the block is corrupted
--
-- __NOTE__: 'Test.Ouroboros.Storage.ImmutableDB.Model.simulateCorruptions'
-- depends on this encoding of 'TestBlock'.
instance Bin.Binary TestBlock where
    put testBlock =
        tag <> mconcat (replicate (fromIntegral testBlockRepeat) (Bin.put slot))
      where
        slot = unSlotNo (tbSlot testBlock)
        tag  = Bin.putByteString $ case testBlock of
          TestBlock {} -> "bbbbbbbb"
          TestEBB   {} -> "ebbebbeb"
    get = do
      tag <- Bin.getByteString 8
      let constr = case tag of
            "bbbbbbbb" -> TestBlock
            "ebbebbeb" -> TestEBB
            _          -> fail ("Unknown tag: " <> show tag)
      (w:ws) <- replicateM (fromIntegral testBlockRepeat) Bin.get
      when (any (/= w) ws) $
        fail "Corrupt TestBlock"
      return $ constr (SlotNo w)

-- | The list contains regular @TestBlock@s ordered by their slot.
-- The @Maybe TestBlock@ is an optional @TestEBB@.
data TestBlocks = TestBlocks (Maybe TestBlock) [TestBlock]
    deriving (Show)

instance Arbitrary TestBlocks where
    arbitrary = do
      regularBlocks <- orderedList
      mbEBB           <- frequency
        [ (2, return Nothing)
        , (1, Just . TestEBB <$> arbitrary)
        ]
      return $ TestBlocks mbEBB regularBlocks

    shrink (TestBlocks mbEBB testBlocks) =
      -- If there is an EBB, also return a list without it
      (if isJust mbEBB then (TestBlocks Nothing testBlocks :) else id)
      (TestBlocks mbEBB <$> shrinkList (const []) testBlocks)

testBlockToBuilder :: TestBlock -> BS.Builder
testBlockToBuilder = BS.lazyByteString . Bin.encode

prop_TestBlock_Binary :: SlotNo -> Property
prop_TestBlock_Binary slot =
    Bin.decode (Bin.encode testBlock) === testBlock .&&.
    Bin.decode (Bin.encode testEBB)   === testEBB
  where
    testBlock = TestBlock slot
    testEBB   = TestEBB   slot

instance Serialise TestBlock

{-------------------------------------------------------------------------------
  EpochFileParser
-------------------------------------------------------------------------------}


-- The EBB can only occur at offset 0
binaryEpochFileParser :: forall b m hash h. (MonadThrow m, Bin.Binary b)
                      => HasFS m h
                      -> (b -> Bool)  -- ^ Is the block an EBB?
                      -> (b -> hash)
                      -> EpochFileParser String hash m b
binaryEpochFileParser hasFS@HasFS{..} isEBB getHash = EpochFileParser $ \fsPath ->
    withFile hasFS fsPath ReadMode $ \eHnd -> do
      bytesInFile <- hGetSize eHnd
      parse bytesInFile 0 [] Nothing . BS.toLazyByteString <$>
        readAll hasFS eHnd
  where
    parse :: SlotOffset
          -> SlotOffset
          -> [(SlotOffset, b)]
          -> Maybe hash
          -> BL.ByteString
          -> ([(SlotOffset, b)], Maybe hash, Maybe String)
    parse bytesInFile offset parsed ebbHash bs
      | offset >= bytesInFile
      = (reverse parsed, ebbHash, Nothing)
      | otherwise
      = case Bin.decodeOrFail bs of
          Left (_, _, e) -> (reverse parsed, ebbHash, Just e)
          Right (remaining, bytesConsumed, b) ->
            let newOffset = offset + fromIntegral bytesConsumed
                newParsed = (offset, b) : parsed
                ebbHash'
                  | offset == 0, isEBB b = Just (getHash b)
                  | otherwise            = ebbHash
            in parse bytesInFile newOffset newParsed ebbHash' remaining

-- | We use 'TestBlock' as the @hash@.
testBlockEpochFileParser' :: MonadThrow m
                          => HasFS m h
                          -> EpochFileParser String TestBlock m (Word64, SlotNo)
testBlockEpochFileParser' hasFS = (\tb -> (testBlockSize, tbSlot tb)) <$>
    binaryEpochFileParser hasFS testBlockIsEBB id


prop_testBlockEpochFileParser :: TestBlocks -> Property
prop_testBlockEpochFileParser (TestBlocks mbEBB regularBlocks) = QCM.monadicIO $ do
    (offsetsAndBlocks, ebbHash, mbErr) <- QCM.run $ runSimIO $ \hasFS -> do
      writeBlocks hasFS
      readBlocks  hasFS
    QSM.liftProperty (mbErr   === Nothing)
    QSM.liftProperty (ebbHash === mbEBB)
    let (offsets', blocks') = unzip offsetsAndBlocks
        offsets = dropLast $ scanl (+) 0 $
          map (const testBlockSize) blocks
    QSM.liftProperty (blocks  ===  blocks')
    QSM.liftProperty (offsets === offsets')
  where
    dropLast xs = zipWith const xs (drop 1 xs)
    file = ["test"]

    blocks = maybeToList mbEBB <> regularBlocks

    writeBlocks :: HasFS IO Mock.Handle -> IO ()
    writeBlocks hasFS@HasFS{..} = do
      let bld = foldMap testBlockToBuilder blocks
      withFile hasFS file (AppendMode MustBeNew) $ \eHnd ->
        void $ hPut hasFS eHnd bld

    readBlocks :: HasFS IO Mock.Handle
               -> IO ([(SlotOffset, TestBlock)], Maybe TestBlock, Maybe String)
    readBlocks hasFS = runEpochFileParser
      (binaryEpochFileParser hasFS testBlockIsEBB id)
      file

    runSimIO :: (HasFS IO Mock.Handle -> IO a) -> IO a
    runSimIO m = fst <$> runSimFS EH.exceptions Mock.empty m

{-------------------------------------------------------------------------------
  CBOR EpochFileParser
-------------------------------------------------------------------------------}

-- [readIncrementalOffsetsEBB bug]
--
-- There was a bug in 'readIncrementalOffsetsEBB' (used in the implementation
-- of 'cborEpochFileParser') that caused it to prematurely stop the
-- incremental deserialisation of blocks from a file. Whenever the 'Done' case
-- (of 'IDecode') was reached with no leftover bytes, deserialisation was
-- stopped incorrectly. In this case, deserialisation only has to stop when
-- there are no more bytes in the file. The absence of leftover bytes in the
-- 'Done' constructor was mistakenly interpreted to represent this. In
-- reality, it means that no more bytes are left over in the /chunk/ that was
-- read from the file, not that no more bytes are left over in the /file/ that
-- is being read.
--
-- The test below tries to trigger this bug by using a chunk size equal to the
-- size of the first block so that it can be deserialised with exactly all the
-- bytes that have been read in one chunk (resulting in no leftover bytes).

prop_testBlockCborEpochFileParser :: TestBlocks -> Property
prop_testBlockCborEpochFileParser (TestBlocks mbEBB regularBlocks) = QCM.monadicIO $ do
    (offsetsAndSizesAndBlocks, ebbHash, mbErr) <- QCM.run $ runSimIO $ \hasFS -> do
      writeBlocks hasFS
      readBlocks  hasFS
    QSM.liftProperty (mbErr   === Nothing)
    QSM.liftProperty (ebbHash === mbEBB)
    let (offsets', sizesAndBlocks) = unzip offsetsAndSizesAndBlocks
        blocks' = map snd sizesAndBlocks
    QSM.liftProperty (blocks  === blocks')
    QSM.liftProperty (offsets === offsets')
  where
    dropLast xs = zipWith const xs (drop 1 xs)
    file = ["test"]

    blocks = maybeToList mbEBB <> regularBlocks
    blockSizes = map blockSize blocks
    offsets = dropLast $ scanl (+) 0 blockSizes
    chunkSize = case blockSizes of
      []               -> 32
      firstBlockSize:_ -> fromIntegral firstBlockSize

    writeBlocks :: HasFS IO Mock.Handle -> IO ()
    writeBlocks hasFS@HasFS{..} = do
      let bld = foldMap serialiseIncremental blocks
      withFile hasFS file (AppendMode MustBeNew) $ \eHnd ->
        void $ hPut hasFS eHnd bld

    readBlocks :: HasFS IO Mock.Handle
               -> IO ( [(SlotOffset, (Word64, TestBlock))]
                     , Maybe TestBlock
                     , Maybe ReadIncrementalErr)
    readBlocks hasFS = runEpochFileParser
      (cborEpochFileParser chunkSize hasFS decode (const extractEBB))
      file

    extractEBB :: TestBlock -> Maybe TestBlock
    extractEBB b | testBlockIsEBB b = Just b
                 | otherwise        = Nothing

    blockSize :: TestBlock -> SlotOffset
    blockSize = fromIntegral . BL.length . BS.toLazyByteString . serialiseIncremental

    runSimIO :: (HasFS IO Mock.Handle -> IO a) -> IO a
    runSimIO m = fst <$> runSimFS EH.exceptions Mock.empty m

{-------------------------------------------------------------------------------
  Corruption
-------------------------------------------------------------------------------}


data FileCorruption
  = DeleteFile
  | DropLastBytes Word64
    -- ^ Drop the last @n@ bytes of a file.
  deriving (Show, Eq)

-- | Returns 'True' when something was actually corrupted. For example, when
-- drop the last bytes of an empty file, we don't actually corrupt it.
corruptFile :: MonadThrow m => HasFS m h -> FileCorruption -> FsPath -> m Bool
corruptFile hasFS@HasFS{..} fc file = case fc of
    DeleteFile              -> removeFile file $> True
    DropLastBytes n         -> withFile hasFS file (AppendMode AllowExisting) $ \hnd -> do
      fileSize <- hGetSize hnd
      let newFileSize = if n >= fileSize then 0 else fileSize - n
      hTruncate hnd newFileSize
      return $ fileSize /= newFileSize

instance Arbitrary FileCorruption where
  arbitrary = frequency
    [ (1, return DeleteFile)
    , (1, DropLastBytes . getSmall . getPositive <$> arbitrary)
    ]
  shrink DeleteFile         = []
  shrink (DropLastBytes n)  =
    DropLastBytes . getSmall . getPositive <$> shrink (Positive (Small n))

-- | Multiple corruptions
type Corruptions = NonEmpty (FileCorruption, FsPath)

-- | The same file will not occur twice.
generateCorruptions :: NonEmpty FsPath -> Gen Corruptions
generateCorruptions allFiles = sized $ \n -> do
    subl  <- sublistOf (NE.toList allFiles) `suchThat` (not . null)
    k     <- choose (1, 1 `max` n)
    let files = NE.fromList $ take k subl
    forM files $ \file -> (, file) <$> arbitrary

shrinkCorruptions :: Corruptions -> [Corruptions]
shrinkCorruptions =
  fmap (NE.fromList . getNonEmpty) . shrink . NonEmpty . NE.toList


{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}


tests :: TestTree
tests = testGroup "TestBlock"
    [ testProperty "TestBlock Binary roundtrip"   prop_TestBlock_Binary
    , testProperty "testBlockEpochFileParser"     prop_testBlockEpochFileParser
    , testProperty "testBlockCborEpochFileParser" prop_testBlockCborEpochFileParser
    ]
