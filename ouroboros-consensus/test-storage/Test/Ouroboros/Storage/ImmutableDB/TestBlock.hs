{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
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

import           Control.Monad (forM, replicateM, void, when)
import           Control.Monad.Class.MonadThrow

import qualified Data.Binary as Bin
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Functor (($>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word64)

import           GHC.Generics (Generic)

import           System.IO (IOMode (..))

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.StateMachine.Utils as QSM
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.FS.API (HasFS (..), withFile)
import           Ouroboros.Storage.FS.API.Types (FsPath)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (runSimFS)
import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.ImmutableDB.Util (readAll)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH


{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}


newtype TestBlock = TestBlock { tbSlot :: Slot }
    deriving (Generic, Eq, Ord)

instance Show TestBlock where
    show = show . getSlot . tbSlot

instance Arbitrary TestBlock where
    arbitrary = TestBlock . Slot <$> arbitrary

-- | The binary representation of 'TestBlock' consists of repeating its 'Slot'
-- @n@ times, where @n = 'testBlockSize'@.
testBlockRepeat :: Word
testBlockRepeat = 10

-- | The number of bytes the binary representation 'TestBlock' takes up.
testBlockSize :: Word
testBlockSize = testBlockRepeat * 8

-- | The encoding of TestBlock @x@ is the encoding of @x@ as a 'Word',
-- repeated 10 times. This means that:
--
-- * We know the size of each block (no delimiters needed)
-- * We know the slot of the block
-- * We know when the block is corrupted
--
-- __NOTE__: 'Test.Ouroboros.Storage.ImmutableDB.Model.simulateCorruptions'
-- depends on this encoding of 'TestBlock'.
instance Bin.Binary TestBlock where
    put (TestBlock (Slot slot)) =
      mconcat $ replicate (fromIntegral testBlockRepeat) (Bin.put slot)
    get = do
      (w:ws) <- replicateM (fromIntegral testBlockRepeat) Bin.get
      when (any (/= w) ws) $
        fail "Corrupt TestBlock"
      return $ TestBlock (Slot w)

newtype TestBlocks = TestBlocks [TestBlock]
    deriving (Show)

instance Arbitrary TestBlocks where
    arbitrary = TestBlocks <$> orderedList
    shrink (TestBlocks testBlocks) =
      TestBlocks <$> shrinkList (const []) testBlocks

testBlockToBuilder :: TestBlock -> BS.Builder
testBlockToBuilder = BS.lazyByteString . Bin.encode


{-------------------------------------------------------------------------------
  EpochFileParser
-------------------------------------------------------------------------------}


binaryEpochFileParser :: forall b m h. (MonadThrow m, Bin.Binary b)
                      => HasFS m h
                      -> EpochFileParser String m b
binaryEpochFileParser hasFS@HasFS{..} = EpochFileParser $ \fsPath ->
    withFile hasFS fsPath ReadMode $ \eHnd -> do
      bytesInFile <- hGetSize eHnd
      parse (fromIntegral bytesInFile) 0 [] . BS.toLazyByteString <$>
        readAll hasFS eHnd
  where
    parse :: SlotOffset
          -> SlotOffset
          -> [(SlotOffset, b)]
          -> BL.ByteString
          -> ([(SlotOffset, b)], Maybe String)
    parse bytesInFile offset parsed bs
      | offset >= bytesInFile
      = (reverse parsed, Nothing)
      | otherwise
      = case Bin.decodeOrFail bs of
          Left (_, _, e) -> (reverse parsed, Just e)
          Right (remaining, bytesConsumed, b) ->
            let newOffset = offset + fromIntegral bytesConsumed
                newParsed = (offset, b) : parsed
            in parse bytesInFile newOffset newParsed remaining

testBlockEpochFileParser' :: MonadThrow m
                          => HasFS m h
                          -> EpochFileParser String m (Word, Slot)
testBlockEpochFileParser' hasFS = (\tb -> (testBlockSize, tbSlot tb)) <$>
    binaryEpochFileParser hasFS


prop_testBlockEpochFileParser :: TestBlocks -> Property
prop_testBlockEpochFileParser (TestBlocks blocks) = QCM.monadicIO $ do
    (offsetsAndBlocks, mbErr) <- QCM.run $ runSimIO $ \hasFS -> do
      writeBlocks hasFS
      readBlocks  hasFS
    QSM.liftProperty (mbErr === Nothing)
    let (offsets', blocks') = unzip offsetsAndBlocks
        offsets = dropLast $ scanl (+) 0 $
          map (const (fromIntegral testBlockSize)) blocks
    QSM.liftProperty (blocks  ===  blocks')
    QSM.liftProperty (offsets === offsets')
  where
    dropLast xs = zipWith const xs (drop 1 xs)
    file = ["test"]

    writeBlocks :: HasFS IO Mock.Handle -> IO ()
    writeBlocks hasFS@HasFS{..} = do
      let bld = foldMap testBlockToBuilder blocks
      withFile hasFS file AppendMode $ \eHnd -> void $ hPut eHnd bld

    readBlocks :: HasFS IO Mock.Handle
               -> IO ([(SlotOffset, TestBlock)], Maybe String)
    readBlocks hasFS = runEpochFileParser (binaryEpochFileParser hasFS) file

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
    DropLastBytes n         -> withFile hasFS file AppendMode $ \hnd -> do
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
    [ testProperty "testBlockEpochFileParser" prop_testBlockEpochFileParser
    ]
