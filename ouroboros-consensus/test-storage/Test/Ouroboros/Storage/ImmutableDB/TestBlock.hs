{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- | Block used for the state machine tests
module Test.Ouroboros.Storage.ImmutableDB.TestBlock
  ( TestBlock(..)
  , testBlockRepeat
  , testBlockSize
  , testBlockToBuilder
  , testBlockEpochFileParser
  , testBlockEpochFileParser'

  , tests
  ) where

import           Control.Monad (void, when, replicateM)
import           Control.Monad.Catch (MonadMask)

import qualified Data.Binary as Bin
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL

import           GHC.Generics (Generic)

import           System.IO (IOMode(..))

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.StateMachine.Utils as QSM
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.FS.API (HasFS(..), withFile)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (SimFS, runSimFS, simHasFS)
import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.ImmutableDB.Util (readAll)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH


{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}


newtype TestBlock = TestBlock { tbRelativeSlot :: RelativeSlot }
    deriving (Generic, Eq, Ord)

instance Show TestBlock where
    show = show . getRelativeSlot . tbRelativeSlot

instance Arbitrary TestBlock where
    arbitrary = TestBlock . RelativeSlot <$> arbitrary

-- | The binary representation of 'TestBlock' consists of repeating its
-- 'RelativeSlot' @n@ times, where @n = 'testBlockSize'@.
testBlockRepeat :: Int
testBlockRepeat = 10

-- | The number of bytes the binary representation 'TestBlock' takes up.
testBlockSize :: Int
testBlockSize = testBlockRepeat * 8

-- | The encoding of TestBlock @x@ is the encoding of @x@ as a 'Word',
-- repeated 10 times. This means that:
--
-- * We know the size of each block (no delimiters needed)
-- * We know the relative slot of the block
-- * We know when the block is corrupted
instance Bin.Binary TestBlock where
    put (TestBlock (RelativeSlot relSlot)) =
      mconcat $ replicate testBlockRepeat (Bin.put relSlot)
    get = do
      (w:ws) <- replicateM testBlockRepeat Bin.get
      when (any (/= w) ws) $
        fail "Corrupt TestBlock"
      return $ TestBlock (RelativeSlot w)

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

testBlockEpochFileParser :: MonadMask m
                         => HasFS m
                         -> EpochFileParser String m TestBlock
testBlockEpochFileParser hasFS@HasFS{..} = EpochFileParser $ \fsPath ->
    withFile hasFS fsPath ReadMode $ \eHnd -> do
      bytesInFile <- hGetSize eHnd
      parse (fromIntegral bytesInFile) 0 [] . BS.toLazyByteString <$>
        readAll hasFS eHnd
  where
    parse :: SlotOffset
          -> SlotOffset
          -> [(SlotOffset, TestBlock)]
          -> BL.ByteString
          -> ([(SlotOffset, TestBlock)], Maybe String)
    parse bytesInFile offset parsed bs
      | offset >= bytesInFile
      = (reverse parsed, Nothing)
      | otherwise
      = case Bin.decodeOrFail bs of
          Left (_, _, e) -> (reverse parsed, Just e)
          Right (remaining, bytesConsumed, testBlock) ->
            let newOffset = offset + fromIntegral bytesConsumed
                newParsed = (offset, testBlock) : parsed
            in parse bytesInFile newOffset newParsed remaining

testBlockEpochFileParser' :: MonadMask m
                          => HasFS m
                          -> EpochFileParser String m (Int, RelativeSlot)
testBlockEpochFileParser' hasFS = (\tb -> (testBlockSize, tbRelativeSlot tb)) <$>
    testBlockEpochFileParser hasFS


prop_testBlockEpochFileParser :: TestBlocks -> Property
prop_testBlockEpochFileParser (TestBlocks blocks) = QCM.monadicIO $ do
    (offsetsAndBlocks, mbErr) <- QCM.run $ runSimIO $ do
      writeBlocks
      readBlocks
    QSM.liftProperty (mbErr === Nothing)
    let (offsets', blocks') = unzip offsetsAndBlocks
        offsets = dropLast $ scanl (+) 0 $
          map (const (fromIntegral testBlockSize)) blocks
    QSM.liftProperty (blocks  ===  blocks')
    QSM.liftProperty (offsets === offsets')
  where
    dropLast xs = zipWith const xs (drop 1 xs)
    file = ["test"]
    hasFS@HasFS{..} = simHasFS EH.exceptions
    writeBlocks = do
      let bld = foldMap testBlockToBuilder blocks
      withFile hasFS file AppendMode $ \eHnd -> void $ hPut eHnd bld
    readBlocks = runEpochFileParser (testBlockEpochFileParser hasFS) file

    runSimIO :: SimFS IO a -> IO a
    runSimIO m = fst <$> runSimFS m Mock.empty


{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}


tests :: TestTree
tests = testGroup "TestBlock"
    [ testProperty "testBlockEpochFileParser" prop_testBlockEpochFileParser
    ]
