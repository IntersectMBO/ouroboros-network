{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Block used for tests
module Test.Ouroboros.Storage.VolatileDB.TestBlock
  ( module TestBlock
  , BlockId
  , Predecessor
  , TestBlock (..)
  , TestHeader (..)
  , FileCorruption (..)
  , Corruptions
  , corruptionFiles
  , generateCorruptions
  , corruptFile
  , createFile
  , getOrigin
  , mkBlockInfo
  )
where

import           Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word64)
import           Test.QuickCheck

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Point (WithOrigin, at, origin)

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.FS.API (HasFS (..), hPut, withFile)
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.VolatileDB
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)

import           Test.Ouroboros.Storage.TestBlock as TestBlock hiding
                     (Corruptions, FileCorruption(..), corruptFile,
                     generateCorruptions)

type BlockId = TestHeaderHash

type Predecessor = WithOrigin BlockId

getOrigin :: ChainHash b -> WithOrigin (HeaderHash b)
getOrigin GenesisHash            = origin
getOrigin (BlockHash headerHash) = at headerHash

mkBlockInfo :: TestBlock -> BlockInfo (BlockId)
mkBlockInfo tb = BlockInfo {
      bbid          = thHash
    , bslot         = thSlotNo
    , bpreBid       = getOrigin thPrevHash
    , bisEBB        = thIsEBB
    , bheaderOffset = testBlockHeaderOffset
    , bheaderSize   = testBlockHeaderSize tb
    }
  where
    TestHeader{..} = testHeader tb

{------------------------------------------------------------------------------
  Corruption
------------------------------------------------------------------------------}

data FileCorruption
    = DeleteFile
    | DropLastBytes Word64
    | AppendBytes Int
    deriving (Show, Eq)

instance Arbitrary FileCorruption where
    arbitrary = frequency
      [ (1, return DeleteFile)
      , (1, DropLastBytes <$> choose (1,5)) -- should be less than size of block
      , (1, AppendBytes . getSmall . getPositive <$> arbitrary)
      ]

-- | Multiple corruptions.
type Corruptions = NonEmpty (FileCorruption, FsPath)

-- | Brings a list of all corrupted files.
corruptionFiles :: Corruptions -> [FsPath]
corruptionFiles = map snd . NE.toList

-- | The same file will not occur twice.
generateCorruptions :: [FsPath] -> Gen Corruptions
generateCorruptions allFiles = sized $ \n -> do
    subl  <- sublistOf allFiles `suchThat` (not . null)
    k     <- choose (1, 1 `max` n)
    let files = NE.fromList $ take k subl
    forM files $ \file -> (, file) <$> arbitrary

corruptFile :: MonadThrow m => HasFS m h -> FileCorruption -> FsPath -> m Bool
corruptFile hasFS@HasFS{..} corr file = case corr of
    DeleteFile -> removeFile file >> return True
    DropLastBytes n ->
      withFile hasFS file (AppendMode AllowExisting) $ \hnd -> do
        fileSize <- hGetSize hnd
        let newFileSize = if n >= fileSize then 0 else fileSize - n
        hTruncate hnd newFileSize
        return $ fileSize /= newFileSize
    AppendBytes n ->
      withFile hasFS file (AppendMode AllowExisting) $ \hnd -> do
        fileSize <- hGetSize hnd
        let newFileSize = fileSize + (fromIntegral n)
        _ <- hPut hasFS hnd (BB.byteString $ BS.replicate n 0)
        return $ fileSize /= newFileSize

createFile :: IOLike m
           => Internal.VolatileDBEnv m blockId
           -> m ()
createFile env =
    -- This will soon be replaced by @withState@.
    modifyState env $ \hasFS st -> do
      let nextFd = Internal._nextNewFileId st
      let path = Internal.filePath nextFd
      withFile hasFS path (AppendMode MustBeNew) $ \_hndl -> do
          return ()
      return (st, ())
