{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Block used for tests
module Test.Ouroboros.Storage.VolatileDB.TestBlock where

import           Control.Monad (forM)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow (MonadThrow)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Serialize
import           Data.Word (Word64)
import qualified System.IO as IO
import           Test.QuickCheck

import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API (HasFS (..), hGetLenient, withFile)
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.VolatileDB (Parser (..), SlotNo (..))
import           Ouroboros.Storage.VolatileDB
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)

type BlockId = Word
type SerialMagic = Int
type Predecessor = Maybe BlockId
type BlockTestInfo = (BlockId, Predecessor)

type TestBlock = (BlockId, Predecessor, SerialMagic)

toBinary :: (BlockId, Maybe BlockId) -> BL.ByteString
toBinary = Binary.encode . toBlock

fromBinary :: BS.ByteString -> Either String BlockTestInfo
fromBinary bs = do
    block <- decode bs
    case block of
        (bid, predb, 1 :: SerialMagic) -> Right (bid, predb)
        _                              -> Left "wrong payload"

guessSlot :: BlockId -> SlotNo
guessSlot = SlotNo . fromIntegral

toBlock :: BlockTestInfo -> TestBlock
toBlock (b, pb) = (b, pb, 1)

fromBlock :: TestBlock -> BlockTestInfo
fromBlock (bid, pbid, 1) = (bid, pbid)
fromBlock _              = error "wrong payload"

binarySize :: Int
binarySize = 25

myParser :: (MonadThrow m)
         => HasFS m h
         -> Parser () m BlockId
myParser hasFs = Parser {
    parse = parseImpl hasFs
    }

parseImpl :: forall m h. (MonadThrow m)
          => HasFS m h
          -> FsPath
          -> m ([(SlotOffset, (BlockSize, BlockInfo BlockId))], Maybe ())
parseImpl hasFS@HasFS{..} path =
    withFile hasFS path IO.ReadMode $ \hndl -> do
        let go :: [(SlotOffset, (BlockSize, BlockInfo BlockId))]
               -> Word64
               -> m ([(SlotOffset, (BlockSize, BlockInfo BlockId))], Maybe ())
            go ls n = do
                bs <- BL.toStrict <$> hGetLenient hasFS hndl binarySize
                if BS.length bs == 0 then return (reverse ls, Nothing)
                else case fromBinary bs of
                    Left _ ->
                        return (reverse ls, Just ())
                    Right (bid, prebid) ->
                        let blockInfo =
                                BlockInfo {
                                      bbid    = bid
                                    , bslot   = guessSlot bid
                                    , bpreBid = prebid
                                }
                            ls' = (n, (fromIntegral binarySize, blockInfo)) : ls
                        in go ls' (n + fromIntegral binarySize)
        go [] 0


{-------------------------------------------------------------------------------
  Corruption
-------------------------------------------------------------------------------}


data FileCorruption
    = DeleteFile
    | DropLastBytes Word64
    | AppendBytes Int
    deriving (Show, Eq)

instance Arbitrary FileCorruption where
    arbitrary = frequency
        [ (1, return DeleteFile)
        , (1, DropLastBytes . getSmall . getPositive <$> arbitrary)
        , (1, AppendBytes . getSmall . getPositive <$> arbitrary)
        ]

-- | Multiple corruptions
type Corruptions = NonEmpty (FileCorruption, String)

-- | The same file will not occur twice.
generateCorruptions :: NonEmpty String -> Gen Corruptions
generateCorruptions allFiles = sized $ \n -> do
    subl  <- sublistOf (NE.toList allFiles) `suchThat` (not . null)
    k     <- choose (1, 1 `max` n)
    let files = NE.fromList $ take k subl
    forM files $ \file -> (, file) <$> arbitrary

corruptFile :: MonadThrow m => HasFS m h -> FileCorruption -> String -> m Bool
corruptFile hasFS@HasFS{..} corr file = case corr of
    DeleteFile -> removeFile [file] >> return True
    DropLastBytes n -> withFile hasFS [file] IO.AppendMode $ \hnd -> do
        fileSize <- hGetSize hnd
        let newFileSize = if n >= fileSize then 0 else fileSize - n
        hTruncate hnd newFileSize
        return $ fileSize /= newFileSize
    AppendBytes n -> withFile hasFS [file] IO.AppendMode $ \hnd -> do
        fileSize <- hGetSize hnd
        let newFileSize = fileSize + (fromIntegral n)
        _ <- hPut hnd (BB.byteString $ BS.replicate n 0)
        return $ fileSize /= newFileSize


createFileImpl :: (MonadSTM m, MonadThrow m)
               => HasFS m h
               -> Internal.VolatileDBEnv m blockId
               -> m ()
createFileImpl hasFS env = do
    SomePair _stHasFS st <- Internal.getInternalState env
    let nextFd = Internal._nextNewFileId st
    let path = Internal.filePath nextFd
    withFile hasFS [path] IO.AppendMode $ \_hndl -> do
        return ()
    return ()
