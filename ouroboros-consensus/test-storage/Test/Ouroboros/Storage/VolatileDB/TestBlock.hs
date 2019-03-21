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
import qualified Data.Map.Strict as M
import           Data.Serialize
import           Data.Word (Word64)
import qualified System.IO as IO
import           Test.QuickCheck

import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Storage.FS.API (HasFS (..), withFile)
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB (Parser (..), Slot (..),
                     VolatileDBError (..))
import qualified Ouroboros.Storage.VolatileDB as Volatile
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)

type BlockId = Word

type TestBlock = (Word, Int)

toBinary :: BlockId -> BL.ByteString
toBinary = Binary.encode . toBlock

fromBinary :: BS.ByteString -> Either String BlockId
fromBinary bs = do
    block <- decode bs
    case block of
        (bid, 1 :: Int) -> Right bid
        _               -> Left "wrong payload"

toSlot :: BlockId -> Slot
toSlot = Slot

toBlock :: BlockId -> TestBlock
toBlock bid = (bid, 1)

fromBlock :: TestBlock -> BlockId
fromBlock (bid, 1) = bid
fromBlock _        = error "wrong payload"

binarySize :: Int
binarySize = 16

myParser :: (Monad m, MonadThrow m)
         => HasFS m h
         -> ErrorHandling (VolatileDBError BlockId) m
         -> Volatile.Parser m BlockId
myParser hasFs err = Volatile.Parser {
    Volatile.parse = parseImpl hasFs err
    }

parseImpl :: forall m h. (Monad m, MonadThrow m)
          => HasFS m h
          -> ErrorHandling (VolatileDBError BlockId) m
          -> FsPath
          -> m (Word64, M.Map Volatile.SlotOffset (Volatile.BlockSize, BlockId))
parseImpl hasFS@HasFS{..} err path =
    withFile hasFS path IO.ReadMode $ \hndl -> do
        let go :: M.Map Word64 (Word64, BlockId)
               -> Word64
               -> Int
               -> [BlockId]
               -> m (Word64, M.Map Word64 (Word64, BlockId))
            go mp n trials bids = do
                bs <- hGet hndl binarySize
                if BS.length bs == 0 then return (n, mp)
                else case fromBinary bs of
                    Left str ->
                        EH.throwError err $ VParserError $
                            Volatile.DecodeFailed (n,mp) str (fromIntegral $ BS.length bs)
                    Right bid ->
                        if elem bid bids
                        then EH.throwError err $ Volatile.VParserError $ Volatile.DuplicatedSlot $ M.singleton bid (path, path)
                        else let mp' = M.insert n (fromIntegral binarySize, bid) mp
                            in go mp' (n + fromIntegral binarySize) (trials + 1) (bid : bids)
        go M.empty 0 0 []


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
