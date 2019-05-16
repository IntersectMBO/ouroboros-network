{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}

module Ouroboros.Consensus.Util.CBOR (
    -- * Incremental parsing in I/O
    IDecodeIO(..)
  , fromIDecode
  , deserialiseIncrementalIO
    -- * Higher-level incremental interface
  , Decoder(..)
  , initDecoderIO
    -- * HasFS interaction
  , ReadIncrementalErr(..)
  , readIncremental
  , readIncrementalOffsets
  , readIncrementalOffsetsEBB
  ) where

import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Read as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import           Control.Exception (assert, throwIO)
import           Control.Monad
import           Control.Monad.ST
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import           Data.Word (Word64)
import           System.IO (IOMode (..))

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types

{-------------------------------------------------------------------------------
  Incremental parsing in I/O
-------------------------------------------------------------------------------}

data IDecodeIO a =
    Partial (Maybe ByteString -> IO (IDecodeIO a))
  | Done !ByteString !CBOR.ByteOffset a
  | Fail !ByteString !CBOR.ByteOffset CBOR.DeserialiseFailure

fromIDecode :: CBOR.IDecode RealWorld a -> IDecodeIO a
fromIDecode (CBOR.Partial k)     = Partial $ fmap fromIDecode . stToIO . k
fromIDecode (CBOR.Done bs off x) = Done bs off x
fromIDecode (CBOR.Fail bs off e) = Fail bs off e

deserialiseIncrementalIO :: Serialise a => IO (IDecodeIO a)
deserialiseIncrementalIO = fromIDecode <$> stToIO S.deserialiseIncremental

{-------------------------------------------------------------------------------
  Higher-level incremental interface
-------------------------------------------------------------------------------}

data Decoder m = Decoder {
      -- | Decode next failure
      --
      -- May throw 'CBOR.DeserialiseFailure'
      decodeNext :: forall a. Serialise a => m a
    }

-- | Construct incremental decoder given a way to get chunks
--
-- Resulting decoder is not thread safe.
initDecoderIO :: IO ByteString -> IO (Decoder IO)
initDecoderIO getChunk = do
    leftover <- newIORef BS.empty
    let go :: forall a. Serialise a => IO a
        go = do
           i <- deserialiseIncrementalIO
           case i of
             Done bs _ a -> assert (BS.null bs) $ return a
             Fail _  _ e -> throwIO e
             Partial k   -> readIORef leftover >>= (k . Just >=> goWith)

        goWith :: forall a. IDecodeIO a -> IO a
        goWith (Partial k)   = getChunk' >>= (k >=> goWith)
        goWith (Done bs _ a) = writeIORef leftover bs >> return a
        goWith (Fail _  _ e) = throwIO e

    return $ Decoder go

  where
    getChunk' :: IO (Maybe ByteString)
    getChunk' = checkEmpty <$> getChunk

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

{-------------------------------------------------------------------------------
  HasFS interaction
-------------------------------------------------------------------------------}

data ReadIncrementalErr =
    -- | Could not deserialise the data
    ReadFailed S.DeserialiseFailure

    -- | Deserialisation was successful, but there was additional data
  | TrailingBytes ByteString
  deriving (Eq, Show)

-- | Read a file incrementally
--
-- NOTE: The 'MonadThrow' constraint is only needed for 'bracket'. This
-- function does not actually throw anything.
--
-- NOTE: This uses a chunk size of roughly 32k. If we use this function to read
-- small things this might not be ideal.
--
-- NOTE: This currently expects the file to contain precisely one value; see also
-- 'readIncrementalOffsets'.
readIncremental :: forall m h a. (MonadST m, MonadThrow m)
                => HasFS m h
                -> (forall s . CBOR.Decoder s a)
                -> FsPath
                -> m (Either ReadIncrementalErr a)
readIncremental hasFS@HasFS{..} decoder fp = withLiftST $ \liftST -> do
    withFile hasFS fp ReadMode $ \h ->
      go liftST h =<< liftST (CBOR.deserialiseIncremental decoder)
  where
    go :: (forall x. ST s x -> m x)
       -> h
       -> S.IDecode s a
       -> m (Either ReadIncrementalErr a)
    go liftST h (S.Partial k) = do
        bs   <- hGet h defaultChunkSize
        dec' <- liftST $ k (checkEmpty bs)
        go liftST h dec'
    go _ _ (S.Done leftover _ a) =
        return $ if BS.null leftover
                   then Right a
                   else Left $ TrailingBytes leftover
    go _ _ (S.Fail _ _ err) =
        return $ Left $ ReadFailed err

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

-- | Read multiple @a@s incrementally from a file.
--
-- Return the offset ('Word64') of the start of each @a@ and the size ('Word64')
-- of each @a@. When deserialising fails, return all already deserialised @a@s
-- and the error.
--
-- TODO parameterise over 'defaultChunkSize'
readIncrementalOffsets :: forall m h a. (MonadST m, MonadThrow m)
                       => HasFS m h
                       -> (forall s . CBOR.Decoder s a)
                       -> FsPath
                       -> m ([(Word64, (Word64, a))], Maybe ReadIncrementalErr)
                          -- ^ ((the offset of the start of @a@ in the file,
                          --     (the size of @a@ in bytes,
                          --      @a@ itself)),
                          --     error encountered during deserialisation)
readIncrementalOffsets hasFS@HasFS{..} decoder fp = withLiftST $ \liftST ->
    withFile hasFS fp ReadMode $ \h -> do
      fileSize <- hGetSize h
      if fileSize == 0
        -- If the file is empty, we will immediately get "end of input"
        then return ([], Nothing)
        else liftST (CBOR.deserialiseIncremental decoder) >>=
             go liftST h 0 [] Nothing fileSize
  where
    go :: (forall x. ST s x -> m x)
       -> h
       -> Word64                  -- ^ Offset
       -> [(Word64, (Word64, a))] -- ^ Already deserialised (reverse order)
       -> Maybe ByteString        -- ^ Unconsumed bytes from last time
       -> Word64                  -- ^ Total file size
       -> S.IDecode s a
       -> m ([(Word64, (Word64, a))], Maybe ReadIncrementalErr)
    go liftST h offset deserialised mbUnconsumed fileSize dec = case dec of
      S.Partial k -> do
        -- First use the unconsumed bytes from a previous read before read
        -- some more bytes from the file.
        bs   <- case mbUnconsumed of
          Just unconsumed -> return unconsumed
          Nothing         -> hGet h defaultChunkSize
        dec' <- liftST $ k (checkEmpty bs)
        go liftST h offset deserialised Nothing fileSize dec'

      S.Done leftover size a -> do
        let nextOffset    = offset + fromIntegral size
            deserialised' = (offset, (fromIntegral size, a)) : deserialised
        case checkEmpty leftover of
          Nothing
            | nextOffset == fileSize
              -- We're at the end of the file, so stop
            -> return (reverse deserialised', Nothing)
          -- Some more bytes, so try to read the next @a@.
          mbLeftover -> liftST (CBOR.deserialiseIncremental decoder) >>=
            go liftST h nextOffset deserialised' mbLeftover fileSize

      S.Fail _ _ err -> return (reverse deserialised, Just (ReadFailed err))

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

-- | Read multiple @a@s incrementally from a file.
--
-- Return the offset ('Word64') of the start of each @a@ and the size ('Word64')
-- of each @a@. When deserialising fails, return all already deserialised @a@s
-- and the error.
--
-- TODO remove this function once we have removed
-- 'Ouroboros.Storage.ImmutableDB.Util.cborEpochFileParser'', the ChainDB will
-- extract the EBB hash for us.
readIncrementalOffsetsEBB :: forall m hash h a. (MonadST m, MonadThrow m)
                          => Int -- ^ Chunk size when reading bytes
                          -> HasFS m h
                          -> (forall s . CBOR.Decoder s a)
                          -> (BSL.ByteString -> a -> Maybe hash)
                              -- ^ In case the given @a@ is an EBB, return its
                              -- @hash@. You also get the bytes from which it
                              -- was decoded.
                          -> FsPath
                          -> m ([(Word64, (Word64, a))],
                                Maybe hash,
                                Maybe ReadIncrementalErr)
                             -- ^ ((the offset of the start of @a@ in the file,
                             --     (the size of @a@ in bytes,
                             --      @a@ itself)),
                             --     the hash of the EBB, if present
                             --     error encountered during deserialisation)
readIncrementalOffsetsEBB chunkSize hasFS decoder getEBBHash fp = withLiftST $ \liftST ->
    withFile hasFS fp ReadMode $ \h -> do
      fileSize <- hGetSize h
      if fileSize == 0
        -- If the file is empty, we will immediately get "end of input"
        then return ([], Nothing, Nothing)
        else liftST (CBOR.deserialiseIncremental decoder) >>=
             go liftST h 0 [] Nothing Nothing [] fileSize
  where
    HasFS{..} = hasFS

    -- The incremental decoder is run in such a way that the bytes fed to it
    -- are retained and then used (in case of `Done`) to convert the
    -- `ByteSpan` annotation into a `ByteString` annotation.
    go :: (forall x. ST s x -> m x)
       -> h
       -> Word64                  -- ^ Offset
       -> [(Word64, (Word64, a))] -- ^ Already deserialised (reverse order)
       -> Maybe hash              -- ^ The hash of the EBB block
       -> Maybe ByteString        -- ^ Unconsumed bytes from last time
       -> [ByteString]            -- ^ Bytes fed to the decoder so far, reverse.
       -> Word64                  -- ^ Total file size
       -> S.IDecode s a
       -> m ([(Word64, (Word64, a))], Maybe hash, Maybe ReadIncrementalErr)
    go liftST h !offset !deserialised !mbEBBHash !mbUnconsumed !consumed fileSize dec = case dec of
      S.Partial k -> case mbUnconsumed of
        Just bs -> do
          dec' <- liftST $ k (Just bs)
          go liftST h offset deserialised mbEBBHash Nothing (bs : consumed) fileSize dec'
        Nothing -> do
          bs <- hGet h chunkSize
          -- Use `checkEmpty` to give `Nothing`, an indication of end of stream.
          -- NB: we don't do that for the other case `Just bs`, because lack of
          -- unconsumed does _not_ imply end of stream.
          dec' <- liftST $ k (checkEmpty bs)
          go liftST h offset deserialised mbEBBHash Nothing (bs : consumed) fileSize dec'

      S.Done leftover size a -> do
        let nextOffset    = offset + fromIntegral size
            deserialised' = (offset, (fromIntegral size, a)) : deserialised
            consumedBytes = BSL.take size (BSL.fromChunks (reverse consumed))
            -- The EBB can only occur at the start of the file
            mbEBBHash'    | offset == 0 = getEBBHash consumedBytes a
                          | otherwise   = mbEBBHash
        case checkEmpty leftover of
          Nothing
            | nextOffset == fileSize
              -- We're at the end of the file, so stop
            -> return (reverse deserialised', mbEBBHash', Nothing)
          -- Otherwise, there are some left-over bytes or we know there are
          -- still some more bytes in the file to read, so try to read the
          -- next @a@.
          mbLeftover -> do
            dec' <- liftST $ CBOR.deserialiseIncremental decoder
            go liftST h nextOffset deserialised' mbEBBHash' mbLeftover []
               fileSize dec'

      S.Fail _ _ err -> return
        (reverse deserialised, mbEBBHash, Just (ReadFailed err))

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs
