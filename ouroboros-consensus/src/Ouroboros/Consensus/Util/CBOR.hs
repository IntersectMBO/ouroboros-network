{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  ) where

import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Read as CBOR
import           Control.Exception (assert, throwIO)
import           Control.Monad
import           Control.Monad.ST
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef
import           Data.Word (Word64)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Util.IOLike

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

deserialiseIncrementalIO :: (forall s. CBOR.Decoder s a) -> IO (IDecodeIO a)
deserialiseIncrementalIO = fmap fromIDecode . stToIO . CBOR.deserialiseIncremental

{-------------------------------------------------------------------------------
  Higher-level incremental interface
-------------------------------------------------------------------------------}

data Decoder m = Decoder {
      -- | Decode next failure
      --
      -- May throw 'CBOR.DeserialiseFailure'
      decodeNext :: forall a. (forall s. CBOR.Decoder s a) -> m a
    }

-- | Construct incremental decoder given a way to get chunks
--
-- Resulting decoder is not thread safe.
initDecoderIO :: IO ByteString -> IO (Decoder IO)
initDecoderIO getChunk = do
    leftover <- newIORef BS.empty
    let go :: forall a. (forall s. CBOR.Decoder s a) -> IO a
        go decoder = do
           i <- deserialiseIncrementalIO decoder
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
    ReadFailed CBOR.DeserialiseFailure

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
       -> Handle h
       -> CBOR.IDecode s a
       -> m (Either ReadIncrementalErr a)
    go liftST h (CBOR.Partial k) = do
        bs   <- hGetSome h (fromIntegral defaultChunkSize)
        dec' <- liftST $ k (checkEmpty bs)
        go liftST h dec'
    go _ _ (CBOR.Done leftover _ a) =
        return $ if BS.null leftover
                   then Right a
                   else Left $ TrailingBytes leftover
    go _ _ (CBOR.Fail _ _ err) =
        return $ Left $ ReadFailed err

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

-- | Read multiple @a@s incrementally from a file.
--
-- Return the offset ('Word64') of the start of each @a@ and the size ('Word64')
-- of each @a@. When deserialising fails, return all already deserialised @a@s
-- and the error.
readIncrementalOffsets :: forall m h a. (MonadST m, MonadThrow m)
                       => HasFS m h
                       -> (forall s . CBOR.Decoder s (LBS.ByteString -> a))
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
             go liftST h 0 [] Nothing [] fileSize
  where
    go :: (forall x. ST s x -> m x)
       -> Handle h
       -> Word64                  -- ^ Offset
       -> [(Word64, (Word64, a))] -- ^ Already deserialised (reverse order)
       -> Maybe ByteString        -- ^ Unconsumed bytes from last time
       -> [ByteString]            -- ^ Chunks pushed for this item (rev order)
       -> Word64                  -- ^ Total file size
       -> CBOR.IDecode s (LBS.ByteString -> a)
       -> m ([(Word64, (Word64, a))], Maybe ReadIncrementalErr)
    go liftST h offset deserialised mbUnconsumed bss fileSize dec = case dec of
      CBOR.Partial k -> do
        -- First use the unconsumed bytes from a previous read before read
        -- some more bytes from the file.
        bs   <- case mbUnconsumed of
          Just unconsumed -> return unconsumed
          Nothing         -> hGetSome h (fromIntegral defaultChunkSize)
        dec' <- liftST $ k (checkEmpty bs)
        go liftST h offset deserialised Nothing (bs:bss) fileSize dec'

      CBOR.Done leftover size a -> do
        let nextOffset    = offset + fromIntegral size
            -- We've been keeping track of the bytes pushed into the decoder
            -- for this item so far in bss. Now there's some trailing data to
            -- remove and we can get the whole bytes used for this item. We
            -- supply the bytes to the final decoded value. This is to support
            -- annotating values with their original input bytes.
            aBytes        = case bss of
                []      -> LBS.empty
                bs:bss' -> LBS.fromChunks (reverse (bs' : bss'))
                  where
                    bs' = BS.take (BS.length bs - BS.length leftover) bs
            -- The bang on the @a'@ here allows the used 'Decoder' to force
            -- its computation. For example, the decoder might decode a whole
            -- block and then (maybe through a use of 'fmap') just return its
            -- hash. If we don't force the value it returned here, we're just
            -- putting a thunk that references the whole block in the list
            -- instead of merely the hash.
            !a'            = a aBytes
            deserialised' = (offset, (fromIntegral size, a')) : deserialised
        case checkEmpty leftover of
          Nothing
            | nextOffset == fileSize
              -- We're at the end of the file, so stop
            -> return (reverse deserialised', Nothing)
          -- Some more bytes, so try to read the next @a@.
          mbLeftover -> liftST (CBOR.deserialiseIncremental decoder) >>=
            go liftST h nextOffset deserialised' mbLeftover [] fileSize

      CBOR.Fail _ _ err -> return (reverse deserialised, Just (ReadFailed err))

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs
