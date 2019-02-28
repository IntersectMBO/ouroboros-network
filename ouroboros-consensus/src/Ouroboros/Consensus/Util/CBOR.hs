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
  ) where

import qualified Codec.CBOR.Read as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import           Control.Exception (assert, throwIO)
import           Control.Monad
import           Control.Monad.ST
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import           Data.IORef
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

-- | Read a file incrementally
--
-- NOTE: This uses a chunk size of roughly 32k. If we use this function to read
-- small things this might not be ideal.
--
-- NOTE: This currently expects the file to contain precisely one value. If we
-- wanted to read a file containing multiple values which are /not/ stored as
-- a proper CBOR list (for instance multiple concatenated blocks) we'd have
-- to generalize this function slightly.
readIncremental :: forall m h a. (Serialise a, MonadST m, MonadThrow m)
                => HasFS m h -> FsPath -> m (Either ReadIncrementalErr a)
readIncremental hasFS@HasFS{..} fp = withLiftST $ \liftST -> do
    withFile hasFS fp ReadMode $ \h ->
      go liftST h =<< liftST S.deserialiseIncremental
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
