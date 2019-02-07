{-# LANGUAGE RankNTypes #-}

module Ouroboros.Consensus.Util.CBOR (
    -- * Incremental parsing in I/O
    IDecodeIO(..)
  , fromIDecode
  , deserialiseIncrementalIO
    -- * Higher-level incremental interface
  , Decoder(..)
  , initDecoderIO
  ) where

import qualified Codec.CBOR.Read as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import           Control.Exception (assert, throwIO)
import           Control.Monad
import           Control.Monad.ST (RealWorld, stToIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.IORef

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
