{-# LANGUAGE MultiParamTypeClasses #-}
module Cardano.KESAgent.DirectBearer
where

import Foreign.C.Types
import Foreign.Ptr
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Network.Socket
import qualified Network.Mux.Channel as Mux
import Control.Monad.Class.MonadST
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.Class.MonadMVar
import Data.Maybe (fromMaybe)
import Cardano.Crypto.MonadMLock
        ( MonadByteStringMemory (..)
        , packByteStringCStringLen
        , MonadUnmanagedMemory (..)
        )

data DirectBearer m =
  DirectBearer
    { send :: Ptr CChar -> CSize -> m CSize
    , recv :: Ptr CChar -> CSize -> m CSize
    }

class ToDirectBearer m s where
  toDirectBearer :: s -> DirectBearer m

unsafeChannelToDirectBearer :: (MonadST m, MonadMVar m, MonadByteStringMemory m, MonadUnmanagedMemory m)
                            => Mux.Channel m
                            -> m (DirectBearer m)
unsafeChannelToDirectBearer chan = do
  -- Hoo boy.
  --
  -- We need to maintain an internal buffer, because when we read from a
  -- channel, we don't get to say how much - we'll just get whatever data is
  -- available. So here's a buffer, in a generalized MVar (via 'MonadMVar').
  bufVar <- newMVar BS.empty

  return DirectBearer
    { -- Sending is easy, we can just pass a bytestring and be done with it.
      -- All the complexity in here is due to the 'MonadST' abstraction and
      -- working around the fact that while 'packCStringLen' is morally in ST,
      -- it actually lives in IO.
      send = \src size -> do
        bs <- packByteStringCStringLen (src, fromIntegral size)
        Mux.send chan $ LBS.fromStrict bs
        return $ fromIntegral size
    , -- For receiving, we need our buffer.
      recv = \dst size -> do
        modifyMVar bufVar $ \buf -> do
          if BS.length buf >= fromIntegral size then do
            withLiftST $ \liftST ->
              liftST . unsafeIOToST $ BS.useAsCStringLen buf $ \(src, _) ->
                copyMem dst src (fromIntegral size)
            return (BS.drop (fromIntegral size) buf, size)
          else do
            buf' <- LBS.toStrict . fromMaybe LBS.empty <$> Mux.recv chan
            sizeWritten <-
              useByteStringAsCStringLen buf $ \(src, sizeAvailable) -> do
                copyMem dst src (fromIntegral size)
                return sizeAvailable
            return (buf', fromIntegral sizeWritten)
            
    }
