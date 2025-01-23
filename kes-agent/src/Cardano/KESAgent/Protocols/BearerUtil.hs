{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Protocols.BearerUtil
where

import Control.Concurrent.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.TChan
import Control.Exception (Exception (..))
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Word
import Foreign (Ptr, castPtr, plusPtr, poke)

import Cardano.Crypto.Libsodium.Memory (
  allocaBytes,
  copyMem,
  packByteStringCStringLen,
  unpackByteStringCStringLen,
 )
import Ouroboros.Network.RawBearer

data BearerConnectionClosed
  = BearerConnectionClosed
  deriving (Show)

instance Exception BearerConnectionClosed

withDuplexBearer ::
  forall m a.
  ( MonadST m
  , MonadSTM m
  , MonadThrow m
  , MonadAsync m
  ) =>
  RawBearer m ->
  (RawBearer m -> m a) ->
  m a
withDuplexBearer s action = do
  recvChan :: TChan m Word8 <- newTChanIO
  let receiver :: m BearerConnectionClosed
      receiver = do
        allocaBytes bufferSize $ \buf -> do
          let go = do
                bytesRead <- recv s buf bufferSize
                case bytesRead of
                  0 -> return BearerConnectionClosed
                  n -> go
          go
      s' =
        RawBearer
          { send = send s
          , recv = recv'
          }

      recv' buf numBytes = do
        forM_ [0 .. numBytes - 1] $ \n -> do
          b <- atomically $ readTChan recvChan
          stToIO . unsafeIOToST $
            poke (buf `plusPtr` n) b
        return numBytes
  let sender = action s'
  result <- race receiver sender
  case result of
    Left e -> throwIO e
    Right x -> return x
  where
    bufferSize = 1024
