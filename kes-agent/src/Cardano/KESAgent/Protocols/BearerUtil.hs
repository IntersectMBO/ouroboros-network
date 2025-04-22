{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Protocols.BearerUtil
where

import Control.Concurrent.Class.MonadSTM
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Word
import Foreign (peek, plusPtr, poke)

import Cardano.Crypto.Libsodium.Memory (allocaBytes)
import Ouroboros.Network.RawBearer

data BearerConnectionClosed
  = BearerConnectionClosed
  deriving (Show)

instance Exception BearerConnectionClosed

-- | Wrap a raw bearer connection such that client disconnects are detected in
-- a timely fashion. We do this by moving reads to a separate thread, and
-- buffering their results; this way, the @recv@ call happens before the
-- protocol expects to receive anything, which will block until the connection
-- is closed, or until any data is sent.
-- *Note that this violates mlocking guarantees for incoming data.* This means
-- that this wrapper should only be used on connections where mlocking is not
-- needed for incoming data, only outgoing.
-- In practice, we will only use this on the agent side of the service
-- protocol; this is fine, because the client never sends anything that needs
-- to be mlocked, and data from the server (== agent) to the client isn't
-- buffered.
-- The client side of the service protocol doesn't need this, because it is
-- already in a receiving state when the protocol is idle, so it will detect
-- disconnects fast.
-- The control protocol doesn't need this either, because interactions follow a
-- typical request-response patterns, and will, in practice, establish a new
-- connection for each request-response cycle.
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
  -- We forward data one byte at a time; this is not the most efficient way, but
  -- it seems to perform well enough.
  -- If needed, we can change this to use @TChan m ByteString@, though this will
  -- require us to do some tricky splicing and dicing to serve the right number
  -- of bytes on the reader end and remember the remainder of the current chunk.
  recvChan :: TChan m Word8 <- newTChanIO

  -- The receiver thread: this will read whatever it can as soon as it can,
  -- sending it into the @recvChan@.  until an EOF is detected, at which point
  -- a 'BearerConnectionClosed' is returned.
  let receiver :: m BearerConnectionClosed
      receiver = do
        allocaBytes bufferSize $ \buf -> do
          let go = do
                bytesRead <- recv s buf bufferSize
                case bytesRead of
                  0 ->
                    -- EOF
                    return BearerConnectionClosed
                  _ -> do
                    -- Data available - send into TChan
                    forM_ [0 .. bytesRead - 1] $ \n -> do
                      (stToIO . unsafeIOToST) (peek (buf `plusPtr` n))
                        >>= atomically . writeTChan recvChan
                    go
          go

  -- Wrapped receiver. This will read bytes from the @recvChan@, blocking until
  -- enough bytes are available.
  let recv' buf numBytes = do
        forM_ [0 .. numBytes - 1] $ \n -> do
          b <- atomically $ readTChan recvChan
          stToIO . unsafeIOToST $
            poke (buf `plusPtr` n) b
        return numBytes

      -- Wrapped bearer. @send@ is simply forwarded to the underlying bearer,
      -- @recv@ uses our wrapped receive function.
      s' =
        RawBearer
          { send = send s
          , recv = recv'
          }

  -- Wrapped payload action.
  let action' = action s'

  -- We'll race the receiver against the payload action; if the receiver wins,
  -- that means it has detected an EOF ('BearerConnectionClosed'), so we throw
  -- that as an exception; otherwise, the payload action has completed
  -- normally, and we return its result.
  result <- race receiver action'
  case result of
    Left e -> throwIO e
    Right x -> return x
  where
    bufferSize = 1024
