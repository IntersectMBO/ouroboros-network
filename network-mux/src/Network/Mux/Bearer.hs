{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}

module Network.Mux.Bearer
  ( Bearer (..)
  , MakeBearer (..)
  , makeSocketBearer
  , makePipeChannelBearer
  , makeQueueChannelBearer
#if defined(mingw32_HOST_OS)
  , makeNamedPipeBearer
#endif
  , withReadBufferIO
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer (Tracer)

import           Data.ByteString.Lazy qualified as BL
import           Network.Socket (getSocketOption, SocketOption (..), Socket)
#if defined(mingw32_HOST_OS)
import           System.Win32 (HANDLE)
#endif
import           Foreign.Marshal.Alloc

import           Network.Mux.Bearer.Pipe
import           Network.Mux.Bearer.Queues
import           Network.Mux.Bearer.Socket
import           Network.Mux.Trace
import           Network.Mux.Types hiding (sduSize)
#if defined(mingw32_HOST_OS)
import           Network.Mux.Bearer.NamedPipe
#endif

newtype MakeBearer m fd = MakeBearer {
    getBearer
      :: DiffTime
      -- timeout for reading an SDU segment, if negative no
      -- timeout is applied.
      -> Tracer m Trace
      -- tracer
      -> fd
      -- file descriptor
      -> Maybe (ReadBuffer m)
      -- Optional Readbuffer
      -> m (Bearer m)
  }

pureBearer :: Applicative m
           => (DiffTime -> Tracer m Trace -> fd -> Maybe (ReadBuffer m) ->   Bearer m)
           ->  DiffTime -> Tracer m Trace -> fd -> Maybe (ReadBuffer m) -> m (Bearer m)
pureBearer f = \sduTimeout rb tr fd -> pure (f sduTimeout rb tr fd)


makeSocketBearer :: MakeBearer IO Socket
makeSocketBearer = MakeBearer $ (\sduTimeout tr fd rb -> do
    batch <- getSocketOption fd SendBuffer
    return $ socketAsBearer size batch rb sduTimeout tr fd)
  where
    size = SDUSize 12_288

withReadBufferIO :: (Maybe (ReadBuffer IO) -> IO b)
                 -> IO b
withReadBufferIO f = allocaBytesAligned size 8 $ \ptr -> do
    v <- atomically $ newTVar BL.empty
    f $ Just $ ReadBuffer v ptr size
  where
    size = 131_072

makePipeChannelBearer :: MakeBearer IO PipeChannel
makePipeChannelBearer = MakeBearer $ pureBearer (\_ tr fd _ -> pipeAsBearer size tr fd)
  where
    size = SDUSize 32_768

makeQueueChannelBearer :: ( MonadSTM   m
                          , MonadMonotonicTime m
                          , MonadThrow m
                          )
                       => MakeBearer m (QueueChannel m)
makeQueueChannelBearer = MakeBearer $ pureBearer (\_ tr q _-> queueChannelAsBearer size tr q)
  where
    size = SDUSize 1_280

#if defined(mingw32_HOST_OS)
makeNamedPipeBearer :: MakeBearer IO HANDLE
MakeBearer $ pureBearer (\_ _ -> namedPipeAsBearer size)
  where
    size = SDUSize 24_576
#endif
