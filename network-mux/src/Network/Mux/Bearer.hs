{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE RankNTypes             #-}
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
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer (Tracer)

import           Network.Socket (getSocketOption, SocketOption (..), Socket)
#if defined(mingw32_HOST_OS)
import           System.Win32 (HANDLE)
#endif

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
      :: forall buffering.
         DiffTime
      -- ^ timeout for reading an SDU segment, if negative no
      -- timeout is applied.
      -> Tracer m Trace
      -- ^ tracer
      -> fd
      -- ^ file descriptor
      -> SBearerBuffering buffering
      -- ^ bearer buffer
      -> m (Bearer m buffering)
  }

pureBearer :: Applicative m
           => (DiffTime -> Tracer m Trace -> fd -> SBearerBuffering s -> Bearer m Unbuffered)
           ->  DiffTime -> Tracer m Trace -> fd -> SBearerBuffering s -> m (Bearer m s)
pureBearer f =
  \sduTimeout tr fd sb ->
    case sb of
      SBuffered _buffer -> error "unimplemented"
      SUnbuffered -> pure (f sduTimeout tr fd sb)

makeSocketBearer :: MakeBearer IO Socket
makeSocketBearer = MakeBearer $ \sduTimeout tr fd singBuffer -> do
  batch <- getSocketOption fd SendBuffer
  case singBuffer of
    SBuffered buffer -> return $ socketAsBearerBuffered size batch buffer sduTimeout tr fd
    SUnbuffered -> return $ socketAsBearer size batch sduTimeout tr fd
  where
    size = SDUSize 12_288

makePipeChannelBearer :: MakeBearer IO PipeChannel
makePipeChannelBearer = MakeBearer $ pureBearer (\_ tr fd _sb -> pipeAsBearer size tr fd)
  where
    size = SDUSize 32_768

makeQueueChannelBearer :: ( MonadSTM   m
                          , MonadMonotonicTime m
                          , MonadThrow m
                          )
                       => MakeBearer m (QueueChannel m)
makeQueueChannelBearer = MakeBearer $ pureBearer (\_ tr q _sb -> queueChannelAsBearer size tr q)
  where
    size = SDUSize 1_280

#if defined(mingw32_HOST_OS)
makeNamedPipeBearer :: MakeBearer IO HANDLE
MakeBearer $ pureBearer (\_ _ -> namedPipeAsBearer size)
  where
    size = SDUSize 24_576
#endif
