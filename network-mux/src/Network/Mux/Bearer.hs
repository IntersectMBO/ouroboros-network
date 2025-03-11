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
  , makeSocketBearer'
  , makePipeChannelBearer
  , makeQueueChannelBearer
  , makeQueueChannelBearer'
#if defined(mingw32_HOST_OS)
  , makeNamedPipeBearer
#endif
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer (Tracer)

import           Network.Socket (Socket)
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
      :: DiffTime
      -- timeout for reading an SDU segment, if negative no
      -- timeout is applied.
      -> Tracer m Trace
      -- tracer
      -> fd
      -- file descriptor
      -> m (Bearer m)
  }


pureBearer :: Applicative m
           => (DiffTime -> Tracer m Trace -> fd ->    Bearer m)
           ->  DiffTime -> Tracer m Trace -> fd -> m (Bearer m)
pureBearer f = \sduTimeout tr fd -> pure (f sduTimeout tr fd)

makeSocketBearer :: MakeBearer IO Socket
makeSocketBearer = makeSocketBearer' False

makeSocketBearer' :: Bool -> MakeBearer IO Socket
makeSocketBearer' bypassEncode = MakeBearer $ pureBearer (socketAsBearer' bypassEncode size)
  where
    size = SDUSize 12_288

makePipeChannelBearer :: MakeBearer IO PipeChannel
makePipeChannelBearer = MakeBearer $ pureBearer (\_ -> pipeAsBearer size)
  where
    size = SDUSize 32_768

makeQueueChannelBearer :: ( MonadSTM   m
                          , MonadMonotonicTime m
                          , MonadThrow m
                          )
                       => MakeBearer m (QueueChannel m)
makeQueueChannelBearer = makeQueueChannelBearer' False

makeQueueChannelBearer' :: ( MonadSTM   m
                          , MonadMonotonicTime m
                          , MonadThrow m
                          )
                       => Bool
                       -> MakeBearer m (QueueChannel m)
makeQueueChannelBearer' bypassEncode = MakeBearer $ pureBearer (\_ -> queueChannelAsBearer' bypassEncode size)
  where
    size = SDUSize 1_280

#if defined(mingw32_HOST_OS)
makeNamedPipeBearer :: MakeBearer IO HANDLE
makeNamedPipeBearer = MakeBearer $ pureBearer (\_ -> namedPipeAsBearer size)
  where
    size = SDUSize 24_576
#endif
