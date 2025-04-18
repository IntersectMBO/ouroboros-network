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
  , BearerTrace (..)
  , makeSocketBearer
  , makeSocketBearer'
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

import           Data.ByteString.Lazy qualified as BL
import           Network.Socket (Socket)
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
      -> fd
      -- file descriptor
      -> Maybe (ReadBuffer m)
      -- Optional Readbuffer
      -> m (Bearer m)
  }

pureBearer :: Applicative m
           => (DiffTime -> fd -> Maybe (ReadBuffer m) ->    Bearer m)
           ->  DiffTime -> fd -> Maybe (ReadBuffer m) -> m (Bearer m)
pureBearer f = \sduTimeout rb fd -> pure (f sduTimeout rb fd)


makeSocketBearer :: MakeBearer IO Socket
makeSocketBearer = makeSocketBearer' 0

makeSocketBearer' :: DiffTime -> MakeBearer IO Socket
makeSocketBearer' pt = MakeBearer $ \sduTimeout fd rb ->
    return $ socketAsBearer size batch rb sduTimeout pt fd
  where
    size = SDUSize 12_288
    batch = 131_072

withReadBufferIO :: (Maybe (ReadBuffer IO) -> IO b)
                 -> IO b
withReadBufferIO f = allocaBytesAligned size 8 $ \ptr -> do
    v <- newTVarIO BL.empty
    f $ Just $ ReadBuffer v ptr size
  where
    -- Maximum amount of data read in one call.
    -- Corresponds to the default readbuffer size on Linux.
    -- We want it larger than 64Kbyte, but not too large since
    -- it is a memory overhead per mux bearer in an application.
    size = 131_072

makePipeChannelBearer :: MakeBearer IO PipeChannel
makePipeChannelBearer = MakeBearer $ pureBearer (\_ fd _ -> pipeAsBearer size fd)
  where
    size = SDUSize 32_768

makeQueueChannelBearer :: ( MonadSTM   m
                          , MonadMonotonicTime m
                          , MonadThrow m
                          )
                       => MakeBearer m (QueueChannel m)
makeQueueChannelBearer = MakeBearer $ pureBearer (\_ q _ -> queueChannelAsBearer size q)
  where
    size = SDUSize 1_280

#if defined(mingw32_HOST_OS)
makeNamedPipeBearer :: MakeBearer IO HANDLE
makeNamedPipeBearer = MakeBearer $ pureBearer (\_ fd _ -> namedPipeAsBearer size fd)
  where
    size = SDUSize 24_576
#endif
