{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE LambdaCase #-}

module Network.Mux.Bearer
  ( Bearer (..)
  , MakeBearer (..)
  , makeSocketBearer
  , makePipeChannelBearer
  , makeQueueChannelBearer
#if defined(mingw32_HOST_OS)
  , makeNamedPipeBearer
#endif
  -- , withReadBufferIO
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

data MakeBearer m fd buffered where
  MakeBearerBuffered :: {
    getBearerBuffered
      :: DiffTime
      -> Tracer m Trace
      -> fd
      -> BearerIngressBuffer
      -> m (Bearer m Buffered) } -> MakeBearer m fd Buffered
  MakeBearer :: {
    getBearer
      :: DiffTime
      -> Tracer m Trace
      -> fd
      -> m (Bearer m Unbuffered) } -> MakeBearer m fd Unbuffered

-- newtype MakeBearer m fd buffered = MakeBearer {
--     getBearer
--       :: DiffTime
--       -- ^ timeout for reading an SDU segment, if negative no
--       -- timeout is applied.
--       -> Tracer m Trace
--       -- ^ tracer
--       -> fd
--       -- ^ file descriptor
--       -> m (Bearer m buffered)
--   }

pureBearer :: Applicative m
           => (DiffTime -> Tracer m Trace -> fd ->   Bearer m Unbuffered)
           ->  DiffTime -> Tracer m Trace -> fd -> m (Bearer m Unbuffered)
pureBearer f =
  \sduTimeout tr fd -> pure (f sduTimeout tr fd)


makeSocketBearer :: SBearerBuffering s -> MakeBearer IO Socket s
makeSocketBearer =
  \case
    SBuffered ->
      MakeBearerBuffered $ \sduTimeout tr fd bb -> do
                               batch <- getSocketOption fd SendBuffer
                               return $ socketAsBearerBuffered size batch bb sduTimeout tr fd
    SUnbuffered ->
      MakeBearer $ \sduTimeout tr fd -> do
        batch <- getSocketOption fd SendBuffer
        return $ undefined --socketAsBearerBuffered size batch rb sduTimeout tr fd
  where
    size = SDUSize 12_288

-- withReadBufferIO :: (Maybe (ReadBuffer IO) -> IO b)
--                  -> IO b
-- withReadBufferIO f = allocaBytesAligned size 8 $ \ptr -> do
--     v <- atomically $ newTVar BL.empty
--     f $ Just $ ReadBuffer v ptr size
--   where
--     size = 131_072

makePipeChannelBearer :: MakeBearer IO PipeChannel Unbuffered
makePipeChannelBearer = undefined --MakeBearer $ pureBearer (\_ tr fd _ -> pipeAsBearer size tr fd)
  where
    size = SDUSize 32_768

makeQueueChannelBearer :: ( MonadSTM   m
                          , MonadMonotonicTime m
                          , MonadThrow m
                          )
                       => MakeBearer m (QueueChannel m) Unbuffered
makeQueueChannelBearer = undefined --MakeBearer $ pureBearer (\_ tr q _-> queueChannelAsBearer size tr q)
  where
    size = SDUSize 1_280

#if defined(mingw32_HOST_OS)
makeNamedPipeBearer :: MakeBearer IO HANDLE
MakeBearer $ pureBearer (\_ _ -> namedPipeAsBearer size)
  where
    size = SDUSize 24_576
#endif
