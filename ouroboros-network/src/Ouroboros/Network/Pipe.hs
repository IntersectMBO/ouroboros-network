{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}

{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Network.Pipe (
    pipeDuplex
  , demo
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.ST (stToIO)
import           Data.Text (Text, unpack)
import           System.IO (Handle, hIsEOF, hFlush)
import           System.Process (createPipe)

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import           Ouroboros.Network.ChainSyncExamples
import           Ouroboros.Network.Protocol.ChainSync.Codec.Cbor
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Serialise

import           Protocol.Channel
import           Protocol.Codec
import           Protocol.Core
import           Protocol.Driver

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Write as CBOR (toBuilder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)

-------------------------------------------

-- | A demonstration that we can run the simple chain consumer protocol
-- over a pipe with full message serialisation, framing etc.
--
demo :: forall block .
        (Chain.HasHeader block, Serialise block, Eq block )
     => Chain block -> [ChainUpdate block] -> IO Bool
demo chain0 updates = do

    -- Create two pipes (each one is unidirectional) to connect up the
    -- producer and consumer ends of the protocol
    (hndRead1, hndWrite1) <- createPipe
    (hndRead2, hndWrite2) <- createPipe

    -- Initialise the producer and consumer state to be the same
    producerVar <- newTVarIO (CPS.initChainProducerState chain0)
    consumerVar <- newTVarIO chain0

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain
        checkTip = atomically $ do
          chain <- readTVar consumerVar
          return (Chain.headPoint chain == target)

        -- This 'Client' value controls the consumer, instructing it when to
        -- stop consuming: when the chain in 'consumerVar' matches (at the
        -- tip) the 'expectedChain'.
        consumerClient :: Client block IO ()
        consumerClient = Client
          { rollforward = \_ -> checkTip >>= \b -> case b of
              True -> pure $ Left ()
              False -> pure $ Right consumerClient
          , rollbackward = \_ _ -> checkTip >>= \b -> case b of
              True -> pure $ Left ()
              False -> pure $ Right consumerClient
          , points = \_ -> pure consumerClient
          }
    
        producerChannel = pipeDuplex hndRead2 hndWrite1
        consumerChannel = pipeDuplex hndRead1 hndWrite2

        producerPeer :: Peer ChainSyncProtocol (ChainSyncMessage block (Point block))
                             (Awaiting StIdle) (Finished StDone)
                             IO ()
        producerPeer = chainSyncServerPeer (chainSyncServerExample () producerVar)

        consumerPeer :: Peer ChainSyncProtocol (ChainSyncMessage block (Point block))
                             (Yielding StIdle) (Finished StDone)
                             IO ()
        consumerPeer = chainSyncClientPeer (chainSyncClientExample consumerVar consumerClient)

        codec :: Codec IO Text CBOR.Encoding BS.ByteString (ChainSyncMessage block (Point block)) 'StIdle
        codec = hoistCodec stToIO codecChainSync

        throwOnUnexpected :: String -> Result Text t -> IO t
        throwOnUnexpected str (Unexpected txt) = error $ str ++ " " ++ unpack txt
        throwOnUnexpected _   (Normal t) = pure t

    -- Fork the producer and consumer
    withAsync (throwOnUnexpected "producer" =<< useCodecWithDuplex producerChannel codec producerPeer) $ \producer ->
      withAsync (throwOnUnexpected "consumer" =<< useCodecWithDuplex consumerChannel codec consumerPeer) $ \consumer -> do
        _ <- link producer
        _ <- link consumer
        -- Apply updates to the producer's chain and let them sync
        -- FIXME why fork?????
        _ <- forkIO $ sequence_
               [ do threadDelay 1000 -- just to provide interest
                    atomically $ do
                      p <- readTVar producerVar
                      let Just p' = CPS.applyChainUpdate update p
                      writeTVar producerVar p'
               | update <- updates ]

        _ <- wait consumer
        _ <- wait producer

        -- Problem: if it doesn't sync, it times out. That was the case before,
        -- though.
        return True

pipeDuplex
  :: Handle -- ^ Read
  -> Handle -- ^ Write
  -> Duplex IO IO CBOR.Encoding BS.ByteString
pipeDuplex hndRead hndWrite = uniformDuplex send recv
  where
    send = \encoding -> do
      BS.hPutBuilder hndWrite (CBOR.toBuilder encoding)
      hFlush hndWrite
    recv = hIsEOF hndRead >>= \eof ->
      if eof
      then pure Nothing
      else fmap Just (BS.hGetSome hndRead LBS.smallChunkSize)
