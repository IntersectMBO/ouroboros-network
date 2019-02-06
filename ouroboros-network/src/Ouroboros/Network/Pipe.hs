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
import           System.IO (Handle, hIsEOF, hFlush)

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import           Ouroboros.Network.Protocol.ChainSync.Codec.Cbor
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Serialise

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver

import qualified Protocol.Channel

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Write    as CBOR (toBuilder)
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

    -- Create channels connected through a pipe to connect client and producer
    -- sides of the protocol
    (producerChannel, consumerChannel) <- createPipeConnectedChannels

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

        producerPeer :: Peer (ChainSync block (Point block)) AsServer StIdle IO ()
        producerPeer = chainSyncServerPeer (chainSyncServerExample () producerVar)

        consumerPeer :: Peer (ChainSync block (Point block)) AsClient StIdle IO ()
        consumerPeer = chainSyncClientPeer (chainSyncClientExample consumerVar consumerClient)

        codec :: Codec (ChainSync block (Point block)) pk CBOR.DeserialiseFailure IO BS.ByteString
        codec = codecChainSync

    -- Fork the producer and consumer
    withAsync (runPeer codec producerChannel producerPeer) $ \producer ->
      withAsync (runPeer codec consumerChannel consumerPeer) $ \consumer -> do
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

        -- FIXME: if it doesn't sync, it times out. Is it possible to decide
        -- when the chain will certainly _not_ sync?
        return True

pipeDuplex
  :: Handle -- ^ Read
  -> Handle -- ^ Write
  -> Protocol.Channel.Duplex IO IO CBOR.Encoding BS.ByteString
pipeDuplex hndRead hndWrite = Protocol.Channel.uniformDuplex send recv
  where
    send = \encoding -> do
      BS.hPutBuilder hndWrite (CBOR.toBuilder encoding)
      hFlush hndWrite
    recv = hIsEOF hndRead >>= \eof ->
      if eof
      then pure Nothing
      else fmap Just (BS.hGetSome hndRead LBS.smallChunkSize)
