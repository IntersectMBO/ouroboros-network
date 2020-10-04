{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Direct (
    direct
  ) where

import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Server

direct
  :: forall block query m a b.
     Monad m
  => LocalStateQueryClient block query m a
  -> LocalStateQueryServer block query m b
  -> m (a, b)
direct (LocalStateQueryClient mclient) (LocalStateQueryServer mserver) = do
    client <- mclient
    server <- mserver
    directIdle client server
  where
    directIdle
      :: ClientStIdle block query m a
      -> ServerStIdle block query m b
      -> m (a, b)
    directIdle (SendMsgAcquire pt client') ServerStIdle{recvMsgAcquire} = do
      server' <- recvMsgAcquire pt
      directAcquiring client' server'
    directIdle (SendMsgDone a) ServerStIdle{recvMsgDone} = do
      b <- recvMsgDone
      return (a, b)

    directAcquiring
      :: ClientStAcquiring block query m a
      -> ServerStAcquiring block query m b
      -> m (a, b)
    directAcquiring ClientStAcquiring{recvMsgAcquired} (SendMsgAcquired server') = do
      client' <- recvMsgAcquired
      directAcquired client' server'
    directAcquiring ClientStAcquiring{recvMsgFailure} (SendMsgFailure failure server') = do
      client' <- recvMsgFailure failure
      directIdle client' server'

    directAcquired
      :: ClientStAcquired block query m a
      -> ServerStAcquired block query m b
      -> m (a, b)
    directAcquired (SendMsgQuery query client') ServerStAcquired{recvMsgQuery} = do
      server' <- recvMsgQuery query
      directQuerying client' server'
    directAcquired (SendMsgReAcquire pt client') ServerStAcquired{recvMsgReAcquire} = do
      server' <- recvMsgReAcquire pt
      directAcquiring client' server'
    directAcquired (SendMsgRelease client) ServerStAcquired{recvMsgRelease} = do
      client' <- client
      server' <- recvMsgRelease
      directIdle client' server'

    directQuerying
      :: ClientStQuerying block query m a result
      -> ServerStQuerying block query m b result
      -> m (a, b)
    directQuerying ClientStQuerying{recvMsgResult} (SendMsgResult result server') = do
      client' <- recvMsgResult result
      directAcquired client' server'
