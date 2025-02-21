{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple server.  The server doesn't control resource usage (e.g. limiting
-- of inbound connections) and thus should only be used in a safe environment.
--
-- The module should be imported qualified.
module Ouroboros.Network.Server.Simple where

import Control.Applicative (Alternative)
import Control.Concurrent.JobPool qualified as JobPool
import Control.Monad (forever)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadFork)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy qualified as BL
import Data.Functor (void)
import Data.Typeable (Typeable)
import Data.Void (Void)

import Network.Mux qualified as Mx

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Snocket as Snocket
import Ouroboros.Network.Socket


-- TODO: should be moved to `ouroboros-network-framework`
-- It is needed in `ekg-forward` and probably wallet too.
with :: forall fd addr vNumber vData m a b.
        ( Alternative (STM m),
          MonadAsync m,
          MonadFork  m,
          MonadLabelledSTM m,
          MonadMask  m,
          MonadTimer m,
          MonadThrow (STM m),
          Ord vNumber,
          Typeable vNumber,
          Show vNumber
        )
     => Snocket m fd addr
     -> Mx.MakeBearer m fd
     -> (fd -> addr -> m ())
     -> addr
     -> HandshakeArguments (ConnectionId addr) vNumber vData m
     -> Versions vNumber vData (SomeResponderApplication addr BL.ByteString m b)
     -> (addr -> Async m Void -> m a)
     -> m a
with sn makeBearer configureSock addr handshakeArgs versions k =
   JobPool.withJobPool $ \jobPool ->
   bracket
     (do sd <- Snocket.open sn (Snocket.addrFamily sn addr)
         configureSock sd addr
         Snocket.bind sn sd addr
         Snocket.listen sn sd
         addr' <- getLocalAddr sn sd
         pure (sd, addr'))
     (Snocket.close sn . fst)
     (\(sock, addr') ->
       -- accept loop
       withAsync (forever $ acceptOne jobPool sock) (k addr')
     )
  where
    acceptOne :: JobPool.JobPool () m () -> fd -> m ()
    acceptOne jobPool sock = accept sn sock >>= runAccept >>= \case
      (Accepted sock' remoteAddr, _) -> do
        let connThread = do
              -- connection responder thread
              let connId = ConnectionId addr remoteAddr
              bearer <- Mx.getBearer makeBearer
                        (-1) nullTracer sock'
              configureSock sock' addr
              r <- runHandshakeServer bearer connId handshakeArgs versions
              case r of
                Left (HandshakeProtocolLimit e) -> throwIO e
                Left (HandshakeProtocolError e) -> throwIO e
                Right HandshakeQueryResult {}   -> error "handshake query is not supported"
                Right (HandshakeNegotiationResult (SomeResponderApplication app) vNumber vData) -> do
                  mux <- Mx.new (toMiniProtocolInfos (runForkPolicy noBindForkPolicy (remoteAddress connId)) app)
                  withAsync (Mx.run nullTracer mux bearer) $ \aid -> do
                    void $ simpleMuxCallback connId vNumber vData app mux aid

            errorHandler = \e -> throwIO e

        JobPool.forkJob jobPool
                      $ JobPool.Job connThread
                                    errorHandler
                                    ()
                                    "conn-thread"
      (AcceptFailure e, _) -> throwIO e
