{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple server.  The server doesn't control resource usage (e.g. limiting
-- of inbound connections) and thus should only be used in a safe environment.
--
-- The module should be imported qualified.
module Ouroboros.Network.Server.Simple
  ( with
  , ServerTracer (..)
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.JobPool qualified as JobPool
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadFork)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (MonadDelay, MonadTimer)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy qualified as BL
import Data.Functor (void)
import Data.Typeable (Typeable)
import Data.Void (Void)

import Network.Mux qualified as Mx

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Server (isECONNABORTED)
import Ouroboros.Network.Snocket (Snocket)
import Ouroboros.Network.Snocket qualified as Snocket
import Ouroboros.Network.Socket

data ServerTracer addr
  = AcceptException SomeException
  | ConnectionHandlerException (ConnectionId addr) SomeException
  deriving Show

with :: forall fd addr vNumber vData m a b.
        ( Alternative (STM m),
          MonadAsync m,
          MonadDelay m,
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
     -> Tracer m (ServerTracer addr)
     -> Mx.TracersWithBearer (ConnectionId addr) m
     -> Mx.MakeBearer m fd
     -> (fd -> addr -> m ())
     -> addr
     -> HandshakeArguments (ConnectionId addr) vNumber vData m
     -> Versions vNumber vData (SomeResponderApplication addr BL.ByteString m b)
     -> (addr -> Async m Void -> m a)
     -> m a
with sn tracer muxTracers makeBearer configureSock addr handshakeArgs versions k =
   JobPool.withJobPool $ \jobPool ->
   bracket
     (do sd <- Snocket.open sn (Snocket.addrFamily sn addr)
         configureSock sd addr
         Snocket.bind sn sd addr
         Snocket.listen sn sd
         addr' <- Snocket.getLocalAddr sn sd
         pure (sd, addr'))
     (Snocket.close sn . fst)
     (\(sock, localAddress) ->
       -- accept loop
       withAsync (Snocket.accept sn sock >>= acceptLoop jobPool localAddress)
                 (k localAddress)
     )
  where
    acceptLoop :: JobPool.JobPool () m ()
               -> addr
               -> Snocket.Accept m fd addr
               -> m Void
    acceptLoop
        jobPool
        localAddress
        Snocket.Accept { Snocket.runAccept }
        = do
        (accepted, acceptNext) <- runAccept
        acceptOne accepted
        acceptLoop jobPool
                   localAddress
                   acceptNext
      where
        -- handle accept failures and fork a connection thread which performs
        -- a handshake and runs mux
        acceptOne :: Snocket.Accepted fd addr -> m ()
        acceptOne (Snocket.AcceptFailure e)
          | Just ioErr <- fromException e
          , isECONNABORTED ioErr
          = traceWith tracer (AcceptException e)
        acceptOne (Snocket.AcceptFailure e)
          = do traceWith tracer (AcceptException e)
               throwIO e

        acceptOne (Snocket.Accepted sock' remoteAddress) = do
            let connId = ConnectionId { localAddress, remoteAddress }
                connThread = do
                  -- connection responder thread
                  bearer <- Mx.getBearer makeBearer (-1) sock' Nothing
                  configureSock sock' localAddress
                  r <- runHandshakeServer bearer connId handshakeArgs versions
                  case r of
                    Left (HandshakeProtocolLimit e) -> throwIO e
                    Left (HandshakeProtocolError e) -> throwIO e
                    Right HandshakeQueryResult {}   -> error "handshake query is not supported"
                    Right (HandshakeNegotiationResult (SomeResponderApplication app) vNumber vData) -> do
                      mux <- Mx.new (connId `Mx.tracersWithBearer` muxTracers)
                                    (toMiniProtocolInfos
                                      (runForkPolicy noBindForkPolicy remoteAddress)
                                      app)
                      withAsync (Mx.run mux bearer) $ \aid -> do
                        void $ simpleMuxCallback connId vNumber vData app mux aid

                errorHandler = \e -> traceWith tracer (ConnectionHandlerException connId e)
                                  >> throwIO e
            JobPool.forkJob jobPool
                          $ JobPool.Job connThread
                                        errorHandler
                                        ()
                                        "conn-thread"
