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
module Ouroboros.Network.Server.Simple where

import Control.Applicative (Alternative)
import Control.Concurrent.JobPool qualified as JobPool
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadFork)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (MonadDelay, MonadTimer)
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
         addr' <- Snocket.getLocalAddr sn sd
         pure (sd, addr'))
     (Snocket.close sn . fst)
     (\(sock, addr') ->
       -- accept loop
       withAsync (Snocket.accept sn sock >>= acceptLoop jobPool) (k addr')
     )
  where
    acceptLoop :: JobPool.JobPool () m ()
               -> Snocket.Accept m fd addr
               -> m Void
    acceptLoop jobPool Snocket.Accept { Snocket.runAccept } = do
        (accepted, acceptNext) <- runAccept
        acceptOne accepted
        acceptLoop jobPool acceptNext
      where
        -- handle accept failures and fork a connection thread which performs
        -- a handshake and runs mux
        acceptOne :: Snocket.Accepted fd addr -> m ()
        acceptOne (Snocket.AcceptFailure e)
          | Just ioErr <- fromException e
          , isECONNABORTED ioErr
          = return ()
        acceptOne (Snocket.AcceptFailure e)
          = throwIO e

        acceptOne (Snocket.Accepted sock' remoteAddr) = do
            let connThread = do
                  -- connection responder thread
                  let connId = ConnectionId addr remoteAddr
                  bearer <- Mx.getBearer makeBearer (-1) sock' Nothing
                  configureSock sock' addr
                  r <- runHandshakeServer bearer connId handshakeArgs versions
                  case r of
                    Left (HandshakeProtocolLimit e) -> throwIO e
                    Left (HandshakeProtocolError e) -> throwIO e
                    Right HandshakeQueryResult {}   -> error "handshake query is not supported"
                    Right (HandshakeNegotiationResult (SomeResponderApplication app) vNumber vData) -> do
                      mux <- Mx.new (toMiniProtocolInfos
                                      (runForkPolicy noBindForkPolicy (remoteAddress connId))
                                      app)
                      withAsync (Mx.run  Mx.nullTracers mux bearer) $ \aid -> do
                        void $ simpleMuxCallback connId vNumber vData app mux aid

                errorHandler = \e -> throwIO e
            JobPool.forkJob jobPool
                          $ JobPool.Job connThread
                                        errorHandler
                                        ()
                                        "conn-thread"
