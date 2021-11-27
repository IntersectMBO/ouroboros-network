{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
-- @UndecidableInstances@ extensions is required for defining @Show@ instance
-- of @'TraceSendRecv'@.
{-# LANGUAGE UndecidableInstances #-}

-- 'runConnectedPeers' would be too polymorphic without a redundant constraint.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Network.TypedProtocol.Driver.Simple
  ( -- * Introduction
    -- $intro
    -- * Run peers
    runPeer
  , TraceSendRecv (..)
    -- * Connected peers
  , runConnectedPeers
    -- * Driver utilities
    -- | This may be useful if you want to write your own driver.
  , driverSimple
  , runDecoderWithChannel
  , Role (..)
  ) where

import           Data.Singletons

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Peer

import           Control.Applicative ((<|>))
import           Control.Exception (SomeAsyncException (..))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer (..), contramap, traceWith)


-- $intro
--
-- A 'Peer' is a particular implementation of an agent that engages in a
-- typed protocol. To actualy run one we need a source and sink for the typed
-- protocol messages. These are provided by a 'Channel' and a 'Codec'. The
-- 'Channel' represents one end of an untyped duplex message transport, and
-- the 'Codec' handles conversion between the typed protocol messages and
-- the untyped channel.
--
-- So given the 'Peer' and a compatible 'Codec' and 'Channel' we can run the
-- peer in some appropriate monad. The peer and codec have to agree on
-- the same protocol and role in that protocol. The codec and channel have to
-- agree on the same untyped medium, e.g. text or bytes. All three have to
-- agree on the same monad in which they will run.
--
-- This module provides drivers for normal and pipelined peers. There is
-- very little policy involved here so typically it should be possible to
-- use these drivers, and customise things by adjusting the peer, or codec
-- or channel.
--
-- It is of course possible to write custom drivers and the code for these ones
-- may provide a useful starting point. The 'runDecoder' function may be a
-- helpful utility for use in custom drives.
--

-- | Structured 'Tracer' output for 'runPeer' and derivitives.
--
data TraceSendRecv ps where
     TraceSendMsg :: AnyMessage ps -> TraceSendRecv ps
     TraceRecvMsg :: AnyMessage ps -> TraceSendRecv ps

instance Show (AnyMessage ps) => Show (TraceSendRecv ps) where
  show (TraceSendMsg msg) = "Send " ++ show msg
  show (TraceRecvMsg msg) = "Recv " ++ show msg


-- | An existential handle to an 'async' thread.
--
data SomeAsync m where
    SomeAsync :: forall m a. !(Async m a) -> SomeAsync m

-- | A simple driver.
--
-- It is not pure, because it exposes access to the thread which is started by
-- 'recvMessageSTM'.  This is useful for proper handling of asynchronous
-- exceptions.  There can be at most one such thread at a time.
--
driverSimple :: forall ps (pr :: PeerRole) failure bytes m.
                ( MonadAsync      m
                , MonadMask       m
                , MonadThrow (STM m)
                , Exception failure
                )
             => Tracer m (TraceSendRecv ps)
             -> Codec ps failure m bytes
             -> Channel m bytes
             -> m ( Driver ps pr bytes failure (Maybe bytes) m
                  , StrictTVar m (Maybe (SomeAsync m))
                  )
driverSimple tracer Codec{encode, decode} channel@Channel{send} = do
    v <- newTVarIO Nothing
    return
      ( Driver { sendMessage
               , recvMessage
               , tryRecvMessage
               , recvMessageSTM = recvMessageSTM v
               , startDState = Nothing
               }
      , v
      )
  where
    sendMessage :: forall (st :: ps) (st' :: ps).
                   SingI (PeerHasAgency st)
                => (ReflRelativeAgency (StateAgency st)
                                        WeHaveAgency
                                       (Relative pr (StateAgency st)))
                -> Message ps st st'
                -> m ()
    sendMessage _ msg = do
      send (encode msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))

    recvMessage :: forall (st :: ps).
                   SingI (PeerHasAgency st)
                => (ReflRelativeAgency (StateAgency st)
                                        TheyHaveAgency
                                       (Relative pr (StateAgency st)))
                -> DriverState ps pr st bytes failure (Maybe bytes) m
                -> m (SomeMessage st, Maybe bytes)
    recvMessage _ state = do
      result  <- case state of
        DecoderState decoder trailing ->
          runDecoderWithChannel channel trailing decoder
        DriverState trailing ->
          runDecoderWithChannel channel trailing =<< decode
        DriverStateSTM stmRecvMessage _trailing ->
          Right <$> atomically stmRecvMessage
      case result of
        Right x@(SomeMessage msg, _trailing') -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return x
        Left failure ->
          throwIO failure

    tryRecvMessage :: forall (st :: ps).
                      SingI (PeerHasAgency st)
                   => (ReflRelativeAgency (StateAgency st)
                                           TheyHaveAgency
                                          (Relative pr (StateAgency st)))
                   -> DriverState ps pr st bytes failure (Maybe bytes) m
                   -> m (Either (DriverState ps pr st bytes failure (Maybe bytes) m)
                                (SomeMessage st, Maybe bytes))
    tryRecvMessage _ state = do
        result <-
          case state of
            DecoderState decoder trailing ->
              tryRunDecoderWithChannel channel trailing decoder
            DriverState trailing ->
              tryRunDecoderWithChannel channel trailing =<< decode
            DriverStateSTM stmRecvMessage _trailing ->
              atomically $
                    Right . Right <$> stmRecvMessage
                <|> pure (Right (Left state))

        case result of
          Right x@(Right (SomeMessage msg, _trailing')) -> do
            traceWith tracer (TraceRecvMsg (AnyMessage msg))
            return x
          Right x@Left {} ->
            return x
          Left failure ->
            throwIO failure

    recvMessageSTM :: forall (st :: ps).
                      SingI (PeerHasAgency st)
                   => StrictTVar m (Maybe (SomeAsync m))
                   -> (ReflRelativeAgency (StateAgency st)
                                           TheyHaveAgency
                                          (Relative pr (StateAgency st)))
                   -> DriverState ps pr st bytes failure (Maybe bytes) m
                   -> m (STM m (SomeMessage st, Maybe bytes))
    recvMessageSTM v _ (DecoderState decoder trailing) = mask_ $ do
      hndl <- asyncWithUnmask $ \unmask ->
                do labelThisThread "recv-stm"
                   unmask (runDecoderWithChannel channel trailing decoder)
                `finally`
                atomically (writeTVar v Nothing)
      atomically (writeTVar v (Just $! SomeAsync hndl))
      return (do r <- waitSTM hndl
                 case r of
                   Left failure -> throwSTM failure
                   Right result -> return result
             )
    recvMessageSTM v _ (DriverState trailing) = mask_ $ do
      hndl <- asyncWithUnmask $ \unmask ->
        do labelThisThread "recv-stm"
           unmask (runDecoderWithChannel channel trailing =<< decode)
        `finally`
        atomically (writeTVar v Nothing)
      atomically (writeTVar v (Just $! SomeAsync hndl))
      return (do r <- waitSTM hndl
                 writeTVar v Nothing
                 case r of
                   Left failure -> throwSTM failure
                   Right result -> return result
             )
    recvMessageSTM _ _ (DriverStateSTM stmRecvMessage _) =
      return stmRecvMessage




-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeer
  :: forall ps (st :: ps) pr pl failure bytes m a .
     ( MonadAsync      m
     , MonadMask       m
     , MonadThrow (STM m)
     , Exception failure
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> Peer ps pr pl Empty st m a
  -> m (a, Maybe bytes)
runPeer tracer codec channel peer = do
    (driver, (v :: StrictTVar m (Maybe (SomeAsync m))))
      <- driverSimple tracer codec channel
    runPeerWithDriver driver peer (startDState driver)
      `catch` handleAsyncException v
  where
    handleAsyncException :: StrictTVar m (Maybe (SomeAsync m))
                         -> SomeAsyncException
                         -> m (a, Maybe bytes)
    handleAsyncException v e = do
      (mbHndl :: Maybe (SomeAsync m))
        <- (atomically :: forall x. STM m x -> m x)
           (readTVar v :: STM m (Maybe (SomeAsync m)))
      case mbHndl of
        Nothing               -> throwIO e
        Just (SomeAsync hndl) -> cancelWith hndl e
                              >> throwIO e



--
-- Utils
--

-- | Run a codec incremental decoder 'DecodeStep' against a channel. It also
-- takes any extra input data and returns any unused trailing data.
--
runDecoderWithChannel :: Monad m
                      => Channel m bytes
                      -> Maybe bytes
                      -> DecodeStep bytes failure m a
                      -> m (Either failure (a, Maybe bytes))

runDecoderWithChannel Channel{recv} = go
  where
    go _ (DecodeDone x trailing)         = return (Right (x, trailing))
    go _ (DecodeFail failure)            = return (Left failure)
    go Nothing         (DecodePartial k) = recv >>= k        >>= go Nothing
    go (Just trailing) (DecodePartial k) = k (Just trailing) >>= go Nothing


-- | Like 'runDecoderWithChannel' but it is only using 'tryRecv', and returns
-- either when we decoding finished, errored or 'tryRecv' returned 'Nothing'.
--
tryRunDecoderWithChannel :: Monad m
                         => Channel m bytes
                         -> Maybe bytes
                         -> DecodeStep bytes failure m (SomeMessage st)
                         -> m (Either failure
                                (Either (DriverState ps pr st bytes failure (Maybe bytes) m)
                                        (SomeMessage st, Maybe bytes)))
tryRunDecoderWithChannel Channel{tryRecv} = go
  where
    go _ (DecodeDone x trailing) = return (Right (Right (x, trailing)))
    go _ (DecodeFail failure)    = return (Left failure)
    go dstate@Nothing d@(DecodePartial k) = do
      r <- tryRecv
      case r of
        Nothing -> return (Right (Left (DecoderState d dstate)))
        Just m  -> k m >>= go Nothing
    go (Just trailing) (DecodePartial k) = k (Just trailing) >>= go Nothing

data Role = Client | Server
  deriving Show

-- | Run two 'Peer's via a pair of connected 'Channel's and a common 'Codec'.
--
-- This is useful for tests and quick experiments.
--
-- The first argument is expected to create two channels that are connected,
-- for example 'createConnectedChannels'.
--
runConnectedPeers :: forall ps pr pr' pl pl' st failure bytes m a b.
                     ( MonadAsync      m
                     , MonadMask       m
                     , MonadSTM        m
                     , MonadCatch      m
                     , MonadThrow (STM m)
                     , Exception failure
                     , pr' ~ FlipAgency pr
                     )
                  => m (Channel m bytes, Channel m bytes)
                  -> Tracer m (Role, TraceSendRecv ps)
                  -> Codec ps failure m bytes
                  -> Peer ps pr  pl  Empty st m a
                  -> Peer ps pr' pl' Empty st m b
                  -> m (a, b)
runConnectedPeers createChannels tracer codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPeer tracerClient codec clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer
