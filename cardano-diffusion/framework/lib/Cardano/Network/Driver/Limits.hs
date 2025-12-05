{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Drivers for running 'Peer's.
--
module Cardano.Network.Driver.Limits
  ( runPeerWithLimitsRnd
  , runPipelinedPeerWithLimitsRnd
  , driverWithLimitsRnd
  , runConnectedPeersWithLimitsRnd
  , runConnectedPipelinedPeersWithLimitsRnd
  ) where

import Data.Bifunctor (first)
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import System.Random

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), contramap, traceWith)

import Network.Mux.Timeout
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Peer

import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.Util.ShowProxy
 
import Cardano.Network.Protocol.Limits


mkDriverWithLimitsRnd
  :: forall ps (pr :: PeerRole) failure bytes m f annotator.
     ( MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , Show failure
     )
  => (forall a.
           Word
        -> (bytes -> Word)
        -> Channel m bytes
        -> Maybe bytes
        -> DecodeStep bytes failure m (f a)
        -> m (Either (Maybe failure) (a, Maybe bytes))
     )
  -- ^ run incremental decoder against a channel
  --

  -> (forall (st :: ps). annotator st -> f (SomeMessage st))
  -- ^ transform annotator to a container holding the decoded
  -- message

  -> Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> StdGen
  -> CodecF ps failure m annotator bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Channel m bytes
  -> Driver ps pr (Maybe bytes, StdGen) m
mkDriverWithLimitsRnd runDecodeStep nat tracer timeoutFn rnd0
                      Codec{encode, decode}
                      ProtocolSizeLimits{sizeLimitForState, dataSize}
                      ProtocolTimeLimitsWithRnd{timeLimitForStateWithRnd}
                      channel@Channel{send} =
    Driver { sendMessage, recvMessage, initialDState = (Nothing, rnd0) }
  where
    sendMessage :: forall (st :: ps) (st' :: ps).
                   StateTokenI st
                => ActiveState st
                => WeHaveAgencyProof pr st
                -> Message ps st st'
                -> m ()
    sendMessage !_ msg = do
      send (encode msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))


    recvMessage :: forall (st :: ps).
                   StateTokenI st
                => ActiveState st
                => TheyHaveAgencyProof pr st
                -> (Maybe bytes, StdGen)
                -> m (SomeMessage st, (Maybe bytes, StdGen))
    recvMessage !_ (trailing, !rnd) = do
      let tok = stateToken
      decoder <- decode tok
      let sizeLimit = sizeLimitForState @st stateToken
          (timeLimit, rnd') = first (fromMaybe (-1))
                            $ timeLimitForStateWithRnd @st stateToken rnd
      result  <- timeoutFn timeLimit $
                   runDecodeStep sizeLimit dataSize
                                 channel trailing (nat <$> decoder)

      case result of
        Just (Right (x@(SomeMessage msg), trailing')) -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return (x, (trailing', rnd'))
        Just (Left (Just failure)) -> throwIO (DecoderFailure tok failure)
        Just (Left Nothing)        -> throwIO (ExceededSizeLimit tok)
        Nothing                    -> throwIO (ExceededTimeLimit tok)


-- | Like 'driverWithLimits' but gives access to `StdGen` to generate
-- `ProtocolTimeouts` for the next state.
--
driverWithLimitsRnd :: forall ps (pr :: PeerRole) failure bytes m.
                       ( MonadThrow m
                       , ShowProxy ps
                       , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
                       , Show failure
                       )
                    => Tracer m (TraceSendRecv ps)
                    -> TimeoutFn m
                    -> StdGen
                    -> Codec ps failure m bytes
                    -> ProtocolSizeLimits ps bytes
                    -> ProtocolTimeLimitsWithRnd ps
                    -> Channel m bytes
                    -> Driver ps pr (Maybe bytes, StdGen) m
driverWithLimitsRnd = mkDriverWithLimitsRnd runDecoderWithLimit Identity


-- | Run a peer with limits.  'ProtocolTimeLimits' have access to
-- a pseudorandom generator.
--
runPeerWithLimitsRnd
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe bytes)
runPeerWithLimitsRnd tracer rnd codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimitsRnd tracer timeoutFn rnd codec slimits tlimits channel
      in     (\(a, (trailing, _)) -> (a, trailing))
         <$> runPeerWithDriver driver peer


-- | Like 'runPipelinedPeerWithLimits' but time limits have access to
-- a pseudorandom generator.
--
runPipelinedPeerWithLimitsRnd
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadTimer m
     , MonadThrow (STM m)
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe bytes)
runPipelinedPeerWithLimitsRnd tracer rnd codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimitsRnd tracer timeoutFn rnd codec slimits tlimits channel
      in     (\(a, (trailing, _)) -> (a, trailing))
         <$> runPipelinedPeerWithDriver driver peer


runConnectedPeersWithLimitsRnd
  :: forall ps pr st failure bytes m a b.
     ( MonadAsync       m
     , MonadFork        m
     , MonadMask        m
     , MonadTimer       m
     , MonadThrow  (STM m)
     , Exception failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     )
  => m (Channel m bytes, Channel m bytes)
  -> Tracer m (Role, TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Peer ps             pr  NonPipelined st m a
  -> Peer ps (FlipAgency pr) NonPipelined st m b
  -> m (a, b)
runConnectedPeersWithLimitsRnd createChannels tracer rnd codec slimits tlimits client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (do labelThisThread "client"
        fst <$> runPeerWithLimitsRnd
                        tracerClient rnd codec slimits tlimits
                                     clientChannel client)
      `concurrently`
    (do labelThisThread "server"
        fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


runConnectedPipelinedPeersWithLimitsRnd
  :: forall ps pr st failure bytes m a b.
     ( MonadAsync      m
     , MonadFork       m
     , MonadMask       m
     , MonadTimer      m
     , MonadThrow (STM m)
     , Exception failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     )
  => m (Channel m bytes, Channel m bytes)
  -> Tracer m (Role, TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> PeerPipelined ps    pr               st m a
  -> Peer ps (FlipAgency pr) NonPipelined st m b
  -> m (a, b)
runConnectedPipelinedPeersWithLimitsRnd createChannels tracer rnd codec slimits tlimits client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPipelinedPeerWithLimitsRnd
                     tracerClient rnd codec slimits tlimits
                                        clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer
