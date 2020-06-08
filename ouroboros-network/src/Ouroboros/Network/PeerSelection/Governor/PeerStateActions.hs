{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- 'startProtocols' is using 'HasInitiator' constraint to limit pattern
-- matches.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module implements 'PeerStateActions'.
--
--
-- The schematic ilustrastion of function calls / threads and shared state
-- variables.  Reads to make assertiions are not included.  The diagram does
-- not include 'establishPeerConnection'.
--
-- > Legend: ─  - functions
-- >         │░ - threads
-- >         ━  - TVars
-- >
-- >         ├──▶┃ - write to a TVar
-- >         │◀──┨ - read from a TVar
-- >         ├──▶│ - function call
-- >
-- >         PeerStateVar - TVar which holds 'PeerState'
-- >         AwaitVar     - TVar with results of a mini-protocol (one per:
-- >                        established / warm / hot); see 'ahAwaitVar' below.
-- >         ControlVar   - TVar which holds 'ContorolMessage'; see
-- >                        'ahContorlVar' below.
-- >
-- >
-- >
-- >
-- >                     ┌──────────────────────────────────────────┐
-- >                     │ ┌────────┐                               │
-- >                     │ │        │                               │
-- >    ┌────────────────┴─┴─┐      │                               │
-- >   ┌────────────────────┐│      ▼                               ▼
-- >  ┌────────────────────┐││   ┌──────────────────────────┐     ┌─────────────────────┐
-- >  │░░░░░░░░░░░░░░░░░░░░│││   │                          │     │                     │
-- >  │░peerMonitoringLoop░││┘   │ deactivatePeerConnection │     │ closePeerConnection │
-- >  │░░░░░░░░░░░░░░░░░░░░│┘    │                          │     │                     │
-- >  └┬───────────────────┘     └┬────────────────────┬────┘     └───────┬─────────────┘
-- >   │     ▲                    │   ▲                │              ▲ ▲ │
-- >   │ ┌───┼────────────────────┘   │                │              │ │ │
-- >   │ │ ┌─┼────────────────────────┼────────────────┼──────────────┘ │ │
-- >   │ │ │ │                        │     ┌──────────┼────────────────┘ │
-- >   │ │ │ │                        │     │          │ ┌────────────────┘
-- >   │ │ │ │                        │     │          │ │
-- >   │ │ │ └───────────────┐        │     │          │ │
-- >   ▼ ▼ ▼                 │        │     │          ▼ ▼
-- >  ┏━━━━━━━━━━━━━━┓      ┏┷━━━━━━━━┷━━━━━┷┓     ┏━━━━━━━━━━━━━━━━┓
-- >  ┃              ┃┓     ┃                ┃┓    ┃                ┃┓
-- >  ┃ PeerStateVar ┃┃┓    ┃  AwaitVar      ┃┃┓   ┃  ControlVar    ┃┃┓
-- >  ┃              ┃┃┃    ┃  - established ┃┃┃   ┃  - established ┃┃┃
-- >  ┗━━━━━━━━━━━━━━┛┃┃    ┃  - warm        ┃┃┃   ┃  - warm        ┃┃┃
-- >   ┗━━━━━━━━━━━━━━┛┃    ┃  - hot         ┃┃┃   ┃  - hot         ┃┃┃
-- >    ┗━━━━━━━━━━━━━━┛    ┃                ┃┃┃   ┃                ┃┃┃
-- >    ▲                   ┗━━━━━━━━━━━━━━━━┛┃┃   ┗━━━━━━━━━━━━━━━━┛┃┃
-- >    │                    ┗━━━━━━━━━━━━━━━━┛┃    ┗━━━━━━━━━━━━━━━━┛┃
-- >    │                     ┗━━━━━━━━━━━━━━━━┛     ┗━━━━━━━━━━━━━━━━┛
-- >    │                                             ▲
-- >    │                                             │
-- >    │                                             │
-- >    │                   ┌─────────────────────────┘
-- >  ┌─┴───────────────────┴──┐
-- >  │                        │
-- >  │ activatePeerConnection │
-- >  │                        │
-- >  └────────────────────────┘
--
-- Notes:
--
-- All three upper boxes: 'peerMonitoringLoop', 'deactivatePeerConnection' and
-- 'closePeerConnection' are reading 'AwaitVar', since they need to wait for
-- protocols to finish (under a specified timeout).
--
-- All of the thin boxes are writing to 'PeerState' variable; it is read by
-- 'monitorPeerConnection.
--
-- All are writing to 'ControlVar': 'peerMonitoringLoop' does that through
-- a call to 'deactivePeerConnection' or 'closePeerConnection'.
--
-- The only asynchronious peer state changes are demotions, that's why
-- 'peerMonitoringLoop' only calls to either 'deactiveatePeerConnection' or
-- 'closePeerConnection'.
--
module Ouroboros.Network.PeerSelection.Governor.PeerStateActions
  ( PeerStateActionsArguments (..)
  , PeerConnectionHandle
  , withPeerStateActions

  -- * Exceptions
  , PeerSelectionActionException (..)
  , EstablishConnectionException (..)
  , PeerSelectionTimeoutException (..)

  -- * Trace
  , PeerSelectionActionsTrace (..)
  ) where

import           Control.Exception (Exception (..), SomeException (..), SomeAsyncException (..), assert)
import           Control.Monad (join)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Monad.Class.MonadSTM.Strict

import           Control.Concurrent.JobPool (JobPool, Job (..))
import qualified Control.Concurrent.JobPool as JobPool
import           Control.Tracer (Tracer, traceWith)


import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Typeable (Typeable, cast)

import qualified Network.Mux        as Mux
import           Network.Mux.Timeout (TimeoutFn)

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake
                   ( HandshakeException
                   , HandshakeClientProtocolError
                   , RefuseReason
                   )
import           Ouroboros.Network.PeerSelection.Governor
                   ( PeerStateActions (..)
                   )
import           Ouroboros.Network.PeerSelection.Types
                   ( PeerStatus (..)
                   )

import           Ouroboros.Network.ConnectionManager.ConnectionHandler
                   ( MuxConnectionManager
                   , MuxPromise (..)
                   )
import           Ouroboros.Network.ConnectionManager.Types


-- | A 'MuxApplicaiton', i.e. a bundle of mini-protocols, can either return an
-- error, if one of them failed (only the first error is recorded), or they all
-- terminated sucessfully.
--
data MuxApplicationResult
    -- | A mini-protocol failed with an exception.
    --
    = MuxApplicationError !MiniProtocolNum !SomeException

    -- | All mini-protocols terminated sucessfuly.
    | MuxApplicationSuccess !(Maybe MiniProtocolNum)
  deriving Show

instance Semigroup MuxApplicationResult where
    err@MuxApplicationError{} <> _ = err
    _ <> err@MuxApplicationError{} = err
    res@MuxApplicationSuccess{} <> MuxApplicationSuccess{} = res



-- | Application Handle which allows to stop or start mux threads.
--
-- TODO: only for hot applications we need 'ahApplication', we never restart
-- / stop the other ones!
data ApplicationHandle muxMode bytes m a b = ApplicationHandle {
    ahApplication :: [MiniProtocol muxMode bytes m a b],
    ahControlVar  :: StrictTVar m ControlMessage,
    ahAwaitVar    :: StrictTVar m (STM m MuxApplicationResult)
  }

--
-- Useful accessors
--

getControlVar :: TokProtocolTemperature pt
              -> Bundle (ApplicationHandle muxMode bytes m a b)
              -> StrictTVar m ControlMessage
getControlVar tok = ahControlVar . projectBundle tok

getProtocols :: TokProtocolTemperature pt
             -> Bundle (ApplicationHandle muxMode bytes m a b)
             -> [MiniProtocol muxMode bytes m a b]
getProtocols tok bundle = ahApplication (projectBundle tok bundle)

getAwaitVar :: TokProtocolTemperature pt
            -> Bundle (ApplicationHandle muxMode bytes m a b)
            -> StrictTVar m (STM m MuxApplicationResult)
getAwaitVar tok = ahAwaitVar . projectBundle tok

-- | Await for first result from any of any of the protocols which belongs to
-- the indicated bundle.
--
awaitResult :: MonadSTM m
            => TokProtocolTemperature pt
            -> Bundle (ApplicationHandle muxMode bytes m a b)
            -> STM m MuxApplicationResult
awaitResult tok = join . readTVar . getAwaitVar tok


-- | Smart construcotor for 'ApplicationHandle'.
--
mkApplicationHandleBundle
    :: forall (muxMode :: MuxMode) bytes m a b.
       MuxBundle muxMode bytes m a b
    -- ^ mux applicaiton
    -> Bundle (StrictTVar m ControlMessage)
    -- ^ schedule stop var
    -> Bundle (StrictTVar m (STM m MuxApplicationResult))
    -- ^ await for application termination
    -> Bundle (ApplicationHandle muxMode bytes m a b)
mkApplicationHandleBundle muxBundle scheduleStopVarBundle awaitVarBundle =
    Bundle
      (mkApplication TokHot)
      (mkApplication TokWarm)
      (mkApplication TokEstablished)
  where
    mkApplication :: TokProtocolTemperature pt
                  -> WithProtocolTemperature pt (ApplicationHandle muxMode bytes m a b)
    mkApplication tok =
      let app =
            ApplicationHandle {
              ahApplication = projectBundle tok muxBundle,
              ahControlVar  = projectBundle tok scheduleStopVarBundle,
              ahAwaitVar    = projectBundle tok awaitVarBundle
            }
      in case tok of
          TokHot -> WithHot app
          TokWarm -> WithWarm app
          TokEstablished -> WithEstablished app


data PeerState
  = PeerStatus      !PeerStatus
  | PromotingToWarm
  | PromotingToHot
  | DemotingToWarm
  | DemotingToCold  !PeerStatus
  -- ^ 'DemotingToCold' also contains the initial state of the peer.


-- | Return the current state of the peer, as it should be viewed by the
-- governor.
--
getCurrentState :: PeerState -> PeerStatus
getCurrentState (PeerStatus peerStatus)     = peerStatus
getCurrentState PromotingToWarm             = PeerCold
getCurrentState PromotingToHot              = PeerWarm
getCurrentState DemotingToWarm              = PeerHot
getCurrentState (DemotingToCold peerStatus) = peerStatus


-- |  Each established connection has access to 'PeerConnectionHandle'.  It
-- allows to promote / demote or close the connection, by having access to
-- 'Mux', three bundles of miniprotocols: for hot, warm and established peers
-- together with their state 'StrictTVar's.
--
data PeerConnectionHandle (muxMode :: MuxMode) peerAddr bytes m a b = PeerConnectionHandle {
    pchConnectionId :: ConnectionId peerAddr,
    pchPeerState    :: StrictTVar m PeerState,
    pchMux          :: Mux.Mux muxMode m,
    pchAppHandles   :: Bundle (ApplicationHandle muxMode bytes m a b)
  }

instance Show peerAddr
      => Show (PeerConnectionHandle muxMode peerAddr bytes m a b) where
    show PeerConnectionHandle { pchConnectionId } =
      "PeerConnectionHandle " ++ show pchConnectionId

--
-- Exceptions
--

-- | Parent exception of all peer selection action exceptions.
--
data PeerSelectionActionException = forall e. Exception e => PeerSelectionActionException e

instance Show PeerSelectionActionException where
    show (PeerSelectionActionException e) = show e

instance Exception PeerSelectionActionException

peerSelectionActionExceptionToException :: Exception e => e -> SomeException
peerSelectionActionExceptionToException = toException . PeerSelectionActionException

peerSelectionActionExceptionFromException :: Exception e => SomeException -> Maybe e
peerSelectionActionExceptionFromException x = do
    PeerSelectionActionException e <- fromException x
    cast e


data EstablishConnectionException versionNumber
      -- | Mux stopped unexpectedly.
    = EstablishConnectionMuxStoppedUnexpectedly

      -- | Handshake client failed
    | EstablishConnectionClientHandshakeException
        !(HandshakeException (HandshakeClientProtocolError versionNumber))

      -- | Handshake server failed
    | EstablishConnectionServerHandshakeException
        !(HandshakeException (RefuseReason versionNumber))
  deriving Show

instance ( Show versionNumber
         , Typeable versionNumber
         ) => Exception (EstablishConnectionException versionNumber) where
    toException   = peerSelectionActionExceptionToException
    fromException = peerSelectionActionExceptionFromException


data PeerSelectionTimeoutException peerAddr
    = PeerActivationTimeoutException      !(ConnectionId peerAddr)
    | PeerDeactivationTimeoutException    !(ConnectionId peerAddr)
    | PeerCloseConnectionTimeoutException !(ConnectionId peerAddr)
  deriving Show

instance ( Show peerAddr
         , Typeable peerAddr
         ) => Exception (PeerSelectionTimeoutException peerAddr) where
    toException   = peerSelectionActionExceptionToException
    fromException = peerSelectionActionExceptionFromException

--
-- 'PeerStateActionsArguments' and 'peerStateActions'
--


-- | Record of arguments of 'peerSelectionActions'.
--
data PeerStateActionsArguments muxMode socket peerAddr versionNumber m a b =
    PeerStateActionsArguments {

      spsTracer                 :: Tracer m (PeerSelectionActionsTrace peerAddr),

      -- | Peer deactivation timeout: timeouts stopping hot protocols.
      --
      spsDeactivateTimeout      :: DiffTime,

      -- | Timeout on closing connection: timeouts stopping established and warm
      -- peer protocols.
      --
      spsCloseConnectionTimeout :: DiffTime,

      spsConnectionManager      :: MuxConnectionManager muxMode socket peerAddr versionNumber ByteString m a b
    }


withPeerStateActions
    :: forall (muxMode :: MuxMode) socket peerAddr versionNumber m a b x.
       ( MonadAsync         m
       , MonadCatch         m
       , MonadMask          m
       , HasInitiator muxMode ~ True
       , Typeable versionNumber
       , Show     versionNumber
       , Ord      peerAddr
       , Typeable peerAddr
       , Show     peerAddr
       )
    => TimeoutFn m
    -- ^ timeout function, created by 'withTimeoutSerial'
    -> PeerStateActionsArguments muxMode socket peerAddr versionNumber m a b
    -> (PeerStateActions
          peerAddr
          (PeerConnectionHandle muxMode peerAddr ByteString m a b)
          m
          -> m x)
    -> m x

withPeerStateActions timeout
                     PeerStateActionsArguments {
                       spsDeactivateTimeout,
                       spsCloseConnectionTimeout,
                       spsTracer,
                       spsConnectionManager
                     }
                     k = do
    JobPool.withJobPool $ \jobPool ->
      k PeerStateActions {
          establishPeerConnection = establishPeerConnection jobPool,
          monitorPeerConnection,
          activatePeerConnection,
          deactivatePeerConnection,
          closePeerConnection
        }

  where
    peerMonitoringLoop
      :: PeerConnectionHandle muxMode peerAddr ByteString m a b
      -> m ()
    peerMonitoringLoop pch@PeerConnectionHandle { pchConnectionId, pchPeerState, pchAppHandles } = do
        -- A first to finish synchronisation on all the bundles; As a result
        -- this is a first to finish synchronisation between all the
        -- mini-protocols runs toward the given peer.
        r <-
          atomically $
            ((WithSomeProtocolTemperature . WithHot)
              <$> awaitResult TokHot pchAppHandles)
          `orElse`
            ((WithSomeProtocolTemperature . WithWarm)
              <$> awaitResult TokWarm pchAppHandles)
          `orElse`
            ((WithSomeProtocolTemperature . WithEstablished)
              <$> awaitResult TokEstablished pchAppHandles)

        traceWith spsTracer (PeerMonitoringResult pchConnectionId r)
        case r of
          --
          -- errors in a protocol thread
          --
          -- On error, the multiplexer closes the bearer, we take advantage of
          -- it here.  The 'peerMonitoringLoop' can terminate.
          --

          WithSomeProtocolTemperature (WithHot MuxApplicationError{}) -> do
            traceWith spsTracer (PeerStatusChanged (HotToCold pchConnectionId))
            atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          WithSomeProtocolTemperature (WithWarm MuxApplicationError{}) -> do
            traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))
            atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          WithSomeProtocolTemperature (WithEstablished MuxApplicationError{}) -> do
            state <- atomically $ do
              peerState <- readTVar pchPeerState
              writeTVar pchPeerState (PeerStatus PeerCold)
              pure peerState
            case getCurrentState state of
              PeerCold -> return ()
              PeerWarm -> traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))
              PeerHot  -> traceWith spsTracer (PeerStatusChanged (HotToCold pchConnectionId))

          --
          -- success returns
          --

          -- A /hot/ protocol terminated, we deactivate the connection and keep
          -- monitoring /warm/ and /established/ protocols.
          WithSomeProtocolTemperature (WithHot MuxApplicationSuccess {}) -> do
            deactivatePeerConnection pch `catches` handlers
            peerMonitoringLoop pch

          -- If an /established/ or /warm/ we demote the peer to 'PeerCold'.
          -- Warm protocols are quieced when a peer becomes hot, but never
          -- terminated by 'PeerStateActions' (with the obious exception of
          -- 'closePeerConnection'); also established mini-protocols are not
          -- supposed to terminate (unless the remote peer did something
          -- wrong).
          WithSomeProtocolTemperature (WithWarm MuxApplicationSuccess {}) ->
            closePeerConnection pch `catches` handlers
          WithSomeProtocolTemperature (WithEstablished MuxApplicationSuccess {}) ->
            closePeerConnection pch `catches` handlers

      where
        -- 'closePeerConnection' and 'deactivatePeerConnection' actions can
        -- throw exceptions, but they maintain consistency of 'peerStateVar',
        -- that's why these handlers are trivial.
        handlers :: [Handler m ()]
        handlers =
          [ Handler (\(_ :: PeerSelectionActionException) -> pure ()),
            Handler (\(_ :: EstablishConnectionException versionNumber) -> pure ()),
            Handler (\(_ :: PeerSelectionTimeoutException peerAddr) -> pure ())
          ]



    establishPeerConnection :: JobPool m (Maybe SomeException)
                            -> peerAddr
                            -> m (PeerConnectionHandle muxMode peerAddr ByteString m a b)
    establishPeerConnection jobPool remotePeerAddr =
      bracketOnError
        (newTVarM PromotingToWarm)
        (\peerStateVar -> atomically $ writeTVar peerStateVar (PeerStatus PeerCold))
        $ \peerStateVar -> do
          (muxPromise :: MuxPromise muxMode peerAddr versionNumber ByteString m a b)
            <- includeOutboundConnection spsConnectionManager remotePeerAddr
               >>= atomically
          case muxPromise of
            MuxRunning connectionId@ConnectionId { localAddress, remoteAddress }
                       mux
                       muxBundle
                       scheduleStopVarBundle -> do

              atomically $ do
                writeTVar (projectBundle TokHot         scheduleStopVarBundle) Terminate
                writeTVar (projectBundle TokWarm        scheduleStopVarBundle) Continue
                writeTVar (projectBundle TokEstablished scheduleStopVarBundle) Continue

              awaitVarBundle <- atomically $
                Bundle
                  <$> (WithHot <$> mkAwaitVar)
                  <*> (WithWarm <$> mkAwaitVar)
                  <*> (WithEstablished <$> mkAwaitVar)

              let connHandle =
                    PeerConnectionHandle {
                        pchConnectionId = connectionId,
                        pchPeerState    = peerStateVar,
                        pchMux          = mux,
                        pchAppHandles   = mkApplicationHandleBundle
                                            muxBundle
                                            scheduleStopVarBundle
                                            awaitVarBundle
                      }

              JobPool.forkJob jobPool
                              (Job (handleJust
                                     (\e -> case fromException e of
                                        Just SomeAsyncException {} -> Nothing
                                        Nothing -> Just e)
                                     (\e -> do
                                        traceWith spsTracer (PeerMonitoringError connectionId e)
                                        throwM e)
                                     (peerMonitoringLoop connHandle $> Nothing))
                                   Just
                                   ("peerMonitoringLoop " ++ show remoteAddress))
              startProtocols TokWarm connHandle
              startProtocols TokEstablished connHandle
              atomically $ writeTVar peerStateVar (PeerStatus PeerWarm)
              traceWith spsTracer (PeerStatusChanged
                                    (ColdToWarm
                                      remoteAddress
                                      (Just localAddress)))
              pure connHandle

            MuxStopped -> do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (ColdToWarm remotePeerAddr Nothing)
                                    MuxStoppedFailure)
              throwM
                (EstablishConnectionMuxStoppedUnexpectedly
                  :: EstablishConnectionException versionNumber)

            MuxPromiseHandshakeClientError err -> do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (ColdToWarm remotePeerAddr Nothing)
                                    HandshakeClientFailure)
              throwM (EstablishConnectionClientHandshakeException err)

            MuxPromiseHandshakeServerError err -> do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (ColdToWarm remotePeerAddr Nothing)
                                    HandshakeServerFailure)
              throwM (EstablishConnectionServerHandshakeException err)

            MuxPromiseError err -> do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (ColdToWarm remotePeerAddr Nothing)
                                    (MuxPromiseFailure err))
              throwM err
      where
        mkAwaitVar :: STM m (StrictTVar m (STM m MuxApplicationResult))
        mkAwaitVar = newTVar retry


    -- 'monitorPeerConnection' is only used against established connections
    monitorPeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                          -> STM m PeerStatus
    monitorPeerConnection PeerConnectionHandle { pchPeerState } =
      getCurrentState <$> readTVar pchPeerState


    -- Take a warm peer and promote it to a hot one.
    activatePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                           -> m ()
    activatePeerConnection
        connHandle@PeerConnectionHandle {
            pchConnectionId,
            pchPeerState,
            pchAppHandles } =
      do
        -- quiesce warm peer protocols and set hot ones in 'Continue' mode.
        atomically $ do
          writeTVar pchPeerState PromotingToHot
          writeTVar (getControlVar TokHot pchAppHandles) Continue
          writeTVar (getControlVar TokWarm pchAppHandles) Quiesce
          e <- readTVar (getControlVar TokEstablished pchAppHandles)
          assert (e == Continue) $ pure ()

        -- start hot peer protocols
        startProtocols TokHot connHandle
        atomically $ writeTVar pchPeerState (PeerStatus PeerHot)
        traceWith spsTracer (PeerStatusChanged (WarmToHot pchConnectionId))
      `onException`
        atomically (writeTVar pchPeerState (PeerStatus PeerCold))


    -- Take a hot peer and demote it to a warm one.
    deactivatePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b -> m ()
    deactivatePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerState,
            pchMux,
            pchAppHandles
          } =
      do
        atomically $ do
          writeTVar pchPeerState DemotingToWarm
          writeTVar (getControlVar TokHot pchAppHandles) Terminate

          w <- readTVar (getControlVar TokWarm pchAppHandles)
          assert (w == Quiesce) $ pure ()
          writeTVar (getControlVar TokWarm pchAppHandles) Continue

          e <- readTVar (getControlVar TokEstablished pchAppHandles)
          assert (e == Continue) $ pure ()

        -- Hot protocols should stop within 'spsDeactivateTimeout'.
        res <-
          timeout spsDeactivateTimeout
                  (atomically $ awaitResult TokHot pchAppHandles)
        case res of
          Nothing -> do
            Mux.stopMux pchMux
            traceWith spsTracer (PeerStatusChangeFailure
                                  (HotToWarm pchConnectionId)
                                  TimeoutError)
            throwM (PeerDeactivationTimeoutException pchConnectionId)

          Just (MuxApplicationError protocolNum e@(SomeException err)) -> do
            traceWith spsTracer (PeerStatusChangeFailure
                                  (HotToCold pchConnectionId)
                                  (ApplicationFailure protocolNum e))
            throwM (peerSelectionActionExceptionToException err)

          Just MuxApplicationSuccess {} -> do
            atomically $ writeTVar pchPeerState (PeerStatus PeerWarm)
            traceWith spsTracer (PeerStatusChanged (HotToWarm pchConnectionId))

        `onException`
          atomically (writeTVar pchPeerState (PeerStatus PeerCold))


    closePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                        -> m ()
    closePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerState,
            pchMux,
            pchAppHandles
          } =
      do
        atomically $ do
          currentState <- getCurrentState <$> readTVar pchPeerState
          writeTVar pchPeerState (DemotingToCold currentState)
          writeTVar (getControlVar TokWarm pchAppHandles) Terminate
          writeTVar (getControlVar TokEstablished pchAppHandles) Terminate
          writeTVar (getControlVar TokHot pchAppHandles) Terminate

        res <-
          timeout spsCloseConnectionTimeout
                  (atomically $
                    (\a b c -> a <> b <> c)
                      <$> awaitResult TokHot pchAppHandles
                      <*> awaitResult TokWarm pchAppHandles
                      <*> awaitResult TokEstablished pchAppHandles)
        case res of
          Nothing -> do
            Mux.stopMux pchMux
            traceWith spsTracer (PeerStatusChangeFailure
                                  (WarmToCold pchConnectionId)
                                  TimeoutError)

            throwM (PeerCloseConnectionTimeoutException pchConnectionId)

          Just (MuxApplicationError protocolNum e@(SomeException err)) -> do
            traceWith spsTracer (PeerStatusChangeFailure
                                  (WarmToCold pchConnectionId)
                                  (ApplicationFailure protocolNum e))
            throwM (peerSelectionActionExceptionToException err)

          Just MuxApplicationSuccess {} ->
            traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))
      `finally`
          atomically (writeTVar pchPeerState (PeerStatus PeerCold))

--
-- Utils
--


-- | Given a singleton 'TokAppKind' and 'PeerConnectionHandle' start the mux
-- protocol bundle indicated by the type of the first argument.
--
startProtocols :: forall (muxMode :: MuxMode) (pt :: ProtocolTemperature) peerAddr m a b.
                  ( MonadAsync m
                  , MonadCatch m
                  , HasInitiator muxMode ~ True
                  )
               => TokProtocolTemperature pt
               -> PeerConnectionHandle muxMode peerAddr ByteString m a b
               -> m ()
startProtocols tok PeerConnectionHandle { pchMux, pchAppHandles } = do
    let ptcls = getProtocols tok pchAppHandles
    as <- traverse runInitiator ptcls
    atomically $ writeTVar (getAwaitVar tok pchAppHandles)
                           (awaitSTM $ zip (miniProtocolNum `map` ptcls) as)
  where
    -- first to finish synchronisation between all the mini-protocols of
    -- a protocol bundle:
    -- * hot → warm transition: if a protocol exists cleanly we need to demote
    --   the peer to warm, using first-to-finish synchronisation allows to act
    --   as soon as a protocol exists.
    -- * if a protocol errors we demote the peer to cold, in this case we the
    --   bearer is closed.  Using first-to-finish synchronisation allows to act
    --   as soon as one of the protcol errors.
    awaitSTM :: [(MiniProtocolNum, STM m (Either SomeException a))]
             -> STM m MuxApplicationResult
    awaitSTM = foldr (\(miniProtocolNum, stm) acc ->
                       (either (MuxApplicationError miniProtocolNum)
                               (const (MuxApplicationSuccess (Just miniProtocolNum)))
                         <$> stm)
                       `orElse` acc)
                     retry

    runInitiator :: MiniProtocol muxMode ByteString m a b
                 -> m (STM m (Either SomeException a))
    runInitiator MiniProtocol {
                      miniProtocolNum,
                      miniProtocolRun
                    } =
      case miniProtocolRun of
        InitiatorProtocolOnly initiator ->
            Mux.runMiniProtocol
              pchMux miniProtocolNum
              Mux.InitiatorDirectionOnly
              Mux.StartEagerly
              (runMuxPeer initiator . fromChannel)
        InitiatorAndResponderProtocol initiator _ ->
            Mux.runMiniProtocol
              pchMux miniProtocolNum
              Mux.InitiatorDirection
              Mux.StartEagerly
              (runMuxPeer initiator . fromChannel)

--
-- Trace
--

-- | Type of failure with additional exception context; We don't log handshake
-- errors as this will be done by the handshake tracer.
--
data FailureType =
      HandshakeClientFailure
    | HandshakeServerFailure
    | MuxPromiseFailure !SomeException
    | MuxStoppedFailure
    | TimeoutError
    | ApplicationFailure !MiniProtocolNum !SomeException
  deriving Show

-- | All transitions.
--
data PeerStatusChangeType peerAddr =
    -- | During the 'ColdToWarm' transition we have the remote address, and only
    -- if establishing connection (establishing bearer & handhsake negotation)
    -- is successful we have access to full `ConnectionId`.
      ColdToWarm
        !peerAddr         -- ^ remote peer address
        !(Maybe peerAddr) -- ^ local peer address
    | WarmToHot  !(ConnectionId peerAddr)
    | HotToWarm  !(ConnectionId peerAddr)
    | WarmToCold !(ConnectionId peerAddr)
    | HotToCold  !(ConnectionId peerAddr)
  deriving Show

-- | Traces produced by 'peerSelectionActions'.
--
data PeerSelectionActionsTrace peerAddr =
      PeerStatusChanged       !(PeerStatusChangeType peerAddr)
    | PeerStatusChangeFailure !(PeerStatusChangeType peerAddr) !FailureType
    | PeerMonitoringError     !(ConnectionId peerAddr) !SomeException
    | PeerMonitoringResult    !(ConnectionId peerAddr) !(WithSomeProtocolTemperature MuxApplicationResult)
  deriving Show
