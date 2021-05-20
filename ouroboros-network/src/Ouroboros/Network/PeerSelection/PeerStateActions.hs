{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}

-- 'startProtocols' is using 'HasInitiator' constraint to limit pattern
-- matches.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Ouroboros.Network.PeerSelection.PeerStateActions
  (
  -- $doc
    PeerStateActionsArguments (..)
  , PeerConnectionHandle
  , withPeerStateActions

  -- * Exceptions
  , PeerSelectionActionException (..)
  , EstablishConnectionException (..)
  , PeerSelectionTimeoutException (..)

  -- * Trace
  , PeerSelectionActionsTrace (..)
  ) where

import           Control.Exception (SomeAsyncException (..))
import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadSTM.Strict

import           Control.Concurrent.JobPool (JobPool, Job (..))
import qualified Control.Concurrent.JobPool as JobPool
import           Control.Tracer (Tracer, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Typeable (Typeable, cast)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Network.Mux        as Mux

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake (HandshakeException)
import           Ouroboros.Network.PeerSelection.Governor
                   ( PeerStateActions (..)
                   )
import           Ouroboros.Network.PeerSelection.Types
                   ( PeerStatus (..)
                   )

import           Ouroboros.Network.ConnectionHandler
                   ( MuxConnectionManager
                   , Handle (..)
                   , HandleError (..)
                   )
import           Ouroboros.Network.ConnectionManager.Types

-- $doc
-- = Introduction
--
-- This module implements 'PeerStateActions', which provide the following
-- capabilities::
--
-- [synchronous promotions / demotions]:
--
--      * 'establishPeerConnection'
--      * 'activatePeerConnection'
--      * 'deactivatePeerConnection'
--      * 'closePeerConnection'
--
-- [asynchronous demotions]:
--
-- Monitor mini-protocols and act on mini-protocol state changes done via
-- 'monitorPeerConnection'.
--
--
-- = Synchronous promotions / demotions
--
-- Synchronous promotions / demotions are directly used by
-- 'Ouroboros.Network.PeerSelection.Governor.peerSelectionGovernor'.
--
-- [synchronous /cold → warm/ transition]:
--    This transition starts with creating or reusing an inbound connection, do
--    handshake (functionality provided by connection manager), start
--    established and warm mini-protocols, start monitoring thread specified
--    below.
--
-- [synchronous /warm → hot/ transition]:
--    This transition quiesce warm protocols and starts hot protocols.  There
--    is no timeout to quiesce warm mini-protocols.  The tip-sample protocol
--    which is the only planned warm protocol has some states that have
--    a longer timeout when the remote peer has agency, but it does not
--    transfers much data.
--
-- [synchronous /hot → warm/ transition]:
--    Within a timeout, stop hot protocols and let the warm protocols continue
--    running.  If the timeout expires the connection is closed.  Note that this
--    will impact inbound side of a duplex connection.  We cannot do any
--    better: closing is a cooperative action since we require to arrive at
--    a well defined state of the multiplexer (no outstanding data in ingress
--    queue).  This transition must use last to finish synchronisation of all
--    hot mini-protocols.
--
-- [synchronous /warm → cold/ transition]:
--    Shutdown established and warm protocols.  As in the previous transition
--    it must use last to finish synchronisation on established and warm
--    protocol termination, if this synchronisation timeouts the connection is
--    closed.
--
-- = Monitoring Loop
--
-- The monitoring loop is responsible for taking an action when one of the
-- mini-protocols either terminates or errors.  Except termination of a hot
-- protocols we shall close the connection.  When one of the hot protocols
-- terminates we trigger a synchronous /hot → warm/ transition.
--
-- The monitoring loop is supposed to stop when the multiplexer stops.
--
-- Note that the monitoring loop must act as soon as one of the mini-protocols
-- terminates or errors, hence the use of first to finish synchronisation.
--
-- The multiplexer guarantees that whenever one of the mini-protocols errors the
-- connection is closed.  This simplifies the actions needed to be taken by the
-- monitoring loop.
--
--
-- = Asynchronous demotions
--
-- [asynchronous /* → cold/ transition]:
-- This demotion is triggered whenever any of the mini-protocol errors.  This
-- does not require a further action by the monitoring loop: mux will close the
-- connection, monitoring loop will terminate.
--
-- [asynchronous /hot → warm/ demotion ]:
-- This demotion is triggered if a hot mini-protocol terminates cleanly.  In
-- this case we trigger synchronous /hot → warm/ demotion which will halt all
-- hot mini-protocols and will notify the peer-to-peer governor about the
-- change.
--
-- = Implementation details
-- 
-- 'PeerStateActions' are build on top of 'ConnectionManager' which provides
-- a primitive to present us a negotiated connection (i.e. after running
-- the handshake) and the multiplexer api which allows to start mini-protocols
-- and track their termination via an 'STM' interface.  Each connection has
-- associated 'PeerConnectionHandle' which holds all the data associated with
-- a connection.
--
-- Most important are @pchMux :: Mux mode m@ which allows us
-- to interact with the multiplexer and 'pchAppHandles'.  The latter contains
-- information about each mini-protocol and its 'STM' mini-protocol monitoring
-- action.  'ahMiniProtocolResults' allows us to build last-to-finish
-- 'awaitAllResults' and first-to-finish 'awaitFirstResult' synchronisations that
-- we need in synchronous transitions and monitoring loop respectively.
--
-- 'ahControlVar' is a per-temperature 'TVar' which holds 'ControlMessage'.  It
-- is passed from 'ConnectionHandler' via 'Handle'.  This variable allows
-- us to terminate, quiesce or re-enable mini-protocols.
--
--
-- Bellow is a schematic illustration of function calls / threads and shared
-- state variables.  Reads done just make assertions are not included.  The
-- diagram does not include 'establishPeerConnection'.
--
-- > Legend: ─  - functions
-- >         │░ - threads
-- >         ━  - STM mutable variables
-- >
-- >         ├──▶┃ - write to a TVar
-- >         │◀──┨ - read from a TVar
-- >         ├──▶│ - function call
-- >
-- >         PeerStateVar        - 'pchPeerState' 'TVar'
-- >         MiniProtocolResults - 'ahMiniProtocolResults' 'TVar'
-- >         ControlVar          - 'ahControlVar' 'TVar'
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
-- > ▒▒│▒│▒│▒│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│▒▒▒▒▒│▒▒▒▒▒▒▒▒▒▒│▒│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
-- > ▒ │ │ │ └───────────────┐        │     │          │ │                       ▒▒▒
-- > ▒ ▼ ▼ ▼                 │        │     │          ▼ ▼                       ▒ ▒▒▒
-- > ▒┏━━━━━━━━━━━━━━┓      ┏┷━━━━━━━━┷━━━━━┷┓     ┏━━━━━━━━━━━━━━━━┓            ▒ ▒ ▒
-- > ▒┃              ┃┓     ┃                ┃┓    ┃                ┃┓           ▒ ▒ ▒
-- > ▒┃ PeerStateVar ┃┃┓    ┃  MiniProtocol  ┃┃┓   ┃  ControlVar    ┃┃┓          ▒ ▒ ▒
-- > ▒┃              ┃┃┃    ┃     Results    ┃┃┃   ┃  - established ┃┃┃          ▒ ▒ ▒
-- > ▒┃              ┃┃┃    ┃  - established ┃┃┃   ┃  - warm        ┃┃┃          ▒ ▒ ▒
-- > ▒┗━━━━━━━━━━━━━━┛┃┃    ┃  - warm        ┃┃┃   ┃  - hot         ┃┃┃          ▒ ▒ ▒
-- > ▒ ┗━━━━━━━━━━━━━━┛┃    ┃  - hot         ┃┃┃   ┃                ┃┃┃          ▒ ▒ ▒
-- > ▒  ┗━━━━━━━━━━━━━━┛    ┃                ┃┃┃   ┃                ┃┃┃          ▒ ▒ ▒
-- > ▒  ▲                   ┗━━━━━━━━━━━━━━━━┛┃┃   ┗━━━━━━━━━━━━━━━━┛┃┃          ▒ ▒ ▒
-- > ▒  │                    ┗━━━━━━━━━━━━━━━━┛┃    ┗━━━━━━━━━━━━━━━━┛┃          ▒ ▒ ▒
-- > ▒  │                     ┗━━━━━━━━━━━━━━━━┛     ┗━━━━━━━━━━━━━━━━┛          ▒ ▒ ▒
-- > ▒  │                                             ▲                          ▒ ▒ ▒
-- > ▒  │                   PeerConnectionHandles     │                          ▒ ▒ ▒
-- > ▒▒▒│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ▒ ▒
-- >  ▒ │                                             │                            ▒ ▒
-- >  ▒▒│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ▒
-- >   ▒│                                             │                              ▒
-- >   ▒│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
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
-- 'closePeerConnection' are reading 'ahMiniProtocolResults' via the
-- last-to-finish 'awaitAllResults' synchronisation.
--
-- All of the thin boxes are writing to 'pchPeerState' variable; which is read
-- by 'monitorPeerConnection.  Also all of them writing to 'ahControlVar':
-- 'peerMonitoringLoop' does that through a call to 'deactivePeerConnection' or
-- 'closePeerConnection'.

-- | `Mux` gives us access to @'Either' 'SomeException' a@ but in this module
-- we also want to explicitly state that a mini-protocol is not running.  This
-- helps us explicitly track if hot protocols are running or not.  Note that
-- established and warm protocol are always running as far as mux is concerned
-- when the peer is not cold (though they might be quiesced).
--
data HasReturned a
    -- | A mini-protocol has returned value of type @a@.
  = Returned !a
    -- | A mini-protocol thrown some exception
  | Errored  !SomeException
   -- | A mini-protocol is not running.  This makes tracking state of hot
   -- protocols explicit, as they will not be running if a peer is in warm
   -- state.
  | NotRunning

hasReturnedFromEither :: Either SomeException a -> HasReturned a
hasReturnedFromEither (Left e)  = Errored e
hasReturnedFromEither (Right a) = Returned a


data MiniProtocolException = MiniProtocolException {
    mpeMiniProtocolNumber    :: !MiniProtocolNum,
    mpeMiniProtocolException :: !SomeException
  }
  deriving Show

newtype MiniProtocolExceptions = MiniProtocolExceptions [MiniProtocolException]
  deriving (Show, Typeable)

instance Exception MiniProtocolExceptions


-- | Application Handle which allows to stop or start mux threads.  This only
-- contains information which depends on peer temperature.
--
-- TODO: only for hot applications we need 'ahApplication', we never restart
-- / stop the other ones!
data ApplicationHandle muxMode bytes m a b = ApplicationHandle {
    -- | List of applications for the given peer temperature.
    --
    ahApplication         :: [MiniProtocol muxMode bytes m a b],

    -- | 'ControlMessage' 'TVar' for the given peer temperature.
    --
    ahControlVar          :: StrictTVar m ControlMessage,

    -- | 'TVar' which allows to track each mini-protocol of a given
    -- temperature.
    --
    ahMiniProtocolResults :: StrictTVar m (Map MiniProtocolNum
                                            (STM m (HasReturned a)))
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

getMiniProtocolsVar :: TokProtocolTemperature pt
                    -> Bundle (ApplicationHandle muxMode bytes m a b)
                    -> StrictTVar m (Map MiniProtocolNum (STM m (HasReturned a)))
getMiniProtocolsVar tok = ahMiniProtocolResults . projectBundle tok


--
-- Synchronisation primitives
--

-- | A result of a mini-protocol used by first-to-finish synchronisation
-- 'awaitFirstResult'.   For first-to-finish synchronisation we would like to
-- know which mini-protocol returned or errored.  This is useful for logging.
--
data FirstToFinishResult
    -- | A mini-protocol failed with an exception.
    = MiniProtocolError   !MiniProtocolException

    -- | A mini-protocols terminated sucessfuly.
    --
    -- TODO: we should record the return value of a protocol: it is meaningful
    -- (for tracing).  But it requires more plumbing to be done: consensus
    -- applications, as we see them, return `()`!
    | MiniProtocolSuccess !MiniProtocolNum
  deriving Show

instance Semigroup FirstToFinishResult where
    err@MiniProtocolError{} <> _ = err
    _ <> err@MiniProtocolError{} = err
    res@MiniProtocolSuccess{} <> MiniProtocolSuccess{} = res


-- | Await for first result from any of any of the protocols which belongs to
-- the indicated bundle.
--
awaitFirstResult :: MonadSTM m
                 => TokProtocolTemperature pt
                 -> Bundle (ApplicationHandle muxMode bytes m a b)
                 -> STM m FirstToFinishResult
awaitFirstResult tok bundle = do
    d <- readTVar (getMiniProtocolsVar tok bundle)
    (miniProtocolNum, result)
      <- Map.foldrWithKey (\num stm acc -> ((num,) <$> stm) `orElse` acc)
                          retry d
    case result of
      Errored  e -> pure $ MiniProtocolError   (MiniProtocolException miniProtocolNum e)
      Returned _ -> pure $ MiniProtocolSuccess miniProtocolNum
      -- We block if a mini-protocol is not running.  For established or warm
      -- mini-protocols this can only happen when we establish the connection.
      -- For hot mini-protocols this will be the case when the peer is warm:
      -- we are interested when the first established or warm mini-protocol
      -- returned.
      NotRunning -> retry


-- | Data structure used in last-to-finish synchronisation 'awaitAll'.
--
data LastToFinishResult =
    AllSucceeded
  | SomeErrored ![MiniProtocolException]

instance Semigroup LastToFinishResult where
    AllSucceeded    <> AllSucceeded    = AllSucceeded
    e@SomeErrored{} <> AllSucceeded    = e
    AllSucceeded    <> e@SomeErrored{} = e
    SomeErrored e   <> SomeErrored e'  = SomeErrored (e ++ e')

instance Monoid LastToFinishResult where
    mempty = AllSucceeded


-- | Last to finish synchronisation for mini-protocols of a given protocol
-- temperature.
--
awaitAllResults :: MonadSTM m
                => TokProtocolTemperature pt
                -> Bundle (ApplicationHandle muxMude bytes m a b)
                -> STM m LastToFinishResult
awaitAllResults tok bundle = do
    results <-  readTVar (getMiniProtocolsVar tok bundle)
            >>= sequence
    return $ Map.foldMapWithKey
               (\num r -> case r of
                          Errored  e -> SomeErrored [MiniProtocolException num e]
                          Returned _ -> AllSucceeded
                          NotRunning -> AllSucceeded)
               results


--
-- Internals: peer state & connection handle
--


data PeerState
  = PeerStatus      !PeerStatus
  | PromotingToWarm
  | PromotingToHot
  | DemotingToWarm
  | DemotingToCold  !PeerStatus
  -- ^ 'DemotingToCold' also contains the initial state of the peer.
  deriving Eq


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
      -- | Handshake client failed
    = ClientException
        !(HandshakeException versionNumber)

      -- | Handshake server failed
    | ServerException
        !(HandshakeException versionNumber)
  deriving Show

instance ( Show versionNumber
         , Typeable versionNumber
         ) => Exception (EstablishConnectionException versionNumber) where
    toException   = peerSelectionActionExceptionToException
    fromException = peerSelectionActionExceptionFromException


data PeerSelectionTimeoutException peerAddr
    = DeactivationTimeout    !(ConnectionId peerAddr)
    | CloseConnectionTimeout !(ConnectionId peerAddr)
  deriving Show

instance ( Show peerAddr
         , Typeable peerAddr
         ) => Exception (PeerSelectionTimeoutException peerAddr) where
    toException   = peerSelectionActionExceptionToException
    fromException = peerSelectionActionExceptionFromException


data ColdActionException peerAddr
    = ColdActivationException   !(ConnectionId peerAddr)
    | ColdDeactivationException !(ConnectionId peerAddr)
  deriving Show

instance ( Show peerAddr
         , Typeable peerAddr
         ) => Exception (ColdActionException peerAddr) where
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
       , MonadTimer         m
       , MonadThrow         (STM m)
       , HasInitiator muxMode ~ True
       , Typeable versionNumber
       , Show     versionNumber
       , Ord      peerAddr
       , Typeable peerAddr
       , Show     peerAddr
       )
    => PeerStateActionsArguments muxMode socket peerAddr versionNumber m a b
    -> (PeerStateActions
          peerAddr
          (PeerConnectionHandle muxMode peerAddr ByteString m a b)
          m
          -> m x)
    -> m x

withPeerStateActions PeerStateActionsArguments {
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

    -- Update PeerState with the new state only if the current state isn't
    -- cold. Returns True if the state wasn't PeerCold
    updateUnlessCold :: StrictTVar m PeerState -> PeerState -> STM m Bool
    updateUnlessCold stateVar newState = do
      status <- getCurrentState <$> readTVar stateVar
      if status == PeerCold
         then return False
         else writeTVar stateVar newState >> return True


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
              <$> awaitFirstResult TokHot pchAppHandles)
          `orElse`
            ((WithSomeProtocolTemperature . WithWarm)
              <$> awaitFirstResult TokWarm pchAppHandles)
          `orElse`
            ((WithSomeProtocolTemperature . WithEstablished)
              <$> awaitFirstResult TokEstablished pchAppHandles)

        traceWith spsTracer (PeerMonitoringResult pchConnectionId r)
        case r of
          --
          -- Errors in a protocol thread (asynchronous demotions to cold state)
          --
          -- On error, the multiplexer closes the bearer, we take advantage of
          -- it here.
          --
          WithSomeProtocolTemperature (WithHot MiniProtocolError{}) -> do
            traceWith spsTracer (PeerStatusChanged (HotToCold pchConnectionId))
            atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          WithSomeProtocolTemperature (WithWarm MiniProtocolError{}) -> do
            traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))
            atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          WithSomeProtocolTemperature (WithEstablished MiniProtocolError{}) -> do
            -- update 'pchPeerState' and log (as the two other transition to
            -- cold state.
            state <- atomically $ do
              peerState <- readTVar pchPeerState
              writeTVar pchPeerState (PeerStatus PeerCold)
              pure peerState
            case getCurrentState state of
              PeerCold -> return ()
              PeerWarm -> traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))
              PeerHot  -> traceWith spsTracer (PeerStatusChanged (HotToCold pchConnectionId))

          --
          -- Successful termination
          --

          -- A /hot/ protocol terminated, we deactivate the connection and keep
          -- monitoring /warm/ and /established/ protocols.
          WithSomeProtocolTemperature (WithHot MiniProtocolSuccess {}) -> do
            deactivatePeerConnection pch `catches` handlers
            peerMonitoringLoop pch

          -- If an /established/ or /warm/ we demote the peer to 'PeerCold'.
          -- Warm protocols are quieced when a peer becomes hot, but never
          -- terminated by 'PeerStateActions' (with the obvious exception of
          -- 'closePeerConnection'); also established mini-protocols are not
          -- supposed to terminate (unless the remote peer did something
          -- wrong).
          WithSomeProtocolTemperature (WithWarm MiniProtocolSuccess {}) ->
            closePeerConnection pch `catches` handlers
          WithSomeProtocolTemperature (WithEstablished MiniProtocolSuccess {}) ->
            closePeerConnection pch `catches` handlers

      where
        -- 'closePeerConnection' and 'deactivatePeerConnection' actions can
        -- throw exceptions, but they maintain consistency of 'peerStateVar',
        -- that's why these handlers are trivial.
        handlers :: [Handler m ()]
        handlers =
          [ Handler (\(_ :: PeerSelectionActionException)               -> pure ()),
            Handler (\(_ :: EstablishConnectionException versionNumber) -> pure ()),
            Handler (\(_ :: PeerSelectionTimeoutException peerAddr)     -> pure ())
          ]



    establishPeerConnection :: JobPool () m (Maybe SomeException)
                            -> peerAddr
                            -> m (PeerConnectionHandle muxMode peerAddr ByteString m a b)
    establishPeerConnection jobPool remotePeerAddr =
      -- Protect consistency of the peer state with 'bracketOnError' if
      -- opening a connection fails.
      bracketOnError
        (newTVarIO PromotingToWarm)
        (\peerStateVar -> atomically $ writeTVar peerStateVar (PeerStatus PeerCold))
        $ \peerStateVar -> do
          res <- requestOutboundConnection spsConnectionManager remotePeerAddr
          case res of
            Connected connectionId@ConnectionId { localAddress, remoteAddress }
                      _dataFlow
                      (Handle mux muxBundle controlMessageBundle) -> do

              atomically $ do
                writeTVar (projectBundle TokHot         controlMessageBundle) Terminate
                writeTVar (projectBundle TokWarm        controlMessageBundle) Continue
                writeTVar (projectBundle TokEstablished controlMessageBundle) Continue

              awaitVarBundle <- atomically $ mkAwaitVars muxBundle

              let connHandle =
                    PeerConnectionHandle {
                        pchConnectionId = connectionId,
                        pchPeerState    = peerStateVar,
                        pchMux          = mux,
                        pchAppHandles   = mkApplicationHandleBundle
                                            muxBundle
                                            controlMessageBundle
                                            awaitVarBundle
                      }

              startProtocols TokWarm connHandle
              startProtocols TokEstablished connHandle
              atomically $ writeTVar peerStateVar (PeerStatus PeerWarm)
              traceWith spsTracer (PeerStatusChanged
                                    (ColdToWarm
                                      (Just localAddress)
                                      remoteAddress))

              JobPool.forkJob jobPool
                              (Job (handleJust
                                     (\e -> case fromException e of
                                        Just SomeAsyncException {} -> Nothing
                                        Nothing -> Just e)
                                     (\e -> do
                                        traceWith spsTracer (PeerMonitoringError connectionId e)
                                        throwIO e)
                                     (peerMonitoringLoop connHandle $> Nothing))
                                   (return . Just)
                                   ()
                                   ("peerMonitoringLoop " ++ show remoteAddress))
              pure connHandle

            Disconnected _ Nothing ->
              -- Disconnected in 'TerminatingState' or 'TerminatedState' without
              -- an exception.
              throwIO $ userError "establishPeerConnection: Disconnected"
            Disconnected _ (Just reason) ->
              case reason of
                HandleHandshakeClientError err -> do
                  traceWith spsTracer (PeerStatusChangeFailure
                                        (ColdToWarm Nothing remotePeerAddr)
                                        HandshakeClientFailure)
                  throwIO (ClientException err)

                HandleHandshakeServerError err -> do
                  traceWith spsTracer (PeerStatusChangeFailure
                                        (ColdToWarm Nothing remotePeerAddr)
                                        HandshakeServerFailure)
                  throwIO (ServerException err)

                HandleError err -> do
                  traceWith spsTracer (PeerStatusChangeFailure
                                        (ColdToWarm Nothing remotePeerAddr )
                                        (HandleFailure err))
                  throwIO err
      where
        mkAwaitVars :: MuxBundle muxMode ByteString m a b
                    -> STM m (Bundle
                               (StrictTVar m
                                 (Map MiniProtocolNum
                                   (STM m (HasReturned a)))))
        mkAwaitVars = traverse f
          where
            f :: [MiniProtocol muxMode ByteString m a b]
              -> STM m (StrictTVar m
                         (Map MiniProtocolNum
                           (STM m (HasReturned a))))
            f = newTVar
              . Map.fromList
              . map (\MiniProtocol { miniProtocolNum } ->
                      ( miniProtocolNum
                      -- Initially none of the protocols is running; This will
                      -- shortly get updated for established and warm
                      -- protocols, since 'establishPeerConnection' runs
                      -- 'startProtocols'; for hot protocols this will be
                      -- updated once the peer is promoted to hot.
                      , pure NotRunning
                      ))


    -- 'monitorPeerConnection' is only used against established connections
    monitorPeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                          -> STM m PeerStatus
    monitorPeerConnection PeerConnectionHandle { pchPeerState } =
      getCurrentState <$> readTVar pchPeerState


    -- Take a warm peer and promote it to a hot one.
    -- NB when adding any operations that can block for an extended period of
    -- of time timeouts should be implemented here in the same way it is in
    -- establishPeerConnection and deactivatePeerConnection.
    activatePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                           -> m ()
    activatePeerConnection
        connHandle@PeerConnectionHandle {
            pchConnectionId,
            pchPeerState,
            pchAppHandles } = do
      -- quiesce warm peer protocols and set hot ones in 'Continue' mode.
      wasWarm <- atomically $ do
        -- if the peer is cold we can't activate it.
        notCold <- updateUnlessCold pchPeerState PromotingToHot
        when notCold $ do
          writeTVar (getControlVar TokHot pchAppHandles) Continue
          writeTVar (getControlVar TokWarm pchAppHandles) Quiesce
        return notCold
      when (not wasWarm) $ do
        traceWith spsTracer (PeerStatusChangeFailure
                              (WarmToHot pchConnectionId)
                              ActiveCold)
        throwIO $ ColdActivationException pchConnectionId

      -- start hot peer protocols
      startProtocols TokHot connHandle

      -- Only set the status to PeerHot if the peer isn't PeerCold.
      -- This can happen asynchronously between the check above and now.
      wasWarm' <- atomically $ updateUnlessCold pchPeerState (PeerStatus PeerHot)
      if wasWarm'
         then traceWith spsTracer (PeerStatusChanged (WarmToHot pchConnectionId))
         else do
           traceWith spsTracer (PeerStatusChangeFailure
                                 (WarmToHot pchConnectionId)
                                 ActiveCold)
           throwIO $ ColdActivationException pchConnectionId


    -- Take a hot peer and demote it to a warm one.
    deactivatePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b -> m ()
    deactivatePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerState,
            pchMux,
            pchAppHandles
          } = do
      wasWarm <- atomically $ do
        notCold <- updateUnlessCold pchPeerState DemotingToWarm
        when notCold $ do
          writeTVar (getControlVar TokHot pchAppHandles) Terminate
          writeTVar (getControlVar TokWarm pchAppHandles) Continue
        return notCold
      when (not wasWarm) $ do
        -- The governor attempted to demote an already cold peer.
        traceWith spsTracer (PeerStatusChangeFailure
                             (HotToWarm pchConnectionId)
                             ActiveCold)
        throwIO $ ColdDeactivationException pchConnectionId


      -- Hot protocols should stop within 'spsDeactivateTimeout'.
      res <-
        timeout spsDeactivateTimeout
                (atomically $ awaitAllResults TokHot pchAppHandles)
      case res of
        Nothing -> do
          Mux.stopMux pchMux
          atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          traceWith spsTracer (PeerStatusChangeFailure
                                (HotToWarm pchConnectionId)
                                TimeoutError)
          throwIO (DeactivationTimeout pchConnectionId)

        Just (SomeErrored errs) -> do
          atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          traceWith spsTracer (PeerStatusChangeFailure
                                (HotToCold pchConnectionId)
                                (ApplicationFailure errs))
          throwIO (MiniProtocolExceptions errs)

        Just AllSucceeded -> do
          wasWarm' <- atomically $ do
            -- Only set the status to PeerWarm if the peer isn't PeerCold
            -- (can happen asynchronously).
            notCold <- updateUnlessCold pchPeerState (PeerStatus PeerWarm)
            when notCold $ do
              -- We need to update hot protocols to indicate that they are not
              -- running.
              stateTVar (getMiniProtocolsVar TokHot pchAppHandles)
                        (\a -> ( ()
                               , Map.map (const (pure NotRunning)) a
                               ))
            return notCold

          if wasWarm'
             then traceWith spsTracer (PeerStatusChanged (HotToWarm pchConnectionId))
             else do
                 traceWith spsTracer (PeerStatusChangeFailure
                                      (WarmToHot pchConnectionId)
                                      ActiveCold)
                 throwIO $ ColdDeactivationException pchConnectionId


    closePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                        -> m ()
    closePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerState,
            pchAppHandles
          } = do
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
                    -- note: we use last to finish on hot, warm and
                    -- established mini-protocols since 'closePeerConnection'
                    -- is also used by asynchronous demotions, not just
                    -- /warm → cold/ transition.
                    <$> awaitAllResults TokHot pchAppHandles
                    <*> awaitAllResults TokWarm pchAppHandles
                    <*> awaitAllResults TokEstablished pchAppHandles)
      case res of
        Nothing -> do
          -- timeout fired
          _ <- unregisterOutboundConnection spsConnectionManager (remoteAddress pchConnectionId)
          atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          traceWith spsTracer (PeerStatusChangeFailure
                                (WarmToCold pchConnectionId)
                                TimeoutError)

        Just (SomeErrored errs) -> do
          -- some mini-protocol errored
          _ <- unregisterOutboundConnection spsConnectionManager (remoteAddress pchConnectionId)
          atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          traceWith spsTracer (PeerStatusChangeFailure
                                (WarmToCold pchConnectionId)
                                (ApplicationFailure errs))
          throwIO (MiniProtocolExceptions errs)

        Just AllSucceeded -> do
          -- all mini-protocols terminated cleanly
          _ <- unregisterOutboundConnection spsConnectionManager (remoteAddress pchConnectionId)
          atomically (writeTVar pchPeerState (PeerStatus PeerCold))
          traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))

--
-- Utilities
--


-- | Smart constructor for 'ApplicationHandle'.
--
mkApplicationHandleBundle
    :: forall (muxMode :: MuxMode) bytes m a b.
       MuxBundle muxMode bytes m a b
    -- ^ mux application
    -> Bundle (StrictTVar m ControlMessage)
    -- ^ 'ControlMessage' bundle
    -> Bundle (StrictTVar m (Map MiniProtocolNum (STM m (HasReturned a))))
    -- ^ await for application termination
    -> Bundle (ApplicationHandle muxMode bytes m a b)
mkApplicationHandleBundle muxBundle controlMessageBundle awaitVarsBundle =
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
              ahApplication         = projectBundle tok muxBundle,
              ahControlVar          = projectBundle tok controlMessageBundle,
              ahMiniProtocolResults = projectBundle tok awaitVarsBundle
            }
      in case tok of
          TokHot -> WithHot app
          TokWarm -> WithWarm app
          TokEstablished -> WithEstablished app


-- | Given a singleton 'TokAppKind' and 'PeerConnectionHandle' start the mux
-- protocol bundle indicated by the type of the first argument.
--
startProtocols :: forall (muxMode :: MuxMode) (pt :: ProtocolTemperature) peerAddr m a b.
                  ( MonadAsync m
                  , MonadCatch m
                  , MonadThrow (STM m)
                  , HasInitiator muxMode ~ True
                  )
               => TokProtocolTemperature pt
               -> PeerConnectionHandle muxMode peerAddr ByteString m a b
               -> m ()
startProtocols tok PeerConnectionHandle { pchMux, pchAppHandles } = do
    let ptcls = getProtocols tok pchAppHandles
    as <- traverse runInitiator ptcls
    atomically $ writeTVar (getMiniProtocolsVar tok pchAppHandles)
                           (miniProtocolResults $ zip (miniProtocolNum `map` ptcls) as)
  where
    miniProtocolResults :: [(MiniProtocolNum, STM m (Either SomeException a))]
                        -> Map MiniProtocolNum (STM m (HasReturned a))
    miniProtocolResults = Map.map (fmap hasReturnedFromEither)
                        . Map.fromList

    runInitiator :: MiniProtocol muxMode ByteString m a b
                 -> m (STM m (Either SomeException a))
    runInitiator MiniProtocol {
                      miniProtocolNum,
                      miniProtocolRun
                    } = do

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
    | HandleFailure !SomeException
    | MuxStoppedFailure
    | TimeoutError
    | ActiveCold
    | ApplicationFailure ![MiniProtocolException]
  deriving Show

-- | All transitions.
--
data PeerStatusChangeType peerAddr =
    -- | During the 'ColdToWarm' transition we have the remote address, and only
    -- if establishing connection (establishing bearer & handshake negotiation)
    -- is successful we have access to full `ConnectionId`.
      ColdToWarm
        !(Maybe peerAddr) -- ^ local peer address
        !peerAddr         -- ^ remote peer address
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
    | PeerMonitoringResult    !(ConnectionId peerAddr) !(WithSomeProtocolTemperature FirstToFinishResult)
  deriving Show
