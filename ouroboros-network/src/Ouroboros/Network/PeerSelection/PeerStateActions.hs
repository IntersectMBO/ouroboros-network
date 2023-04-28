{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

-- 'startProtocols' is using 'HasInitiator' constraint to limit pattern
-- matches.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Ouroboros.Network.PeerSelection.PeerStateActions
  ( -- * Introduction
    -- $doc
    -- * Create PeerStateActions
    PeerStateActionsArguments (..)
  , PeerConnectionHandle
  , withPeerStateActions
  , pchPeerSharing
    -- * Exceptions
  , PeerSelectionActionException (..)
  , EstablishConnectionException (..)
  , PeerSelectionTimeoutException (..)
  , MonitorPeerConnectionBlocked (..)
    -- * Trace
  , PeerSelectionActionsTrace (..)
  , PeerStatusChangeType (..)
  , FailureType (..)
  ) where

import           Control.Applicative (Alternative)
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (SomeAsyncException (..))
import           Control.Monad (when, (<=<))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer.SI

import           Control.Concurrent.JobPool (Job (..), JobPool)
import qualified Control.Concurrent.JobPool as JobPool
import           Control.Tracer (Tracer, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (void, ($>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable, cast)

import qualified Network.Mux as Mux

import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.ExitPolicy
import           Ouroboros.Network.Mux
import           Ouroboros.Network.PeerSelection.Governor
                     (PeerStateActions (..))
import           Ouroboros.Network.Protocol.Handshake (HandshakeException)

import           Ouroboros.Network.ConnectionHandler (Handle (..),
                     HandleError (..), MuxConnectionManager)
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import           Ouroboros.Network.PeerSelection.Types (PeerStatus (..))

-- $doc
--
-- This module implements 'withPeerStateActions', giving the user access to the
-- 'PeerStateActions' API, which provides the following capabilities:
--
--   [synchronous promotions / demotions]:
--        * 'establishPeerConnection'
--        * 'activatePeerConnection'
--        * 'deactivatePeerConnection'
--        * 'closePeerConnection'
--
--   [monitoring]: 'monitorPeerConnection' - returns the state of the connection.
--
--   [asynchronous demotions]: happens when a mini-protocol terminates or errors.
--
-- = Synchronous promotions / demotions
--
-- Synchronous promotions / demotions are used by
-- 'Ouroboros.Network.PeerSelection.Governor.peerSelectionGovernor'.
--
-- [synchronous /cold → warm/ transition]:
--    This transition starts with creating or reusing an inbound connection, do
--    handshake (functionality provided by connection manager), start
--    established and warm mini-protocols, start monitoring thread specified
--    below.
--
-- [synchronous /warm → hot/ transition]:
--    This transition quiesces warm protocols and starts hot protocols.  There
--    is no timeout to quiesce warm mini-protocols.  The tip-sample protocol
--    which is the only planned warm protocol has some states that have
--    a longer timeout when the remote peer has agency, but it does not
--    transfer much data.
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
-- mini-protocols either terminates or errors.  When a mini-protocol terminates
--
--    * if (mini-protocol was hot): trigger a synchronous /hot → warm/ transition.
--    * otherwise: close the connection.
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
-- and track their termination via an 'STM' interface.  Each connection has an
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
-- Below is a schematic illustration of function calls / threads and shared
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
-- >         PeerStateVar        - 'pchPeerStatus' 'TVar'
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
-- All of the thin boxes are writing to 'pchPeerStatus' variable; which is read
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
   --
   -- The argument is the return type of previous run.  We preserve it to not
   -- miss updating the reactivate delay.
  | NotRunning !(Either SomeException a)

    -- | A mini-protocol has never been started yet.
  | NotStarted

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

getControlVar :: SingProtocolTemperature pt
              -> TemperatureBundle (ApplicationHandle muxMode bytes m a b)
              -> StrictTVar m ControlMessage
getControlVar tok = ahControlVar . projectBundle tok

getProtocols :: SingProtocolTemperature pt
             -> TemperatureBundle (ApplicationHandle muxMode bytes m a b)
             -> [MiniProtocol muxMode bytes m a b]
getProtocols tok bundle = ahApplication (projectBundle tok bundle)

getMiniProtocolsVar :: SingProtocolTemperature pt
                    -> TemperatureBundle (ApplicationHandle muxMode bytes m a b)
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

    -- | A mini-protocols terminated successfully.
    --
    -- TODO: we should record the return value of a protocol: it is meaningful
    -- (for tracing).  But it requires more plumbing to be done: consensus
    -- applications, as we see them, return `()`!
    | MiniProtocolSuccess !MiniProtocolNum
  deriving Show

instance Semigroup FirstToFinishResult where
    err@MiniProtocolError{} <> _                       = err
    _ <> err@MiniProtocolError{}                       = err
    res@MiniProtocolSuccess{} <> MiniProtocolSuccess{} = res


-- | Await first result from any of any of the protocols which belongs to
-- the indicated bundle.
--
awaitFirstResult :: MonadSTM m
                 => SingProtocolTemperature pt
                 -> TemperatureBundle (ApplicationHandle muxMode bytes m a b)
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
      NotRunning _ -> retry
      NotStarted   -> retry


-- | Data structure used in last-to-finish synchronisation for all
-- mini-protocols of a given temperature (see 'awaitAllResults').
--
data LastToFinishResult a =
    -- | All mini protocols returned successfully.
    -- The map will contain results of all mini-protocols (of a given
    -- temperature).
    --
    AllSucceeded !(Map MiniProtocolNum a)

    -- | Some mini-protocols (of a given temperature) errored.
    --
  | SomeErrored  ![MiniProtocolException]

instance Semigroup (LastToFinishResult a) where
    AllSucceeded a  <> AllSucceeded b  = AllSucceeded (a <> b)
    e@SomeErrored{} <> AllSucceeded{}  = e
    AllSucceeded{}  <> e@SomeErrored{} = e
    SomeErrored e   <> SomeErrored e'  = SomeErrored (e ++ e')

instance Monoid (LastToFinishResult a) where
    mempty = AllSucceeded mempty


-- | Last to finish synchronisation for mini-protocols of a given protocol
-- temperature.
--
awaitAllResults :: MonadSTM m
                => SingProtocolTemperature pt
                -> TemperatureBundle (ApplicationHandle muxMude bytes m a b)
                -> STM m (LastToFinishResult a)
awaitAllResults tok bundle = do
    results <-  readTVar (getMiniProtocolsVar tok bundle)
            >>= sequence
    return $ Map.foldMapWithKey
               (\num r -> case r of
                          Errored  e           -> SomeErrored [MiniProtocolException num e]
                          Returned a           -> AllSucceeded (Map.singleton num a)
                          NotRunning (Right a) -> AllSucceeded (Map.singleton num a)
                          NotRunning (Left e)  -> SomeErrored [MiniProtocolException num e]
                          NotStarted           -> AllSucceeded mempty)
               results


--
-- Internals: peer state & connection handle
--

-- |  Each established connection has access to 'PeerConnectionHandle'.  It
-- allows to promote / demote or close the connection, by having access to
-- 'Mux', three bundles of miniprotocols: for hot, warm and established peers
-- together with their state 'StrictTVar's.
--
data PeerConnectionHandle (muxMode :: MuxMode) peerAddr versionData bytes m a b = PeerConnectionHandle {
    pchConnectionId :: ConnectionId peerAddr,
    pchPeerStatus   :: StrictTVar m PeerStatus,
    pchMux          :: Mux.Mux muxMode m,
    pchAppHandles   :: TemperatureBundle (ApplicationHandle muxMode bytes m a b),
    pchVersionData  :: !versionData
  }

instance (Show peerAddr, Show versionData)
      => Show (PeerConnectionHandle muxMode peerAddr versionData bytes m a b) where
    show PeerConnectionHandle { pchConnectionId, pchVersionData } =
      "PeerConnectionHandle " ++ show pchConnectionId ++ " " ++ show pchVersionData

pchPeerSharing :: (versionData -> PeerSharing)
               -> PeerConnectionHandle muxMode peerAddr versionData bytes m a b
               -> PeerSharing
pchPeerSharing f = f . pchVersionData

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

-- | Throw an exception when 'monitorPeerConnection' blocks.
--
data MonitorPeerConnectionBlocked = MonitorPeerConnectionBlocked
  deriving Show

instance Exception MonitorPeerConnectionBlocked

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
data PeerStateActionsArguments muxMode socket peerAddr versionData versionNumber m a b =
    PeerStateActionsArguments {

      spsTracer                 :: Tracer m (PeerSelectionActionsTrace peerAddr versionNumber),

      -- | Peer deactivation timeout: timeouts stopping hot protocols.
      --
      spsDeactivateTimeout      :: DiffTime,

      -- | Timeout on closing connection: timeouts stopping established and warm
      -- peer protocols.
      --
      spsCloseConnectionTimeout :: DiffTime,

      spsConnectionManager      :: MuxConnectionManager muxMode socket peerAddr
                                                      versionData versionNumber
                                                      ByteString m a b,

      spsExitPolicy             :: ExitPolicy a
    }


withPeerStateActions
    :: forall (muxMode :: MuxMode) socket peerAddr versionData versionNumber m a b x.
       ( Alternative (STM m)
       , MonadAsync         m
       , MonadCatch         m
       , MonadLabelledSTM   m
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
    => PeerStateActionsArguments muxMode socket peerAddr versionData versionNumber m a b
    -> (PeerStateActions
          peerAddr
          (PeerConnectionHandle muxMode peerAddr versionData ByteString m a b)
          m
          -> m x)
    -> m x

withPeerStateActions PeerStateActionsArguments {
                       spsDeactivateTimeout,
                       spsCloseConnectionTimeout,
                       spsTracer,
                       spsConnectionManager,
                       spsExitPolicy
                     }
                     k =
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
    updateUnlessCold :: StrictTVar m PeerStatus -> PeerStatus -> STM m Bool
    updateUnlessCold stateVar newState = do
      status <- readTVar stateVar
      if status == PeerCold
         then return False
         else writeTVar stateVar newState >> return True

    isNotCold :: StrictTVar m PeerStatus -> STM m Bool
    isNotCold stateVar =
      (/= PeerCold) <$> readTVar stateVar


    peerMonitoringLoop
      :: PeerConnectionHandle muxMode peerAddr versionData ByteString m a b
      -> m ()
    peerMonitoringLoop pch@PeerConnectionHandle { pchConnectionId, pchPeerStatus, pchAppHandles } = do
        -- A first-to-finish synchronisation on all the bundles; As a result
        -- this is a first-to-finish synchronisation between all the
        -- mini-protocols runs toward the given peer.
        r <-
          atomically $
            (WithSomeProtocolTemperature . WithEstablished
              <$> awaitFirstResult SingEstablished pchAppHandles)
          `orElse`
            (WithSomeProtocolTemperature . WithWarm
              <$> awaitFirstResult SingWarm pchAppHandles)
          `orElse`
            (WithSomeProtocolTemperature . WithHot
              <$> awaitFirstResult SingHot pchAppHandles)

        traceWith spsTracer (PeerMonitoringResult pchConnectionId r)
        case r of
          --
          -- Errors in a protocol thread (asynchronous demotions to cold state)
          --
          -- On error, the multiplexer closes the bearer, we take advantage of
          -- it here.
          --
          -- we don't need to update connection manager; the connection handler
          -- thread terminated abruptly and the connection state will be
          -- updated by the finally handler of a connection handler.
          --

          WithSomeProtocolTemperature (WithHot MiniProtocolError{}) -> do
            -- current `pchPeerStatus` must be 'HotPeer'
            traceWith spsTracer (PeerStatusChanged (HotToCold pchConnectionId))
            void $ atomically (updateUnlessCold pchPeerStatus PeerCold)
          WithSomeProtocolTemperature (WithWarm MiniProtocolError{}) -> do
            -- current `pchPeerStatus` must be 'WarmPeer'
            traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))
            void $ atomically (updateUnlessCold pchPeerStatus PeerCold)
          WithSomeProtocolTemperature (WithEstablished MiniProtocolError{}) -> do
            -- update 'pchPeerStatus' and log (as the two other transition to
            -- cold state.
            state <- atomically $ do
              peerState <- readTVar pchPeerStatus
              _  <- updateUnlessCold pchPeerStatus PeerCold
              pure peerState
            case state of
              PeerCold -> return ()
              PeerWarm -> traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))
              PeerHot  -> traceWith spsTracer (PeerStatusChanged (HotToCold pchConnectionId))

          --
          -- Successful termination
          --

          -- A /hot/ protocol terminated, we deactivate the connection and keep
          -- monitoring /warm/ and /established/ protocols.
          WithSomeProtocolTemperature (WithHot MiniProtocolSuccess {}) -> do
            deactivatePeerConnection pch
            peerMonitoringLoop pch

          -- If an /established/ or /warm/ we demote the peer to 'PeerCold'.
          -- Warm protocols are quiesced when a peer becomes hot, but never
          -- terminated by 'PeerStateActions' (with the obvious exception of
          -- 'closePeerConnection'); also, established mini-protocols are not
          -- supposed to terminate (unless the remote peer did something
          -- wrong).
          WithSomeProtocolTemperature (WithWarm MiniProtocolSuccess {}) ->
            closePeerConnection pch
          WithSomeProtocolTemperature (WithEstablished MiniProtocolSuccess {}) ->
            closePeerConnection pch



    establishPeerConnection :: JobPool () m (Maybe SomeException)
                            -> peerAddr
                            -> m (PeerConnectionHandle muxMode peerAddr versionData ByteString m a b)
    establishPeerConnection jobPool remotePeerAddr =
      -- Protect consistency of the peer state with 'bracketOnError' if
      -- opening a connection fails.
      bracketOnError
        (newTVarIO PeerCold)
        (\peerStateVar -> atomically $ writeTVar peerStateVar PeerCold)
        $ \peerStateVar -> do
          res <- requestOutboundConnection spsConnectionManager remotePeerAddr
          case res of
            Connected connectionId@ConnectionId { localAddress, remoteAddress }
                      _dataFlow
                      (Handle mux muxBundle controlMessageBundle versionData) -> do

              atomically $ do
                writeTVar (projectBundle SingHot         controlMessageBundle) Terminate
                writeTVar (projectBundle SingWarm        controlMessageBundle) Continue
                writeTVar (projectBundle SingEstablished controlMessageBundle) Continue

              awaitVarBundle <- atomically $ mkAwaitVars muxBundle

              let connHandle =
                    PeerConnectionHandle {
                        pchConnectionId = connectionId,
                        pchPeerStatus   = peerStateVar,
                        pchMux          = mux,
                        pchAppHandles   = mkApplicationHandleBundle
                                            muxBundle
                                            controlMessageBundle
                                            awaitVarBundle,
                        pchVersionData  = versionData
                      }

              startProtocols SingWarm connHandle
              startProtocols SingEstablished connHandle
              atomically $ writeTVar peerStateVar PeerWarm
              traceWith spsTracer (PeerStatusChanged
                                    (ColdToWarm
                                      (Just localAddress)
                                      remoteAddress))

              JobPool.forkJob jobPool
                              (Job (handleJust
                                     (\e -> case fromException e of
                                        Just SomeAsyncException {} -> Nothing
                                        Nothing                    -> Just e)
                                     (\e -> do
                                        traceWith spsTracer (PeerMonitoringError connectionId e)
                                        throwIO e)
                                     (peerMonitoringLoop connHandle $> Nothing))
                                   (return . Just)
                                   ()  -- unit group, not using JobPool to group jobs.
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
                                        (HandshakeClientFailure err))
                  throwIO (ClientException err)

                HandleHandshakeServerError err -> do
                  traceWith spsTracer (PeerStatusChangeFailure
                                        (ColdToWarm Nothing remotePeerAddr)
                                        (HandshakeServerFailure err))
                  throwIO (ServerException err)

                HandleError err -> do
                  traceWith spsTracer (PeerStatusChangeFailure
                                        (ColdToWarm Nothing remotePeerAddr )
                                        (HandleFailure err))
                  throwIO err
      where
        mkAwaitVars :: MuxBundle muxMode ByteString m a b
                    -> STM m (TemperatureBundle
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
                      , pure NotStarted
                      ))


    -- 'monitorPeerConnection' is only used against established connections.
    -- It returns 'Nothing' only if all mini-protocols are either not running
    -- or still executing.
    --
    monitorPeerConnection :: PeerConnectionHandle muxMode peerAddr versionData ByteString m a b
                          -> STM m (PeerStatus, Maybe ReconnectDelay)
    monitorPeerConnection PeerConnectionHandle { pchPeerStatus, pchAppHandles } =
        (,) <$> readTVar pchPeerStatus
            <*> (g <$> traverse f pchAppHandles)
        `orElse` throwSTM MonitorPeerConnectionBlocked
      where
        f :: ApplicationHandle muxMode ByteString m a b
          -> STM m (Map MiniProtocolNum (Maybe (HasReturned a)))
             -- do not block when a mini-protocol is still running, otherwise
             -- outbound governor
             -- `Ouroboros.Network.PeerSelection.Governor.Monitor.connections`
             -- will not be able to get the 'PeerStatus' of all peers.
        f =  traverse (((\stm -> (Just <$> stm) `orElse` pure Nothing)))
         <=< readTVar . ahMiniProtocolResults

        g :: TemperatureBundle (Map MiniProtocolNum (Maybe (HasReturned a)))
          -> Maybe ReconnectDelay
        g = foldMap (foldMap h)

        h :: Maybe (HasReturned a) -> Maybe ReconnectDelay
        h (Just (Returned a)) = Just $ epReturnDelay spsExitPolicy a
        -- Note: we do 'RethrowPolicy' in 'ConnectionHandler' (see
        -- 'makeConnectionHandler').
        h (Just Errored {})     = Just $ epErrorDelay spsExitPolicy
        h (Just (NotRunning a)) = case a of
                                    Left {} -> Just $ epErrorDelay spsExitPolicy
                                    Right b -> Just $ epReturnDelay spsExitPolicy b
        h (Just NotStarted)     = Nothing
        h Nothing               = Nothing


    -- Take a warm peer and promote it to a hot one.
    -- NB when adding any operations that can block for an extended period of
    -- of time timeouts should be implemented here in the same way it is in
    -- establishPeerConnection and deactivatePeerConnection.
    activatePeerConnection :: PeerConnectionHandle muxMode peerAddr versionData ByteString m a b
                           -> m ()
    activatePeerConnection
        connHandle@PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchAppHandles } = do
      -- quiesce warm peer protocols and set hot ones in 'Continue' mode.
      wasWarm <- atomically $ do
        -- if the peer is cold we can't activate it.
        notCold <- isNotCold pchPeerStatus
        when notCold $ do
          writeTVar (getControlVar SingHot pchAppHandles) Continue
          writeTVar (getControlVar SingWarm pchAppHandles) Quiesce
        return notCold
      when (not wasWarm) $ do
        traceWith spsTracer (PeerStatusChangeFailure
                              (WarmToHot pchConnectionId)
                              ActiveCold)
        throwIO $ ColdActivationException pchConnectionId

      -- start hot peer protocols
      startProtocols SingHot connHandle

      -- Only set the status to PeerHot if the peer isn't PeerCold.
      -- This can happen asynchronously between the check above and now.
      wasWarm' <- atomically $ updateUnlessCold pchPeerStatus PeerHot
      if wasWarm'
         then traceWith spsTracer (PeerStatusChanged (WarmToHot pchConnectionId))
         else do
           traceWith spsTracer (PeerStatusChangeFailure
                                 (WarmToHot pchConnectionId)
                                 ActiveCold)
           throwIO $ ColdActivationException pchConnectionId


    -- Take a hot peer and demote it to a warm one.
    deactivatePeerConnection :: PeerConnectionHandle muxMode peerAddr versionData ByteString m a b -> m ()
    deactivatePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchMux,
            pchAppHandles
          } = do
      wasCold <- atomically $ do
        notCold <- isNotCold pchPeerStatus
        when notCold $ do
          writeTVar (getControlVar SingHot pchAppHandles) Terminate
          writeTVar (getControlVar SingWarm pchAppHandles) Continue
        return (not notCold)
      when wasCold $ do
        -- The governor attempted to demote an already cold peer.
        traceWith spsTracer (PeerStatusChangeFailure
                             (HotToWarm pchConnectionId)
                             ActiveCold)
        throwIO $ ColdDeactivationException pchConnectionId


      -- Hot protocols should stop within 'spsDeactivateTimeout'.
      res <-
        timeout spsDeactivateTimeout
                (atomically $ awaitAllResults SingHot pchAppHandles)
      case res of
        Nothing -> do
          Mux.stopMux pchMux
          atomically (writeTVar pchPeerStatus PeerCold)
          traceWith spsTracer (PeerStatusChangeFailure
                                (HotToWarm pchConnectionId)
                                TimeoutError)
          throwIO (DeactivationTimeout pchConnectionId)

        -- some of the hot mini-protocols errored
        Just (SomeErrored errs) -> do
          -- we don't need to notify the connection manager, we can instead
          -- relay on mux property: if any of the mini-protocols errors, mux
          -- throws an exception as well.
          atomically (writeTVar pchPeerStatus PeerCold)
          traceWith spsTracer (PeerStatusChangeFailure
                                (HotToCold pchConnectionId)
                                (ApplicationFailure errs))
          throwIO (MiniProtocolExceptions errs)

        -- all hot mini-protocols succeeded
        Just (AllSucceeded results) -> do
          -- we don't notify the connection manager as this connection is still
          -- useful to the outbound governor (warm peer).
          wasWarm <- atomically $ do
            -- Only set the status to PeerWarm if the peer isn't PeerCold
            -- (can happen asynchronously).
            notCold <- updateUnlessCold pchPeerStatus PeerWarm
            when notCold $ do
              -- We need to update hot protocols to indicate that they are not
              -- running. Preserve the results returned by their previous
              -- execution.
              modifyTVar (getMiniProtocolsVar SingHot pchAppHandles)
                         (\_ -> Map.map (pure . NotRunning . Right) results)
            return notCold

          if wasWarm
             then traceWith spsTracer (PeerStatusChanged (HotToWarm pchConnectionId))
             else do
                 traceWith spsTracer (PeerStatusChangeFailure
                                      (WarmToHot pchConnectionId)
                                      ActiveCold)
                 throwIO $ ColdDeactivationException pchConnectionId


    closePeerConnection :: PeerConnectionHandle muxMode peerAddr versionData ByteString m a b
                        -> m ()
    closePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchAppHandles,
            pchMux
          } = do
      atomically $ do
        writeTVar (getControlVar SingWarm pchAppHandles) Terminate
        writeTVar (getControlVar SingEstablished pchAppHandles) Terminate
        writeTVar (getControlVar SingHot pchAppHandles) Terminate

      res <-
        timeout spsCloseConnectionTimeout
                (atomically $
                  (\a b c -> a <> b <> c)
                    -- note: we use last to finish on hot, warm and
                    -- established mini-protocols since 'closePeerConnection'
                    -- is also used by asynchronous demotions, not just
                    -- /warm → cold/ transition.
                    <$> awaitAllResults SingHot pchAppHandles
                    <*> awaitAllResults SingWarm pchAppHandles
                    <*> awaitAllResults SingEstablished pchAppHandles)
      case res of
        Nothing -> do
          -- timeout fired
          Mux.stopMux pchMux
          atomically (writeTVar pchPeerStatus PeerCold)
          traceWith spsTracer (PeerStatusChangeFailure
                                (WarmToCold pchConnectionId)
                                TimeoutError)

        Just (SomeErrored errs) -> do
          -- some mini-protocol errored
          --
          -- we don't need to notify the connection manager, we can instead
          -- rely on mux property: if any of the mini-protocols errors, mux
          -- throws an exception as well.
          atomically (writeTVar pchPeerStatus PeerCold)
          traceWith spsTracer (PeerStatusChangeFailure
                                (WarmToCold pchConnectionId)
                                (ApplicationFailure errs))
          throwIO (MiniProtocolExceptions errs)

        Just AllSucceeded {} -> do
          -- all mini-protocols terminated cleanly
          --
          -- 'unregisterOutboundConnection' could only fail to demote the peer if
          -- connection manager would simultaneously promote it, but this is not
          -- possible.
          _ <- unregisterOutboundConnection spsConnectionManager (remoteAddress pchConnectionId)
          atomically (writeTVar pchPeerStatus PeerCold)
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
    -> TemperatureBundle (StrictTVar m ControlMessage)
    -- ^ 'ControlMessage' bundle
    -> TemperatureBundle (StrictTVar m (Map MiniProtocolNum (STM m (HasReturned a))))
    -- ^ await for application termination
    -> TemperatureBundle (ApplicationHandle muxMode bytes m a b)
mkApplicationHandleBundle muxBundle controlMessageBundle awaitVarsBundle =
    TemperatureBundle
      (mkApplication SingHot)
      (mkApplication SingWarm)
      (mkApplication SingEstablished)
  where
    mkApplication :: SingProtocolTemperature pt
                  -> WithProtocolTemperature pt (ApplicationHandle muxMode bytes m a b)
    mkApplication tok =
      let app =
            ApplicationHandle {
              ahApplication         = projectBundle tok muxBundle,
              ahControlVar          = projectBundle tok controlMessageBundle,
              ahMiniProtocolResults = projectBundle tok awaitVarsBundle
            }
      in case tok of
          SingHot         -> WithHot app
          SingWarm        -> WithWarm app
          SingEstablished -> WithEstablished app


-- | Given a singleton 'SingProtocolTemperature' and 'PeerConnectionHandle' start the mux
-- protocol bundle indicated by the type of the first argument.
--
startProtocols :: forall (muxMode :: MuxMode) (pt :: ProtocolTemperature) peerAddr versionData m a b.
                  ( Alternative (STM m)
                  , MonadAsync m
                  , MonadCatch m
                  , MonadThrow (STM m)
                  , HasInitiator muxMode ~ True
                  )
               => SingProtocolTemperature pt
               -> PeerConnectionHandle muxMode peerAddr versionData ByteString m a b
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
data FailureType versionNumber =
      HandshakeClientFailure !(HandshakeException versionNumber)
    | HandshakeServerFailure !(HandshakeException versionNumber)
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
data PeerSelectionActionsTrace peerAddr vNumber =
      PeerStatusChanged       (PeerStatusChangeType peerAddr)
    | PeerStatusChangeFailure (PeerStatusChangeType peerAddr) (FailureType vNumber)
    | PeerMonitoringError     (ConnectionId peerAddr) SomeException
    | PeerMonitoringResult    (ConnectionId peerAddr) (WithSomeProtocolTemperature FirstToFinishResult)
  deriving Show
