{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
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
  , getPromotedHotTime
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

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData)
import Control.Exception (SomeAsyncException (..), assert)
import Control.Monad (join, when, (<=<))
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI

import Control.Concurrent.JobPool (Job (..), JobPool)
import Control.Concurrent.JobPool qualified as JobPool
import Control.Tracer (Tracer, traceWith)

import Data.ByteString.Lazy (ByteString)
import Data.Functor (void, ($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Typeable (Typeable, cast)

import Network.Mux qualified as Mux

import Ouroboros.Network.Context
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.DiffusionMode
import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.Mux
import Ouroboros.Network.PeerSelection.Governor.Types (PeerStateActions (..))
import Ouroboros.Network.Protocol.Handshake (HandshakeException)
import Ouroboros.Network.RethrowPolicy

import Ouroboros.Network.ConnectionHandler (Handle (..), HandlerError (..),
           MuxConnectionManager)
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.Types (PeerStatus (..))

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
  deriving Show

instance Exception MiniProtocolExceptions


-- | Application Handle which allows to stop or start mux threads.  This only
-- contains information which depends on peer temperature.
--
-- TODO: only for hot applications we need 'ahApplication', we never restart
-- / stop the other ones!
data ApplicationHandle muxMode responderCtx peerAddr extraFlags bytes m a b = ApplicationHandle {
    -- | List of applications for the given peer temperature.
    --
    ahApplication         :: [MiniProtocol muxMode (ExpandedInitiatorContext peerAddr extraFlags m)
                                                   responderCtx bytes m a b],

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
              -> TemperatureBundle (ApplicationHandle muxMode responderCtx peerAddr extraFlags bytes m a b)
              -> StrictTVar m ControlMessage
getControlVar tok = ahControlVar . projectBundle tok

getProtocols :: SingProtocolTemperature pt
             -> TemperatureBundle (ApplicationHandle muxMode responderCtx peerAddr extraFlags bytes m a b)
             -> [MiniProtocol muxMode (ExpandedInitiatorContext peerAddr extraFlags m) responderCtx bytes m a b]
getProtocols tok bundle = ahApplication (projectBundle tok bundle)

getMiniProtocolsVar :: SingProtocolTemperature pt
                    -> TemperatureBundle (ApplicationHandle muxMode responderCtx peerAddr extraFlags bytes m a b)
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
                 -> TemperatureBundle (ApplicationHandle muxMode responderCtx peerAddr extraFlags bytes m a b)
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
                -> TemperatureBundle (ApplicationHandle muxMude responderCtx peerAddr extraFlags bytes m a b)
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
data PeerConnectionHandle (muxMode :: Mux.Mode) responderCtx peerAddr extraFlags versionData bytes m a b = PeerConnectionHandle {
    pchConnectionId    :: !(ConnectionId peerAddr),
    pchPeerStatus      :: !(StrictTVar m PeerStatus),
    pchMux             :: !(Mux.Mux muxMode m),
    pchAppHandles      :: !(TemperatureBundle (ApplicationHandle muxMode responderCtx peerAddr extraFlags bytes m a b)),
    pchVersionData     :: !versionData,
    pchPromotedHotVar  :: !(StrictTVar m (Maybe Time))
  }

-- | Retrieve the time the remote peer has been promoted to hot state
-- or Nothing if either the peer was not promoted or is being currently demoted
--
getPromotedHotTime :: (MonadSTM m)
                   => PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData bytes m a b
                   -> STM m (Maybe Time)
getPromotedHotTime PeerConnectionHandle { pchPromotedHotVar } =
  readTVar pchPromotedHotVar

mkInitiatorContext :: MonadSTM m
                   => SingProtocolTemperature pt
                   -> IsBigLedgerPeer
                   -> extraFlags
                   -> PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionDat bytes m a b
                   -> ExpandedInitiatorContext peerAddr extraFlags m
mkInitiatorContext tok isBigLedgerPeer extraFlags
                   PeerConnectionHandle {
                       pchConnectionId = connectionId,
                       pchAppHandles   = appHandles
                    }
                   =
                   ExpandedInitiatorContext {
                       eicConnectionId    = connectionId,
                       eicControlMessage  = readTVar (getControlVar tok appHandles),
                       eicIsBigLedgerPeer = isBigLedgerPeer,
                       eicExtraFlags      = extraFlags
                     }


instance (Show peerAddr, Show versionData)
      => Show (PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData bytes m a b) where
    show PeerConnectionHandle { pchConnectionId, pchVersionData } =
      "PeerConnectionHandle " ++ show pchConnectionId ++ " " ++ show pchVersionData

pchPeerSharing :: (versionData -> PeerSharing)
               -> PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData bytes m a b
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
data PeerStateActionsArguments muxMode socket responderCtx peerAddr extraFlags versionData versionNumber m a b =
    PeerStateActionsArguments {

      spsTracer                 :: Tracer m (PeerSelectionActionsTrace peerAddr versionNumber),

      -- | Peer deactivation timeout: timeouts stopping hot protocols.
      --
      spsDeactivateTimeout      :: DiffTime,

      -- | Timeout on closing connection: timeouts stopping established and warm
      -- peer protocols.
      --
      spsCloseConnectionTimeout :: DiffTime,

      spsConnectionManager      :: MuxConnectionManager muxMode socket
                                                        (ExpandedInitiatorContext peerAddr extraFlags m)
                                                        responderCtx peerAddr
                                                        versionData versionNumber
                                                        ByteString m a b,

      spsExitPolicy             :: ExitPolicy a,
      spsRethrowPolicy          :: RethrowPolicy,
      spsMainThreadId           :: ThreadId m
    }


withPeerStateActions
    :: forall (muxMode :: Mux.Mode) socket responderCtx peerAddr extraFlags versionData versionNumber m a b x.
       ( Alternative (STM m)
       , MonadAsync         m
       , MonadCatch         m
       , MonadLabelledSTM   m
       , MonadFork          m
       , MonadMask          m
       , MonadTimer         m
       , MonadThrow         (STM m)
       , HasInitiator muxMode ~ True
       , Typeable versionNumber
       , Show     versionNumber
       , Ord      peerAddr
       , Typeable peerAddr
       , Show     peerAddr
       , NFData a
       , NFData b
       )
    => PeerStateActionsArguments muxMode socket responderCtx peerAddr extraFlags versionData versionNumber m a b
    -> (PeerStateActions
          peerAddr
          extraFlags
          (PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData ByteString m a b)
          m
          -> m x)
    -> m x

withPeerStateActions PeerStateActionsArguments {
                       spsDeactivateTimeout,
                       spsCloseConnectionTimeout,
                       spsTracer,
                       spsConnectionManager,
                       spsExitPolicy,
                       spsRethrowPolicy,
                       spsMainThreadId
                     }
                     k =
    JobPool.withJobPool $ \jobPool ->
      k PeerStateActions {
          establishPeerConnection = establishPeerConnection jobPool,
          monitorPeerConnection,
          activatePeerConnection,
          deactivatePeerConnection,
          closePeerConnection = void . closePeerConnection,
          errorDelay = epErrorDelay spsExitPolicy
        }

  where

    -- Update PeerState with the new state only if the current state isn't
    -- cold. Returns True if the state wasn't cold
    updateUnlessCoolingOrCold :: StrictTVar m PeerStatus -> PeerStatus -> STM m Bool
    updateUnlessCoolingOrCold stateVar newState = do
      status <- readTVar stateVar
      if status <= PeerCooling
         then return False
         else writeTVar stateVar newState >> return True

    tracePeerHotDuration
      :: PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData bytes m a b
      -> m ()
    tracePeerHotDuration PeerConnectionHandle { pchConnectionId, pchPromotedHotVar } = do
      pchPromotedHot <- atomically $ stateTVar pchPromotedHotVar (, Nothing)
      case pchPromotedHot of
        Just t1 -> do
          dt <- (`diffTime` t1) <$> getMonotonicTime
          traceWith spsTracer (PeerHotDuration pchConnectionId dt)
        Nothing -> pure ()

    peerMonitoringLoop
      :: PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData ByteString m a b
      -> m ()
    peerMonitoringLoop pch@PeerConnectionHandle {
                             pchConnectionId,
                             pchPeerStatus,
                             pchAppHandles
                           } = do
        -- A first-to-finish synchronisation on all the bundles; As a result
        -- this is a first-to-finish synchronisation between all the
        -- mini-protocols runs toward the given peer.
        r <-
          atomically $ do
            peerStatus <- readTVar pchPeerStatus
            -- If we already monitored mux and are waiting for the peer connection
            -- to be cleaned then the peer status will be 'PeerCold' and we can't
            -- make progress until it is 'PeerReallyCold'
            case peerStatus of
              PeerCold ->
                return Nothing
              PeerCooling -> do
                waitForOutboundDemotion spsConnectionManager pchConnectionId
                writeTVar pchPeerStatus PeerCold
                return Nothing
              _ ->
                  (Just . WithSomeProtocolTemperature . WithEstablished
                    <$> awaitFirstResult SingEstablished pchAppHandles)
                `orElse`
                  (Just . WithSomeProtocolTemperature . WithWarm
                    <$> awaitFirstResult SingWarm pchAppHandles)
                `orElse`
                  (Just . WithSomeProtocolTemperature . WithHot
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

          Just (WithSomeProtocolTemperature (WithHot MiniProtocolError{})) -> do
            -- current `pchPeerStatus` must be 'HotPeer'
            state <- atomically $ do
              peerState <- readTVar pchPeerStatus
              _  <- updateUnlessCoolingOrCold pchPeerStatus PeerCooling
              return peerState
            case state of
              PeerCold    -> return ()
              PeerCooling -> return ()
              hotOrWarm -> assert (hotOrWarm == PeerHot) $
                           traceWith spsTracer (PeerStatusChanged (HotToCooling pchConnectionId))
            peerMonitoringLoop pch
          Just (WithSomeProtocolTemperature (WithWarm MiniProtocolError{})) -> do
            -- current `pchPeerStatus` must be 'WarmPeer'
            traceWith spsTracer (PeerStatusChanged (WarmToCooling pchConnectionId))
            void $ atomically (updateUnlessCoolingOrCold pchPeerStatus PeerCooling)
            peerMonitoringLoop pch
          Just (WithSomeProtocolTemperature (WithEstablished MiniProtocolError{})) -> do
            -- update 'pchPeerStatus' and log (as the two other transition to
            -- cold state.
            state <- atomically $ do
              peerState <- readTVar pchPeerStatus
              _  <- updateUnlessCoolingOrCold pchPeerStatus PeerCooling
              pure peerState
            case state of
              PeerCold    -> return ()
              PeerCooling -> return ()
              PeerWarm    -> traceWith spsTracer (PeerStatusChanged (WarmToCooling pchConnectionId))
              PeerHot     -> traceWith spsTracer (PeerStatusChanged (HotToCooling pchConnectionId))
            peerMonitoringLoop pch

          --
          -- Successful termination
          --

          -- A /hot/ protocol terminated, we deactivate the connection and keep
          -- monitoring /warm/ and /established/ protocols.
          Just (WithSomeProtocolTemperature (WithHot MiniProtocolSuccess {})) -> do
            deactivatePeerConnection pch
            peerMonitoringLoop pch

          -- If an /established/ or /warm/ we demote the peer to 'PeerCold'.
          -- Warm protocols are quiesced when a peer becomes hot, but never
          -- terminated by 'PeerStateActions' (with the obvious exception of
          -- 'closePeerConnection'); also, established mini-protocols are not
          -- supposed to terminate (unless the remote peer did something
          -- wrong).
          Just (WithSomeProtocolTemperature (WithWarm MiniProtocolSuccess {})) -> do
            _peerStatus <- closePeerConnection pch
            -- if peerStatus is `PeerCold`, in the next loop we'll log
            -- `CoolingToCold`; it's likely it is `PeerCooling` in which case
            -- we'll block until whole connection is demoted.
            peerMonitoringLoop pch
          Just (WithSomeProtocolTemperature (WithEstablished MiniProtocolSuccess {})) -> do
            _peerStatus <- closePeerConnection pch
            -- if peerStatus is `PeerCold`, in the next loop we'll log
            -- `CoolingToCold`; it's likely it is `PeerCooling` in which case
            -- we'll block until whole connection is demoted.
            peerMonitoringLoop pch

          --
          -- peerMonitingLoop exit
          --

          Nothing ->
            tracePeerHotDuration pch >>
            traceWith spsTracer (PeerStatusChanged (CoolingToCold pchConnectionId))

    establishPeerConnection :: JobPool () m (Maybe SomeException)
                            -> IsBigLedgerPeer
                            -> DiffusionMode
                            -> Provenance
                            -> peerAddr
                            -> extraFlags
                            -> m (PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData ByteString m a b)
    establishPeerConnection jobPool isBigLedgerPeer diffusionMode provenance remotePeerAddr extraFlags =
      -- Protect consistency of the peer state with 'bracketOnError' if
      -- opening a connection fails.
      bracketOnError
        (newTVarIO PeerCold)
        (\peerStateVar -> atomically $ writeTVar peerStateVar PeerCold)
        $ \peerStateVar -> do
          res <- try $ acquireOutboundConnection
                         spsConnectionManager
                         diffusionMode
                         remotePeerAddr
                         provenance
          case res of
            Left e -> do
              when (isNothing $ fromException @SomeAsyncException e) $
                traceWith spsTracer (AcquireConnectionError e)
              case runRethrowPolicy spsRethrowPolicy OutboundError e of
                ShutdownNode -> throwTo spsMainThreadId e
                             >> throwIO e
                ShutdownPeer -> throwIO e

            Right (Connected connId@ConnectionId { localAddress, remoteAddress }
                             _dataFlow
                            (Handle mux muxBundle controlMessageBundle versionData)) -> do

              atomically $ do
                writeTVar (projectBundle SingHot         controlMessageBundle) Terminate
                writeTVar (projectBundle SingWarm        controlMessageBundle) Continue
                writeTVar (projectBundle SingEstablished controlMessageBundle) Continue

              awaitVarBundle    <- atomically $ mkAwaitVars muxBundle
              pchPromotedHotVar <- newTVarIO Nothing

              let connHandle =
                    PeerConnectionHandle {
                        pchConnectionId = connId,
                        pchPeerStatus   = peerStateVar,
                        pchMux          = mux,
                        pchAppHandles   = mkApplicationHandleBundle
                                            muxBundle
                                            controlMessageBundle
                                            awaitVarBundle,
                        pchVersionData  = versionData,
                        pchPromotedHotVar
                      }

              startProtocols SingWarm isBigLedgerPeer extraFlags connHandle
              startProtocols SingEstablished isBigLedgerPeer extraFlags connHandle
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
                                        atomically do
                                          waitForOutboundDemotion spsConnectionManager connId
                                          writeTVar peerStateVar PeerCold
                                        tracePeerHotDuration connHandle
                                        traceWith spsTracer (PeerMonitoringError connId e)
                                        throwIO e)
                                     (peerMonitoringLoop connHandle $> Nothing))
                                   (return . Just)
                                   ()  -- unit group, not using JobPool to group jobs.
                                   ("peerMonitoringLoop " ++ show remoteAddress))
              pure connHandle

            Right (Disconnected _ disconnectionError) ->
              case disconnectionError of
                ConnectionHandlerError handlerError ->
                  case handlerError of
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

                    HandlerError err -> do
                      traceWith spsTracer (PeerStatusChangeFailure
                                            (ColdToWarm Nothing remotePeerAddr )
                                            (HandleFailure err))
                      throwIO err

                _ -> throwIO disconnectionError
      where
        mkAwaitVars :: OuroborosBundle muxMode (ExpandedInitiatorContext peerAddr extraFlags m)
                                               responderCtx ByteString m a b
                    -> STM m (TemperatureBundle
                               (StrictTVar m
                                 (Map MiniProtocolNum
                                   (STM m (HasReturned a)))))
        mkAwaitVars = traverse f
          where
            f :: [MiniProtocol muxMode (ExpandedInitiatorContext peerAddr extraFlags m)
                                       responderCtx ByteString m a b]
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
    monitorPeerConnection :: PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData ByteString m a b
                          -> STM m (PeerStatus, Maybe RepromoteDelay)
    monitorPeerConnection PeerConnectionHandle { pchPeerStatus, pchAppHandles } =
         p  <$> readTVar pchPeerStatus
            <*> (g <$> traverse f pchAppHandles)
        `orElse` throwSTM MonitorPeerConnectionBlocked
      where
        f :: ApplicationHandle muxMode responderCtx peerAddr extraFlags ByteString m a b
          -> STM m (Map MiniProtocolNum (Maybe (HasReturned a)))
             -- do not block when a mini-protocol is still running, otherwise
             -- outbound governor
             -- `Ouroboros.Network.PeerSelection.Governor.Monitor.connections`
             -- will not be able to get the 'PeerStatus' of all peers.
        f =  traverse (\stm -> (Just <$> stm) `orElse` pure Nothing)
         <=< readTVar . ahMiniProtocolResults

        g :: TemperatureBundle (Map MiniProtocolNum (Maybe (HasReturned a)))
          -> Maybe RepromoteDelay
        g = foldMap (foldMap h)

        h :: Maybe (HasReturned a) -> Maybe RepromoteDelay
        h (Just (Returned a)) = Just $ epReturnDelay spsExitPolicy a
        -- Note: we do 'RethrowPolicy' in 'ConnectionHandler' (see
        -- 'makeConnectionHandler').
        h (Just Errored {})     = Just $ epErrorDelay spsExitPolicy
        h (Just (NotRunning a)) = case a of
                                    Left {} -> Just $ epErrorDelay spsExitPolicy
                                    Right b -> Just $ epReturnDelay spsExitPolicy b
        h (Just NotStarted)     = Nothing
        h Nothing               = Nothing

        -- the delay in the `PeerCooling` state is ignored, let's make it
        -- explicit.
        p :: PeerStatus
          -> Maybe RepromoteDelay
          -> (PeerStatus, Maybe RepromoteDelay)
        p st@PeerCooling _ = (st, Nothing)
        p st delay         = (st, delay)


    -- Take a warm peer and promote it to a hot one.
    -- NB when adding any operations that can block for an extended period of
    -- of time timeouts should be implemented here in the same way it is in
    -- establishPeerConnection and deactivatePeerConnection.
    activatePeerConnection :: IsBigLedgerPeer
                           -> extraFlags
                           -> PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData ByteString m a b
                           -> m ()
    activatePeerConnection
        isBigLedgerPeer
        extraFlags
        connHandle@PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchAppHandles,
            pchPromotedHotVar } = do
      join . atomically $ do
        peerStatus <- readTVar pchPeerStatus
        case peerStatus of
          PeerWarm -> do
            writeTVar (getControlVar SingHot pchAppHandles) Continue
            writeTVar (getControlVar SingWarm pchAppHandles) Quiesce
            writeTVar pchPeerStatus PeerHot
            return $ pure ()

          _otherwise -> return do
            traceWith spsTracer (PeerStatusChangeFailure
                                  (WarmToHot pchConnectionId)
                                  (ActiveCold peerStatus))
            throwIO $ ColdActivationException pchConnectionId

      startProtocols SingHot isBigLedgerPeer extraFlags connHandle
      atomically . writeTVar pchPromotedHotVar . (Just $!) =<< getMonotonicTime
      traceWith spsTracer (PeerStatusChanged (WarmToHot pchConnectionId))


    -- Take a hot peer and demote it to a warm one.
    -- this can be raced by 'peerMonitoringLoop' and peer selection demotion activity
    deactivatePeerConnection :: PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData ByteString m a b -> m ()
    deactivatePeerConnection
        pch@PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchMux,
            pchAppHandles
          } = do
      join . atomically $ do
        peerStatus <- readTVar pchPeerStatus
        case peerStatus of
          PeerHot -> do
            writeTVar (getControlVar SingHot pchAppHandles) Terminate
            writeTVar (getControlVar SingWarm pchAppHandles) Continue
            return do
              -- Hot protocols should stop within 'spsDeactivateTimeout'.
              res <-
                timeout spsDeactivateTimeout
                      $ join . atomically $ do
                          res <- awaitAllResults SingHot pchAppHandles
                          case res of
                            AllSucceeded results -> do
                              modifyTVar (getMiniProtocolsVar SingHot pchAppHandles)
                                         (\_ -> Map.map (pure . NotRunning . Right) results)
                              stateTVar pchPeerStatus \case
                                PeerHot -> (  traceWith spsTracer (PeerStatusChanged
                                                                    (HotToWarm pchConnectionId))
                                           ,  PeerWarm)
                                x       -> (pure () , x)
                            SomeErrored errs ->
                              stateTVar pchPeerStatus \status ->
                                if status <= PeerCooling then
                                     (throwIO (MiniProtocolExceptions errs), status)
                                else (  traceWith spsTracer (PeerStatusChangeFailure
                                                              (HotToCooling pchConnectionId)
                                                              (ApplicationFailure errs))
                                        >> throwIO (MiniProtocolExceptions errs)
                                     ,  PeerCooling)

              case res of
                Nothing -> do
                  Mux.stop pchMux
                  trace <- atomically $ updateUnlessCoolingOrCold pchPeerStatus PeerCooling
                  when trace do
                    traceWith spsTracer (PeerStatusChangeFailure
                                          (HotToCooling pchConnectionId)
                                          TimeoutError)
                  throwIO (DeactivationTimeout pchConnectionId)
                Just _ -> tracePeerHotDuration pch

          -- we could genuinly hit this case due to a race between 'peerMonitoringLoop'
          -- and peer selection demotion job
          PeerWarm -> return $ pure ()

          _otherwise ->
            return $ do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (HotToWarm pchConnectionId)
                                    (ActiveCold peerStatus))
              throwIO $ ColdDeactivationException pchConnectionId


    closePeerConnection :: PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData ByteString m a b
                        -> m PeerStatus
    closePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchAppHandles,
            pchMux
          } = do
      peerStatus <- atomically do
        writeTVar (getControlVar SingWarm pchAppHandles) Terminate
        writeTVar (getControlVar SingEstablished pchAppHandles) Terminate
        writeTVar (getControlVar SingHot pchAppHandles) Terminate
        readTVar pchPeerStatus <* updateUnlessCoolingOrCold pchPeerStatus PeerCooling

      case peerStatus of
        PeerCooling -> return peerStatus
        PeerCold    -> return peerStatus
        _otherwise  -> do
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

          PeerCooling <$ case res of
            Nothing -> do
              -- timeout fired
              Mux.stop pchMux
              traceWith spsTracer (PeerStatusChangeFailure
                                    (WarmToCooling pchConnectionId)
                                    TimeoutError)

            Just (SomeErrored errs) -> do
              -- some mini-protocol errored
              --
              -- we don't need to notify the connection manager, we can instead
              -- rely on mux property: if any of the mini-protocols errors, mux
              -- throws an exception as well.
              traceWith spsTracer (PeerStatusChangeFailure
                                     (WarmToCooling pchConnectionId)
                                     (ApplicationFailure errs))
              throwIO (MiniProtocolExceptions errs)

            Just AllSucceeded {} -> do
              -- all mini-protocols terminated cleanly
              --
              -- 'unregisterOutboundConnection' could only fail to demote the peer if
              -- connection manager would simultaneously promote it, but this is not
              -- possible.
              _ <- releaseOutboundConnection spsConnectionManager pchConnectionId
              traceWith spsTracer (PeerStatusChanged (WarmToCooling pchConnectionId))


--
-- Utilities
--


-- | Smart constructor for 'ApplicationHandle'.
--
mkApplicationHandleBundle
    :: forall (muxMode :: Mux.Mode) responderCtx peerAddr extraFlags bytes m a b.
       OuroborosBundle muxMode (ExpandedInitiatorContext peerAddr extraFlags m)
                               responderCtx bytes m a b
    -- ^ mux application
    -> TemperatureBundle (StrictTVar m ControlMessage)
    -- ^ 'ControlMessage' bundle
    -> TemperatureBundle (StrictTVar m (Map MiniProtocolNum (STM m (HasReturned a))))
    -- ^ await for application termination
    -> TemperatureBundle (ApplicationHandle muxMode responderCtx peerAddr extraFlags bytes m a b)
mkApplicationHandleBundle muxBundle controlMessageBundle awaitVarsBundle =
    TemperatureBundle
      (mkApplication SingHot)
      (mkApplication SingWarm)
      (mkApplication SingEstablished)
  where
    mkApplication :: SingProtocolTemperature pt
                  -> WithProtocolTemperature pt (ApplicationHandle muxMode responderCtx peerAddr extraFlags bytes m a b)
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
startProtocols :: forall (muxMode :: Mux.Mode) (pt :: ProtocolTemperature)
                         responderCtx peerAddr extraFlags versionData m a b.
                  ( Alternative (STM m)
                  , MonadAsync m
                  , MonadCatch m
                  , MonadThrow (STM m)
                  , HasInitiator muxMode ~ True
                  , NFData a
                  , NFData b
                  )
               => SingProtocolTemperature pt
               -> IsBigLedgerPeer
               -> extraFlags
               -> PeerConnectionHandle muxMode responderCtx peerAddr extraFlags versionData ByteString m a b
               -> m ()
startProtocols tok isBigLedgerPeer extraFlags connHandle@PeerConnectionHandle { pchMux, pchAppHandles } = do
    let ptcls = getProtocols tok pchAppHandles
    as <- traverse runInitiator ptcls
    atomically $ writeTVar (getMiniProtocolsVar tok pchAppHandles)
                           (miniProtocolResults $ zip (miniProtocolNum `map` ptcls) as)
  where

    miniProtocolResults :: [(MiniProtocolNum, STM m (Either SomeException a))]
                        -> Map MiniProtocolNum (STM m (HasReturned a))
    miniProtocolResults = Map.map (fmap hasReturnedFromEither)
                        . Map.fromList

    runInitiator :: MiniProtocol muxMode (ExpandedInitiatorContext peerAddr extraFlags m)
                                         responderCtx ByteString m a b
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
                (runMiniProtocolCb initiator context)
          InitiatorAndResponderProtocol initiator _ ->
              Mux.runMiniProtocol
                pchMux miniProtocolNum
                Mux.InitiatorDirection
                Mux.StartEagerly
                (runMiniProtocolCb initiator context)
      where
        context :: ExpandedInitiatorContext peerAddr extraFlags m
        context = mkInitiatorContext tok isBigLedgerPeer extraFlags connHandle


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
    | ActiveCold !PeerStatus
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
    | WarmToHot        !(ConnectionId peerAddr)
    | HotToWarm        !(ConnectionId peerAddr)
    | WarmToCooling    !(ConnectionId peerAddr)
    | HotToCooling     !(ConnectionId peerAddr)
    | CoolingToCold    !(ConnectionId peerAddr)
  deriving Show

-- | Traces produced by 'peerSelectionActions'.
--
data PeerSelectionActionsTrace peerAddr vNumber =
      PeerStatusChanged       (PeerStatusChangeType peerAddr)
    | PeerStatusChangeFailure (PeerStatusChangeType peerAddr) (FailureType vNumber)
    | PeerMonitoringError     (ConnectionId peerAddr) SomeException
    | PeerMonitoringResult    (ConnectionId peerAddr) (Maybe (WithSomeProtocolTemperature FirstToFinishResult))
    | AcquireConnectionError  SomeException
    | PeerHotDuration         (ConnectionId peerAddr) DiffTime
  deriving Show
