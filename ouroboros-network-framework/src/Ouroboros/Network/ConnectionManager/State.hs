{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
-- Undecidable instances are need for 'Show' instance of 'ConnectionState'.
{-# LANGUAGE QuantifiedConstraints      #-}

module Ouroboros.Network.ConnectionManager.State
  ( -- * ConnectionManagerState API
    ConnectionManagerState
  , module ConnMap
    -- ** Monadic API
  , readConnectionStates
  , readAbstractStateMap
    -- * MutableConnState
  , MutableConnState (..)
  , ConnStateIdSupply
  , ConnStateId (..)
  , ConnectionTransitionTrace
  , newConnStateIdSupply
  , newMutableConnState
    -- * ConnectionState
  , ConnectionState (..)
  , abstractState
  , connectionTerminated
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Data.Function (on)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Prelude hiding (lookup)

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.ConnMap as ConnMap
import Ouroboros.Network.ConnectionManager.Types

import Test.Ouroboros.Network.Utils (WithName (..))

-- | 'ConnectionManager' state: for each peer we keep a 'ConnectionState' in
-- a mutable variable, which reduces congestion on the 'TMVar' which keeps
-- 'ConnectionManagerState'.
--
type ConnectionManagerState peerAddr handle handleError version m =
    ConnMap peerAddr (MutableConnState peerAddr handle handleError version m)


readConnectionStates
  :: MonadSTM m
  => ConnectionManagerState peerAddr handle handleError version m
  -> STM m (ConnMap peerAddr (ConnectionState peerAddr handle handleError version m))
readConnectionStates = traverse (readTVar . connVar)


readAbstractStateMap
  :: MonadSTM m
  => ConnectionManagerState peerAddr handle handleError version m
  -> STM m (ConnMap peerAddr AbstractState)
readAbstractStateMap = traverse (fmap (abstractState . Known) . readTVar . connVar)

-- | A unique identifier of a connection.
--
-- It's used even when we don't yet know `ConnectionId`.
--
newtype ConnStateId = ConnStateId Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Enum

-- | 'MutableConnState', which supplies a unique identifier.
--
-- TODO: We can get away without id, by tracking connections in
-- `TerminatingState` using a separate priority search queue.
--
data MutableConnState peerAddr handle handleError version m = MutableConnState {
    -- | A unique identifier
    --
    connStateId  :: !ConnStateId

  , -- | Mutable state
    --
    connVar      :: !(StrictTVar m (ConnectionState peerAddr handle handleError
                                                    version m))
  }


instance Eq (MutableConnState peerAddr handle handleError version m) where
    (==) =  (==) `on` connStateId


-- | A supply of fresh id's.
--
-- We use a fresh ids for 'MutableConnState'.
--
newtype ConnStateIdSupply m = ConnStateIdSupply { getConnStateId :: STM m ConnStateId }


-- | Create a 'FreshIdSupply' inside an 'STM' monad.
--
newConnStateIdSupply :: forall m. MonadSTM m
                     => Proxy m
                     -> STM m (ConnStateIdSupply m)
newConnStateIdSupply _ = do
    (v :: StrictTVar m ConnStateId) <- newTVar (ConnStateId 0)
    let getConnStateId :: STM m ConnStateId
        getConnStateId = do
          c <- readTVar v
          writeTVar v (succ c)
          return c
    return $ ConnStateIdSupply { getConnStateId }


type ConnectionTransitionTrace peerAddr = WithName peerAddr (AbstractTransitionTrace ConnStateId)


newMutableConnState :: forall peerAddr handle handleError version m.
                      ( MonadTraceSTM m
                      , Typeable peerAddr
                      )
                    => peerAddr
                    -> ConnStateIdSupply m
                    -> ConnectionState peerAddr handle handleError
                                       version m
                    -> STM m (MutableConnState peerAddr handle handleError
                                               version m)
newMutableConnState peerAddr freshIdSupply connState = do
      connStateId <- getConnStateId freshIdSupply
      connVar <- newTVar connState
      -- This tracing is a no op in IO.
      --
      -- We need this for IOSimPOR testing of connection manager state
      -- transition tests. It can happen that the transitions happen
      -- correctly but IOSimPOR reorders the threads that log the transitions.
      -- This is a false positive and we don't want that to happen.
      --
      -- The simplest way to do so is to leverage the `traceTVar` IOSim
      -- capabilities. These trace messages won't be reordered by IOSimPOR
      -- since these happen atomically in STM.
      --
      traceTVar
        (Proxy @m) connVar
        (\mbPrev curr ->
          let currAbs = abstractState (Known curr)
           in case mbPrev of
                Just prev |
                    let prevAbs = abstractState (Known prev)
                  , prevAbs /= currAbs -> pure
                                       $ TraceDynamic
                                       ( WithName peerAddr
                                                  (TransitionTrace connStateId $ mkAbsTransition prevAbs currAbs)
                                       :: ConnectionTransitionTrace peerAddr)
                Nothing                -> pure
                                       $ TraceDynamic
                                       ( WithName peerAddr
                                       $ TransitionTrace connStateId
                                       $ mkAbsTransition TerminatedSt
                                                         currAbs
                                       :: ConnectionTransitionTrace peerAddr)
                _                      -> pure DontTrace
        )
      return $ MutableConnState { connStateId, connVar }

abstractState :: MaybeUnknown (ConnectionState muxMode peerAddr m a b)
              -> AbstractState
abstractState = \case
    Unknown  -> UnknownConnectionSt
    Race s'  -> go s'
    Known s' -> go s'
  where
    go :: ConnectionState muxMode peerAddr m a b -> AbstractState
    go ReservedOutboundState {}       = ReservedOutboundSt
    go (UnnegotiatedState pr _ _)     = UnnegotiatedSt pr
    go OutboundUniState {}            = OutboundUniSt
    go (OutboundDupState    _ _ _ te) = OutboundDupSt te
    go (OutboundIdleState _ _ _ df)   = OutboundIdleSt df
    go (InboundIdleState _ _ _ df)    = InboundIdleSt df
    go (InboundState     _ _ _ df)    = InboundSt df
    go DuplexState {}                 = DuplexSt
    go TerminatingState {}            = TerminatingSt
    go TerminatedState {}             = TerminatedSt


-- | State of a connection.
--
data ConnectionState peerAddr handle handleError version m =
    -- | Each outbound connections starts in this state.
    ReservedOutboundState

    -- | Each inbound connection starts in this state, outbound connection
    -- reach this state once `connect` call returns.
    --
    -- note: the async handle is lazy, because it's passed with 'mfix'.
  | UnnegotiatedState   !Provenance
                        !(ConnectionId peerAddr)
                         (Async m ())

    -- | @OutboundState Unidirectional@ state.
  | OutboundUniState    !(ConnectionId peerAddr) !(Async m ()) !handle

    -- | Either @OutboundState Duplex@ or @OutboundState^\tau Duplex@.
  | OutboundDupState    !(ConnectionId peerAddr) !(Async m ()) !handle !TimeoutExpired

    -- | Before connection is reset it is put in 'OutboundIdleState' for the
    -- duration of 'outboundIdleTimeout'.
    --
  | OutboundIdleState   !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | InboundIdleState    !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | InboundState        !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | DuplexState         !(ConnectionId peerAddr) !(Async m ()) !handle
  | TerminatingState    !(ConnectionId peerAddr) !(Async m ()) !(Maybe handleError)
  | TerminatedState                              !(Maybe handleError)


instance ( Show peerAddr
         -- , Show handleError
         , MonadAsync m
         )
      => Show (ConnectionState peerAddr handle handleError version m) where
    show ReservedOutboundState = "ReservedOutboundState"
    show (UnnegotiatedState pr connId connThread) =
      concat ["UnnegotiatedState "
             , show pr
             , " "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             ]
    show (OutboundUniState connId connThread _handle) =
      concat [ "OutboundState Unidirectional "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             ]
    show (OutboundDupState connId connThread _handle expired) =
      concat [ "OutboundState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             , " "
             , show expired
             ]
    show (OutboundIdleState connId connThread _handle df) =
      concat [ "OutboundIdleState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             , " "
             , show df
             ]
    show (InboundIdleState connId connThread _handle df) =
      concat [ "InboundIdleState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             , " "
             , show df
             ]
    show (InboundState  connId connThread _handle df) =
      concat [ "InboundState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             , " "
             , show df
             ]
    show (DuplexState   connId connThread _handle) =
      concat [ "DuplexState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             ]
    show (TerminatingState connId connThread _handleError) =
      concat ([ "TerminatingState "
              , show connId
              , " "
              , show (asyncThreadId connThread)
              ]
              )-- ++ maybeToList ((' ' :) . show <$> handleError))
    show (TerminatedState _handleError) =
      concat (["TerminatedState"]
              )-- ++ maybeToList ((' ' :) . show <$> handleError))


-- | Return 'True' for states in which the connection was already closed.
--
connectionTerminated :: ConnectionState peerAddr handle handleError version m
                     -> Bool
connectionTerminated TerminatingState {} = True
connectionTerminated TerminatedState  {} = True
connectionTerminated _                   = False
