{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
-- Undecidable instances are need for 'Show' instance of 'ConnectionState'.
{-# LANGUAGE QuantifiedConstraints #-}

module Ouroboros.Network.ConnectionManager.State
  ( ConnectionManagerState
  , MutableConnState (..)
  , FreshIdSupply
  , newFreshIdSupply
  , newMutableConnState
  , abstractState
  , ConnectionState (..)
  , connectionTerminated
  ) where

import Control.Monad.Class.MonadAsync
import Control.Concurrent.Class.MonadSTM.Strict
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Types

import Ouroboros.Network.Testing.Utils (WithName (..))

-- | 'ConnectionManager' state: for each peer we keep a 'ConnectionState' in
-- a mutable variable, which reduces congestion on the 'TMVar' which keeps
-- 'ConnectionManagerState'.
--
-- It is important we can lookup by remote @peerAddr@; this way we can find if
-- the connection manager is already managing a connection towards that
-- @peerAddr@ and reuse the 'ConnectionState'.
--
type ConnectionManagerState peerAddr handle handleError version m
  = Map peerAddr (MutableConnState peerAddr handle handleError version m)


-- | 'MutableConnState', which supplies a unique identifier.
--
-- TODO: We can get away without id, by tracking connections in
-- `TerminatingState` using a separate priority search queue.
--
data MutableConnState peerAddr handle handleError version m = MutableConnState {
    -- | A unique identifier
    --
    connStateId  :: !Int

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
newtype FreshIdSupply m = FreshIdSupply { getFreshId :: STM m Int }


-- | Create a 'FreshIdSupply' inside an 'STM' monad.
--
newFreshIdSupply :: forall m. MonadSTM m
                 => Proxy m -> STM m (FreshIdSupply m)
newFreshIdSupply _ = do
    (v :: StrictTVar m Int) <- newTVar 0
    let getFreshId :: STM m Int
        getFreshId = do
          c <- readTVar v
          writeTVar v (succ c)
          return c
    return $ FreshIdSupply { getFreshId }


newMutableConnState :: forall peerAddr handle handleError version m.
                      ( MonadTraceSTM m
                      , Typeable peerAddr
                      )
                    => peerAddr
                    -> FreshIdSupply m
                    -> ConnectionState peerAddr handle handleError
                                       version m
                    -> STM m (MutableConnState peerAddr handle handleError
                                               version m)
newMutableConnState peerAddr freshIdSupply connState = do
      connStateId <- getFreshId freshIdSupply
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
                                       $ WithName connStateId
                                       $ TransitionTrace peerAddr
                                       $ mkAbsTransition prevAbs
                                                         currAbs
                Nothing                -> pure
                                       $ TraceDynamic
                                       $ WithName connStateId
                                       $ TransitionTrace peerAddr
                                       $ mkAbsTransition TerminatedSt
                                                         currAbs
                _                      -> pure DontTrace
        )
      return $ MutableConnState { connStateId, connVar }


abstractState :: MaybeUnknown (ConnectionState muxMode peerAddr m a b) -> AbstractState
abstractState = \case
    Unknown  -> UnknownConnectionSt
    Race s'  -> go s'
    Known s' -> go s'
  where
    go :: ConnectionState muxMode peerAddr m a b -> AbstractState
    go ReservedOutboundState {}       = ReservedOutboundSt
    go (UnnegotiatedState pr _ _)     = UnnegotiatedSt pr
    go (OutboundUniState    _ _ _)    = OutboundUniSt
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
         , Show handleError
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
    show (TerminatingState connId connThread handleError) =
      concat ([ "TerminatingState "
              , show connId
              , " "
              , show (asyncThreadId connThread)
              ]
              ++ maybeToList ((' ' :) . show <$> handleError))
    show (TerminatedState handleError) =
      concat (["TerminatedState"]
              ++ maybeToList ((' ' :) . show <$> handleError))


-- | Return 'True' for states in which the connection was already closed.
--
connectionTerminated :: ConnectionState peerAddr handle handleError version m
                     -> Bool
connectionTerminated TerminatingState {} = True
connectionTerminated TerminatedState  {} = True
connectionTerminated _                   = False
