{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Error policies, and integration with 'SuspendDecision'-semigroup action on
-- 'PeerState'.
--
module Ouroboros.Network.ErrorPolicy
  ( ErrorPolicies (..)
  , nullErrorPolicies
  , ErrorPolicy (..)
  , evalErrorPolicy
  , evalErrorPolicies
  , CompleteApplication
  , Result (..)
  , completeApplicationTx

  -- * Traces
  , ErrorPolicyTrace (..)
  , traceErrorPolicy
  , WithAddr (..)

  -- * Re-exports of PeerState
  , PeerStates
  , SuspendDecision (..)
  ) where

import           Control.Exception (Exception, SomeException (..))
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Semigroup (sconcat)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable ( Proxy (..)
                               , cast
                               , tyConName
                               , typeRepTyCon
                               , typeRep
                               )
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Data.Semigroup.Action

import           Ouroboros.Network.Subscription.PeerState

data ErrorPolicy where
     ErrorPolicy :: forall e.
                      Exception e
                   => (e -> SuspendDecision DiffTime)
                   -> ErrorPolicy

instance Show ErrorPolicy where
    show (ErrorPolicy (_err :: e -> SuspendDecision DiffTime)) =
           "ErrorPolicy ("
        ++ tyConName (typeRepTyCon (typeRep (Proxy :: Proxy e)))
        ++ ")"


evalErrorPolicy :: forall e.
                   Exception e
                => e
                -> ErrorPolicy
                -> Maybe (SuspendDecision DiffTime)
evalErrorPolicy e p =
    case p of
      ErrorPolicy (f :: e' -> SuspendDecision DiffTime)
        -> case cast e :: Maybe e' of
              Nothing -> Nothing
              Just e' -> Just $ f e'

-- | Evaluate a list of 'ErrorPolicy's; If none of them applies this function
-- returns 'Nothing', in this case the exception will be traced and not thrown.
--
evalErrorPolicies :: forall e.
                     Exception e
                  => e
                  -> [ErrorPolicy]
                  -> Maybe (SuspendDecision DiffTime)
evalErrorPolicies e =
    f . mapMaybe (evalErrorPolicy e)
  where
    f :: [SuspendDecision DiffTime]
      -> Maybe (SuspendDecision DiffTime)
    f []          = Nothing
    f (cmd : rst) = Just $ sconcat (cmd :| rst)


-- | List of error policies for exception handling and a policy for handing
-- application return values.
--
data ErrorPolicies addr a = ErrorPolicies {
    -- | Application Error Policies
    epAppErrorPolicies  :: [ErrorPolicy]
    -- | `connect` Error Policies
  , epConErrorPolicies  :: [ErrorPolicy]
  , epReturnCallback :: Time -> addr -> a -> SuspendDecision DiffTime
  }

nullErrorPolicies :: ErrorPolicies addr a
nullErrorPolicies = ErrorPolicies [] [] (\_ _ _ -> Throw)

instance Semigroup (ErrorPolicies addr a) where
    ErrorPolicies aep cep fn <> ErrorPolicies aep' cep' fn'
      = ErrorPolicies (aep <> aep') (cep <> cep') (fn <> fn')

-- | Sum type which distinguishes between connection and application
-- exception traces.
--
data ConnectionOrApplicationExceptionTrace err =
     -- | Trace of exception thrown by `connect`
     ConnectionExceptionTrace err
     -- | Trace of exception thrown by an application
   | ApplicationExceptionTrace err
   deriving (Show, Functor)

-- | Complete a connection, which receive application result (or exception).
--
type CompleteApplication m s addr r = Result addr r -> s -> STM m (s, m ())


-- | Result of the connection thread.  It's either result of an application, or
-- an exception thrown by it.
--
data Result addr r where
     ApplicationResult
       :: !Time
       -> !addr
       -> !r
       -> Result addr r

     Connected
       :: !Time
       -> !addr
       -> Result addr r

     ConnectionError
       :: Exception e
       => !Time
       -> !addr
       -> !e
       -> Result addr r

     ApplicationError
       :: Exception e
       => !Time
       -> !addr
       -> !e
       -> Result addr r


-- | 'CompleteApplication' callback
--
completeApplicationTx
  :: forall m addr a.
     ( MonadAsync  m
     , Ord addr
     , Ord (Async m ())
     )
  => Tracer m (WithAddr addr ErrorPolicyTrace)
  -> ErrorPolicies addr a
  -> CompleteApplication m
       (PeerStates m addr)
       addr
       a

-- the 'ResultQ' did not throw the exception yet; it should not happen.
completeApplicationTx _ _ _ ps@ThrowException{} = pure ( ps, pure () )

-- application returned; classify the return value and update the state.
completeApplicationTx tracer ErrorPolicies {epReturnCallback} (ApplicationResult t addr r) (PeerStates ps) =
  let cmd = epReturnCallback t addr r
      fn :: Maybe (PeerState m)
         -> ( Set (Async m ())
            , Maybe (PeerState m)
            )
      fn mbps = ( maybe Set.empty (`threadsToCancel` cmd) mbps
                , mbps <| (flip addTime t <$> cmd)
                )
  in case alterAndLookup fn addr ps of
    (ps', mbthreads) -> pure
      ( PeerStates ps'
      , do
          traverse_ (traceWith tracer . WithAddr addr)
                    (traceErrorPolicy (Right r) cmd)
          traverse_ cancel
                    (fromMaybe Set.empty mbthreads)
      )

-- application errored
completeApplicationTx tracer ErrorPolicies {epAppErrorPolicies} (ApplicationError t addr e) ps =
  case evalErrorPolicies e epAppErrorPolicies of
    -- the error is not handled by any policy; we're not rethrowing the
    -- error from the main thread, we only trace it.  This will only kill
    -- the local consumer application.
    Nothing  -> pure ( ps
                     , traceWith tracer
                        (WithAddr addr
                          (ErrorPolicyUnhandledApplicationException
                            (SomeException e)))
                     )
    -- the error was classified; act with the 'SuspendDecision' on the state
    -- and find threads to cancel.
    Just cmd -> case runSuspendDecision t addr e cmd ps of
      (ps', threads) ->
        pure ( ps'
             , do
                traverse_ (traceWith tracer . WithAddr addr)
                          (traceErrorPolicy (Left $ ApplicationExceptionTrace (SomeException e))
                                            cmd)
                traverse_ cancel threads
            )

-- we connected to a peer; this does not require to update the 'PeerState'.
completeApplicationTx _ _ (Connected _t  _addr) ps =
  pure ( ps, pure () )

-- error raised by the 'connect' call
completeApplicationTx tracer ErrorPolicies {epConErrorPolicies} (ConnectionError t addr e) ps =
  case evalErrorPolicies e epConErrorPolicies of
    Nothing  ->
      let fn p@(HotPeer producers consumers)
             | Set.null producers && Set.null consumers
             = Just ColdPeer
             | otherwise
             = Just p
          fn p = Just p

      in pure ( case ps of
                  PeerStates peerStates -> PeerStates $ Map.update fn addr peerStates
                  ThrowException{} -> ps
              , traceWith tracer
                 (WithAddr addr
                   (ErrorPolicyUnhandledConnectionException
                     (SomeException e)))
              )
    Just cmd -> case runSuspendDecision t addr e cmd ps of
      (ps', threads) ->
        pure ( ps'
             , do
                 traverse_ (traceWith tracer . WithAddr addr)
                           (traceErrorPolicy (Left $ ConnectionExceptionTrace (SomeException e)) cmd)
                 traverse_ cancel threads
             )

--
-- Traces
--

-- | Trace data for error policies
data ErrorPolicyTrace
  = ErrorPolicySuspendPeer (Maybe (ConnectionOrApplicationExceptionTrace SomeException)) !DiffTime !DiffTime
  -- ^ suspending peer with a given exception until
  | ErrorPolicySuspendConsumer (Maybe (ConnectionOrApplicationExceptionTrace SomeException)) !DiffTime
  -- ^ suspending consumer until
  | ErrorPolicyLocalNodeError (ConnectionOrApplicationExceptionTrace SomeException)
  -- ^ caught a local exception
  | ErrorPolicyResumePeer
  -- ^ resume a peer (both consumer and producer)
  | ErrorPolicyKeepSuspended
  -- ^ consumer was suspended until producer will resume
  | ErrorPolicyResumeConsumer
  -- ^ resume consumer
  | ErrorPolicyResumeProducer
  -- ^ resume producer
  | ErrorPolicyUnhandledApplicationException SomeException
  -- ^ an application throwed an exception, which was not handled by any
  -- 'ErrorPolicy'.
  | ErrorPolicyUnhandledConnectionException SomeException
  -- ^ 'connect' throwed an exception, which was not handled by any
  -- 'ErrorPolicy'.
  deriving Show

traceErrorPolicy :: Either (ConnectionOrApplicationExceptionTrace SomeException) r
                 -> SuspendDecision DiffTime
                 -> Maybe ErrorPolicyTrace
traceErrorPolicy (Left e) (SuspendPeer prodT consT) =
    Just $ ErrorPolicySuspendPeer (Just e) prodT consT
traceErrorPolicy (Right _) (SuspendPeer prodT consT) =
    Just $ ErrorPolicySuspendPeer Nothing prodT consT
traceErrorPolicy (Left e) (SuspendConsumer consT) =
    Just $ ErrorPolicySuspendConsumer (Just e) consT
traceErrorPolicy (Right _) (SuspendConsumer consT) =
    Just $ ErrorPolicySuspendConsumer Nothing consT
traceErrorPolicy (Left e) Throw =
    Just $ ErrorPolicyLocalNodeError e
traceErrorPolicy _ _ =
    Nothing

data WithAddr addr a = WithAddr {
      wiaAddr  :: !addr
    , wiaEvent :: !a
    }

instance (Show addr, Show a) => Show (WithAddr addr a) where
    show WithAddr { wiaAddr, wiaEvent } =
        printf "IP %s %s" (show wiaAddr) (show wiaEvent)
