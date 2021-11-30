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
  , CompleteApplicationResult (..)
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

import           Control.Exception (Exception, IOException, SomeException (..))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
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

import           Ouroboros.Network.Subscription.PeerState

data ErrorPolicy where
     ErrorPolicy :: forall e.
                      Exception e
                   => (e -> Maybe (SuspendDecision DiffTime))
                   -- ^ @Nothing@ means no decision. It is equivalent to not
                   -- having the policy at all. In 'evalErrorPolicies' this will
                   -- fall-through and match against the remaining policies.
                   -> ErrorPolicy

instance Show ErrorPolicy where
    show (ErrorPolicy (_err :: e -> Maybe (SuspendDecision DiffTime))) =
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
      ErrorPolicy (f :: e' -> Maybe (SuspendDecision DiffTime))
        -> case cast e :: Maybe e' of
              Nothing -> Nothing
              Just e' -> f e'

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
data ErrorPolicies = ErrorPolicies {
    -- | Application Error Policies
    epAppErrorPolicies  :: [ErrorPolicy]
    -- | `connect` Error Policies
  , epConErrorPolicies  :: [ErrorPolicy]
  }

nullErrorPolicies :: ErrorPolicies
nullErrorPolicies = ErrorPolicies [] []

instance Semigroup ErrorPolicies where
    ErrorPolicies aep cep <> ErrorPolicies aep' cep' 
      = ErrorPolicies (aep <> aep') (cep <> cep')

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
type CompleteApplication m s addr r =
    Result addr r -> s -> STM m (CompleteApplicationResult m addr s)


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


data CompleteApplicationResult m addr s =
    CompleteApplicationResult {
        carState   :: !s,
        -- ^ new state
        carThreads :: Set (Async m ()),
        -- ^ threads to kill
        carTrace   :: Maybe (WithAddr addr ErrorPolicyTrace)
        -- ^ trace points
      }
  deriving Functor


-- | 'CompleteApplication' callback
--
completeApplicationTx
  :: forall m addr a.
     ( MonadAsync  m
     , Ord addr
     , Ord (Async m ())
     )
  => ErrorPolicies
  -> CompleteApplication m
       (PeerStates m addr)
       addr
       a

-- the 'ResultQ' did not throw the exception yet; it should not happen.
completeApplicationTx _ _ ps@ThrowException{} = pure $
    CompleteApplicationResult {
        carState   = ps,
        carThreads = Set.empty,
        carTrace   = Nothing
      }

-- application returned; classify the return value and update the state.
completeApplicationTx _ ApplicationResult{} ps =
    pure $ CompleteApplicationResult {
        carState   = ps,
        carThreads = Set.empty,
        carTrace   = Nothing
      }

-- application errored
completeApplicationTx ErrorPolicies {epAppErrorPolicies} (ApplicationError t addr e) ps =
  case evalErrorPolicies e epAppErrorPolicies of
    -- the error is not handled by any policy; we're not rethrowing the
    -- error from the main thread, we only trace it.  This will only kill
    -- the local consumer application.
    Nothing  -> pure $
      CompleteApplicationResult {
          carState   = ps,
          carThreads = Set.empty,
          carTrace   = Just
                        (WithAddr addr
                          (ErrorPolicyUnhandledApplicationException
                            (SomeException e)))
        }
    -- the error was classified; act with the 'SuspendDecision' on the state
    -- and find threads to cancel.
    Just cmd -> case runSuspendDecision t addr e cmd ps of
      (ps', threads) ->
        pure $
          CompleteApplicationResult {
              carState   = ps',
              carThreads = threads,
              carTrace   = WithAddr addr <$>
                            traceErrorPolicy
                              (Left $ ApplicationExceptionTrace (SomeException e))
                              cmd
            }

-- we connected to a peer; this does not require to update the 'PeerState'.
completeApplicationTx _ (Connected _t  _addr) ps =
    pure $
      CompleteApplicationResult {
          carState   = ps,
          carThreads = Set.empty,
          carTrace   = Nothing
        }

-- error raised by the 'connect' call
completeApplicationTx ErrorPolicies {epConErrorPolicies} (ConnectionError t addr e) ps =
  case evalErrorPolicies e epConErrorPolicies of
    Nothing  ->
      let fn p@(HotPeer producers consumers)
             | Set.null producers && Set.null consumers
             = Just ColdPeer
             | otherwise
             = Just p
          fn p = Just p

      in pure $
          CompleteApplicationResult {
              carState =
                case ps of
                  PeerStates peerStates -> PeerStates $ Map.update fn addr peerStates
                  ThrowException{}      -> ps,
              carThreads = Set.empty,
              carTrace   = Just $
                            WithAddr addr
                             (ErrorPolicyUnhandledConnectionException
                               (SomeException e))
            }
    Just cmd -> case runSuspendDecision t addr e cmd ps of
      (ps', threads) ->
        pure $
          CompleteApplicationResult {
              carState   = ps',
              carThreads = threads,
              carTrace   = WithAddr addr <$>
                           (traceErrorPolicy
                             (Left $ ConnectionExceptionTrace (SomeException e))
                             cmd)
            }

--
-- Traces
--

-- | Trace data for error policies
data ErrorPolicyTrace
  = ErrorPolicySuspendPeer (Maybe (ConnectionOrApplicationExceptionTrace SomeException)) DiffTime DiffTime
  -- ^ suspending peer with a given exception until
  | ErrorPolicySuspendConsumer (Maybe (ConnectionOrApplicationExceptionTrace SomeException)) DiffTime
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
  | ErrorPolicyAcceptException IOException
  -- ^ 'accept' throwed an exception
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
      wiaAddr  :: addr
    , wiaEvent :: a
    }

instance (Show addr, Show a) => Show (WithAddr addr a) where
    show WithAddr { wiaAddr, wiaEvent } =
        printf "IP %s %s" (show wiaAddr) (show wiaEvent)
