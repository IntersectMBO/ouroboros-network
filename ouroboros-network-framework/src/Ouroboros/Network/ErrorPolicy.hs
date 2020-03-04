{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Error policies
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

  -- * Traces
  , ErrorPolicyTrace (..)
  , traceErrorPolicy
  , WithAddr (..)

  , SuspendDecision (..)
  ) where

import           Control.Exception (Exception, IOException, SomeException (..))
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe (mapMaybe)
import           Data.Semigroup (sconcat)
import           Data.Set (Set)
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

-- | Semigroup of commands which acts on 'PeerState'.  The @t@ variable might
-- be initiated to 'DiffTime' or @Time m@.
--
-- This semigroup allows to either suspend both consumer and producer or just
-- the consumer part.
--
data SuspendDecision t
    = SuspendPeer !t !t
    -- ^ peer is suspend; The first @t@ is the time until which a local
    -- producer is suspended, the second one is the time until which a local
    -- consumer is suspended.
    | SuspendConsumer !t
    -- ^ suspend local consumer \/ initiator side until @t@ (this mean we are
    -- not allowing to communicate with the producer \/ responder of a remote
    -- peer).
    | Throw
    -- ^ throw an error from the main thread.
    deriving (Eq, Ord, Show, Functor)

-- | The semigroup instance.  Note that composing 'SuspendPeer' with
-- 'SuspendConsumer' gives 'SuspendPeer'.  'SuspendPeer' and 'SuspendConsumer'
-- form a sub-semigroup.
--
instance Ord t => Semigroup (SuspendDecision t) where
    Throw <> _ = Throw
    _ <> Throw = Throw
    SuspendPeer prodT consT <> SuspendPeer prodT' consT'
      = SuspendPeer (prodT `max` prodT') (consT `max` consT')
    SuspendConsumer consT <> SuspendPeer prodT consT'
      = SuspendPeer prodT (consT `max` consT')
    SuspendPeer prodT consT <> SuspendConsumer consT'
      = SuspendPeer prodT (consT `max` consT')
    SuspendConsumer consT <> SuspendConsumer consT'
      = SuspendConsumer (consT `max` consT')

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
      wiaAddr  :: !addr
    , wiaEvent :: !a
    }

instance (Show addr, Show a) => Show (WithAddr addr a) where
    show WithAddr { wiaAddr, wiaEvent } =
        printf "IP %s %s" (show wiaAddr) (show wiaEvent)
