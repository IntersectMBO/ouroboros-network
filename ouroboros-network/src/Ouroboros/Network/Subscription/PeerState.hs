{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module contains peer state management and error policies.
--
module Ouroboros.Network.Subscription.PeerState
  ( SuspendDecision (..)
  , suspend
  -- * Error policy GADT
  , ErrorPolicy (..)
  , ConnectionOrApplicationException (..)
  , evalErrorPolicy
  , evalErrorPolicies
  -- * PeerStates and its operations
  , PeerState (..)
  , threadsToCancel
  , PeerStates (..)
  , newPeerStatesVar
  , runSuspendDecision
  , registerConsumer
  , unregisterConsumer
  , registerProducer
  , unregisterProducer

  -- * Tracing
  , ErrorPolicyTrace (..)
  , traceErrorPolicy
  , WithAddr (..)

  -- * Re-exports
  , DiffTime

  -- * Auxiliary functions
  , alterAndLookup
  ) where

import           Control.Exception (Exception, SomeException (..), assert)
import           Control.Monad.State
import           Data.Map.Strict (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Semigroup (sconcat)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable ( Proxy (..)
                               , (:~:) (..)
                               , eqT
                               , gcast
                               , tyConName
                               , typeRepTyCon
                               , typeRep
                               )
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime

import           Data.Semigroup.Action

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

consumerSuspendedUntil :: SuspendDecision t -> Maybe t
consumerSuspendedUntil (SuspendPeer _ consT)   = Just consT
consumerSuspendedUntil (SuspendConsumer consT) = Just consT
consumerSuspendedUntil Throw                   = Nothing

producerSuspendedUntil :: SuspendDecision t -> Maybe t
producerSuspendedUntil (SuspendPeer prodT _) = Just prodT
producerSuspendedUntil (SuspendConsumer _) = Nothing
producerSuspendedUntil Throw               = Nothing

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


-- | Sum type which distinguishes between connection and application
-- exceptions.
--
data ConnectionOrApplicationException err =
     -- | Exception thrown by `connect`
     ConnectionException err
     -- | Exception thrown by an application
   | ApplicationException err
   deriving (Show, Functor)

data ErrorPolicy where
     ErrorPolicy :: forall err.
                      Exception err
                   => (ConnectionOrApplicationException err -> SuspendDecision DiffTime)
                   -> ErrorPolicy

instance Show ErrorPolicy where
    show (ErrorPolicy (_err :: ConnectionOrApplicationException err -> SuspendDecision DiffTime)) =
           "ErrorPolicy ("
        ++ tyConName (typeRepTyCon (typeRep (Proxy :: Proxy err)))
        ++ ")"

evalErrorPolicy :: forall e.
                   Exception e
                => ConnectionOrApplicationException e
                -> ErrorPolicy
                -> Maybe (SuspendDecision DiffTime)
evalErrorPolicy e p =
    case p of
      ErrorPolicy (f :: ConnectionOrApplicationException e' -> SuspendDecision DiffTime)
        -> case gcast e :: Maybe (ConnectionOrApplicationException e') of
              Nothing -> Nothing
              Just e' -> Just $ f e'

-- | Evaluate a list of 'ErrorPolicy's; If none of them applies this function
-- returns 'Nothing', in this case the exception will be traced and not thrown.
--
evalErrorPolicies :: forall e.
                     Exception e
                  => ConnectionOrApplicationException e
                  -> [ErrorPolicy]
                  -> Maybe (SuspendDecision DiffTime)
evalErrorPolicies e =
    f . mapMaybe (evalErrorPolicy e)
  where
    f :: [SuspendDecision DiffTime]
      -> Maybe (SuspendDecision DiffTime)
    f []          = Nothing
    f (cmd : rst) = Just $ sconcat (cmd :| rst)


data PeerState m t
  = HotPeer !(Set (Async m ())) !(Set (Async m ()))
  -- ^ active peer with its producers and consumer threads
  | SuspendedConsumer !(Set (Async m ())) !t
  -- ^ suspended consumer: with producer threads and time until the consumer is
  -- suspended
  | SuspendedPeer !t !t
  -- ^ suspended peer: producer & consumer suspend time
  | ColdPeer
  -- ^ peer with no opened connections in either direction
  deriving Functor

instance ( Show t
         , MonadAsync m
         , Ord (ThreadId m)
         , Show (ThreadId m)
         ) => Show (PeerState m t) where
    show (HotPeer producers consumers)
       = "HotPeer"
      ++ " "
      ++ show (Set.map (asyncThreadId (Proxy :: Proxy m)) producers)
      ++ " "
      ++ show (Set.map (asyncThreadId (Proxy :: Proxy m)) consumers)
    show (SuspendedConsumer producers consT)
       = "SuspendedConsumer"
      ++ " "
      ++ show (Set.map (asyncThreadId (Proxy :: Proxy m)) producers)
      ++ " "
      ++ show consT
    show (SuspendedPeer prodT consT)
       = "SuspendedPeer"
      ++ " "
      ++ show prodT
      ++ " "
      ++ show consT
    show ColdPeer = "ColdPeer"

deriving instance (Eq (Async m ()), Eq t) => Eq (PeerState m t)

deriving instance (Ord (Async m ()), Ord t) => Ord (PeerState m t)

-- | Action of 'SuspendDecision' on @Maybe 'PeerState'@.  We use this action
-- together with 'Map.alter' function.
--
-- Note: 'SuspendDecision' does not act on 'PeerState', only the sub-semigroup
-- generated by 'SuspendConsumer' and 'SuspendPeer' does.
--
--
instance Ord t => SAct (SuspendDecision t) (Maybe (PeerState m t)) where

    -- this means we will remove the entry from the state map; this is fine
    -- since we are about to throw an exception to kill a node.
    _ <| Throw   = Nothing
    Nothing <| _ = Nothing

    -- this might apply when a connection to a 'ColdPeer' thrown an
    -- exception.
    (Just ColdPeer) <| (SuspendConsumer consT)
      = Just $ SuspendedConsumer Set.empty consT
    (Just ColdPeer) <| (SuspendPeer prodT consT)
      = Just (SuspendedPeer prodT consT)

    (Just (HotPeer producers _consumers)) <| (SuspendConsumer consT)
      = Just $ SuspendedConsumer producers consT
    (Just (HotPeer _prodcuers _consumers)) <| (SuspendPeer prodT consT)
      = Just $ SuspendedPeer prodT consT

    (Just (SuspendedConsumer producers consT)) <| (SuspendConsumer consT')
      = Just $ SuspendedConsumer producers (consT `max` consT')
    (Just (SuspendedConsumer _producers consT)) <| (SuspendPeer prodT consT')
      = Just $ SuspendedPeer prodT (consT `max` consT')

    (Just (SuspendedPeer prodT consT)) <| cmd
      = case producerSuspendedUntil cmd of
          Nothing ->
            Just $ SuspendedPeer
                    prodT
                    (maybe consT (consT `max`) $ consumerSuspendedUntil cmd)
          Just prodT' ->
            Just $ SuspendedPeer
                    (prodT `max` prodT')
                    (maybe consT (consT `max`) $ consumerSuspendedUntil cmd)

-- | Threads which needs to be cancelled when updating the 'PeerState' with
-- 'SuspendDecision'.
--
threadsToCancel :: Ord (Async m ())
                => PeerState m t
                -> SuspendDecision diffTime
                -> Set (Async m ())
threadsToCancel _ Throw
    = Set.empty
threadsToCancel ColdPeer _
    = Set.empty
threadsToCancel (HotPeer _producers consumers) SuspendConsumer{}
    = consumers
threadsToCancel (HotPeer consumers producers) SuspendPeer{}
    = consumers <> producers
threadsToCancel SuspendedConsumer{} SuspendConsumer{}
    = Set.empty
threadsToCancel (SuspendedConsumer producers _consT) SuspendPeer{}
    = producers
threadsToCancel SuspendedPeer{} _cmd
    = Set.empty


-- | Action of 'SuspendDecision' on @Maybe 'PeerState'@.  Action laws are only
-- satisfied for the submonoid form by 'SuspendPeer' and 'SuspendConsumer'.
--
suspend :: ( Ord t
           , Ord (Async m ())
           )
        => Maybe (PeerState m t)
        -> SuspendDecision t
        -> ( Set (Async m ())
           , Maybe (PeerState m t)
           )
suspend mbps cmd = ( maybe Set.empty (`threadsToCancel` cmd) mbps
                   , mbps <| cmd
                   )


-- | Map from addresses to 'PeerState's; it will be be shared in a 'StrictTVar'.
--
-- Abstracting @t@ is useful for tests, the @IO@ version will use @Time IO@.
--
data PeerStates m addr t where
     -- | Map of peer states
     PeerStates :: !(Map addr (PeerState m t))
                -> PeerStates m addr t

     -- | Or an exception to throw
     ThrowException :: Exception e
                    => e
                    -> PeerStates m addr t

instance ( Show addr
         , Show t
         ) => Show (PeerStates IO addr t) where
    show (PeerStates ps)    = "PeerStates "     ++ show ps
    show (ThrowException e) = "ThrowException " ++ show e

-- TODO: move to Test.PeerStates as eqPeerStates
instance ( Eq addr
         , Eq t
         ) => Eq (PeerStates IO addr t) where
    ThrowException (_ :: e) == ThrowException (_ :: e') =
      case eqT :: Maybe (e :~: e') of
        Nothing   -> False
        Just Refl -> True
    PeerStates ps == PeerStates ps' = ps == ps'
    _ == _ = False


newPeerStatesVar :: MonadSTM m => m (StrictTVar m (PeerStates m addr t))
newPeerStatesVar = newTVarM (PeerStates Map.empty)

-- | Update 'PeerStates' for a given 'addr', using 'suspend', and return
-- threads which must be cancelled.
--
-- This is more efficient that using the action of 'SuspendDecision' on
-- 'PeerStates', since it only uses a single dictionary lookup to update the
-- state and return the set of threads to be cancelled.
--
runSuspendDecision
    :: forall m addr e t.
       ( Monad m
       , TimeMeasure t
       , Ord addr
       , Ord t
       , Ord (Async m ())
       , Exception e
       )
    => t
    -> addr
    -> e
    -> SuspendDecision DiffTime
    -> PeerStates m addr t
    -> ( PeerStates m addr t
       , Set (Async m ())
       )
runSuspendDecision _t _addr _e _cmd ps0@ThrowException{} =
    ( ps0
    , Set.empty
    )
runSuspendDecision _t _addr  e  Throw _ =
    ( ThrowException (SomeException e)
    , Set.empty
    )
runSuspendDecision  t  addr _e  cmd (PeerStates ps0) =
    gn $ alterAndLookup fn addr ps0
  where
    fn :: Maybe (PeerState m t)
       -> ( Set (Async m ())
          , Maybe (PeerState m t)
          )
    fn mbps = ( maybe Set.empty (`threadsToCancel` cmd) mbps
              , mbps <| (flip addTime t <$> cmd)
              )

    gn :: ( Map addr (PeerState m t)
          , Maybe (Set (Async m ()))
          )
       -> ( PeerStates m addr t
          , Set (Async m ())
          )
    gn (ps, Nothing) = (PeerStates ps, Set.empty)
    gn (ps, Just s)  = (PeerStates ps, s)


-- Using pure 'State' monad and 'alterF' to avoid searching the 'PeerState'
-- twice.
alterAndLookup
    :: forall k s a.
       Ord k
    => (Maybe a -> (s, Maybe a))
    -> k
    -> Map k a
    -> ( Map k a
       , Maybe s
       )
alterAndLookup f k m = runState (Map.alterF g k m) Nothing
  where
    g :: Maybe a -> State (Maybe s) (Maybe a)
    g mba = case f mba of
      (s, mba') -> mba' <$ modify' (const (Just s))


traceErrorPolicy :: Either (ConnectionOrApplicationException SomeException) r
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


-- | Register producer in PeerStates.  This is a partial function which assumes
-- that the 'PeerState' is either 'HotPeer' or 'SuspendedConsumer'.
--
registerProducer :: forall m addr t.
                    ( Ord addr
                    , Ord (Async m ())
                    )
                 => addr
                 -> Async m ()
                 -> PeerStates m addr t
                 -> PeerStates m addr t
registerProducer _addr _tid ps@ThrowException{} = ps
registerProducer addr  tid  (PeerStates peerStates) =
    PeerStates $ Map.alter fn addr peerStates
  where
    fn :: Maybe (PeerState m t) -> Maybe (PeerState m t)
    fn Nothing =
        Just (HotPeer (Set.singleton tid) Set.empty)
    fn (Just (HotPeer producers consumers)) =
        Just (HotPeer (tid `Set.insert` producers) consumers)
    fn (Just ColdPeer) =
        Just (HotPeer (Set.singleton tid) Set.empty)
    fn (Just (SuspendedConsumer producers consT)) =
        Just (SuspendedConsumer (tid `Set.insert` producers) consT)
    fn (Just ps@SuspendedPeer{}) =
        -- registerProducer on a suspended peer
        assert False $ Just ps

unregisterProducer :: forall m addr t.
                      ( Ord addr
                      , Ord (Async m ())
                      )
                   => addr
                   -> Async m ()
                   -> PeerStates m addr t
                   -> PeerStates m addr t
unregisterProducer _addr _tid ps@ThrowException{} = ps
unregisterProducer addr  tid  (PeerStates peerStates) =
    PeerStates $ Map.alter fn addr peerStates
  where
    fn :: Maybe (PeerState m t) -> Maybe (PeerState m t)
    fn Nothing = Nothing
    fn (Just (HotPeer producers consumers)) =
        let producers' = tid `Set.delete` producers
        in if Set.null producers' && Set.null consumers
             then Nothing
             else Just (HotPeer producers' consumers)
    fn (Just ColdPeer) = Nothing
    fn (Just p@SuspendedPeer{}) = Just p
    fn (Just (SuspendedConsumer producers consT)) =
        Just (SuspendedConsumer (tid `Set.delete` producers) consT)


-- | Register consumer in 'PeerState'.  This is a partial function which
-- assumes that the 'PeerState' is 'HotPeer'.
--
registerConsumer :: forall m addr t.
                    ( Ord addr
                    , Ord (Async m ())
                    )
                 => addr
                 -> Async m ()
                 -> PeerStates m addr t
                 -> PeerStates m addr t
registerConsumer _addr _tid ps@ThrowException{} = ps
registerConsumer addr  tid  (PeerStates peerStates) =
    PeerStates $ Map.alter fn addr peerStates
  where
    fn :: Maybe (PeerState m t) -> Maybe (PeerState m t)
    fn  Nothing =
        Just (HotPeer Set.empty (Set.singleton tid))
    fn  (Just (HotPeer producers consumers)) =
        Just (HotPeer producers (tid `Set.insert` consumers))
    fn (Just ColdPeer) =
        Just (HotPeer Set.empty (Set.singleton tid))
    fn (Just ps) =
        -- registerConsumer on a suspended peer
        assert False $ Just ps


-- | Unregister consumer from a 'PeerState'.
--
unregisterConsumer :: forall m addr t.
                      ( Ord addr
                      , Ord (Async m ())
                      )
                   => addr
                   -> Async m ()
                   -> PeerStates m addr t
                   -> PeerStates m addr t
unregisterConsumer _addr _tid ps@ThrowException{} = ps
unregisterConsumer addr  tid (PeerStates peerStates) =
    PeerStates $ Map.alter fn addr peerStates
  where
    fn :: Maybe (PeerState m t) -> Maybe (PeerState m t)
    fn Nothing = Nothing
    fn (Just (HotPeer producers consumers)) =
      let consumers' = tid `Set.delete` consumers
      in if Set.null producers && Set.null consumers'
           then Nothing
           else Just (HotPeer producers consumers')
    fn (Just ColdPeer) = Nothing
    fn (Just ps) = Just ps


-- | Trace data for error policies
data ErrorPolicyTrace
  = ErrorPolicySuspendPeer (Maybe (ConnectionOrApplicationException SomeException)) !DiffTime !DiffTime
  -- ^ suspending peer with a given exception until
  | ErrorPolicySuspendConsumer (Maybe (ConnectionOrApplicationException SomeException)) !DiffTime
  -- ^ suspending consumer until
  | ErrorPolicyLocalNodeError (ConnectionOrApplicationException SomeException)
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

data WithAddr addr a = WithAddr {
      wiaAddr  :: !addr
    , wiaEvent :: !a
    }

instance (Show addr, Show a) => Show (WithAddr addr a) where
    show WithAddr { wiaAddr, wiaEvent } =
        printf "IP %s %s" (show wiaAddr) (show wiaEvent)
