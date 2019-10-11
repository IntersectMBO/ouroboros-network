{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IP subscription worker implentation.
module Ouroboros.Network.Subscription.Ip
    ( ipSubscriptionWorker
    , subscriptionWorker
    , SubscriptionTrace (..)
    , IPSubscriptionTarget (..)
    , ipSubscriptionTarget
    , WithIPList (..)

    -- * 'PeerState' STM transactions
    , BeforeConnect
    , runBeforeConnect
    , beforeConnectTx
    , completeApplicationTx
    , socketStateChangeTx
    , mainTx
    ) where


{- The parallel connection attemps implemented in this module is inspired by
 - RFC8305, https://tools.ietf.org/html/rfc8305 .
 -}

import           Control.Exception (SomeException (..))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadThrow
import           Control.Tracer
import           Data.Foldable (traverse_)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (DiffTime)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.PeerState
import           Ouroboros.Network.Subscription.Subscriber
import           Ouroboros.Network.Subscription.Worker

import           Data.Semigroup.Action

data IPSubscriptionTarget = IPSubscriptionTarget {
    -- | List of destinations to possibly connect to
      ispIps     :: ![Socket.SockAddr]
    -- | Number of parallel connections to keep actice.
    , ispValency :: !Int
    } deriving (Eq, Show)

-- | Spawns a subscription worker which will attempt to keep the specified
-- number of connections (Valency) active towards the list of IP addresses
-- given in IPSubscriptionTarget.
--
ipSubscriptionWorker
    :: forall a.
       Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr (Time IO))
    -> Maybe Socket.SockAddr
    -- ^ Local IPv4 address to use, Nothing indicates don't use IPv4
    -> Maybe Socket.SockAddr
    -- ^ Local IPv6 address to use, Nothing indicates don't use IPv6
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> [ErrorPolicy]
    -> (Time IO -> Socket.SockAddr -> a -> SuspendCommand DiffTime)
    -> IPSubscriptionTarget
    -> (Socket.Socket -> IO a)
    -> IO Void
ipSubscriptionWorker tracer errTracer tbl peerStatesVar localIPv4 localIPv6 connectionAttemptDelay errPolicies returnCallback ips k = do
    subscriptionWorker
            tracer'
            errTracer
            tbl
            peerStatesVar
            localIPv4
            localIPv6
            connectionAttemptDelay
            (pure $ ipSubscriptionTarget tracer' peerStatesVar $ ispIps ips)
            (ispValency ips)
            errPolicies
            returnCallback
            mainTx
            k
  where
    tracer' =  WithIPList localIPv4 localIPv6 (ispIps ips) `contramap` tracer

ipSubscriptionTarget :: forall m addr.
                        ( MonadSTM  m
                        , MonadTime m
                        , Ord addr
                        )
                     => Tracer m (SubscriptionTrace addr)
                     -> StrictTVar m (PeerStates m addr (Time m))
                     -> [addr]
                     -> SubscriptionTarget m addr
ipSubscriptionTarget tr peerStatesVar ips = go ips
  where
    go :: [addr]
       -> SubscriptionTarget m addr
    go [] = SubscriptionTarget $ pure Nothing
    go (a : as) = SubscriptionTarget $ do
      b <- runBeforeConnect peerStatesVar beforeConnectTx a
      if b
        then do
          traceWith tr $ SubscriptionTraceTryConnectToPeer a
          pure $ Just (a, go as)
        else do
          traceWith tr $ SubscriptionTraceSkippingPeer a
          getSubscriptionTarget $ go as


-- | Check state before connecting to a remote peer.  We will connect only if
-- it retuns 'True'.
--
type BeforeConnect m s addr = Time m -> addr -> s -> STM m (s, Bool)

-- | Run 'BeforeConnect' callback in a 'MonadTime' monad.
--
runBeforeConnect :: ( MonadSTM  m
                    , MonadTime m
                    )
                 => StrictTVar m s
                 -> BeforeConnect m s addr
                 -> addr
                 -> m Bool
runBeforeConnect sVar beforeConnect addr = do
    t <- getMonotonicTime
    atomically $ do
      (!s, b) <- readTVar sVar >>= beforeConnect t addr
      writeTVar sVar s
      pure b


-- | 'BeforeConnect' callback: it updates peer state and return boolean value
-- wheather to connect to it or not.  If a peer hasn't been recorded in
-- 'PeerStates', we add it and try to connect to it.
--
beforeConnectTx
  :: forall m addr.
     ( MonadSTM m
     , Ord addr
     , Ord (Time m)
     )
  => BeforeConnect m
      (PeerStates m addr (Time m))
      addr

beforeConnectTx _t _addr ps@ThrowException{} = pure (ps, False)

beforeConnectTx  t  addr (PeerStates s) =
    case alterAndLookup fn addr s of
      (s', mbb) -> pure (PeerStates s', fromMaybe False mbb)
  where
    fn :: Maybe (PeerState m (Time m))
       -> ( Bool
          , Maybe (PeerState m (Time m))
          )

    -- we see the peer for the first time; consider it as a good peer and
    -- try to connect to it.
    fn Nothing = ( True
                 , Just ColdPeer
                 )

    fn (Just p@ColdPeer{}) = ( True
                             , Just p
                             )

    fn (Just p@(HotPeer producers consumers))
      = if Set.null producers && Set.null consumers
        -- the peer has no registered producers nor consumers, thus it should
        -- be marked as a 'ColdPeer'
        then ( True
             , Just ColdPeer
             )
        else ( True
             , Just p
             )

    fn (Just p@(SuspendedConsumer producers consT)) =
      if consT < t
        then if Set.null producers
               -- the consumer is not suspended any longer, and it has no
               -- producers; thus it's a 'ColdPeer'.
               then (True, Just ColdPeer)
               else (True, Just (HotPeer producers Set.empty))
        else (False, Just p)

    fn (Just p@(SuspendedPeer prodT consT)) =
      if t < prodT `max` consT
          then if t < prodT `min` consT
                 then (False, Just p)
                 else if prodT < consT
                   then -- prodT ≤ t < consT
                        -- allow the remote peer to connect to us, but we're
                        -- still not allowed to connect to it.
                        (False, Just $ SuspendedConsumer Set.empty consT)
                   else -- consT ≤ t < prodT
                        -- the local consumer is suspended shorter than local
                        -- producer; In this case we suspend both until `prodT`.
                        -- This means we effectively make local consumer
                        -- errors more sevier than the ones which come from
                        -- a local producer.
                        (False, Just $ SuspendedPeer prodT prodT)

          -- the peer is not suspended any longer
          else (True, Just ColdPeer)


-- | 'CompleteApplication' callback
--
completeApplicationTx
  :: forall m addr a.
     ( MonadAsync  m
     , Ord addr
     , Ord (Async m ())
     , TimeMeasure (Time m)
     )
  => Tracer m (WithAddr addr ErrorPolicyTrace)
  -> (Time m -> addr -> a -> SuspendCommand DiffTime)
  -> [ErrorPolicy]
  -> CompleteApplication m
       (PeerStates m addr (Time m))
       addr
       a

-- the 'ResultQ' did not throw the exception yet; it should not happen.
completeApplicationTx _ _ _ _ ps@ThrowException{} = pure ( ps, pure () )

-- application returned; classify the return value and update the state.
completeApplicationTx tracer returnCallback _ (ApplicationResult t addr r) (PeerStates ps) =
  let cmd = returnCallback t addr r
      fn :: Maybe (PeerState m (Time m))
         -> ( Set (Async m ())
            , Maybe (PeerState m (Time m))
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
completeApplicationTx tracer  _ errPolicies (ApplicationError t addr e) ps =
  case evalErrorPolicies (AppException e) errPolicies of
    -- the error is not handled by any policy; we're not rethrowing the
    -- error from the main thread, we only trace it.  This will only kill
    -- the local consumer application.
    Nothing  -> pure ( ps
                     , traceWith tracer
                        (WithAddr addr
                          (ErrorPolicyUnhandledApplicationException
                            (SomeException e)))
                     )
    -- the error was classified; act with the 'SuspendCommand' on the state
    -- and find threads to cancel.
    Just cmd -> case runSuspendCommand t addr e cmd ps of
      (ps', threads) ->
        pure ( ps'
             , do
                traverse_ (traceWith tracer . WithAddr addr)
                          (traceErrorPolicy (Left $ AppException (SomeException e))
                                            cmd)
                traverse_ cancel threads
            )

-- we connected to a peer; this does not require to update the 'PeerState'.
completeApplicationTx _ _ _ (Connected _t  _addr) ps =
  pure ( ps, pure () )

-- error raised by the 'connect' call; we handle this in the same way as
-- application exceptions, the only difference is that we wrap the exception
-- with 'ConnException' type constructor.
completeApplicationTx tracer _ errPolicies (ConnectionError t addr e) ps =
  case evalErrorPolicies (ConnException e) errPolicies of
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
    Just cmd -> case runSuspendCommand t addr e cmd ps of
      (ps', threads) ->
        pure ( ps'
             , do
                 traverse_ (traceWith tracer . WithAddr addr)
                           (traceErrorPolicy (Left $ ConnException (SomeException e)) cmd)
                 traverse_ cancel threads
             )


-- when creating a new socket: register consumer thread
-- when tearing down a socket: unregister consumer thread
socketStateChangeTx
    :: ( Ord addr
       , Show addr
       )
    => SocketStateChange IO
        (PeerStates IO addr (Time IO))
        addr

socketStateChangeTx (CreatedSocket addr thread) ps =
  pure (registerConsumer addr thread ps)

socketStateChangeTx ClosedSocket{} ps@ThrowException{} =
  pure ps

socketStateChangeTx (ClosedSocket addr thread) ps =
  pure $ unregisterConsumer addr thread ps


-- | Main callback.  It throws an exception when the state becomes
-- 'ThrowException'.  This exception is thrown from the main thread.
--
mainTx :: ( MonadThrow m
          , MonadThrow (STM m)
          , MonadSTM m
          )
       => Main m (PeerStates m addr (Time m)) Void
mainTx (ThrowException e) = throwM e
mainTx PeerStates{}       = retry


-- | Like 'worker' but in 'IO'; It only instantiates local address selection.
--
subscriptionWorker
    :: Tracer IO (SubscriptionTrace Socket.SockAddr)
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> ConnectionTable IO Socket.SockAddr
    -> StateVar IO (PeerStates IO Socket.SockAddr (Time IO))

    -> Maybe Socket.SockAddr
    -- ^ local IPv4 address
    -> Maybe Socket.SockAddr
    -- ^ local IPv6 address
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> IO (SubscriptionTarget IO Socket.SockAddr)
    -- ^ subscription targets
    -> Int
    -- ^ valency
    -> [ErrorPolicy]
    -> (Time IO -> Socket.SockAddr -> a -> SuspendCommand DiffTime)
    -> Main IO (PeerStates IO Socket.SockAddr (Time IO)) x
    -- ^ main callback
    -> (Socket.Socket -> IO a)
    -- ^ application to run on each connection
    -> IO x
subscriptionWorker
  tracer errTracer tbl sVar mbLocalIPv4 mbLocalIPv6
  connectionAttemptDelay getTargets valency errPolicies returnCallback main k =
    worker tracer
           tbl
           sVar
           ioSocket
           socketStateChangeTx
           (completeApplicationTx errTracer returnCallback errPolicies)
           main
           mbLocalIPv4 mbLocalIPv6
           selectAddr connectionAttemptDelay
           getTargets valency k

  where
    selectAddr :: Socket.SockAddr
               -> Maybe Socket.SockAddr
               -- ^ IPv4 address
               -> Maybe Socket.SockAddr
               -- ^ IPv6 address
               -> Maybe Socket.SockAddr
    selectAddr Socket.SockAddrInet{} (Just localAddr) _ = Just localAddr
    selectAddr Socket.SockAddrInet6{} _ (Just localAddr) = Just localAddr
    selectAddr _ _ _ = Nothing

data WithIPList a = WithIPList {
      wilIPv4  :: !(Maybe Socket.SockAddr)
    , wilIPv6  :: !(Maybe Socket.SockAddr)
    , wilDsts  :: ![Socket.SockAddr]
    , wilEvent :: !a
    }

instance (Show a) => Show (WithIPList a) where
    show (WithIPList Nothing (Just wilIPv6) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show wilIPv6) (show wilDsts) (show wilEvent)
    show (WithIPList (Just wilIPv4) Nothing wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show wilIPv4) (show wilDsts) (show wilEvent)
    show WithIPList {wilIPv4, wilIPv6, wilDsts, wilEvent}
      = printf  "IPs: %s %s %s %s" (show wilIPv4) (show wilIPv6)
                                   (show wilDsts) (show wilEvent)
