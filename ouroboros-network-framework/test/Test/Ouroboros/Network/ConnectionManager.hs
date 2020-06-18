{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Test.Ouroboros.Network.ConnectionManager
  ( tests
  ) where

import           Prelude hiding (read)

import           Control.Exception (assert)
import           Control.Monad (forever, (>=>))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), nullTracer)

import           GHC.Generics

import           Data.Functor (($>))
import           Data.Foldable (traverse_)
import           Data.List (find, intercalate)
import           Data.Maybe (isJust, mapMaybe)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)
import           Quiet

import           Network.Mux.Types

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.Snocket (Snocket (..), Accept (..), Accepted (..),
                   AddressFamily(TestFamily), TestAddress (..))
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Server.RateLimiting
import qualified Ouroboros.Network.Server2.ControlChannel as ControlChannel

import           Ouroboros.Network.Testing.Utils (Delay (..))


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.ConnectionManager"
  [ testProperty "fixupConnections"        prop_fixupConnections
  , testProperty "Connections shrinker"    prop_shrinker_Connections
  , testProperty "pure connection manager" prop_connectionManager
  ]


newtype TestPeerAddr = TestPeerAddr { getPeerAddr :: Int }
  deriving stock (Generic, Eq, Ord)
  deriving Show via Quiet TestPeerAddr

instance CoArbitrary TestPeerAddr where
instance Function TestPeerAddr where

instance Arbitrary TestPeerAddr where
    arbitrary =
      TestPeerAddr <$>
        -- from one side we want a small address pool (this makes a greater
        -- chance of reusing a connection), but we also want to allow
        -- variability
        frequency [ (66, elements [1..4])
                  , (33, suchThat arbitrary (\a -> a > 5 && a < 25) )
                  ]

    shrink (TestPeerAddr addr) = TestPeerAddr . getPositive
                             <$> shrink (Positive addr)


type Addr = TestAddress TestPeerAddr

newtype ArbDataFlow = ArbDataFlow { getDataFlowType :: DataFlow }
  deriving stock Generic
  deriving Show via Quiet ArbDataFlow

instance Arbitrary ArbDataFlow where
    arbitrary = ArbDataFlow <$>
                  elements [ Unidirectional
                           , Duplex ]
    shrink (ArbDataFlow Duplex)         = [ArbDataFlow Unidirectional]
    shrink (ArbDataFlow Unidirectional) = []


data ConnState = UnconnectedState
               | ConnectedState
               | AcceptedState
               | ListeningState
               | ClosedState

data Bound = Bound | NotBound

data FDState = FDState {
    fdLocalAddress    :: TestPeerAddr,
    fdRemoteAddress   :: Maybe TestPeerAddr,
    fdConnectionState :: ConnState,
    fdBound           :: Bound
  }

newtype FD m = FD (StrictTVar m FDState)

-- TODO: use `IOException` instead
data SnocketErr =
      InvalidArgument
    | AcceptErr
    | ConnectErr
    | BindErr
    | ListenErr
  deriving Show

instance Exception SnocketErr


-- | A pure snocket.  Reading always blocks forever, writing is immediate.
--
-- This very roughly captures socket semantics, but it's good enough for the
-- time being for the testing we want to do.  In particular this does not rule
-- out situations which the kernel would forbid, e.g. have the two connections
-- with the same four-tuples.
--
-- TODO: 'connect' should use a random delay.
--
pureSnocket :: forall m.
               ( MonadDelay m
               , MonadMonotonicTime m
               , MonadSTM   m
               , MonadThrow m
               , MonadThrow (STM m)
               )
            => [TestPeerAddr]
            -- ^ list of remote addresses which connect to us
            -> Snocket m (FD m) Addr
pureSnocket remoteAddresses =
    Snocket {
      getLocalAddr,
      getRemoteAddr,
      addrFamily,
      open,
      openToConnect,
      connect,
      listen,
      accept,
      bind,
      close,
      reset = close,
      toBearer
    }
  where
    getLocalAddr (FD v) =
      TestAddress . fdLocalAddress <$> atomically (readTVar v)

    getRemoteAddr (FD v) = do
      mbRemote <- fdRemoteAddress <$> atomically (readTVar v)
      case mbRemote of
        Nothing   -> throwIO InvalidArgument
        Just addr -> pure (TestAddress addr)

    addrFamily _ = TestFamily

    open _ =
      FD <$>
        newTVarIO FDState {
            fdLocalAddress = TestPeerAddr 0,
            fdRemoteAddress = Nothing,
            fdConnectionState = UnconnectedState,
            fdBound = NotBound
          }

    openToConnect _ =
      FD <$>
        newTVarIO FDState {
            fdLocalAddress = TestPeerAddr 0,
            fdRemoteAddress = Nothing,
            fdConnectionState = UnconnectedState,
            fdBound = NotBound
          }

    connect (FD v) (TestAddress remoteAddr) =
      atomically $ do
        fds@FDState { fdConnectionState } <- readTVar v
        case fdConnectionState of
          UnconnectedState ->
            writeTVar v fds { fdRemoteAddress = Just remoteAddr
                            , fdConnectionState = ConnectedState }
          _ -> throwIO ConnectErr

    bind (FD v) (TestAddress localAddr) =
      atomically $ do
        fds@FDState { fdBound } <- readTVar v
        case fdBound of
          NotBound -> writeTVar v fds { fdLocalAddress = localAddr
                                      , fdBound = Bound
                                      }
          Bound -> throwIO BindErr

    accept :: FD m -> Accept m (FD m) Addr
    accept (FD v) = Accept $ go remoteAddresses
      where
        go [] = pure (AcceptFailure (toException AcceptErr), Accept $ go [])
        go (x : xs) = do
          v' <- atomically $ do
            FDState { fdLocalAddress = localAddr } <- readTVar v
            newTVar FDState {
                        -- this is not adequate
                        fdLocalAddress = localAddr,
                        fdRemoteAddress = Just x,
                        fdConnectionState = AcceptedState,
                        fdBound = Bound
                      }
          pure (Accepted (FD v') (TestAddress x), Accept $ go xs)

    toBearer _ _ _ =
      MuxBearer {
          write   = \_ _ -> getMonotonicTime,
          read    = \_ -> forever (threadDelay 3600),
          sduSize = 1500
        }

    listen (FD v) = atomically $ do
      fds@FDState{ fdConnectionState } <- readTVar v
      case fdConnectionState of
        UnconnectedState ->
          writeTVar v (fds { fdConnectionState = ListeningState })
        _ -> throwIO ListenErr

    close (FD v) =
      atomically $ modifyTVar v (\fds -> fds { fdConnectionState = ClosedState })

-- | Connection handle.
data Handle = Handle

-- | Version
data Version = Version DataFlow

-- | A connection handler which does not do any effects, other than blocking
-- idefinitely.
--
-- We pass 'ConnDescription's for outbound and inbound connections via a mutable
-- variable which contains a map of lists of connections.  We pop from each list
-- when we connected, but we also need to pop when we tried to include
-- a connection and connection manager thrown 'ConnectionManagerError'.
--
mkConnectionHandler :: ( MonadLabelledSTM m
                       , MonadFork        m
                       , MonadTimer       m
                       )
                    => [ConnDescription TestPeerAddr]
                    -- ^ list of all connections
                    -> m (ConnectionHandler
                            InitiatorResponderMode
                            handlerTrace
                            (TestAddress TestPeerAddr)
                            Handle
                            Void
                            Version
                            m)
mkConnectionHandler conns = do
    outboundV <-
      newTVarIO
        (Map.fromListWith (flip (++))
          [ (connAddr conn, [conn])
          | conn@ConnDescription { connProvenance = Outbound, connReused } <- conns
          , not connReused
          ])
    labelTVarIO outboundV "tv-outbound"
    inboundV <-
      newTVarIO
        (Map.fromListWith (flip (++))
          [ (connAddr conn, [conn])
          | conn@ConnDescription { connProvenance = Inbound } <- conns
          ])
    labelTVarIO inboundV "tv-inbound"
    activeVar <- newTVarIO True 

    return $ ConnectionHandler {
        connectionHandler =
          WithInitiatorResponderMode
          (\v _ ConnectionId { remoteAddress } _ -> Action
            (do labelThisThread ("outbound-handler-"
                                  ++ show (getPeerAddr (getTestAddress remoteAddress)))
                conn <-
                  atomically $ do
                    outs <- readTVar outboundV
                    let (conn, outs') = Map.alterF
                          (\mbconns -> case mbconns of
                              Nothing -> error "outbound connection handler: invariant violation"
                              Just conns' ->
                                assert (not (null conns')) $
                                (head conns', Just $ tail conns'))
                          (getTestAddress remoteAddress)
                          outs
                    writeTVar outboundV outs'
                    return conn
                threadDelay (connHandshakeDelay conn)
                atomically (writePromise v
                             (Right ( Handle
                                    , Version (connDataFlow conn)
                                    )))
                threadDelay (connActiveDelay conn)
                atomically (writeTVar activeVar False))
            id)
          (\v _ ConnectionId { remoteAddress } _ -> Action
            (do labelThisThread ("inbound-handler-"
                                  ++ show (getPeerAddr (getTestAddress remoteAddress)))
                conn <-
                  atomically $ do
                    ins <- readTVar inboundV
                    let (conn, ins') = Map.alterF
                          (\mbconns -> case mbconns of
                              Nothing -> error "inbound connection handler: invariant violation"
                              Just conns' ->
                                assert (not (null conns'))
                                (head conns', Just $ tail conns'))
                          (getTestAddress remoteAddress)
                          ins
                    writeTVar inboundV ins'
                    return conn
                threadDelay (connHandshakeDelay conn)
                atomically (writePromise v
                             (Right ( Handle
                                    , Version (connDataFlow conn)
                                    )))
                threadDelay (connActiveDelay conn)
                atomically (writeTVar activeVar False))
            id),
        connectionIdle = \_ ->
          readTVar activeVar >>= ($> Inactive) . check . not
      }


data ConnDescription addr = ConnDescription {
      -- | remote address of the connection with provenance
      connProvenance     :: Provenance,
      connAddr           :: addr,
      connDataFlow       :: DataFlow,
      -- | how long it will take before accepting, connecting after previous
      -- conection is accepted / created.
      connDelay          :: DiffTime,
      connHandshakeDelay :: DiffTime,
      connActiveDelay    :: DiffTime,
      -- | 'connReused' field is set by `fixupConnections', where we know if
      -- a connection will be reused or not.  We need it, since for a reused
      -- connections a connection handler does not run, hence we need to filter
      -- these out.
      connReused         :: Bool
   } 
  deriving stock (Eq, Generic)
  deriving Show via Quiet (ConnDescription addr)

connLifeTime :: ConnDescription addr -> DiffTime
connLifeTime ConnDescription { connHandshakeDelay, connActiveDelay } =
    connHandshakeDelay + connActiveDelay

instance Arbitrary addr
      => Arbitrary (ConnDescription addr) where
    arbitrary = ConnDescription
            <$> elements [Inbound, Outbound]
            <*> arbitrary
            <*> elements [Unidirectional, Duplex]
            <*> (getDelay <$> arbitrary)
            <*> (getDelay <$> arbitrary)
            <*> (getDelay <$> arbitrary)
            <*> (pure False)

    shrink ConnDescription { connProvenance,
                             connAddr,
                             connDelay,
                             connDataFlow,
                             connHandshakeDelay,
                             connActiveDelay,
                             connReused } =
      (case connProvenance of
        Outbound -> []
        Inbound -> [ConnDescription { connProvenance = Outbound,
                                      connAddr,
                                      connDataFlow,
                                      connDelay,
                                      connHandshakeDelay,
                                      connActiveDelay,
                                      connReused }])
      ++
      [ ConnDescription { connProvenance,
                          connAddr = connAddr',
                          connDelay,
                          connDataFlow,
                          connHandshakeDelay,
                          connActiveDelay,
                          connReused }
      | connAddr' <- shrink connAddr ]
      ++
      (case connDataFlow of
        Unidirectional -> []
        Duplex -> [ConnDescription { connProvenance,
                                     connAddr,
                                     connDataFlow = Unidirectional,
                                     connDelay,
                                     connHandshakeDelay,
                                     connActiveDelay,
                                     connReused }])
      ++
      [ ConnDescription { connProvenance,
                          connAddr,
                          connDataFlow,
                          connDelay = connDelay',
                          connHandshakeDelay,
                          connActiveDelay,
                          connReused }
      | Delay connDelay' <- shrink (Delay connDelay) ]
      ++
      [ ConnDescription { connProvenance,
                          connAddr,
                          connDataFlow,
                          connDelay,
                          connHandshakeDelay = connHandshakeDelay',
                          connActiveDelay,
                          connReused }
      | Delay connHandshakeDelay' <- shrink (Delay connHandshakeDelay) ]
      ++
      [ ConnDescription { connProvenance,
                          connAddr,
                          connDataFlow,
                          connDelay,
                          connHandshakeDelay,
                          connActiveDelay = connActiveDelay',
                          connReused }
      | Delay connActiveDelay' <- shrink (Delay connActiveDelay) ]



-- | Generator for timed instructions when to connect or accept a conection from
-- some address @a@.
--
-- This generator models tcp connection where the local side binds to some port,
-- with the assumption that connections live forever, but it allows to have
-- outbound connections while an inbound connection is alive.  This allows to
-- test that duplex connections are reused.
--
newtype Connections addr =
    Connections
      -- | When to accept / connect, how long it takes to negotiate
      -- a connection, how long the connection is alive afterwards.
      [ConnDescription addr]
  deriving newtype Show


-- | Like provenance, but allows to track duplex connections too.
--
data InOut = In | Out | InOut
  deriving (Show, Eq)


-- | Enforce connection manager constraints, which are weaker than tcp
-- constraint on connections: we allow to make outbound connection when there
-- exists an inbound connection.
--
fixupConnections :: forall addr. (Ord addr)
                 => Bool
                 -- ^ if 'True' also exlude forbidden connections, e.g. outbound
                 -- connections when an inbound connection exists and it was
                 -- negotiated as unidirectional one.
                 -> [ConnDescription addr]
                 -> [ConnDescription addr]
fixupConnections ff = go 0 Map.empty
  where
    go :: DiffTime
       -> Map addr (DiffTime, InOut, DataFlow)
       -- ^ time until which the connection will run, provenance and data flow
       -- type
       -> [ConnDescription addr]
       -> [ConnDescription addr]
    go _ _ [] = []
    go t m0 (a : as) =
      let -- remove expired entries, 'ct' marks the end of life time of
          -- a connection.
          m = Map.filter (\(ttl, _pr, _df) -> ttl >= t) m0
      in case connProvenance a of
        -- provenance of the new connection
        Outbound ->
          case Map.lookup (connAddr a) m of
            -- state of a possibly existing connection
            Nothing ->
                a { connReused = False }
              : go (t + connDelay a)
                   (Map.insert (connAddr a)
                               ( t + connDelay a + connLifeTime a,
                                 Out,
                                 connDataFlow a )
                               m)
                   as

            Just (ttl, In, df)
              -- We exlude outbound unidirectional connection when there is
              -- ongoing inbound duplex connection.  That's a limitation of the
              -- test.
              | Unidirectional <- connDataFlow a
              , Duplex <- df
              , ttl >= t + connDelay a
              -> go t m as

              -- We allow to make an outbound connection when an inbound
              -- connection already exists: connection manager will reuse the
              -- existing connection.  We only filter them out if 'ff' is 'True'
              -- and the original connections is unidirectional.  This allows us
              -- to test weather connection manager excludes such connections.
              | True <- ff
              , Unidirectional <- df
              , ttl >= t + connDelay a
              -> go (t + connDelay a) m as

              | otherwise
              ->  let reuse = ttl >= t + connDelay a in
                  a { connReused = reuse }
                : go (t + connDelay a)
                     (Map.insert (connAddr a)
                                 ( max ttl (t + connDelay a + connLifeTime a),
                                   if reuse then InOut else Out,
                                   df )
                                 m) 
                     as

            Just (ttl, _io, _df) ->
              let d = max (connDelay a) (ttl - t + 2) in
                a { connDelay = d,
                    connReused = False }
              : go (t + d)
                   (Map.insert (connAddr a)
                               ( t + d + connLifeTime a,
                                 Out,
                                 connDataFlow a )
                               m) 
                   as

        Inbound  ->
          case Map.lookup (connAddr a) m of
            Nothing ->
                a { connReused = False }
              : go (t + connDelay a)
                   (Map.insert (connAddr a)
                               ( t + connDelay a + connLifeTime a,
                                 In,
                                 connDataFlow a )
                               m)
                   as

            --  next outbound or inbound conection can only be made after the current one
            --  finishes
            Just (ttl, _pr, _df) ->
              let d = max (connDelay a) (ttl - t + 1) in
                a { connDelay = d,
                    connReused = False }
              : go (t + d)
                   (Map.insert (connAddr a)
                               ( t + d + connLifeTime a,
                                 In,
                                 connDataFlow a )
                               m)
                   as


instance (Arbitrary a, Ord a)
      => Arbitrary (Connections a) where
    arbitrary = Connections
              . fixupConnections False
            <$> resize 200 (listOf1 arbitrary)

    shrink (Connections as) =
      [ Connections (fixupConnections False as')
      | as'@(_:_) <- shrinkList (const []) as
      ]


prop_fixupConnections :: [ConnDescription TestPeerAddr]
                      -> Property
prop_fixupConnections conns =
    -- fixupConnections is idempotent, but only when it is called with 'False'
    let fixed = fixupConnections False conns
    in fixed === fixupConnections False fixed

prop_shrinker_Connections :: Connections TestPeerAddr
                          -> Bool
prop_shrinker_Connections conns =
    all (\(Connections as) -> as == fixupConnections False as) (shrink conns)


type TestConnectionManagerTrace = ConnectionManagerTrace Addr ()

-- | This property interleaves inbound and outbound connections and then
-- verifies that:
--
-- * all threads forked by the connection manager are killed when the callback
--   exists
-- * it generates the expected trace
--
-- This test relies on the fact that 'Connections' exclude behaviour that throws
-- exceptions (see 'checkExceptions' for which exceptions are not allowed).
--
-- TODO: this test does not test all possible interactions in connection manager
-- state machine.  We should specify them using something like
-- `quickcheck-state-machine`.
--
prop_connectionManager
    :: Maybe (Negative Int)
    -- ^ local address, by using a nagative integer we force it to be
    -- different from any one from the list of remote addresses.
    -> Connections TestPeerAddr
    -- ^ A list of addresses to which we connect or which connect to us.  We use
    -- 'Blind' since we show the arguments using `counterexample` in a nicer
    -- way.
    -> Property
prop_connectionManager myAddress' (Connections conns) =
    let tr = runSimTrace experiment

        cmTrace :: [TestConnectionManagerTrace]
        cmTrace = selectTraceEventsDynamic tr

        forbiddenPred =
            isJust
          . find (\msg ->
                   case msg of { TrForbiddenConnection {} -> True; _ -> False })
          $ cmTrace

        numberOfReused =
            length
          . filter (\msg ->
                   case msg of { TrReusedConnection {} -> True; _ -> False })
          $ cmTrace
    in
      -- `selectTraceEventsDynamic`, can throw 'Failure', hence we run
      -- `traceResults` first.
      counterexample (intercalate "\n" . map show $ traceEvents tr) $
        case traceResult True tr of
          Left failure ->
            counterexample (displayException failure) False
          Right _ ->
            classify forbiddenPred "has a forbidden connection" $
              classify True ("number of reused connections: " ++ show numberOfReused) $
                verifyTrace cmTrace
  where
    inboundAddresses :: [TestPeerAddr]
    inboundAddresses = connAddr <$> filter (\c -> connProvenance c == Inbound) conns

    myAddress :: Maybe Addr
    myAddress = (\(Negative addr) -> TestAddress (TestPeerAddr addr)) <$> myAddress'

    verifyTrace :: [TestConnectionManagerTrace] -> Property
    verifyTrace tr0 =
          Map.foldlWithKey'
            (\p k t -> vrf k t .&&. p)
            (property True)
        . Map.fromListWith (flip (++))
        -- group traces by 'TestPeerAddr'
        . mapMaybe (\t -> (,[t]) <$> cmtPeerAddr t)
        $ tr0
      where
        vrf :: Addr -> [TestConnectionManagerTrace] -> Property
        vrf (TestAddress peerAddr) tr =

          let -- reconstruct list of connections from the trace
              fromTrace :: [(TestPeerAddr, Provenance, Maybe DataFlow)]
                        -> [TestConnectionManagerTrace]
                        -> [(TestPeerAddr, Provenance, Maybe DataFlow)]
              fromTrace as [] = reverse as
              fromTrace as (TrIncludedConnection pr connId : tr') = 
                fromTrace
                  (( getTestAddress (remoteAddress connId)
                   , pr
                   , Nothing
                   ) : as) tr'
              -- we can modify the head of the accumulator, since it contains
              -- only connctions with `peerAddr`
              fromTrace ((addr, pr, Nothing) : as) (TrNegotiatedConnection _ _ df : tr') =
                fromTrace ((addr, pr, Just df) : as) tr'
              fromTrace as (TrReusedConnection connId : tr') =
                fromTrace
                  (( getTestAddress (remoteAddress connId)
                   , Outbound
                   , Just Duplex
                   ) : as)
                  tr'
              fromTrace as (_ : tr') = fromTrace as tr'

          in counterexample (unlines $ map show tr0) $
             fromTrace [] tr
             ===
             ( map (\c -> (connAddr c, connProvenance c, Just (connDataFlow c)))
             . filter (\conn -> connAddr conn == peerAddr)
             $ connsWithoutForbidden )

    connsWithoutForbidden = fixupConnections True conns

    experiment :: forall s. IOSim s Int
    experiment = do
        labelThisThread "th-main"
        let tr :: Tracer (IOSim s) TestConnectionManagerTrace
            tr = Tracer traceM
        inbgovControlChannel <- ControlChannel.newControlChannel
        connectionHandler <- mkConnectionHandler conns
        withConnectionManager
          ConnectionManagerArguments {
              cmTracer = tr <> Tracer (say . show),
              cmMuxTracer = nullTracer,
              cmIPv4Address = myAddress,
              cmIPv6Address = Nothing,
              cmAddressType = \_ -> Just IPv4Address,
              cmSnocket,
              connectionDataFlow = \(Version df) -> df,
              cmPrunePolicy = simplePrunePolicy,
              cmConnectionsLimits = AcceptedConnectionsLimit {
                  acceptedConnectionsHardLimit = maxBound,
                  acceptedConnectionsSoftLimit = maxBound,
                  acceptedConnectionsDelay     = 0
                },
              cmWaitTimeTimeout = defaultWaitTimeTimeout,
              cmProtocolIdleTimeout = defaultProtocolIdleTimeout
            }
            connectionHandler
            (\_ -> HandshakeFailure)
            (InResponderMode inbgovControlChannel)
          $ \connectionManager -> do
            fd <- open cmSnocket TestFamily
            case myAddress of
              Just localAddr ->
                bind cmSnocket fd localAddr
              Nothing ->
                pure ()

            let go :: [Async (IOSim s) ()]
                   -> Accept (IOSim s) (FD (IOSim s)) Addr
                   -> [ConnDescription TestPeerAddr]
                   -> IOSim s [Async (IOSim s) ()]
                go threads _acceptOne [] = pure threads
                go threads (Accept acceptOne) (conn : conns') =
                  case connProvenance conn of
                    Outbound -> do
                      threadDelay (connDelay conn)
                      thread <-
                        async $ do
                          labelThisThread ("th-outbound-"
                                            ++ show (getPeerAddr $ connAddr conn))
                          let addr = TestAddress (connAddr conn)
                          _ <-
                            requestOutboundConnection
                              connectionManager addr
                          threadDelay (connActiveDelay conn)
                          _ <-
                            unregisterOutboundConnection
                              connectionManager addr
                          return ()
                      go (thread : threads) (Accept acceptOne) conns'
                    Inbound -> do
                      threadDelay (connDelay conn)
                      r <- acceptOne
                      case r of
                        (Accepted fd' _, acceptNext) -> do
                          thread <-
                            async $ do
                              labelThisThread ("th-inbound-"
                                                ++ show (getPeerAddr $ connAddr conn))
                              let addr = TestAddress (connAddr conn)
                              _ <-
                                includeInboundConnection
                                  connectionManager fd' addr
                              case connDataFlow conn of
                                Duplex -> do
                                  promotedToWarmRemote
                                    connectionManager addr $> ()
                                _ -> return ()
                              threadDelay (connActiveDelay conn)
                              _ <-
                                unregisterInboundConnection
                                  connectionManager addr
                              return ()
                          go (thread : threads) acceptNext conns'
                        (AcceptFailure err, _acceptNext) ->
                          throwIO err

            -- run poor man's server which just reads the control channel,
            -- otherwise it would block if there are more than 10 connections.
            forever (atomically (ControlChannel.readMessage inbgovControlChannel) $> ())
              `race_`
              (do threads <- go [] (accept cmSnocket fd) conns
                  -- awaits until all 'Promise's are resolved (or throw an exception)
                  traverse_ (waitCatch >=> checkException) threads)

            atomically $ numberOfConnections connectionManager

    cmSnocket :: forall s. Snocket (IOSim s) (FD (IOSim s)) Addr
    cmSnocket = pureSnocket inboundAddresses

    checkException :: Either SomeException a -> IOSim s ()
    checkException Right {}   = pure ()
    checkException (Left err) =
      case fromException err :: Maybe (ConnectionManagerError Addr) of
        Nothing                        -> throwIO err
        -- 'ConnectionExists', 'ImpossibleConnection' and 'ConnectionFailure'
        -- are excluded by the of generator.  On any of these exception the test
        -- will fail.
        Just e@ConnectionExists {}     -> throwIO e
        Just e@ImpossibleConnection {} -> throwIO e
        Just e@ImpossibleState {}      -> throwIO e
        Just e@ForbiddenOperation {}   -> throwIO e
        Just e@UnknownPeer {}          -> throwIO e

        -- If 'ForbiddenConnection' is thrown we let the test continue.
        Just ForbiddenConnection {}    -> pure ()
        Just ConnectionTerminating {}  -> pure ()
        Just ConnectionTerminated {}   -> pure ()


--
-- Utils
--

cmtPeerAddr :: ConnectionManagerTrace peerAddr a -> Maybe peerAddr
cmtPeerAddr (TrIncludedConnection _ connId)     = Just (remoteAddress connId)
cmtPeerAddr (TrNegotiatedConnection _ connId _) = Just (remoteAddress connId)
cmtPeerAddr (TrConnect _ peerAddr)              = Just peerAddr
cmtPeerAddr (TrConnectError _ peerAddr _)       = Just peerAddr
cmtPeerAddr (TrReusedConnection connId)         = Just (remoteAddress connId)
cmtPeerAddr (TrConnectionTerminating _ connId)  = Just (remoteAddress connId)
cmtPeerAddr (TrConnectionTerminated _ peerAddr) = Just peerAddr
cmtPeerAddr (TrConnectionHandler connId _)      = Just (remoteAddress connId)
cmtPeerAddr  TrShutdown                         = Nothing
cmtPeerAddr (TrConnectionExists _ peerAddr)     = Just peerAddr
cmtPeerAddr (TrForbiddenConnection connId)      = Just (remoteAddress connId)
cmtPeerAddr (TrImpossibleConnection connId)     = Just (remoteAddress connId)
cmtPeerAddr (TrConnectionFailure connId)        = Just (remoteAddress connId)
cmtPeerAddr (TrConnectionNotFound _ peerAddr)   = Just peerAddr
cmtPeerAddr (TrForbiddenOperation peerAddr _)   = Just peerAddr
cmtPeerAddr (TrDemotedToColdLocal connId _)     = Just (remoteAddress connId)
cmtPeerAddr (TrPruneConnections _)              = Nothing
cmtPeerAddr (TrCleanup connId)                  = Just (remoteAddress connId)
cmtPeerAddr (TrConnectionExit connId _)         = Just (remoteAddress connId)
cmtPeerAddr (TrIncludeConnection _ peerAddr)    = Just peerAddr
cmtPeerAddr (TrUnregisterConnection _ peerAddr) = Just peerAddr
cmtPeerAddr (TrWaitIdle connId)                 = Just (remoteAddress connId)
