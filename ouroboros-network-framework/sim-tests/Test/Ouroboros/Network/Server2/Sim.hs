{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- for 'debugTracer'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Ouroboros.Network.Server2.Sim (tests) where

import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeAsyncException (..), SomeException (..))
import Control.Monad (replicateM)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Fix (MonadFix)
import Control.Monad.IOSim
import Control.Tracer (Tracer (..), nullTracer)

import Codec.Serialise.Class (Serialise)
import Data.Bifoldable
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import Data.Dynamic (fromDynamic)
import Data.Foldable (foldMap')
import Data.Functor (void, ($>), (<&>))
import Data.List as List (delete, foldl', intercalate, nub, (\\))
import Data.List.Trace qualified as Trace
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as OrdPSQ
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import System.Random (StdGen, mkStdGen, split)

import Text.Printf

import Test.QuickCheck
import Test.QuickCheck.Monoids (All (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Control.Concurrent.JobPool

import Network.Mux qualified as Mux

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.InboundGovernor qualified as IG
import Ouroboros.Network.InboundGovernor.State (ConnectionState (..))
import Ouroboros.Network.InboundGovernor.State qualified as IG
import Ouroboros.Network.Mux
import Ouroboros.Network.MuxMode
import Ouroboros.Network.Protocol.Handshake.Codec (noTimeLimitsHandshake,
           timeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Server2 (RemoteTransitionTrace)
import Ouroboros.Network.Snocket (Snocket, TestAddress (..))
import Ouroboros.Network.Snocket qualified as Snocket

import Simulation.Network.Snocket

import Ouroboros.Network.Testing.Data.AbsBearerInfo hiding (delay)
import Ouroboros.Network.Testing.Utils (WithName (..), WithTime (..),
           genDelayWithPrecision, nightlyTest, sayTracer, tracerWithTime)

import Ouroboros.Network.Test.Orphans ()
import Test.Simulation.Network.Snocket hiding (tests)

import Ouroboros.Network.ConnectionManager.Test.Experiments
import Ouroboros.Network.ConnectionManager.Test.Timeouts
import Ouroboros.Network.ConnectionManager.Test.Utils
           (abstractStateIsFinalTransition, allValidTransitionsNames,
           validTransitionMap, verifyAbstractTransition,
           verifyAbstractTransitionOrder)
import Ouroboros.Network.InboundGovernor.Test.Utils
           (allValidRemoteTransitionsNames, remoteStrIsFinalTransition,
           validRemoteTransitionMap, verifyRemoteTransition,
           verifyRemoteTransitionOrder)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network"
  [ testGroup "ConnectionManager"
    [ testProperty "valid transitions"      prop_connection_manager_valid_transitions
    , nightlyTest $ testProperty "valid transitions (racy)"
                    prop_connection_manager_valid_transitions_racy
    , testProperty "valid transition order" prop_connection_manager_valid_transition_order
    , nightlyTest $ testProperty "valid transition order (racy)"
                    prop_connection_manager_valid_transition_order_racy
    , testProperty "transitions coverage"   prop_connection_manager_transitions_coverage
    , testProperty "no invalid traces"      prop_connection_manager_no_invalid_traces
    , testProperty "counters"               prop_connection_manager_counters
    , testProperty "pruning"                prop_connection_manager_pruning
    ]
  , testGroup "InboundGovernor"
    [ testProperty "valid transitions"      prop_inbound_governor_valid_transitions
    , testProperty "valid transition order" prop_inbound_governor_valid_transition_order
    , testProperty "transitions coverage"   prop_inbound_governor_transitions_coverage
    , testProperty "no invalid traces"      prop_inbound_governor_no_invalid_traces
    , testProperty "no unsupported state"   prop_inbound_governor_no_unsupported_state
    , testProperty "pruning"                prop_inbound_governor_pruning
    , testProperty "counters"               prop_inbound_governor_counters
    , testProperty "InboundGovernorState"   prop_inbound_governor_state
    , testProperty "matured peers"          prop_inbound_governor_maturedPeers
    , testProperty "timeouts enforced"      prop_timeouts_enforced
    ]
  , testGroup "Server2"
    [ testProperty "unidirectional Sim"     prop_unidirectional_Sim
    , testProperty "bidirectional Sim"      prop_bidirectional_Sim
    , testProperty "never above hardlimit"  prop_never_above_hardlimit
    , testGroup      "accept errors"
      [ testProperty "ConnectionAborted"
                    (flip unit_server_accept_error IOErrConnectionAborted)
      , testProperty "ResourceExhausted"
                    (flip unit_server_accept_error IOErrResourceExhausted)
      ]
    ]
  , testProperty "connection terminated when negotiating"
                 unit_connection_terminated_when_negotiating
  , testGroup "generators"
    [ testProperty "MultiNodeScript" prop_generator_MultiNodeScript
    ]
  ]


--
-- Server tests
--

prop_unidirectional_Sim :: Fixed Int
                        -> ClientAndServerData Int
                        -> Property
prop_unidirectional_Sim (Fixed rnd) clientAndServerData =
  simulatedPropertyWithTimeout 7200 $
    withSnocket nullTracer
                noAttenuation
                Map.empty
      $ \snock _ ->
        bracket (Snocket.open snock Snocket.TestFamily)
                (Snocket.close snock) $ \fd -> do
          Snocket.bind   snock fd serverAddr
          Snocket.listen snock fd
          unidirectionalExperiment (mkStdGen rnd) simTimeouts snock makeFDBearer mempty fd clientAndServerData
  where
    serverAddr = Snocket.TestAddress (0 :: Int)

prop_bidirectional_Sim :: Fixed Int
                       -> ClientAndServerData Int
                       -> ClientAndServerData Int
                       -> Property
prop_bidirectional_Sim (Fixed rnd) data0 data1 =
  simulatedPropertyWithTimeout 7200 $
    withSnocket sayTracer
                noAttenuation
                Map.empty
                $ \snock _ ->
      bracket ((,) <$> Snocket.open snock Snocket.TestFamily
                   <*> Snocket.open snock Snocket.TestFamily)
              (\ (socket0, socket1) -> Snocket.close snock socket0 >>
                                       Snocket.close snock socket1)
        $ \ (socket0, socket1) -> do
          let addr0 = Snocket.TestAddress (0 :: Int)
              addr1 = Snocket.TestAddress 1
          Snocket.bind   snock socket0 addr0
          Snocket.bind   snock socket1 addr1
          Snocket.listen snock socket0
          Snocket.listen snock socket1
          bidirectionalExperiment False  (mkStdGen rnd)simTimeouts snock
                                        makeFDBearer
                                        (\_ -> pure ())
                                        socket0 socket1
                                        addr0 addr1
                                        data0 data1

--
-- Multi-node experiment
--

-- | A test case for the multi-node property contains a sequence of connection
-- events. The `DiffTime` in each constructor is relative to the previous event
-- in the sequence.
data ConnectionEvent req peerAddr
  = StartClient DiffTime peerAddr
    -- ^ Start a new client at the given address
  | StartServer DiffTime peerAddr req
    -- ^ Start a new server at the given address
  | InboundConnection DiffTime peerAddr
    -- ^ Create a connection from client or server with the given address to the central server.
  | OutboundConnection DiffTime peerAddr
    -- ^ Create a connection from the central server to another server.
  | InboundMiniprotocols DiffTime peerAddr (TemperatureBundle [req])
    -- ^ Run a bundle of mini protocols on the inbound connection from the given address.
  | OutboundMiniprotocols DiffTime peerAddr (TemperatureBundle [req])
    -- ^ Run a bundle of mini protocols on the outbound connection to the given address.
  | CloseInboundConnection DiffTime peerAddr
    -- ^ Close an inbound connection.
  | CloseOutboundConnection DiffTime peerAddr
    -- ^ Close an outbound connection.
  | ShutdownClientServer DiffTime peerAddr
    -- ^ Shuts down a client/server (simulates power loss)
  deriving (Show, Functor)

-- | A sequence of connection events that make up a test scenario for `prop_multinode_Sim`.
data MultiNodeScript req peerAddr = MultiNodeScript
  { mnsEvents         :: [ConnectionEvent req peerAddr]
  , mnsAttenuationMap :: Map peerAddr
                             (Script AbsBearerInfo)
  }
  deriving (Show)

-- | A sequence of connection events that make up a test scenario for `prop_multinode_Sim_Pruning`.
-- This test optimizes for triggering pruning.
data MultiNodePruningScript req = MultiNodePruningScript
  { mnpsAcceptedConnLimit :: AcceptedConnectionsLimit
    -- ^ Should yield small values to trigger pruning
    -- more often
  , mnpsEvents            :: [ConnectionEvent req TestAddr]
  , mnpsAttenuationMap    :: Map TestAddr
                                 (Script AbsBearerInfo)
  }
  deriving Show

-- | To generate well-formed scripts we need to keep track of what nodes are
-- started and what connections they've made.
--
-- NOTE: this does not track failures, e.g. `requestOutboundConnection` when
-- there's already a `Unidirectional` inbound connection (i.e.
-- a `ForbiddenOperation`).
--
data ScriptState peerAddr = ScriptState { startedClients      :: [peerAddr]
                                        , startedServers      :: [peerAddr]
                                        , clientConnections   :: [peerAddr]
                                        , inboundConnections  :: [peerAddr]
                                        , outboundConnections :: [peerAddr] }

-- | Update the state after a connection event.
nextState :: Eq peerAddr => ConnectionEvent req peerAddr -> ScriptState peerAddr -> ScriptState peerAddr
nextState e s@ScriptState{..} =
  case e of
    StartClient             _ a   -> s{ startedClients      = a : startedClients }
    StartServer             _ a _ -> s{ startedServers      = a : startedServers }
    InboundConnection       _ a   -> s{ inboundConnections  = a : inboundConnections }
    OutboundConnection      _ a   -> s{ outboundConnections = a : outboundConnections }
    CloseInboundConnection  _ a   -> s{ inboundConnections  = delete a inboundConnections }
    CloseOutboundConnection _ a   -> s{ outboundConnections = delete a outboundConnections }
    InboundMiniprotocols{}        -> s
    OutboundMiniprotocols{}       -> s
    ShutdownClientServer    _ a   -> s{ startedClients      = delete a startedClients
                                     , startedServers       = delete a startedServers }

-- | Check if an event makes sense in a given state.
isValidEvent :: Eq peerAddr => ConnectionEvent req peerAddr -> ScriptState peerAddr -> Bool
isValidEvent e ScriptState{..} =
  case e of
    StartClient             _ a   -> notElem a (startedClients ++ startedServers)
    StartServer             _ a _ -> notElem a (startedClients ++ startedServers)
    InboundConnection       _ a   -> elem a (startedServers ++ startedClients) && notElem a inboundConnections
    OutboundConnection      _ a   -> elem a startedServers && notElem a outboundConnections
    CloseInboundConnection  _ a   -> elem a inboundConnections
    CloseOutboundConnection _ a   -> elem a outboundConnections
    InboundMiniprotocols    _ a _ -> elem a inboundConnections
    OutboundMiniprotocols   _ a _ -> elem a outboundConnections
    ShutdownClientServer    _ a   -> elem a (startedClients ++ startedServers)

-- This could be an Arbitrary instance, but it would be an orphan.
genBundle :: Arbitrary a => Gen (TemperatureBundle a)
genBundle = traverse id $ pure arbitrary

shrinkBundle :: Arbitrary a => TemperatureBundle a -> [TemperatureBundle a]
shrinkBundle (TemperatureBundle (WithHot hot) (WithWarm warm) (WithEstablished est)) =
  (shrink hot  <&> \ hot'  -> TemperatureBundle (WithHot hot') (WithWarm warm)  (WithEstablished est)) ++
  (shrink warm <&> \ warm' -> TemperatureBundle (WithHot hot)  (WithWarm warm') (WithEstablished est)) ++
  (shrink est  <&> \ est'  -> TemperatureBundle (WithHot hot)  (WithWarm warm)  (WithEstablished est'))

genAttenuationMap :: Ord peerAddr
                  => [ConnectionEvent req peerAddr]
                  -> Gen (Map peerAddr (Script AbsBearerInfo))
genAttenuationMap events = do
  let nodes = map
                (\ case
                  StartClient _ addr   -> pure addr
                  StartServer _ addr _ -> pure addr
                  _                    -> error "Impossible happened"
                )
            . filter
                (\ case
                  StartClient {} -> True
                  StartServer {} -> True
                  _              -> False
                )
            $ events

  size <- chooseInt (0, length nodes)
  nodeSample <- nub <$> replicateM size (oneof nodes)

  attenuationMap <- mapM (\addr -> do
                            script <- arbitrary
                            return (addr, script)
                        )
                        nodeSample

  return (Map.fromList attenuationMap)


instance (Arbitrary peerAddr, Arbitrary req, Ord peerAddr) =>
         Arbitrary (MultiNodeScript req peerAddr) where
  arbitrary = do
      Positive len <- scale ((* 2) . (`div` 3)) arbitrary
      events <- go (ScriptState [] [] [] [] []) (len :: Integer)
      attenuationMap <- genAttenuationMap events
      return (MultiNodeScript events attenuationMap)
    where
      -- Divide delays by 100 to avoid running in to protocol and SDU timeouts if waiting
      -- too long between connections and mini protocols.
      delay = frequency [(1, pure 0), (3, (/ 100) <$> genDelayWithPrecision 2)]

      go _ 0 = pure []
      go s@ScriptState{..} n = do
        event <- frequency $
                    [ (6,  StartClient             <$> delay <*> newClient)
                    , (6,  StartServer             <$> delay <*> newServer <*> arbitrary) ] ++
                    [ (4,  InboundConnection       <$> delay <*> elements possibleInboundConnections)        | not $ null possibleInboundConnections] ++
                    [ (4,  OutboundConnection      <$> delay <*> elements possibleOutboundConnections)       | not $ null possibleOutboundConnections] ++
                    [ (6,  CloseInboundConnection  <$> delay <*> elements inboundConnections)                | not $ null inboundConnections ] ++
                    [ (4,  CloseOutboundConnection <$> delay <*> elements outboundConnections)               | not $ null outboundConnections ] ++
                    [ (10, InboundMiniprotocols    <$> delay <*> elements inboundConnections  <*> genBundle) | not $ null inboundConnections ] ++
                    [ (8,  OutboundMiniprotocols   <$> delay <*> elements outboundConnections <*> genBundle) | not $ null outboundConnections ] ++
                    [ (4,  ShutdownClientServer    <$> delay <*> elements possibleStoppable)                 | not $ null possibleStoppable ]
        (event :) <$> go (nextState event s) (n - 1)
        where
          possibleStoppable  = startedClients ++ startedServers
          possibleInboundConnections  = (startedClients ++ startedServers) \\ inboundConnections
          possibleOutboundConnections = startedServers \\ outboundConnections
          newClient = arbitrary `suchThat` (`notElem` (startedClients ++ startedServers))
          newServer = arbitrary `suchThat` (`notElem` (startedClients ++ startedServers))

  shrink (MultiNodeScript events attenuationMap) = do
    events' <- makeValid <$> shrinkList shrinkEvent events
    attenuationMap' <- shrink attenuationMap
    return (MultiNodeScript events' attenuationMap')
    where
      makeValid = go (ScriptState [] [] [] [] [])
        where
          go _ [] = []
          go s (e : es)
            | isValidEvent e s = e : go (nextState e s) es
            | otherwise        = go s es

      shrinkDelay = map fromRational . shrink . toRational

      shrinkEvent (StartServer d a p) =
        (shrink p      <&> \ p' -> StartServer d  a p') ++
        (shrinkDelay d <&> \ d' -> StartServer d' a p)
      shrinkEvent (StartClient             d a) = shrinkDelay d <&> \ d' -> StartClient d' a
      shrinkEvent (InboundConnection       d a) = shrinkDelay d <&> \ d' -> InboundConnection  d' a
      shrinkEvent (OutboundConnection      d a) = shrinkDelay d <&> \ d' -> OutboundConnection d' a
      shrinkEvent (CloseInboundConnection  d a) = shrinkDelay d <&> \ d' -> CloseInboundConnection  d' a
      shrinkEvent (CloseOutboundConnection d a) = shrinkDelay d <&> \ d' -> CloseOutboundConnection d' a
      shrinkEvent (InboundMiniprotocols    d a r) =
        (shrinkBundle r <&> \ r' -> InboundMiniprotocols d  a r') ++
        (shrinkDelay  d <&> \ d' -> InboundMiniprotocols d' a r)
      shrinkEvent (OutboundMiniprotocols d a r) =
        (shrinkBundle r <&> \ r' -> OutboundMiniprotocols d  a r') ++
        (shrinkDelay  d <&> \ d' -> OutboundMiniprotocols d' a r)
      shrinkEvent (ShutdownClientServer d a) = shrinkDelay d <&> \ d' -> ShutdownClientServer d' a


prop_generator_MultiNodeScript :: MultiNodeScript Int TestAddr -> Property
prop_generator_MultiNodeScript (MultiNodeScript script _) =
    label ("Number of events: " ++ within_ 10 (length script))
  $ label ( "Number of servers: "
          ++ ( within_ 2
             . length
             . filter (\ case
                         StartServer {} -> True
                         _              -> False
                      )
                      $ script
             ))
  $ label ("Number of clients: "
          ++ ( within_ 2
             . length
             . filter (\ case
                         StartClient {} -> True
                         _              -> False
                      )
             $ script
             ))
  $ label ("Active connections: "
          ++ ( within_ 5
             . length
             . filter (\ case
                         InboundMiniprotocols {}  -> True
                         OutboundMiniprotocols {} -> True
                         _                        -> False)
             $ script
             ))
  $ label ("Closed connections: "
          ++ ( within_ 5
             . length
             . filter (\ case
                         CloseInboundConnection {}  -> True
                         CloseOutboundConnection {} -> True
                         _                          -> False)
             $ script
             ))
  $ label ("Number of shutdown connections: "
          ++ ( within_ 2
             . length
             . filter (\ case
                         ShutdownClientServer {} -> True
                         _                       -> False
                      )
             $ script
             ))
  $ True

-- | Max bound AcceptedConnectionsLimit
maxAcceptedConnectionsLimit :: AcceptedConnectionsLimit
maxAcceptedConnectionsLimit = AcceptedConnectionsLimit maxBound maxBound 0

-- | This Script has a percentage of events more favourable to trigger pruning
--   transitions. And forces a bidirectional connection between each server.
--   It also starts inbound protocols in order to trigger the:
--
--   'Connected',
--   'NegotiatedDuplexOutbound',
--   'PromotedToWarmDuplexRemote',
--   'DemotedToColdDuplexLocal'
--
--   transitions.
--
instance Arbitrary req =>
         Arbitrary (MultiNodePruningScript req) where
  arbitrary = do
    Positive len <- scale ((* 2) . (`div` 3)) arbitrary
    -- NOTE: Although we still do not enforce the configured hard limit to be
    -- strictly positive. We assume that the hard limit is always bigger than
    -- 0.
    Small hardLimit <- (`div` 10) <$> arbitrary
    softLimit <- chooseBoundedIntegral (hardLimit `div` 2, hardLimit)
    events <- go (ScriptState [] [] [] [] []) (len :: Integer)
    attenuationMap <- genAttenuationMap events
    return
      $ MultiNodePruningScript (AcceptedConnectionsLimit hardLimit softLimit 0)
                               events
                               attenuationMap
   where
     -- Divide delays by 100 to avoid running in to protocol and SDU timeouts
     -- if waiting too long between connections and mini protocols.
     delay = frequency [ (1,  pure 0)
                       , (16, (/ 10) <$> genDelayWithPrecision 2)
                       , (32, (/ 100) <$> genDelayWithPrecision 2)
                       , (16, (/ 1000) <$> genDelayWithPrecision 2)
                       ]
     go _ 0 = pure []
     go s@ScriptState{..} n = do
       event <-
         frequency $
           [ (1, StartClient <$> delay <*> newServer)
           , (16, StartServer <$> delay <*> newServer <*> arbitrary) ] ++
           [ (4, InboundConnection
                  <$> delay <*> elements possibleInboundConnections)
           | not $ null possibleInboundConnections ] ++
           [ (4, OutboundConnection
                  <$> delay <*> elements possibleOutboundConnections)
           | not $ null possibleOutboundConnections] ++
           [ (4, CloseInboundConnection
                  <$> delay <*> elements inboundConnections)
           | not $ null inboundConnections ] ++
           [ (20, CloseOutboundConnection
                  <$> delay <*> elements outboundConnections)
           | not $ null outboundConnections ] ++
           [ (16, InboundMiniprotocols
                  <$> delay <*> elements inboundConnections <*> genBundle)
           | not $ null inboundConnections ] ++
           [ (4, OutboundMiniprotocols
                  <$> delay <*> elements outboundConnections <*> genBundle)
           | not $ null outboundConnections ] ++
           [ (1, ShutdownClientServer
                  <$> delay <*> elements possibleStoppable)
           | not $ null possibleStoppable ]
       case event of
         StartServer _ c _ -> do
           inboundConnection <- InboundConnection <$> delay <*> pure c
           outboundConnection <- OutboundConnection <$> delay <*> pure c
           inboundMiniprotocols <- InboundMiniprotocols <$> delay
                                                       <*> pure c
                                                       <*> genBundle
           let events = [ event, inboundConnection
                        , outboundConnection, inboundMiniprotocols]
           (events ++) <$> go (List.foldl' (flip nextState) s events) (n - 1)

         _ -> (event :) <$> go (nextState event s) (n - 1)
       where
         possibleStoppable = startedClients ++ startedServers
         possibleInboundConnections  = (startedClients ++ startedServers)
                                       \\ inboundConnections
         possibleOutboundConnections = startedServers \\ outboundConnections
         newServer = arbitrary `suchThat` (`notElem` possibleStoppable)

  -- TODO: The shrinking here is not optimal. It works better if we shrink one
  -- value at a time rather than all of them at once. If we shrink to quickly,
  -- we could miss which change actually introduces the failure, and be lift
  -- with a larger counter example.
  shrink (MultiNodePruningScript
            (AcceptedConnectionsLimit hardLimit softLimit delay)
            events
            attenuationMap) =
    MultiNodePruningScript
        <$> (AcceptedConnectionsLimit
              <$> shrink hardLimit
              <*> shrink softLimit
              <*> pure delay)
        <*> (makeValid
            <$> shrinkList shrinkEvent events)
        <*> shrink attenuationMap
    where
      makeValid = go (ScriptState [] [] [] [] [])
        where
          go _ [] = []
          go s (e : es)
            | isValidEvent e s = e : go (nextState e s) es
            | otherwise        = go s es

      shrinkDelay = map fromRational . shrink . toRational

      shrinkEvent (StartServer d a p) =
        (shrink p      <&> \ p' -> StartServer d  a p') ++
        (shrinkDelay d <&> \ d' -> StartServer d' a p)
      shrinkEvent (StartClient             d a) =
        shrinkDelay d <&> \ d' -> StartClient d' a
      shrinkEvent (InboundConnection       d a) =
        shrinkDelay d <&> \ d' -> InboundConnection  d' a
      shrinkEvent (OutboundConnection      d a) =
        shrinkDelay d <&> \ d' -> OutboundConnection d' a
      shrinkEvent (CloseInboundConnection  d a) =
        shrinkDelay d <&> \ d' -> CloseInboundConnection  d' a
      shrinkEvent (CloseOutboundConnection d a) =
        shrinkDelay d <&> \ d' -> CloseOutboundConnection d' a
      shrinkEvent (InboundMiniprotocols    d a r) =
        (shrinkBundle r <&> \ r' -> InboundMiniprotocols d  a r') ++
        (shrinkDelay  d <&> \ d' -> InboundMiniprotocols d' a r)
      shrinkEvent (OutboundMiniprotocols d a r) =
        (shrinkBundle r <&> \ r' -> OutboundMiniprotocols d  a r') ++
        (shrinkDelay  d <&> \ d' -> OutboundMiniprotocols d' a r)
      shrinkEvent (ShutdownClientServer d a) =
        shrinkDelay d <&> \ d' -> ShutdownClientServer d' a

-- | Each node in the multi-node experiment is controlled by a thread responding to these messages.
data ConnectionHandlerMessage peerAddr req
  = NewConnection peerAddr
    -- ^ Connect to the server at the given address.
  | Disconnect peerAddr
    -- ^ Disconnect from the server at the given address.
  | RunMiniProtocols peerAddr (TemperatureBundle [req])
    -- ^ Run a bundle of mini protocols against the server at the given address (requires an active
    --   connection).
  | Shutdown
    -- ^ Shutdowns a server at the given address


data Name addr = Client addr
               | Node addr
               | MainServer
  deriving Eq

instance Show addr => Show (Name addr) where
    show (Client addr) = "client-" ++ show addr
    show (Node   addr) = "node-"   ++ show addr
    show  MainServer   = "main-server"


data ExperimentError =
      forall addr. (Typeable addr, Show addr) => NodeNotRunningException addr
    -- | forall addr. (Typeable addr, Show addr) => NoActiveConnection addr addr
    | SimulationTimeout

deriving instance Show ExperimentError
deriving instance Typeable ExperimentError
instance Exception ExperimentError where

-- | Run a central server that talks to any number of clients and other nodes.
multinodeExperiment
    :: forall peerAddr socket acc req resp m.
       ( ConnectionManagerMonad m
       , MonadAsync m
       , MonadDelay m
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
       , MonadSay m
       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       )
    => Tracer m (WithName (Name peerAddr)
                          (RemoteTransitionTrace peerAddr))
    -> Tracer m (WithName (Name peerAddr)
                          (AbstractTransitionTrace peerAddr))
    -> Tracer m (WithName (Name peerAddr)
                          (IG.Trace peerAddr))
    -> Tracer m (WithName (Name peerAddr)
                          (IG.Debug peerAddr DataFlowProtocolData))
    -> Tracer m (WithName (Name peerAddr)
                          (CM.Trace
                            peerAddr
                            (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)))
    -> StdGen
    -> Snocket m socket peerAddr
    -> Mux.MakeBearer m socket
    -> Snocket.AddressFamily peerAddr
    -- ^ either run the main node in 'Duplex' or 'Unidirectional' mode.
    -> peerAddr
    -> req
    -> DataFlow
    -> AcceptedConnectionsLimit
    -> MultiNodeScript req peerAddr
    -> m ()
multinodeExperiment inboundTrTracer trTracer inboundTracer debugTracer cmTracer
                    stdGen0 snocket makeBearer addrFamily serverAddr accInit
                    dataFlow0 acceptedConnLimit
                    (MultiNodeScript script _) =
  withJobPool $ \jobpool -> do
  stdGenVar <- newTVarIO stdGen0
  cc <- startServerConnectionHandler stdGenVar MainServer dataFlow0 [accInit] serverAddr jobpool
  loop stdGenVar (Map.singleton serverAddr [accInit]) (Map.singleton serverAddr cc) script jobpool
  where

    loop :: StrictTVar m StdGen
         -> Map.Map peerAddr acc
         -> Map.Map peerAddr (StrictTQueue m (ConnectionHandlerMessage peerAddr req))
         -> [ConnectionEvent req peerAddr]
         -> JobPool () m ()
         -> m ()
    loop _ _ _ [] _ = threadDelay 3600
    loop stdGenVar nodeAccs servers (event : events) jobpool =
      case event of

        StartClient delay localAddr -> do
          threadDelay delay
          cc <- startClientConnectionHandler stdGenVar (Client localAddr) localAddr jobpool
          loop stdGenVar nodeAccs (Map.insert localAddr cc servers) events jobpool

        StartServer delay localAddr nodeAcc -> do
          threadDelay delay
          cc <- startServerConnectionHandler stdGenVar (Node localAddr) Duplex [nodeAcc] localAddr jobpool
          loop stdGenVar (Map.insert localAddr [nodeAcc] nodeAccs) (Map.insert localAddr cc servers) events jobpool

        InboundConnection delay nodeAddr -> do
          threadDelay delay
          sendMsg nodeAddr $ NewConnection serverAddr
          loop stdGenVar nodeAccs servers events jobpool

        OutboundConnection delay nodeAddr -> do
          threadDelay delay
          sendMsg serverAddr $ NewConnection nodeAddr
          loop stdGenVar nodeAccs servers events jobpool

        CloseInboundConnection delay remoteAddr -> do
          threadDelay delay
          sendMsg remoteAddr $ Disconnect serverAddr
          loop stdGenVar nodeAccs servers events jobpool

        CloseOutboundConnection delay remoteAddr -> do
          threadDelay delay
          sendMsg serverAddr $ Disconnect remoteAddr
          loop stdGenVar nodeAccs servers events jobpool

        InboundMiniprotocols delay nodeAddr reqs -> do
          threadDelay delay
          sendMsg nodeAddr $ RunMiniProtocols serverAddr reqs
          loop stdGenVar nodeAccs servers events jobpool

        OutboundMiniprotocols delay nodeAddr reqs -> do
          threadDelay delay
          sendMsg serverAddr $ RunMiniProtocols nodeAddr reqs
          loop stdGenVar nodeAccs servers events jobpool

        ShutdownClientServer delay nodeAddr -> do
          threadDelay delay
          sendMsg nodeAddr Shutdown
          loop stdGenVar nodeAccs servers events jobpool
      where
        sendMsg :: peerAddr -> ConnectionHandlerMessage peerAddr req -> m ()
        sendMsg addr msg = atomically $
          case Map.lookup addr servers of
            Nothing -> throwIO (NodeNotRunningException addr)
            Just cc -> writeTQueue cc msg

    mkNextRequests :: StrictTVar m (Map.Map (ConnectionId peerAddr) (TemperatureBundle (StrictTQueue m [req]))) ->
                      TemperatureBundle (ConnectionId peerAddr -> STM m [req])
    mkNextRequests connVar = makeBundle next
      where
        next :: forall pt. SingProtocolTemperature pt -> ConnectionId peerAddr -> STM m [req]
        next tok connId = do
          connMap <- readTVar connVar
          case Map.lookup connId connMap of
            Nothing -> retry
            Just qs -> readTQueue (projectBundle tok qs)

    startClientConnectionHandler :: StrictTVar m StdGen
                                 -> Name peerAddr
                                 -> peerAddr
                                 -> JobPool () m ()
                                 -> m (StrictTQueue m (ConnectionHandlerMessage peerAddr req))
    startClientConnectionHandler stdGenVar name localAddr jobpool  = do
        cc <- atomically newTQueue
        labelTQueueIO cc $ "cc/" ++ show name
        connVar <- newTVarIO Map.empty
        labelTVarIO connVar $ "connVar/" ++ show name
        threadId <- myThreadId
        stdGen <- atomically (stateTVar stdGenVar split)
        forkJob jobpool
          $ Job
              ( withInitiatorOnlyConnectionManager
                    name simTimeouts nullTracer nullTracer stdGen
                    snocket makeBearer (Just localAddr) (mkNextRequests connVar)
                    timeLimitsHandshake acceptedConnLimit
                  ( \ connectionManager ->
                      connectionLoop SingInitiatorMode localAddr cc connectionManager Map.empty connVar
                  )
              )
              (handleError threadId)
              ()
              (show name)
        return cc

    startServerConnectionHandler :: StrictTVar m StdGen
                                 -> Name peerAddr
                                 -> DataFlow
                                 -> acc
                                 -> peerAddr
                                 -> JobPool () m ()
                                 -> m (StrictTQueue m (ConnectionHandlerMessage peerAddr req))
    startServerConnectionHandler stdGenVar name dataFlow serverAcc localAddr jobpool = do
        fd <- Snocket.open snocket addrFamily
        Snocket.bind   snocket fd localAddr
        Snocket.listen snocket fd
        cc <- atomically newTQueue
        labelTQueueIO cc $ "cc/" ++ show name
        connVar <- newTVarIO Map.empty
        labelTVarIO connVar $ "connVar/" ++ show name
        threadId <- myThreadId
        stdGen <- atomically (stateTVar stdGenVar split)
        let job =
              case dataFlow of
                Duplex ->
                  Job ( withBidirectionalConnectionManager
                          name simTimeouts
                          inboundTrTracer trTracer cmTracer
                          inboundTracer debugTracer
                          stdGen
                          snocket makeBearer (\_ -> pure ()) fd (Just localAddr) serverAcc
                          (mkNextRequests connVar)
                          timeLimitsHandshake
                          acceptedConnLimit
                          ( \ connectionManager _ _serverAsync -> do
                            connectionLoop SingInitiatorResponderMode localAddr cc connectionManager Map.empty connVar
                          )
                        -- `JobPool` does not catch any async exceptions, so we
                        -- need to unwrap `ExceptionInLinkedThread`.
                        `catch` (\(ExceptionInLinkedThread _ e) -> throwIO e)
                        `finally` Snocket.close snocket fd
                      )
                      (handleError threadId)
                      ()
                      (show name)
                Unidirectional ->
                  Job ( withInitiatorOnlyConnectionManager
                          name simTimeouts trTracer cmTracer stdGen snocket makeBearer (Just localAddr)
                          (mkNextRequests connVar)
                          timeLimitsHandshake
                          acceptedConnLimit
                          ( \ connectionManager -> do
                            connectionLoop SingInitiatorMode localAddr cc connectionManager Map.empty connVar
                          )
                        -- `JobPool` does not catch any async exceptions, so we
                        -- need to unwrap `ExceptionInLinkedThread`.
                        `catch` (\(ExceptionInLinkedThread _ e) -> throwIO e)
                        `finally` Snocket.close snocket fd
                      )
                      (handleError threadId)
                      ()
                      (show name)
        forkJob jobpool job
        return cc
      where

    handleError :: ThreadId m -> SomeException -> m ()
    handleError threadId = \e -> case classifyError e of
        Just err -> say ("Ignored: " ++ show err)
        Nothing  -> throwTo threadId e
                 >> throwIO e
      where
        -- A white list of exceptions which are ignored by the tests. All
        -- other exceptions are rethrown and will halt the simulation.
        classifyError :: SomeException -> Maybe SomeException
        classifyError e =
                SomeException <$> (fromException e :: Maybe IOError)
            <|> SomeException <$> (fromException e :: Maybe Mux.Error)
            <|> SomeException <$> (fromException e >>= \(ExceptionInHandler _ e') ->
                                                        classifyError e')
            <|> SomeException <$> (fromException e >>= \(ExceptionInLinkedThread _ e') ->
                                                        classifyError e')

                -- TODO: we shouldn't need these:
            <|> SomeException <$> (fromException e :: Maybe ExperimentError)
            <|> SomeException <$> (fromException e :: Maybe ResourceException)

    connectionLoop
         :: forall muxMode a.
            (HasInitiator muxMode ~ True)
         => SingMuxMode muxMode
         -> peerAddr
         -> StrictTQueue m (ConnectionHandlerMessage peerAddr req)
         -- ^ control channel
         -> ConnectionManagerWithExpandedCtx muxMode socket peerAddr DataFlowProtocolData UnversionedProtocol ByteString m [resp] a
         -> Map.Map peerAddr (HandleWithExpandedCtx muxMode peerAddr DataFlowProtocolData ByteString m [resp] a)
         -- ^ active connections
         -> StrictTVar m (Map.Map (ConnectionId peerAddr) (TemperatureBundle (StrictTQueue m [req])))
         -- ^ mini protocol queues
         -> m ()
    connectionLoop muxMode localAddr cc cm connMap0 connVar = go connMap0
      where
        go :: Map.Map peerAddr (HandleWithExpandedCtx muxMode peerAddr DataFlowProtocolData ByteString m [resp] a) -- active connections
           -> m ()
        go !connMap = atomically (readTQueue cc) >>= \ case
          NewConnection remoteAddr -> do
            let mkQueue :: forall pt. SingProtocolTemperature pt
                        -> STM m (StrictTQueue m [req])
                mkQueue tok = do
                  q <- newTQueue
                  let temp = case tok of
                        SingHot         -> "hot"
                        SingWarm        -> "warm"
                        SingEstablished -> "cold"
                  q <$ labelTQueue q ("protoVar." ++ temp ++ "@" ++ show localAddr)
            connHandle <- tryJust (\(e :: SomeException) ->
                                       case fromException e of
                                         Just SomeAsyncException {} -> Nothing
                                         _                          -> Just e)
                          $ requestOutboundConnection cm remoteAddr
            case connHandle of
              Left _ ->
                go connMap
              Right (Connected _ _ h) -> do
                qs <- atomically $ sequenceA $ makeBundle mkQueue
                atomically $ modifyTVar connVar
                           $ Map.insert (connId remoteAddr) qs
                go (Map.insert remoteAddr h connMap)
              Right Disconnected {} -> return ()
          Disconnect remoteAddr -> do
            atomically $ do
              m <- readTVar connVar
              check (Map.member (connId remoteAddr) m)
              writeTVar connVar (Map.delete (connId remoteAddr) m)
            void (unregisterOutboundConnection cm remoteAddr)
            go (Map.delete remoteAddr connMap)
          RunMiniProtocols remoteAddr reqs -> do
            atomically $ do
              mqs <- Map.lookup (connId remoteAddr) <$> readTVar connVar
              case mqs of
                Nothing ->
                  -- TODO: we don't throw, because the server could be down due
                  -- to network attenuation, other than that it's a generator
                  -- invariant violation.
                  --
                  -- throwIO (NoActiveConnection localAddr remoteAddr)
                  return ()
                Just qs -> do
                  sequence_ $ writeTQueue <$> qs <*> reqs
            case Map.lookup remoteAddr connMap of
              Nothing -> -- TODO: as above
                         -- throwIO (NoActiveConnection localAddr remoteAddr)
                         return ()
              Just (Handle mux muxBundle controlBundle _) -> do
                -- TODO:
                -- At times this throws 'ProtocolAlreadyRunning'.
                r <- tryJust (\(e :: SomeException) ->
                                  case fromException e of
                                    Just SomeAsyncException {} -> Nothing -- rethrown
                                    _                          -> Just e)
                     $ runInitiatorProtocols
                          muxMode mux
                          muxBundle
                          controlBundle
                          ConnectionId { localAddress  = localAddr,
                                         remoteAddress = remoteAddr
                                       }
                case r of
                  -- Lost connection to peer
                  Left  {} -> do
                    atomically
                      $ modifyTVar connVar (Map.delete (connId remoteAddr))
                    go (Map.delete remoteAddr connMap)
                  Right {} -> go connMap
          Shutdown -> return ()
          where
            connId remoteAddr = ConnectionId { localAddress  = localAddr
                                             , remoteAddress = remoteAddr }


data Three a b c
    = First  a
    | Second b
    | Third  c
  deriving Show

validate_transitions :: MultiNodeScript Int TestAddr
                     -> SimTrace ()
                     -> Property
validate_transitions mns@(MultiNodeScript events _) trace =
      tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    -- . counterexample (Trace.ppTrace show show abstractTransitionEvents)
    . mkProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> mempty { tpProperty = counterexample (show v) False }
       )
       ( \ case
           Left trs ->
             TestProperty {
               tpProperty =
                   (counterexample $!
                     (  "\nconnection:\n"
                     ++ intercalate "\n" (map ppTransition trs))
                     )
                 . foldMap ( \ tr
                            -> All
                             . (counterexample $!
                                 (  "\nUnexpected transition: "
                                 ++ show tr)
                                 )
                             . verifyAbstractTransition
                             $ tr
                           )
                 $ trs,
               tpNumberOfTransitions = Sum (length trs),
               tpNumberOfConnections = Sum 1,
               tpNumberOfPrunings    = Sum 0,
               tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
               tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
               tpTerminationTypes    = [classifyTermination        trs],
               tpActivityTypes       = [classifyActivityType       trs],
               tpTransitions         = trs
            }
           Right b -> mempty { tpNumberOfPrunings = classifyPruning b }
       )
    . fmap (first (map ttTransition))
    . groupConnsEither id abstractStateIsFinalTransition
    $ evs
  where
    -- abstractTransitionEvents :: Trace (SimResult ())
    --                                   (AbstractTransitionTrace SimAddr)
    -- abstractTransitionEvents = traceWithNameTraceEvents trace

    evs :: Trace (SimResult ()) (Either (AbstractTransitionTrace SimAddr)
                                        (CM.Trace SimAddr (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)))
    evs = fmap (bimap wnEvent wnEvent)
        . Trace.filter ((MainServer ==) . either wnName wnName)
        . traceSelectTraceEvents fn
        $ trace
      where
        fn :: Time -> SimEventType
           -> Maybe (Either (WithName (Name SimAddr) (AbstractTransitionTrace SimAddr))
                            (WithName (Name SimAddr) (CM.Trace SimAddr
                                                        (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData))))
        fn _ (EventLog dyn) = Left  <$> fromDynamic dyn
                          <|> Right <$> fromDynamic dyn
        fn _ _              = Nothing



-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates connection manager state changes.
--
prop_connection_manager_valid_transitions
  :: Fixed Int
  -> Int
  -> ArbDataFlow
  -> AbsBearerInfo
  -> MultiNodeScript Int TestAddr
  -> Property
prop_connection_manager_valid_transitions
  (Fixed rnd) serverAcc (ArbDataFlow dataFlow) defaultBearerInfo
  mns@(MultiNodeScript events attenuationMap) =
    validate_transitions mns trace
  where
    trace = runSimTrace sim
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap


prop_connection_manager_valid_transitions_racy
  :: Fixed Int
  -> Int
  -> ArbDataFlow
  -> AbsBearerInfo
  -> MultiNodeScript Int TestAddr
  -> Property
prop_connection_manager_valid_transitions_racy
   (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
  defaultBearerInfo mns@(MultiNodeScript events attenuationMap) =
    exploreSimTrace id sim $ \_ trace ->
                             validate_transitions mns trace
  where
    sim :: IOSim s ()
    sim = exploreRaces
       >> multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap


-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test coverage of connection manager state transitions.
-- TODO: Fix transitions that are not covered. #3516
prop_connection_manager_transitions_coverage :: Fixed Int
                                             -> Int
                                             -> ArbDataFlow
                                             -> AbsBearerInfo
                                             -> MultiNodeScript Int TestAddr
                                             -> Property
prop_connection_manager_transitions_coverage (Fixed rnd) serverAcc
    (ArbDataFlow dataFlow)
    defaultBearerInfo
    (MultiNodeScript events attenuationMap) =
  let trace = runSimTrace sim

      abstractTransitionEvents :: [AbstractTransitionTrace SimAddr]
      abstractTransitionEvents = withNameTraceEvents trace

      transitionsSeen = nub [ tran | TransitionTrace _ tran <- abstractTransitionEvents]
      transitionsSeenNames = map (snd . validTransitionMap) transitionsSeen

   in coverTable "valid transitions" [ (n, 0.01) | n <- allValidTransitionsNames ] $
      tabulate   "valid transitions" transitionsSeenNames
      True
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates that we do not get undesired traces, such as
-- TrUnexpectedlyMissingConnectionState in connection manager.
--
prop_connection_manager_no_invalid_traces :: Fixed Int
                                          -> Int
                                          -> ArbDataFlow
                                          -> AbsBearerInfo
                                          -> MultiNodeScript Int TestAddr
                                          -> Property
prop_connection_manager_no_invalid_traces (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                                          defaultBearerInfo
                                          (MultiNodeScript events
                                                           attenuationMap) =
  let trace = runSimTrace sim

      connectionManagerEvents :: Trace (SimResult ())
                                       (CM.Trace
                                         SimAddr
                                         (ConnectionHandlerTrace
                                           UnversionedProtocol
                                           DataFlowProtocolData))
      connectionManagerEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (intercalate "\n"
                     [ "========== Script =========="
                     , ppScript (MultiNodeScript events attenuationMap)
                     , "========== ConnectionManager Events =========="
                     , Trace.ppTrace show show connectionManagerEvents
                     ])
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> All (counterexample (show v) False)
       )
       ( \ tr
        -> case tr of
          CM.TrUnexpectedlyFalseAssertion _
            -> All (counterexample (show tr) False)
          _ -> mempty
       )
    $ connectionManagerEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates the order of connection manager state changes.
--
prop_connection_manager_valid_transition_order :: Fixed Int
                                               -> Int
                                               -> ArbDataFlow
                                               -> AbsBearerInfo
                                               -> MultiNodeScript Int TestAddr
                                               -> Property
prop_connection_manager_valid_transition_order (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                                               defaultBearerInfo
                                               mns@(MultiNodeScript
                                                        events
                                                        attenuationMap) =
  let trace = runSimTrace sim

      abstractTransitionEvents :: Trace (SimResult ())
                                        (AbstractTransitionTrace SimAddr)
      abstractTransitionEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show abstractTransitionEvents)
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           _             -> All False
       )
       (verifyAbstractTransitionOrder True)
    . fmap (map ttTransition)
    . groupConns id abstractStateIsFinalTransition
    $ abstractTransitionEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap


prop_connection_manager_valid_transition_order_racy :: Fixed Int
                                                    -> Int
                                                    -> ArbDataFlow
                                                    -> AbsBearerInfo
                                                    -> MultiNodeScript Int TestAddr
                                                    -> Property
prop_connection_manager_valid_transition_order_racy (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                                                    defaultBearerInfo
                                                    mns@(MultiNodeScript
                                                             events
                                                             attenuationMap) =
    exploreSimTrace
          (\a -> a { explorationReplay = Just ControlDefault })
          sim $ \_ trace ->
      let abstractTransitionEvents :: Trace (SimResult ())
                                            (AbstractTransitionTrace SimAddr)
          abstractTransitionEvents = traceWithNameTraceEvents trace

      in  -- ppDebug trace
          tabulate "ConnectionEvents" (map showConnectionEvents events)
        . counterexample (ppScript mns)
        . counterexample (Trace.ppTrace show show abstractTransitionEvents)
        . bifoldMap
           ( \ case
               MainReturn {} -> mempty
               _             -> All False
           )
           (verifyAbstractTransitionOrder True)
        . fmap (map ttTransition)
        . groupConns id abstractStateIsFinalTransition
        $ abstractTransitionEvents
  where
    sim :: IOSim s ()
    sim = exploreRaces
       >> multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap


-- | Check connection manager counters in `multinodeExperiment`.
--
-- Note: this test validates connection manager counters using an upper bound
-- approach since there's no reliable way to reconstruct the value that the
-- counters should have at a given point in time. It's not quite possible to
-- have the god's view of the system consistent with the information that's
-- traced because there can be timing issues where a connection is in
-- TerminatingSt (hence counted as 0) but in the God's view the connection is
-- still being deleted (hence counted as 1). This is all due to the not having
-- a better way of injecting god's view traces in a way that the timing issues
-- aren't an issue.
--
-- TODO #4698: this function should be rewritten to analyse the trace as
-- a stream, to reduce its memory footprint.
prop_connection_manager_counters :: Fixed Int
                                 -> Int
                                 -> ArbDataFlow
                                 -> MultiNodeScript Int TestAddr
                                 -> Property
prop_connection_manager_counters (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                                 (MultiNodeScript events
                                                  attenuationMap) =
  let trace = runSimTrace sim

      connectionManagerEvents :: Trace (SimResult ())
                                       (CM.Trace
                                         SimAddr
                                         (ConnectionHandlerTrace
                                           UnversionedProtocol
                                           DataFlowProtocolData))
      connectionManagerEvents = traceWithNameTraceEvents trace

      -- Needed for calculating a more accurate upper bound
      networkEvents :: [ObservableNetworkState SimAddr]
      networkEvents = selectTraceEventsDynamic trace

      upperBound =
        multiNodeScriptToCounters dataFlow events networkEvents

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (concat
        [ "\n\n====== Say Events ======\n"
        , intercalate "\n" $ selectTraceEventsSay' trace
        , "\n"
        ])
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> All
                            $ counterexample (show v) False
       )
       ( \ case
          CM.TrConnectionManagerCounters cmc ->
            All
              $ counterexample
                  ("Upper bound is: " ++ show upperBound
                  ++ "\n But got: " ++ show cmc)
                  (property $ collapseCounters False cmc
                           <= collapseCounters True upperBound)
          _ -> mempty
       )
    $ connectionManagerEvents
  where
    serverAddress :: SimAddr
    serverAddress = TestAddress 0

    -- We count all connections as prunable because we do not have a better way
    -- to know what transitions will a given connection go through. We also
    -- count every client connection as unidirectional because they do not
    -- negotiate duplex data flow.
    --
    -- We do not count Inbound/Outbound/Closing of connections since the
    -- 'ObservableNetworkState' is a much more reliable source of information.
    --
    -- Note that this is only valid in the case of no attenuation.
    --
    multiNodeScriptToCounters :: DataFlow
                              -> [ConnectionEvent Int TestAddr]
                              -> [ObservableNetworkState SimAddr]
                              -> ConnectionManagerCounters
    multiNodeScriptToCounters df ces uss =
      let ifDuplex = bool 0 1 (df == Duplex)
          ifUni = bool 0 1 (df == Unidirectional)
          -- Every StartClient and StartServer will be associated to an Inbound
          -- or Outbound connection request to/from the main node. Since we do
          -- not know if it will perform both (making a duplex connection),
          -- connectionManagerCounters look at the StartClient and StartServer
          -- events and count the number of unidirectional/duplex/prunable
          -- connections assuming it reaches the last state of the state
          -- machine (DuplexSt). Then the ObservableNetworkState is used to make
          -- a more accurate guess of the actual upper bound of inbound/outbound
          -- connections.
          --
          -- TODO: we are computing upper bound of contribution of each
          -- address separately.  This avoids tracking timing information of
          -- events, which is less accurate but it might be less fragile.  We
          -- should investigate if it's possible to make accurate and robust
          -- time series of counter changes.
          connectionManagerCounters =
              foldMap' id
            . List.foldl'
               (\ st ce -> case ce of
                 StartClient _ ta ->
                   Map.alter (let c = ConnectionManagerCounters 0 0 1 0 0 in
                              maybe (Just c) (Just . maxCounters c))
                             ta st

                 OutboundConnection _ ta ->
                   Map.alter (let c = ConnectionManagerCounters 0 ifDuplex ifUni 0 1 in
                              maybe (Just c) (Just . maxCounters c))
                             ta st

                 InboundConnection _ ta ->
                   Map.alter (let c = ConnectionManagerCounters 0 ifDuplex ifUni 1 0 in
                              maybe (Just c) (Just . maxCounters c))
                             ta st

                 _ -> st
               )
               Map.empty
            $ ces

          -- This calculation is right for the main node, because the simulation
          -- never attempts to make other connections that go to or from the
          -- main node.
          networkStateCounters = List.foldl'
                        (\cmc (ObservableNetworkState conns) ->
                          maxCounters cmc $
                          Map.foldl'
                           (\cmc' provenance ->
                             cmc' <>
                             if provenance == serverAddress
                                then ConnectionManagerCounters 0 0 0 0 1
                                else ConnectionManagerCounters 0 0 0 1 0
                           )
                           mempty
                           conns
                        )
                       mempty
                       uss
       in maxCounters connectionManagerCounters networkStateCounters

    maxCounters :: ConnectionManagerCounters
                -> ConnectionManagerCounters
                -> ConnectionManagerCounters
    maxCounters (ConnectionManagerCounters a b c d e)
                (ConnectionManagerCounters a' b' c' d' e') =
      ConnectionManagerCounters
        (max a a')
        (max b b')
        (max c c')
        (max d d')
        (max e e')

    -- It is possible for the ObservableNetworkState to have discrepancies between the
    -- counters traced by TrConnectionManagerCounters. This leads to different
    -- observations where an inbound connection can go into a state that is
    -- counted as outbound but the provenance in the Snocket's observable
    -- network state does not change so we can not know for sure what's
    -- happening inside the ConnectionManager's state machine.
    --
    -- Given this we collapse the count of incoming and outgoing connections
    -- since this value should always be the same in both views. Note that for
    -- values besides the upper bound we have to correct the sum by removing the
    -- duplicates.
    --
    -- TODO: Try idea in: ouroboros-network/pull/3429#discussion_r746406157
    --       See issue: #3509
    --
    -- TODO: test the number of full duplex connections.
    --
    collapseCounters :: Bool -- ^ Should we remove Duplex duplicate counters out
                             -- of the total sum.
                     -> ConnectionManagerCounters
                     -> (Int, Int, Int)
    collapseCounters t (ConnectionManagerCounters _ a b c d) =
      if t
         then (a, b, c + d)
         else (a, b, c + d - a)

    networkStateTracer getState =
      Tracer $ \_ -> getState >>= traceM

    sim :: IOSim s ()
    sim = do
      mb <- timeout 7200
                    ( withSnocket nullTracer
                                  noAttenuation
                                  Map.empty
              $ \snocket getState ->
                multinodeExperiment (sayTracer <> Tracer traceM)
                                    (sayTracer <> Tracer traceM)
                                    (sayTracer <> Tracer traceM)
                                    nullTracer
                                    (   sayTracer
                                     <> Tracer traceM
                                     <> networkStateTracer getState)
                                    (mkStdGen rnd)
                                    snocket
                                    makeFDBearer
                                    Snocket.TestFamily
                                    serverAddress
                                    serverAcc
                                    dataFlow
                                    maxAcceptedConnectionsLimit
                                    (MultiNodeScript
                                      (fmap unTestAddr <$> events)
                                      (Map.mapKeys unTestAddr attenuationMap)
                                    )
              )
      case mb of
        Nothing -> throwIO SimulationTimeout
        Just a  -> return a


-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test verifies for for all \tau state we do not stay longer than
-- the designated timeout.
--
-- This test tests simultaneously the ConnectionManager and InboundGovernor's
-- timeouts.
--
-- TODO #4698: reduce memory footprint.
prop_timeouts_enforced :: Fixed Int
                       -> Int
                       -> ArbDataFlow
                       -> MultiNodeScript Int TestAddr
                       -> Property
prop_timeouts_enforced (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                       (MultiNodeScript events attenuationMap) =
  let trace = runSimTrace sim

      transitionSignal :: Trace (SimResult ()) [(Time, AbstractTransitionTrace SimAddr)]
      transitionSignal = fmap (map ((,) <$> wtTime <*> wtEvent))
                       . groupConns wtEvent abstractStateIsFinalTransition
                       . withTimeNameTraceEvents
                       $ trace

  in counterexample (ppTrace trace)
   $ verifyAllTimeouts False transitionSignal
  where
    sim :: IOSim s ()
    sim = multiNodeSimTracer (mkStdGen rnd) serverAcc dataFlow
                             absNoAttenuation
                             maxAcceptedConnectionsLimit
                             events
                             attenuationMap
                             dynamicTracer
                             (tracerWithTime (Tracer traceM) <> dynamicTracer)
                             dynamicTracer
                             nullTracer
                             dynamicTracer

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates inbound governor state changes.
--
prop_inbound_governor_valid_transitions :: Fixed Int
                                        -> Int
                                        -> ArbDataFlow
                                        -> AbsBearerInfo
                                        -> MultiNodeScript Int TestAddr
                                        -> Property
prop_inbound_governor_valid_transitions (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                                        defaultBearerInfo
                                        mns@(MultiNodeScript
                                                  events
                                                  attenuationMap) =
  let trace = runSimTrace sim

      remoteTransitionTraceEvents :: Trace (SimResult ())
                                           (RemoteTransitionTrace SimAddr)
      remoteTransitionTraceEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show remoteTransitionTraceEvents)
    -- Verify that all Inbound Governor remote transitions are valid
    . bifoldMap
       ( \ _ -> All True )
       ( \ TransitionTrace {ttPeerAddr = peerAddr, ttTransition = tr} ->
             All
           . counterexample (concat [ "Unexpected transition: "
                                    , show peerAddr
                                    , " "
                                    , show tr
                                    ])
           . verifyRemoteTransition
           $ tr
       )
    $ remoteTransitionTraceEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates inbound governor state changes.
--
prop_inbound_governor_no_unsupported_state :: Fixed Int
                                           -> Int
                                           -> ArbDataFlow
                                           -> AbsBearerInfo
                                           -> MultiNodeScript Int TestAddr
                                           -> Property
prop_inbound_governor_no_unsupported_state (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                                           defaultBearerInfo
                                           mns@(MultiNodeScript
                                                    events
                                                    attenuationMap) =
  let trace = runSimTrace sim

      inboundGovernorEvents :: Trace (SimResult ())
                                     (IG.Trace SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show inboundGovernorEvents)
    -- Verify we do not return unsupported states in any of the
    -- RemoteTransitionTrace
    . bifoldMap
        ( \ _ -> All True)
        ( \ tr -> case tr of
            -- verify that 'unregisterInboundConnection' does not return
            -- 'UnsupportedState'.
            IG.TrDemotedToColdRemote _ res ->
              case res of
                UnsupportedState {}
                  -> All (counterexample (show tr) False)
                _ -> All True

            -- verify that 'demotedToColdRemote' does not return
            -- 'UnsupportedState'
            IG.TrWaitIdleRemote _ res ->
              case res of
                UnsupportedState {}
                  -> All (counterexample (show tr) False)
                _ -> All True

            _     -> All True
        )
    $ inboundGovernorEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates that we do not get undesired traces, such as
-- TrUnexpectedlyMissingConnectionState in inbound governor.
--
prop_inbound_governor_no_invalid_traces :: Fixed Int
                                        -> Int
                                        -> ArbDataFlow
                                        -> AbsBearerInfo
                                        -> MultiNodeScript Int TestAddr
                                        -> Property
prop_inbound_governor_no_invalid_traces (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                                        defaultBearerInfo
                                        mns@(MultiNodeScript events
                                                             attenuationMap) =
  let trace = runSimTrace sim

      inboundGovernorEvents :: Trace (SimResult ()) (IG.Trace SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (intercalate "\n"
                     [ "========== Script =========="
                     , ppScript mns
                     , "========== Inbound Governor Events =========="
                     , Trace.ppTrace show show inboundGovernorEvents
                     -- , "========== Simulation Trace =========="
                     -- , ppTrace trace
                     ])
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> All (counterexample (show v) False)
       )
       ( \ tr
        -> case tr of
          IG.TrUnexpectedlyFalseAssertion _
            -> All (counterexample (show tr) False)
          _ -> mempty
       )
    $ inboundGovernorEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test coverage of inbound governor state transitions.
-- TODO: Fix transitions that are not covered. #3516
prop_inbound_governor_transitions_coverage :: Fixed Int
                                           -> Int
                                           -> ArbDataFlow
                                           -> AbsBearerInfo
                                           -> MultiNodeScript Int TestAddr
                                           -> Property
prop_inbound_governor_transitions_coverage (Fixed rnd) serverAcc
  (ArbDataFlow dataFlow)
  defaultBearerInfo
  (MultiNodeScript events attenuationMap) =
  let trace = runSimTrace sim

      remoteTransitionTraceEvents :: [RemoteTransitionTrace SimAddr]
      remoteTransitionTraceEvents = withNameTraceEvents trace

      transitionsSeen = nub [ tran
                            | TransitionTrace _ tran
                                <- remoteTransitionTraceEvents]
      transitionsSeenNames = map (snd . validRemoteTransitionMap)
                                 transitionsSeen

   in coverTable "valid transitions"
                  [ (n, 0.01) | n <- allValidRemoteTransitionsNames ] $
      tabulate   "valid transitions" transitionsSeenNames
      True
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates the order of inbound governor state changes.
--
prop_inbound_governor_valid_transition_order :: Fixed Int
                                             -> Int
                                             -> ArbDataFlow
                                             -> AbsBearerInfo
                                             -> MultiNodeScript Int TestAddr
                                             -> Property
prop_inbound_governor_valid_transition_order (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                                             defaultBearerInfo
                                             mns@(MultiNodeScript
                                                      events
                                                      attenuationMap) =
  let trace = runSimTrace sim

      remoteTransitionTraceEvents :: Trace (SimResult ()) (RemoteTransitionTrace SimAddr)
      remoteTransitionTraceEvents = traceWithNameTraceEvents trace

      -- inboundGovernorEvents :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      -- inboundGovernorEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    -- . counterexample (Trace.ppTrace show show inboundGovernorEvents)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show remoteTransitionTraceEvents)
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           _             -> All False
       )
       (verifyRemoteTransitionOrder True)
    . fmap (map ttTransition)
    . groupConns id remoteStrIsFinalTransition
    $ remoteTransitionTraceEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Check inbound governor counters in `multinodeExperiment`.
--
-- Note: this test validates warm and hot inbound governor counters only.
--
prop_inbound_governor_counters :: Fixed Int
                               -> Int
                               -> ArbDataFlow
                               -> MultiNodeScript Int TestAddr
                               -> Property
prop_inbound_governor_counters (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                               mns@(MultiNodeScript
                                        events
                                        attenuationMap) =
  let trace = runSimTrace sim

      inboundGovernorEvents :: Trace (SimResult ())
                                     (IG.Trace SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

      upperBound = multiNodeScriptToCounters events

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show inboundGovernorEvents)
    . bifoldMap
       (\ case
          MainReturn {} -> mempty
          v             -> All
                         $ counterexample (show v) (property False)
       )
       (\ case
          IG.TrInboundGovernorCounters igc ->
            All
              $ counterexample
                  ("Upper bound is: " ++ show upperBound
                  ++ "\n But got: " ++ show igc)
                  (    IG.warmPeersRemote igc <= IG.warmPeersRemote upperBound
                  .&&. IG.hotPeersRemote  igc <= IG.hotPeersRemote  upperBound
                  )
          _ -> mempty
       )
    $ inboundGovernorEvents
  where
    -- Note that this is only valid in the case of no attenuation.
    bundleToCounters :: TemperatureBundle [Int] -> IG.Counters
    bundleToCounters (TemperatureBundle hot warm _) =
      let warmRemote = bool 1 0 (null warm)
          hotRemote  = bool 1 0 (null hot)
       in IG.Counters 0 0 warmRemote hotRemote

    -- We check for starting of miniprotocols that can potentially lead to
    -- inbound governor states of remote warm or remote hot connections. An
    -- upper bound is established because it is not possible to predict whether
    -- some failure will occur.
    multiNodeScriptToCounters :: [ConnectionEvent Int TestAddr]
                              -> IG.Counters
    multiNodeScriptToCounters =
      let taServerAcc = TestAddr (TestAddress 0)
       in
        (\x ->
          let serverAccEntry = x Map.! taServerAcc
              mapWithoutServerAcc = Map.delete taServerAcc x
           in Map.foldlWithKey'
                (\igt ta entry ->
                  case Map.lookup ta serverAccEntry of
                    Nothing  -> igt <> foldMap' bundleToCounters entry
                    Just bun -> igt <> foldMap' bundleToCounters entry
                                    <> bundleToCounters bun
                )
                mempty
                mapWithoutServerAcc

        )
        . List.foldl'
          (\ st ce -> case ce of
            StartClient _ ta ->
              Map.insertWith (<>) ta Map.empty st
            StartServer _ ta _ ->
              Map.insertWith (<>) ta Map.empty st
            InboundConnection _ ta ->
              Map.update (Just . Map.insertWith (<>) taServerAcc mempty) ta st
            OutboundConnection _ ta ->
              Map.update (Just . Map.insertWith (<>) ta mempty) taServerAcc st
            InboundMiniprotocols _ ta bun ->
              Map.update (Just . Map.update (Just . (<> bun)) taServerAcc) ta st
            OutboundMiniprotocols _ ta bun ->
              Map.update (Just . Map.update (Just . (<> bun)) ta) taServerAcc st
            _ -> st
          )
          (Map.singleton taServerAcc Map.empty)

    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       absNoAttenuation
                       maxAcceptedConnectionsLimit
                       events
                       (toNonFailing <$> attenuationMap)


prop_inbound_governor_state :: Fixed Int
                            -> Int
                            -> ArbDataFlow
                            -> AbsBearerInfo
                            -> MultiNodeScript Int TestAddr
                            -> Property
prop_inbound_governor_state (Fixed rnd) serverAcc (ArbDataFlow dataFlow)
                            defaultBearerInfo
                            mns@(MultiNodeScript events attenuationMap) =
  let trace = runSimTrace sim

      evs :: Trace (SimResult ()) (IG.Debug SimAddr DataFlowProtocolData)
      evs = traceWithNameTraceEvents trace

      -- inboundGovernorEvents :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      -- inboundGovernorEvents = traceWithNameTraceEvents trace

  in  counterexample (ppScript mns)
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           _             -> All False
       )
       (All . inboundGovernorStateInvariant)
    $ evs
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

    inboundGovernorStateInvariant :: IG.Debug SimAddr DataFlowProtocolData
                                  -> Property
    inboundGovernorStateInvariant
        (IG.Debug IG.State { IG.connections,
                             IG.matureDuplexPeers,
                             IG.freshDuplexPeers })
        =
        keys === keys'
      where
        keys  = Set.map remoteAddress
              . Map.keysSet
              . Map.filter (\ConnectionState { csVersionData } -> getProtocolDataFlow csVersionData == Duplex)
              $ connections

        keys' = Set.fromList (OrdPSQ.keys freshDuplexPeers)
             <> Map.keysSet matureDuplexPeers



-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- This test tests if with a higher chance of pruning happening and a smaller number of
-- connections hard limit we do not end up triggering any illegal transition in Connection
-- Manager.
--
prop_connection_manager_pruning :: Fixed Int
                                -> Int
                                -> MultiNodePruningScript Int
                                -> Property
prop_connection_manager_pruning (Fixed rnd) serverAcc
                                (MultiNodePruningScript
                                  acceptedConnLimit
                                  events
                                  attenuationMap) =
  let trace = runSimTrace sim

      evs :: Trace (SimResult ()) (Either (AbstractTransitionTrace SimAddr)
                                          (CM.Trace SimAddr (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)))
      evs = fmap (bimap wnEvent wnEvent)
          . Trace.filter ((MainServer ==) . either wnName wnName)
          . traceSelectTraceEvents fn
          $ trace
        where
          fn :: Time -> SimEventType
             -> Maybe (Either (WithName (Name SimAddr) (AbstractTransitionTrace SimAddr))
                              (WithName (Name SimAddr) (CM.Trace SimAddr
                                                          (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData))))
          fn _ (EventLog dyn) = Left  <$> fromDynamic dyn
                            <|> Right <$> fromDynamic dyn
          fn _ _              = Nothing

  in  tabulate "ConnectionEvents" (map showConnectionEvents events)
    -- . counterexample (ppScript (MultiNodeScript events attenuationMap))
    . mkPropertyPruning
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> mempty { tpProperty = counterexample (show v) False }
       )
       ( \ case
           Left trs ->
             TestProperty {
               tpProperty =
                   (counterexample $!
                     (  "\nconnection:\n"
                     ++ intercalate "\n" (map ppTransition trs))
                     )
                 . foldMap ( \ tr
                            -> All
                             . (counterexample $!
                                 (  "\nUnexpected transition: "
                                 ++ show tr)
                                 )
                             . verifyAbstractTransition
                             $ tr
                           )
                 $ trs,
               tpNumberOfTransitions = Sum (length trs),
               tpNumberOfConnections = Sum 1,
               tpNumberOfPrunings    = Sum 0,
               tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
               tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
               tpTerminationTypes    = [classifyTermination        trs],
               tpActivityTypes       = [classifyActivityType       trs],
               tpTransitions         = trs
            }
           Right b ->
              mempty { tpNumberOfPrunings = classifyPruning b }
       )
    . fmap (first (map ttTransition))
    . groupConnsEither id abstractStateIsFinalTransition
    $ evs
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc Duplex
                       absNoAttenuation
                       acceptedConnLimit
                       events
                       (toNonFailing <$> attenuationMap)

-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- This test tests if with a higher chance of pruning happening and a smaller number of
-- connections hard limit we do not end up triggering any illegal transition in the
-- Inbound Governor.
--
prop_inbound_governor_pruning :: Fixed Int
                              -> Int
                              -> MultiNodePruningScript Int
                              -> Property
prop_inbound_governor_pruning (Fixed rnd) serverAcc
                              (MultiNodePruningScript
                                acceptedConnLimit
                                events
                                attenuationMap) =
  let trace = runSimTrace sim

      evs :: Trace (SimResult ()) (Either (IG.Trace SimAddr)
                                          (RemoteTransitionTrace SimAddr))
      evs = fmap (bimap wnEvent wnEvent)
          . Trace.filter ((MainServer ==) . either wnName wnName)
          . traceSelectTraceEvents fn
          $ trace
        where
          fn :: Time -> SimEventType -> Maybe (Either (WithName (Name SimAddr) (IG.Trace SimAddr))
                                                      (WithName (Name SimAddr) (RemoteTransitionTrace SimAddr)))
          fn _ (EventLog dyn) = Left  <$> fromDynamic dyn
                            <|> Right <$> fromDynamic dyn
          fn _ _              = Nothing


  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    -- . counterexample (ppTrace trace)
    . bifoldMap
        (\ _ -> All True)
        (\ case
           Left tr ->
             case tr of
               -- verify that 'unregisterInboundConnection' does not return
               -- 'UnsupportedState'.
               IG.TrDemotedToColdRemote _ res ->
                 case res of
                   UnsupportedState {} ->
                     All
                       $ counterexample
                           ("Unexpected UnsupportedState "
                           ++ "in unregisterInboundConnection "
                           ++ show tr)
                           False
                   _ -> All True

               -- verify that 'demotedToColdRemote' does not return
               -- 'UnsupportedState'
               IG.TrWaitIdleRemote _ UnsupportedState {} ->
                 All
                   $ counterexample
                       ("Unexpected UnsupportedState "
                       ++ "in demotedToColdRemote "
                       ++ show tr)
                       False

               _ -> All True

           -- Verify we do not return unsupported states in any of the
           -- RemoteTransitionTrace
           Right TransitionTrace {ttPeerAddr = peerAddr, ttTransition = tr } ->
                   All
                 . counterexample (concat [ "Unexpected transition: "
                                          , show peerAddr
                                          , " "
                                          , show tr
                                          ])
                 . verifyRemoteTransition
                 $ tr
        )
    $ evs
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc Duplex
                       absNoAttenuation
                       acceptedConnLimit
                       events
                       (toNonFailing <$> attenuationMap)


data FreshPeers peerAddr versionData =
    FreshPeers Time
               (OrdPSQ peerAddr Time versionData)

    deriving Show


instance (Arbitrary peerAddr, Arbitrary versionData, Ord peerAddr)
    => Arbitrary (FreshPeers peerAddr versionData) where
    arbitrary = FreshPeers <$> (mkTime <$> (arbitrary :: Gen Rational))
                           <*> ( OrdPSQ.fromList . map (\(a, b, c) -> (a, mkTime b, c))
                               <$> arbitrary)
      where
        mkTime :: Rational -> Time
        mkTime = Time . realToFrac


prop_inbound_governor_maturedPeers :: FreshPeers Int Int -> Property
prop_inbound_governor_maturedPeers (FreshPeers now fresh) = property $
         -- all peers which are kept as fresh are younger than `15min`
         foldMap (\(addr, t, _) -> All $ counterexample (show (addr, t, now))
                                       $ t >= (-delay) `addTime` now)
                 (OrdPSQ.toList fresh')
         -- peers are preserved
      <> (All $ Map.keysSet matured <> keysSet fresh' === keysSet fresh)
    where
      (matured, fresh') = IG.maturedPeers now fresh

      -- matured delay
      delay :: DiffTime
      delay = 15 * 60

      keysSet :: Ord k => OrdPSQ k p v -> Set k
      keysSet = Set.fromList . OrdPSQ.keys


-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- We test that:
--
-- * we never go above hard limit of incoming connections;
-- * the pruning set is at least as big as expected, and that
--   the picked peers belong to the choice set.
--
prop_never_above_hardlimit :: Fixed Int
                           -> Int
                           -> MultiNodePruningScript Int
                           -> Property
prop_never_above_hardlimit (Fixed rnd) serverAcc
                           (MultiNodePruningScript
                             acceptedConnLimit@AcceptedConnectionsLimit
                               { acceptedConnectionsHardLimit = hardlimit }
                             events
                             attenuationMap
                           ) =
  let trace = runSimTrace sim

      connectionManagerEvents :: Trace (SimResult ())
                                       (CM.Trace
                                         SimAddr
                                         (ConnectionHandlerTrace
                                           UnversionedProtocol
                                           DataFlowProtocolData))
      connectionManagerEvents = traceWithNameTraceEvents trace

      -- abstractTransitionEvents :: Trace (SimResult ()) (AbstractTransitionTrace SimAddr)
      -- abstractTransitionEvents = traceWithNameTraceEvents trace

      -- inboundGovernorEvents :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      -- inboundGovernorEvents = traceWithNameTraceEvents trace

  in  tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (Trace.ppTrace show show connectionManagerEvents)
    -- . counterexample (Trace.ppTrace show show abstractTransitionEvents)
    -- . counterexample (Trace.ppTrace show show inboundGovernorEvents)
    . bifoldMap
        ( \ case
            MainReturn {} -> mempty
            MainException _ _ e _
                          -> All (counterexample (show e) False)
            _             -> All False
        )
        ( \ case
            (CM.TrConnectionManagerCounters cmc) ->
                  All
                . counterexample ("HardLimit: " ++ show hardlimit ++
                                  ", but got: " ++ show (inboundConns cmc) ++
                                  " inbound connections!\n" ++
                                  show cmc
                                 )
                $! inboundConns cmc <= fromIntegral hardlimit
            (CM.TrPruneConnections prunnedSet numberToPrune choiceSet) ->
              ( All
              . counterexample (concat
                               [ "prunned set too small: "
                               , show numberToPrune
                               , " ≰ "
                               , show $ length prunnedSet
                               ])
              $ numberToPrune <= length prunnedSet )
              <>
              ( All
              . counterexample (concat [ "prunnedSet not a subset of choice set: "
                                       , show prunnedSet
                                       , " ⊈ "
                                       , show choiceSet
                                       ])
              $! prunnedSet `Set.isSubsetOf` choiceSet )
            _ -> mempty
        )
   $ connectionManagerEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim (mkStdGen rnd) serverAcc Duplex
                       absNoAttenuation
                       acceptedConnLimit
                       events
                       (toNonFailing <$> attenuationMap)


-- | Checks that the server re-throws exceptions returned by an 'accept' call.
--
unit_server_accept_error :: Fixed Int -> IOErrType -> Property
unit_server_accept_error (Fixed rnd) ioErrType =
    runSimOrThrow sim
  where
    -- The following attenuation make sure that the `accept` call will throw.
    --
    bearerAttenuation :: BearerInfo
    bearerAttenuation =
      noAttenuation { biAcceptFailures = Just (0, ioErrType) }

    sim :: IOSim s Property
    sim = handle (\e -> return $ case fromException e :: Maybe IOError of
                          Just _  -> property True
                          Nothing -> property $ counterexample ("not an IOError: " ++ show e) False
                 )
        $ withSnocket nullTracer
                      bearerAttenuation
                      Map.empty
        $ \snock _ ->
           bracket ((,) <$> Snocket.open snock Snocket.TestFamily
                        <*> Snocket.open snock Snocket.TestFamily)
                   (\ (socket0, socket1) -> Snocket.close snock socket0 >>
                                            Snocket.close snock socket1)
             $ \ (socket0, socket1) -> do

               let addr :: SimAddr
                   addr = Snocket.TestAddress (0 :: Int)
                   pdata :: ClientAndServerData Int
                   pdata = ClientAndServerData 0 [] [] []
               Snocket.bind snock socket0 addr
               Snocket.listen snock socket0
               nextRequests <- oneshotNextRequests pdata
               withBidirectionalConnectionManager "node-0" simTimeouts
                                                  nullTracer nullTracer
                                                  nullTracer nullTracer
                                                  nullTracer
                                                  (mkStdGen rnd)
                                                  snock
                                                  makeFDBearer
                                                  (\_ -> pure ())
                                                  socket0 (Just addr)
                                                  [accumulatorInit pdata]
                                                  nextRequests
                                                  noTimeLimitsHandshake
                                                  maxAcceptedConnectionsLimit
                 (\_connectionManager _serverAddr serverAsync -> do
                   -- connect to the server
                   Snocket.connect snock socket1 addr
                     --  connect will fail, and it will trigger accept error
                     `catch` \(_ :: SomeException) -> return ()
                   -- verify that server's `accept` error is rethrown by the
                   -- server
                   v <- registerDelay 1
                   r <- atomically $ runFirstToFinish $
                         FirstToFinish (Just <$> waitCatchSTM serverAsync)
                         <>
                         FirstToFinish (LazySTM.readTVar v >>= \a -> check a $> Nothing)
                   return $ case r of
                     Nothing        -> counterexample "server did not throw"
                                         (ioErrType == IOErrConnectionAborted)
                     Just (Right _) -> counterexample "unexpected value" False
                     Just (Left e)  -- If we throw exceptions, any IO exception
                                    -- can go through; the check for
                                    -- 'ECONNABORTED' relies on that.  It is
                                    -- a bug in a snocket implementation if it
                                    -- throws instead of returns io errors.
                                    | Just (_ :: IOError) <- fromException e
                                    -> property True

                                    | otherwise
                                    -> counterexample ("unexpected error " ++ show e)
                                                      False
                 )



multiNodeSimTracer :: ( Alternative (STM m), Monad m, MonadFix m
                      , MonadDelay m, MonadTimer m, MonadLabelledSTM m
                      , MonadTraceSTM m, MonadMask m, MonadTime m
                      , MonadThrow (STM m), MonadSay m, MonadAsync m
                      , MonadEvaluate m, MonadFork m, MonadST m
                      , Serialise req, Show req, Eq req, Typeable req
                      )
                   => StdGen
                   -> req
                   -> DataFlow
                   -> AbsBearerInfo
                   -> AcceptedConnectionsLimit
                   -> [ConnectionEvent req TestAddr]
                   -> Map TestAddr (Script AbsBearerInfo)
                   -> Tracer m
                      (WithName (Name SimAddr) (RemoteTransitionTrace SimAddr))
                   -> Tracer m
                      (WithName (Name SimAddr) (AbstractTransitionTrace SimAddr))
                   -> Tracer m
                      (WithName (Name SimAddr) (IG.Trace SimAddr))
                   -> Tracer m
                      (WithName (Name SimAddr) (IG.Debug SimAddr DataFlowProtocolData))
                   -> Tracer m
                      (WithName
                       (Name SimAddr)
                        (CM.Trace
                         SimAddr
                          (ConnectionHandlerTrace
                            UnversionedProtocol DataFlowProtocolData)))
                   -> m ()
multiNodeSimTracer stdGen serverAcc dataFlow defaultBearerInfo
                   acceptedConnLimit events attenuationMap
                   remoteTrTracer abstractTrTracer
                   inboundGovTracer debugTracer connMgrTracer = do

      let attenuationMap' = (fmap toBearerInfo <$>)
                          . Map.mapKeys ( normaliseId
                                        . ConnectionId mainServerAddr
                                        . unTestAddr)
                          $ attenuationMap

      mb <- timeout 7200
                    ( withSnocket (Tracer (say . show))
                                  (toBearerInfo defaultBearerInfo)
                                  attenuationMap'
              $ \snocket _ ->
                 multinodeExperiment remoteTrTracer
                                     abstractTrTracer
                                     inboundGovTracer
                                     debugTracer
                                     connMgrTracer
                                     stdGen
                                     snocket
                                     makeFDBearer
                                     Snocket.TestFamily
                                     mainServerAddr
                                     serverAcc
                                     dataFlow
                                     acceptedConnLimit
                                     (MultiNodeScript
                                       ((unTestAddr <$>) <$> events)
                                       (Map.mapKeys unTestAddr attenuationMap)
                                     )
              )
      case mb of
        Nothing -> throwIO SimulationTimeout
        Just a  -> return a
  where
    mainServerAddr :: SimAddr
    mainServerAddr = Snocket.TestAddress 0


multiNodeSim :: (Serialise req, Show req, Eq req, Typeable req)
             => StdGen
             -> req
             -> DataFlow
             -> AbsBearerInfo
             -> AcceptedConnectionsLimit
             -> [ConnectionEvent req TestAddr]
             -> Map TestAddr (Script AbsBearerInfo)
             -> IOSim s ()
multiNodeSim stdGen serverAcc dataFlow defaultBearerInfo
                   acceptedConnLimit events attenuationMap = do
  multiNodeSimTracer stdGen serverAcc dataFlow defaultBearerInfo acceptedConnLimit
                     events attenuationMap dynamicTracer dynamicTracer dynamicTracer
                     (Tracer traceM) dynamicTracer


-- | Connection terminated while negotiating it.
--
unit_connection_terminated_when_negotiating :: Property
unit_connection_terminated_when_negotiating =
  let arbDataFlow = ArbDataFlow Unidirectional
      absBearerInfo =
        AbsBearerInfo
          { abiConnectionDelay = SmallDelay
          , abiInboundAttenuation = NoAttenuation FastSpeed
          , abiOutboundAttenuation = NoAttenuation FastSpeed
          , abiInboundWriteFailure = Nothing
          , abiOutboundWriteFailure = Just 3
          , abiAcceptFailure = Nothing
          , abiSDUSize = LargeSDU
          }
      multiNodeScript =
        MultiNodeScript
         [ StartServer 0           (TestAddr {unTestAddr = TestAddress 24}) 0
         , OutboundConnection 0    (TestAddr {unTestAddr = TestAddress 24})
         , StartServer 0           (TestAddr {unTestAddr = TestAddress 40}) 0
         , OutboundMiniprotocols 0 (TestAddr {unTestAddr = TestAddress 24})
                                   (TemperatureBundle
                                     { withHot         = WithHot [0]
                                     , withWarm        = WithWarm []
                                     , withEstablished = WithEstablished []
                                     })
         , OutboundConnection 0    (TestAddr {unTestAddr = TestAddress 40})
         ]
         Map.empty
   in
    prop_connection_manager_valid_transitions
         (Fixed 0) 0 arbDataFlow absBearerInfo
        multiNodeScript
    .&&.
    prop_inbound_governor_valid_transitions
        (Fixed 0) 0 arbDataFlow absBearerInfo
        multiNodeScript
    .&&.
    prop_inbound_governor_no_unsupported_state
        (Fixed 0) 0 arbDataFlow absBearerInfo
        multiNodeScript

ppScript :: (Show peerAddr, Show req) => MultiNodeScript peerAddr req -> String
ppScript (MultiNodeScript script _) = intercalate "\n" $ go 0 script
  where
    delay (StartServer             d _ _) = d
    delay (StartClient             d _)   = d
    delay (InboundConnection       d _)   = d
    delay (OutboundConnection      d _)   = d
    delay (InboundMiniprotocols    d _ _) = d
    delay (OutboundMiniprotocols   d _ _) = d
    delay (CloseInboundConnection  d _)   = d
    delay (CloseOutboundConnection d _)   = d
    delay (ShutdownClientServer    d _)   = d

    ppEvent (StartServer             _ a i) = "Start server " ++ show a ++ " with accInit=" ++ show i
    ppEvent (StartClient             _ a)   = "Start client " ++ show a
    ppEvent (InboundConnection       _ a)   = "Connection from " ++ show a
    ppEvent (OutboundConnection      _ a)   = "Connecting to " ++ show a
    ppEvent (InboundMiniprotocols    _ a p) = "Miniprotocols from " ++ show a ++ ": " ++ ppData p
    ppEvent (OutboundMiniprotocols   _ a p) = "Miniprotocols to " ++ show a ++ ": " ++ ppData p
    ppEvent (CloseInboundConnection  _ a)   = "Close connection from " ++ show a
    ppEvent (CloseOutboundConnection _ a)   = "Close connection to " ++ show a
    ppEvent (ShutdownClientServer    _ a)   = "Shutdown client/server " ++ show a

    ppData (TemperatureBundle hot warm est) =
      concat [ "hot:", show (withoutProtocolTemperature hot)
             , " warm:", show (withoutProtocolTemperature warm)
             , " est:", show (withoutProtocolTemperature est)]

    go _ [] = []
    go t (e : es) = printf "%5s: %s" (show t') (ppEvent e) : go t' es
      where t' = t + delay e

--
-- Utils
--


dynamicTracer :: (Typeable a, Show a) => Tracer (IOSim s) a
dynamicTracer = Tracer traceM <> sayTracer

toNonFailing :: Script AbsBearerInfo -> Script AbsBearerInfo
toNonFailing = unNFBIScript
             . toNonFailingAbsBearerInfoScript
             . AbsBearerInfoScript

traceWithNameTraceEvents :: forall b. Typeable b
                         => SimTrace () -> Trace (SimResult ()) b
traceWithNameTraceEvents = fmap wnEvent
          . Trace.filter ((MainServer ==) . wnName)
          . traceSelectTraceEventsDynamic
              @(SimResult ())
              @(WithName (Name SimAddr) b)

withNameTraceEvents :: forall b. Typeable b => SimTrace () -> [b]
withNameTraceEvents = fmap wnEvent
          . filter ((MainServer ==) . wnName)
          . selectTraceEventsDynamic
              @()
              @(WithName (Name SimAddr) b)

withTimeNameTraceEvents :: forall b. Typeable b
                        => SimTrace ()
                        -> Trace (SimResult ()) (WithTime b)
withTimeNameTraceEvents = fmap (\(WithTime t (WithName _ e)) -> WithTime t e)
          . Trace.filter ((MainServer ==) . wnName . wtEvent)
          . traceSelectTraceEventsDynamic
              @(SimResult ())
              @(WithTime (WithName (Name SimAddr) b))

showConnectionEvents :: ConnectionEvent req peerAddr -> String
showConnectionEvents StartClient{}             = "StartClient"
showConnectionEvents StartServer{}             = "StartServer"
showConnectionEvents InboundConnection{}       = "InboundConnection"
showConnectionEvents OutboundConnection{}      = "OutboundConnection"
showConnectionEvents InboundMiniprotocols{}    = "InboundMiniprotocols"
showConnectionEvents OutboundMiniprotocols{}   = "OutboundMiniprotocols"
showConnectionEvents CloseInboundConnection{}  = "CloseInboundConnection"
showConnectionEvents CloseOutboundConnection{} = "CloseOutboundConnection"
showConnectionEvents ShutdownClientServer{}    = "ShutdownClientServer"


-- | Redefine this tracer to get valuable tracing information from various
-- components:
--
-- * connection-manager
-- * inbound governor
-- * server
--
{-
debugTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
debugTracer = Tracer (\msg -> (\a -> (a,msg)) <$> getCurrentTime >>= say . show)
           <> Tracer (\msg -> (\a -> (a,msg)) <$> getCurrentTime >>= Debug.traceShowM)
-}

-- | Convenience function to create a TemperatureBundle. Could move to Ouroboros.Network.Mux.
makeBundle :: (forall pt. SingProtocolTemperature pt -> a) -> TemperatureBundle a
makeBundle f = TemperatureBundle (WithHot         $ f SingHot)
                                 (WithWarm        $ f SingWarm)
                                 (WithEstablished $ f SingEstablished)


-- TODO: we should use @traceResult True@; the `prop_unidirectional_Sim` and
-- `prop_bidirectional_Sim` test are failing with `<<io-sim sloppy shutdown>>`
-- exception.
--
simulatedPropertyWithTimeout :: DiffTime -> (forall s. IOSim s Property) -> Property
simulatedPropertyWithTimeout t test =
  counterexample ("\nTrace:\n" ++ prettyPrintTrace tr) $
  case traceResult False tr of
    Left failure ->
      counterexample ("Failure:\n" ++ displayException failure) False
    Right prop -> fromMaybe (counterexample "timeout" $ property False) prop
  where
    tr = runSimTrace $ timeout t test


prettyPrintTrace :: SimTrace a -> String
prettyPrintTrace tr = concat
    [ "====== Trace ======\n"
    , ppTrace_ tr
    , "\n\n====== Say Events ======\n"
    , intercalate "\n" $ selectTraceEventsSay' tr
    , "\n"
    ]
