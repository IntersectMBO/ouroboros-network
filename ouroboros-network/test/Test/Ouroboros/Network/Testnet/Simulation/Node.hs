{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ouroboros.Network.Testnet.Simulation.Node
  ( SimArgs (..)
  , NodeArgs (..)
  , DiffusionScript (..)
  , HotDiffusionScript (..)
  , DiffusionSimulationTrace (..)
  , prop_diffusionScript_fixupCommands
  , prop_diffusionScript_commandScript_valid
  , diffusionSimulation
  , Command (..)
    -- * Tracing
  , DiffusionTestTrace (..)
  , iosimTracer
    -- * Re-exports
  , TestAddress (..)
  , RelayAccessPoint (..)
  , Script (..)
  , module PeerSelection
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad (forM, replicateM, (>=>))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Fix
import           Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)

import           Control.Monad.IOSim (IOSim, traceM)

import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (traverse_)
import           Data.IP (IP (..), toIPv4, toIPv6)
import           Data.List (delete, intercalate, nub, (\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToDiffTime)
import           Data.Void (Void)
import qualified System.Random as Random
import           System.Random (StdGen, mkStdGen)

import           Network.DNS (Domain, TTL)

import           Network.TypedProtocol.Core (PeerHasAgency (..))
import qualified Network.TypedProtocol.PingPong.Type as PingPong

import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import           Ouroboros.Network.ConnectionManager.Types
                     (AbstractTransitionTrace, ConnectionManagerTrace)
import qualified Ouroboros.Network.Diffusion.P2P as Diff.P2P
import           Ouroboros.Network.Driver.Limits (ProtocolSizeLimits (..),
                     ProtocolTimeLimits (..))
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace,
                     RemoteTransitionTrace)
import           Ouroboros.Network.Mux (MiniProtocolLimits (..))
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.Governor
                     (DebugPeerSelection (..), PeerSelectionTargets (..),
                     TracePeerSelection)
import qualified Ouroboros.Network.PeerSelection.Governor as PeerSelection
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface (..), UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.PeerStateActions
                     (PeerSelectionActionsTrace)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DomainAccessPoint (..), LookupReqs (..), PortNumber,
                     RelayAccessPoint (..), TraceLocalRootPeers,
                     TracePublicRootPeers)
import           Ouroboros.Network.Protocol.BlockFetch.Codec
                     (byteLimitsBlockFetch, timeLimitsBlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..), byteLimitsChainSync,
                     timeLimitsChainSync)
import           Ouroboros.Network.Protocol.Handshake.Version (Accept (Accept))
import           Ouroboros.Network.Protocol.KeepAlive.Codec
                     (byteLimitsKeepAlive, timeLimitsKeepAlive)
import           Ouroboros.Network.Protocol.Limits (shortWait, smallByteLimit)
import           Ouroboros.Network.Server.RateLimiting
                     (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Server2 (ServerTrace)
import           Ouroboros.Network.Snocket (Snocket, TestAddress (..))

import           Ouroboros.Network.Block (BlockNo)
import           Ouroboros.Network.Mock.ConcreteBlock (Block (..),
                     BlockHeader (..))
import           Ouroboros.Network.Testing.Data.Script (Script (..))
import           Ouroboros.Network.Testing.Utils (WithName (..), WithTime (..),
                     genDelayWithPrecision, tracerWithName,
                     tracerWithTime)
import           Simulation.Network.Snocket (BearerInfo (..), FD, SnocketTrace,
                     WithAddr (..), makeFDBearer, withSnocket)

import qualified Test.Ouroboros.Network.Diffusion.Node as NodeKernel
import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel
                     (BlockGeneratorArgs, NtCAddr, NtCVersion, NtCVersionData,
                     NtNAddr, NtNAddr_ (IPAddr), NtNVersion, NtNVersionData,
                     randomBlockGenerationArgs)
import qualified Test.Ouroboros.Network.PeerSelection.RootPeersDNS as PeerSelection hiding
                     (tests)
import           Test.Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DNSLookupDelay (..), DNSTimeout (..))

import           Control.Monad.Class.MonadMVar (MonadMVar)
import           Ouroboros.Network.PeerSelection.PeerAdvertise
                     (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import           Ouroboros.Network.Protocol.PeerSharing.Codec
                     (byteLimitsPeerSharing, timeLimitsPeerSharing)
import           Ouroboros.Network.BlockFetch (TraceFetchClientState,
                     TraceLabelPeer (..))
import           Test.QuickCheck

-- | Diffusion Simulator Arguments
--
-- Contains all necessary randomly generated values needed to run diffusion in
-- simulation.
--
data SimArgs =
  SimArgs
    { saSlot  :: DiffTime
      -- ^ 'randomBlockGenerationArgs' slot duration argument
    , saQuota :: Int
      -- ^ 'randomBlockGenerationArgs' quota value
    }

instance Show SimArgs where
    show SimArgs { saSlot, saQuota } =
      unwords [ "SimArgs"
              , show saSlot
              , show saQuota
              ]

-- | Diffusion Simulator Node Arguments
--
-- Contains all necessary randomly generated values needed to run a node in
-- simulation.
--
data NodeArgs =
  NodeArgs
    { naSeed                   :: Int
      -- ^ 'randomBlockGenerationArgs' seed argument
    , naDiffusionMode          :: DiffusionMode
    , naMbTime                 :: Maybe DiffTime
      -- ^ 'LimitsAndTimeouts' argument
    , naRelays                 :: Map RelayAccessPoint PeerAdvertise
      -- ^ 'Interfaces' relays auxiliary value
    , naDomainMap              :: Map Domain [IP]
      -- ^ 'Interfaces' 'iDomainMap' value
    , naAddr                   :: NtNAddr
      -- ^ 'Arguments' 'aIPAddress' value
    , naPeerSharing            :: PeerSharing
      -- ^ 'Arguments' 'aIPAddress' value
    , naLocalRootPeers         :: [(Int, Map RelayAccessPoint PeerAdvertise)]
      -- ^ 'Arguments' 'LocalRootPeers' values
    , naLocalSelectionTargets  :: PeerSelectionTargets
      -- ^ 'Arguments' 'aLocalSelectionTargets' value
    , naDNSTimeoutScript       :: Script DNSTimeout
      -- ^ 'Arguments' 'aDNSTimeoutScript' value
    , naDNSLookupDelayScript   :: Script DNSLookupDelay
      -- ^ 'Arguments' 'aDNSLookupDelayScript' value
    , naChainSyncExitOnBlockNo :: Maybe BlockNo
    , naChainSyncEarlyExit     :: Bool
    }

instance Show NodeArgs where
    show NodeArgs { naSeed, naMbTime, naRelays, naDomainMap,
                   naAddr, naLocalRootPeers, naLocalSelectionTargets,
                   naDNSTimeoutScript, naDNSLookupDelayScript } =
      unwords [ "NodeArgs"
              , "(" ++ show naSeed ++ ")"
              , "(" ++ show naMbTime ++ ")"
              , show naRelays
              , "(Map.fromList ["
                ++ Map.foldMapWithKey
                    (\domain ips
                      -> "(" ++ show domain ++ ", " ++ showIPs ips ++ ")")
                    naDomainMap
                ++ "])"
              , "(" ++ show naAddr ++ ")"
              , show naLocalRootPeers
              , show naLocalSelectionTargets
              , "(" ++ show naDNSTimeoutScript ++ ")"
              , "(" ++ show naDNSLookupDelayScript ++ ")"
              ]
      where
        showIPs :: [IP] -> String
        showIPs ips = "["
                   ++ intercalate ", "
                                 (map (\ip -> "read \"" ++ show ip ++ "\"") ips)
                   ++ "]"

data Command = JoinNetwork DiffTime (Maybe NtNAddr)
             | Kill DiffTime
             | Reconfigure DiffTime
                           [(Int, Map RelayAccessPoint PeerAdvertise)]
  deriving Eq

instance Show Command where
    showsPrec d (JoinNetwork delay (Just addr)) = showString "JoinNetwork "
                                                . showsPrec d delay
                                                . showString " "
                                                . showParen True ( showString "Just "
                                                                 . showParen True (showsPrec d addr))
    showsPrec d (JoinNetwork delay Nothing)     = showString "JoinNetwork "
                                                . showsPrec d delay
                                                . showString " Nothing"
    showsPrec d (Kill delay)                    = showString "Kill "
                                                . showsPrec d delay
    showsPrec d (Reconfigure delay localRoots)  = showString "Reconfigure "
                                                . showsPrec d delay
                                                . showString " "
                                                . showsPrec d localRoots

-- | Generate DNS table
genDomainMap :: [RelayAccessPoint] -> IP -> Gen (Map Domain [IP])
genDomainMap raps selfIP = do
  let domains = [ d | RelayAccessDomain d _ <- raps ]
      ips     = [ ip | RelayAccessAddress ip _ <- raps ]
  m <- mapM (\d -> do
    size <- chooseInt (1, 5)
    ips' <- nub <$> vectorOf size (genIP ips)
    return (d, delete selfIP ips')) domains

  return (Map.fromList m)

genIP :: [IP] -> Gen IP
genIP ips =
  let genIPv4 = IPv4 . toIPv4 <$> replicateM 4 (choose (0,255))
      genIPv6 = IPv6 . toIPv6 <$> replicateM 8 (choose (0,0xffff))
   in oneof ([genIPv4, genIPv6] ++ map pure ips)

genCommands :: [(Int, Map RelayAccessPoint PeerAdvertise)] -> Gen [Command]
genCommands localRoots = sized $ \size -> do
  port <- fromIntegral <$> (arbitrary :: Gen Int)
  commands <- vectorOf size (frequency [ (1, JoinNetwork
                                              <$> delay
                                              <*> ( Just
                                                  . TestAddress
                                                  . flip IPAddr port
                                                  <$> genIP []
                                                  ))
                                       , (10, JoinNetwork
                                              <$> delay
                                              <*> pure Nothing)
                                       , (6, Reconfigure
                                              <$> delay
                                              <*> subLocalRootPeers)
                                       , (3, Kill <$> delay)
                                       ])
  return (fixupCommands commands)
  where
    subLocalRootPeers :: Gen [(Int, Map RelayAccessPoint PeerAdvertise)]
    subLocalRootPeers = do
      subLRP <- sublistOf localRoots
      mapM (mapM (fmap Map.fromList . sublistOf . Map.toList)) subLRP

    delay = frequency [ (3, genDelayWithPrecision 100)
                      , (2, (* 10) <$> genDelayWithPrecision 100)
                      , (1, (/ 10) <$> genDelayWithPrecision 100)
                      ]

fixupCommands :: [Command] -> [Command]
fixupCommands [] = []
fixupCommands (jn@(JoinNetwork _ _):t) = jn : go jn t
  where
    go :: Command -> [Command] -> [Command]
    go _ [] = []
    go prev (cmd:cmds) =
      case (prev, cmd) of
        (JoinNetwork _ _ , JoinNetwork _ _ ) -> go prev cmds
        (Kill _          , Kill _          ) -> go prev cmds
        (Kill _          , Reconfigure _ _ ) -> go prev cmds
        (Reconfigure _ _ , JoinNetwork _ _ ) -> go prev cmds
        _                                    -> cmd : go cmd cmds
fixupCommands (_:t) = fixupCommands t

-- | Arguments to run in simulation
--
mainnetSimArgs :: [RelayAccessPoint] -> SimArgs
mainnetSimArgs raps =
  -- Slot length needs to be greater than 0 else we get a livelock on
  -- the IOSim.
  --
  -- Quota values matches mainnet, so a slot length of 1s and 1 / 20
  -- chance that someone gets to make a block
  let bgaSlotDuration = secondsToDiffTime 1
      numberOfNodes   = length [ r | r@(RelayAccessAddress _ _) <- raps ]
      quota = if numberOfNodes > 0
                then 20 `div` numberOfNodes
                else 100

   in SimArgs { saSlot  = bgaSlotDuration,
                saQuota = quota
              }

-- | Given a NtNAddr generate the necessary things to run a node in
-- Simulation
genNodeArgs :: [RelayAccessPoint]
           -> Int
           -> ( [RelayAccessPoint]
             -> RelayAccessPoint
             -> Gen [(Int, Map RelayAccessPoint PeerAdvertise)] )
           -> (NtNAddr, RelayAccessPoint)
           -> Gen NodeArgs
genNodeArgs raps minConnected genLocalRootPeers (ntnAddr, rap) = do
  -- Slot length needs to be greater than 0 else we get a livelock on
  -- the IOSim.
  --
  -- Quota values matches mainnet, so a slot length of 1s and 1 / 20
  -- chance that someone gets to make a block
  let rapsWithoutSelf = delete rap raps
      (RelayAccessAddress rapIP _) = rap
  seed <- arbitrary

  -- Generating an InitiatorResponderMode node is 3 times more likely since we
  -- want our tests to cover more this case.
  diffusionMode <- frequency [ (1, pure InitiatorOnlyDiffusionMode)
                             , (3, pure InitiatorAndResponderDiffusionMode)
                             ]

  dMap <- genDomainMap rapsWithoutSelf rapIP

  -- These values approximately correspond to false positive
  -- thresholds for streaks of empty slots with 99% probability,
  -- 99.9% probability up to 99.999% probability.
  -- t = T_s [log (1-Y) / log (1-f)]
  -- Y = [0.99, 0.999...]
  --
  -- T_s = slot length of 1s.
  -- f = 0.05
  -- The timeout is randomly picked per bearer to avoid all bearers
  -- going down at the same time in case of a long streak of empty
  -- slots. TODO: workaround until peer selection governor.
  -- Taken from ouroboros-consensus/src/Ouroboros/Consensus/Node.hs
  mustReplyTimeout <- Just <$> oneof (pure <$> [90, 135, 180, 224, 269])

  lrp <- genLocalRootPeers rapsWithoutSelf rap
  relays <- sublistOf rapsWithoutSelf
  relayPeerAdvertise <- vectorOf (length relays) arbitrary
  let relayMap = Map.fromList (zip relays relayPeerAdvertise)

  -- Make sure our targets for active peers cover the maximum of peers
  -- one generated
  peerSelectionTargets <- arbitrary `suchThat` hasActive minConnected

  dnsTimeout <- arbitrary
  dnsLookupDelay <- arbitrary
  chainSyncExitOnBlockNo
    <- frequency [ (1,      Just . fromIntegral . getPositive
                        <$> (arbitrary :: Gen (Positive Int))
                            `suchThat` (\(Positive a) -> a < 5))
                 , (4, pure Nothing)
                 ]

  chainSyncEarlyExit <- frequency [ (1, pure True)
                                  , (9, pure False)
                                  ]

  peerSharing <- arbitrary

  return
   $ NodeArgs
      { naSeed                   = seed
      , naDiffusionMode          = diffusionMode
      , naMbTime                 = mustReplyTimeout
      , naRelays                 = relayMap
      , naDomainMap              = dMap
      , naAddr                   = ntnAddr
      , naLocalRootPeers         = lrp
      , naLocalSelectionTargets  = peerSelectionTargets
      , naDNSTimeoutScript       = dnsTimeout
      , naDNSLookupDelayScript   = dnsLookupDelay
      , naChainSyncExitOnBlockNo = chainSyncExitOnBlockNo
      , naChainSyncEarlyExit     = chainSyncEarlyExit
      , naPeerSharing            = peerSharing
      }
  where
    hasActive :: Int -> PeerSelectionTargets -> Bool
    hasActive minConn (PeerSelectionTargets _ _ _ y) = y > minConn

-- | Multinode Diffusion Simulator Script
--
-- 'SimArgs' with all the values needed for running the simulation, followed
-- by a list of 'NodeArgs' where each element represents one running node and
-- respective 'Command's.
--
data DiffusionScript = DiffusionScript SimArgs [(NodeArgs, [Command])]
  deriving Show

-- | Multinode Diffusion Simulator Script
--
-- Tries to generate a reasonable looking network with at most 3 nodes that can
-- or can not be connected to one another. These nodes can also randomly die or
-- have their local configuration changed.
--
genNonHotDiffusionScript :: Gen (SimArgs, [(NodeArgs, [Command])])
genNonHotDiffusionScript = do
  -- Limit the number of nodes to run in Simulation otherwise it is going
  -- to take very long time for tests to run
  size <- chooseInt (0, 3)
  raps <- nub <$> vectorOf size arbitrary

  let toRunRaps = [ r | r@(RelayAccessAddress _ _) <- raps ]
      simArgs = mainnetSimArgs raps
  toRun <- mapM (genNodeArgs raps 0 genLocalRootPeers)
               [ (ntnToPeerAddr ip p, r)
               | r@(RelayAccessAddress ip p) <- toRunRaps ]

  comands <- mapM (genLocalRootPeers raps >=> genCommands) toRunRaps

  return (simArgs, zip toRun comands)
  where
    -- | Generate Local Root Peers
    --
    genLocalRootPeers :: [RelayAccessPoint]
                      -> RelayAccessPoint
                      -> Gen [(Int, Map RelayAccessPoint PeerAdvertise)]
    genLocalRootPeers l r = do
      nrGroups <- chooseInt (1, 3)
      -- Remove self from local root peers
      let newL = l \\ [r]
          size = length newL
          sizePerGroup = (size `div` nrGroups) + 1

      peerAdvertise <- vectorOf size arbitrary

      let relaysAdv = zip newL peerAdvertise
          relayGroups = divvy sizePerGroup sizePerGroup relaysAdv
          relayGroupsMap = Map.fromList <$> relayGroups

      target <- forM relayGroups
                    (\x -> if null x
                          then pure 0
                          else chooseInt (1, length x))

      let lrpGroups = zip target relayGroupsMap

      return lrpGroups

-- | Multinode Hot Diffusion Simulator Script
--
-- Tries to generate a network with at most 2 nodes that should
-- be connected to one another. This generator tries to obtain high ratios of
-- active peers so we can test the miniprotocols that run when we have such
-- active connections. These nodes can not randomly die or have their local
-- configuration changed. Their local root peers consist of a single group.
--
genHotDiffusionScript :: Gen (SimArgs, [(NodeArgs, [Command])])
genHotDiffusionScript = do

    -- Since we want to maximize active peers in tests we want to have
    -- a set of RelayAccessAddress peers to immediately try to connect.
    let minConnected = 2

    -- We want to make sure the nodes we connect to are RelayAccessAddresses.
    -- They could be domains but it is easier to connect to IPs straight away.
    -- We also want to make sure we get as many peers as specified by
    -- 'minConnected'.
    raas <- (nub <$> vectorOf minConnected (arbitrary `suchThat` isRelayAccessAddress))
            `suchThat` ((>= minConnected) . length)

    let allRaps = nub raas
        toRunRaps = [ r | r@(RelayAccessAddress _ _) <- allRaps ]

        -- Nodes are not killed
        comands = repeat [JoinNetwork 0 Nothing]

        simArgs = mainnetSimArgs allRaps
    toRun <- mapM (genNodeArgs allRaps minConnected genLocalRootPeers)
                 [ (ntnToPeerAddr ip p, r)
                 | r@(RelayAccessAddress ip p) <- toRunRaps ]


    return (simArgs, zip toRun comands)
    where
      isRelayAccessAddress :: RelayAccessPoint -> Bool
      isRelayAccessAddress RelayAccessAddress{} = True
      isRelayAccessAddress _                    = False

      -- | Generate Local Root Peers
      -- This only generates 1 group
      genLocalRootPeers :: [RelayAccessPoint]
                        -> RelayAccessPoint
                        -> Gen [(Int, Map RelayAccessPoint PeerAdvertise)]
      genLocalRootPeers l r = do
        -- Remove self from local root peers
        let newL = delete r l
            size = length newL

        peerAdvertise <- vectorOf size arbitrary

        let relaysAdv      = zip newL peerAdvertise
            relayGroupsMap = Map.fromList relaysAdv
            target         = length relaysAdv

        return [(target, relayGroupsMap)]

instance Arbitrary DiffusionScript where
  arbitrary = uncurry DiffusionScript
            <$> frequency [ (1, genNonHotDiffusionScript)
                          , (1, genHotDiffusionScript)
                          ]
  shrink (DiffusionScript _ []) = []
  shrink (DiffusionScript sargs ((nargs, cmds):s)) = do
    shrinkedCmds <- fixupCommands <$> shrinkList shrinkCommand cmds
    DiffusionScript sa ss <- shrink (DiffusionScript sargs s)
    return (DiffusionScript sa ((nargs, shrinkedCmds) : ss))
    where
      shrinkDelay = map fromRational . shrink . toRational

      shrinkCommand :: Command -> [Command]
      shrinkCommand (JoinNetwork d ip)  = JoinNetwork <$> shrinkDelay d
                                                      <*> pure ip
      shrinkCommand (Kill d)            = Kill        <$> shrinkDelay d
      shrinkCommand (Reconfigure d lrp) = Reconfigure <$> shrinkDelay d
                                                      <*> shrink lrp

-- | Multinode Hot Diffusion Simulator Script
--
-- List of 'SimArgs'. Each element of the list represents one running node.
--
data HotDiffusionScript = HotDiffusionScript SimArgs [(NodeArgs, [Command])]
  deriving Show

instance Arbitrary HotDiffusionScript where
  arbitrary = uncurry HotDiffusionScript <$> genHotDiffusionScript
  shrink (HotDiffusionScript sargs hds) =
    [ HotDiffusionScript sa ds
    | DiffusionScript sa ds <- shrink (DiffusionScript sargs hds) ]

-- Tests if the fixupCommand is idempotent.
-- Note that the generator for DiffusionScript already fixups the Command list.
--
prop_diffusionScript_fixupCommands :: DiffusionScript -> Property
prop_diffusionScript_fixupCommands (DiffusionScript _ []) = property True
prop_diffusionScript_fixupCommands (DiffusionScript sa ((_, cmds): t)) =
  counterexample ("Failed with cmds: " ++ show cmds ++ "\n"
                  ++ "fixupCommands cmds = " ++ show (fixupCommands cmds)
                 ) $
  fixupCommands cmds == cmds
  .&&. prop_diffusionScript_fixupCommands (DiffusionScript sa t)

-- Tests if the fixupCommand outputs valid command scripts.
--
-- Note that the generator for DiffusionScript already fixups the Command list.
--
prop_diffusionScript_commandScript_valid :: DiffusionScript -> Property
prop_diffusionScript_commandScript_valid (DiffusionScript _ []) = property True
prop_diffusionScript_commandScript_valid (DiffusionScript sa ((_, cmds): t)) =
  counterexample ("Failed with cmds: " ++ show cmds) $
  isValid cmds
  .&&. prop_diffusionScript_commandScript_valid (DiffusionScript sa t)
  where
    isValid :: [Command] -> Property
    isValid [] = property True
    isValid [_] = property True
    isValid (x:y:xs) =
      case (x, y) of
        (JoinNetwork _ _, JoinNetwork _ _)   ->
          counterexample ("Invalid sequence: " ++ show x ++ " " ++ show y) $
            property False
        (Kill _, Kill _)                 ->
          counterexample ("Invalid sequence: " ++ show x ++ " " ++ show y) $
            property False
        (Kill _, Reconfigure _ _)        ->
          counterexample ("Invalid sequence: " ++ show x ++ " " ++ show y) $
            property False
        (Reconfigure _ _, JoinNetwork _ _) ->
          counterexample ("Invalid sequence: " ++ show x ++ " " ++ show y) $
            property False
        _                                -> isValid (y:xs)

-- | Diffusion Simulation Trace so we know what command is concurrently
-- running
--
data DiffusionSimulationTrace
  = TrJoiningNetwork
  | TrKillingNode
  | TrReconfiguringNode
  | TrUpdatingDNS
  | TrRunning
  deriving (Show)

-- Warning: be careful with writing properties that rely
-- on trace events from multiple components environment.
-- These events typically occur in separate threads and
-- so are not casually ordered. It is ok to use them for
-- timeout/eventually properties, but not for properties
-- that check conditions synchronously.
--
data DiffusionTestTrace =
      DiffusionLocalRootPeerTrace (TraceLocalRootPeers NtNAddr SomeException)
    | DiffusionPublicRootPeerTrace TracePublicRootPeers
    | DiffusionPeerSelectionTrace (TracePeerSelection NtNAddr)
    | DiffusionPeerSelectionActionsTrace (PeerSelectionActionsTrace NtNAddr NtNVersion)
    | DiffusionDebugPeerSelectionTrace (DebugPeerSelection NtNAddr)
    | DiffusionConnectionManagerTrace
        (ConnectionManagerTrace NtNAddr
          (ConnectionHandlerTrace NtNVersion NtNVersionData))
    | DiffusionDiffusionSimulationTrace DiffusionSimulationTrace
    | DiffusionConnectionManagerTransitionTrace
        (AbstractTransitionTrace NtNAddr)
    | DiffusionInboundGovernorTransitionTrace
        (RemoteTransitionTrace NtNAddr)
    | DiffusionInboundGovernorTrace (InboundGovernorTrace NtNAddr)
    | DiffusionServerTrace (ServerTrace NtNAddr)
    | DiffusionFetchTrace (TraceFetchClientState BlockHeader)
    | DiffusionDebugTrace String
    deriving (Show)


-- | A debug tracer which embeds events in DiffusionTestTrace.
--
iosimTracer :: forall s. Tracer (IOSim s) (WithTime (WithName NtNAddr DiffusionTestTrace))
iosimTracer = Tracer traceM


-- | Run an arbitrary topology
diffusionSimulation
  :: forall m. ( MonadAsync       m
               , MonadFix         m
               , MonadFork        m
               , MonadSay         m
               , MonadST          m
               , MonadEvaluate    m
               , MonadLabelledSTM m
               , MonadTraceSTM    m
               , MonadCatch       m
               , MonadMask        m
               , MonadTime        m
               , MonadTimer       m
               , MonadThrow  (STM m)
               , MonadMVar        m
               , Eq (Async m Void)
               , forall a. Semigroup a => Semigroup (m a)
               )
  => BearerInfo
  -> DiffusionScript
  -> Tracer m (WithTime (WithName NtNAddr DiffusionTestTrace))
  -- ^ timed trace of nodes in the system
  -> Tracer m (WithName NtNAddr DiffusionSimulationTrace)
  -> m Void
diffusionSimulation
  defaultBearerInfo
  (DiffusionScript simArgs nodeArgs)
  nodeTracer
  tracer =
    withSnocket netSimTracer defaultBearerInfo Map.empty
      $ \ntnSnocket _ ->
        withSnocket nullTracer defaultBearerInfo Map.empty
      $ \ntcSnocket _ -> do
        let dnsMaps = map (\(na, _)
                            -> (naAddr na, fmap (, 0) <$> naDomainMap na))
                          nodeArgs
        dnsMapVarMap <- Map.fromList <$> mapM (mapM (newTVarIO @m)) dnsMaps
        withAsyncAll
          (map (uncurry (runCommand Nothing ntnSnocket ntcSnocket dnsMapVarMap simArgs))
                        nodeArgs)
          $ \nodes -> do
            (_, x) <- waitAny nodes
            return x
  where
    netSimTracer :: Tracer m (WithAddr NtNAddr (SnocketTrace m NtNAddr))
    netSimTracer = (\(WithAddr l _ a) -> WithName (fromMaybe (TestAddress $ IPAddr (read "0.0.0.0") 0) l) (show a))
       `contramap` tracerWithTime nullTracer

    -- | Runs a single node according to a list of commands.
    runCommand
      :: Maybe ( Async m Void
               , StrictTVar m [(Int, Map RelayAccessPoint PeerAdvertise)])
         -- ^ If the node is running and corresponding local root configuration
         -- TVar.
      -> Snocket m (FD m NtNAddr) NtNAddr
        -- ^ Node to node Snocket
      -> Snocket m (FD m NtCAddr) NtCAddr
        -- ^ Node to client Snocket
      -> Map NtNAddr (StrictTVar m (Map Domain [(IP, TTL)]))
        -- ^ Map of domain map TVars to be updated in case a node changes its IP
      -> SimArgs -- ^ Simulation arguments needed in order to run a simulation
      -> NodeArgs -- ^ Simulation arguments needed in order to run a single node
      -> [Command] -- ^ List of commands/actions to perform for a single node
      -> m Void
    runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs [] = do
      threadDelay 3600
      traceWith tracer (WithName (naAddr nArgs) TrRunning)
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs []
    runCommand (Just (_, _)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs [] = do
      -- We shouldn't block this thread waiting
      -- on the async since this will lead to a deadlock
      -- as thread returns 'Void'.
      threadDelay 3600
      traceWith tracer (WithName (naAddr nArgs) TrRunning)
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs []
    runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs
               (JoinNetwork delay Nothing:cs) = do
      threadDelay delay
      traceWith tracer (WithName (naAddr nArgs) TrJoiningNetwork)
      lrpVar <- newTVarIO $ naLocalRootPeers nArgs
      let dnsMapVar = dMapVarMap Map.! naAddr nArgs
      withAsync (runNode sArgs nArgs ntnSnocket ntcSnocket lrpVar dnsMapVar) $ \nodeAsync ->
        runCommand (Just (nodeAsync, lrpVar)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs cs
    runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs
               (JoinNetwork delay (Just ip):cs) = do
      threadDelay delay
      let nArgs' = nArgs { naAddr = ip }
      traceWith tracer (WithName ip TrJoiningNetwork)
      lrpVar <- newTVarIO $ naLocalRootPeers nArgs'

      -- Updating DomainMap entry now that the node is having a new IP
      let dnsMapVar = dMapVarMap Map.! naAddr nArgs
      let dMapVarMap' = Map.delete (naAddr nArgs) dMapVarMap
          dMapVarMap'' = Map.insert ip dnsMapVar dMapVarMap'

      withAsync (runNode sArgs nArgs' ntnSnocket ntcSnocket lrpVar dnsMapVar)
        $ \nodeAsync ->
          withAsync (updateDomainMap delay (naAddr nArgs) ip dMapVarMap'')
            $ \_ ->
              runCommand (Just (nodeAsync, lrpVar)) ntnSnocket ntcSnocket
                         dMapVarMap'' sArgs nArgs' cs
    runCommand _ _ _ _ _ _ (JoinNetwork _ _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async_, _)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs
               (Kill delay:cs) = do
      threadDelay delay
      traceWith tracer (WithName (naAddr nArgs) TrKillingNode)
      cancel async_
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs cs
    runCommand _ _ _ _ _ _ (Kill _:_) = do
      error "runCommand: Impossible happened"
    runCommand Nothing _ _ _ _ _ (Reconfigure _ _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async_, lrpVar)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs
               (Reconfigure delay newLrp:cs) = do
      threadDelay delay
      traceWith tracer (WithName (naAddr nArgs) TrReconfiguringNode)
      _ <- atomically $ writeTVar lrpVar newLrp
      runCommand (Just (async_, lrpVar)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs
                 cs

    updateDomainMap :: DiffTime
                    -> NtNAddr
                    -> NtNAddr
                    -> Map NtNAddr (StrictTVar m (Map Domain [(IP, TTL)]))
                    -> m ()
    updateDomainMap delay
                    oip@(TestAddress (IPAddr oldIP _))
                    (TestAddress (IPAddr newIP _))
                    dMapVarMap = do
      threadDelay delay
      traceWith tracer (WithName oip TrUpdatingDNS)
      traverse_ (\dMapVar -> atomically $ do
                  dnsMap <- readTVar dMapVar
                  let dnsMap' =
                        Map.mapWithKey
                          (\_ l ->
                            case lookup oldIP l of
                              Nothing  -> l
                              Just ttl -> (newIP, ttl):delete (oldIP, ttl) l
                          )
                          dnsMap
                  writeTVar dMapVar dnsMap'
                )
                dMapVarMap
    updateDomainMap _ _ _ _ = return ()

    runNode :: SimArgs
            -> NodeArgs
            -> Snocket m (FD m NtNAddr) NtNAddr
            -> Snocket m (FD m NtCAddr) NtCAddr
            -> StrictTVar m [(Int, Map RelayAccessPoint PeerAdvertise)]
            -> StrictTVar m (Map Domain [(IP, TTL)])
            -> m Void
    runNode SimArgs
            { saSlot                  = bgaSlotDuration
            , saQuota                 = quota
            }
            NodeArgs
            { naSeed                   = seed
            , naMbTime                 = mustReplyTimeout
            , naRelays                 = raps
            , naAddr                   = rap
            , naLocalSelectionTargets  = peerSelectionTargets
            , naDNSTimeoutScript       = dnsTimeout
            , naDNSLookupDelayScript   = dnsLookupDelay
            , naChainSyncExitOnBlockNo = chainSyncExitOnBlockNo
            , naChainSyncEarlyExit     = chainSyncEarlyExit
            , naPeerSharing            = peerSharing
            }
            ntnSnocket
            ntcSnocket
            lrpVar
            dMapVar = do
      chainSyncExitVar <- newTVarIO chainSyncExitOnBlockNo
      let (bgaRng, rng) = Random.split $ mkStdGen seed
          acceptedConnectionsLimit =
            AcceptedConnectionsLimit maxBound maxBound 0
          diffusionMode = InitiatorAndResponderDiffusionMode
          readLocalRootPeers  = readTVar lrpVar
          readPublicRootPeers = return raps
          readUseLedgerAfter  = return (UseLedgerAfter 0)

          acceptVersion = \_ v -> Accept v

          defaultMiniProtocolsLimit :: MiniProtocolLimits
          defaultMiniProtocolsLimit =
            MiniProtocolLimits { maximumIngressQueue = 64000 }

          blockGeneratorArgs :: BlockGeneratorArgs Block StdGen
          blockGeneratorArgs =
            randomBlockGenerationArgs bgaSlotDuration
                                      bgaRng
                                      quota

          stdChainSyncTimeout :: ChainSyncTimeout
          stdChainSyncTimeout = do
              ChainSyncTimeout
                { canAwaitTimeout  = shortWait
                , intersectTimeout = shortWait
                , mustReplyTimeout
                }

          limitsAndTimeouts :: NodeKernel.LimitsAndTimeouts Block
          limitsAndTimeouts
            = NodeKernel.LimitsAndTimeouts
                { NodeKernel.chainSyncLimits      = defaultMiniProtocolsLimit
                , NodeKernel.chainSyncSizeLimits  = byteLimitsChainSync (const 0)
                , NodeKernel.chainSyncTimeLimits  =
                    timeLimitsChainSync stdChainSyncTimeout
                , NodeKernel.blockFetchLimits     = defaultMiniProtocolsLimit
                , NodeKernel.blockFetchSizeLimits = byteLimitsBlockFetch (const 0)
                , NodeKernel.blockFetchTimeLimits = timeLimitsBlockFetch
                , NodeKernel.keepAliveLimits      = defaultMiniProtocolsLimit
                , NodeKernel.keepAliveSizeLimits  = byteLimitsKeepAlive (const 0)
                , NodeKernel.keepAliveTimeLimits  = timeLimitsKeepAlive
                , NodeKernel.pingPongLimits       = defaultMiniProtocolsLimit
                , NodeKernel.pingPongSizeLimits   = byteLimitsPingPong
                , NodeKernel.pingPongTimeLimits   = timeLimitsPingPong
                , NodeKernel.handshakeLimits      = defaultMiniProtocolsLimit
                , NodeKernel.handshakeTimeLimits  =
                    ProtocolTimeLimits (const shortWait)
                , NodeKernel.handhsakeSizeLimits  =
                    ProtocolSizeLimits (const (4 * 1440))
                                       (fromIntegral . BL.length)
                , NodeKernel.peerSharingLimits     = defaultMiniProtocolsLimit
                , NodeKernel.peerSharingTimeLimits =
                    timeLimitsPeerSharing
                , NodeKernel.peerSharingSizeLimits =
                    byteLimitsPeerSharing (const 0)

                }

          interfaces :: NodeKernel.Interfaces m
          interfaces =
            NodeKernel.Interfaces
              { NodeKernel.iNtnSnocket        = ntnSnocket
              , NodeKernel.iNtnBearer         = makeFDBearer
              , NodeKernel.iAcceptVersion     = acceptVersion
              , NodeKernel.iNtnDomainResolver = domainResolver raps dMapVar
              , NodeKernel.iNtcSnocket        = ntcSnocket
              , NodeKernel.iNtcBearer         = makeFDBearer
              , NodeKernel.iRng               = rng
              , NodeKernel.iDomainMap         = dMapVar
              , NodeKernel.iLedgerPeersConsensusInterface
                                        = LedgerPeersConsensusInterface
                                        $ \_ -> return Nothing
              }

          shouldChainSyncExit :: StrictTVar m (Maybe BlockNo) -> Block -> m Bool
          shouldChainSyncExit v block = atomically $ do
            mbBlockNo <- readTVar v
            case mbBlockNo of
              Nothing ->
                return False

              Just blockNo | blockNo >= headerBlockNo (blockHeader block) -> do
                -- next time exit in 10 blocks
                writeTVar v (Just $ blockNo + 10)
                return True

                           | otherwise ->
                return False

          arguments :: NodeKernel.Arguments m
          arguments =
            NodeKernel.Arguments
              { NodeKernel.aIPAddress            = rap
              , NodeKernel.aAcceptedLimits       = acceptedConnectionsLimit
              , NodeKernel.aDiffusionMode        = diffusionMode
              , NodeKernel.aKeepAliveInterval    = 10
              , NodeKernel.aPingPongInterval     = 10
              , NodeKernel.aPeerSelectionTargets = peerSelectionTargets
              , NodeKernel.aShouldChainSyncExit  = shouldChainSyncExit chainSyncExitVar
              , NodeKernel.aChainSyncEarlyExit   = chainSyncEarlyExit
              , NodeKernel.aReadLocalRootPeers   = readLocalRootPeers
              , NodeKernel.aReadPublicRootPeers  = readPublicRootPeers
              , NodeKernel.aOwnPeerSharing       = peerSharing
              , NodeKernel.aReadUseLedgerAfter   = readUseLedgerAfter
              , NodeKernel.aProtocolIdleTimeout  = 5
              , NodeKernel.aTimeWaitTimeout      = 30
              , NodeKernel.aDNSTimeoutScript     = dnsTimeout
              , NodeKernel.aDNSLookupDelayScript = dnsLookupDelay
              , NodeKernel.aDebugTracer          = nullTracer
              }

      NodeKernel.run blockGeneratorArgs
                     limitsAndTimeouts
                     interfaces
                     arguments
                     (tracersExtra rap)
                     ( contramap (DiffusionFetchTrace . (\(TraceLabelPeer _ a) -> a))
                     . tracerWithName rap
                     . tracerWithTime
                     $ nodeTracer)

    domainResolver :: Map RelayAccessPoint PeerAdvertise
                   -> StrictTVar m (Map Domain [(IP, TTL)])
                   -> LookupReqs
                   -> [DomainAccessPoint]
                   -> m (Map DomainAccessPoint (Set NtNAddr))
    domainResolver raps dMapVar _ daps = do
      dMap <- fmap (map fst) <$> atomically (readTVar dMapVar)
      let domains    = [ (d, p) | (RelayAccessDomain d p, _) <- Map.assocs raps ]
          domainsAP  = uncurry DomainAccessPoint <$> domains
          mapDomains = [ ( DomainAccessPoint d p
                         , Set.fromList
                         $ uncurry ntnToPeerAddr
                         <$> zip (dMap Map.! d) (repeat p)
                         )
                       | DomainAccessPoint d p <- domainsAP \\ daps
                       , Map.member d dMap
                       ]
      return (Map.fromList mapDomains)

    tracersExtra
      :: NtNAddr
      -> Diff.P2P.TracersExtra NtNAddr NtNVersion NtNVersionData
                               NtCAddr NtCVersion NtCVersionData
                               SomeException m
    tracersExtra ntnAddr =
      Diff.P2P.TracersExtra {
          Diff.P2P.dtTraceLocalRootPeersTracer         = contramap DiffusionLocalRootPeerTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtTracePublicRootPeersTracer        = contramap
                                                          DiffusionPublicRootPeerTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtTracePeerSelectionTracer          = contramap
                                                          DiffusionPeerSelectionTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtDebugPeerSelectionInitiatorTracer = contramap
                                                          ( DiffusionDebugPeerSelectionTrace
                                                          . voidDebugPeerSelection
                                                          )
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtDebugPeerSelectionInitiatorResponderTracer
            = contramap
               ( DiffusionDebugPeerSelectionTrace
               . voidDebugPeerSelection
               )
            . tracerWithName ntnAddr
            . tracerWithTime
            $ nodeTracer
        , Diff.P2P.dtTracePeerSelectionCounters        = nullTracer
        , Diff.P2P.dtPeerSelectionActionsTracer        = contramap
                                                          DiffusionPeerSelectionActionsTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtConnectionManagerTracer           = contramap
                                                          DiffusionConnectionManagerTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtConnectionManagerTransitionTracer = contramap
                                                           DiffusionConnectionManagerTransitionTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtServerTracer                      = contramap DiffusionServerTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtInboundGovernorTracer             = contramap
                                                           DiffusionInboundGovernorTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtInboundGovernorTransitionTracer   = contramap
                                                           DiffusionInboundGovernorTransitionTrace
                                                       . tracerWithName ntnAddr
                                                       . tracerWithTime
                                                       $ nodeTracer
        , Diff.P2P.dtLocalConnectionManagerTracer      = nullTracer
        , Diff.P2P.dtLocalServerTracer                 = nullTracer
        , Diff.P2P.dtLocalInboundGovernorTracer        = nullTracer
      }
      where
        voidDebugPeerSelection :: DebugPeerSelection peeraddr -> DebugPeerSelection peeraddr
        voidDebugPeerSelection (TraceGovernorState btime wtime state) =
                                TraceGovernorState btime wtime (const () <$> state)


--
-- PingPong byte & time limits
--

byteLimitsPingPong :: ProtocolSizeLimits PingPong.PingPong BL.ByteString
byteLimitsPingPong = ProtocolSizeLimits (const smallByteLimit) (fromIntegral . BL.length)

timeLimitsPingPong :: ProtocolTimeLimits PingPong.PingPong
timeLimitsPingPong = ProtocolTimeLimits $ \case
    ClientAgency PingPong.TokIdle -> Nothing
    ServerAgency PingPong.TokBusy -> Just 60

--
-- Utils
--


ntnToPeerAddr :: IP -> PortNumber -> NtNAddr
ntnToPeerAddr a b = TestAddress (IPAddr a b)

withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action (reverse as)
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)

--
-- Taken from Data.List.Split.Internals from the split package
--

-- | A useful recursion pattern for processing a list to produce a new
--   list, often used for \"chopping\" up the input list.  Typically
--   chop is called with some function that will consume an initial
--   prefix of the list and produce a value and the rest of the list.
--
--   For example, many common Prelude functions can be implemented in
--   terms of @chop@:
--
-- > group :: (Eq a) => [a] -> [[a]]
-- > group = chop (\ xs@(x:_) -> span (==x) xs)
-- >
-- > words :: String -> [String]
-- > words =
--    filter (not . null) . chop (span (not . isSpace) . dropWhile isSpace)

chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
  where (b, as') = f as

-- | Divides up an input list into a set of sublists, according to 'n' and 'm'
--   input specifications you provide. Each sublist will have 'n' items, and the
--   start of each sublist will be offset by 'm' items from the previous one.
--
-- > divvy 5 5 [1..20] ==
--   [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
--
--   In the case where a source list's trailing elements do no fill an entire
--   sublist, those trailing elements will be dropped.
--
-- > divvy 5 2 [1..10] == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
--
--   As an example, you can generate a moving average over a list of prices:
--
-- > type Prices = [Float]
-- > type AveragePrices = [Float]
-- >
-- > average :: [Float] -> Float
-- > average xs = sum xs / (fromIntegral $ length xs)
-- >
-- > simpleMovingAverage :: Prices -> AveragePrices
-- > simpleMovingAverage priceList =
-- >   map average divvyedPrices
-- >     where divvyedPrices = divvy 20 1 priceList

divvy :: Int -> Int -> [a] -> [[a]]
divvy _ _ [] = []
divvy n m lst = filter (\ws -> (n == length ws)) choppedl
  where choppedl = chop (\xs -> (take n xs , drop m xs)) lst
