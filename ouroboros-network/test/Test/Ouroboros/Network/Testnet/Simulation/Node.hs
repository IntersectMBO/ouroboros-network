{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module Test.Ouroboros.Network.Testnet.Simulation.Node
  ( SimArgs (..)
  , DiffusionScript (..)
  , HotDiffusionScript (..)
  , DiffusionSimulationTrace (..)
  , prop_diffusionScript_fixupCommands
  , prop_diffusionScript_commandScript_valid
  , diffusionSimulation
  , Command (..)
    -- * Re-exports
  , TestAddress (..)
  , RelayAccessPoint (..)
  , Script (..)
  , module PeerSelection
  ) where

import           Control.Monad (forM, replicateM, (>=>))
import           Control.Monad.Class.MonadAsync
                     (MonadAsync (Async, cancel, waitAny, withAsync))
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM.Strict (MonadLabelledSTM,
                     MonadSTM (STM), MonadTraceSTM, StrictTVar, atomically,
                     newTVarIO, readTVar, writeTVar)
import           Control.Monad.Class.MonadThrow (MonadCatch, MonadEvaluate,
                     MonadMask, MonadThrow, SomeException)
import           Control.Monad.Class.MonadTime (DiffTime, MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Control.Monad.Fix (MonadFix)
import           Control.Tracer (Tracer, nullTracer, traceWith)

import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (traverse_)
import           Data.IP (IP (..), toIPv4, toIPv6)
import           Data.List (delete, intersperse, nub, (\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToDiffTime)
import           Data.Void (Void)
import           System.Random (StdGen, mkStdGen)
import qualified System.Random as Random

import           Network.DNS (Domain, TTL)

import qualified Ouroboros.Network.Diffusion.P2P as Diff.P2P
import           Ouroboros.Network.Driver.Limits (ProtocolSizeLimits (..),
                     ProtocolTimeLimits (..))
import           Ouroboros.Network.Mux (MiniProtocolLimits (..))
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.Governor
                     (PeerSelectionTargets (..))
import qualified Ouroboros.Network.PeerSelection.Governor as PeerSelection
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface (..), UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DomainAccessPoint (..), LookupReqs (..), PortNumber,
                     RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
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
import           Ouroboros.Network.Snocket (Snocket, TestAddress (..))

import           Ouroboros.Network.Testing.ConcreteBlock (Block)
import           Ouroboros.Network.Testing.Data.Script (Script (..))
import           Ouroboros.Network.Testing.Utils (genDelayWithPrecision)
import           Simulation.Network.Snocket (BearerInfo (..), FD, withSnocket)

import qualified Test.Ouroboros.Network.Diffusion.Node as NodeKernel
import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel
                     (BlockGeneratorArgs, NtCAddr, NtCVersion, NtCVersionData,
                     NtNAddr, NtNAddr_ (IPAddr), NtNVersion, NtNVersionData,
                     randomBlockGenerationArgs)
import           Test.Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DNSLookupDelay (..), DNSTimeout (..))
import qualified Test.Ouroboros.Network.PeerSelection.RootPeersDNS as PeerSelection hiding
                     (tests)

import           Test.QuickCheck (Arbitrary (..), Gen, Property, choose,
                     chooseInt, counterexample, frequency, oneof, property,
                     shrinkList, sized, sublistOf, suchThat, vectorOf, (.&&.))


-- | Diffusion Simulator Arguments
--
-- Contains all necessary randomly generated values needed to run diffusion in
-- simulation.
--
data SimArgs =
  SimArgs
    { saSlot                  :: DiffTime
      -- ^ 'randomBlockGenerationArgs' slot duration argument
    , saSeed                  :: Int
      -- ^ 'randomBlockGenerationArgs' seed argument
    , saQuota                 :: Int
      -- ^ 'randomBlockGenerationArgs' quota value
    , saMbTime                :: Maybe DiffTime
      -- ^ 'LimitsAndTimeouts' argument
    , saRelays                :: [RelayAccessPoint]
      -- ^ 'Interfaces' relays auxiliary value
    , saDomainMap             :: Map Domain [IP]
      -- ^ 'Interfaces' 'iDomainMap' value
    , saAddr                  :: NtNAddr
      -- ^ 'Arguments' 'aIPAddress' value
    , saLocalRootPeers        :: [(Int, Map RelayAccessPoint PeerAdvertise)]
      -- ^ 'Arguments' 'LocalRootPeers' values
    , saLocalSelectionTargets :: PeerSelectionTargets
      -- ^ 'Arguments' 'aLocalSelectionTargets' value
    , saDNSTimeoutScript      :: Script DNSTimeout
      -- ^ 'Arguments' 'aDNSTimeoutScript' value
    , saDNSLookupDelayScript  :: Script DNSLookupDelay
      -- ^ 'Arguments' 'aDNSLookupDelayScript' value
    }

instance Show SimArgs where
    show SimArgs { saSlot, saSeed, saQuota, saMbTime, saRelays, saDomainMap,
                   saAddr, saLocalRootPeers, saLocalSelectionTargets,
                   saDNSTimeoutScript, saDNSLookupDelayScript } =
      concat $ intersperse " " [ "SimArgs"
                               , show saSlot
                               , "(" ++ show saSeed ++ ")"
                               , show saQuota
                               , "(" ++ show saMbTime ++ ")"
                               , show saRelays
                               , "(Map.fromList [" ++ Map.foldMapWithKey (\domain ips -> "(" ++ show domain ++ ", " ++ showIPs ips ++ ")") saDomainMap ++ "])"
                               , "(" ++ show saAddr ++ ")"
                               , show saLocalRootPeers
                               , show saLocalSelectionTargets
                               , "(" ++ show saDNSTimeoutScript ++ ")"
                               , "(" ++ show saDNSLookupDelayScript ++ ")"
                               ]
      where
        showIPs :: [IP] -> String
        showIPs ips = "["
                   ++ concat (intersperse ", " (map (\ip -> "read \"" ++ show ip ++ "\"") ips))
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

-- | Multinode Diffusion Simulator Script
--
-- List of 'SimArgs'. Each element of the list represents one running node.
--
newtype DiffusionScript = DiffusionScript {
    dsToRun :: [(SimArgs, [Command])]
  } deriving Show

-- | Multinode Diffusion Simulator Script
--
-- Tries to generate a reasonable looking network with at most 3 nodes that can
-- or can not be connected to one another. These nodes can also randomly die or
-- have their local configuration changed.
--
genNonHotDiffusionScript :: Gen [(SimArgs, [Command])]
genNonHotDiffusionScript = do
  -- Limit the number of nodes to run in Simulation otherwise it is going
  -- to take very long time for tests to run
  size <- chooseInt (0, 3)
  raps <- nub <$> vectorOf size arbitrary

  let toRunRaps = [ r | r@(RelayAccessAddress _ _) <- raps ]
  toRun <- mapM (genSimArgs raps)
               [ (ntnToPeerAddr ip p, r)
               | r@(RelayAccessAddress ip p) <- toRunRaps ]

  comands <- mapM (genLocalRootPeers raps >=> genCommands) toRunRaps

  return (zip toRun comands)
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

    -- | Given a NtNAddr generate the necessary things to run in Simulation
    genSimArgs :: [RelayAccessPoint]
                 -> (NtNAddr, RelayAccessPoint)
                 -> Gen SimArgs
    genSimArgs raps (ntnAddr, rap) = do
      -- Slot length needs to be greater than 0 else we get a livelock on
      -- the IOSim.
      --
      -- Quota values matches mainnet, so a slot length of 1s and 1 / 20
      -- chance that someone gets to make a block
      let rapsWithoutSelf = delete rap raps
          bgaSlotDuration = secondsToDiffTime 1
          numberOfNodes   = length [ r | r@(RelayAccessAddress _ _) <- raps ]
          quota = 20 `div` numberOfNodes
          (RelayAccessAddress rapIP _) = rap
      seed <- arbitrary

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

      peerSelectionTargets <- arbitrary
      dnsTimeout <- arbitrary
      dnsLookupDelay <- arbitrary

      return SimArgs
          { saSlot                  = bgaSlotDuration
          , saSeed                  = seed
          , saQuota                 = quota
          , saMbTime                = mustReplyTimeout
          , saRelays                = relays
          , saDomainMap             = dMap
          , saAddr                  = ntnAddr
          , saLocalRootPeers        = lrp
          , saLocalSelectionTargets = peerSelectionTargets
          , saDNSTimeoutScript      = dnsTimeout
          , saDNSLookupDelayScript  = dnsLookupDelay
          }

-- | Multinode Hot Diffusion Simulator Script
--
-- Tries to generate a network with at most 2 nodes that should
-- be connected to one another. This generator tries to obtain high ratios of
-- active peers so we can test the miniprotocols that run when we have such
-- active connections. These nodes can not randomly die or have their local
-- configuration changed. Their local root peers consist of a single group.
--
-- TODO: Refactor and abstract common parts with the
-- 'genNonHotDiffusionScript' generator
--
genHotDiffusionScript :: Gen [(SimArgs, [Command])]
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

    toRun <- mapM (genSimArgs allRaps minConnected)
                 [ (ntnToPeerAddr ip p, r)
                 | r@(RelayAccessAddress ip p) <- toRunRaps ]


    return (zip toRun comands)
    where
      isRelayAccessAddress :: RelayAccessPoint -> Bool
      isRelayAccessAddress RelayAccessAddress{} = True
      isRelayAccessAddress _                    = False

      hasActive :: Int -> PeerSelectionTargets -> Bool
      hasActive minConnected (PeerSelectionTargets _ _ _ y) = y > minConnected

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

      -- | Given a NtNAddr generate the necessary things to run in Simulation
      genSimArgs :: [RelayAccessPoint]
                 -> Int
                 -> (NtNAddr, RelayAccessPoint)
                 -> Gen SimArgs
      genSimArgs raps minConnected (ntnAddr, rap) = do
        -- Slot length needs to be greater than 0 else we get a livelock on
        -- the IOSim.
        --
        -- Quota values matches mainnet, so a slot length of 1s and 1 / 20
        -- chance that someone gets to make a block
        let rapsWithoutSelf = delete rap raps
            bgaSlotDuration = secondsToDiffTime 1
            numberOfNodes   = length [ r | r@(RelayAccessAddress _ _) <- raps ]
            quota = 20 `div` numberOfNodes
            (RelayAccessAddress rapIP _) = rap
        seed <- arbitrary

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

        -- Make sure our targets for active peers cover the maximum of peers
        -- one generated
        peerSelectionTargets <- arbitrary `suchThat` hasActive minConnected

        dnsTimeout <- arbitrary
        dnsLookupDelay <- arbitrary

        return
         $ SimArgs
            { saSlot                  = bgaSlotDuration
            , saSeed                  = seed
            , saQuota                 = quota
            , saMbTime                = mustReplyTimeout
            , saRelays                = relays
            , saDomainMap             = dMap
            , saAddr                  = ntnAddr
            , saLocalRootPeers        = lrp
            , saLocalSelectionTargets = peerSelectionTargets
            , saDNSTimeoutScript      = dnsTimeout
            , saDNSLookupDelayScript  = dnsLookupDelay
            }

instance Arbitrary DiffusionScript where
  arbitrary = DiffusionScript
            <$> frequency [ (1, genNonHotDiffusionScript)
                          , (1, genHotDiffusionScript)
                          ]
  shrink (DiffusionScript []) = []
  shrink (DiffusionScript ((sargs, cmds):s)) = do
    shrinkedCmds <- fixupCommands <$> shrinkList shrinkCommand cmds
    DiffusionScript ss <- shrink (DiffusionScript s)
    return (DiffusionScript ((sargs, shrinkedCmds) : ss))
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
newtype HotDiffusionScript = HotDiffusionScript {
    hdsToRun :: [(SimArgs, [Command])]
  } deriving Show

instance Arbitrary HotDiffusionScript where
  arbitrary = HotDiffusionScript <$> genHotDiffusionScript
  shrink (HotDiffusionScript hds) =
    [ HotDiffusionScript ds
    | DiffusionScript ds <- shrink (DiffusionScript hds) ]

-- Tests if the fixupCommand is idempotent.
-- Note that the generator for DiffusionScript already fixups the Command list.
--
prop_diffusionScript_fixupCommands :: DiffusionScript -> Property
prop_diffusionScript_fixupCommands (DiffusionScript []) = property True
prop_diffusionScript_fixupCommands (DiffusionScript ((_, cmds): t)) =
  counterexample ("Failed with cmds: " ++ show cmds ++ "\n"
                  ++ "fixupCommands cmds = " ++ show (fixupCommands cmds)
                 ) $
  fixupCommands cmds == cmds
  .&&. prop_diffusionScript_fixupCommands (DiffusionScript t)

-- Tests if the fixupCommand outputs valid command scripts.
--
-- Note that the generator for DiffusionScript already fixups the Command list.
--
prop_diffusionScript_commandScript_valid :: DiffusionScript -> Property
prop_diffusionScript_commandScript_valid (DiffusionScript []) = property True
prop_diffusionScript_commandScript_valid (DiffusionScript ((_, cmds): t)) =
  counterexample ("Failed with cmds: " ++ show cmds) $
  isValid cmds
  .&&. prop_diffusionScript_commandScript_valid (DiffusionScript t)
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
  | TrReconfigurionNode
  | TrUpdatingDNS
  | TrRunning
  deriving (Show)

-- | Run an arbitrary topology
diffusionSimulation
  :: forall m. ( MonadAsync       m
               , MonadFix         m
               , MonadFork        m
               , MonadST          m
               , MonadEvaluate    m
               , MonadLabelledSTM m
               , MonadTraceSTM    m
               , MonadCatch       m
               , MonadMask        m
               , MonadTime        m
               , MonadTimer       m
               , MonadThrow  (STM m)
               , Eq (Async m Void)
               , forall a. Semigroup a => Semigroup (m a)
               )
  => BearerInfo
  -> DiffusionScript
  -> ( NtNAddr
     -> Diff.P2P.TracersExtra NtNAddr NtNVersion NtNVersionData
                              NtCAddr NtCVersion NtCVersionData
                              SomeException m )
  -> ( NtNAddr
     -> Tracer m DiffusionSimulationTrace )
  -> m Void
diffusionSimulation
  defaultBearerInfo
  (DiffusionScript args)
  tracersExtraWithTimeName
  diffSimTracerWithTimName =
    withSnocket nullTracer defaultBearerInfo Map.empty
      $ \ntnSnocket _ ->
        withSnocket nullTracer defaultBearerInfo Map.empty
      $ \ntcSnocket _ -> do
        let dnsMaps = map (\(sa, _)
                            -> (saAddr sa, fmap (, 0) <$> saDomainMap sa))
                          args
        dnsMapVarMap <- Map.fromList <$> mapM (mapM (newTVarIO @m)) dnsMaps
        withAsyncAll
          (map (uncurry (runCommand Nothing ntnSnocket ntcSnocket dnsMapVarMap))
                        args)
          $ \nodes -> do
            (_, x) <- waitAny nodes
            return x
  where
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
      -> SimArgs -- ^ Simulation arguments needed in order to run a single node
      -> [Command] -- ^ List of commands/actions to perform for a single node
      -> m Void
    runCommand Nothing ntnSnocket ntcSnocket dMapVarMap simArgs [] = do
      threadDelay 3600
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrRunning
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap simArgs []
    runCommand (Just (_, _)) ntnSnocket ntcSnocket dMapVarMap simArgs [] = do
      -- We shouldn't block this thread waiting
      -- on the async since this will lead to a deadlock
      -- as thread returns 'Void'.
      threadDelay 3600
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrRunning
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap simArgs []
    runCommand Nothing ntnSnocket ntcSnocket dMapVarMap simArgs
               (JoinNetwork delay Nothing:cs) = do
      threadDelay delay
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrJoiningNetwork
      lrpVar <- newTVarIO $ saLocalRootPeers simArgs
      let dnsMapVar = dMapVarMap Map.! saAddr simArgs
      withAsync (runNode simArgs ntnSnocket ntcSnocket lrpVar dnsMapVar) $ \nodeAsync ->
        runCommand (Just (nodeAsync, lrpVar)) ntnSnocket ntcSnocket dMapVarMap simArgs cs
    runCommand Nothing ntnSnocket ntcSnocket dMapVarMap simArgs
               (JoinNetwork delay (Just ip):cs) = do
      threadDelay delay
      let simArgs' = simArgs { saAddr = ip }
      traceWith (diffSimTracerWithTimName ip) TrJoiningNetwork
      lrpVar <- newTVarIO $ saLocalRootPeers simArgs'

      -- Updating DomainMap entry now that the node is having a new IP
      let dnsMapVar = dMapVarMap Map.! saAddr simArgs
      let dMapVarMap' = Map.delete (saAddr simArgs) dMapVarMap
          dMapVarMap'' = Map.insert ip dnsMapVar dMapVarMap'

      withAsync (runNode simArgs' ntnSnocket ntcSnocket lrpVar dnsMapVar)
        $ \nodeAsync ->
          withAsync (updateDomainMap delay (saAddr simArgs) ip dMapVarMap'')
            $ \_ ->
              runCommand (Just (nodeAsync, lrpVar)) ntnSnocket ntcSnocket
                         dMapVarMap'' simArgs' cs
    runCommand _ _ _ _ _ (JoinNetwork _ _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async, _)) ntnSnocket ntcSnocket dMapVarMap simArgs
               (Kill delay:cs) = do
      threadDelay delay
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrKillingNode
      cancel async
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap simArgs cs
    runCommand _ _ _ _ _ (Kill _:_) = do
      error "runCommand: Impossible happened"
    runCommand Nothing _ _ _ _ (Reconfigure _ _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async, lrpVar)) ntnSnocket ntcSnocket dMapVarMap simArgs
               (Reconfigure delay newLrp:cs) = do
      threadDelay delay
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrReconfigurionNode
      _ <- atomically $ writeTVar lrpVar newLrp
      runCommand (Just (async, lrpVar)) ntnSnocket ntcSnocket dMapVarMap simArgs
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
      traceWith (diffSimTracerWithTimName oip) TrUpdatingDNS
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
            -> Snocket m (FD m NtNAddr) NtNAddr
            -> Snocket m (FD m NtCAddr) NtCAddr
            -> StrictTVar m [(Int, Map RelayAccessPoint PeerAdvertise)]
            -> StrictTVar m (Map Domain [(IP, TTL)])
            -> m Void
    runNode SimArgs
            { saSlot                  = bgaSlotDuration
            , saSeed                  = seed
            , saQuota                 = quota
            , saMbTime                = mustReplyTimeout
            , saRelays                = raps
            , saAddr                  = rap
            , saLocalSelectionTargets = peerSelectionTargets
            , saDNSTimeoutScript      = dnsTimeout
            , saDNSLookupDelayScript  = dnsLookupDelay
            }
            ntnSnocket
            ntcSnocket
            lrpVar
            dMapVar =
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
                , NodeKernel.pingPongSizeLimits   =
                    ProtocolSizeLimits (const smallByteLimit) (const 0)
                , NodeKernel.pingPongTimeLimits   =
                    ProtocolTimeLimits (const (Just 60))
                , NodeKernel.handshakeLimits      = defaultMiniProtocolsLimit
                , NodeKernel.handshakeTimeLimits  =
                    ProtocolSizeLimits (const (4 * 1440))
                                       (fromIntegral . BL.length)
                , NodeKernel.handhsakeSizeLimits  =
                    ProtocolTimeLimits (const shortWait)
                }

          interfaces :: NodeKernel.Interfaces m
          interfaces =
            NodeKernel.Interfaces
              { NodeKernel.iNtnSnocket        = ntnSnocket
              , NodeKernel.iAcceptVersion     = acceptVersion
              , NodeKernel.iNtnDomainResolver = domainResolver raps dMapVar
              , NodeKernel.iNtcSnocket        = ntcSnocket
              , NodeKernel.iRng               = rng
              , NodeKernel.iDomainMap         = dMapVar
              , NodeKernel.iLedgerPeersConsensusInterface
                                        = LedgerPeersConsensusInterface
                                        $ \_ -> return Nothing
              }

          arguments :: NodeKernel.Arguments m
          arguments =
            NodeKernel.Arguments
              { NodeKernel.aIPAddress            = rap
              , NodeKernel.aAcceptedLimits       = acceptedConnectionsLimit
              , NodeKernel.aDiffusionMode        = diffusionMode
              , NodeKernel.aKeepAliveInterval    = 0
              , NodeKernel.aPingPongInterval     = 0
              , NodeKernel.aPeerSelectionTargets = peerSelectionTargets
              , NodeKernel.aReadLocalRootPeers   = readLocalRootPeers
              , NodeKernel.aReadPublicRootPeers  = readPublicRootPeers
              , NodeKernel.aReadUseLedgerAfter   = readUseLedgerAfter
              , NodeKernel.aProtocolIdleTimeout  = 5
              , NodeKernel.aTimeWaitTimeout      = 30
              , NodeKernel.aDNSTimeoutScript     = dnsTimeout
              , NodeKernel.aDNSLookupDelayScript = dnsLookupDelay
              }

       in NodeKernel.run blockGeneratorArgs
                         limitsAndTimeouts
                         interfaces
                         arguments
                         (tracersExtraWithTimeName rap)

    domainResolver :: [RelayAccessPoint]
                   -> StrictTVar m (Map Domain [(IP, TTL)])
                   -> LookupReqs
                   -> [DomainAccessPoint]
                   -> m (Map DomainAccessPoint (Set NtNAddr))
    domainResolver raps dMapVar _ daps = do
      dMap <- fmap (map fst) <$> atomically (readTVar dMapVar)
      let domains    = [ (d, p) | RelayAccessDomain d p <- raps ]
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
