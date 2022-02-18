{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.Ouroboros.Network.Testnet.Simulation.Node
  ( SimArgs(..)
  , DiffusionScript (..)
  , DiffusionSimulationTrace (..)
  , prop_diffusionScript_fixupCommands
  , prop_diffusionScript_commandScript_valid
  , diffusionSimulation
  ) where

import           Control.Monad (replicateM, (>=>), forM)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.Class.MonadAsync
                     (MonadAsync (Async, cancel, waitAny, withAsync))
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM.Strict (MonadLabelledSTM,
                     MonadTraceSTM, MonadSTM (STM), StrictTVar, atomically, newTVarIO, readTVar, writeTVar)
import           Control.Monad.Class.MonadThrow (MonadCatch, MonadEvaluate,
                     MonadMask, MonadThrow, SomeException)
import           Control.Monad.Class.MonadTime (DiffTime, MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Control.Tracer (nullTracer, Tracer, traceWith)

import qualified Data.ByteString.Lazy as BL
import           Data.IP (IP (..), toIPv4, toIPv6)
import           Data.List ((\\), nub)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToDiffTime)
import           Data.Void (Void)
import           System.Random (StdGen, mkStdGen)

import           Network.DNS (Domain)

import           Ouroboros.Network.Driver.Limits (ProtocolSizeLimits (..),
                     ProtocolTimeLimits (..))
import           Ouroboros.Network.Mux (MiniProtocolLimits (..))
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.Governor
                     (PeerSelectionTargets (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface (..), UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DomainAccessPoint (..), LookupReqs (..), PortNumber,
                     RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..), byteLimitsChainSync,
                     timeLimitsChainSync)
import           Ouroboros.Network.Protocol.Handshake.Version (Accept (Accept))
import           Ouroboros.Network.Protocol.KeepAlive.Codec
                     (byteLimitsKeepAlive, timeLimitsKeepAlive)
import           Ouroboros.Network.Protocol.Limits (shortWait, smallByteLimit)
import           Ouroboros.Network.Server.RateLimiting
                     (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Snocket (TestAddress (..), Snocket)
import qualified Ouroboros.Network.Diffusion.P2P as Diff.P2P

import           Ouroboros.Network.Testing.ConcreteBlock (Block)
import           Ouroboros.Network.Testing.Data.Script (Script (..))
import           Ouroboros.Network.Testing.Utils (genDelayWithPrecision)
import           Simulation.Network.Snocket (BearerInfo (..), withSnocket, FD)

import qualified Test.Ouroboros.Network.Diffusion.Node as Node
import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel
                     (BlockGeneratorArgs, NtNAddr, randomBlockGenerationArgs,
                     NtNVersion, NtCVersion, NtCAddr, NtCVersionData,
                     NtNVersionData)
import qualified Test.Ouroboros.Network.Diffusion.Node.NodeKernel as Node
import           Test.Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DNSLookupDelay, DNSTimeout)

import           Test.QuickCheck (Arbitrary (..), Gen, Property, choose,
                     chooseInt, counterexample, frequency, oneof, property,
                     shrinkList, sized, sublistOf, vectorOf, (.&&.))


-- | Diffusion Simulator Arguments
--
-- Contains all necessary randomly generated values needed to run diffusion in
-- simulation.
--
data SimArgs =
  SimArgs
    { saSlot                  :: DiffTime
      -- ^ 'randomBlockGenerationArgs' slot duration argument
    , saSeed                  :: StdGen
      -- ^ 'randomBlockGenerationArgs' seed argument
    , saQuota                 :: Int
      -- ^ 'randomBlockGenerationArgs' quota value
    , saMbTime                :: Maybe DiffTime
      -- ^ 'LimitsAndTimeouts' argument
    , saRelays                :: [RelayAccessPoint]
      -- ^ 'Interfaces' relays auxiliary value
    , saRng                   :: StdGen
      -- ^ 'Interfaces' 'iRng' value
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
    deriving (Show)

data Command = JoinNetwork DiffTime
             | Kill DiffTime
             | Reconfigure DiffTime
                           [(Int, Map RelayAccessPoint PeerAdvertise)]
  deriving (Show, Eq)

-- | Generate DNS table
genDomainMap :: [RelayAccessPoint] -> Gen (Map Domain [IP])
genDomainMap raps = do
  let domains = [ d | RelayAccessDomain d _ <- raps ]
  m <- mapM (\d -> do
    size <- chooseInt (1, 5)
    ips <- vectorOf size genIP
    return (d, ips)) domains

  return (Map.fromList m)

  where
    genIP :: Gen IP
    genIP =
      let genIPv4 = IPv4 . toIPv4 <$> replicateM 4 (choose (0,255))
          genIPv6 = IPv6 . toIPv6 <$> replicateM 8 (choose (0,0xffff))
       in oneof [genIPv4, genIPv6]

genCommands :: [(Int, Map RelayAccessPoint PeerAdvertise)] -> Gen [Command]
genCommands localRoots = sized $ \size -> do
  commands <- vectorOf size (frequency [ (3, JoinNetwork <$> delay)
                                       , (2, Reconfigure
                                              <$> delay
                                              <*> subLocalRootPeers)
                                       , (1, Kill <$> delay)
                                       ])
  return (fixupCommands commands)
  where
    subLocalRootPeers :: Gen [(Int, Map RelayAccessPoint PeerAdvertise)]
    subLocalRootPeers = do
      subLRP <- sublistOf localRoots
      mapM (mapM (fmap Map.fromList . sublistOf . Map.toList)) subLRP

    delay = frequency [(3, genDelayWithPrecision 10), (1, (/ 10) <$> genDelayWithPrecision 2)]

fixupCommands :: [Command] -> [Command]
fixupCommands [] = []
fixupCommands (jn@(JoinNetwork _):t) = jn : go jn t
  where
    go :: Command -> [Command] -> [Command]
    go _ [] = []
    go prev (cmd:cmds) =
      case (prev, cmd) of
        (JoinNetwork _, JoinNetwork _)   -> go prev cmds
        (Kill _, Kill _)                 -> go prev cmds
        (Kill _, Reconfigure _ _)        -> go prev cmds
        (Reconfigure _ _, JoinNetwork _) -> go prev cmds
        _                                -> cmd : go cmd cmds
fixupCommands (_:t) = fixupCommands t

-- | Multinode Diffusion Simulator Script
--
-- List of 'SimArgs'. Each element of the list represents one running node.
--
newtype DiffusionScript = DiffusionScript
  { dsToRun :: [(SimArgs, [Command])]
  } deriving Show


instance Arbitrary DiffusionScript where
  arbitrary = do
    -- Limit the number of nodes to run in Simulation otherwise it is going
    -- to take very long time for tests to run
    size <- chooseInt (0, 5)
    raps <- nub <$> vectorOf size arbitrary
    dMap <- genDomainMap raps

    let toRunRaps = [ r | r@(RelayAccessAddress _ _) <- raps ]
    toRun <- mapM (addressToRun raps dMap)
                 [ (ntnToPeerAddr ip p, r)
                 | r@(RelayAccessAddress ip p) <- toRunRaps ]

    comands <- mapM (genLocalRootPeers raps >=> genCommands) toRunRaps

    return (DiffusionScript (zip toRun comands))
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
      addressToRun :: [RelayAccessPoint]
                   -> Map Domain [IP]
                   -> (NtNAddr, RelayAccessPoint)
                   -> Gen SimArgs
      addressToRun raps dMap (ntnAddr, rap) = do
        -- Slot length needs to be greater than 0 else we get a livelock in
        -- IOSim.
        --
        -- Quota values matches mainnet, so a slot length of 1s and 1 / 20
        -- chance that someone gets to make a block
        let rapsWithoutSelf = raps \\ [rap]
            bgaSlotDuration = secondsToDiffTime 1
            numberOfNodes   = length [ r | r@(RelayAccessAddress _ _) <- raps ]
            quota = 20 `div` numberOfNodes
        bgaSeed <- mkStdGen <$> arbitrary

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

        stdGen <- mkStdGen <$> arbitrary

        lrp <- genLocalRootPeers rapsWithoutSelf rap
        relays <- sublistOf rapsWithoutSelf

        peerSelectionTargets <- arbitrary
        dnsTimeout <- arbitrary
        dnsLookupDelay <- arbitrary

        return
         $ SimArgs
            { saSlot                  = bgaSlotDuration
            , saSeed                  = bgaSeed
            , saQuota                 = quota
            , saMbTime                = mustReplyTimeout
            , saRelays                = relays
            , saRng                   = stdGen
            , saDomainMap             = dMap
            , saAddr                  = ntnAddr
            , saLocalRootPeers        = lrp
            , saLocalSelectionTargets = peerSelectionTargets
            , saDNSTimeoutScript      = dnsTimeout
            , saDNSLookupDelayScript  = dnsLookupDelay
            }
  shrink (DiffusionScript []) = []
  shrink (DiffusionScript ((sargs, cmds):s)) = do
    shrinkedCmds <- fixupCommands <$> shrinkList shrinkCommand cmds
    DiffusionScript ss <- shrink (DiffusionScript s)
    return (DiffusionScript ((sargs, shrinkedCmds) : ss))
    where
      shrinkDelay = map fromRational . shrink . toRational

      shrinkCommand :: Command -> [Command]
      shrinkCommand (JoinNetwork d)     = JoinNetwork <$> shrinkDelay d
      shrinkCommand (Kill d)            = Kill        <$> shrinkDelay d
      shrinkCommand (Reconfigure d lrp) = Reconfigure <$> shrinkDelay d
                                                      <*> shrink lrp

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
        (JoinNetwork _, JoinNetwork _)   ->
          counterexample ("Invalid sequence: " ++ show x ++ " " ++ show y) $
            property False
        (Kill _, Kill _)                 ->
          counterexample ("Invalid sequence: " ++ show x ++ " " ++ show y) $
            property False
        (Kill _, Reconfigure _ _)        ->
          counterexample ("Invalid sequence: " ++ show x ++ " " ++ show y) $
            property False
        (Reconfigure _ _, JoinNetwork _) ->
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
      $ \ntcSnocket _ ->
        withAsyncAll
          (map (uncurry (runCommand Nothing ntnSnocket ntcSnocket)) args)
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
      -> SimArgs -- ^ Simulation arguments needed in order to run a single node
      -> [Command] -- ^ List of commands/actions to perform for a single node
      -> m Void
    runCommand Nothing ntnSnocket ntcSnocket simArgs [] = do
      threadDelay 3600
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrRunning
      runCommand Nothing ntnSnocket ntcSnocket simArgs []
    runCommand (Just (_, _)) ntnSnocket ntcSnocket simArgs [] = do
      -- We shouldn't block this thread waiting
      -- on the async since this will lead to a deadlock
      -- as thread returns 'Void'.
      threadDelay 3600
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrRunning
      runCommand Nothing ntnSnocket ntcSnocket simArgs []
    runCommand Nothing ntnSnocket ntcSnocket simArgs (JoinNetwork delay:cs) = do
      threadDelay delay
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrJoiningNetwork
      lrpVar <- newTVarIO $ saLocalRootPeers simArgs
      withAsync (runNode simArgs ntnSnocket ntcSnocket lrpVar) $ \nodeAsync ->
        runCommand (Just (nodeAsync, lrpVar)) ntnSnocket ntcSnocket simArgs cs
    runCommand _ _ _ _ (JoinNetwork _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async, _)) ntnSnocket ntcSnocket simArgs
               (Kill delay:cs) = do
      threadDelay delay
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrKillingNode
      cancel async
      runCommand Nothing ntnSnocket ntcSnocket simArgs cs
    runCommand _ _ _ _ (Kill _:_) = do
      error "runCommand: Impossible happened"
    runCommand Nothing _ _ _ (Reconfigure _ _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async, lrpVar)) ntnSnocket ntcSnocket simArgs
               (Reconfigure delay newLrp:cs) = do
      threadDelay delay
      traceWith (diffSimTracerWithTimName (saAddr simArgs)) TrReconfigurionNode
      _ <- atomically $ writeTVar lrpVar newLrp
      runCommand (Just (async, lrpVar)) ntnSnocket ntcSnocket simArgs cs

    runNode :: SimArgs
            -> Snocket m (FD m NtNAddr) NtNAddr
            -> Snocket m (FD m NtCAddr) NtCAddr
            -> StrictTVar m [(Int, Map RelayAccessPoint PeerAdvertise)]
            -> m Void
    runNode SimArgs
            { saSlot                  = bgaSlotDuration
            , saSeed                  = bgaSeed
            , saQuota                 = quota
            , saMbTime                = mustReplyTimeout
            , saRelays                = raps
            , saRng                   = stdGen
            , saDomainMap             = dMap
            , saAddr                  = rap
            , saLocalSelectionTargets = peerSelectionTargets
            , saDNSTimeoutScript      = dnsTimeout
            , saDNSLookupDelayScript  = dnsLookupDelay
            }
            ntnSnocket
            ntcSnocket
            lrpVar =
      let acceptedConnectionsLimit =
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
                                      bgaSeed
                                      quota

          stdChainSyncTimeout :: ChainSyncTimeout
          stdChainSyncTimeout = do
              ChainSyncTimeout
                { canAwaitTimeout  = shortWait
                , intersectTimeout = shortWait
                , mustReplyTimeout
                }

          limitsAndTimeouts :: Node.LimitsAndTimeouts Block
          limitsAndTimeouts
            = Node.LimitsAndTimeouts
                { Node.chainSyncLimits     = defaultMiniProtocolsLimit
                , Node.chainSyncSizeLimits = byteLimitsChainSync (const 0)
                , Node.chainSyncTimeLimits =
                    timeLimitsChainSync stdChainSyncTimeout
                , Node.keepAliveLimits     = defaultMiniProtocolsLimit
                , Node.keepAliveSizeLimits = byteLimitsKeepAlive (const 0)
                , Node.keepAliveTimeLimits = timeLimitsKeepAlive
                , Node.pingPongLimits      = defaultMiniProtocolsLimit
                , Node.pingPongSizeLimits  =
                    ProtocolSizeLimits (const smallByteLimit) (const 0)
                , Node.pingPongTimeLimits  =
                    ProtocolTimeLimits (const (Just 60))
                , Node.handshakeLimits     = defaultMiniProtocolsLimit
                , Node.handshakeTimeLimits =
                    ProtocolSizeLimits (const (4 * 1440))
                                       (fromIntegral . BL.length)
                , Node.handhsakeSizeLimits =
                    ProtocolTimeLimits (const shortWait)
                }

          interfaces :: Node.Interfaces m
          interfaces =
            Node.Interfaces
              { Node.iNtnSnocket        = ntnSnocket
              , Node.iAcceptVersion     = acceptVersion
              , Node.iNtnDomainResolver = (return .)
                                        <$> domainResolver raps dMap
              , Node.iNtcSnocket        = ntcSnocket
              , Node.iRng               = stdGen
              , Node.iDomainMap         = dMap
              , Node.iLedgerPeersConsensusInterface
                                        = LedgerPeersConsensusInterface
                                        $ \_ -> return Nothing
              }

          arguments :: Node.Arguments m
          arguments =
            Node.Arguments
              { Node.aIPAddress            = rap
              , Node.aAcceptedLimits       = acceptedConnectionsLimit
              , Node.aDiffusionMode        = diffusionMode
              , Node.aKeepAliveInterval    = 0
              , Node.aPingPongInterval     = 0
              , Node.aPeerSelectionTargets = peerSelectionTargets
              , Node.aReadLocalRootPeers   = readLocalRootPeers
              , Node.aReadPublicRootPeers  = readPublicRootPeers
              , Node.aReadUseLedgerAfter   = readUseLedgerAfter
              , Node.aProtocolIdleTimeout  = 5
              , Node.aTimeWaitTimeout      = 30
              , Node.aDNSTimeoutScript     = dnsTimeout
              , Node.aDNSLookupDelayScript = dnsLookupDelay
              }

       in Node.run blockGeneratorArgs
                   limitsAndTimeouts
                   interfaces
                   arguments
                   (tracersExtraWithTimeName rap)

    domainResolver :: [RelayAccessPoint]
                   -> Map Domain [IP]
                   -> LookupReqs
                   -> [DomainAccessPoint]
                   -> Map DomainAccessPoint (Set NtNAddr)
    domainResolver raps dMap _ daps = do
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
      Map.fromList mapDomains


ntnToPeerAddr :: IP -> PortNumber -> NtNAddr
ntnToPeerAddr a b = TestAddress (Node.IPAddr a b)

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
