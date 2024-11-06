{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ouroboros.Network.Diffusion.Testnet.Cardano.Simulation
  ( SimArgs (..)
  , mainnetSimArgs
  , NodeArgs (..)
  , ServiceDomainName (..)
  , DiffusionScript (..)
  , HotDiffusionScript (..)
  , DiffusionSimulationTrace (..)
  , prop_diffusionScript_fixupCommands
  , prop_diffusionScript_commandScript_valid
  , fixupCommands
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

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Fix
import Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)

import Control.Monad.IOSim (IOSim, traceM)

import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.IP (IP (..))
import Data.List (delete, nubBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (secondsToDiffTime)
import Data.Void (Void)
import System.Random (StdGen, mkStdGen)
import System.Random qualified as Random

import Network.DNS (Domain, TTL)

import Network.TypedProtocol.Core
import Network.TypedProtocol.PingPong.Type qualified as PingPong

import Cardano.Network.ConsensusMode
import Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import Ouroboros.Network.ConnectionManager.Types (AbstractTransitionTrace)
import Ouroboros.Network.Driver.Limits (ProtocolSizeLimits (..),
           ProtocolTimeLimits (..))
import Ouroboros.Network.InboundGovernor (RemoteTransitionTrace)
import Ouroboros.Network.Mux (MiniProtocolLimits (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Governor (DebugPeerSelection (..),
           PeerSelectionTargets (..), TracePeerSelection)
import Ouroboros.Network.PeerSelection.Governor qualified as PeerSelection
import Ouroboros.Network.PeerSelection.LedgerPeers (AfterSlot (..),
           LedgerPeersConsensusInterface (..), TraceLedgerPeers,
           UseLedgerPeers (..), accPoolStake)
import Ouroboros.Network.PeerSelection.PeerStateActions
           (PeerSelectionActionsTrace)
import Ouroboros.Network.Protocol.BlockFetch.Codec (byteLimitsBlockFetch,
           timeLimitsBlockFetch)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout (..),
           byteLimitsChainSync, timeLimitsChainSync)
import Ouroboros.Network.Protocol.Handshake.Version (Accept (Accept))
import Ouroboros.Network.Protocol.KeepAlive.Codec (byteLimitsKeepAlive,
           timeLimitsKeepAlive)
import Ouroboros.Network.Protocol.Limits (shortWait, smallByteLimit)
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (Snocket, TestAddress (..))

import Ouroboros.Network.Block (BlockNo)
import Ouroboros.Network.Mock.ConcreteBlock (Block (..), BlockHeader (..))
import Ouroboros.Network.Testing.Data.Script
import Ouroboros.Network.Testing.Utils
import Simulation.Network.Snocket (BearerInfo (..), FD, SnocketTrace,
           WithAddr (..), makeFDBearer, withSnocket)

import Test.Ouroboros.Network.Diffusion.Node.Kernel (BlockGeneratorArgs,
           NtCAddr, NtCVersion, NtCVersionData, NtNAddr, NtNAddr_ (IPAddr),
           NtNVersion, NtNVersionData, ntnAddrToRelayAccessPoint,
           randomBlockGenerationArgs)
import Test.Ouroboros.Network.PeerSelection.Cardano.Instances ()
import Test.Ouroboros.Network.PeerSelection.Instances qualified as PeerSelection
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS (DNSLookupDelay (..),
           DNSTimeout (..), mockDNSActions)
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS qualified as PeerSelection hiding
           (tests)

import Cardano.Diffusion.Configuration (defaultMinBigLedgerPeersForTrustedState)
import Cardano.Network.ArgumentsExtra (CardanoArgumentsExtra (..))
import Cardano.Network.LedgerPeerConsensusInterface
           (CardanoLedgerPeersConsensusInterface (..))
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.PeerSelection.Governor.PeerSelectionState
import Cardano.Network.PeerSelection.Governor.PeerSelectionActions
           (cardanoExtraArgsToPeerSelectionActions)
           (CardanoPeerSelectionState)
import Cardano.Network.PeerSelection.Governor.Types (CardanoPeerSelectionView)
import Cardano.Network.PeerSelection.LocalRootPeers
           (OutboundConnectionsState (..))
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as CPST
import Cardano.Network.PeerSelection.Governor.Types (CardanoPeerSelectionView,
           cardanoPeerSelectionGovernorArgs)
import Cardano.Network.PeerSelection.PeerChurnArgs (CardanoPeerChurnArgs (..))
import Cardano.Network.PeerSelection.Governor.Types qualified as CPSV
import Cardano.Network.PeerSelection.Types (ChurnMode (..))
import Cardano.Network.PublicRootPeers (CardanoPublicRootPeers)
import Cardano.Network.PublicRootPeers qualified as CPRP
import Cardano.Network.Types (LedgerStateJudgement (..),
           MinBigLedgerPeersForTrustedState (..))
import Cardano.PeerSelection.Churn (peerChurnGovernor)
import Cardano.PeerSelection.PeerSelectionActions (requestPublicRootPeers)
import Data.Bool (bool)
import Data.Function (on)
import Data.Typeable (Typeable)
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.BlockFetch (FetchMode (..), TraceFetchClientState,
import Ouroboros.Network.InboundGovernor qualified as IG
           TraceLabelPeer (..))
import Ouroboros.Network.Diffusion.Common qualified as Common
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (DomainAccessPoint (..),
           PortNumber, RelayAccessPoint (..))
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSLookupType)
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers)
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
           (TracePublicRootPeers)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           WarmValency (..))
import Ouroboros.Network.Protocol.PeerSharing.Codec (byteLimitsPeerSharing,
import Ouroboros.Network.PeerSelection.Governor.Types
           (BootstrapPeersCriticalTimeoutError)
import Ouroboros.Network.PeerSelection.RootPeersDNS (PeerActionsDNS (..))
           timeLimitsPeerSharing)
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
           (newLedgerAndPublicRootDNSSemaphore)
import Ouroboros.Network.Server2 qualified as Server
import Test.Ouroboros.Network.LedgerPeers (LedgerPools (..), genLedgerPoolsFrom)
import Test.Ouroboros.Network.PeerSelection.LocalRootPeers ()
import Test.QuickCheck

-- | Diffusion Simulator Arguments
--
import Test.Ouroboros.Network.Diffusion.Testnet.Minimal.Node (Arguments (..),
           Interfaces (..), LimitsAndTimeouts (..), run)
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

data ServiceDomainName =
      DomainName Domain
      -- ^ a well configured domain name
    | Misconfigured Domain
      -- ^ a domain name which is advertised but its' IPs are wrong.
    | NoDomainName
  deriving Show

instance Arbitrary ServiceDomainName where
    arbitrary = frequency [ (8, pure $ DomainName (BSC.pack "iog.io"))
                          , (1, pure $ Misconfigured (BSC.pack "error.iog.io"))
                          , (1, pure $ NoDomainName)
                          ]
    shrink (DomainName _)    = []
    shrink (Misconfigured a) = [DomainName a]
    shrink  NoDomainName     = []


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
    , naPublicRoots            :: Map RelayAccessPoint PeerAdvertise
      -- ^ 'Interfaces' relays auxiliary value
    , naConsensusMode          :: ConsensusMode
    , naBootstrapPeers         :: Script UseBootstrapPeers
      -- ^ 'Interfaces' relays auxiliary value
    , naAddr                   :: NtNAddr
      -- ^ 'Arguments' 'aIPAddress' value
    , naPeerSharing            :: PeerSharing
      -- ^ 'Arguments' 'aOwnPeerSharing' value
    , naLocalRootPeers         :: [( HotValency
                                   , WarmValency
                                   , Map RelayAccessPoint ( PeerAdvertise
                                                          , PeerTrustable)
                                   )]
    , naLedgerPeers            :: Script LedgerPools
      -- ^ 'Arguments' 'LocalRootPeers' values
    , naPeerTargets            :: (PeerSelectionTargets, PeerSelectionTargets)
      -- ^ 'Arguments' 'aLocalSelectionTargets' value
    , naDNSTimeoutScript       :: Script DNSTimeout
      -- ^ 'Arguments' 'aDNSTimeoutScript' value
    , naDNSLookupDelayScript   :: Script DNSLookupDelay
      -- ^ 'Arguments' 'aDNSLookupDelayScript' value
    , naChainSyncExitOnBlockNo :: Maybe BlockNo
    , naChainSyncEarlyExit     :: Bool
    , naFetchModeScript        :: Script FetchMode
    }

instance Show NodeArgs where
    show NodeArgs { naSeed, naDiffusionMode, naMbTime, naBootstrapPeers, naPublicRoots,
                   naAddr, naPeerSharing, naLocalRootPeers, naPeerTargets,
                   naDNSTimeoutScript, naDNSLookupDelayScript, naChainSyncExitOnBlockNo,
                   naChainSyncEarlyExit, naFetchModeScript, naConsensusMode } =
      unwords [ "NodeArgs"
              , "(" ++ show naSeed ++ ")"
              , show naDiffusionMode
              , show naConsensusMode
              , "(" ++ show naMbTime ++ ")"
              , "(" ++ show naPublicRoots ++ ")"
              , "(" ++ show naBootstrapPeers ++ ")"
              , "(" ++ show naAddr ++ ")"
              , show naPeerSharing
              , show naLocalRootPeers
              , show naPeerTargets
              , "(" ++ show naDNSTimeoutScript ++ ")"
              , "(" ++ show naDNSLookupDelayScript ++ ")"
              , "(" ++ show naChainSyncExitOnBlockNo ++ ")"
              , show naChainSyncEarlyExit
              , show naFetchModeScript
              , "============================================\n"
              ]

data Command = JoinNetwork DiffTime
             | Kill DiffTime
             | Reconfigure DiffTime
                           [( HotValency
                            , WarmValency
                            , Map RelayAccessPoint ( PeerAdvertise
                                                   , PeerTrustable)
                            )]
  deriving Eq

instance Show Command where
    showsPrec d (JoinNetwork delay)             = showString "JoinNetwork "
                                                . showsPrec d delay
    showsPrec d (Kill delay)                    = showString "Kill "
                                                . showsPrec d delay
    showsPrec d (Reconfigure delay localRoots)  = showString "Reconfigure "
                                                . showsPrec d delay
                                                . showString " "
                                                . showsPrec d localRoots

genCommands :: [( HotValency
                , WarmValency
                , Map RelayAccessPoint (PeerAdvertise, PeerTrustable)
                )]
            -> Gen [Command]
genCommands localRoots = sized $ \size -> do
  commands <- vectorOf size (frequency [ (10, JoinNetwork <$> delay)
                                       , (6, Reconfigure
                                              <$> delay
                                              <*> subLocalRootPeers)
                                       , (3, Kill <$> delay)
                                       ])
  return (fixupCommands commands)
  where
    subLocalRootPeers :: Gen [( HotValency
                              , WarmValency
                              , Map RelayAccessPoint (PeerAdvertise, PeerTrustable)
                              )]
    subLocalRootPeers = do
      subLRP <- sublistOf localRoots
      mapM (\(h, w, g) -> (h, w,) <$> (fmap Map.fromList . sublistOf . Map.toList $ g)) subLRP

    delay = frequency [ (3, genDelayWithPrecision 65)
                      , (1, (/ 10) <$> genDelayWithPrecision 60)
                      ]

fixupCommands :: [Command] -> [Command]
fixupCommands [] = []
fixupCommands (jn@(JoinNetwork _):t) = jn : go jn t
  where
    go :: Command -> [Command] -> [Command]
    go _ [] = []
    go prev (cmd:cmds) =
      case (prev, cmd) of
        (JoinNetwork _   , JoinNetwork _   ) -> go prev cmds
        (Kill _          , Kill _          ) -> go prev cmds
        (Kill _          , Reconfigure _ _ ) -> go prev cmds
        (Reconfigure _ _ , JoinNetwork _   ) -> go prev cmds
        _                                    -> cmd : go cmd cmds
fixupCommands (_:t) = fixupCommands t

-- | Simulation arguments.
--
-- Slot length needs to be greater than 0 else we get a livelock on the IOSim.
--
-- Quota values matches mainnet, so a slot length of 1s and 1 / 20 chance that
-- someone gets to make a block.
--
mainnetSimArgs :: Int -> SimArgs
mainnetSimArgs numberOfNodes =
  SimArgs {
      saSlot  = secondsToDiffTime 1,
      saQuota = if numberOfNodes > 0
                then 20 `div` numberOfNodes
                else 100
    }


newtype SmallPeerSelectionTargets = SmallTargets PeerSelectionTargets

instance Arbitrary SmallPeerSelectionTargets where
  arbitrary = sized $ \size -> do
    targetNumberOfKnownPeers       <- getNonNegative <$> resize size arbitrary
    targetNumberOfRootPeers        <- choose (0, targetNumberOfKnownPeers)
    targetNumberOfEstablishedPeers <- choose (0, targetNumberOfKnownPeers)
    targetNumberOfActivePeers      <- choose (0, targetNumberOfEstablishedPeers)

    targetNumberOfKnownBigLedgerPeers
      <- getNonNegative <$> resize size arbitrary
    targetNumberOfEstablishedBigLedgerPeers
      <- choose (0 , targetNumberOfKnownBigLedgerPeers)
    targetNumberOfActiveBigLedgerPeers
      <- choose (0, targetNumberOfEstablishedBigLedgerPeers)

    return $ SmallTargets $ PeerSelectionTargets {
      targetNumberOfRootPeers,
      targetNumberOfKnownPeers,
      targetNumberOfEstablishedPeers,
      targetNumberOfActivePeers,
      targetNumberOfKnownBigLedgerPeers,
      targetNumberOfEstablishedBigLedgerPeers,
      targetNumberOfActiveBigLedgerPeers
    }

  shrink (SmallTargets (PeerSelectionTargets r k e a kb eb ab)) =
    [ SmallTargets targets'
    | (r',k',e',a',kb',eb',ab') <- shrink (r,k,e,a,kb,eb,ab)
    , let targets' = PeerSelectionTargets r' k' e' a' kb' eb' ab'
    , PeerSelection.sanePeerSelectionTargets targets'
    ]


-- | Given a NtNAddr generate the necessary things to run a node in
-- Simulation
genNodeArgs :: [RelayAccessInfo]
            -> Int
            -> [(HotValency, WarmValency, Map RelayAccessPoint (PeerAdvertise, PeerTrustable))]
            -> RelayAccessInfo
            -> Gen NodeArgs
genNodeArgs relays minConnected localRootPeers relay = flip suchThat hasUpstream $ do
  -- Slot length needs to be greater than 0 else we get a livelock on
  -- the IOSim.
  --
  -- Quota values matches mainnet, so a slot length of 1s and 1 / 20
  -- chance that someone gets to make a block
  seed <- arbitrary

  -- Generating an InitiatorResponderMode node is 3 times more likely since we
  -- want our tests to cover more this case.
  diffusionMode <- frequency [ (1, pure InitiatorOnlyDiffusionMode)
                             , (3, pure InitiatorAndResponderDiffusionMode)
                             ]

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

  -- Make sure our targets for active peers cover the maximum of peers
  -- one generated
  SmallTargets deadlineTargets <- resize (length relays * 2) arbitrary
                                       `suchThat` hasActive
  SmallTargets syncTargets <- resize (length relays * 2) arbitrary
                                       `suchThat` hasActive
  let peerTargets = (deadlineTargets, syncTargets)
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

  let (ledgerPeersRelays, publicRootsRelays) =
        splitAt (length relays `div` 2) relays
      publicRoots =
        Map.fromList [ (makeRelayAccessPoint relay', advertise)
                     | relay' <- publicRootsRelays
                     , relay' /= relay
                     , let advertise = case relay' of
                             RelayAddrInfo        _ip _port adv -> adv
                             RelayDomainInfo _dns _ip _port adv -> adv
                     ]
  ledgerPeers <- fmap (map makeRelayAccessPoint) <$> listOf (sublistOf ledgerPeersRelays)
  ledgerPeerPools <- traverse genLedgerPoolsFrom ledgerPeers
  firstLedgerPool <- arbitrary
  let ledgerPeerPoolsScript = Script (firstLedgerPool :| ledgerPeerPools)

  fetchModeScript <- fmap (bool FetchModeBulkSync FetchModeDeadline) <$> arbitrary

  naConsensusMode <- arbitrary
  bootstrapPeersDomain <-
    case naConsensusMode of
      GenesisMode -> pure . singletonScript $ DontUseBootstrapPeers
      PraosMode   -> Script . NonEmpty.fromList <$> listOf1 arbitrary

  return
   $ NodeArgs
      { naSeed                   = seed
      , naDiffusionMode          = diffusionMode
      , naMbTime                 = mustReplyTimeout
      , naPublicRoots            = publicRoots
        -- TODO: we haven't been using public root peers so far because we set
        -- `UseLedgerPeers 0`!
      , naConsensusMode
      , naBootstrapPeers         = bootstrapPeersDomain
      , naAddr                   = makeNtNAddr relay
      , naLocalRootPeers         = localRootPeers
      , naLedgerPeers            = ledgerPeerPoolsScript
      , naPeerTargets            = peerTargets
      , naDNSTimeoutScript       = dnsTimeout
      , naDNSLookupDelayScript   = dnsLookupDelay
      , naChainSyncExitOnBlockNo = chainSyncExitOnBlockNo
      , naChainSyncEarlyExit     = chainSyncEarlyExit
      , naPeerSharing            = peerSharing
      , naFetchModeScript        = fetchModeScript
      }
  where
    hasActive :: SmallPeerSelectionTargets -> Bool
    hasActive (SmallTargets (PeerSelectionTargets {
                 targetNumberOfActivePeers = y,
                 targetNumberOfActiveBigLedgerPeers = z
               })) =
      y + z >= minConnected

    hasUpstream :: NodeArgs -> Bool
    hasUpstream NodeArgs { naAddr, naPublicRoots, naLocalRootPeers } =
         not (Map.null $ naPublicRoots
                         `Map.withoutKeys`
                         Set.fromList (maybeToList (ntnAddrToRelayAccessPoint naAddr)))
      || any id [ v > 0 && not (Map.null m)
                | (HotValency v, _, m) <- naLocalRootPeers
                ]

--
-- DomainMapScript
--

-- 'DomainMapScript' describes evolution of domain name resolution.
--
type DomainMapScript = TimedScript (Map Domain [(IP, TTL)])


-- | Make sure that the final domain map can resolve all the domains correctly.
--
fixupDomainMapScript :: RelayAccessInfos -> DomainMapScript -> DomainMapScript
fixupDomainMapScript relays (Script (a@(_, delay) :| as)) =
    case reverse as of
      []                  -> Script $ (dnsMap, delay) :| as
      ((_, delay') : as') -> Script $ a :| reverse ((dnsMap, delay') : as')
  where
    dnsMap :: Map Domain [(IP, TTL)]
    dnsMap = Map.fromListWith (++)
      [ (domain, [(ip, 300)])
      | RelayDomainInfo domain ip _ _ <- getRelayAccessInfos relays
      ]


-- | Generate a `DomainMapScript`.  Each step contains modification of the full
-- dns map with at most 20% entries removed and 20% entries modified.  The last
-- scripted value is the full dns map which ensures that eventually all dns
-- names resolve to correct ip addresses.
--
genDomainMapScript :: RelayAccessInfos -> Gen DomainMapScript
genDomainMapScript relays = fixupDomainMapScript relays
                         <$> arbitraryScriptOf 10
                               ((,) <$> genDomainMap <*> arbitrary)
  where
    genDomainMap :: Gen (Map Domain [(IP, TTL)])
    genDomainMap = do
      rm <- removedDomains
      md <- modifiedDomains
      return $ Map.fromList md `Map.union` foldr Map.delete dnsMap rm

    removedDomains :: Gen [Domain]
    removedDomains = do
        as <- vectorOf (length domains) (frequency [(4, pure True), (1, pure False)])
        return $ map fst . filter snd $ zip domains as
      where
        domains = Map.keys dnsMap

    modifiedDomains :: Gen [(Domain, [(IP, TTL)])]
    modifiedDomains = do
        as <- vectorOf (length domains) (frequency [(4, pure True), (1, pure False)])
        let ds :: [Domain]
            ds = map fst . filter snd $ zip domains as
        ips <- vectorOf (length ds) (case relays of
                                       IPv4RelayAccessInfos _ -> PeerSelection.genIPv4
                                       IPv6RelayAccessInfos _ -> PeerSelection.genIPv6)
        return $ zip ds ((\a -> [(a,ttl)]) <$> ips)
      where
        domains = Map.keys dnsMap

    dnsMap :: Map Domain [(IP, TTL)]
    dnsMap = Map.fromListWith (++)
      [ (domain, [(ip, ttl)])
      | RelayDomainInfo domain ip _ _ <- getRelayAccessInfos relays
      ]

    ttl = 300


shrinkDomainMapScript :: RelayAccessInfos -> DomainMapScript -> [DomainMapScript]
shrinkDomainMapScript relays script =
    catMaybes $
        -- make sure `fixupDomainMapScript` didn't return something that's
        -- equal to the original `script`
        (\script' -> if script == script' then Nothing else Just script')
     .  fixupDomainMapScript relays
    <$> shrinkScriptWith (shrinkTuple shrinkMap_ shrink) script
  where
    shrinkMap_ :: Ord a => Map a b -> [Map a b]
    shrinkMap_ = map Map.fromList . shrinkList (const []) . Map.toList

    shrinkTuple :: (a -> [a]) -> (b -> [b]) -> (a, b) -> [(a, b)]
    shrinkTuple f g (a, b) = [(a', b) | a' <- f a]
                          ++ [(a, b') | b' <- g b]

--
-- DiffusionScript
--

-- | Multinode Diffusion Simulator Script
--
-- 'SimArgs' with all the values needed for running the simulation, followed
-- by a list of 'NodeArgs' where each element represents one running node and
-- respective 'Command's.
--
data DiffusionScript = DiffusionScript
                         SimArgs
                         DomainMapScript
                         [(NodeArgs, [Command])]

instance Show DiffusionScript where
    show (DiffusionScript args dnsScript nodes) =
      "DiffusionScript (" ++ show args ++ ") (" ++ show dnsScript ++ ") " ++ show nodes

-- | Information describing how nodes can be accessed.
--
data RelayAccessInfo
    = RelayAddrInfo          IP PortNumber PeerAdvertise
    -- ^ relays available using ip / port pair
    | RelayDomainInfo Domain IP PortNumber PeerAdvertise
    -- ^ relays available either using the given domain.
  deriving (Show, Eq)

genRelayAccessInfo :: Gen IP
                   -> Gen RelayAccessInfo
genRelayAccessInfo genIP =
  oneof [ RelayAddrInfo <$> genIP
                        <*> (fromIntegral <$> (arbitrary :: Gen Int))
                        <*> arbitrary
        , (\(DomainAccessPoint domain port) ip advertise -> RelayDomainInfo domain ip port advertise)
                        <$> arbitrary
                        <*> genIP
                        <*> arbitrary
        ]

makeRelayAccessPoint :: RelayAccessInfo -> RelayAccessPoint
makeRelayAccessPoint (RelayAddrInfo ip port _) = RelayAccessAddress ip port
makeRelayAccessPoint (RelayDomainInfo domain _ip port _) = RelayAccessDomain domain port

makeNtNAddr :: RelayAccessInfo -> NtNAddr
makeNtNAddr (RelayAddrInfo ip port _)        = TestAddress (IPAddr ip port)
makeNtNAddr (RelayDomainInfo _dns ip port _) = TestAddress (IPAddr ip port)

data RelayAccessInfos
    -- IPv4 only network
  = IPv4RelayAccessInfos { getRelayAccessInfos :: [RelayAccessInfo] }
    -- IPv6 only network
  | IPv6RelayAccessInfos { getRelayAccessInfos :: [RelayAccessInfo] }
  deriving Show

fixupRelayAccessInfos :: [RelayAccessInfo] -> [RelayAccessInfo]
fixupRelayAccessInfos as = f <$> as
    where
      -- map domains to the same port number
      m = Map.fromList [ (domain, port)
                       | RelayDomainInfo domain _ port _ <- as
                       ]

      f a@RelayAddrInfo {} = a
      f (RelayDomainInfo domain ip _ advertise) = RelayDomainInfo domain ip (m Map.! domain) advertise


-- Generate a list of either IPv4 only or IPv6 only `RelayAccessInfo`.  All
-- `IP`'s using the same domain name are guaranteed to use the same port
-- number.
instance Arbitrary RelayAccessInfos where
    arbitrary = oneof
      [ do -- Limit the number of nodes to run in Simulation in order to limit
           -- simulation execution (real) time.
           size <- chooseInt (1,3)
           IPv4RelayAccessInfos . fixupRelayAccessInfos
             <$> vectorOf size (genRelayAccessInfo PeerSelection.genIPv4)

      , do -- Limit the number of nodes to run in Simulation in order to limit
           -- simulation execution (real) time.
           size <- chooseInt (1,3)
           IPv6RelayAccessInfos . fixupRelayAccessInfos
             <$> vectorOf size (genRelayAccessInfo PeerSelection.genIPv6)
      ]

    shrink (IPv4RelayAccessInfos as) = IPv4RelayAccessInfos . fixupRelayAccessInfos
                                   <$> shrinkList (const []) as
    shrink (IPv6RelayAccessInfos as) = IPv6RelayAccessInfos . fixupRelayAccessInfos
                                   <$> shrinkList (const []) as



-- | Relays access info and dns script.
--
data RelayAccessInfosWithDNS = RelayAccessInfosWithDNS RelayAccessInfos DomainMapScript
  deriving Show


instance Arbitrary RelayAccessInfosWithDNS where
    arbitrary =
      flip suchThat (\(RelayAccessInfosWithDNS infos _)
                      -> length (getRelayAccessInfos infos) >= 2) $ do
        infos <- arbitrary
        domainMapScript <- genDomainMapScript infos
        return $ RelayAccessInfosWithDNS infos domainMapScript

    shrink (RelayAccessInfosWithDNS infos dnsMapScript) =
      [ RelayAccessInfosWithDNS infos (fixupDomainMapScript infos' dnsMapScript)
      | infos' <- shrink infos
      , length (getRelayAccessInfos infos') >= 2
      ]
      ++
      [ RelayAccessInfosWithDNS infos dnsMapScript'
      | dnsMapScript' <- shrinkDomainMapScript infos dnsMapScript
      ]


genDiffusionScript :: ([RelayAccessInfo]
                        -> RelayAccessInfo
                        -> Gen [( HotValency
                                , WarmValency
                                , Map RelayAccessPoint (PeerAdvertise, PeerTrustable))])
                   -> RelayAccessInfosWithDNS
                   -> Gen (SimArgs, DomainMapScript, [(NodeArgs, [Command])])
genDiffusionScript genLocalRootPeers
                   (RelayAccessInfosWithDNS relays dnsMapScript)
                   = do
    let simArgs = mainnetSimArgs (length relays')
    nodesWithCommands <- mapM go (nubBy ((==) `on` getRelayIP) relays')
    return (simArgs, dnsMapScript, nodesWithCommands)
  where
    getRelayIP :: RelayAccessInfo -> IP
    getRelayIP (RelayAddrInfo ip _ _)     = ip
    getRelayIP (RelayDomainInfo _ ip _ _) = ip

    relays' :: [RelayAccessInfo]
    relays' = getRelayAccessInfos relays

    go :: RelayAccessInfo -> Gen (NodeArgs, [Command])
    go relay = do
      let otherRelays  = relay `delete` relays'
          minConnected = 3 `max` (length relays' - 1)
      localRts <- genLocalRootPeers otherRelays relay
      nodeArgs <- genNodeArgs relays' minConnected localRts relay
      commands <- genCommands localRts
      return (nodeArgs, commands)


-- | Multinode Diffusion Simulator Script
--
-- Tries to generate a reasonable looking network with at most 3 nodes that can
-- or can not be connected to one another. These nodes can also randomly die or
-- have their local configuration changed.
--
genNonHotDiffusionScript :: RelayAccessInfosWithDNS
                         -> Gen (SimArgs, DomainMapScript, [(NodeArgs, [Command])])
genNonHotDiffusionScript = genDiffusionScript genLocalRootPeers
  where
    -- | Generate Local Root Peers
    --
    genLocalRootPeers :: [RelayAccessInfo]
                      -> RelayAccessInfo
                      -> Gen [( HotValency
                              , WarmValency
                              , Map RelayAccessPoint (PeerAdvertise, PeerTrustable)
                              )]
    genLocalRootPeers relays _relay = flip suchThat hasUpstream $ do
      nrGroups <- chooseInt (1, 3)
      -- Remove self from local root peers
      let size = length relays
          sizePerGroup = (size `div` nrGroups) + 1

      peerAdvertise <- vectorOf size arbitrary

      let relaysAdv = zip (makeRelayAccessPoint <$> relays) peerAdvertise
          relayGroups = divvy sizePerGroup relaysAdv
          relayGroupsMap = Map.fromList <$> relayGroups

      target <- forM relayGroups
                    (\x -> if null x
                           then pure (0, 0)
                           else genTargets (length x))

      let lrpGroups = zipWith (\(h, w) g -> (h, w, g))
                              target
                              relayGroupsMap

      return lrpGroups

    genTargets :: Int -> Gen (HotValency, WarmValency)
    genTargets l = do
      warmValency <- WarmValency <$> chooseEnum (1, l)
      hotValency <- HotValency <$> chooseEnum (1, getWarmValency warmValency)
      return (hotValency, warmValency)

    hasUpstream :: [( HotValency
                    , WarmValency
                    , Map RelayAccessPoint (PeerAdvertise, PeerTrustable)
                    )]
                -> Bool
    hasUpstream localRootPeers =
      any id [ v > 0 && not (Map.null m)
             | (HotValency v, _, m) <- localRootPeers
             ]


-- | Multinode Hot Diffusion Simulator Script
--
-- Tries to generate a network with at most 2 nodes that should
-- be connected to one another. This generator tries to obtain high ratios of
-- active peers so we can test the miniprotocols that run when we have such
-- active connections. These nodes can not randomly die or have their local
-- configuration changed. Their local root peers consist of a single group.
--
genHotDiffusionScript :: RelayAccessInfosWithDNS
                      -> Gen (SimArgs, DomainMapScript, [(NodeArgs, [Command])])
genHotDiffusionScript = genDiffusionScript genLocalRootPeers
    where
      -- | Generate Local Root Peers.  This only generates 1 group
      --
      genLocalRootPeers :: [RelayAccessInfo]
                        -> RelayAccessInfo
                        -> Gen [( HotValency
                                , WarmValency
                                , Map RelayAccessPoint (PeerAdvertise, PeerTrustable)
                                )]
      genLocalRootPeers relays _relay = flip suchThat hasUpstream $ do
        let size = length relays

        peerAdvertise <- vectorOf size arbitrary

        let relaysAdv      = zip (makeRelayAccessPoint <$> relays) peerAdvertise
            relayGroupsMap = Map.fromList relaysAdv
            warmTarget         = length relaysAdv

        hotTarget <- choose (0 , warmTarget)

        return [( HotValency hotTarget
                , WarmValency warmTarget
                , relayGroupsMap
                )]

      hasUpstream :: [( HotValency
                      , WarmValency
                      , Map RelayAccessPoint (PeerAdvertise, PeerTrustable)
                      )]
                  -> Bool
      hasUpstream localRootPeers =
        any id [ v > 0 && not (Map.null m)
               | (HotValency v, _, m) <- localRootPeers
               ]


instance Arbitrary DiffusionScript where
  arbitrary = (\(a,b,c) -> DiffusionScript a b c)
            <$> frequency [ (1, arbitrary >>= genNonHotDiffusionScript)
                          , (1, arbitrary >>= genHotDiffusionScript)
                          ]
  -- TODO: shrink dns map
  -- TODO: we should write more careful shrinking than recursively shrinking
  -- `DiffusionScript`!
  shrink (DiffusionScript _ _ []) = []
  shrink (DiffusionScript sargs dnsMap ((nargs, cmds):s)) = do
    shrinkedCmds <- fixupCommands <$> shrinkList shrinkCommand cmds
    DiffusionScript sa dnsMap' ss <- shrink (DiffusionScript sargs dnsMap s)
    return (DiffusionScript sa dnsMap' ((nargs, shrinkedCmds) : ss))
    where
      shrinkDelay = map fromRational . shrink . toRational

      shrinkCommand :: Command -> [Command]
      shrinkCommand (JoinNetwork d)     = JoinNetwork <$> shrinkDelay d
      shrinkCommand (Kill d)            = Kill        <$> shrinkDelay d
      shrinkCommand (Reconfigure d lrp) = Reconfigure <$> shrinkDelay d
                                                      <*> pure lrp

-- | Multinode Hot Diffusion Simulator Script
--
-- List of 'SimArgs'. Each element of the list represents one running node.
--
data HotDiffusionScript = HotDiffusionScript
                            SimArgs
                            DomainMapScript
                            [(NodeArgs, [Command])]
  deriving Show

instance Arbitrary HotDiffusionScript where
  arbitrary = (\(a,b,c) -> HotDiffusionScript a b c) <$> (arbitrary >>= genHotDiffusionScript)
  shrink (HotDiffusionScript sargs dnsMap hds) =
    [ HotDiffusionScript sa dnsMap' ds
    | DiffusionScript sa dnsMap' ds <- shrink (DiffusionScript sargs dnsMap hds) ]

-- Tests if the fixupCommand is idempotent.
-- Note that the generator for DiffusionScript already fixups the Command list.
--
prop_diffusionScript_fixupCommands :: DiffusionScript -> Property
prop_diffusionScript_fixupCommands (DiffusionScript _ _ []) = property True
prop_diffusionScript_fixupCommands (DiffusionScript sa dnsMap ((_, cmds): t)) =
  counterexample ("Failed with cmds: " ++ show cmds ++ "\n"
                  ++ "fixupCommands cmds = " ++ show (fixupCommands cmds)
                 ) $
  fixupCommands cmds == cmds
  .&&. prop_diffusionScript_fixupCommands (DiffusionScript sa dnsMap t)

-- Tests if the fixupCommand outputs valid command scripts.
--
-- Note that the generator for DiffusionScript already fixups the Command list.
--
prop_diffusionScript_commandScript_valid :: DiffusionScript -> Property
prop_diffusionScript_commandScript_valid (DiffusionScript _ _ []) = property True
prop_diffusionScript_commandScript_valid (DiffusionScript sa dnsMap ((_, cmds): t)) =
  counterexample ("Failed with cmds: " ++ show cmds) $
  isValid cmds
  .&&. prop_diffusionScript_commandScript_valid (DiffusionScript sa dnsMap t)
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
  | TrReconfiguringNode
  | TrUpdatingDNS
  | TrRunning
  | TrErrored SomeException
  deriving (Show)

-- Warning: be careful with writing properties that rely
-- on trace events from multiple components environment.
-- These events typically occur in separate threads and
-- so are not casually ordered. It is ok to use them for
-- timeout/eventually properties, but not for properties
-- that check conditions synchronously.
--
data DiffusionTestTrace =
      DiffusionLocalRootPeerTrace (TraceLocalRootPeers PeerTrustable NtNAddr SomeException)
    | DiffusionPublicRootPeerTrace TracePublicRootPeers
    | DiffusionLedgerPeersTrace TraceLedgerPeers
    | DiffusionPeerSelectionTrace (TracePeerSelection CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers NtNAddr) NtNAddr)
    | DiffusionPeerSelectionActionsTrace (PeerSelectionActionsTrace NtNAddr NtNVersion)
    | DiffusionDebugPeerSelectionTrace (DebugPeerSelection CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers NtNAddr) NtNAddr)
    | DiffusionConnectionManagerTrace
        (CM.Trace NtNAddr
          (ConnectionHandlerTrace NtNVersion NtNVersionData))
    | DiffusionDiffusionSimulationTrace DiffusionSimulationTrace
    | DiffusionConnectionManagerTransitionTrace
        (AbstractTransitionTrace NtNAddr)
    | DiffusionInboundGovernorTransitionTrace
        (RemoteTransitionTrace NtNAddr)
    | DiffusionInboundGovernorTrace (IG.Trace NtNAddr)
    | DiffusionServerTrace (Server.Trace NtNAddr)
    | DiffusionFetchTrace (TraceFetchClientState BlockHeader)
    | DiffusionDebugTrace String
    deriving (Show)


-- | A debug tracer which embeds events in DiffusionTestTrace.
--
iosimTracer :: forall s a.
              ( Show a
              , Typeable a
              )
            => Tracer (IOSim s) (WithTime (WithName NtNAddr a))
iosimTracer = Tracer traceM <> sayTracer

-- | Run an arbitrary topology
diffusionSimulation
  :: forall m. ( Alternative (STM m)
               , MonadAsync       m
               , MonadDelay       m
               , MonadFix         m
               , MonadFork        m
               , MonadSay         m
               , MonadST          m
               , MonadEvaluate    m
               , MonadLabelledSTM m
               , MonadTraceSTM    m
               , MonadMask        m
               , MonadTime        m
               , MonadTimer       m
               , MonadThrow  (STM m)
               , MonadMVar        m
               , forall a. Semigroup a => Semigroup (m a)
               )
  => BearerInfo
  -> DiffusionScript
  -> Tracer m (WithTime (WithName NtNAddr DiffusionTestTrace))
  -- ^ timed trace of nodes in the system
  -> m Void
diffusionSimulation
  defaultBearerInfo
  (DiffusionScript simArgs dnsMapScript nodeArgs)
  nodeTracer =
    withSnocket netSimTracer defaultBearerInfo Map.empty
      $ \ntnSnocket _ ->
        withSnocket nullTracer defaultBearerInfo Map.empty
      $ \ntcSnocket _ -> do
        dnsMapVar <- fromLazyTVar <$> playTimedScript nullTracer dnsMapScript
        withAsyncAll
          (map (uncurry (runCommand Nothing ntnSnocket ntcSnocket dnsMapVar simArgs))
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
               , StrictTVar m [( HotValency
                               , WarmValency
                               , Map RelayAccessPoint (PeerAdvertise, PeerTrustable)
                               )])
         -- ^ If the node is running and corresponding local root configuration
         -- TVar.
      -> Snocket m (FD m NtNAddr) NtNAddr
        -- ^ Node to node Snocket
      -> Snocket m (FD m NtCAddr) NtCAddr
        -- ^ Node to client Snocket
      -> StrictTVar m (Map Domain [(IP, TTL)])
        -- ^ Map of domain map TVars to be updated in case a node changes its IP
      -> SimArgs -- ^ Simulation arguments needed in order to run a simulation
      -> NodeArgs -- ^ Simulation arguments needed in order to run a single node
      -> [Command] -- ^ List of commands/actions to perform for a single node
      -> m Void
    runCommand Nothing ntnSnocket ntcSnocket dnsMapVar sArgs nArgs [] = do
      threadDelay 3600
      traceWith (diffSimTracer (naAddr nArgs)) TrRunning
      runCommand Nothing ntnSnocket ntcSnocket dnsMapVar sArgs nArgs []
    runCommand (Just (_, _)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs [] = do
      -- We shouldn't block this thread waiting
      -- on the async since this will lead to a deadlock
      -- as thread returns 'Void'.
      threadDelay 3600
      traceWith (diffSimTracer (naAddr nArgs)) TrRunning
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs []
    runCommand Nothing ntnSnocket ntcSnocket dnsMapVar sArgs nArgs
               (JoinNetwork delay :cs) = do
      threadDelay delay
      traceWith (diffSimTracer (naAddr nArgs)) TrJoiningNetwork
      lrpVar <- newTVarIO $ naLocalRootPeers nArgs
      withAsync (runNode sArgs nArgs ntnSnocket ntcSnocket lrpVar dnsMapVar) $ \nodeAsync ->
        runCommand (Just (nodeAsync, lrpVar)) ntnSnocket ntcSnocket dnsMapVar sArgs nArgs cs
    runCommand _ _ _ _ _ _ (JoinNetwork _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async_, _)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs
               (Kill delay:cs) = do
      threadDelay delay
      traceWith (diffSimTracer (naAddr nArgs)) TrKillingNode
      cancel async_
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs cs
    runCommand _ _ _ _ _ _ (Kill _:_) = do
      error "runCommand: Impossible happened"
    runCommand Nothing _ _ _ _ _ (Reconfigure _ _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async_, lrpVar)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs
               (Reconfigure delay newLrp:cs) = do
      threadDelay delay
      traceWith (diffSimTracer (naAddr nArgs)) TrReconfiguringNode
      _ <- atomically $ writeTVar lrpVar newLrp
      runCommand (Just (async_, lrpVar)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs
                 cs

    runNode :: SimArgs
            -> NodeArgs
            -> Snocket m (FD m NtNAddr) NtNAddr
            -> Snocket m (FD m NtCAddr) NtCAddr
            -> StrictTVar m [( HotValency
                             , WarmValency
                             , Map RelayAccessPoint (PeerAdvertise, PeerTrustable)
                             )]
            -> StrictTVar m (Map Domain [(IP, TTL)])
            -> m Void
    runNode SimArgs
            { saSlot                  = bgaSlotDuration
            , saQuota                 = quota
            }
            NodeArgs
            { naSeed                   = seed
            , naMbTime                 = mustReplyTimeout
            , naPublicRoots            = publicRoots
            , naConsensusMode          = consensusMode
            , naBootstrapPeers         = bootstrapPeers
            , naAddr                   = addr
            , naLedgerPeers            = ledgerPeers
            , naPeerTargets            = peerTargets
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
      ledgerPeersVar <- initScript' ledgerPeers
      onlyOutboundConnectionsStateVar <- newTVarIO UntrustedState
      let (bgaRng, rng) = Random.split $ mkStdGen seed
          acceptedConnectionsLimit =
            AcceptedConnectionsLimit maxBound maxBound 0
          diffusionMode = InitiatorAndResponderDiffusionMode
          readLocalRootPeers  = readTVar lrpVar
          readPublicRootPeers = return publicRoots
          readUseLedgerPeers  = return (UseLedgerPeers (After 0))

          acceptVersion = \_ v -> Accept v

      useBootstrapPeersScriptVar <- newTVarIO bootstrapPeers
      churnModeVar <- newTVarIO ChurnModeNormal
      dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore
      dnsTimeoutScriptVar <- newTVarIO dnsTimeout
      dnsLookupDelayScriptVar <- newTVarIO dnsLookupDelay

      let dnsActions :: PeerActionsDNS (TestAddress NtNAddr_) () BootstrapPeersCriticalTimeoutError m
          dnsActions =
            PeerActionsDNS {
              paToPeerAddr = (\a b -> TestAddress (IPAddr a b))
            , paDnsActions = mockDNSActions dMapVar
                                            dnsTimeoutScriptVar
                                            dnsLookupDelayScriptVar
            }
          readUseBootstrapPeers = stepScriptSTM' useBootstrapPeersScriptVar
          (bgaRng, rng) = Random.split $ mkStdGen seed
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
                , idleTimeout      = Nothing
                }

          limitsAndTimeouts :: NodeKernel.LimitsAndTimeouts BlockHeader Block
          limitsAndTimeouts
            = NodeKernel.LimitsAndTimeouts
                { NodeKernel.chainSyncLimits      = defaultMiniProtocolsLimit
                , NodeKernel.chainSyncSizeLimits  = byteLimitsChainSync (const 0)
                , NodeKernel.chainSyncTimeLimits  =
                    timeLimitsChainSync stdChainSyncTimeout
                , NodeKernel.blockFetchLimits     = defaultMiniProtocolsLimit
                , NodeKernel.blockFetchSizeLimits = byteLimitsBlockFetch (const 0)
                , NodeKernel.blockFetchTimeLimits = timeLimitsBlockFetch
          limitsAndTimeouts :: LimitsAndTimeouts BlockHeader Block
                , NodeKernel.keepAliveSizeLimits  = byteLimitsKeepAlive (const 0)
            = LimitsAndTimeouts
                { chainSyncLimits      = defaultMiniProtocolsLimit
                , chainSyncSizeLimits  = byteLimitsChainSync (const 0)
                , chainSyncTimeLimits  =
                , NodeKernel.handshakeLimits      = defaultMiniProtocolsLimit
                , blockFetchLimits     = defaultMiniProtocolsLimit
                , blockFetchSizeLimits = byteLimitsBlockFetch (const 0)
                , blockFetchTimeLimits = timeLimitsBlockFetch
                , keepAliveLimits      = defaultMiniProtocolsLimit
                , keepAliveSizeLimits  = byteLimitsKeepAlive (const 0)
                , keepAliveTimeLimits  = timeLimitsKeepAlive
                , pingPongLimits       = defaultMiniProtocolsLimit
                , pingPongSizeLimits   = byteLimitsPingPong
                , pingPongTimeLimits   = timeLimitsPingPong
                , handshakeLimits      = defaultMiniProtocolsLimit
                , handshakeTimeLimits  =
                }
                , handhsakeSizeLimits  =
          interfaces :: NodeKernel.Interfaces (CardanoLedgerPeersConsensusInterface m) m
          interfaces =
                , peerSharingLimits     = defaultMiniProtocolsLimit
                , peerSharingTimeLimits =
              , NodeKernel.iNtnBearer         = makeFDBearer
                , peerSharingSizeLimits =
              , NodeKernel.iNtnDomainResolver = domainResolver dMapVar
              , NodeKernel.iNtcSnocket        = ntcSnocket
              , NodeKernel.iNtcBearer         = makeFDBearer
              , NodeKernel.iRng               = rng
          interfaces :: Interfaces (CardanoLedgerPeersConsensusInterface m) m
              , NodeKernel.iLedgerPeersConsensusInterface
            Interfaces
              { iNtnSnocket        = ntnSnocket
              , iNtnBearer         = makeFDBearer
              , iAcceptVersion     = acceptVersion
              , iNtnDomainResolver = domainResolver dMapVar
              , iNtcSnocket        = ntcSnocket
              , iNtcBearer         = makeFDBearer
              , iRng               = rng
              , iDomainMap         = dMapVar
              , iLedgerPeersConsensusInterface
                      clpciGetLedgerStateJudgement = pure TooOld
                    , clpciUpdateOutboundConnectionsState =
                        \a -> do
                          a' <- readTVar onlyOutboundConnectionsStateVar
                          when (a /= a') $
                            writeTVar onlyOutboundConnectionsStateVar a
                    }
              }

          shouldChainSyncExit :: StrictTVar m (Maybe BlockNo) -> BlockHeader -> m Bool
          shouldChainSyncExit v header = atomically $ do
            mbBlockNo <- readTVar v
            case mbBlockNo of
              Nothing ->
                return False

              Just blockNo | blockNo >= headerBlockNo header -> do
                -- next time exit in 10 blocks
                writeTVar v (Just $ blockNo + 10)
                return True

                           | otherwise ->
                return False

          arguments :: NodeKernel.Arguments m
          arguments =
            NodeKernel.Arguments
              { NodeKernel.aIPAddress            = addr
              , NodeKernel.aAcceptedLimits       = acceptedConnectionsLimit
              , NodeKernel.aDiffusionMode        = diffusionMode
              , NodeKernel.aKeepAliveInterval    = 10
              , NodeKernel.aPingPongInterval     = 10
              , NodeKernel.aPeerTargets          = peerTargets
              , NodeKernel.aShouldChainSyncExit  = shouldChainSyncExit chainSyncExitVar
          cardanoExtraArgs :: CardanoArgumentsExtra m
          cardanoExtraArgs =
            CardanoArgumentsExtra {
              caeSyncPeerTargets                  = snd peerTargets
            , caeReadUseBootstrapPeers            = readUseBootstrapPeers
            , caeMinBigLedgerPeersForTrustedState = defaultMinBigLedgerPeersForTrustedState
            , caeConsensusMode                    = consensusMode
            }

          cardanoChurnArgs :: CardanoPeerChurnArgs m
          cardanoChurnArgs =
            CardanoPeerChurnArgs {
              cpcaModeVar             = churnModeVar
            , cpcaReadFetchMode       = pure FetchModeDeadline
            , cpcaSyncPeerTargets     = caeSyncPeerTargets cardanoExtraArgs
            , cpcaReadUseBootstrap    = caeReadUseBootstrapPeers cardanoExtraArgs
            , cpcaConsensusMode       = consensusMode
            }

          arguments :: Arguments (CardanoArgumentsExtra m) (CardanoPeerChurnArgs m) PeerTrustable m
              , NodeKernel.aReadLocalRootPeers   = readLocalRootPeers
            Arguments
              { aIPAddress            = addr
              , aAcceptedLimits       = acceptedConnectionsLimit
              , aDiffusionMode        = diffusionMode
              , aKeepAliveInterval    = 10
              , aPingPongInterval     = 10
              , aPeerTargets          = fst peerTargets
              , aShouldChainSyncExit  = shouldChainSyncExit chainSyncExitVar
              , aChainSyncEarlyExit   = chainSyncEarlyExit
              , aReadLocalRootPeers   = readLocalRootPeers
              , aReadPublicRootPeers  = readPublicRootPeers
              , aOwnPeerSharing       = peerSharing
              , aReadUseLedgerPeers   = readUseLedgerPeers
              , aProtocolIdleTimeout  = 5
              , aTimeWaitTimeout      = 30
              , aDNSTimeoutScript     = dnsTimeout
              , aDNSLookupDelayScript = dnsLookupDelay
              , aDebugTracer          = (\s -> WithTime (Time (-1)) (WithName addr (DiffusionDebugTrace s)))
               . tracerWithTime
              , aExtraArgs      = cardanoExtraArgs
              , aExtraChurnArgs = cardanoChurnArgs
               $ nodeTracer)
        `catch` \e -> traceWith (diffSimTracer addr) (TrErrored e)
                   >> throwIO e

    domainResolver :: StrictTVar m (Map Domain [(IP, TTL)])
                   -> DNSLookupType
                   -> [DomainAccessPoint]
                   -> m (Map DomainAccessPoint (Set NtNAddr))
    -- TODO: we can take into account the `LookupReqs` and return only `IPv4`
    -- / `IPv6` if so requested.  But we should make sure the connectivity graph
    -- is not severely reduced.
    domainResolver dnsMapVar _ daps = do
      dnsMap <- fmap (map fst) <$> atomically (readTVar dnsMapVar)
      let mapDomains :: [(DomainAccessPoint, Set NtNAddr)]
          mapDomains = [ ( dap
                         , Set.fromList [ ntnToPeerAddr a p | a <- addrs ]
                         )
                       | dap@(DomainAccessPoint d p) <- daps
                       , addrs <- maybeToList (d `Map.lookup` dnsMap) ]
      return (Map.fromListWith (<>) mapDomains)

    diffSimTracer :: NtNAddr -> Tracer m DiffusionSimulationTrace
    diffSimTracer ntnAddr = contramap DiffusionDiffusionSimulationTrace
                          . tracerWithName ntnAddr
                          . tracerWithTime
                          $ nodeTracer

    tracersExtra
      :: NtNAddr
      -> Common.TracersExtra NtNAddr NtNVersion NtNVersionData
                             NtCAddr NtCVersion NtCVersionData
                             SomeException CardanoPeerSelectionState
                             CardanoPeerSelectionState PeerTrustable
                             (CardanoPublicRootPeers NtNAddr) m
    tracersExtra ntnAddr =
      Common.TracersExtra {
          Common.dtTraceLocalRootPeersTracer         = contramap
                                                        DiffusionLocalRootPeerTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtTracePublicRootPeersTracer        = contramap
                                                        DiffusionPublicRootPeerTrace
                                                     . tracerWithName ntnAddr
          tracersExtraAddr = tracersExtra addr

          requestPublicRootPeers' =
            requestPublicRootPeers (Common.dtTracePublicRootPeersTracer tracersExtraAddr)
                                   (caeReadUseBootstrapPeers cardanoExtraArgs)
                                   (pure TooOld)
                                   (\a b -> TestAddress (IPAddr a b))
                                   dnsSemaphore
                                   readPublicRootPeers
                                   (paDnsActions dnsActions)

      run blockGeneratorArgs
                     limitsAndTimeouts
                     interfaces
                     arguments
                     (CPST.empty consensusMode (MinBigLedgerPeersForTrustedState 0))
                     (cardanoExtraArgsToPeerSelectionActions cardanoExtraArgs)
                     CPRP.empty
                     CPSV.empty
                     CPRP.cardanoPublicRootPeersActions
                     (cardanoPeerSelectionGovernorArgs readUseLedgerPeers peerSharing (iLedgerPeersConsensusInterface interfaces))
                     CPSV.cardanoPeerSelectionStatetoCounters
                     requestPublicRootPeers'
                     peerChurnGovernor
                     tracersExtraAddr
                     ( contramap (DiffusionFetchTrace . (\(TraceLabelPeer _ a) -> a))
                     . tracerWithName addr
                     . tracerWithTime
                     $ nodeTracer)
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtDebugPeerSelectionInitiatorTracer = contramap
                                                        DiffusionDebugPeerSelectionTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtDebugPeerSelectionInitiatorResponderTracer
            = contramap DiffusionDebugPeerSelectionTrace
            . tracerWithName ntnAddr
            . tracerWithTime
            $ nodeTracer
        , Common.dtTracePeerSelectionCounters        = nullTracer
        , Common.dtTraceChurnCounters                = nullTracer
        , Common.dtPeerSelectionActionsTracer        = contramap
                                                        DiffusionPeerSelectionActionsTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtConnectionManagerTracer           = contramap
                                                        DiffusionConnectionManagerTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtConnectionManagerTransitionTracer = contramap
                                                         DiffusionConnectionManagerTransitionTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtServerTracer                      = contramap
                                                         DiffusionServerTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtInboundGovernorTracer             = contramap
                                                         DiffusionInboundGovernorTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtInboundGovernorTransitionTracer   = contramap
                                                         DiffusionInboundGovernorTransitionTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
        , Common.dtLocalConnectionManagerTracer      = nullTracer
        , Common.dtLocalServerTracer                 = nullTracer
        , Common.dtLocalInboundGovernorTracer        = nullTracer
      }


--
-- PingPong byte & time limits
--

byteLimitsPingPong :: ProtocolSizeLimits PingPong.PingPong BL.ByteString
byteLimitsPingPong = ProtocolSizeLimits (const smallByteLimit) (fromIntegral . BL.length)

timeLimitsPingPong :: ProtocolTimeLimits PingPong.PingPong
timeLimitsPingPong = ProtocolTimeLimits $ \case
    PingPong.SingIdle   -> Nothing
    PingPong.SingBusy   -> Just 60
    a@PingPong.SingDone -> notActiveState a

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


-- | Split a list into sub list of at most `n` elements.
--
divvy :: Int -> [a] -> [[a]]
divvy _ [] = []
divvy n as = take n as : divvy n (drop n as)
