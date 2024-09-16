{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
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
  , TurbulentCommands (..)
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
import Control.Monad.IOSim (IOSim, traceM)
import Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)

import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Either (fromLeft, fromRight)
import Data.Foldable (foldlM)
import Data.List (delete, nub, partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (secondsToDiffTime)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Network.DNS (Domain)
import Network.DNS qualified as DNS
import System.Random (StdGen, mkStdGen)
import System.Random qualified as Random

import Network.TypedProtocol.Core
import Network.TypedProtocol.PingPong.Type qualified as PingPong

import Cardano.Network.ConsensusMode
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.PeerSelection.LocalRootPeers
           (OutboundConnectionsState (..))
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Cardano.Network.Types (LedgerStateJudgement (..),
           NumberOfBigLedgerPeers (..))

import Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Cardano.Network.PeerSelection.Churn (ChurnMode (..), TracerChurnMode,
           peerChurnGovernor)
import Cardano.Network.PeerSelection.Churn qualified as Churn
import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionActions qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano hiding
           (consensusMode)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as ExtraState
import Cardano.Network.PeerSelection.Governor.Types qualified as Cardano
import Cardano.Network.PeerSelection.Governor.Types qualified as ExtraSizes
import Cardano.Network.PeerSelection.PeerSelectionActions
           (requestPublicRootPeers)

import Ouroboros.Network.Block (BlockNo)
import Ouroboros.Network.BlockFetch (FetchMode (..), PraosFetchMode (..),
           TraceFetchClientState, TraceLabelPeer (..))
import Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CM
import Ouroboros.Network.ConnectionManager.Types (AbstractTransitionTrace)
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.Driver.Limits (ProtocolSizeLimits (..),
           ProtocolTimeLimits (..))
import Ouroboros.Network.Handshake.Acceptable (Acceptable (acceptableVersion))
import Ouroboros.Network.InboundGovernor (RemoteTransitionTrace)
import Ouroboros.Network.InboundGovernor qualified as IG
import Ouroboros.Network.Mux (MiniProtocolLimits (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection hiding (peerChurnGovernor,
           requestPublicRootPeers)
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.LedgerPeers (accPoolStake)
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           LocalRootConfig, WarmValency (..))
import Ouroboros.Network.Protocol.BlockFetch.Codec (byteLimitsBlockFetch,
           timeLimitsBlockFetch)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout (..),
           byteLimitsChainSync, timeLimitsChainSync)
import Ouroboros.Network.Protocol.KeepAlive.Codec (byteLimitsKeepAlive,
           timeLimitsKeepAlive)
import Ouroboros.Network.Protocol.Limits (shortWait, smallByteLimit)
import Ouroboros.Network.Protocol.PeerSharing.Codec (byteLimitsPeerSharing,
           timeLimitsPeerSharing)
import Ouroboros.Network.Protocol.TxSubmission2.Codec (byteLimitsTxSubmission2,
           timeLimitsTxSubmission2)
import Ouroboros.Network.Server qualified as Server
import Ouroboros.Network.Snocket (Snocket, TestAddress (..))
import Ouroboros.Network.TxSubmission.Inbound.Policy (TxDecisionPolicy)
import Ouroboros.Network.TxSubmission.Inbound.Registry (DebugTxLogic)
import Ouroboros.Network.TxSubmission.Inbound.State (DebugSharedTxState)
import Ouroboros.Network.TxSubmission.Inbound.Types (TraceTxSubmissionInbound)

import Ouroboros.Network.Mock.ConcreteBlock (Block (..), BlockHeader (..))
import Simulation.Network.Snocket (BearerInfo (..), FD, SnocketTrace,
           WithAddr (..), makeFDBearer, withSnocket)

import Test.Ouroboros.Network.Data.Script
import Test.Ouroboros.Network.Diffusion.Node qualified as Node
import Test.Ouroboros.Network.Diffusion.Node.Kernel (NtCAddr, NtCVersion,
           NtCVersionData, NtNAddr, NtNAddr_ (IPAddr), NtNVersion,
           NtNVersionData)
import Test.Ouroboros.Network.LedgerPeers (LedgerPools (..), genLedgerPoolsFrom)
import Test.Ouroboros.Network.PeerSelection.Cardano.Instances ()
import Test.Ouroboros.Network.PeerSelection.Instances qualified as PeerSelection
import Test.Ouroboros.Network.PeerSelection.LocalRootPeers ()
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS (DNSLookupDelay (..),
           DNSTimeout (..), DomainAccessPoint (..), MockDNSMap, genDomainName)
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS qualified as PeerSelection hiding
           (tests)
import Test.Ouroboros.Network.TxSubmission.Common (ArbTxDecisionPolicy (..),
           Tx (..))
import Test.Ouroboros.Network.Utils
import Test.QuickCheck

-- | Diffusion Simulator Arguments
--
-- Contains all necessary randomly generated values needed to run diffusion in
-- simulation.
--
data SimArgs =
  SimArgs
    { saSlot             :: DiffTime
      -- ^ 'randomBlockGenerationArgs' slot duration argument
    , saQuota            :: Int
      -- ^ 'randomBlockGenerationArgs' quota value
    , saTxDecisionPolicy :: TxDecisionPolicy
      -- ^ Decision policy for tx submission protocol
    }

instance Show SimArgs where
    show SimArgs { saSlot, saQuota, saTxDecisionPolicy } =
      unwords [ "SimArgs"
              , show saSlot
              , show saQuota
              , "(" ++ show saTxDecisionPolicy ++ ")"
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
                                   , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
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
    , naTxs                    :: [Tx Int]
    }

instance Show NodeArgs where
    show NodeArgs { naSeed, naDiffusionMode, naMbTime, naBootstrapPeers,
                    naPublicRoots, naAddr, naPeerSharing, naLedgerPeers,
                    naLocalRootPeers, naPeerTargets, naDNSTimeoutScript,
                    naDNSLookupDelayScript, naChainSyncExitOnBlockNo,
                    naChainSyncEarlyExit, naFetchModeScript, naConsensusMode,
                    naTxs } =
      unwords [ "NodeArgs"
              , "(" ++ show naSeed ++ ")"
              , show naDiffusionMode
              , "(" ++ show naMbTime ++ ")"
              , "(" ++ show naPublicRoots ++ ")"
              , show naConsensusMode
              , "(" ++ show naBootstrapPeers ++ ")"
              , "(" ++ show naAddr ++ ")"
              , show naPeerSharing
              , show naLocalRootPeers
              , show naLedgerPeers
              , show naPeerTargets
              , "(" ++ show naDNSTimeoutScript ++ ")"
              , "(" ++ show naDNSLookupDelayScript ++ ")"
              , "(" ++ show naChainSyncExitOnBlockNo ++ ")"
              , show naChainSyncEarlyExit
              , show naFetchModeScript
              , show naTxs
              ]

data Command = JoinNetwork DiffTime
             | Kill DiffTime
             | Reconfigure DiffTime
                           [( HotValency
                            , WarmValency
                            , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
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
                , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
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
                              , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
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

-- | Turbulent commands have some turbulence by connecting and disconnecting
-- the node, but eventually keeping the node online.
--
newtype TurbulentCommands = TurbulentCommands [Command]
  deriving (Eq, Show)

instance Arbitrary TurbulentCommands where
  arbitrary = do
    turbulenceNumber <- choose (2, 7)
    -- Make sure turbulenceNumber is an even number
    -- This simplifies making sure we keep the node online.
    let turbulenceNumber' =
          if odd turbulenceNumber
             then turbulenceNumber + 1
             else turbulenceNumber
    delays <- vectorOf turbulenceNumber' delay
    let commands = zipWith (\f d -> f d) (cycle [JoinNetwork, Kill]) delays
                 ++ [JoinNetwork 0]
    return (TurbulentCommands commands)
    where
      delay = frequency [ (3, genDelayWithPrecision 65)
                        , (1, (/ 10) <$> genDelayWithPrecision 60)
                        ]
  shrink (TurbulentCommands xs) =
    [ TurbulentCommands xs' | xs' <- shrinkList shrinkCommand xs, invariant xs' ] ++
    [ TurbulentCommands (take n xs) | n <- [0, length xs - 3], n `mod` 3 == 0, invariant (take n xs) ]

    where
      shrinkDelay = map fromRational . shrink . toRational

      shrinkCommand :: Command -> [Command]
      shrinkCommand (JoinNetwork d)     = JoinNetwork <$> shrinkDelay d
      shrinkCommand (Kill d)            = Kill        <$> shrinkDelay d
      shrinkCommand (Reconfigure d lrp) = Reconfigure <$> shrinkDelay d
                                                      <*> pure lrp

      invariant :: [Command] -> Bool
      invariant [JoinNetwork _]                                 = True
      invariant [JoinNetwork _, Kill _, JoinNetwork _]          = True
      invariant (JoinNetwork _ : Kill _ : JoinNetwork _ : rest) = invariant rest
      invariant _                                               = False

-- | Simulation arguments.
--
-- Slot length needs to be greater than 0 else we get a livelock on the IOSim.
--
-- Quota values matches mainnet, so a slot length of 1s and 1 / 20 chance that
-- someone gets to make a block.
--
mainnetSimArgs :: Int
               -> TxDecisionPolicy
               -> SimArgs
mainnetSimArgs numberOfNodes txDecisionPolicy =
  SimArgs {
      saSlot  = secondsToDiffTime 1,
      saQuota = if numberOfNodes > 0
                then 20 `div` numberOfNodes
                else 100,
      saTxDecisionPolicy = txDecisionPolicy
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
    , Governor.sanePeerSelectionTargets targets'
    ]


-- | Given a NtNAddr generate the necessary things to run a node in
-- Simulation
genNodeArgs :: [TestnetRelayInfo]
            -> Int
            -> [(HotValency, WarmValency, Map RelayAccessPoint (LocalRootConfig PeerTrustable))]
            -> TestnetRelayInfo
            -> [Tx Int]
            -> Gen NodeArgs
genNodeArgs relays minConnected localRootPeers self txs = flip suchThat hasUpstream $ do
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
        Map.fromList [ (other, advertise)
                     | pubRelay <- publicRootsRelays
                     , pubRelay /= self
                     , let (other, _, _, advertise) = pubRelay
                     ]

  ledgerPeers :: [[NonEmpty RelayAccessPoint]]
     <- listOf1 (listOf1 (sublistOf1 (NonEmpty.fromList $ makeRelayAccessPoint <$> ledgerPeersRelays)))
  ledgerPeersScript_ <- traverse genLedgerPoolsFrom ledgerPeers
  let ledgerPeersScript = Script (NonEmpty.fromList ledgerPeersScript_)

  fetchModeScript <- fmap (PraosFetchMode . bool FetchModeBulkSync FetchModeDeadline) <$> arbitrary

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
      , naAddr                   = TestAddress ((\(_, ip, port, _) -> IPAddr ip port) self)
      , naLocalRootPeers         = localRootPeers
      , naLedgerPeers            = ledgerPeersScript
      , naPeerTargets            = peerTargets
      , naDNSTimeoutScript       = dnsTimeout
      , naDNSLookupDelayScript   = dnsLookupDelay
      , naChainSyncExitOnBlockNo = chainSyncExitOnBlockNo
      , naChainSyncEarlyExit     = chainSyncEarlyExit
      , naPeerSharing            = peerSharing
      , naFetchModeScript        = fetchModeScript
      , naTxs                    = txs
      }
  where
    makeRelayAccessPoint (relay, _, _, _) = relay

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
                         Set.fromList (maybeToList (Node.ntnAddrToRelayAccessPoint naAddr)))
      || any id [ v > 0 && not (Map.null m)
                | (HotValency v, _, m) <- naLocalRootPeers
                ]

--
-- DomainMapScript
--

-- 'DomainMapScript' describes evolution of domain name resolution.
--
type DomainMapScript = TimedScript MockDNSMap


-- | Make sure that the final domain map can resolve all the domains correctly.
--
fixupDomainMapScript :: MockDNSMap -> DomainMapScript -> DomainMapScript
fixupDomainMapScript mockMap (Script (a@(_, delay) :| as)) =
    case reverse as of
      []                  -> Script $ (mockMap, delay) :| as
      ((_, delay') : as') -> Script $ a :| reverse ((mockMap, delay') : as')

-- | Generate a `DomainMapScript`.  Each step contains modification of the full
-- dns map with at most 20% entries removed and 20% entries modified.  The last
-- scripted value is the full dns map which ensures that eventually all dns
-- names resolve to correct ip addresses.
--
genDomainMapScript :: TestnetRelayInfos -> Gen DomainMapScript
genDomainMapScript relays = do
  mockMap <- dnsMapGen
  fixupDomainMapScript mockMap <$>
    arbitraryScriptOf 10 ((,) <$> alterDomainMap mockMap <*> arbitrary)
  where
    them = unTestnetRelays relays
    dnsType = case relays of
      TestnetRelays4 {} -> DNS.A
      TestnetRelays6 {} -> DNS.AAAA

    alterDomainMap mockMap = do
      let dnsAssoc = Map.toList mockMap
      rm <- removedDomains (fst <$> dnsAssoc)
      md <- modifiedDomains dnsAssoc
      return $ Map.fromList md `Map.union` foldr Map.delete mockMap rm

    removedDomains domains = do
        as <- tosses (length domains)
        return $ map fst . filter snd $ zip domains as

    modifiedDomains assoc = do
        mask' <- tosses (length assoc)
        let picked = map fst . filter snd $ zip assoc mask'
            singleton x = [x]
        forM picked \(k, v) ->
          case v of
            Left _ipsttls -> case relays of
              TestnetRelays4 {} ->
                (k,) . Left . singleton . (, ttl) <$> PeerSelection.genIPv4
              TestnetRelays6 {} ->
                (k,) . Left . singleton . (, ttl) <$> PeerSelection.genIPv6
            Right doms -> do
              (k,) . Right . singleton <$> do
                case listToMaybe doms of
                  Just (_, prio, wt, port) -> (, prio, wt, port) <$> genDomainName
                  Nothing -> error "impossible!"

    tosses count = vectorOf count (frequency [(4, pure True), (1, pure False)])

    dnsMapGen = do
      let (srvs, nonsrvs) = partition isSRV them
          srvs' = [(k, d, ip, port)
                  | (relay, k) <- zip srvs [0..]
                  , let (RelayAccessSRVDomain d, ip, port, _) = relay]
      srvMap <- foldlM stepSRV Map.empty srvs'
      let nonSRVMap = foldl stepNonSRV Map.empty nonsrvs
      return $ Map.union srvMap nonSRVMap

    isSRV = \case
      (RelayAccessSRVDomain {}, _, _, _) -> True
      _otherwise -> False

    stepSRV m (k, d, ip, port) = do
      i <- choose (1, 5)
      subordinates <- zipWith addTag [0 :: Int ..] <$> vectorOf i genDomainName
      (_, subs) <- head' <$> PeerSelection.genGroupSrvs [(d, subordinates)]
      let fixupPort = map relayPort subs
          lookupSequence =
            Map.fromList $
              ((d, DNS.SRV), Right fixupPort)
              : [((sub, dnsType), Left [(ip, ttl)])
                | (sub, _, _, _) <- subs]
      return $ Map.union lookupSequence m

      where
        head' (x : _xs) = x
        head' []        = error "impossible!"
        relayPort (a, b, c', _d) = (a, b, c', port)
        c = BSC.pack . show
        addTag i dom = dom <> "_" <> c k <> "_" <> c i


    stepNonSRV b (relay, ip, _port, _advAndTrust) = do
      case relay of
        RelayAccessAddress {}  -> b
        RelayAccessDomain d _p -> Map.alter fa (d, dnsType) b
        RelayAccessSRVDomain _ -> error "impossible!"
      where
        fa Nothing               = Just . Left $ [(ip, ttl)]
        fa (Just (Left ipsttls)) = Just . Left $ (ip, ttl) : ipsttls
        fa _                     = error "impossible!"

    ttl = 300

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

genDiffusionScript :: (   [TestnetRelayInfo]
                       -> TestnetRelayInfo
                       -> Gen [( HotValency
                                , WarmValency
                                , Map RelayAccessPoint (LocalRootConfig PeerTrustable))])
                   -> TestnetRelayInfos
                   -> Gen (SimArgs, DomainMapScript, [(NodeArgs, [Command])])
genDiffusionScript genLocalRootPeers
                   relays
                   = do
    ArbTxDecisionPolicy txDecisionPolicy <- arbitrary
    let simArgs = mainnetSimArgs (length relays') txDecisionPolicy
    dnsMapScript <- genDomainMapScript relays
    txs <- makeUniqueIds 0
       <$> vectorOf (length relays') (choose (10, 100) >>= \c -> vectorOf c arbitrary)
    nodesWithCommands <- mapM go (zip relays' txs)
    return (simArgs, dnsMapScript, nodesWithCommands)
  where
    relays' = unTestnetRelays relays

    makeUniqueIds :: Int -> [[Tx Int]] -> [[Tx Int]]
    makeUniqueIds _ [] = []
    makeUniqueIds i (l:ls) =
      let (r, i') = makeUniqueIds' l i
       in r : makeUniqueIds i' ls

    makeUniqueIds' :: [Tx Int] -> Int -> ([Tx Int], Int)
    makeUniqueIds' l i = ( map (\(tx, x) -> tx {getTxId = x}) (zip l [i..])
                         , i + length l + 1
                         )

    go :: (TestnetRelayInfo, [Tx Int]) -> Gen (NodeArgs, [Command])
    go (relay, txs) = do
      let otherRelays  = relay `delete` relays'
          minConnected = 3 `max` (length relays' - 1) -- ^ TODO is this ever different from 3?
                                                      -- since we generate {2,3} relays?
      localRts <- genLocalRootPeers otherRelays relay
      nodeArgs <- genNodeArgs relays' minConnected localRts relay txs
      commands <- genCommands localRts
      return (nodeArgs, commands)


-- | Multinode Diffusion Simulator Script
--
-- Tries to generate a reasonable looking network with at most 3 nodes that can
-- or can not be connected to one another. These nodes can also randomly die or
-- have their local configuration changed.
--
genNonHotDiffusionScript :: TestnetRelayInfos
                         -> Gen (SimArgs, DomainMapScript, [(NodeArgs, [Command])])
genNonHotDiffusionScript = genDiffusionScript genLocalRootPeers
  where
    -- | Generate Local Root Peers
    --
    genLocalRootPeers :: [TestnetRelayInfo]
                      -> TestnetRelayInfo
                      -> Gen [( HotValency
                              , WarmValency
                              , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
                              )]
    genLocalRootPeers others _self = flip suchThat hasUpstream $ do
      nrGroups <- chooseInt (1, 3)
      -- Remove self from local root peers
      let size = length others
          sizePerGroup = (size `div` nrGroups) + 1

      localRootConfigs <- vectorOf size arbitrary
      let relaysAdv = zipWith (\(rap, _ip, _port, _advertise) lrc ->
                                 (rap, lrc))
                              others
                              localRootConfigs
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
                    , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
                    )]
                -> Bool
    hasUpstream localRootPeers =
      or [ v > 0 && not (Map.null m)
         | (HotValency v, _, m) <- localRootPeers
         ]

-- | there's some duplication of information, but saves some silly pattern
-- matches where we don't care about the particular value of RelayAccessPoint
--
type TestnetRelayInfo = (RelayAccessPoint, IP, PortNumber, PeerAdvertise)
data TestnetRelayInfos = TestnetRelays4 { unTestnetRelays :: [TestnetRelayInfo] }
                       | TestnetRelays6 { unTestnetRelays :: [TestnetRelayInfo] }

instance Arbitrary TestnetRelayInfos where
  arbitrary = oneof [ TestnetRelays4 <$> gen PeerSelection.genIPv4
                    , TestnetRelays6 <$> gen PeerSelection.genIPv6
                    ]
    where
      uniqueIps xs =
        let ips = (\(_, _, c, _) -> c) <$> xs
        in length (nub ips) == length ips

      gen genIP = do
        i <- choose (2,3)
        (vectorOf i arbitrary >>= traverse (uncurry $ extractOrGen genIP)) `suchThat` uniqueIps

      extractOrGen genIP peerAdvertise = \case
        raa@(RelayAccessAddress ip port) -> pure (raa, ip, port, peerAdvertise)
        rad@(RelayAccessDomain _d port) -> (rad,, port, peerAdvertise) <$> genIP
        ras@(RelayAccessSRVDomain _d) -> (ras,,, peerAdvertise) <$> genIP <*> PeerSelection.genPort

  shrink = \case
    TestnetRelays4 infos -> TestnetRelays4 <$> go infos
    TestnetRelays6 infos -> TestnetRelays6 <$> go infos
    where
      go infos = [candidate
                 | candidate <- shrinkList (const []) infos
                 , length candidate >= 2
                 ]

-- | Multinode Hot Diffusion Simulator Script
--
-- Tries to generate a network with at most 2 nodes that should
-- be connected to one another. This generator tries to obtain high ratios of
-- active peers so we can test the miniprotocols that run when we have such
-- active connections. These nodes can not randomly die or have their local
-- configuration changed. Their local root peers consist of a single group.
--
genHotDiffusionScript :: TestnetRelayInfos
                      -> Gen (SimArgs, DomainMapScript, [(NodeArgs, [Command])])
genHotDiffusionScript = genDiffusionScript genLocalRootPeers
    where
      -- | Generate Local Root Peers.  This only generates 1 group
      --
      genLocalRootPeers :: [TestnetRelayInfo]
                        -> TestnetRelayInfo
                        -> Gen [( HotValency
                                , WarmValency
                                , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
                                )]
      genLocalRootPeers others _self = flip suchThat hasUpstream $ do
        localRootConfigs <- vectorOf (length others) arbitrary
        let relaysAdv = zipWith (\(rap, _ip, _port, _advertise) lrc ->
                                    (rap, lrc))
                              others
                              localRootConfigs
            relayGroupsMap = Map.fromList relaysAdv
            warmTarget     = length relaysAdv

        hotTarget <- choose (0 , warmTarget)

        return [( HotValency hotTarget
                , WarmValency warmTarget
                , relayGroupsMap
                )]

      hasUpstream :: [( HotValency
                      , WarmValency
                      , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
                      )]
                  -> Bool
      hasUpstream localRootPeers =
        or [ v > 0 && not (Map.null m)
           | (HotValency v, _, m) <- localRootPeers
           ]


instance Arbitrary DiffusionScript where
  arbitrary = (\(a,b,c) -> DiffusionScript a b c)
              <$> frequency [ (1, arbitrary >>= genNonHotDiffusionScript)
                            , (1, arbitrary >>= genHotDiffusionScript)]
  -- TODO: shrink dns map
  -- TODO: we should write more careful shrinking than recursively shrinking
  -- `DiffusionScript`!
  shrink (DiffusionScript sargs dnsScript cmds0) = shrinkCmds cmds0 ++ shrinkDns
    where
      shrinkDns =
        [DiffusionScript sargs script cmds0
        | script <-
            mapMaybe
              -- make sure `fixupDomainMapScript` didn't return something that's
              -- equal to the original `script`
              ((\dnsScript' -> if dnsScript == dnsScript' then Nothing else Just dnsScript')
               .  fixupDomainMapScript (getLast dnsScript))
              $ shrinkScriptWith (shrinkTuple shrinkMap_ shrink) dnsScript
        ]

      getLast (Script ne) = fst $ NonEmpty.last ne

      shrinkMap_ :: Ord a => Map a b -> [Map a b]
      shrinkMap_ = map Map.fromList . shrinkList (const []) . Map.toList

      shrinkTuple :: (a -> [a]) -> (b -> [b]) -> (a, b) -> [(a, b)]
      shrinkTuple f g (a, b) = [(a', b) | a' <- f a]
                            ++ [(a, b') | b' <- g b]

      shrinkCmds [] = []
      shrinkCmds ((nargs, cmds):rest) =
        let shrunkCmdss = fixupCommands <$> shrinkList shrinkCommand cmds
            rest' = shrinkCmds rest
        in [DiffusionScript sargs dnsScript ((nargs, shrunkCmds):rest)
           | shrunkCmds <- shrunkCmdss] ++ rest'
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
    | DiffusionPeerSelectionTrace (Governor.TracePeerSelection Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr)
    | DiffusionPeerSelectionActionsTrace (PeerSelectionActionsTrace NtNAddr NtNVersion)
    | DiffusionDebugPeerSelectionTrace (DebugPeerSelection Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr)
    | DiffusionConnectionManagerTrace
        (CM.Trace NtNAddr
          (ConnectionHandlerTrace NtNVersion NtNVersionData))
    | DiffusionDiffusionSimulationTrace DiffusionSimulationTrace
    | DiffusionConnectionManagerTransitionTrace
        (AbstractTransitionTrace CM.ConnStateId)
    | DiffusionInboundGovernorTransitionTrace
        (RemoteTransitionTrace NtNAddr)
    | DiffusionInboundGovernorTrace (IG.Trace NtNAddr)
    | DiffusionServerTrace (Server.Trace NtNAddr)
    | DiffusionFetchTrace (TraceFetchClientState BlockHeader)
    | DiffusionChurnModeTrace TracerChurnMode
    | DiffusionTxSubmissionInbound (TraceTxSubmissionInbound Int (Tx Int))
    | DiffusionTxSubmissionDebug (DebugSharedTxState NtNAddr Int (Tx Int))
    | DiffusionTxLogicDebug (DebugTxLogic NtNAddr Int (Tx Int))
    | DiffusionDebugTrace String
    | DiffusionDNSTrace DNSTrace
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
  nodeTracer = do
    connStateIdSupply <- atomically $ CM.newConnStateIdSupply Proxy
    -- TODO: we should use `snocket` per node, this will allow us to set up
    -- bearer info per node
    withSnocket netSimTracer defaultBearerInfo Map.empty
      $ \ntnSnocket _ ->
        withSnocket nullTracer defaultBearerInfo Map.empty
      $ \ntcSnocket _ -> do
        dnsMapVar <- fromLazyTVar <$> playTimedScript nullTracer dnsMapScript
        withAsyncAll
          (map ((\(args, commands) -> runCommand Nothing ntnSnocket ntcSnocket dnsMapVar simArgs args connStateIdSupply commands))
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
                               , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
                               )])
         -- ^ If the node is running and corresponding local root configuration
         -- TVar.
      -> Snocket m (FD m NtNAddr) NtNAddr
        -- ^ Node to node Snocket
      -> Snocket m (FD m NtCAddr) NtCAddr
        -- ^ Node to client Snocket
      -> StrictTVar m MockDNSMap
        -- ^ Map of domain map TVars to be updated in case a node changes its IP
      -> SimArgs -- ^ Simulation arguments needed in order to run a simulation
      -> NodeArgs -- ^ Simulation arguments needed in order to run a single node
      -> CM.ConnStateIdSupply m
      -> [Command] -- ^ List of commands/actions to perform for a single node
      -> m Void
    runCommand Nothing ntnSnocket ntcSnocket dnsMapVar sArgs nArgs connStateIdSupply [] = do
      threadDelay 3600
      traceWith (diffSimTracer (naAddr nArgs)) TrRunning
      runCommand Nothing ntnSnocket ntcSnocket dnsMapVar sArgs nArgs connStateIdSupply []
    runCommand (Just (_, _)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs connStateIdSupply [] = do
      -- We shouldn't block this thread waiting
      -- on the async since this will lead to a deadlock
      -- as thread returns 'Void'.
      threadDelay 3600
      traceWith (diffSimTracer (naAddr nArgs)) TrRunning
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs connStateIdSupply []
    runCommand Nothing ntnSnocket ntcSnocket dnsMapVar sArgs nArgs connStateIdSupply
               (JoinNetwork delay :cs) = do
      threadDelay delay
      traceWith (diffSimTracer (naAddr nArgs)) TrJoiningNetwork
      lrpVar <- newTVarIO $ naLocalRootPeers nArgs
      withAsync (runNode sArgs nArgs ntnSnocket ntcSnocket connStateIdSupply lrpVar dnsMapVar) $ \nodeAsync ->
        runCommand (Just (nodeAsync, lrpVar)) ntnSnocket ntcSnocket dnsMapVar sArgs nArgs connStateIdSupply cs
    runCommand _ _ _ _ _ _ _ (JoinNetwork _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async_, _)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs connStateIdSupply
               (Kill delay:cs) = do
      threadDelay delay
      traceWith (diffSimTracer (naAddr nArgs)) TrKillingNode
      cancel async_
      runCommand Nothing ntnSnocket ntcSnocket dMapVarMap sArgs nArgs connStateIdSupply cs
    runCommand _ _ _ _ _ _ _ (Kill _:_) = do
      error "runCommand: Impossible happened"
    runCommand Nothing _ _ _ _ _ _ (Reconfigure _ _:_) =
      error "runCommand: Impossible happened"
    runCommand (Just (async_, lrpVar)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs connStateIdSupply
               (Reconfigure delay newLrp:cs) = do
      threadDelay delay
      traceWith (diffSimTracer (naAddr nArgs)) TrReconfiguringNode
      _ <- atomically $ writeTVar lrpVar newLrp
      runCommand (Just (async_, lrpVar)) ntnSnocket ntcSnocket dMapVarMap sArgs nArgs connStateIdSupply
                 cs

    runNode :: SimArgs
            -> NodeArgs
            -> Snocket m (FD m NtNAddr) NtNAddr
            -> Snocket m (FD m NtCAddr) NtCAddr
            -> CM.ConnStateIdSupply m
            -> StrictTVar m [( HotValency
                             , WarmValency
                             , Map RelayAccessPoint (LocalRootConfig PeerTrustable)
                             )]
            -> StrictTVar m MockDNSMap
            -> m Void
    runNode SimArgs
            { saSlot                  = bgaSlotDuration
            , saQuota                 = quota
            , saTxDecisionPolicy      = txDecisionPolicy
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
            , naTxs                    = txs
            }
            ntnSnocket
            ntcSnocket
            connStateIdSupply
            lrpVar
            dMapVar = do
      chainSyncExitVar <- newTVarIO chainSyncExitOnBlockNo
      ledgerPeersVar <- initScript' ledgerPeers
      onlyOutboundConnectionsStateVar <- newTVarIO UntrustedState
      useBootstrapPeersScriptVar <- newTVarIO bootstrapPeers
      churnModeVar <- newTVarIO ChurnModeNormal

      let readUseBootstrapPeers = stepScriptSTM' useBootstrapPeersScriptVar
          (bgaRng, rng) = Random.split $ mkStdGen seed
          acceptedConnectionsLimit =
            Node.AcceptedConnectionsLimit maxBound maxBound 0
          diffusionMode = InitiatorAndResponderDiffusionMode
          readLocalRootPeers  = readTVar lrpVar
          readPublicRootPeers = return publicRoots
          readUseLedgerPeers  = return (UseLedgerPeers (After 0))
          acceptVersion = acceptableVersion
          defaultMiniProtocolsLimit :: MiniProtocolLimits
          defaultMiniProtocolsLimit =
            MiniProtocolLimits { maximumIngressQueue = 64000 }

          blockGeneratorArgs :: Node.BlockGeneratorArgs Block StdGen
          blockGeneratorArgs =
            Node.randomBlockGenerationArgs bgaSlotDuration
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

          limitsAndTimeouts :: Node.LimitsAndTimeouts BlockHeader Block
          limitsAndTimeouts
            = Node.LimitsAndTimeouts
                { Node.chainSyncLimits      = defaultMiniProtocolsLimit
                , Node.chainSyncSizeLimits  = byteLimitsChainSync (fromIntegral . BL.length)
                , Node.chainSyncTimeLimits  =
                    timeLimitsChainSync stdChainSyncTimeout
                , Node.blockFetchLimits     = defaultMiniProtocolsLimit
                , Node.blockFetchSizeLimits = byteLimitsBlockFetch (fromIntegral . BL.length)
                , Node.blockFetchTimeLimits = timeLimitsBlockFetch
                , Node.keepAliveLimits      = defaultMiniProtocolsLimit
                , Node.keepAliveSizeLimits  = byteLimitsKeepAlive (fromIntegral . BL.length)
                , Node.keepAliveTimeLimits  = timeLimitsKeepAlive
                , Node.pingPongLimits       = defaultMiniProtocolsLimit
                , Node.pingPongSizeLimits   = byteLimitsPingPong
                , Node.pingPongTimeLimits   = timeLimitsPingPong
                , Node.handshakeLimits      = defaultMiniProtocolsLimit
                , Node.handshakeTimeLimits  =
                    ProtocolTimeLimits (const shortWait)
                , Node.handhsakeSizeLimits  =
                    ProtocolSizeLimits (const (4 * 1440))
                                       (fromIntegral . BL.length)
                , Node.peerSharingLimits     = defaultMiniProtocolsLimit
                , Node.peerSharingTimeLimits =
                    timeLimitsPeerSharing
                , Node.peerSharingSizeLimits =
                    byteLimitsPeerSharing (fromIntegral . BL.length)
                , Node.txSubmissionLimits = defaultMiniProtocolsLimit
                , Node.txSubmissionTimeLimits = timeLimitsTxSubmission2
                , Node.txSubmissionSizeLimits = byteLimitsTxSubmission2 (fromIntegral . BL.length)
                }

          interfaces :: Node.Interfaces (Cardano.LedgerPeersConsensusInterface m) m
          interfaces =
            Node.Interfaces
              { Node.iNtnSnocket        = ntnSnocket
              , Node.iNtnBearer         = makeFDBearer
              , Node.iAcceptVersion     = acceptVersion
              , Node.iNtnDomainResolver = domainResolver dMapVar
              , Node.iNtcSnocket        = ntcSnocket
              , Node.iNtcBearer         = makeFDBearer
              , Node.iRng               = rng
              , Node.iDomainMap         = dMapVar
              , Node.iLedgerPeersConsensusInterface
                                        =
                  LedgerPeersConsensusInterface
                    (pure maxBound)
                    (do
                      ledgerPools <- stepScriptSTM' ledgerPeersVar
                      return $ Map.elems
                             $ accPoolStake
                             $ getLedgerPools
                             $ ledgerPools)
                    Cardano.LedgerPeersConsensusInterface {
                      Cardano.readFetchMode = pure (PraosFetchMode FetchModeDeadline)
                    , Cardano.getLedgerStateJudgement = pure TooOld
                    , Cardano.updateOutboundConnectionsState =
                        \a -> do
                          a' <- readTVar onlyOutboundConnectionsStateVar
                          when (a /= a') $
                            writeTVar onlyOutboundConnectionsStateVar a
                    }
              , Node.iConnStateIdSupply = connStateIdSupply
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

          cardanoChurnArgs :: Churn.ExtraArguments m
          cardanoChurnArgs =
            Churn.ExtraArguments {
              Churn.modeVar             = churnModeVar
            , Churn.genesisPeerTargets  = snd peerTargets
            , Churn.readUseBootstrap    = readUseBootstrapPeers
            , Churn.consensusMode       = consensusMode
            , Churn.tracerChurnMode     = (\s -> WithTime (Time (-1)) (WithName addr (DiffusionChurnModeTrace s)))
                                            `contramap` nodeTracer
            }

          arguments :: Node.Arguments (Churn.ExtraArguments m) PeerTrustable m
          arguments =
            Node.Arguments
              { Node.aIPAddress            = addr
              , Node.aAcceptedLimits       = acceptedConnectionsLimit
              , Node.aDiffusionMode        = diffusionMode
              , Node.aKeepAliveInterval    = 10
              , Node.aPingPongInterval     = 10
              , Node.aPeerTargets          = fst peerTargets
              , Node.aShouldChainSyncExit  = shouldChainSyncExit chainSyncExitVar
              , Node.aChainSyncEarlyExit   = chainSyncEarlyExit
              , Node.aReadLocalRootPeers   = readLocalRootPeers
              , Node.aReadPublicRootPeers  = readPublicRootPeers
              , Node.aOwnPeerSharing       = peerSharing
              , Node.aReadUseLedgerPeers   = readUseLedgerPeers
              , Node.aProtocolIdleTimeout  = 5
              , Node.aTimeWaitTimeout      = 30
              , Node.aDNSTimeoutScript     = dnsTimeout
              , Node.aDNSLookupDelayScript = dnsLookupDelay
              , Node.aDebugTracer          = (\s -> WithTime (Time (-1)) (WithName addr (DiffusionDebugTrace s)))
                                                   `contramap` nodeTracer
              , Node.aExtraChurnArgs = cardanoChurnArgs
              , Node.aTxDecisionPolicy     = txDecisionPolicy
              , Node.aTxs                  = txs
              }

          tracers = mkTracers addr

          requestPublicRootPeers' =
            requestPublicRootPeers (Diffusion.dtTracePublicRootPeersTracer tracers)
                                   readUseBootstrapPeers
                                   (pure TooOld)
                                   readPublicRootPeers

      Node.run blockGeneratorArgs
          limitsAndTimeouts
          interfaces
          arguments
          (ExtraState.empty consensusMode (NumberOfBigLedgerPeers 0))
          ExtraSizes.empty
          Cardano.cardanoPublicRootPeersAPI
          (Cardano.cardanoPeerSelectionGovernorArgs
            (Cardano.ExtraPeerSelectionActions
              (snd peerTargets)
              readUseBootstrapPeers)
          )
          Cardano.cardanoPeerSelectionStatetoCounters
          (flip Cardano.ExtraPeers Set.empty)
          requestPublicRootPeers'
          peerChurnGovernor
          tracers
          ( contramap (DiffusionFetchTrace . (\(TraceLabelPeer _ a) -> a))
          . tracerWithName addr
          . tracerWithTime
          $ nodeTracer)
          ( contramap DiffusionTxSubmissionInbound
          . tracerWithName addr
          . tracerWithTime
          $ nodeTracer)
          ( contramap DiffusionTxSubmissionDebug
          . tracerWithName addr
          . tracerWithTime
          $ nodeTracer)
          ( contramap DiffusionTxLogicDebug
          . tracerWithName addr
          . tracerWithTime
          $ nodeTracer)
        `catch` \e -> traceWith (diffSimTracer addr) (TrErrored e)
                   >> throwIO e

    domainResolver :: StrictTVar m MockDNSMap
                   -> DNSLookupType
                   -> [DomainAccessPoint]
                   -> m (Map DomainAccessPoint (Set NtNAddr))
    -- TODO: we can take into account the `LookupReqs` and return only `IPv4`
    -- / `IPv6` if so requested.  But we should make sure the connectivity graph
    -- is not severely reduced.
    domainResolver dnsMapVar _ daps = do
      dnsMap <- readTVarIO dnsMapVar
      let mapDomains :: [(DomainAccessPoint, Set NtNAddr)]
          mapDomains =
            [ ( dap
              , Set.fromList [ TestAddress (IPAddr a p) | (a, p) <- addrs ]
              )
            | dap <- daps
            , let addrs = case dap of
                    DomainAccessPoint d p -> (,p) <$> retrieveIPs dnsMap d
                    DomainSRVAccessPoint dSRV ->
                      let subordinates = dnsMap Map.! (dSRV, DNS.SRV)
                          subordinates' = fromRight (error "impossible") subordinates
                       in case listToMaybe subordinates' of
                            Just (d, _, _, p) -> (,p) <$> retrieveIPs dnsMap d
                            Nothing           -> []
            ]
      return (Map.fromListWith (<>) mapDomains)
      where
        retrieveIPs dnsMap d =
          let ipsttlsI4 = dnsMap Map.! (d, DNS.A)
              ipsttlsI4' = fromLeft (error "impossible") ipsttlsI4
              ipsttlsI6 =  dnsMap Map.! (d, DNS.AAAA)
              ipsttlsI6' = fromLeft (error "impossible") ipsttlsI6
              ipsttls = ipsttlsI4' <> ipsttlsI6'
           in fst <$> ipsttls

    diffSimTracer :: NtNAddr -> Tracer m DiffusionSimulationTrace
    diffSimTracer ntnAddr = contramap DiffusionDiffusionSimulationTrace
                          . tracerWithName ntnAddr
                          . tracerWithTime
                          $ nodeTracer

    mkTracers
      :: NtNAddr
      -> Diffusion.Tracers NtNAddr NtNVersion NtNVersionData
                           NtCAddr NtCVersion NtCVersionData
                           SomeException Cardano.ExtraState
                           Cardano.ExtraState PeerTrustable
                           (Cardano.ExtraPeers NtNAddr)
                           (Cardano.ExtraPeerSelectionSetsWithSizes NtNAddr) m
    mkTracers ntnAddr =
      Diffusion.nullTracers {
          Diffusion.dtTraceLocalRootPeersTracer  = contramap
                                                    DiffusionLocalRootPeerTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtTracePublicRootPeersTracer = contramap
                                                    DiffusionPublicRootPeerTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtTraceLedgerPeersTracer     = contramap
                                                  DiffusionLedgerPeersTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtTracePeerSelectionTracer   = contramap
                                                    DiffusionPeerSelectionTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtDebugPeerSelectionInitiatorTracer
                                                 = contramap
                                                    DiffusionDebugPeerSelectionTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtDebugPeerSelectionInitiatorResponderTracer
            = contramap DiffusionDebugPeerSelectionTrace
            . tracerWithName ntnAddr
            . tracerWithTime
            $ nodeTracer
        , Diffusion.dtTracePeerSelectionCounters = nullTracer
        , Diffusion.dtTraceChurnCounters         = nullTracer
        , Diffusion.dtPeerSelectionActionsTracer = contramap
                                                    DiffusionPeerSelectionActionsTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtConnectionManagerTracer    = contramap
                                                    DiffusionConnectionManagerTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtConnectionManagerTransitionTracer
                                                 = contramap
                                                     DiffusionConnectionManagerTransitionTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
          -- note: we have two ways getting transition trace:
          -- * through `traceTVar` installed in `newMutableConnState`
          -- * the `dtConnectionManagerTransitionTracer`
                                                 $ nodeTracer
        , Diffusion.dtServerTracer               = contramap
                                                     DiffusionServerTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtInboundGovernorTracer      = contramap
                                                     DiffusionInboundGovernorTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtInboundGovernorTransitionTracer
                                                 = contramap
                                                     DiffusionInboundGovernorTransitionTrace
                                                 . tracerWithName ntnAddr
                                                 . tracerWithTime
                                                 $ nodeTracer
        , Diffusion.dtLocalConnectionManagerTracer   = nullTracer
        , Diffusion.dtLocalServerTracer              = nullTracer
        , Diffusion.dtLocalInboundGovernorTracer     = nullTracer
        , Diffusion.dtDnsTracer                      = contramap DiffusionDNSTrace
                                                     . tracerWithName ntnAddr
                                                     . tracerWithTime
                                                     $ nodeTracer
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


sublistOf1 :: NonEmpty a -> Gen (NonEmpty a)
sublistOf1 as = do
    -- boolean mask
    msk <- vectorOf len arbitrary
    -- index for which we force `True` in the `msk`
    idx <- chooseInt (0, len - 1)
    let msk' = case splitAt (idx - 1) msk of
          (hs, _:ts) -> hs ++ (True : ts)
          _          -> error "sublistOf1: impossible happened"
    return . NonEmpty.fromList
           . fmap snd
           . NonEmpty.filter fst
           . NonEmpty.zip (NonEmpty.fromList msk')
           $ as
  where
    len = NonEmpty.length as
