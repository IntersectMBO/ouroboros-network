{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Ouroboros.Network.Diffusion.Node
  ( -- * run a node
    Interfaces (..)
  , Arguments (..)
  , run
    -- * node types
  , NtNAddr
  , NtNFD
  , NtNVersion
  , NtNVersionData
  , NtCAddr
  , NtCFD
  , NtCVersion
  , NtCVersionData
  , Node.NtNAddr_ (..)
    -- * extra types used by the node
  , AcceptedConnectionsLimit (..)
  , DiffusionMode (..)
  , PeerAdvertise (..)
  , PeerSelectionTargets (..)
    -- * configuration constants
  , config_REPROMOTE_DELAY
    -- * re-exports
  , Node.BlockGeneratorArgs (..)
  , Node.LimitsAndTimeouts (..)
  , Node.randomBlockGenerationArgs
  , Node.ntnAddrToRelayAccessPoint
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad ((>=>))
import Control.Monad.Class.MonadAsync (MonadAsync (wait, withAsync))
import Control.Monad.Class.MonadFork (MonadFork)
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadEvaluate, MonadMask, MonadThrow,
           SomeException)
import Control.Monad.Class.MonadTime.SI (DiffTime, MonadTime, Time (..))
import Control.Monad.Class.MonadTimer.SI (MonadDelay, MonadTimer)
import Control.Monad.Fix (MonadFix)
import Control.Tracer (Tracer (..), nullTracer)

import Codec.CBOR.Term qualified as CBOR
import Data.Foldable as Foldable (foldl')
import Data.IP (IP (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Exception (Exception)
import Network.DNS (Domain, TYPE)
import System.Random (StdGen, split)

import Ouroboros.Network.Mux (noBindForkPolicy)
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (VersionDataCodec (..),
           noTimeLimitsHandshake, timeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned
           (unversionedHandshakeCodec, unversionedProtocolDataCodec)
import Ouroboros.Network.Protocol.Handshake.Version (Accept (Accept))

import Ouroboros.Network.AnchoredFragment qualified as AF
import Ouroboros.Network.Block (MaxSlotNo (..), maxSlotNoFromWithOrigin,
           pointSlot)
import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.ConsensusInterface
           (ChainSelStarvation (ChainSelStarvationEndedAt))
import Ouroboros.Network.ConnectionManager.State (ConnStateIdSupply)
import Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import Ouroboros.Network.Diffusion qualified as Diff
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.Mock.Chain (Chain, toAnchoredFragment, toOldestFirst)
import Ouroboros.Network.Mock.ConcreteBlock (Block (..), BlockHeader (..),
           convertSlotToTimeForTestsAssumingNoHardFork)
import Ouroboros.Network.Mock.ProducerState (ChainProducerState (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Churn (PeerChurnArgs)
import Ouroboros.Network.PeerSelection.Governor (PeerSelectionState (..),
           PeerSelectionTargets (..), PublicPeerSelectionState (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionGovernorArgs)
import Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface, LedgerPeersKind, UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics,
           PeerMetricsConfiguration (..), newPeerMetric)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerStateActions (PeerConnectionHandle)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import Ouroboros.Network.PeerSelection.RootPeersDNS (DNSLookupType (..),
           DNSSemaphore, PeerActionsDNS)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency,
           LocalRootConfig, WarmValency)
import Ouroboros.Network.PeerSelection.Types (PublicExtraPeersAPI (..))
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (MakeBearer, Snocket, TestAddress (..),
           invalidFileDescriptor)

import Simulation.Network.Snocket (AddressType (..), FD)

import Test.Ouroboros.Network.Data.Script (Script)
import Test.Ouroboros.Network.Diffusion.Node.ChainDB (addBlock,
           getBlockPointSet)
import Test.Ouroboros.Network.Diffusion.Node.Kernel (NodeKernel (..), NtCAddr,
           NtCVersion, NtCVersionData, NtNAddr, NtNVersion, NtNVersionData (..))
import Test.Ouroboros.Network.Diffusion.Node.Kernel qualified as Node
import Test.Ouroboros.Network.Diffusion.Node.MiniProtocols qualified as Node
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS (DNSLookupDelay,
           DNSTimeout, DomainAccessPoint (..), MockDNSLookupResult,
           mockDNSActions)


data Interfaces extraAPI m = Interfaces
    { iNtnSnocket        :: Snocket m (NtNFD m) NtNAddr
    , iNtnBearer         :: MakeBearer m (NtNFD m)
    , iAcceptVersion     :: NtNVersionData -> NtNVersionData -> Accept NtNVersionData
    , iNtnDomainResolver :: DNSLookupType -> [DomainAccessPoint] -> m (Map DomainAccessPoint (Set NtNAddr))
    , iNtcSnocket        :: Snocket m (NtCFD m) NtCAddr
    , iNtcBearer         :: MakeBearer m (NtCFD m)
    , iRng               :: StdGen
    , iDomainMap         :: StrictTVar m (Map (Domain, TYPE) MockDNSLookupResult)
    , iLedgerPeersConsensusInterface
                         :: LedgerPeersConsensusInterface extraAPI m
    , iConnStateIdSupply :: ConnStateIdSupply m
    }

type NtNFD m = FD m NtNAddr
type NtCFD m = FD m NtCAddr

data Arguments extraChurnArgs extraFlags m = Arguments
    { aIPAddress            :: NtNAddr
    , aAcceptedLimits       :: AcceptedConnectionsLimit
    , aDiffusionMode        :: DiffusionMode
    , aKeepAliveInterval    :: DiffTime
    , aPingPongInterval     :: DiffTime
    , aShouldChainSyncExit  :: BlockHeader -> m Bool
    , aChainSyncEarlyExit   :: Bool

    , aPeerTargets          :: PeerSelectionTargets
    , aReadLocalRootPeers   :: STM m [( HotValency
                                      , WarmValency
                                      , Map RelayAccessPoint (LocalRootConfig extraFlags))]
    , aReadPublicRootPeers  :: STM m (Map RelayAccessPoint PeerAdvertise)
    , aOwnPeerSharing       :: PeerSharing
    , aReadUseLedgerPeers   :: STM m UseLedgerPeers
    , aProtocolIdleTimeout  :: DiffTime
    , aTimeWaitTimeout      :: DiffTime
    , aDNSTimeoutScript     :: Script DNSTimeout
    , aDNSLookupDelayScript :: Script DNSLookupDelay
    , aDebugTracer          :: Tracer m String
    , aExtraChurnArgs       :: extraChurnArgs
    }

-- The 'mockDNSActions' is not using \/ specifying 'resolverException', thus we
-- set it to 'SomeException'.
--
type ResolverException = SomeException

run :: forall extraState extraDebugState extraAPI
             extraPeers extraFlags extraChurnArgs extraCounters
             exception resolver resolverError m.
       ( Alternative (STM m)
       , MonadAsync       m
       , MonadDelay       m
       , MonadEvaluate    m
       , MonadFix         m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadTraceSTM    m
       , MonadMask        m
       , MonadSay         m
       , MonadST          m
       , MonadTime        m
       , MonadTimer       m
       , MonadThrow       (STM m)
       , MonadMVar        m

       , Eq extraFlags
       , Eq extraCounters
       , Monoid extraPeers
       , Exception exception

       , resolver ~ ()
       , resolverError ~ ResolverException
       , forall a. Semigroup a => Semigroup (m a)
       )
    => Node.BlockGeneratorArgs Block StdGen
    -> Node.LimitsAndTimeouts BlockHeader Block
    -> Interfaces extraAPI m
    -> Arguments extraChurnArgs extraFlags m
    -> extraState
    -> extraCounters
    -> PublicExtraPeersAPI extraPeers NtNAddr
    -> (forall muxMode responderCtx ntnVersionData bytes a b .
        PeerSelectionGovernorArgs
          extraState
          extraDebugState
          extraFlags
          extraPeers
          extraAPI
          extraCounters
          NtNAddr
          (PeerConnectionHandle
             muxMode responderCtx NtNAddr ntnVersionData bytes m a b)
          exception
          m)
    -> (forall muxMode responderCtx ntnVersionData bytes a b.
        PeerSelectionState
          extraState
          extraFlags
          extraPeers
          NtNAddr
          (PeerConnectionHandle
             muxMode responderCtx NtNAddr ntnVersionData bytes m a b)
        -> extraCounters)
    -> (Map NtNAddr PeerAdvertise -> extraPeers)
    -> (   PeerActionsDNS NtNAddr resolver resolverError m
        -> DNSSemaphore m
        -> (Map NtNAddr PeerAdvertise -> extraPeers)
        -> (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set NtNAddr, DiffTime)))
        -> LedgerPeersKind
        -> StdGen
        -> Int
        -> m (PublicRootPeers extraPeers NtNAddr, DiffTime))
    -> (PeerChurnArgs
             m
             extraChurnArgs
             extraDebugState
             extraFlags
             extraPeers
             extraAPI
             extraCounters
             NtNAddr
        -> m Void)
    -> Diff.Tracers NtNAddr NtNVersion NtNVersionData
                    NtCAddr NtCVersion NtCVersionData
                    ResolverException extraState extraDebugState extraFlags
                    extraPeers extraCounters m
    -> Tracer m (TraceLabelPeer NtNAddr (TraceFetchClientState BlockHeader))
    -> m Void
run blockGeneratorArgs limits ni na
    emptyExtraState emptyExtraCounters
    extraPeersAPI psArgs psToExtraCounters
    toExtraPeers requestPublicRootPeers peerChurnGovernor
    tracers tracerBlockFetch =
    Node.withNodeKernelThread blockGeneratorArgs
      $ \ nodeKernel nodeKernelThread -> do
        dnsTimeoutScriptVar <- newTVarIO (aDNSTimeoutScript na)
        dnsLookupDelayScriptVar <- newTVarIO (aDNSLookupDelayScript na)
        peerMetrics <- newPeerMetric PeerMetricsConfiguration { maxEntriesToTrack = 180 }

        let -- diffusion interfaces
            interfaces :: Diff.Interfaces (NtNFD m) NtNAddr NtNVersion NtNVersionData
                                          (NtCFD m) NtCAddr NtCVersion NtCVersionData
                                          resolver ResolverException extraState extraFlags extraPeers extraAPI m
            interfaces = Diff.Interfaces
              { Diff.diNtnSnocket            = iNtnSnocket ni
              , Diff.diNtnBearer             = iNtnBearer ni
              , Diff.diNtnConfigureSocket    = \_ _ -> return ()
              , Diff.diNtnConfigureSystemdSocket
                                               = \_ _ -> return ()
              , Diff.diNtnHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer  = nullTracer
                    , haHandshakeCodec   = unversionedHandshakeCodec
                    , haVersionDataCodec = ntnUnversionedDataCodec
                    , haAcceptVersion    = iAcceptVersion ni
                    , haQueryVersion     = const False
                    , haTimeLimits       = timeLimitsHandshake
                    }
              , Diff.diNtnAddressType = ntnAddressType
              , Diff.diNtnDataFlow    = \NtNVersionData { ntnDiffusionMode } ->
                  case ntnDiffusionMode of
                    InitiatorOnlyDiffusionMode         -> Unidirectional
                    InitiatorAndResponderDiffusionMode -> Duplex
              , Diff.diNtnPeerSharing        = ntnPeerSharing
              , Diff.diNtnToPeerAddr         = \a b -> TestAddress (Node.IPAddr a b)
              , Diff.diNtcSnocket            = iNtcSnocket ni
              , Diff.diNtcBearer             = iNtcBearer ni
              , Diff.diNtcHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer  = nullTracer
                    , haHandshakeCodec   = unversionedHandshakeCodec
                    , haVersionDataCodec = unversionedProtocolDataCodec
                    , haAcceptVersion    = \_ v -> Accept v
                    , haQueryVersion     = const False
                    , haTimeLimits       = noTimeLimitsHandshake
                    }
              , Diff.diNtcGetFileDescriptor  = \_ -> pure invalidFileDescriptor
              , Diff.diRng                   = diffStgGen
              , Diff.diInstallSigUSR1Handler = \_ _ _ -> pure ()
              , Diff.diDnsActions            = \tracer lookupType toPeerAddr ->
                  mockDNSActions
                    tracer
                    lookupType
                    toPeerAddr
                    (iDomainMap ni)
                    dnsTimeoutScriptVar
                    dnsLookupDelayScriptVar
              , Diff.diUpdateVersionData     = \versionData diffusionMode ->
                                                    versionData { ntnDiffusionMode = diffusionMode }
              , Diff.diConnStateIdSupply     = iConnStateIdSupply ni
              }

            apps = Node.applications
                     (aDebugTracer na)
                     nodeKernel
                     Node.cborCodecs
                     limits
                     (appArgs peerMetrics)
                     blockHeader

        withAsync
           (Diff.runM interfaces
                      tracers
                      (mkArgs (nkPublicPeerSelectionVar nodeKernel))
                      apps)
           $ \ diffusionThread ->
               withAsync (blockFetch nodeKernel) $ \blockFetchLogicThread ->
                 wait diffusionThread
              <> wait blockFetchLogicThread
              <> wait nodeKernelThread
  where
    blockFetch :: NodeKernel BlockHeader Block s m
               -> m Void
    blockFetch nodeKernel = do
      blockFetchLogic
        nullTracer
        tracerBlockFetch
        (blockFetchPolicy nodeKernel)
        (nkFetchClientRegistry nodeKernel)
        (BlockFetchConfiguration {
          bfcMaxConcurrencyBulkSync = 1,
          bfcMaxConcurrencyDeadline = 2,
          bfcMaxRequestsInflight    = 10,
          bfcDecisionLoopIntervalGenesis = 0.04,
          bfcDecisionLoopIntervalPraos = 0.01,
          bfcGenesisBFConfig        = GenesisBlockFetchConfiguration
            { gbfcGracePeriod = 10 },  -- second
          bfcSalt                   = 0
        })

    blockFetchPolicy :: NodeKernel BlockHeader Block s m
                     -> BlockFetchConsensusInterface NtNAddr BlockHeader Block m
    blockFetchPolicy nodeKernel =
        BlockFetchConsensusInterface {
          readCandidateChains    = readTVar (nkClientChains nodeKernel)
                                   >>= traverse (readTVar
                                       >=> (return . toAnchoredFragment)),
          readCurrentChain       = readTVar (nkChainProducerState nodeKernel)
                                   >>= (return . toAnchoredFragmentHeader . chainState),
          readFetchMode          = return $ PraosFetchMode FetchModeBulkSync,
          readFetchedBlocks      = flip Set.member <$> getBlockPointSet (nkChainDB nodeKernel),
          readFetchedMaxSlotNo   = Foldable.foldl' max NoMaxSlotNo .
                                   map (maxSlotNoFromWithOrigin . pointSlot) .
                                   Set.elems <$>
                                   getBlockPointSet (nkChainDB nodeKernel),
          mkAddFetchedBlock        =
              pure $ \_p b ->
                atomically (addBlock b (nkChainDB nodeKernel)),

          plausibleCandidateChain,
          compareCandidateChains,

          blockFetchSize         = \_ -> 1000,
          blockMatchesHeader     = \_ _ -> True,

          headerForgeUTCTime,

          readChainSelStarvation       = pure (ChainSelStarvationEndedAt (Time 0)),
          demoteChainSyncJumpingDynamo = \_ -> pure ()
        }
      where
        plausibleCandidateChain cur candidate =
            AF.headBlockNo candidate > AF.headBlockNo cur

        headerForgeUTCTime (FromConsensus hdr) =
            pure $
            convertSlotToTimeForTestsAssumingNoHardFork (headerSlot hdr)

        compareCandidateChains c1 c2 =
          AF.headBlockNo c1 `compare` AF.headBlockNo c2

        -- | Convert a 'Chain' to an 'AnchoredFragment' with an header.
        --
        -- The anchor of the fragment will be 'Chain.genesisPoint'.
        toAnchoredFragmentHeader :: Chain Block -> AF.AnchoredFragment BlockHeader
        toAnchoredFragmentHeader = AF.fromOldestFirst AF.AnchorGenesis
                                 . map blockHeader
                                 . toOldestFirst

    ntnAddressType :: NtNAddr -> Maybe AddressType
    ntnAddressType (TestAddress (Node.EphemeralIPv4Addr _)) = Just IPv4Address
    ntnAddressType (TestAddress (Node.EphemeralIPv6Addr _)) = Just IPv6Address
    ntnAddressType (TestAddress (Node.IPAddr (IPv4 _) _))   = Just IPv4Address
    ntnAddressType (TestAddress (Node.IPAddr (IPv6 _) _))   = Just IPv6Address

    -- various pseudo random generators
    (diffStgGen, keepAliveStdGen) = split (iRng ni)

    ntnUnversionedDataCodec :: VersionDataCodec CBOR.Term NtNVersion NtNVersionData
    ntnUnversionedDataCodec = VersionDataCodec { encodeData, decodeData }
      where
        encodeData _ NtNVersionData { ntnDiffusionMode, ntnPeerSharing } =
          let peerSharing = case ntnPeerSharing of
                PeerSharingDisabled -> 0
                PeerSharingEnabled  -> 1
           in case ntnDiffusionMode of
             InitiatorOnlyDiffusionMode         ->
               CBOR.TList [CBOR.TBool False, CBOR.TInt peerSharing]
             InitiatorAndResponderDiffusionMode ->
               CBOR.TList [CBOR.TBool True, CBOR.TInt peerSharing]

        toPeerSharing :: Int -> Either Text PeerSharing
        toPeerSharing 0 = Right PeerSharingDisabled
        toPeerSharing 1 = Right PeerSharingEnabled
        toPeerSharing _ = Left "toPeerSharing: out of bounds"

        decodeData _ (CBOR.TList [CBOR.TBool False, CBOR.TInt a]) = NtNVersionData InitiatorOnlyDiffusionMode <$> (toPeerSharing a)
        decodeData _ (CBOR.TList [CBOR.TBool True, CBOR.TInt a])  = NtNVersionData InitiatorAndResponderDiffusionMode <$> (toPeerSharing a)
        decodeData _ _                                            = Left (Text.pack "unversionedDataCodec: unexpected term")

    mkArgs :: StrictTVar m (PublicPeerSelectionState NtNAddr)
           -> Diff.Arguments
                   extraState extraDebugState
                   extraFlags extraPeers extraAPI
                   extraChurnArgs extraCounters exception
                   resolver resolverError
                   m (NtNFD m) NtNAddr (NtCFD m) NtCAddr
    mkArgs daPublicPeerSelectionVar = Diff.Arguments
      { Diff.daIPv4Address   = Right <$> (ntnToIPv4 . aIPAddress) na
      , Diff.daIPv6Address   = Right <$> (ntnToIPv6 . aIPAddress) na
      , Diff.daLocalAddress  = Nothing
      , Diff.daAcceptedConnectionsLimit
                             = aAcceptedLimits na
      , Diff.daMode          = aDiffusionMode na
      , Diff.daPublicPeerSelectionVar
      , Diff.daPeerSelectionTargets   = aPeerTargets na
      , Diff.daReadLocalRootPeers     = aReadLocalRootPeers na
      , Diff.daReadPublicRootPeers    = aReadPublicRootPeers na
      , Diff.daOwnPeerSharing         = aOwnPeerSharing na
      , Diff.daReadUseLedgerPeers     = aReadUseLedgerPeers na
      , Diff.daProtocolIdleTimeout    = aProtocolIdleTimeout na
      , Diff.daTimeWaitTimeout        = aTimeWaitTimeout na
      , Diff.daDeadlineChurnInterval  = 3300
      , Diff.daBulkChurnInterval      = 300
      , Diff.daReadLedgerPeerSnapshot = pure Nothing -- ^ tested independently
      , Diff.daEmptyExtraState        = emptyExtraState
      , Diff.daEmptyExtraCounters     = emptyExtraCounters
      , Diff.daExtraPeersAPI          = extraPeersAPI
      , Diff.daExtraChurnArgs         = aExtraChurnArgs na
      , Diff.daToExtraPeers           = toExtraPeers
      , Diff.daRequestPublicRootPeers = Just requestPublicRootPeers
      , Diff.daPeerChurnGovernor      = peerChurnGovernor
      , Diff.daPeerSelectionGovernorArgs         = psArgs
      , Diff.daPeerSelectionStateToExtraCounters = psToExtraCounters
      , Diff.daMuxForkPolicy          = noBindForkPolicy
      , Diff.daLocalMuxForkPolicy     = noBindForkPolicy
      }

    appArgs :: PeerMetrics m NtNAddr
            -> Node.AppArgs extraAPI BlockHeader Block m
    appArgs peerMetrics = Node.AppArgs
      { Node.aaLedgerPeersConsensusInterface
                                   = iLedgerPeersConsensusInterface ni
      , Node.aaKeepAliveStdGen     = keepAliveStdGen
      , Node.aaDiffusionMode       = aDiffusionMode na
      , Node.aaKeepAliveInterval   = aKeepAliveInterval na
      , Node.aaPingPongInterval    = aPingPongInterval na
      , Node.aaShouldChainSyncExit = aShouldChainSyncExit na
      , Node.aaChainSyncEarlyExit  = aChainSyncEarlyExit na
      , Node.aaOwnPeerSharing      = aOwnPeerSharing na
      , Node.aaPeerMetrics         = peerMetrics
      }

--- Utils

ntnToIPv4 :: NtNAddr -> Maybe NtNAddr
ntnToIPv4 ntnAddr@(TestAddress (Node.EphemeralIPv4Addr _)) = Just ntnAddr
ntnToIPv4 ntnAddr@(TestAddress (Node.IPAddr (IPv4 _) _))   = Just ntnAddr
ntnToIPv4 (TestAddress _)                                  = Nothing

ntnToIPv6 :: NtNAddr -> Maybe NtNAddr
ntnToIPv6 ntnAddr@(TestAddress (Node.EphemeralIPv6Addr _)) = Just ntnAddr
ntnToIPv6 ntnAddr@(TestAddress (Node.IPAddr (IPv6 _) _))   = Just ntnAddr
ntnToIPv6 (TestAddress _)                                  = Nothing

--
-- Constants
--

config_REPROMOTE_DELAY :: RepromoteDelay
config_REPROMOTE_DELAY = 10
