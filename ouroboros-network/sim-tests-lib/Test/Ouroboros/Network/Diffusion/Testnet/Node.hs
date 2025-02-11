{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Ouroboros.Network.Diffusion.Testnet.Node
  ( -- * run a node
    Node.BlockGeneratorArgs (..)
  , Node.LimitsAndTimeouts (..)
  , Interfaces (..)
  , Arguments (..)
  , run
    -- * node types
  , NtNAddr
  , NtNFD
  , NtCAddr
  , NtCFD
    -- * extra types used by the node
  , AcceptedConnectionsLimit (..)
  , DiffusionMode (..)
  , PeerAdvertise (..)
  , PeerSelectionTargets (..)
    -- * configuration constants
  , config_REPROMOTE_DELAY
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

import Data.Foldable as Foldable (foldl')
import Data.IP (IP (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import System.Random (StdGen, split)

import Codec.CBOR.Term qualified as CBOR

import Network.DNS (Domain, TTL)

import Ouroboros.Network.Mock.Chain (Chain, toAnchoredFragment, toOldestFirst)
import Ouroboros.Network.Mock.ConcreteBlock (Block (..), BlockHeader (..),
           convertSlotToTimeForTestsAssumingNoHardFork)
import Ouroboros.Network.Mock.ProducerState (ChainProducerState (..))

import Ouroboros.Network.AnchoredFragment qualified as AF
import Ouroboros.Network.Block (MaxSlotNo (..), maxSlotNoFromWithOrigin,
           pointSlot)
import Ouroboros.Network.BlockFetch
import Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Governor (PeerSelectionState (..),
           PeerSelectionTargets (..), PublicPeerSelectionState (..))
import Ouroboros.Network.PeerSelection.PeerMetric
           (PeerMetricsConfiguration (..), newPeerMetric)
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (VersionDataCodec (..),
           noTimeLimitsHandshake, timeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned
           (unversionedHandshakeCodec, unversionedProtocolDataCodec)
import Ouroboros.Network.Protocol.Handshake.Version (Accept (Accept))
import Ouroboros.Network.RethrowPolicy (ErrorCommand (ShutdownNode),
           ioErrorRethrowPolicy, mkRethrowPolicy, muxErrorRethrowPolicy)
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (MakeBearer, Snocket, TestAddress (..),
           invalidFileDescriptor)

import Simulation.Network.Snocket (AddressType (..), FD)

import GHC.Exception (Exception)
import Ouroboros.Network.BlockFetch.ConsensusInterface
           (ChainSelStarvation (ChainSelStarvationEndedAt))
import Ouroboros.Network.ConnectionManager.State (ConnStateIdSupply)
import Ouroboros.Network.Diffusion.Common qualified as Common
import Ouroboros.Network.Diffusion.P2P qualified as P2P
import Ouroboros.Network.PeerSelection.Churn (PeerChurnArgs)
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionGovernorArgs)
import Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface, LedgerPeersKind, UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerStateActions (PeerConnectionHandle)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (DomainAccessPoint,
           RelayAccessPoint)
import Ouroboros.Network.PeerSelection.RootPeersDNS (PeerActionsDNS)
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSLookupType)
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore (DNSSemaphore)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency,
           LocalRootConfig, WarmValency)
import Ouroboros.Network.PeerSelection.Types (PublicExtraPeersAPI (..))
import Test.Ouroboros.Network.Data.Script (Script)
import Test.Ouroboros.Network.Diffusion.Node.ChainDB (addBlock,
           getBlockPointSet)
import Test.Ouroboros.Network.Diffusion.Node.Kernel (NodeKernel (..), NtCAddr,
           NtCVersion, NtCVersionData, NtNAddr, NtNVersion, NtNVersionData (..))
import Test.Ouroboros.Network.Diffusion.Node.Kernel qualified as Node
import Test.Ouroboros.Network.Diffusion.Node.MiniProtocols qualified as Node
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS (DNSLookupDelay,
           DNSTimeout, mockDNSActions)


data Interfaces extraAPI m = Interfaces
    { iNtnSnocket        :: Snocket m (NtNFD m) NtNAddr
    , iNtnBearer         :: MakeBearer m (NtNFD m)
    , iAcceptVersion     :: NtNVersionData -> NtNVersionData -> Accept NtNVersionData
    , iNtnDomainResolver :: DNSLookupType -> [DomainAccessPoint] -> m (Map DomainAccessPoint (Set NtNAddr))
    , iNtcSnocket        :: Snocket m (NtCFD m) NtCAddr
    , iNtcBearer         :: MakeBearer m (NtCFD m)
    , iRng               :: StdGen
    , iDomainMap         :: StrictTVar m (Map Domain [(IP, TTL)])
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
    -> P2P.TracersExtra NtNAddr NtNVersion NtNVersionData
                        NtCAddr NtCVersion NtCVersionData
                        ResolverException extraState extraDebugState extraFlags
                        extraPeers extraCounters m
    -> Tracer m (TraceLabelPeer NtNAddr (TraceFetchClientState BlockHeader))
    -> m Void
run blockGeneratorArgs limits ni na
    emptyExtraState emptyExtraCounters
    extraPeersAPI psArgs psToExtraCounters
    toExtraPeers requestPublicRootPeers peerChurnGovernor
    tracersExtra tracerBlockFetch =
    Node.withNodeKernelThread blockGeneratorArgs
      $ \ nodeKernel nodeKernelThread -> do
        dnsTimeoutScriptVar <- newTVarIO (aDNSTimeoutScript na)
        dnsLookupDelayScriptVar <- newTVarIO (aDNSLookupDelayScript na)
        peerMetrics <- newPeerMetric PeerMetricsConfiguration { maxEntriesToTrack = 180 }

        let -- diffusion interfaces
            interfaces :: P2P.Interfaces (NtNFD m) NtNAddr NtNVersion NtNVersionData
                                            (NtCFD m) NtCAddr NtCVersion NtCVersionData
                                            resolver ResolverException extraState extraFlags extraPeers extraAPI m
            interfaces = P2P.Interfaces
              { P2P.diNtnSnocket            = iNtnSnocket ni
              , P2P.diNtnBearer             = iNtnBearer ni
              , P2P.diNtnConfigureSocket    = \_ _ -> return ()
              , P2P.diNtnConfigureSystemdSocket
                                               = \_ _ -> return ()
              , P2P.diNtnHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer  = nullTracer
                    , haHandshakeCodec   = unversionedHandshakeCodec
                    , haVersionDataCodec = ntnUnversionedDataCodec
                    , haAcceptVersion    = iAcceptVersion ni
                    , haQueryVersion     = const False
                    , haTimeLimits       = timeLimitsHandshake
                    }
              , P2P.diNtnAddressType = ntnAddressType
              , P2P.diNtnDataFlow    = \NtNVersionData { ntnDiffusionMode } ->
                  case ntnDiffusionMode of
                    InitiatorOnlyDiffusionMode         -> Unidirectional
                    InitiatorAndResponderDiffusionMode -> Duplex
              , P2P.diNtnPeerSharing        = ntnPeerSharing
              , P2P.diNtnToPeerAddr         = \a b -> TestAddress (Node.IPAddr a b)
              , P2P.diNtcSnocket            = iNtcSnocket ni
              , P2P.diNtcBearer             = iNtcBearer ni
              , P2P.diNtcHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer  = nullTracer
                    , haHandshakeCodec   = unversionedHandshakeCodec
                    , haVersionDataCodec = unversionedProtocolDataCodec
                    , haAcceptVersion    = \_ v -> Accept v
                    , haQueryVersion     = const False
                    , haTimeLimits       = noTimeLimitsHandshake
                    }
              , P2P.diNtcGetFileDescriptor  = \_ -> pure invalidFileDescriptor
              , P2P.diRng                   = diffStgGen
              , P2P.diInstallSigUSR1Handler = \_ _ _ -> pure ()
              , P2P.diDnsActions            = const (mockDNSActions
                                                       (iDomainMap ni)
                                                       dnsTimeoutScriptVar
                                                       dnsLookupDelayScriptVar)
              , P2P.diUpdateVersionData     = \versionData diffusionMode ->
                                                    versionData { ntnDiffusionMode = diffusionMode }
              , P2P.diConnStateIdSupply     = iConnStateIdSupply ni
              }

            appsExtra :: P2P.ApplicationsExtra NtNAddr m ()
            appsExtra = P2P.ApplicationsExtra
              { -- TODO: simulation errors should be critical
                P2P.daRethrowPolicy     =
                     muxErrorRethrowPolicy
                  <> ioErrorRethrowPolicy

                -- we are not using local connections, so we can make all the
                -- errors fatal.
              , P2P.daLocalRethrowPolicy =
                     mkRethrowPolicy
                       (\ _ (_ :: SomeException) -> ShutdownNode)
              , P2P.daPeerMetrics         = peerMetrics
                -- fetch mode is not used (no block-fetch mini-protocol)
              , P2P.daReturnPolicy        = \_ -> config_REPROMOTE_DELAY
              , P2P.daPeerSharingRegistry = nkPeerSharingRegistry nodeKernel
              }

        let apps = Node.applications (aDebugTracer na) nodeKernel Node.cborCodecs limits appArgs blockHeader

        withAsync
           (P2P.runM interfaces
                     Common.nullTracers
                     tracersExtra
                     (mkArgs (nkPublicPeerSelectionVar nodeKernel))
                     argsExtra apps appsExtra)
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
           -> Common.Arguments m (NtNFD m) NtNAddr (NtCFD m) NtCAddr
    mkArgs daPublicPeerSelectionVar = Common.Arguments
      { Common.daIPv4Address   = Right <$> (ntnToIPv4 . aIPAddress) na
      , Common.daIPv6Address   = Right <$> (ntnToIPv6 . aIPAddress) na
      , Common.daLocalAddress  = Nothing
      , Common.daAcceptedConnectionsLimit
                             = aAcceptedLimits na
      , Common.daMode          = aDiffusionMode na
      , Common.daPublicPeerSelectionVar
      }

    argsExtra :: P2P.ArgumentsExtra
                   extraState extraDebugState
                   extraFlags extraPeers extraAPI
                   extraChurnArgs extraCounters exception
                   NtNAddr resolver resolverError m
    argsExtra = P2P.ArgumentsExtra
      { P2P.daPeerSelectionTargets   = aPeerTargets na
      , P2P.daReadLocalRootPeers     = aReadLocalRootPeers na
      , P2P.daReadPublicRootPeers    = aReadPublicRootPeers na
      , P2P.daOwnPeerSharing         = aOwnPeerSharing na
      , P2P.daReadUseLedgerPeers     = aReadUseLedgerPeers na
      , P2P.daProtocolIdleTimeout    = aProtocolIdleTimeout na
      , P2P.daTimeWaitTimeout        = aTimeWaitTimeout na
      , P2P.daDeadlineChurnInterval  = 3300
      , P2P.daBulkChurnInterval      = 300
      , P2P.daReadLedgerPeerSnapshot = pure Nothing -- ^ tested independently
      , P2P.daEmptyExtraState        = emptyExtraState
      , P2P.daEmptyExtraCounters     = emptyExtraCounters
      , P2P.daExtraPeersAPI          = extraPeersAPI
      , P2P.daExtraChurnArgs         = aExtraChurnArgs na
      , P2P.daToExtraPeers           = toExtraPeers
      , P2P.daRequestPublicRootPeers = Just requestPublicRootPeers
      , P2P.daPeerChurnGovernor      = peerChurnGovernor
      , P2P.daPeerSelectionGovernorArgs         = psArgs
      , P2P.daPeerSelectionStateToExtraCounters = psToExtraCounters
      }

    appArgs :: Node.AppArgs extraAPI BlockHeader Block m
    appArgs = Node.AppArgs
      { Node.aaLedgerPeersConsensusInterface
                                   = iLedgerPeersConsensusInterface ni
      , Node.aaKeepAliveStdGen     = keepAliveStdGen
      , Node.aaDiffusionMode       = aDiffusionMode na
      , Node.aaKeepAliveInterval   = aKeepAliveInterval na
      , Node.aaPingPongInterval    = aPingPongInterval na
      , Node.aaShouldChainSyncExit = aShouldChainSyncExit na
      , Node.aaChainSyncEarlyExit  = aChainSyncEarlyExit na
      , Node.aaOwnPeerSharing      = aOwnPeerSharing na
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
