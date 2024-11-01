{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Ouroboros.Network.Testnet.Node
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
    -- * configuration
  , Node.config_REPROMOTE_DELAY
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
import Control.Monad.Class.MonadTime.SI (DiffTime, MonadTime)
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
import Ouroboros.Network.ConsensusMode
import Ouroboros.Network.Diffusion qualified as Diff
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Governor (ConsensusModePeerTargets,
           PeerSelectionTargets (..), PublicPeerSelectionState (..))
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics,
           PeerMetricsConfiguration (..), newPeerMetric)
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (VersionDataCodec (..),
           noTimeLimitsHandshake, timeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned
           (unversionedHandshakeCodec, unversionedProtocolDataCodec)
import Ouroboros.Network.Protocol.Handshake.Version (Accept (Accept))
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (MakeBearer, Snocket, TestAddress (..),
           invalidFileDescriptor)

import Ouroboros.Network.Testing.Data.Script (Script (..), stepScriptSTM')

import Simulation.Network.Snocket (AddressType (..), FD)

import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface,
           MinBigLedgerPeersForTrustedState (..), UseLedgerPeers)
import Ouroboros.Network.PeerSelection.LocalRootPeers (OutboundConnectionsState)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (DomainAccessPoint,
           RelayAccessPoint)
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSLookupType)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency,
           WarmValency)
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS (DNSLookupDelay,
           DNSTimeout, mockDNSActions)
import Test.Ouroboros.Network.Testnet.Node.ChainDB (addBlock, getBlockPointSet)
import Test.Ouroboros.Network.Testnet.Node.Kernel (NodeKernel (..), NtCAddr,
           NtCVersion, NtCVersionData, NtNAddr, NtNVersion, NtNVersionData (..))
import Test.Ouroboros.Network.Testnet.Node.Kernel qualified as Node
import Test.Ouroboros.Network.Testnet.Node.MiniProtocols qualified as Node


data Interfaces m = Interfaces
    { iNtnSnocket        :: Snocket m (NtNFD m) NtNAddr
    , iNtnBearer         :: MakeBearer m (NtNFD m)
    , iAcceptVersion     :: NtNVersionData -> NtNVersionData -> Accept NtNVersionData
    , iNtnDomainResolver :: DNSLookupType -> [DomainAccessPoint] -> m (Map DomainAccessPoint (Set NtNAddr))
    , iNtcSnocket        :: Snocket m (NtCFD m) NtCAddr
    , iNtcBearer         :: MakeBearer m (NtCFD m)
    , iRng               :: StdGen
    , iDomainMap         :: StrictTVar m (Map Domain [(IP, TTL)])
    , iLedgerPeersConsensusInterface
                         :: LedgerPeersConsensusInterface m
    , iUpdateOutboundConnectionsState
                         :: OutboundConnectionsState -> STM m ()
    }

type NtNFD m = FD m NtNAddr
type NtCFD m = FD m NtCAddr

data Arguments m = Arguments
    { aIPAddress            :: NtNAddr
    , aAcceptedLimits       :: AcceptedConnectionsLimit
    , aDiffusionMode        :: DiffusionMode
    , aKeepAliveInterval    :: DiffTime
    , aPingPongInterval     :: DiffTime
    , aShouldChainSyncExit  :: BlockHeader -> m Bool
    , aChainSyncEarlyExit   :: Bool

    , aPeerTargets          :: ConsensusModePeerTargets
    , aReadLocalRootPeers   :: STM m [( HotValency
                                      , WarmValency
                                      , Map RelayAccessPoint ( PeerAdvertise
                                                             , PeerTrustable))]
    , aReadPublicRootPeers  :: STM m (Map RelayAccessPoint PeerAdvertise)
    , aReadUseBootstrapPeers :: Script UseBootstrapPeers
    , aConsensusMode        :: ConsensusMode
    , aOwnPeerSharing       :: PeerSharing
    , aReadUseLedgerPeers   :: STM m UseLedgerPeers
    , aProtocolIdleTimeout  :: DiffTime
    , aTimeWaitTimeout      :: DiffTime
    , aDNSTimeoutScript     :: Script DNSTimeout
    , aDNSLookupDelayScript :: Script DNSLookupDelay
    , aDebugTracer          :: Tracer m String
    }

-- The 'mockDNSActions' is not using \/ specifying 'resolverException', thus we
-- set it to 'SomeException'.
--
type ResolverException = SomeException

run :: forall resolver m.
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

       , resolver ~ ()
       , forall a. Semigroup a => Semigroup (m a)
       )
    => Node.BlockGeneratorArgs Block StdGen
    -> Node.LimitsAndTimeouts BlockHeader Block
    -> Interfaces m
    -> Arguments m
    -> Diff.Tracers NtNAddr NtNVersion NtNVersionData
                    NtCAddr NtCVersion NtCVersionData
                    ResolverException m
    -> Tracer m (TraceLabelPeer NtNAddr (TraceFetchClientState BlockHeader))
    -> m Void
run blockGeneratorArgs limits ni na tracers tracerBlockFetch =
    Node.withNodeKernelThread blockGeneratorArgs
      $ \ nodeKernel nodeKernelThread -> do
        dnsTimeoutScriptVar <- newTVarIO (aDNSTimeoutScript na)
        dnsLookupDelayScriptVar <- newTVarIO (aDNSLookupDelayScript na)
        useBootstrapPeersScriptVar <- newTVarIO (aReadUseBootstrapPeers na)
        peerMetrics <- newPeerMetric PeerMetricsConfiguration { maxEntriesToTrack = 180 }

        let -- diffusion interfaces
            interfaces :: Diff.Interfaces (NtNFD m) NtNAddr NtNVersion NtNVersionData
                                          (NtCFD m) NtCAddr NtCVersion NtCVersionData
                                          resolver ResolverException
                                          m
            interfaces = Diff.Interfaces
              { Diff.diNtnSnocket            = iNtnSnocket ni
              , Diff.diNtnBearer             = iNtnBearer ni
              , Diff.diNtnConfigureSocket    = \_ _ -> return ()
              , Diff.diNtnConfigureSystemdSocket
                                                 = \_ _ -> return ()
              , Diff.diNtnHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer      = nullTracer
                    , haHandshakeCodec       = unversionedHandshakeCodec
                    , haVersionDataCodec     = ntnUnversionedDataCodec
                    , haAcceptVersion        = iAcceptVersion ni
                    , haQueryVersion         = const False
                    , haTimeLimits           = timeLimitsHandshake
                    }
              , Diff.diNtnAddressType    = ntnAddressType
              , Diff.diNtnDataFlow       = \NtNVersionData { ntnDiffusionMode } ->
                  case ntnDiffusionMode of
                    InitiatorOnlyDiffusionMode         -> Unidirectional
                    InitiatorAndResponderDiffusionMode -> Duplex
              , Diff.diNtnPeerSharing        = ntnPeerSharing
              , Diff.diNtnToPeerAddr         = \a b -> TestAddress (Node.IPAddr a b)
              , Diff.diNtcSnocket            = iNtcSnocket ni
              , Diff.diNtcBearer             = iNtcBearer ni
              , Diff.diNtcHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer      = nullTracer
                    , haHandshakeCodec       = unversionedHandshakeCodec
                    , haVersionDataCodec     = unversionedProtocolDataCodec
                    , haAcceptVersion        = \_ v -> Accept v
                    , haQueryVersion         = const False
                    , haTimeLimits           = noTimeLimitsHandshake
                    }
              , Diff.diNtcGetFileDescriptor  = \_ -> pure invalidFileDescriptor
              , Diff.diRng                   = diffStgGen
              , Diff.diInstallSigUSR1Handler = \_ _ _ -> pure ()
              , Diff.diDnsActions            = const (mockDNSActions
                                                     (iDomainMap ni)
                                                     dnsTimeoutScriptVar
                                                     dnsLookupDelayScriptVar)
              }

        let apps = Node.applications (aDebugTracer na) nodeKernel Node.cborCodecs limits (appArgs peerMetrics) blockHeader

        withAsync
           (Diff.runM interfaces
                      tracers
                      (mkArgs (nkPublicPeerSelectionVar nodeKernel) useBootstrapPeersScriptVar)
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
          bfcDecisionLoopInterval   = 0.01,
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
          readFetchMode          = return FetchModeBulkSync,
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
          blockForgeUTCTime      = headerForgeUTCTime . fmap blockHeader
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
           -> StrictTVar m (Script UseBootstrapPeers)
           -> Diff.Arguments m (NtNFD m) NtNAddr (NtCFD m) NtCAddr
    mkArgs daPublicPeerSelectionVar ubpVar = Diff.Arguments
      { Diff.daIPv4Address   = Right <$> (ntnToIPv4 . aIPAddress) na
      , Diff.daIPv6Address   = Right <$> (ntnToIPv6 . aIPAddress) na
      , Diff.daLocalAddress  = Nothing
      , Diff.daAcceptedConnectionsLimit
                             = aAcceptedLimits na
      , Diff.daMode          = aDiffusionMode na
      , Diff.daPublicPeerSelectionVar
      , Diff.daPeerTargets            = aPeerTargets na
      , Diff.daReadLocalRootPeers     = aReadLocalRootPeers na
      , Diff.daReadPublicRootPeers    = aReadPublicRootPeers na
      , Diff.daReadUseBootstrapPeers  = stepScriptSTM' ubpVar
      , Diff.daOwnPeerSharing         = aOwnPeerSharing na
      , Diff.daReadUseLedgerPeers     = aReadUseLedgerPeers na
      , Diff.daProtocolIdleTimeout    = aProtocolIdleTimeout na
      , Diff.daTimeWaitTimeout        = aTimeWaitTimeout na
      , Diff.daDeadlineChurnInterval  = 3300
      , Diff.daBulkChurnInterval      = 300
      , Diff.daReadLedgerPeerSnapshot = pure Nothing -- ^ tested independently
      , Diff.daConsensusMode          = aConsensusMode na
      , Diff.daMinBigLedgerPeersForTrustedState
          = MinBigLedgerPeersForTrustedState 0 -- ^ todo: fix
      }

    appArgs :: PeerMetrics m NtNAddr -> Node.AppArgs BlockHeader Block m
    appArgs peerMetrics = Node.AppArgs
      { Node.aaLedgerPeersConsensusInterface
                                        = iLedgerPeersConsensusInterface ni
      , Node.aaKeepAliveStdGen          = keepAliveStdGen
      , Node.aaDiffusionMode            = aDiffusionMode na
      , Node.aaKeepAliveInterval        = aKeepAliveInterval na
      , Node.aaPingPongInterval         = aPingPongInterval na
      , Node.aaShouldChainSyncExit      = aShouldChainSyncExit na
      , Node.aaChainSyncEarlyExit       = aChainSyncEarlyExit na
      , Node.aaOwnPeerSharing           = aOwnPeerSharing na
      , Node.aaUpdateOutboundConnectionsState =
          iUpdateOutboundConnectionsState ni
      , Node.aaPeerMetrics              = peerMetrics
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
