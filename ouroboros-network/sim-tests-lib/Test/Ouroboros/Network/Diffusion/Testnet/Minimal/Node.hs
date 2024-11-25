{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Ouroboros.Network.Diffusion.Testnet.Minimal.Node
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
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Governor (PeerChurnArgs,
           PeerSelectionState (..), PeerSelectionTargets (..),
           PublicPeerSelectionState (..))
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

import Ouroboros.Network.Testing.Data.Script (Script (..))

import Simulation.Network.Snocket (AddressType (..), FD)

import GHC.Exception (Exception)
import Ouroboros.Network.Diffusion.Common qualified as Common
import Ouroboros.Network.Diffusion.MinimalP2P (runM)
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
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSLookupType)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency,
           WarmValency)
import Ouroboros.Network.PeerSelection.Types (PublicExtraPeersActions (..))
import Test.Ouroboros.Network.Diffusion.Node.ChainDB (addBlock,
           getBlockPointSet)
import Test.Ouroboros.Network.Diffusion.Node.MiniProtocols qualified as Node
import Test.Ouroboros.Network.Diffusion.Node.Kernel (NodeKernel (..),
           NtCAddr, NtCVersion, NtCVersionData, NtNAddr, NtNVersion,
           NtNVersionData (..))
import Test.Ouroboros.Network.Diffusion.Node.Kernel qualified as Node
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
    }

type NtNFD m = FD m NtNAddr
type NtCFD m = FD m NtCAddr

data Arguments extraArgs extraChurnArgs extraFlags m = Arguments
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
                                      , Map RelayAccessPoint ( PeerAdvertise, extraFlags ))]
    , aReadPublicRootPeers  :: STM m (Map RelayAccessPoint PeerAdvertise)
    , aOwnPeerSharing       :: PeerSharing
    , aReadUseLedgerPeers   :: STM m UseLedgerPeers
    , aProtocolIdleTimeout  :: DiffTime
    , aTimeWaitTimeout      :: DiffTime
    , aDNSTimeoutScript     :: Script DNSTimeout
    , aDNSLookupDelayScript :: Script DNSLookupDelay
    , aDebugTracer          :: Tracer m String
    , aExtraArgs            :: extraArgs
    , aExtraChurnArgs       :: extraChurnArgs
    }

-- The 'mockDNSActions' is not using \/ specifying 'resolverException', thus we
-- set it to 'SomeException'.
--
type ResolverException = SomeException

run :: forall extraArgs extraState extraActions extraAPI
             extraPeers extraFlags extraChurnArgs extraCounters
             exception resolver m.
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
       , forall a. Semigroup a => Semigroup (m a)
       )
    => Node.BlockGeneratorArgs Block StdGen
    -> Node.LimitsAndTimeouts BlockHeader Block
    -> Interfaces extraAPI m
    -> Arguments extraArgs extraChurnArgs extraFlags m
    -> extraState
    -> extraActions
    -> extraPeers
    -> extraCounters
    -> PublicExtraPeersActions extraPeers NtNAddr
    -> (forall muxMode responderCtx ntnVersionData bytes a b .
        PeerSelectionGovernorArgs
          extraState
          extraActions
          extraPeers
          extraAPI
          extraFlags
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
    -> ( (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set NtNAddr, DiffTime)))
        -> LedgerPeersKind
        -> Int
        -> m (PublicRootPeers extraPeers NtNAddr, DiffTime))
    -> (PeerChurnArgs
             m
             extraChurnArgs
             extraState
             extraFlags
             extraPeers
             extraAPI
             extraCounters
             NtNAddr
        -> m Void)
    -> Common.TracersExtra NtNAddr NtNVersion NtNVersionData
                           NtCAddr NtCVersion NtCVersionData
                           ResolverException extraState extraState extraFlags
                           extraPeers extraCounters m
    -> Tracer m (TraceLabelPeer NtNAddr (TraceFetchClientState BlockHeader))
    -> m Void
run blockGeneratorArgs limits ni na
    emptyExtraState extraActions emptyExtraPeers
    emptyExtraCounters extraPeersActions psArgs
    psToExtraCounters requestPublicRootPeers peerChurnGovernor
    tracersExtra tracerBlockFetch =
    Node.withNodeKernelThread blockGeneratorArgs
      $ \ nodeKernel nodeKernelThread -> do
        dnsTimeoutScriptVar <- newTVarIO (aDNSTimeoutScript na)
        dnsLookupDelayScriptVar <- newTVarIO (aDNSLookupDelayScript na)
        peerMetrics <- newPeerMetric PeerMetricsConfiguration { maxEntriesToTrack = 180 }

        let -- diffusion interfaces
            interfaces :: Common.Interfaces (NtNFD m) NtNAddr NtNVersion NtNVersionData
                                            (NtCFD m) NtCAddr NtCVersion NtCVersionData
                                            resolver ResolverException extraState extraFlags extraPeers m
            interfaces = Common.Interfaces
              { Common.diNtnSnocket            = iNtnSnocket ni
              , Common.diNtnBearer             = iNtnBearer ni
              , Common.diNtnConfigureSocket    = \_ _ -> return ()
              , Common.diNtnConfigureSystemdSocket
                                               = \_ _ -> return ()
              , Common.diNtnHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer  = nullTracer
                    , haHandshakeCodec   = unversionedHandshakeCodec
                    , haVersionDataCodec = ntnUnversionedDataCodec
                    , haAcceptVersion    = iAcceptVersion ni
                    , haQueryVersion     = const False
                    , haTimeLimits       = timeLimitsHandshake
                    }
              , Common.diNtnAddressType = ntnAddressType
              , Common.diNtnDataFlow    = \NtNVersionData { ntnDiffusionMode } ->
                  case ntnDiffusionMode of
                    InitiatorOnlyDiffusionMode         -> Unidirectional
                    InitiatorAndResponderDiffusionMode -> Duplex
              , Common.diNtnPeerSharing        = ntnPeerSharing
              , Common.diNtnToPeerAddr         = \a b -> TestAddress (Node.IPAddr a b)
              , Common.diNtcSnocket            = iNtcSnocket ni
              , Common.diNtcBearer             = iNtcBearer ni
              , Common.diNtcHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer  = nullTracer
                    , haHandshakeCodec   = unversionedHandshakeCodec
                    , haVersionDataCodec = unversionedProtocolDataCodec
                    , haAcceptVersion    = \_ v -> Accept v
                    , haQueryVersion     = const False
                    , haTimeLimits       = noTimeLimitsHandshake
                    }
              , Common.diNtcGetFileDescriptor  = \_ -> pure invalidFileDescriptor
              , Common.diRng                   = diffStgGen
              , Common.diInstallSigUSR1Handler = \_ _ _ -> pure ()
              , Common.diDnsActions            = const (mockDNSActions
                                                       (iDomainMap ni)
                                                       dnsTimeoutScriptVar
                                                       dnsLookupDelayScriptVar)
              }

            appsExtra :: Common.ApplicationsExtra NtNAddr m ()
            appsExtra = Common.ApplicationsExtra
              { -- TODO: simulation errors should be critical
                Common.daRethrowPolicy     =
                     muxErrorRethrowPolicy
                  <> ioErrorRethrowPolicy

                -- we are not using local connections, so we can make all the
                -- errors fatal.
              , Common.daLocalRethrowPolicy =
                     mkRethrowPolicy
                       (\ _ (_ :: SomeException) -> ShutdownNode)
              , Common.daPeerMetrics         = peerMetrics
                -- fetch mode is not used (no block-fetch mini-protocol)
              , Common.daBlockFetchMode      = pure FetchModeDeadline
              , Common.daReturnPolicy        = \_ -> config_REPROMOTE_DELAY
              , Common.daPeerSharingRegistry = nkPeerSharingRegistry nodeKernel
              }

        let apps = Node.applications (aDebugTracer na) nodeKernel Node.cborCodecs limits appArgs blockHeader

        withAsync
           (runM interfaces
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

    argsExtra :: Common.ArgumentsExtra
                   extraArgs extraState extraActions
                   extraAPI extraPeers extraFlags
                   extraChurnArgs extraCounters exception
                   NtNAddr m
    argsExtra = Common.ArgumentsExtra
      { Common.daPeerSelectionTargets   = aPeerTargets na
      , Common.daReadLocalRootPeers     = aReadLocalRootPeers na
      , Common.daReadPublicRootPeers    = aReadPublicRootPeers na
      , Common.daOwnPeerSharing         = aOwnPeerSharing na
      , Common.daReadUseLedgerPeers     = aReadUseLedgerPeers na
      , Common.daProtocolIdleTimeout    = aProtocolIdleTimeout na
      , Common.daTimeWaitTimeout        = aTimeWaitTimeout na
      , Common.daDeadlineChurnInterval  = 3300
      , Common.daBulkChurnInterval      = 300
      , Common.daReadLedgerPeerSnapshot = pure Nothing -- ^ tested independently
      , Common.daEmptyExtraState        = emptyExtraState
      , Common.daEmptyExtraPeers        = emptyExtraPeers
      , Common.daEmptyExtraCounters     = emptyExtraCounters
      , Common.daExtraPeersActions      = extraPeersActions
      , Common.daExtraActions           = extraActions
      , Common.daExtraChurnArgs         = aExtraChurnArgs na
      , Common.daExtraArgs              = aExtraArgs na
      , Common.daRequestPublicRootPeers = requestPublicRootPeers
      , Common.daPeerChurnGovernor      = peerChurnGovernor
      , Common.daPeerSelectionGovernorArgs         = psArgs
      , Common.daPeerSelectionStateToExtraCounters = psToExtraCounters
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
