{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Ouroboros.Network.Diffusion.Node
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
  , LedgerPeersConsensusInterface (..)
  , PeerAdvertise (..)
  , PeerSelectionTargets (..)
  , RelayAccessPoint (..)
  , UseLedgerAfter (..)
  ) where

import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad ((>=>))
import           Control.Monad.Class.MonadAsync
                     (MonadAsync (Async, wait, withAsync))
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadEvaluate, MonadMask,
                     MonadThrow, SomeException)
import           Control.Monad.Class.MonadTime (DiffTime, MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Monad.Fix (MonadFix)
import           Control.Tracer (Tracer (..), nullTracer)

import           Data.Foldable (foldl')
import           Data.IP (IP (..))
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Void (Void)
import           System.Random (StdGen, split)

import qualified Codec.CBOR.Term as CBOR

import           Network.DNS (Domain, TTL)

import           Ouroboros.Network.Mock.Chain (Chain, toOldestFirst)
import           Ouroboros.Network.Mock.ConcreteBlock (Block (..),
                     BlockHeader (..),
                     convertSlotToTimeForTestsAssumingNoHardFork)
import           Ouroboros.Network.Mock.ProducerState (ChainProducerState (..))

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (MaxSlotNo (..), Point,
                     maxSlotNoFromWithOrigin, pointSlot)
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import qualified Ouroboros.Network.Diffusion as Diff
import qualified Ouroboros.Network.Diffusion.P2P as Diff.P2P
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.Governor
                     (PeerSelectionTargets (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface (..), UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.PeerMetric
                     (PeerMetricsConfiguration (..), newPeerMetric)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DomainAccessPoint (..), LookupReqs (..),
                     RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
import           Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import           Ouroboros.Network.Protocol.Handshake.Codec
                     (VersionDataCodec (..), noTimeLimitsHandshake,
                     timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned
                     (unversionedHandshakeCodec, unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (Accept (Accept))
import           Ouroboros.Network.RethrowPolicy (ErrorCommand (ShutdownNode),
                     ioErrorRethrowPolicy, mkRethrowPolicy,
                     muxErrorRethrowPolicy)
import           Ouroboros.Network.Server.RateLimiting
                     (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Snocket (FileDescriptor (..), MakeBearer,
                     Snocket, TestAddress (..))

import           Ouroboros.Network.Testing.Data.Script (Script (..))

import           Simulation.Network.Snocket (AddressType (..), FD)

import qualified Test.Ouroboros.Network.Diffusion.Node.MiniProtocols as Node
import qualified Test.Ouroboros.Network.Diffusion.Node.NodeKernel as Node
import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel
                     (NodeKernel (..), NtCAddr, NtCVersion, NtCVersionData,
                     NtNAddr, NtNVersion, NtNVersionData (..))
import           Test.Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DNSLookupDelay, DNSTimeout, mockDNSActions)


data Interfaces m = Interfaces
    { iNtnSnocket        :: Snocket m (NtNFD m) NtNAddr
    , iNtnBearer         :: MakeBearer m (NtNFD m)
    , iAcceptVersion     :: NtNVersionData -> NtNVersionData -> Accept NtNVersionData
    , iNtnDomainResolver :: LookupReqs -> [DomainAccessPoint] -> m (Map DomainAccessPoint (Set NtNAddr))
    , iNtcSnocket        :: Snocket m (NtCFD m) NtCAddr
    , iNtcBearer         :: MakeBearer m (NtCFD m)
    , iRng               :: StdGen
    , iDomainMap         :: StrictTVar m (Map Domain [(IP, TTL)])
    , iLedgerPeersConsensusInterface
                         :: LedgerPeersConsensusInterface m
    }

type NtNFD m = FD m NtNAddr
type NtCFD m = FD m NtCAddr

data Arguments m = Arguments
    { aIPAddress            :: NtNAddr
    , aAcceptedLimits       :: AcceptedConnectionsLimit
    , aDiffusionMode        :: DiffusionMode
    , aKeepAliveInterval    :: DiffTime
    , aPingPongInterval     :: DiffTime
    , aShouldChainSyncExit  :: Block -> m Bool

    , aPeerSelectionTargets :: PeerSelectionTargets
    , aReadLocalRootPeers   :: STM m [(Int, Map RelayAccessPoint PeerAdvertise)]
    , aReadPublicRootPeers  :: STM m [RelayAccessPoint]
    , aReadUseLedgerAfter   :: STM m UseLedgerAfter
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
       ( MonadAsync       m
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
       , MonadThrow       m
       , MonadThrow       (STM m)

       , resolver ~ ()
       , forall a. Semigroup a => Semigroup (m a)
       , Eq (Async m Void)
       )
    => Tracer m String
    -> Node.BlockGeneratorArgs Block StdGen
    -> Node.LimitsAndTimeouts Block
    -> Interfaces m
    -> Arguments m
    -> Diff.P2P.TracersExtra NtNAddr NtNVersion NtNVersionData
                             NtCAddr NtCVersion NtCVersionData
                             ResolverException m
    -> m Void
run _debugTracer blockGeneratorArgs limits ni na tracersExtra =
    Node.withNodeKernelThread blockGeneratorArgs
      $ \ nodeKernel nodeKernelThread -> do
        dnsTimeoutScriptVar <- LazySTM.newTVarIO (aDNSTimeoutScript na)
        dnsLookupDelayScriptVar <- LazySTM.newTVarIO (aDNSLookupDelayScript na)
        peerMetrics <- newPeerMetric PeerMetricsConfiguration { maxEntriesToTrack = 180 }
        let -- diffusion interfaces
            interfaces :: Diff.P2P.Interfaces (NtNFD m) NtNAddr NtNVersion NtNVersionData
                                              (NtCFD m) NtCAddr NtCVersion NtCVersionData
                                              resolver ResolverException
                                              m
            interfaces = Diff.P2P.Interfaces
              { Diff.P2P.diNtnSnocket            = iNtnSnocket ni
              , Diff.P2P.diNtnBearer             = iNtnBearer ni
              , Diff.P2P.diNtnConfigureSocket    = \_ _ -> return ()
              , Diff.P2P.diNtnConfigureSystemdSocket
                                                 = \_ _ -> return ()
              , Diff.P2P.diNtnHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer      = nullTracer
                    , haHandshakeCodec       = unversionedHandshakeCodec
                    , haVersionDataCodec     = ntnUnversionedDataCodec
                    , haAcceptVersion        = iAcceptVersion ni
                    , haTimeLimits           = timeLimitsHandshake
                    }
              , Diff.P2P.diNtnAddressType    = ntnAddressType
              , Diff.P2P.diNtnDataFlow       = \_ NtNVersionData { ntnDiffusionMode } ->
                  case ntnDiffusionMode of
                    InitiatorOnlyDiffusionMode         -> Unidirectional
                    InitiatorAndResponderDiffusionMode -> Duplex
              , Diff.P2P.diNtnToPeerAddr         = \a b -> TestAddress (Node.IPAddr a b)
              , Diff.P2P.diNtnDomainResolver     = iNtnDomainResolver ni
              , Diff.P2P.diNtcSnocket            = iNtcSnocket ni
              , Diff.P2P.diNtcBearer             = iNtcBearer ni
              , Diff.P2P.diNtcHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer      = nullTracer
                    , haHandshakeCodec       = unversionedHandshakeCodec
                    , haVersionDataCodec     = unversionedProtocolDataCodec
                    , haAcceptVersion        = \_ v -> Accept v
                    , haTimeLimits           = noTimeLimitsHandshake
                    }
              , Diff.P2P.diNtcGetFileDescriptor  = \_ -> pure (FileDescriptor (-1))
              , Diff.P2P.diRng                   = diffStgGen
              , Diff.P2P.diInstallSigUSR1Handler = \_ -> pure ()
              , Diff.P2P.diDnsActions            = const (mockDNSActions
                                                     (iDomainMap ni)
                                                     dnsTimeoutScriptVar
                                                     dnsLookupDelayScriptVar)
              }

            appsExtra :: Diff.P2P.ApplicationsExtra NtNAddr m ()
            appsExtra = Diff.P2P.ApplicationsExtra
              { -- TODO: simulation errors should be critical
                Diff.P2P.daRethrowPolicy          =
                     muxErrorRethrowPolicy
                  <> ioErrorRethrowPolicy

                -- we are not using local connections, so we can make all the
                -- errors fatal.
              , Diff.P2P.daLocalRethrowPolicy     =
                     mkRethrowPolicy
                       (\ _ (_ :: SomeException) -> ShutdownNode)
              , Diff.P2P.daPeerMetrics            = peerMetrics
                -- fetch mode is not used (no block-fetch mini-protocol)
              , Diff.P2P.daBlockFetchMode         = pure FetchModeDeadline
              , Diff.P2P.daReturnPolicy           = \_ -> 0
              }

        apps <- Node.applications @_ @BlockHeader (aDebugTracer na) nodeKernel Node.cborCodecs limits appArgs

        withAsync
           (Diff.P2P.runM interfaces
                          Diff.nullTracers
                          tracersExtra
                          args argsExtra apps appsExtra)
           $ \ diffusionThread ->
               withAsync (blockFetch nodeKernel) $ \blockFetchLogicThread ->
                 wait diffusionThread
              <> wait blockFetchLogicThread
              <> wait nodeKernelThread
  where
    blockFetch :: NodeKernel BlockHeader Block m
               -> m Void
    blockFetch nodeKernel = do
      blockHeapVar <- LazySTM.newTVarIO Set.empty

      blockFetchLogic
        nullTracer
        nullTracer
        (blockFetchPolicy blockHeapVar nodeKernel)
        (nkFetchClientRegistry nodeKernel)
        (BlockFetchConfiguration {
          bfcMaxConcurrencyBulkSync = 1,
          bfcMaxConcurrencyDeadline = 2,
          bfcMaxRequestsInflight    = 10,
          bfcDecisionLoopInterval   = 0.01,
          bfcSalt                   = 0
        })

    blockFetchPolicy :: LazySTM.TVar m (Set (Point Block))
                     -> NodeKernel BlockHeader Block m
                     -> BlockFetchConsensusInterface NtNAddr BlockHeader Block m
    blockFetchPolicy blockHeapVar nodeKernel =
        BlockFetchConsensusInterface {
          readCandidateChains    = readTVar (nkClientChains nodeKernel)
                                   >>= traverse (readTVar
                                       >=> (return . toAnchoredFragmentHeader)),
          readCurrentChain       = readTVar (nkChainProducerState nodeKernel)
                                   >>= (return . toAnchoredFragmentHeader . chainState),
          readFetchMode          = return FetchModeBulkSync,
          readFetchedBlocks      = flip Set.member <$> LazySTM.readTVar blockHeapVar,
          readFetchedMaxSlotNo   = foldl' max NoMaxSlotNo .
                                   map (maxSlotNoFromWithOrigin . pointSlot) .
                                   Set.elems <$>
                                   LazySTM.readTVar blockHeapVar,
          mkAddFetchedBlock        = \_enablePipelining -> do
              pure $ \p _b ->
                atomically (LazySTM.modifyTVar' blockHeapVar (Set.insert p)),

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
        encodeData _ NtNVersionData { ntnDiffusionMode } =
          case ntnDiffusionMode of
            InitiatorOnlyDiffusionMode         -> CBOR.TBool False
            InitiatorAndResponderDiffusionMode -> CBOR.TBool True
        decodeData _ bytes = case bytes of
          CBOR.TBool False -> Right (NtNVersionData InitiatorOnlyDiffusionMode)
          CBOR.TBool True  -> Right (NtNVersionData InitiatorAndResponderDiffusionMode)
          _                -> Left (Text.pack "unversionedDataCodec: unexpected term")

    args :: Diff.Arguments (NtNFD m) NtNAddr (NtCFD m) NtCAddr
    args = Diff.Arguments
      { Diff.daIPv4Address   = Right <$> (ntnToIPv4 . aIPAddress) na
      , Diff.daIPv6Address   = Right <$> (ntnToIPv6 . aIPAddress) na
      , Diff.daLocalAddress  = Nothing
      , Diff.daAcceptedConnectionsLimit
                             = aAcceptedLimits na
      , Diff.daMode          = aDiffusionMode na
      }

    argsExtra :: Diff.P2P.ArgumentsExtra m
    argsExtra = Diff.P2P.ArgumentsExtra
      { Diff.P2P.daPeerSelectionTargets  = aPeerSelectionTargets na
      , Diff.P2P.daReadLocalRootPeers    = aReadLocalRootPeers na
      , Diff.P2P.daReadPublicRootPeers   = aReadPublicRootPeers na
      , Diff.P2P.daReadUseLedgerAfter    = aReadUseLedgerAfter na
      , Diff.P2P.daProtocolIdleTimeout   = aProtocolIdleTimeout na
      , Diff.P2P.daTimeWaitTimeout       = aTimeWaitTimeout na
      , Diff.P2P.daDeadlineChurnInterval = 3300
      , Diff.P2P.daBulkChurnInterval     = 300
      }

    appArgs :: Node.AppArgs Block m
    appArgs = Node.AppArgs
      { Node.aaLedgerPeersConsensusInterface
                                        = iLedgerPeersConsensusInterface ni
      , Node.aaKeepAliveStdGen          = keepAliveStdGen
      , Node.aaDiffusionMode            = aDiffusionMode na
      , Node.aaKeepAliveInterval        = aKeepAliveInterval na
      , Node.aaPingPongInterval         = aPingPongInterval na
      , Node.aaShouldChainSyncExit      = aShouldChainSyncExit na
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
