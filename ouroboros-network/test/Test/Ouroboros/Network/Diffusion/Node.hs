{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TupleSections #-}

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

import           Control.Monad.Fix (MonadFix)
import           Control.Monad.Class.MonadAsync
                     (MonadAsync (Async, wait, withAsync))
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict (MonadLabelledSTM,
                     MonadTraceSTM, MonadSTM (STM, atomically), newTVar)
import           Control.Monad.Class.MonadThrow (MonadEvaluate, MonadMask,
                     MonadThrow, SomeException)
import           Control.Monad.Class.MonadTime (DiffTime, MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Tracer (nullTracer)

import           Data.IP (IP (..))
import qualified Data.IntPSQ as IntPSQ
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Text as Text
import           Data.Void (Void)
import           System.Random (StdGen, split)

import qualified Codec.CBOR.Term as CBOR

import           Network.DNS (Domain)

import           Ouroboros.Network.BlockFetch.Decision (FetchMode (..))
import           Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import qualified Ouroboros.Network.Diffusion as Diff
import qualified Ouroboros.Network.Diffusion.P2P as Diff.P2P
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.Governor
                     (PeerSelectionTargets (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface (..), UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics (..))
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
import           Ouroboros.Network.Snocket (FileDescriptor (..), Snocket,
                     TestAddress (..))

import           Ouroboros.Network.Testing.ConcreteBlock (Block)
import           Ouroboros.Network.Testing.Data.Script (Script (..), singletonScript)
import           Simulation.Network.Snocket (AddressType (IPv4Address), FD)

import qualified Test.Ouroboros.Network.Diffusion.Node.MiniProtocols as Node
import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel (NtCAddr,
                     NtCVersion, NtCVersionData, NtNAddr, NtNVersion,
                     NtNVersionData (..))
import qualified Test.Ouroboros.Network.Diffusion.Node.NodeKernel as Node
import           Test.Ouroboros.Network.PeerSelection.RootPeersDNS
                     (DNSLookupDelay, DNSTimeout, mockDNSActions)


data Interfaces m = Interfaces
    { iNtnSnocket        :: Snocket m (NtNFD m) NtNAddr
    , iAcceptVersion     :: NtNVersionData -> NtNVersionData -> Accept NtNVersionData
    , iNtnDomainResolver :: LookupReqs -> [DomainAccessPoint] -> m (Map DomainAccessPoint (Set NtNAddr))
    , iNtcSnocket        :: Snocket m (NtCFD m) NtCAddr
    , iRng               :: StdGen
    , iDomainMap         :: Map Domain [IP]
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

    , aPeerSelectionTargets :: PeerSelectionTargets
    , aReadLocalRootPeers   :: STM m [(Int, Map RelayAccessPoint PeerAdvertise)]
    , aReadPublicRootPeers  :: STM m [RelayAccessPoint]
    , aReadUseLedgerAfter   :: STM m UseLedgerAfter
    , aProtocolIdleTimeout  :: DiffTime
    , aTimeWaitTimeout      :: DiffTime
    , aDNSTimeoutScript     :: Script DNSTimeout
    , aDNSLookupDelayScript :: Script DNSLookupDelay
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
       , MonadST          m
       , MonadTime        m
       , MonadTimer       m
       , MonadThrow       m
       , MonadThrow       (STM m)

       , resolver ~ ()
       , forall a. Semigroup a => Semigroup (m a)
       , Eq (Async m Void)
       )
    => Node.BlockGeneratorArgs Block StdGen
    -> Node.LimitsAndTimeouts Block
    -> Interfaces m
    -> Arguments m
    -> m Void
run blockGeneratorArgs limits ni na =
    Node.withNodeKernelThread blockGeneratorArgs
      $ \ nodeKernel nodeKernelThread -> do
        dnsMapScriptVar <- LazySTM.newTVarIO
                        $ singletonScript (fmap (, 0) <$> iDomainMap ni)
        dnsTimeoutScriptVar <- LazySTM.newTVarIO (aDNSTimeoutScript na)
        dnsLookupDelayScriptVar <- LazySTM.newTVarIO (aDNSLookupDelayScript na)
        peerMetrics  <- atomically $ PeerMetrics
          <$> newTVar IntPSQ.empty
          <*> newTVar IntPSQ.empty
        let -- diffusion interfaces
            interfaces :: Diff.P2P.Interfaces (NtNFD m) NtNAddr NtNVersion NtNVersionData
                                              (NtCFD m) NtCAddr NtCVersion NtCVersionData
                                              resolver ResolverException
                                              m
            interfaces = Diff.P2P.Interfaces
              { Diff.P2P.diNtnSnocket            = iNtnSnocket ni
              , Diff.P2P.diNtnHandshakeArguments =
                  HandshakeArguments
                    { haHandshakeTracer      = nullTracer
                    , haHandshakeCodec       = unversionedHandshakeCodec
                    , haVersionDataCodec     = ntnUnversionedDataCodec
                    , haAcceptVersion        = iAcceptVersion ni
                    , haTimeLimits           = timeLimitsHandshake
                    }
              , Diff.P2P.diNtnAddressType    = const (Just IPv4Address)
              , Diff.P2P.diNtnDataFlow       = \_ NtNVersionData { ntnDiffusionMode } ->
                  case ntnDiffusionMode of
                    InitiatorOnlyDiffusionMode         -> Unidirectional
                    InitiatorAndResponderDiffusionMode -> Duplex
              , Diff.P2P.diNtnToPeerAddr         = \a b -> TestAddress (Node.IPAddr a b)
              , Diff.P2P.diNtnDomainResolver     = iNtnDomainResolver ni
              , Diff.P2P.diNtcSnocket            = iNtcSnocket ni
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
                                                     dnsMapScriptVar
                                                     dnsTimeoutScriptVar
                                                     dnsLookupDelayScriptVar)
              }

            tracersExtra :: Diff.P2P.TracersExtra NtNAddr NtNVersion NtNVersionData
                                                  NtCAddr NtCVersion NtCVersionData
                                                  ResolverException m
            tracersExtra = Diff.P2P.nullTracers

            appsExtra :: Diff.P2P.ApplicationsExtra NtNAddr m
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
              }

        apps <- Node.applications nodeKernel Node.cborCodecs limits appArgs

        withAsync
           (Diff.P2P.runM interfaces
                          Diff.nullTracers tracersExtra
                          args argsExtra apps appsExtra)
           $ \ diffusionThread ->
               wait diffusionThread
            <> wait nodeKernelThread
  where
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
      { Diff.P2P.daPeerSelectionTargets = aPeerSelectionTargets na
      , Diff.P2P.daReadLocalRootPeers   = aReadLocalRootPeers na
      , Diff.P2P.daReadPublicRootPeers  = aReadPublicRootPeers na
      , Diff.P2P.daReadUseLedgerAfter   = aReadUseLedgerAfter na
      , Diff.P2P.daProtocolIdleTimeout  = aProtocolIdleTimeout na
      , Diff.P2P.daTimeWaitTimeout      = aTimeWaitTimeout na
      }

    appArgs :: Node.AppArgs m
    appArgs = Node.AppArgs
      { Node.aaLedgerPeersConsensusInterface
                                        = iLedgerPeersConsensusInterface ni
      , Node.aaKeepAliveStdGen          = keepAliveStdGen
      , Node.aaDiffusionMode            = aDiffusionMode na
      , Node.aaKeepAliveInterval        = aKeepAliveInterval na
      , Node.aaPingPongInterval         = aPingPongInterval na
      }

--- Utils

ntnToIPv4 :: NtNAddr -> Maybe NtNAddr
ntnToIPv4 ntnAddr@(TestAddress (Node.IPAddr (IPv4 _) _)) = Just ntnAddr
ntnToIPv4 (TestAddress _)                                = Nothing

ntnToIPv6 :: NtNAddr -> Maybe NtNAddr
ntnToIPv6 ntnAddr@(TestAddress (Node.IPAddr (IPv6 _) _)) = Just ntnAddr
ntnToIPv6 (TestAddress _)                                = Nothing
