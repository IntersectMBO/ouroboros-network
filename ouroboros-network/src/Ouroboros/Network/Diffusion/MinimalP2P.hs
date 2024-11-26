{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

-- | This module is expected to be imported qualified (it will clash
-- with the "Ouroboros.Network.Diffusion.NonP2P").
--
module Ouroboros.Network.Diffusion.MinimalP2P
  ( run
  , runM
  ) where


import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync (Async, MonadAsync)
import Control.Monad.Class.MonadAsync qualified as Async
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Fix (MonadFix)
import Control.Tracer (Tracer, contramap, nullTracer, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.IP qualified as IP
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Network.Socket (Socket)
import Ouroboros.Network.Snocket (LocalAddress, LocalSocket (..),
           localSocketFileDescriptor, makeLocalBearer, makeSocketBearer)
import Ouroboros.Network.Snocket qualified as Snocket
import System.Exit (ExitCode)
import System.Random (StdGen, newStdGen, split)

import Ouroboros.Network.Context (ExpandedInitiatorContext)
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Socket (configureSocket, configureSystemdSocket)

import Cardano.Network.ArgumentsExtra (CardanoArgumentsExtra (..))
import Cardano.Network.LedgerPeerConsensusInterface
           (CardanoLedgerPeersConsensusInterface (..))
import Cardano.Network.PeerSelection.Governor.PeerSelectionActions
           (CardanoPeerSelectionActions (..))
           (CardanoPeerSelectionState (..))
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as CPST
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Cardano.Network.PublicRootPeers (CardanoPublicRootPeers)
import Cardano.Network.PublicRootPeers qualified as CPRP
import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.InformationChannel
           (newInformationChannel)
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Diffusion.Common hiding (nullTracers)
import Ouroboros.Network.Diffusion.Common qualified as Common
import Ouroboros.Network.Diffusion.Configuration
import Ouroboros.Network.Diffusion.Policies qualified as Diffusion.Policies
import Ouroboros.Network.Diffusion.Utils
import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux hiding (MiniProtocol (..))
import Ouroboros.Network.MuxMode
import Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
           NodeToClientVersionData)
import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode (NodeToNodeVersion (..),
           NodeToNodeVersionData (..), RemoteAddress)
import Ouroboros.Network.NodeToNode qualified as NodeToNode
import Ouroboros.Network.PeerSelection.Churn (PeerChurnArgs (..))
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.Governor.Types hiding (peerSharing)
import Ouroboros.Network.PeerSelection.LedgerPeers (WithLedgerPeersArgs (..))
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
import Ouroboros.Network.PeerSelection.PeerStateActions (PeerConnectionHandle,
           PeerStateActionsArguments (..), pchPeerSharing, withPeerStateActions)
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
           (DNSLookupType (..), ioDNSActions)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingRegistry (..))
import Ouroboros.Network.RethrowPolicy
import Ouroboros.Network.Server2 qualified as Server
#ifdef POSIX
import System.Posix.Signals qualified as Signals
#endif
#ifdef POSIX
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Mux qualified as Mx
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.Diffusion.Policies (simplePeerSelectionPolicy)
import Ouroboros.Network.InboundGovernor qualified as IG
                resolver resolverError exception a
                extraArgs extraState extraActions extraPeers
                extraAPI extraFlags extraChurnArgs extraCounters .

           (LedgerPeersConsensusInterface (..))
import Ouroboros.Network.PeerSelection.PeerMetric (fetchynessBlocks,
           upstreamyness)
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
           (newLedgerAndPublicRootDNSSemaphore)
#else
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot,
           MinBigLedgerPeersForTrustedState, UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
#endif

runM
    :: forall m ntnFd ntnAddr ntnVersion ntnVersionData
                ntcFd ntcAddr ntcVersion ntcVersionData
                resolver resolverError a.
       ( Alternative (STM m)
       , MonadAsync       m
       , MonadDelay       m
       , MonadEvaluate    m
       , MonadFix         m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadTraceSTM    m
       , MonadMask        m
       , MonadThrow  (STM m)
       , MonadTime        m
       , Monoid extraPeers
       , Eq extraFlags
       , Eq extraCounters
       , Exception exception
       , MonadTimer       m
       , MonadMVar        m
       , Typeable  ntnAddr
       , Ord       ntnAddr
       , Show      ntnAddr
                  extraState extraFlags extraPeers m
       , Ord       ntnVersion
       , Show      ntnVersion
       , Show      ntnVersionData
       , Typeable  ntcAddr
       , Ord       ntcAddr
       , Show      ntcAddr
       , Ord       ntcVersion
       , Exception resolverError
       )
    => -- | interfaces
       Interfaces ntnFd ntnAddr ntnVersion ntnVersionData
                  ntcFd ntcAddr ntcVersion ntcVersionData
                  resolver resolverError
                  CardanoPeerSelectionState PeerTrustable
       ArgumentsExtra extraArgs extraState extraActions extraAPI
                      extraPeers extraFlags extraChurnArgs extraCounters
                      exception ntnAddr m
    -> -- | tracers
       Tracers ntnAddr ntnVersion
               ntcAddr ntcVersion
               m
                    extraAPI m a
       TracersExtra ntnAddr ntnVersion ntnVersionData
                    ntcAddr ntcVersion ntcVersionData
                    resolverError CardanoPeerSelectionState
                    CardanoPeerSelectionState PeerTrustable
                    (CardanoPublicRootPeers ntnAddr)
                    (CardanoPeerSelectionView ntnAddr)
                    m
    -> -- | configuration
       Arguments m ntnFd ntnAddr
                   ntcFd ntcAddr
    -> -- | p2p configuration
       ArgumentsExtra (CardanoArgumentsExtra m) PeerTrustable m

    -> -- | protocol handlers
       Applications ntnAddr ntnVersion ntnVersionData
                    ntcAddr ntcVersion ntcVersionData
                    (CardanoLedgerPeersConsensusInterface m) m a
    -> -- | p2p protocol handlers
       ApplicationsExtra ntnAddr m a
    -> m Void
runM Interfaces
       { diNtnSnocket
       , diNtnBearer
       , diNtnConfigureSocket
       , diNtnConfigureSystemdSocket
       , diNtnHandshakeArguments
       , diNtnAddressType
       , diNtnDataFlow
       , diNtnPeerSharing
       , diNtnToPeerAddr
       , diNtcSnocket
       , diNtcBearer
       , diNtcHandshakeArguments
       , diNtcGetFileDescriptor
       , diRng
       , diInstallSigUSR1Handler
       , diDnsActions
       }
     Tracers
       { dtMuxTracer
       , dtLocalMuxTracer
       , dtDiffusionTracer = tracer
       }
     TracersExtra
       { dtTracePeerSelectionTracer
       , dtTraceChurnCounters
       , dtDebugPeerSelectionInitiatorTracer
       , dtDebugPeerSelectionInitiatorResponderTracer
       , dtTracePeerSelectionCounters
       , dtPeerSelectionActionsTracer
       , dtTraceLocalRootPeersTracer
       , dtTraceLedgerPeersTracer
       , dtConnectionManagerTracer
       , dtConnectionManagerTransitionTracer
       , dtServerTracer
       , dtInboundGovernorTracer
       , dtInboundGovernorTransitionTracer
       , dtLocalConnectionManagerTracer
       , dtLocalServerTracer
       , dtLocalInboundGovernorTracer
       }
     Arguments
       { daIPv4Address
       , daIPv6Address
       , daLocalAddress
       , daAcceptedConnectionsLimit
       , daMode = diffusionMode
       , daPublicPeerSelectionVar
       }
     ArgumentsExtra
       { daPeerSelectionTargets
       , daReadLocalRootPeers
       , daOwnPeerSharing
       , daEmptyExtraState
       , daEmptyExtraPeers
       , daEmptyExtraCounters
       , daExtraPeersActions
       , daPeerSelectionGovernorArgs
       , daPeerSelectionStateToExtraCounters
       , daPeerChurnGovernor
       , daRequestPublicRootPeers
       , daExtraActions
       , daExtraChurnArgs
       , daExtraArgs = CardanoArgumentsExtra {
           caeSyncPeerTargets
         , caeMinBigLedgerPeersForTrustedState
         , caeConsensusMode
         , caeReadUseBootstrapPeers
         }
       }
     Applications
       { daApplicationInitiatorMode
       , daApplicationInitiatorResponderMode
       , daLocalResponderApplication
       , daLedgerPeersCtx
       }
     ApplicationsExtra
       { daRethrowPolicy
       , daLocalRethrowPolicy
       , daReturnPolicy
       , daPeerMetrics
       , daPeerSharingRegistry
       }
  = do
    -- Thread to which 'RethrowPolicy' will throw fatal exceptions.
    mainThreadId <- myThreadId

    Async.runConcurrently
      $ asum
      $ Async.Concurrently <$>
          ( mkRemoteThread mainThreadId
          : maybeToList (mkLocalThread mainThreadId <$> daLocalAddress)
          )

  where
    (ledgerPeersRng, rng1) = split diRng
    (policyRng,      rng2) = split rng1
    (churnRng,       rng3) = split rng2
    (fuzzRng,        rng4) = split rng3
    (cmLocalStdGen,  rng5) = split rng4
    (cmStdGen1, cmStdGen2) = split rng5


    mkInboundPeersMap :: IG.PublicState ntnAddr ntnVersionData
                      -> Map ntnAddr PeerSharing
    mkInboundPeersMap
      IG.PublicState { IG.inboundDuplexPeers }
      =
      Map.map diNtnPeerSharing inboundDuplexPeers

    -- TODO: this policy should also be used in `PeerStateActions` and
    -- `InboundGovernor` (when creating or accepting connections)
    rethrowPolicy =
      -- Only the 'IOManagerError's are fatal, all the other exceptions in the
      -- networking code will only shutdown the bearer (see 'ShutdownPeer' why
      -- this is so).
      RethrowPolicy (\_ctx err ->
        case fromException err of
          Just (_ :: IOManagerError) -> ShutdownNode
          Nothing                    -> mempty)
      <>
      RethrowPolicy (\_ctx err ->
        case fromException err of
          -- if we are out of file descriptors (either because we exhausted
          -- process or system limit) we should shut down the node and let the
          -- operator investigate.
          --
          -- Refs:
          -- * https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/src/GHC.Internal.Foreign.C.Error.html#errnoToIOError
          -- * man socket.2
          -- * man connect.2
          -- * man accept.2
          -- NOTE: many `connect` and `accept` exceptions are classified as
          -- `OtherError`, here we only distinguish fatal IO errors (e.g.
          -- ones that propagate to the main thread).
          -- NOTE: we don't use the rethrow policy for `accept` calls, where
          -- all but `ECONNABORTED` are fatal exceptions.
          Just IOError { ioe_type } ->
            case ioe_type of
              ResourceExhausted    -> ShutdownNode
              -- EAGAIN            -- connect, accept
              -- EMFILE            -- socket, accept
              -- ENFILE            -- socket, accept
              -- ENOBUFS           -- socket, accept
              -- ENOMEM            -- socket, accept

              UnsupportedOperation -> ShutdownNode
              -- EADDRNOTAVAIL     -- connect
              -- EAFNOSUPPRT       -- connect

              InvalidArgument      -> ShutdownNode
              -- EINVAL            -- socket, accept
              -- ENOTSOCK          -- connect
              -- EBADF             -- connect, accept

              ProtocolError        -> ShutdownNode
              -- EPROTONOSUPPOPRT  -- socket
              -- EPROTO            -- accept

              _                    -> mempty
          Nothing -> mempty)
      <>
      RethrowPolicy (\ctx err -> case  (ctx, fromException err) of
                        -- mux unknown mini-protocol errors on the outbound
                        -- side are fatal, since this is misconfiguration of the
                        -- ouroboros-network stack.
                        (OutboundError, Just Mx.UnknownMiniProtocol {})
                          -> ShutdownNode
                        _ -> mempty)


    -- | mkLocalThread - create local connection manager

    mkLocalThread :: ThreadId m -> Either ntcFd ntcAddr -> m Void
    mkLocalThread mainThreadId localAddr =
      withLocalSocket tracer diNtcGetFileDescriptor diNtcSnocket localAddr
      $ \localSocket -> do
        localInbInfoChannel <- newInformationChannel

        let localConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0

            localConnectionHandler :: NodeToClientConnectionHandler
                                        ntcFd ntcAddr ntcVersion ntcVersionData m
            localConnectionHandler =
              makeConnectionHandler
                dtLocalMuxTracer
                SingResponderMode
                diNtcHandshakeArguments
                ( ( \ (OuroborosApplication apps)
                   -> TemperatureBundle
                        (WithHot apps)
                        (WithWarm [])
                        (WithEstablished [])
                  ) <$> daLocalResponderApplication )
                (mainThreadId, rethrowPolicy <> daLocalRethrowPolicy)

            localConnectionManagerArguments
              :: NodeToClientConnectionManagerArguments
                   ntcFd ntcAddr ntcVersion ntcVersionData m
            localConnectionManagerArguments =
              CM.Arguments {
                  CM.tracer              = dtLocalConnectionManagerTracer,
                  CM.trTracer            = nullTracer, -- TODO: issue #3320
                  CM.muxTracer           = dtLocalMuxTracer,
                  CM.ipv4Address         = Nothing,
                  CM.ipv6Address         = Nothing,
                  CM.addressType         = const Nothing,
                  CM.snocket             = diNtcSnocket,
                  CM.makeBearer          = diNtcBearer,
                  CM.configureSocket     = \_ _ -> return (),
                  CM.timeWaitTimeout     = local_TIME_WAIT_TIMEOUT,
                  CM.outboundIdleTimeout = local_PROTOCOL_IDLE_TIMEOUT,
                  CM.connectionDataFlow  = ntcDataFlow,
                  CM.prunePolicy         = Diffusion.Policies.prunePolicy,
                  CM.stdGen              = cmLocalStdGen,
                  CM.connectionsLimits   = localConnectionLimits
              }

        CM.with
          localConnectionManagerArguments
          localConnectionHandler
          classifyHandleError
          (InResponderMode localInbInfoChannel)
          $ \localConnectionManager-> do
            --
            -- run local server
            --
            traceWith tracer . RunLocalServer
              =<< Snocket.getLocalAddr diNtcSnocket localSocket

            (Server.with
              Server.Arguments {
                  Server.sockets               = localSocket :| [],
                  Server.snocket               = diNtcSnocket,
                  Server.tracer                = dtLocalServerTracer,
                  Server.trTracer              = nullTracer, -- TODO: issue #3320
                  Server.debugInboundGovernor  = nullTracer,
                  Server.inboundGovernorTracer = dtLocalInboundGovernorTracer,
                  Server.inboundIdleTimeout    = Nothing,
                  Server.connectionLimits      = localConnectionLimits,
                  Server.connectionManager     = localConnectionManager,
                  Server.connectionDataFlow    = ntcDataFlow,
                  Server.inboundInfoChannel    = localInbInfoChannel
                })
              (\inboundGovernorThread _ -> Async.wait inboundGovernorThread)


    -- | mkRemoteThread - create remote connection manager

    mkRemoteThread :: ThreadId m -> m Void
    mkRemoteThread mainThreadId = do
      let
        exitPolicy :: ExitPolicy a
        exitPolicy = stdExitPolicy daReturnPolicy

      ipv4Address
        <- traverse (either (Snocket.getLocalAddr diNtnSnocket) pure)
                    daIPv4Address
      case ipv4Address of
        Just addr | Just IPv4Address <- diNtnAddressType addr
                  -> pure ()
                  | otherwise
                  -> throwIO (UnexpectedIPv4Address addr)
        Nothing   -> pure ()

      ipv6Address
        <- traverse (either (Snocket.getLocalAddr diNtnSnocket) pure)
                    daIPv6Address
      case ipv6Address of
        Just addr | Just IPv6Address <- diNtnAddressType addr
                  -> pure ()
                  | otherwise
                  -> throwIO (UnexpectedIPv6Address addr)
        Nothing   -> pure ()

      lookupReqs <- case (ipv4Address, ipv6Address) of
                           (Just _ , Nothing) -> return LookupReqAOnly
                           (Nothing, Just _ ) -> return LookupReqAAAAOnly
                           (Just _ , Just _ ) -> return LookupReqAAndAAAA
                           (Nothing, Nothing) -> throwIO NoSocket

      -- RNGs used for picking random peers from the ledger and for
      -- demoting/promoting peers.
      countersVar <- newTVarIO (emptyPeerSelectionCounters daEmptyExtraCounters)

      localRootsVar <- newTVarIO mempty

      peerSelectionTargetsVar <- newTVarIO daPeerSelectionTargets

      countersVar <- newTVarIO (emptyPeerSelectionCounters CPSV.empty)

      -- Design notes:
      --  - We split the following code into two parts:
      --    - Part (a): plumb data flow (in particular arguments and tracersr)
      --      and define common functions as a sequence of 'let's in which we
      --      define needed 'withXXX' functions (and similar) which
      --       - are used in Part (b),
      --       - handle the plumbing of tracers, and
      --       - capture commonalities between the two cases.
      --
      --    - Part (b): capturing the major control-flow of runM:
      --      in particular, two different case alternatives in which is captured
      --      the monadic flow of the program stripped down to its essence:
      ---     ```
      --       <setup...>
      --       case diffusionMode of
      --         InitiatorOnlyDiffusionMode -> ...
      --         InitiatorAndResponderDiffusionMode -> ...
      --      ```

      --
      -- Part (a): plumb data flow and define common functions
      --

      let connectionManagerArguments'
            :: forall handle handleError.
               PrunePolicy ntnAddr
            -> StdGen
            -> CM.Arguments
                 (ConnectionHandlerTrace ntnVersion ntnVersionData)
                 ntnFd ntnAddr handle handleError ntnVersion ntnVersionData m
          connectionManagerArguments' prunePolicy stdGen =
            CM.Arguments {
                CM.tracer              = dtConnectionManagerTracer,
                CM.trTracer            =
                  fmap CM.abstractState
                  `contramap` dtConnectionManagerTransitionTracer,
                CM.muxTracer           = dtMuxTracer,
                CM.ipv4Address,
                CM.ipv6Address,
                CM.addressType         = diNtnAddressType,
                CM.snocket             = diNtnSnocket,
                CM.makeBearer          = diNtnBearer,
                CM.configureSocket     = diNtnConfigureSocket,
                CM.connectionDataFlow  = diNtnDataFlow,
                CM.prunePolicy         = prunePolicy,
                CM.stdGen,
                CM.connectionsLimits   = daAcceptedConnectionsLimit,
                CM.timeWaitTimeout     = daTimeWaitTimeout,
                CM.outboundIdleTimeout = daProtocolIdleTimeout
            }

      let peerSelectionPolicy =
            simplePeerSelectionPolicy
              policyRngVar daPeerMetrics (epErrorDelay exitPolicy)

      let makeConnectionHandler'
            :: forall muxMode socket initiatorCtx responderCtx b c.
               SingMuxMode muxMode
            -> Versions ntnVersion ntnVersionData
                 (OuroborosBundle muxMode initiatorCtx responderCtx ByteString m b c)
            -> MuxConnectionHandler
                 muxMode socket initiatorCtx responderCtx ntnAddr
                 ntnVersion ntnVersionData ByteString m b c
          makeConnectionHandler' muxMode versions =
            makeConnectionHandler
              dtMuxTracer
              muxMode
              diNtnHandshakeArguments
              versions
              (mainThreadId, rethrowPolicy <> daRethrowPolicy)

          -- | Capture the two variations (InitiatorMode,InitiatorResponderMode) of
          --   withConnectionManager:

          withConnectionManagerInitiatorOnlyMode =
            CM.with
              (connectionManagerArguments' simplePrunePolicy cmStdGen1)
                 -- Server is not running, it will not be able to
                 -- advise which connections to prune.  It's also not
                 -- expected that the governor targets will be larger
                 -- than limits imposed by 'cmConnectionsLimits'.
              (makeConnectionHandler'
                SingInitiatorMode
                daApplicationInitiatorMode)
              classifyHandleError
              NotInResponderMode

          withConnectionManagerInitiatorAndResponderMode
            inbndInfoChannel =
              CM.with
                (connectionManagerArguments' Diffusion.Policies.prunePolicy cmStdGen2)
                (makeConnectionHandler'
                   SingInitiatorResponderMode
                   daApplicationInitiatorResponderMode)
                classifyHandleError
                (InResponderMode inbndInfoChannel)

      --
      -- peer state actions
      --
      -- Peer state actions run a job pool in the background which
      -- tracks threads forked by 'PeerStateActions'
      --

      let -- | parameterized version of 'withPeerStateActions'
          withPeerStateActions'
            :: forall (muxMode :: Mx.Mode) responderCtx socket b c.
               HasInitiator muxMode ~ True
            => MuxConnectionManager
                 muxMode socket (ExpandedInitiatorContext ntnAddr m)
                 responderCtx ntnAddr ntnVersionData ntnVersion
                 ByteString m a b
            -> (Governor.PeerStateActions
                  ntnAddr
                  (PeerConnectionHandle muxMode responderCtx ntnAddr
                     ntnVersionData ByteString m a b)
                  m
                -> m c)
            -> m c
          withPeerStateActions' connectionManager =
            withPeerStateActions
              PeerStateActionsArguments {
                    spsTracer = dtPeerSelectionActionsTracer,
                    spsDeactivateTimeout = Diffusion.Policies.deactivateTimeout,
                    spsCloseConnectionTimeout =
                      Diffusion.Policies.closeConnectionTimeout,
                    spsConnectionManager = connectionManager,
                    spsExitPolicy = exitPolicy,
                    spsRethrowPolicy = rethrowPolicy,
                    spsMainThreadId = mainThreadId
                  }

      dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore
      let dnsActions =
            PeerActionsDNS {
              paToPeerAddr = diNtnToPeerAddr
            , paDnsActions = diDnsActions lookupReqs
            }
      --
      -- Run peer selection (p2p governor)
      --
      let
          withPeerSelectionActions'
            :: m (Map ntnAddr PeerSharing)
            -> PeerStateActions
                     extraState
                     extraActions
                     extraPeers
                     extraFlags
                     extraAPI
                     extraCounters
                     CardanoPeerSelectionState
                     (CardanoPeerSelectionActions m)
                     (CardanoPublicRootPeers ntnAddr)
                     PeerTrustable
                     (CardanoLedgerPeersConsensusInterface m)
                     (CardanoPeerSelectionView ntnAddr)
                     ntnAddr
                     (PeerConnectionHandle
                        muxMode responderCtx ntnAddr ntnVersionData bytes m a b)
                     m
                -> m c)
            -> m c
          withPeerSelectionActions' readInboundPeers peerStateActions =
              withPeerSelectionActions dtTraceLocalRootPeersTracer
                                       localRootsVar
                                       dnsActions
                                       (\getLedgerPeers -> PeerSelectionActions {
                                         originalPeerSelectionTargets = daPeerSelectionTargets,
                                         readPeerSelectionTargets   = readTVar peerSelectionTargetsVar,
                                         getLedgerStateCtx          = daLedgerPeersCtx,
                                         readOriginalLocalRootPeers = daReadLocalRootPeers,
                                         readLocalRootPeers         = readTVar localRootsVar,
                                         peerSharing                = daOwnPeerSharing,
                                         peerConnToPeerSharing      = pchPeerSharing diNtnPeerSharing,
                                         requestPeerShare           =
                                           getPeerShare (readTVar (getPeerSharingRegistry daPeerSharingRegistry)),
                                         requestPublicRootPeers     = daRequestPublicRootPeers getLedgerPeers,
                                         readInboundPeers =
                                           case daOwnPeerSharing of
                                             PeerSharingDisabled -> pure Map.empty
                                             PeerSharingEnabled  -> readInboundPeers,
                                         readLedgerPeerSnapshot = daReadLedgerPeerSnapshot,
                                         extraActions = CardanoPeerSelectionActions {
                                           cpsaSyncPeerTargets       = caeSyncPeerTargets,
                                           cpsaReadUseBootstrapPeers = caeReadUseBootstrapPeers
                                         },
                                         peerStateActions
                                         extraStateToExtraCounters = cardanoPeerSelectionStatetoCounters,
                                       })
                                       WithLedgerPeersArgs {
                                         wlpRng                   = ledgerPeersRng,
                                         wlpConsensusInterface    = daLedgerPeersCtx,
                                         wlpTracer                = dtTraceLedgerPeersTracer,
                                         wlpGetUseLedgerPeers     = daReadUseLedgerPeers,
                                         wlpGetLedgerPeerSnapshot = daReadLedgerPeerSnapshot,
                                         wlpSemaphore             = dnsSemaphore
                                       }

          peerSelectionGovernorArgs
              daEmptyExtraState
              daEmptyExtraPeers
                 extraActions
                 extraPeers
                 extraAPI
                 extraFlags
                 extraCounters
                 ntnAddr
                 peerconn
              daPeerSelectionGovernorArgs
                 m
          peerSelectionGovernorArgs = PeerSelectionGovernorArgs {
            abortGovernor   = const Nothing
          , updateWithState = \_ _ -> pure ()
          , extraDecisions  = ExtraGuardedDecisions [] [] [] []
          }
            daPeerChurnGovernor
          peerSelectionGovernor'
            :: forall (muxMode :: Mx.Mode) b.
               Tracer m (DebugPeerSelection CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers ntnAddr) ntnAddr)
            -> StrictTVar m (PeerSelectionState CardanoPeerSelectionState PeerTrustable (CardanoPublicRootPeers ntnAddr) ntnAddr
                              (NodeToNodePeerConnectionHandle
                               muxMode ntnAddr ntnVersionData m a b))
            -> NodeToNodePeerSelectionActions (CardanoPeerSelectionActions m) (CardanoPublicRootPeers ntnAddr) PeerTrustable (CardanoLedgerPeersConsensusInterface m)
                                              muxMode ntnAddr ntnVersionData m a b
            -> NodeToNodePeerSelectionActions
                (CardanoPeerSelectionActions m)
                (CardanoPublicRootPeers ntnAddr)
                CardanoPeerSelectionState
                PeerTrustable
                (CardanoLedgerPeersConsensusInterface m)
                (CardanoPeerSelectionView ntnAddr)
                muxMode
                ntnAddr ntnVersionData m a b
            Governor.peerSelectionGovernor
              dtTracePeerSelectionTracer
              peerSelectionTracer
              dtTracePeerSelectionCounters
              peerSelectionGovernorArgs
              fuzzRng
              (CPST.empty caeConsensusMode caeMinBigLedgerPeersForTrustedState)
              CPRP.empty
              peerSelectionActions
              peerSelectionPolicy
              PeerSelectionInterfaces {
                countersVar,
                publicStateVar     = daPublicPeerSelectionVar,
                debugStateVar      = dbgVar,
                readUseLedgerPeers = daReadUseLedgerPeers
              }


      --
      -- The peer churn governor:
      --
      let peerChurnGovernor' =
            Governor.peerChurnGovernor
              PeerChurnArgs {
              , getExtraArgs           = daExtraChurnArgs
              , pcaChurnTracer         = dtTraceChurnCounters
              , pcaDeadlineInterval    = daDeadlineChurnInterval
              , pcaBulkInterval        = daBulkChurnInterval
              , pcaPeerRequestTimeout  = policyPeerShareOverallTimeout peerSelectionPolicy
              , pcaMetrics             = daPeerMetrics
              , pcaRng                 = churnRng
              , pcaPeerSelectionVar    = peerSelectionTargetsVar
              , pcaReadCounters        = readTVar countersVar
              , getLedgerStateCtx      = daLedgerPeersCtx
              , getLocalRootHotTarget  =
                   LocalRootPeers.hotTarget
                 . LocalRootPeers.fromGroups
                 <$> readTVar localRootsVar
              , getOriginalPeerTargets = daPeerSelectionTargets
              , getExtraArgs = ()
              }

      --
          debugStateVar <- newTVarIO $ emptyPeerSelectionState fuzzRng daEmptyExtraState daEmptyExtraPeers
      --
      let
          -- create sockets
          withSockets' f =
            withSockets tracer diNtnSnocket
              (\sock addr -> diNtnConfigureSocket sock (Just addr))
              (\sock addr -> diNtnConfigureSystemdSocket sock addr)
              ( catMaybes
                [ daIPv4Address
                , daIPv6Address
                ]
              )
              f

          -- run server
          withServer sockets connectionManager inboundInfoChannel =
            Server.with
              Server.Arguments {
                  Server.sockets               = sockets,
                  Server.snocket               = diNtnSnocket,
                  Server.tracer                = dtServerTracer,
                  Server.trTracer              = dtInboundGovernorTransitionTracer,
                  Server.debugInboundGovernor  = nullTracer,
                  Server.inboundGovernorTracer = dtInboundGovernorTracer,
                  debugStateVar <- newTVarIO $ emptyPeerSelectionState fuzzRng daEmptyExtraState daEmptyExtraPeers
                  Server.connectionManager     = connectionManager,
                  Server.connectionDataFlow    = diNtnDataFlow,
                  Server.inboundIdleTimeout    = Just daProtocolIdleTimeout,
                  Server.inboundInfoChannel    = inboundInfoChannel
                }

      --
      -- Part (b): capturing the major control-flow of runM:
      --
      case diffusionMode of

        -- InitiatorOnly mode, run peer selection only:
        InitiatorOnlyDiffusionMode ->
          withConnectionManagerInitiatorOnlyMode $ \connectionManager-> do
          debugStateVar <- newTVarIO $ emptyPeerSelectionState fuzzRng (CPST.empty caeConsensusMode caeMinBigLedgerPeersForTrustedState) CPRP.empty
          diInstallSigUSR1Handler connectionManager debugStateVar daPeerMetrics
          withPeerStateActions' connectionManager $ \peerStateActions->
            withPeerSelectionActions'
              (return Map.empty)
              peerStateActions $
              \(ledgerPeersThread, localRootPeersProvider) peerSelectionActions->
                Async.withAsync
                  (peerSelectionGovernor'
                    dtDebugPeerSelectionInitiatorTracer
                    debugStateVar
                    peerSelectionActions) $ \governorThread ->
                    Async.withAsync
                      peerChurnGovernor' $ \churnGovernorThread ->
                      -- wait for any thread to fail:
                      snd <$> Async.waitAny
                                [ledgerPeersThread, localRootPeersProvider, governorThread, churnGovernorThread]

        -- InitiatorAndResponder mode, run peer selection and the server:
        InitiatorAndResponderDiffusionMode -> do
          inboundInfoChannel  <- newInformationChannel
          withConnectionManagerInitiatorAndResponderMode
            inboundInfoChannel $ \connectionManager ->
              withSockets' $ \sockets addresses -> do
                withServer sockets connectionManager inboundInfoChannel $ \inboundGovernorThread readInboundState -> do
                  debugStateVar <- newTVarIO $ emptyPeerSelectionState fuzzRng (CPST.empty caeConsensusMode caeMinBigLedgerPeersForTrustedState) CPRP.empty
                  diInstallSigUSR1Handler connectionManager debugStateVar daPeerMetrics
                  withPeerStateActions' connectionManager $ \peerStateActions ->
                    withPeerSelectionActions'
                      (mkInboundPeersMap <$> readInboundState)
                      peerStateActions $
                        \(ledgerPeersThread, localRootPeersProvider) peerSelectionActions ->
                          Async.withAsync
                            (peerSelectionGovernor' dtDebugPeerSelectionInitiatorResponderTracer debugStateVar peerSelectionActions) $ \governorThread -> do
                              -- begin, unique to InitiatorAndResponder mode:
                              traceWith tracer (RunServer addresses)
                              -- end, unique to ...
                              Async.withAsync peerChurnGovernor' $ \churnGovernorThread ->
                                -- wait for any thread to fail:
                                snd <$> Async.waitAny [ledgerPeersThread, localRootPeersProvider, governorThread, churnGovernorThread, inboundGovernorThread]

-- | Main entry point for data diffusion service.  It allows to:
--
-- * connect to upstream peers;
-- * accept connection from downstream peers, if run in
--  'InitiatorAndResponderDiffusionMode'.
-- * runs a local service which allows to use node-to-client protocol to obtain
--   information from the running system.  This is used by 'cardano-cli' or
--   a wallet and a like local services.
--
run
    :: forall a .
       Tracers RemoteAddress NodeToNodeVersion
               LocalAddress  NodeToClientVersion
               IO
    -> TracersExtra RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
                    LocalAddress  NodeToClientVersion NodeToClientVersionData
                    IOException CardanoPeerSelectionState CardanoPeerSelectionState
                    PeerTrustable (CardanoPublicRootPeers RemoteAddress) IO
    -> Arguments IO
                    PeerTrustable (CardanoPublicRootPeers RemoteAddress)
                    (CardanoPeerSelectionView RemoteAddress)
                    IO
                 LocalSocket LocalAddress
    -> ArgumentsExtra (CardanoArgumentsExtra IO) PeerTrustable IO
    -> Applications
         RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress  NodeToClientVersion NodeToClientVersionData
        RemoteAddress
        NodeToNodeVersion
        NodeToNodeVersionData
        LocalAddress
        NodeToClientVersion
        NodeToClientVersionData
        extraAPI
        IO
        a
    -> ApplicationsExtra
        RemoteAddress
        IO
        a
    let tracer = dtDiffusionTracer tracers
run sigUSR1Signal tracers tracersExtra args argsExtra apps appsExtra = do
          HandshakeArguments {
              haHandshakeTracer = dtHandshakeTracer tracers,
              haHandshakeCodec  = NodeToNode.nodeToNodeHandshakeCodec,
              haVersionDataCodec =
                cborTermVersionDataCodec
                  NodeToNode.nodeToNodeCodecCBORTerm,
              haAcceptVersion = acceptableVersion,
              haQueryVersion = queryVersion,
              haTimeLimits = timeLimitsHandshake
            }
        diNtcHandshakeArguments =
          HandshakeArguments {
              haHandshakeTracer  = dtLocalHandshakeTracer tracers,
              haHandshakeCodec   = NodeToClient.nodeToClientHandshakeCodec,
              haVersionDataCodec =
                cborTermVersionDataCodec
                  NodeToClient.nodeToClientCodecCBORTerm,
              haAcceptVersion = acceptableVersion,
              haQueryVersion = queryVersion,
              haTimeLimits = noTimeLimitsHandshake
            }

        diInstallSigUSR1Handler
          :: forall mode x y ntnconn.
    -- We run two services: for /node-to-node/ and /node-to-client/.  The
    -- naming convention is that we use /local/ prefix for /node-to-client/
    -- related terms, as this is a local only service running over a unix
    -- socket / windows named pipe.
    handleJust (\e -> case fromException e :: Maybe ExitCode of
                  Nothing -> Just e
                  Just {} -> Nothing)
               (\e -> traceWith tracer (DiffusionErrored e)
                   >> throwIO (DiffusionError e))
         $ withIOManager $ \iocp -> do
             runM
               Interfaces {
                 diNtnSnocket                = Snocket.socketSnocket iocp,
                 diNtnBearer                 = makeSocketBearer,
                 diNtnConfigureSocket        = configureSocket,
                 diNtnConfigureSystemdSocket =
                   configureSystemdSocket
                     (SystemdSocketConfiguration `contramap` tracer),
                 diNtnAddressType       = socketAddressType,
                 diNtnDataFlow          = ntnDataFlow,
                 diNtnPeerSharing       = peerSharing,
                 diNtnToPeerAddr        = curry IP.toSockAddr,
                 diNtcSnocket           = Snocket.localSnocket iocp,
                 diNtcBearer            = makeLocalBearer,
                 diNtcGetFileDescriptor = localSocketFileDescriptor,
                 diDnsActions           = ioDNSActions,
                 diNtnHandshakeArguments,
                 diNtcHandshakeArguments,
                 diInstallSigUSR1Handler = sigUSR1Signal,
                 diRng,
                 diInstallSigUSR1Handler
                 diRng


--
--

-- | Node-To-Node protocol connections which negotiated
-- `InitiatorAndResponderDiffusionMode` are `Duplex`.
--
ntnDataFlow :: NodeToNodeVersionData -> DataFlow
ntnDataFlow NodeToNodeVersionData { diffusionMode } =
  case diffusionMode of
    InitiatorAndResponderDiffusionMode -> Duplex
    InitiatorOnlyDiffusionMode         -> Unidirectional


-- | All Node-To-Client protocol connections are considered 'Unidirectional'.
--
ntcDataFlow :: ntcVersionData -> DataFlow
ntcDataFlow _ = Unidirectional
