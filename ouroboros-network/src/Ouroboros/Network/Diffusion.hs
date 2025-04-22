{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

-- | This module is expected to be imported qualified.
--
module Ouroboros.Network.Diffusion
  ( run
  , runM
  , mkInterfaces
  , socketAddressType
  , module Ouroboros.Network.Diffusion.Types
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
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.IP qualified as IP
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Typeable (Proxy (..), Typeable)
import Data.Void (Void)
import System.Exit (ExitCode)
import System.Random (StdGen, newStdGen, split)

import Network.DNS (Resolver)
import Network.Mux qualified as Mx
import Network.Mux.Bearer (withReadBufferIO)
import Network.Socket (Socket)
import Network.Socket qualified as Socket

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.InformationChannel
           (newInformationChannel)
import Ouroboros.Network.ConnectionManager.State qualified as CM
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context (ExpandedInitiatorContext)
import Ouroboros.Network.Diffusion.Configuration
import Ouroboros.Network.Diffusion.Policies (simplePeerSelectionPolicy)
import Ouroboros.Network.Diffusion.Policies qualified as Diffusion.Policies
import Ouroboros.Network.Diffusion.Types
import Ouroboros.Network.Diffusion.Utils
import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.InboundGovernor qualified as IG
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux hiding (MiniProtocol (..))
import Ouroboros.Network.MuxMode
import Ouroboros.Network.NodeToNode (RemoteAddress)
import Ouroboros.Network.PeerSelection as PeerSelection
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.RootPeersDNS (PeerActionsDNS (..))
import Ouroboros.Network.PeerSelection.RootPeersDNS qualified as RootPeersDNS
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingRegistry (..))
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.RethrowPolicy
import Ouroboros.Network.Server qualified as Server
import Ouroboros.Network.Snocket (LocalAddress, LocalSocket (..),
           localSocketFileDescriptor, makeLocalBearer, makeSocketBearer)
import Ouroboros.Network.Snocket qualified as Snocket
import Ouroboros.Network.Socket (configureSocket, configureSystemdSocket)


socketAddressType :: Socket.SockAddr -> Maybe AddressType
socketAddressType Socket.SockAddrInet {}  = Just IPv4Address
socketAddressType Socket.SockAddrInet6 {} = Just IPv6Address
socketAddressType Socket.SockAddrUnix {}  = Nothing


runM
    :: forall m ntnFd ntnAddr ntnVersion ntnVersionData
                ntcFd ntcAddr ntcVersion ntcVersionData
                resolver resolverError exception a
                extraState extraDebugState extraPeers
                extraAPI extraFlags extraChurnArgs extraCounters .

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
       , MonadTimer       m
       , MonadMVar        m
       , Typeable  ntnAddr
       , Ord       ntnAddr
       , Show      ntnAddr
       , Hashable  ntnAddr
       , Typeable  ntnVersion
       , Ord       ntnVersion
       , Show      ntnVersion
       , Show      ntnVersionData
       , Typeable  ntcAddr
       , Ord       ntcAddr
       , Show      ntcAddr
       , Ord       ntcVersion
       , Exception resolverError
       , Monoid extraPeers
       , Eq extraFlags
       , Eq extraCounters
       , Exception exception
       )
    => -- | arguments
       Arguments extraState extraDebugState extraFlags
                 extraPeers extraAPI extraChurnArgs
                 extraCounters exception resolver
                 resolverError m ntnFd
                 ntnAddr ntnVersion ntnVersionData
                 ntcAddr ntcVersion ntcVersionData
      -- | interfaces
    -> Interfaces ntnFd ntnAddr ntcFd ntcAddr
                  resolver resolverError m
    -> -- | tracers
       Tracers ntnAddr ntnVersion ntnVersionData
               ntcAddr ntcVersion ntcVersionData
               resolverError
               extraState extraDebugState extraFlags
               extraPeers extraCounters m
    -> -- | configuration
       Configuration extraFlags m ntnFd ntnAddr ntcFd ntcAddr

    -> -- | protocol handlers
       Applications ntnAddr ntnVersion ntnVersionData
                    ntcAddr ntcVersion ntcVersionData
                    m a
    -> m Void
runM Arguments
       { daNtnDataFlow
       , daNtnPeerSharing
       , daUpdateVersionData
       , daNtnHandshakeArguments
       , daNtcHandshakeArguments
       , daLedgerPeersCtx
       , daEmptyExtraState
       , daEmptyExtraCounters
       , daExtraPeersAPI
       , daInstallSigUSR1Handler
       , daPeerSelectionGovernorArgs
       , daPeerSelectionStateToExtraCounters
       , daToExtraPeers
       , daRequestPublicRootPeers
       , daPeerChurnGovernor
       , daExtraChurnArgs
       }
     Interfaces
       { diNtnSnocket
       , diNtnBearer
       , diWithBuffer
       , diNtnConfigureSocket
       , diNtnConfigureSystemdSocket
       , diNtnAddressType
       , diNtnToPeerAddr
       , diNtcSnocket
       , diNtcBearer
       , diNtcGetFileDescriptor
       , diRng
       , diDnsActions
       , diConnStateIdSupply
       }
     Tracers
       { dtMuxTracer
       , dtLocalMuxTracer
       , dtDiffusionTracer = tracer
       , dtTracePeerSelectionTracer
       , dtTraceChurnCounters
       , dtDebugPeerSelectionInitiatorTracer
       , dtDebugPeerSelectionInitiatorResponderTracer
       , dtTracePeerSelectionCounters
       , dtPeerSelectionActionsTracer
       , dtTraceLocalRootPeersTracer
       , dtTracePublicRootPeersTracer
       , dtTraceLedgerPeersTracer
       , dtConnectionManagerTracer
       , dtConnectionManagerTransitionTracer
       , dtServerTracer
       , dtInboundGovernorTracer
       , dtInboundGovernorTransitionTracer
       , dtLocalConnectionManagerTracer
       , dtLocalServerTracer
       , dtLocalInboundGovernorTracer
       , dtDnsTracer
       }
     Configuration
       { dcIPv4Address
       , dcIPv6Address
       , dcLocalAddress
       , dcAcceptedConnectionsLimit
       , dcMode = diffusionMode
       , dcPublicPeerSelectionVar
       , dcPeerSelectionTargets
       , dcReadLocalRootPeers
       , dcReadPublicRootPeers
       , dcOwnPeerSharing
       , dcReadUseLedgerPeers
       , dcProtocolIdleTimeout
       , dcTimeWaitTimeout
       , dcDeadlineChurnInterval
       , dcBulkChurnInterval
       , dcReadLedgerPeerSnapshot
       , dcMuxForkPolicy
       , dcLocalMuxForkPolicy
       }
     Applications
       { daApplicationInitiatorMode
       , daApplicationInitiatorResponderMode
       , daLocalResponderApplication
       , daRethrowPolicy
       , daLocalRethrowPolicy
       , daReturnPolicy
       , daPeerMetrics
       , daPeerSharingRegistry
       }
  = do
    -- Thread to which 'RethrowPolicy' will throw fatal exceptions.
    mainThreadId <- myThreadId

    -- If we have a local address, race the remote and local threads. Otherwise
    -- just launch the remote thread.
    mkRemoteThread mainThreadId &
      (case dcLocalAddress of
         Nothing -> id
         Just addr -> (fmap (either id id) . (`Async.race` mkLocalThread mainThreadId addr))
      )

  where
    (ledgerPeersRng, rng1) = split diRng
    (policyRng,      rng2) = split rng1
    (churnRng,       rng3) = split rng2
    (fuzzRng,        rng4) = split rng3
    (cmLocalStdGen,  rng5) = split rng4
    (cmStdGen1,      rng6) = split rng5
    (cmStdGen2, peerSelectionActionsRng) = split rng6

    mkInboundPeersMap :: IG.PublicState ntnAddr ntnVersionData
                      -> Map ntnAddr PeerSharing
    mkInboundPeersMap
      IG.PublicState { IG.inboundDuplexPeers }
      =
      Map.map daNtnPeerSharing inboundDuplexPeers

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
      -- IOError rethrow-policy
      --
      -- After a critical bug, we decided that `IOError` policy should only
      -- kill the connection which thrown it.  `IOError`s are not propagated.
      -- There's a risk that one could arm an attack if one discovers
      -- a mechanism to trigger fatal `IOError`s, e.g. through a kernel bug.
      --
      -- It is responsibility for an SPO to monitor the node if it is making
      -- progress and have enough resources to do so, e.g. if it has enough
      -- memory, file descriptors.
      --
      -- The `ouroboros-network` guarantees running on a fixed number of file
      -- descriptors given a topology file, see
      -- https://github.com/IntersectMBO/ouroboros-network/issues/4585#issuecomment-1591777447
      -- There's also a calculation for `ouroboros-consensus`, see
      -- https://github.com/IntersectMBO/ouroboros-consensus/issues/20#issuecomment-1514554680
      -- File descriptors could be drained by the tracing system in
      -- `cardano-node` (such a bug existed), or even an external process.
      --
      RethrowPolicy (\_ctx err ->
        case fromException err :: Maybe IOException of
          Just {} -> mempty
          Nothing -> mempty)
      <>
      RethrowPolicy (\ctx err -> case  (ctx, fromException err) of
                        (OutboundError, Just Mx.UnknownMiniProtocol {})
                          -> ShutdownPeer
                        _ -> mempty)


    -- | mkLocalThread - create local connection manager

    mkLocalThread :: ThreadId m -> Either ntcFd ntcAddr -> m Void
    mkLocalThread mainThreadId localAddr = do
     labelThisThread "local connection manager"
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
                dcLocalMuxForkPolicy
                daNtcHandshakeArguments
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
                  CM.withBuffer          = diWithBuffer,
                  CM.configureSocket     = \_ _ -> return (),
                  CM.timeWaitTimeout     = local_TIME_WAIT_TIMEOUT,
                  CM.outboundIdleTimeout = local_PROTOCOL_IDLE_TIMEOUT,
                  CM.connectionDataFlow  = ntcDataFlow,
                  CM.prunePolicy         = Diffusion.Policies.prunePolicy,
                  CM.stdGen              = cmLocalStdGen,
                  CM.connectionsLimits   = localConnectionLimits,
                  CM.updateVersionData   = \a _ -> a,
                  CM.connStateIdSupply   = diConnStateIdSupply
              }

        CM.with
          localConnectionManagerArguments
          localConnectionHandler
          classifyHandleError
          (InResponderMode localInbInfoChannel)
          $ \localConnectionManager-> do
            --
            -- run node-to-client server
            --
            traceWith tracer . RunLocalServer
              =<< Snocket.getLocalAddr diNtcSnocket localSocket

            Server.with
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
                }
              (\inboundGovernorThread _ -> Async.wait inboundGovernorThread)


    -- | mkRemoteThread - create remote connection manager

    mkRemoteThread :: ThreadId m -> m Void
    mkRemoteThread mainThreadId = do
      labelThisThread "remote connection manager"
      let
        exitPolicy :: ExitPolicy a
        exitPolicy = stdExitPolicy daReturnPolicy

      ipv4Address
        <- traverse (either (Snocket.getLocalAddr diNtnSnocket) pure)
                    dcIPv4Address
      case ipv4Address of
        Just addr | Just IPv4Address <- diNtnAddressType addr
                  -> pure ()
                  | otherwise
                  -> throwIO (UnexpectedIPv4Address addr)
        Nothing   -> pure ()

      ipv6Address
        <- traverse (either (Snocket.getLocalAddr diNtnSnocket) pure)
                    dcIPv6Address
      case ipv6Address of
        Just addr | Just IPv6Address <- diNtnAddressType addr
                  -> pure ()
                  | otherwise
                  -> throwIO (UnexpectedIPv6Address addr)
        Nothing   -> pure ()

      lookupReqs <- case (ipv4Address, ipv6Address) of
                           (Just _ , Nothing) -> return RootPeersDNS.LookupReqAOnly
                           (Nothing, Just _ ) -> return RootPeersDNS.LookupReqAAAAOnly
                           (Just _ , Just _ ) -> return RootPeersDNS.LookupReqAAndAAAA
                           (Nothing, Nothing) -> throwIO NoSocket

      -- RNGs used for picking random peers from the ledger and for
      -- demoting/promoting peers.
      policyRngVar <- newTVarIO policyRng

      localRootsVar <- newTVarIO mempty

      peerSelectionTargetsVar <- newTVarIO dcPeerSelectionTargets

      countersVar <- newTVarIO (Governor.emptyPeerSelectionCounters daEmptyExtraCounters)

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
                CM.withBuffer          = diWithBuffer,
                CM.configureSocket     = diNtnConfigureSocket,
                CM.connectionDataFlow  = daNtnDataFlow,
                CM.prunePolicy         = prunePolicy,
                CM.stdGen,
                CM.connectionsLimits   = dcAcceptedConnectionsLimit,
                CM.timeWaitTimeout     = dcTimeWaitTimeout,
                CM.outboundIdleTimeout = dcProtocolIdleTimeout,
                CM.updateVersionData   = daUpdateVersionData,
                CM.connStateIdSupply   = diConnStateIdSupply
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
              dcMuxForkPolicy
              daNtnHandshakeArguments
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

      dnsSemaphore <- RootPeersDNS.newLedgerAndPublicRootDNSSemaphore
      let dnsActions =
            PeerActionsDNS {
              paToPeerAddr = diNtnToPeerAddr
            , paDnsActions = diDnsActions dtDnsTracer lookupReqs diNtnToPeerAddr
            }
      --
      -- Run peer selection (p2p governor)
      --
      let
          withPeerSelectionActions'
            :: m (Map ntnAddr PeerSharing)
            -> PeerStateActions
                 ntnAddr
                 (PeerConnectionHandle
                    muxMode responderCtx ntnAddr ntnVersionData bytes m a b)
                 m
            -> ((Async m Void, Async m Void)
                -> PeerSelectionActions
                     extraState
                     extraFlags
                     extraPeers
                     extraAPI
                     extraCounters
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
                                         peerSelectionTargets = dcPeerSelectionTargets,
                                         readPeerSelectionTargets   = readTVar peerSelectionTargetsVar,
                                         getLedgerStateCtx          = daLedgerPeersCtx,
                                         readLocalRootPeersFromFile = dcReadLocalRootPeers,
                                         readLocalRootPeers         = readTVar localRootsVar,
                                         peerSharing                = dcOwnPeerSharing,
                                         peerConnToPeerSharing      = pchPeerSharing daNtnPeerSharing,
                                         requestPeerShare           =
                                           requestPeerSharingResult (readTVar (getPeerSharingRegistry daPeerSharingRegistry)),
                                         requestPublicRootPeers     =
                                           case daRequestPublicRootPeers of
                                             Nothing ->
                                               PeerSelection.requestPublicRootPeers
                                                 dtTracePublicRootPeersTracer
                                                 dcReadPublicRootPeers
                                                 dnsActions
                                                 dnsSemaphore
                                                 daToExtraPeers
                                                 getLedgerPeers
                                             Just requestPublicRootPeers' ->
                                               requestPublicRootPeers' dnsActions dnsSemaphore daToExtraPeers getLedgerPeers,
                                         readInboundPeers =
                                           case dcOwnPeerSharing of
                                             PeerSharingDisabled -> pure Map.empty
                                             PeerSharingEnabled  -> readInboundPeers,
                                         readLedgerPeerSnapshot = dcReadLedgerPeerSnapshot,
                                         extraPeersAPI             = daExtraPeersAPI,
                                         extraStateToExtraCounters = daPeerSelectionStateToExtraCounters,
                                         peerStateActions
                                       })
                                       WithLedgerPeersArgs {
                                         wlpRng                   = ledgerPeersRng,
                                         wlpConsensusInterface    = daLedgerPeersCtx,
                                         wlpTracer                = dtTraceLedgerPeersTracer,
                                         wlpGetUseLedgerPeers     = dcReadUseLedgerPeers,
                                         wlpGetLedgerPeerSnapshot = dcReadLedgerPeerSnapshot,
                                         wlpSemaphore             = dnsSemaphore
                                       }
                                       peerSelectionActionsRng

          peerSelectionGovernor'
            :: Tracer m (DebugPeerSelection extraState extraFlags extraPeers ntnAddr)
            -> StrictTVar m (PeerSelectionState extraState extraFlags extraPeers ntnAddr
                (PeerConnectionHandle muxMode responderCtx ntnAddr ntnVersionData ByteString m a b))
            -> PeerSelectionActions
                extraState extraFlags extraPeers
                extraAPI extraCounters ntnAddr
                (PeerConnectionHandle muxMode responderCtx ntnAddr ntnVersionData ByteString m a b)
                m
            -> m Void
          peerSelectionGovernor' peerSelectionTracer dbgVar peerSelectionActions =
            Governor.peerSelectionGovernor
              dtTracePeerSelectionTracer
              peerSelectionTracer
              dtTracePeerSelectionCounters
              daPeerSelectionGovernorArgs
              fuzzRng
              daEmptyExtraState
              mempty
              peerSelectionActions
              peerSelectionPolicy
              PeerSelectionInterfaces {
                countersVar,
                publicStateVar     = dcPublicPeerSelectionVar,
                debugStateVar      = dbgVar,
                readUseLedgerPeers = dcReadUseLedgerPeers
              }


      --
      -- The peer churn governor:
      --
      let peerChurnGovernor' =
            daPeerChurnGovernor
              PeerChurnArgs {
                pcaPeerSelectionTracer = dtTracePeerSelectionTracer
              , pcaChurnTracer         = dtTraceChurnCounters
              , pcaDeadlineInterval    = dcDeadlineChurnInterval
              , pcaBulkInterval        = dcBulkChurnInterval
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
              , getOriginalPeerTargets = dcPeerSelectionTargets
              , getExtraArgs           = daExtraChurnArgs
              }

      --
      -- Two functions only used in InitiatorAndResponder mode
      --
      let
          -- create sockets
          withSockets' f =
            withSockets tracer diNtnSnocket
              (\sock addr -> diNtnConfigureSocket sock (Just addr))
              (\sock addr -> diNtnConfigureSystemdSocket sock addr)
              ( catMaybes
                [ dcIPv4Address
                , dcIPv6Address
                ]
              )
              f

          -- run node-to-node server
          withServer sockets connectionManager inboundInfoChannel =
            Server.with
              Server.Arguments {
                  Server.sockets               = sockets,
                  Server.snocket               = diNtnSnocket,
                  Server.tracer                = dtServerTracer,
                  Server.trTracer              = dtInboundGovernorTransitionTracer,
                  Server.debugInboundGovernor  = nullTracer,
                  Server.inboundGovernorTracer = dtInboundGovernorTracer,
                  Server.connectionLimits      = dcAcceptedConnectionsLimit,
                  Server.connectionManager     = connectionManager,
                  Server.connectionDataFlow    = daNtnDataFlow,
                  Server.inboundIdleTimeout    = Just dcProtocolIdleTimeout,
                  Server.inboundInfoChannel    = inboundInfoChannel
                }

      --
      -- Part (b): capturing the major control-flow of runM:
      --
      case diffusionMode of

        -- InitiatorOnly mode, run peer selection only:
        InitiatorOnlyDiffusionMode ->
          withConnectionManagerInitiatorOnlyMode $ \connectionManager-> do
          debugStateVar <- newTVarIO $ Governor.emptyPeerSelectionState fuzzRng daEmptyExtraState mempty
          daInstallSigUSR1Handler connectionManager debugStateVar daPeerMetrics
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
              --
              -- node-to-node sockets
              --
              withSockets' $ \sockets addresses -> do
                --
                -- node-to-node server
                --
                withServer sockets connectionManager inboundInfoChannel $
                  \inboundGovernorThread readInboundState -> do
                    debugStateVar <- newTVarIO $ Governor.emptyPeerSelectionState fuzzRng daEmptyExtraState mempty
                    daInstallSigUSR1Handler connectionManager debugStateVar daPeerMetrics
                    withPeerStateActions' connectionManager $
                      \peerStateActions ->
                        withPeerSelectionActions'
                          (mkInboundPeersMap <$> readInboundState)
                          peerStateActions $
                            \(ledgerPeersThread, localRootPeersProvider) peerSelectionActions ->
                              Async.withAsync
                                (do
                                  labelThisThread "Peer selection governor"
                                  peerSelectionGovernor' dtDebugPeerSelectionInitiatorResponderTracer debugStateVar peerSelectionActions) $
                                    \governorThread -> do
                                      -- begin, unique to InitiatorAndResponder mode:
                                      traceWith tracer (RunServer addresses)
                                      -- end, unique to ...
                                      Async.withAsync (do
                                                          labelThisThread "Peer churn governor"
                                                          peerChurnGovernor') $
                                        \churnGovernorThread ->
                                          -- wait for any thread to fail:
                                          snd <$> Async.waitAny [ ledgerPeersThread
                                                                , localRootPeersProvider
                                                                , governorThread
                                                                , churnGovernorThread
                                                                , inboundGovernorThread
                                                                ]

-- | Main entry point for data diffusion service.  It allows to:
--
-- * connect to upstream peers;
-- * accept connection from downstream peers, if run in
--  'InitiatorAndResponderDiffusionMode'.
-- * runs a local service which allows to use node-to-client protocol to obtain
--   information from the running system.  This is used by 'cardano-cli' or
--   a wallet and a like local services.
--
run :: ( Monoid extraPeers
       , Eq extraFlags
       , Eq extraCounters
       , Exception exception
       , Typeable ntnVersion
       , Ord ntnVersion
       , Show ntnVersion
       , Show ntnVersionData
       , Ord ntcVersion
       )
    => Arguments
        extraState
        extraDebugState
        extraFlags
        extraPeers
        extraAPI
        extraChurnArgs
        extraCounters
        exception
        Resolver
        IOException
        IO
        Socket
        RemoteAddress
        ntnVersion
        ntnVersionData
        LocalAddress
        ntcVersion
        ntcVersionData
    -> Tracers
        RemoteAddress
        ntnVersion
        ntnVersionData
        LocalAddress
        ntcVersion
        ntcVersionData
        IOException
        extraState
        extraDebugState
        extraFlags
        extraPeers
        extraCounters
        IO
    -> Configuration
        extraFlags
        IO
        Socket
        RemoteAddress
        LocalSocket
        LocalAddress
    -> Applications
        RemoteAddress
        ntnVersion
        ntnVersionData
        LocalAddress
        ntcVersion
        ntcVersionData
        IO
        a
    -> IO Void
run extraParams tracers args apps = do
    let tracer = dtDiffusionTracer tracers

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

             interfaces <- mkInterfaces iocp tracer

             runM
               extraParams
               interfaces
               tracers args apps

--
-- Interfaces
--

mkInterfaces :: IOManager
             -> Tracer IO (DiffusionTracer ntnAddr ntcAddr)
             -> IO (Interfaces Socket
                               RemoteAddress
                               LocalSocket
                               LocalAddress
                               Resolver
                               IOException
                               IO)
mkInterfaces iocp tracer = do

  diRng <- newStdGen
  diConnStateIdSupply <- atomically $ CM.newConnStateIdSupply Proxy

  return $ Interfaces {
    diNtnSnocket                = Snocket.socketSnocket iocp,
    diNtnBearer                 = makeSocketBearer,
    diWithBuffer                = withReadBufferIO,
    diNtnConfigureSocket        = configureSocket,
    diNtnConfigureSystemdSocket =
      configureSystemdSocket
        (SystemdSocketConfiguration `contramap` tracer),
    diNtnAddressType       = socketAddressType,
    diNtnToPeerAddr        = curry IP.toSockAddr,
    diNtcSnocket           = Snocket.localSnocket iocp,
    diNtcBearer            = makeLocalBearer,
    diNtcGetFileDescriptor = localSocketFileDescriptor,
    diDnsActions           = RootPeersDNS.ioDNSActions,
    diRng,
    diConnStateIdSupply
  }

--
-- Data flow
--

-- | All Node-To-Client protocol connections are considered 'Unidirectional'.
--
ntcDataFlow :: ntcVersionData -> DataFlow
ntcDataFlow _ = Unidirectional
