{-# LANGUAGE BlockArguments           #-}
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

-- | This module is expected to be imported qualified (it will clash
-- with the "Ouroboros.Network.Diffusion.NonP2P").
--
module Ouroboros.Network.Diffusion
  ( Tracers (..)
  , nullTracers
  , Arguments (..)
  , run
  , Interfaces (..)
  , runM
    -- * Re-exports
  , AbstractTransitionTrace
  , IG.RemoteTransitionTrace
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
import Network.Mux.Types
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
import Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
           NodeToClientVersionData)
import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode (NodeToNodeVersion (..),
           NodeToNodeVersionData (..), RemoteAddress)
import Ouroboros.Network.NodeToNode qualified as NodeToNode
import Ouroboros.Network.PeerSelection as PeerSelection
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.RootPeersDNS (PeerActionsDNS (..))
import Ouroboros.Network.PeerSelection.RootPeersDNS qualified as RootPeersDNS
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingRegistry (..))
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Version
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
    => -- | interfaces
       Interfaces ntnFd ntnAddr ntnVersion ntnVersionData
                  ntcFd ntcAddr ntcVersion ntcVersionData
                  resolver resolverError
                  extraState extraFlags extraPeers extraAPI m
    -> -- | tracers
       Tracers ntnAddr ntnVersion ntnVersionData
               ntcAddr ntcVersion ntcVersionData
               resolverError
               extraState extraDebugState extraFlags
               extraPeers extraCounters m
    -> -- | configuration
       Arguments extraState extraDebugState extraFlags extraPeers
                 extraAPI extraChurnArgs extraCounters exception
                 resolver resolverError
                 m ntnFd ntnAddr ntcFd ntcAddr

    -> -- | protocol handlers
       Applications ntnAddr ntnVersion ntnVersionData
                    ntcAddr ntcVersion ntcVersionData
                    extraAPI m a
    -> m Void
runM Interfaces
       { diNtnSnocket
       , diNtnBearer
       , diWithBuffer
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
       , diUpdateVersionData
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
     Arguments
       { daIPv4Address
       , daIPv6Address
       , daLocalAddress
       , daAcceptedConnectionsLimit
       , daMode = diffusionMode
       , daPublicPeerSelectionVar
       , daPeerSelectionTargets
       , daReadLocalRootPeers
       , daReadPublicRootPeers
       , daOwnPeerSharing
       , daReadUseLedgerPeers
       , daProtocolIdleTimeout
       , daTimeWaitTimeout
       , daDeadlineChurnInterval
       , daBulkChurnInterval
       , daReadLedgerPeerSnapshot
       , daEmptyExtraState
       , daEmptyExtraCounters
       , daExtraPeersAPI
       , daPeerSelectionGovernorArgs
       , daPeerSelectionStateToExtraCounters
       , daPeerChurnGovernor
       , daToExtraPeers
       , daRequestPublicRootPeers
       , daExtraChurnArgs
       , daMuxForkPolicy
       , daLocalMuxForkPolicy
       }
     Applications
       { daApplicationInitiatorMode
       , daApplicationInitiatorResponderMode
       , daLocalResponderApplication
       , daLedgerPeersCtx
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
      (case daLocalAddress of
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

            mkLocalConnectionHandler :: NodeToClientMkConnectionHandler
                                        ntcFd ntcAddr ntcVersion ntcVersionData m
            mkLocalConnectionHandler responderMuxChannelTracer =
              makeConnectionHandler
                dtLocalMuxTracer
                daLocalMuxForkPolicy
                diNtcHandshakeArguments
                ( ( \ (OuroborosApplication apps)
                   -> TemperatureBundle
                        (WithHot apps)
                        (WithWarm [])
                        (WithEstablished [])
                  ) <$> daLocalResponderApplication )
                (mainThreadId, rethrowPolicy <> daLocalRethrowPolicy)
                SingResponderMode
                responderMuxChannelTracer

            localWithConnectionManager responderInfoChannel connectionHandler k =
              CM.with CM.Arguments {
                  tracer              = dtLocalConnectionManagerTracer,
                  trTracer            = nullTracer, -- TODO: issue #3320
                  muxTracer           = dtLocalMuxTracer,
                  ipv4Address         = Nothing,
                  ipv6Address         = Nothing,
                  addressType         = const Nothing,
                  snocket             = diNtcSnocket,
                  makeBearer          = diNtcBearer,
                  withBuffer          = diWithBuffer,
                  configureSocket     = \_ _ -> return (),
                  timeWaitTimeout     = local_TIME_WAIT_TIMEOUT,
                  outboundIdleTimeout = local_PROTOCOL_IDLE_TIMEOUT,
                  connectionDataFlow  = ntcDataFlow,
                  prunePolicy         = Diffusion.Policies.prunePolicy,
                  stdGen              = cmLocalStdGen,
                  connectionsLimits   = localConnectionLimits,
                  updateVersionData   = \a _ -> a,
                  connStateIdSupply   = diConnStateIdSupply,
                  classifyHandleError
              }
              (InResponderMode responderInfoChannel)
              connectionHandler
              k

        traceWith tracer . RunLocalServer =<< Snocket.getLocalAddr diNtcSnocket localSocket
        Server.with
          Server.Arguments {
              sockets               = localSocket :| [],
              snocket               = diNtcSnocket,
              tracer                = dtLocalServerTracer,
              connectionLimits      = localConnectionLimits,
              inboundGovernorArgs   =
                IG.Arguments {
                  tracer = dtLocalInboundGovernorTracer,
                  transitionTracer = nullTracer,
                  debugTracer = nullTracer,
                  connectionDataFlow = ntcDataFlow,
                  idleTimeout = Nothing,
                  withConnectionManager = localWithConnectionManager localInbInfoChannel,
                  mkConnectionHandler = mkLocalConnectionHandler,
                  infoChannel = localInbInfoChannel } }
          (\inboundGovernorThread _ _ -> Async.wait inboundGovernorThread)


    -- | mkRemoteThread - create remote connection manager

    mkRemoteThread :: ThreadId m -> m Void
    mkRemoteThread mainThreadId = do
      labelThisThread "remote connection manager"
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
                           (Just _ , Nothing) -> return RootPeersDNS.LookupReqAOnly
                           (Nothing, Just _ ) -> return RootPeersDNS.LookupReqAAAAOnly
                           (Just _ , Just _ ) -> return RootPeersDNS.LookupReqAAndAAAA
                           (Nothing, Nothing) -> throwIO NoSocket

      -- RNGs used for picking random peers from the ledger and for
      -- demoting/promoting peers.
      policyRngVar <- newTVarIO policyRng

      localRootsVar <- newTVarIO mempty

      peerSelectionTargetsVar <- newTVarIO daPeerSelectionTargets

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
            :: forall muxMode handle b.
               PrunePolicy ntnAddr
            -> StdGen
            -> CM.Arguments
                 (ConnectionHandlerTrace ntnVersion ntnVersionData)
                 ntnFd ntnAddr handle (HandleError muxMode ntnVersion) ntnVersion ntnVersionData m a b
          connectionManagerArguments' prunePolicy stdGen =
            CM.Arguments {
                tracer              = dtConnectionManagerTracer,
                trTracer            =
                  fmap CM.abstractState
                  `contramap` dtConnectionManagerTransitionTracer,
                muxTracer           = dtMuxTracer,
                ipv4Address,
                ipv6Address,
                addressType         = diNtnAddressType,
                snocket             = diNtnSnocket,
                makeBearer          = diNtnBearer,
                withBuffer          = diWithBuffer,
                configureSocket     = diNtnConfigureSocket,
                connectionDataFlow  = diNtnDataFlow,
                prunePolicy         = prunePolicy,
                stdGen,
                connectionsLimits   = daAcceptedConnectionsLimit,
                timeWaitTimeout     = daTimeWaitTimeout,
                outboundIdleTimeout = daProtocolIdleTimeout,
                updateVersionData   = diUpdateVersionData,
                connStateIdSupply   = diConnStateIdSupply,
                classifyHandleError
            }

      let peerSelectionPolicy =
            simplePeerSelectionPolicy
              policyRngVar daPeerMetrics (epErrorDelay exitPolicy)

      let makeConnectionHandler'
            :: forall muxMode initiatorCtx responderCtx b c.
               Versions ntnVersion ntnVersionData
                 (OuroborosBundle muxMode initiatorCtx responderCtx ByteString m b c)
            -> SingMuxMode muxMode
            -> MkMuxConnectionHandler
                 muxMode ntnFd initiatorCtx responderCtx ntnAddr
                 ntnVersion ntnVersionData ByteString m b c
          makeConnectionHandler' versions singMuxMode =
            makeConnectionHandler
              dtMuxTracer
              daMuxForkPolicy
              diNtnHandshakeArguments
              versions
              (mainThreadId, rethrowPolicy <> daRethrowPolicy)
              singMuxMode

          -- | Capture the two variations (InitiatorMode,InitiatorResponderMode) of
          --   withConnectionManager:

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
                                         peerSelectionTargets = daPeerSelectionTargets,
                                         readPeerSelectionTargets   = readTVar peerSelectionTargetsVar,
                                         getLedgerStateCtx          = daLedgerPeersCtx,
                                         readLocalRootPeersFromFile = daReadLocalRootPeers,
                                         readLocalRootPeers         = readTVar localRootsVar,
                                         peerSharing                = daOwnPeerSharing,
                                         peerConnToPeerSharing      = pchPeerSharing diNtnPeerSharing,
                                         requestPeerShare           =
                                           requestPeerSharingResult (readTVar (getPeerSharingRegistry daPeerSharingRegistry)),
                                         requestPublicRootPeers     =
                                           case daRequestPublicRootPeers of
                                             Nothing ->
                                               PeerSelection.requestPublicRootPeers
                                                 dtTracePublicRootPeersTracer
                                                 daReadPublicRootPeers
                                                 dnsActions
                                                 dnsSemaphore
                                                 daToExtraPeers
                                                 getLedgerPeers
                                             Just requestPublicRootPeers' ->
                                               requestPublicRootPeers' dnsActions dnsSemaphore daToExtraPeers getLedgerPeers,
                                         readInboundPeers =
                                           case daOwnPeerSharing of
                                             PeerSharingDisabled -> pure Map.empty
                                             PeerSharingEnabled  -> readInboundPeers,
                                         readLedgerPeerSnapshot = daReadLedgerPeerSnapshot,
                                         extraPeersAPI             = daExtraPeersAPI,
                                         extraStateToExtraCounters = daPeerSelectionStateToExtraCounters,
                                         peerStateActions
                                       })
                                       WithLedgerPeersArgs {
                                         wlpRng                   = ledgerPeersRng,
                                         wlpConsensusInterface    = daLedgerPeersCtx,
                                         wlpTracer                = dtTraceLedgerPeersTracer,
                                         wlpGetUseLedgerPeers     = daReadUseLedgerPeers,
                                         wlpGetLedgerPeerSnapshot = daReadLedgerPeerSnapshot,
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
                publicStateVar     = daPublicPeerSelectionVar,
                debugStateVar      = dbgVar,
                readUseLedgerPeers = daReadUseLedgerPeers
              }


      --
      -- The peer churn governor:
      --
      let peerChurnGovernor' =
            daPeerChurnGovernor
              PeerChurnArgs {
                pcaPeerSelectionTracer = dtTracePeerSelectionTracer
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
                [ daIPv4Address
                , daIPv6Address
                ]
              )
              f

      --
      -- Part (b): capturing the major control-flow of runM:
      --
      case diffusionMode of

        -- InitiatorOnly mode, run peer selection only:
        InitiatorOnlyDiffusionMode ->
          let withConnectionManagerInitiatorOnlyMode k =
                CM.with
                  (connectionManagerArguments' simplePrunePolicy cmStdGen1)
                     -- Server is not running, it will not be able to
                     -- advise which connections to prune.  It's also not
                     -- expected that the governor targets will be larger
                     -- than limits imposed by 'cmConnectionsLimits'.
                  NotInResponderMode
                  mkConnectionHandler
                  k

              mkConnectionHandler =
                makeConnectionHandler' daApplicationInitiatorMode
                                       SingInitiatorMode in

          withConnectionManagerInitiatorOnlyMode $ \connectionManager -> do
            debugStateVar <- newTVarIO $ Governor.emptyPeerSelectionState fuzzRng daEmptyExtraState mempty
            diInstallSigUSR1Handler connectionManager debugStateVar daPeerMetrics
            withPeerStateActions' connectionManager $ \peerStateActions ->
              withPeerSelectionActions'
                (return Map.empty)
                peerStateActions
                \(ledgerPeersThread, localRootPeersProvider) peerSelectionActions ->
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
          inboundInfoChannel <- newInformationChannel
          let mkConnectionHandler =
                makeConnectionHandler' daApplicationInitiatorResponderMode

              -- bootstrap node-to-node server continuation
              withServer sockets =
                Server.with
                  Server.Arguments {
                    sockets               = sockets,
                    snocket               = diNtnSnocket,
                    tracer                = dtServerTracer,
                    connectionLimits      = daAcceptedConnectionsLimit,
                    inboundGovernorArgs   =
                      IG.Arguments {
                        tracer = dtInboundGovernorTracer,
                        transitionTracer = dtInboundGovernorTransitionTracer,
                        debugTracer = nullTracer,
                        connectionDataFlow = diNtnDataFlow,
                        idleTimeout = Just daProtocolIdleTimeout,
                        withConnectionManager =
                          withConnectionManagerInitiatorAndResponderMode inboundInfoChannel,
                        mkConnectionHandler = mkConnectionHandler SingInitiatorResponderMode diNtnDataFlow,
                        infoChannel = inboundInfoChannel } }

              -- bootstrap connection manager continuation
              withConnectionManagerInitiatorAndResponderMode
                responderInfoChannel connectionHandler k =
                  CM.with
                      (connectionManagerArguments'
                                                    Diffusion.Policies.prunePolicy
                                                    cmStdGen2
                                                    )
                      (InResponderMode responderInfoChannel)
                      connectionHandler
                      k
          --
          -- node-to-node sockets
          --
          withSockets' \sockets addresses -> do
            --
            -- node-to-node server
            --
            -- begin, unique to InitiatorAndResponder mode:
            traceWith tracer (RunServer addresses)
            -- end, unique to ...
            withServer sockets
              \inboundGovernorThread readInboundState connectionManager -> do
                debugStateVar <- newTVarIO $ Governor.emptyPeerSelectionState fuzzRng daEmptyExtraState mempty
                diInstallSigUSR1Handler connectionManager debugStateVar daPeerMetrics
                withPeerStateActions' connectionManager
                  \peerStateActions ->
                    withPeerSelectionActions'
                      (mkInboundPeersMap <$> readInboundState)
                      peerStateActions
                      \(ledgerPeersThread, localRootPeersProvider) peerSelectionActions ->
                        Async.withAsync
                          do
                            labelThisThread "Peer selection governor"
                            peerSelectionGovernor' dtDebugPeerSelectionInitiatorResponderTracer debugStateVar peerSelectionActions
                          \governorThread -> do
                            Async.withAsync
                              do
                                labelThisThread "Peer churn governor"
                                peerChurnGovernor'
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
       )
    => ( forall (mode :: Mx.Mode) x y.
         NodeToNodeConnectionManager mode Socket
            RemoteAddress NodeToNodeVersionData
            NodeToNodeVersion IO x y
       -> StrictTVar IO
            (PeerSelectionState extraState extraFlags extraPeers
                               RemoteAddress
                               (NodeToNodePeerConnectionHandle
                                   mode RemoteAddress
                                   NodeToNodeVersionData IO x y))
       -> PeerMetrics IO RemoteAddress
       -> IO ())
    -> Tracers
        RemoteAddress
        NodeToNodeVersion
        NodeToNodeVersionData
        LocalAddress
        NodeToClientVersion
        NodeToClientVersionData
        IOException
        extraState
        extraDebugState
        extraFlags
        extraPeers
        extraCounters
        IO
    -> Arguments
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
        LocalSocket
        LocalAddress
    -> Applications
        RemoteAddress
        NodeToNodeVersion
        NodeToNodeVersionData
        LocalAddress
        NodeToClientVersion
        NodeToClientVersionData
        extraAPI
        IO
        a
    -> IO Void
run sigUSR1Signal tracers args apps = do
    let tracer = dtDiffusionTracer tracers
        diNtnHandshakeArguments =
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

    diRng <- newStdGen
    diConnStateIdSupply <- atomically $ CM.newConnStateIdSupply Proxy

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
                 diWithBuffer                = withReadBufferIO,
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
                 diDnsActions           = RootPeersDNS.ioDNSActions,
                 diInstallSigUSR1Handler = sigUSR1Signal,
                 diNtnHandshakeArguments,
                 diNtcHandshakeArguments,
                 diRng,
                 diUpdateVersionData = \versionData diffusionMode -> versionData { diffusionMode },
                 diConnStateIdSupply
               }
               tracers args apps


--
-- Data flow
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
