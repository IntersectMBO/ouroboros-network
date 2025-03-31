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

module Ouroboros.Network.Diffusion
  ( -- * Common API
    P2P (..)
  , P2PDecision (..)
  , ExtraTracers (..)
  , ArgumentsExtra (..)
  , Applications (..)
  , ApplicationsExtra (..)
    -- * Run data diffusion
  , run
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import Control.Exception (Exception, IOException)
import Data.Functor (void)
import Network.DNS (Resolver)
import Network.Mux qualified as Mx
import Network.Mux.Types
import Network.Socket (Socket)
import Ouroboros.Network.Diffusion.Common (Applications (..), Arguments,
           Tracers)
import Ouroboros.Network.Diffusion.NonP2P qualified as NonP2P
import Ouroboros.Network.Diffusion.P2P qualified as P2P
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionState)
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)

-- | Promoted data types.
--
data P2P = P2P        -- ^ General P2P mode. Can be instantiated with custom
                      -- data types
         | NonP2P     -- ^ Cardano non-P2P mode. Deprecated

-- | Auxiliary type to define arbitrary decision types based on type level
-- P2P
--
data P2PDecision (p2p :: P2P) a b where
  P2PDecision :: a
              -> P2PDecision 'P2P a b
  NonP2PDecision :: b
              -> P2PDecision 'NonP2P a b


-- | Tracers which depend on p2p mode.
--
data ExtraTracers (p2p :: P2P) extraState extraDebugState extraFlags extraPeers extraCounters m where
  P2PTracers
    :: P2P.TracersExtra
           RemoteAddress NodeToNodeVersion         NodeToNodeVersionData
           LocalAddress  NodeToClientVersion       NodeToClientVersionData
           IOException   extraState extraDebugState extraFlags extraPeers
           extraCounters m
    -> ExtraTracers 'P2P extraState extraDebugState extraFlags extraPeers extraCounters m

  NonP2PTracers
    :: NonP2P.TracersExtra
    -> ExtraTracers 'NonP2P extraState extraDebugState extraFlags extraPeers extraCounters m


-- | Diffusion arguments which depend on p2p mode.
--
-- TODO: do we need both `exception` and `resolverError`?
data ArgumentsExtra
       (p2p :: P2P)
       extraArgs extraState extraDebugState
       extraFlags extraPeers extraAPI
       extraChurnArgs extraCounters exception
       ntnAddr ntcAddr
       resolver resolverError m where
  P2PArguments
    :: P2P.ArgumentsExtra extraState extraDebugState
                          extraFlags extraPeers extraAPI extraChurnArgs
                          extraCounters exception ntnAddr ntcAddr resolver resolverError m
    -> ArgumentsExtra 'P2P extraArgs extraState extraDebugState
                           extraFlags extraPeers extraAPI extraChurnArgs
                           extraCounters exception ntnAddr ntcAddr resolver resolverError m

  NonP2PArguments
    :: NonP2P.ArgumentsExtra
    -> ArgumentsExtra 'NonP2P extraArgs extraState extraDebugState
                              extraFlags extraPeers extraAPI extraChurnArgs
                              extraCounters exception ntnAddr ntcAddr resolver resolverError m


-- | Application data which depend on p2p mode.
--
data ApplicationsExtra (p2p :: P2P) ntnAddr m a where
  P2PApplicationsExtra
    :: P2P.ApplicationsExtra ntnAddr m a
    -> ApplicationsExtra 'P2P ntnAddr m a

  NonP2PApplicationsExtra
    :: NonP2P.ApplicationsExtra
    -> ApplicationsExtra 'NonP2P ntnAddr m a


-- | Run data diffusion in either 'P2P' or 'NonP2P' mode.
--
run :: forall (p2p :: P2P) extraArgs extraState extraDebugState extraFlags
             extraPeers extraAPI extraChurnArgs extraCounters exception a.
      ( Monoid extraPeers
      , Eq extraCounters
      , Eq extraFlags
      , Exception exception
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
         RemoteAddress NodeToNodeVersion
         LocalAddress  NodeToClientVersion
         IO
    -> ExtraTracers p2p extraState extraDebugState extraFlags extraPeers extraCounters IO
    -> Arguments
         IO
         Socket      RemoteAddress
         LocalSocket LocalAddress
    -> ArgumentsExtra p2p
         extraArgs extraState extraDebugState
         extraFlags extraPeers extraAPI
         extraChurnArgs extraCounters exception
         RemoteAddress LocalAddress Resolver IOException IO
    -> Applications RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
                    LocalAddress  NodeToClientVersion NodeToClientVersionData
                    extraAPI IO a
    -> ApplicationsExtra p2p RemoteAddress IO a
    -> IO ()
run sigUSR1Signal
    tracers (P2PTracers tracersExtra)
            args (P2PArguments argsExtra)
            apps
            (P2PApplicationsExtra appsExtra) =
    void $
    P2P.run
      sigUSR1Signal tracers tracersExtra
      args argsExtra apps appsExtra
run _
    tracers (NonP2PTracers tracersExtra)
            args (NonP2PArguments argsExtra)
            apps
            (NonP2PApplicationsExtra appsExtra) = do
    NonP2P.run tracers tracersExtra
               args argsExtra
               apps appsExtra
