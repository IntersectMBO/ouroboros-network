{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Diffusion
  (
  -- * Common API
    DiffusionTracers
  , DiffusionArguments
  , getDiffusionArguments
  , daDiffusionMode
  , DiffusionApplications
  , runDataDiffusion
  , DiffusionInitializationTracer(..)
  , DiffusionFailure
  -- * Non P2P API
  , nullTracersNonP2P
  , mkDiffusionTracersNonP2P
  , mkDiffusionArgumentsNonP2P
  , mkDiffusionApplicationsNonP2P
  -- * P2P API
  , nullTracersP2P
  , mkDiffusionTracersP2P
  , mkDiffusionArgumentsP2P
  , mkDiffusionApplicationsP2P
  )
  where

import           Control.Monad.Class.MonadSTM (STM)
import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)
import           Data.Map.Strict (Map)
import           Data.Time (DiffTime)
import           Data.Functor (void)
import           Control.Exception (IOException)
import           Control.Tracer (Tracer)

import           Network.Mux (WithMuxBearer, MuxTrace)
import           Network.Socket (SockAddr, Socket, AddrInfo)

import           Ouroboros.Network.PeerSelection.RootPeersDNS
                  ( TracePublicRootPeers
                  , TraceLocalRootPeers
                  )
import           Ouroboros.Network.PeerSelection.Governor.Types
                  ( TracePeerSelection
                  , DebugPeerSelection
                  , PeerSelectionCounters
                  )
import           Ouroboros.Network.ConnectionManager.Types
                  ( ConnectionManagerTrace
                  )
import           Ouroboros.Network.PeerSelection.PeerStateActions
                  ( PeerSelectionActionsTrace
                  )
import           Ouroboros.Network.ConnectionHandler
                  ( ConnectionHandlerTrace
                  )
import           Ouroboros.Network.Server2
                  ( ServerTrace
                  )
import           Ouroboros.Network.InboundGovernor
                  ( InboundGovernorTrace
                  )

import           Ouroboros.Network.NodeToNode
                  ( RemoteAddress
                  , NodeToNodeVersionData
                  , DiffusionMode
                  , NodeToNodeVersion
                  , MiniProtocolParameters
                  , AcceptedConnectionsLimit
                  , IPSubscriptionTarget
                  , DnsSubscriptionTarget
                  )
import qualified Ouroboros.Network.NodeToNode as NTN
import           Ouroboros.Network.NodeToClient
                  ( LocalAddress
                  , NodeToClientVersionData
                  , Versions
                  , ConnectionId
                  , NodeToClientVersion
                  )
import qualified Ouroboros.Network.NodeToClient as NTC

import           Ouroboros.Network.RethrowPolicy (RethrowPolicy)
import           Ouroboros.Network.BlockFetch (FetchMode)
import           Ouroboros.Network.Mux
                  ( Bundle
                  , MiniProtocol
                  , MuxMode (..)
                  , OuroborosApplication
                  , ControlMessage
                  )
import           Ouroboros.Network.PeerSelection.LedgerPeers
                  ( LedgerPeersConsensusInterface
                  , TraceLedgerPeers
                  , RelayAddress
                  , UseLedgerAfter
                  )
import           Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)

import qualified Ouroboros.Network.Diffusion.Common as Common
import           Ouroboros.Network.Diffusion.Common
                  ( DiffusionInitializationTracer
                  , DiffusionFailure
                  , daDiffusionMode
                  , dtExtra
                  , daExtra
                  , dapExtra
                  )
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P

-- | DiffusionTracers for either P2P or Non-P2P node
--
newtype DiffusionTracers =
    DiffusionTracers
      (Common.DiffusionTracers
        (Either NonP2P.DiffusionTracersExtra P2P.DiffusionTracersExtra))

-- | NonP2P null tracers
--
nullTracersNonP2P :: DiffusionTracers
nullTracersNonP2P =
  DiffusionTracers (Common.nullTracers (Left NonP2P.nullTracers))

-- | P2P null tracers
--
nullTracersP2P :: DiffusionTracers
nullTracersP2P =
  DiffusionTracers (Common.nullTracers (Right P2P.nullTracers))

-- | DiffusionArguments for either P2P or Non-P2P node
--
newtype DiffusionArguments m =
  DiffusionArguments
    { getDiffusionArguments
        :: Common.DiffusionArguments
            (Either NonP2P.DiffusionArgumentsExtra
                    (P2P.DiffusionArgumentsExtra m))
    }

-- | DiffusionApplications for either P2P or Non-P2P node
--
newtype DiffusionApplications ntnAddr ntcAddr ntnVersionData ntcVersionData m =
  DiffusionApplications
   (Common.DiffusionApplications
     (Either
        NonP2P.DiffusionApplicationsExtra
        (P2P.DiffusionApplicationsExtra ntnAddr m))
     ntnAddr
     ntcAddr
     ntnVersionData
     ntcVersionData
     m
   )

-- | Construct a value of NonP2P DiffusionArguments data type.
-- 'ouroboros-consensus' needs access to this constructor so we export this
-- function in order to avoid exporting the P2P and NonP2P internal modules.
--
mkDiffusionArgumentsNonP2P
  :: Maybe (Either Socket AddrInfo)
  -> Maybe (Either Socket AddrInfo)
  -> Maybe (Either Socket FilePath)
  -> AcceptedConnectionsLimit
  -> DiffusionMode
  -> IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> DiffusionArguments m
mkDiffusionArgumentsNonP2P
  a1 a2 a3 a4 a5 a6
  a7 =
    DiffusionArguments
    $ Common.DiffusionArguments
      a1 a2 a3 a4 a5
    $ Left
    $ NonP2P.DiffusionArgumentsExtra
      a6 a7

-- | Construct a value of P2P DiffusionArguments data type.
-- 'ouroboros-consensus' needs access to this constructor so we export this
-- function in order to avoid exporting the P2P and NonP2P internal modules.
--
mkDiffusionArgumentsP2P
  :: Maybe (Either Socket AddrInfo)
  -> Maybe (Either Socket AddrInfo)
  -> Maybe (Either Socket FilePath)
  -> AcceptedConnectionsLimit
  -> DiffusionMode
  -> NTN.PeerSelectionTargets
  -> STM m [(Int, Map RelayAddress NTN.PeerAdvertise)]
  -> STM m [RelayAddress]
  -> STM m UseLedgerAfter
  -> DiffTime
  -> DiffTime
  -> DiffusionArguments m
mkDiffusionArgumentsP2P
  a1 a2 a3 a4 a5 a6
  a7 a8 a9 a10 a11
  =
    DiffusionArguments
    $ Common.DiffusionArguments
      a1 a2 a3 a4 a5
    $ Right
    $ P2P.DiffusionArgumentsExtra
      a6 a7 a8 a9 a10 a11

-- | Construct a value of NonP2P DiffusionApplications data type.
-- 'ouroboros-consensus' needs access to this constructor so we export this
-- function in order to avoid exporting the P2P and NonP2P internal modules.
--
mkDiffusionApplicationsNonP2P
  :: Versions
      NodeToNodeVersion
      ntnVersionData
      (Bundle
         (ConnectionId ntnAddr
          -> STM m ControlMessage
          -> [MiniProtocol 'InitiatorMode ByteString m () Void]))
  -> Versions
       NodeToNodeVersion
       ntnVersionData
       (Bundle
          (ConnectionId ntnAddr
           -> STM m ControlMessage
           -> [MiniProtocol 'InitiatorResponderMode ByteString m () ()]))
  -> Versions
       NodeToClientVersion
       ntcVersionData
       (OuroborosApplication 'ResponderMode ntcAddr ByteString m Void ())
  -> LedgerPeersConsensusInterface m
  -> NTC.ErrorPolicies
  -> DiffusionApplications
       ntnAddr
       ntcAddr
       ntnVersionData
       ntcVersionData
       m
mkDiffusionApplicationsNonP2P
  a1 a2 a3 a4 a5 =
    DiffusionApplications
    $ Common.DiffusionApplications
      a1 a2 a3 a4
    $ Left
    $ NonP2P.DiffusionApplicationsExtra
      a5

-- | Construct a value of P2P DiffusionApplications data type.
-- 'ouroboros-consensus' needs access to this constructor so we export this
-- function in order to avoid exporting the P2P and NonP2P internal modules.
--
mkDiffusionApplicationsP2P
  :: Versions
      NodeToNodeVersion
      ntnVersionData
      (Bundle
         (ConnectionId ntnAddr
          -> STM m ControlMessage
          -> [MiniProtocol 'InitiatorMode ByteString m () Void]))
  -> Versions
       NodeToNodeVersion
       ntnVersionData
       (Bundle
          (ConnectionId ntnAddr
           -> STM m ControlMessage
           -> [MiniProtocol 'InitiatorResponderMode ByteString m () ()]))
  -> Versions
       NodeToClientVersion
       ntcVersionData
       (OuroborosApplication 'ResponderMode ntcAddr ByteString m Void ())
  -> LedgerPeersConsensusInterface m
  -> MiniProtocolParameters
  -> RethrowPolicy
  -> RethrowPolicy
  -> PeerMetrics m ntnAddr
  -> STM m FetchMode
  -> DiffusionApplications
       ntnAddr
       ntcAddr
       ntnVersionData
       ntcVersionData
       m
mkDiffusionApplicationsP2P
  a1 a2 a3 a4 a5 a6
  a7 a8 a9 =
    DiffusionApplications
    $ Common.DiffusionApplications
      a1 a2 a3 a4
    $ Right
    $ P2P.DiffusionApplicationsExtra
      a5 a6 a7 a8 a9

-- | Construct a value of NonP2P DiffusionTracers data type.
-- 'ouroboros-consensus' needs access to this constructor so we export this
-- function in order to avoid exporting the P2P and NonP2P internal modules.
--
mkDiffusionTracersNonP2P
  :: Tracer IO (WithMuxBearer (ConnectionId SockAddr) MuxTrace)
  -> Tracer IO NTN.HandshakeTr
  -> Tracer IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)
  -> Tracer IO NTC.HandshakeTr
  -> Tracer IO DiffusionInitializationTracer
  -> Tracer IO TraceLedgerPeers
  -> Tracer IO (NTN.WithIPList (NTC.SubscriptionTrace SockAddr))
  -> Tracer IO (NTN.WithDomainName (NTC.SubscriptionTrace SockAddr))
  -> Tracer IO (NTN.WithDomainName NTN.DnsTrace)
  -> Tracer IO (NTC.WithAddr SockAddr NTC.ErrorPolicyTrace)
  -> Tracer IO (NTC.WithAddr LocalAddress NTC.ErrorPolicyTrace)
  -> Tracer IO NTN.AcceptConnectionsPolicyTrace
  -> DiffusionTracers
mkDiffusionTracersNonP2P
  a1 a2 a3 a4 a5 a6 a7 a8 a9
  a10 a11 a12 =
    DiffusionTracers
    $ Common.DiffusionTracers
      a1 a2 a3 a4 a5 a6
    $ Left $ NonP2P.DiffusionTracersExtra
      a7 a8 a9 a10 a11 a12

-- | Construct a value of P2P DiffusionTracers data type.
-- ouroboros-consensus needs access to this constructor so we export this
-- function in order to avoid exporting the P2P and NonP2P internal modules.
--
mkDiffusionTracersP2P
  :: Tracer IO (WithMuxBearer (ConnectionId SockAddr) MuxTrace)
  -> Tracer IO NTN.HandshakeTr
  -> Tracer IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)
  -> Tracer IO NTC.HandshakeTr
  -> Tracer IO DiffusionInitializationTracer
  -> Tracer IO TraceLedgerPeers
  -> Tracer IO (TraceLocalRootPeers SockAddr IOException)
  -> Tracer IO TracePublicRootPeers
  -> Tracer IO (TracePeerSelection SockAddr)
  -> Tracer
       IO
       (DebugPeerSelection
          SockAddr (P2P.NodeToNodePeerConnectionHandle 'InitiatorMode Void))
  -> Tracer
       IO
       (DebugPeerSelection
          SockAddr
          (P2P.NodeToNodePeerConnectionHandle 'InitiatorResponderMode ()))
  -> Tracer IO PeerSelectionCounters
  -> Tracer IO (PeerSelectionActionsTrace SockAddr)
  -> Tracer
       IO
       (ConnectionManagerTrace
          SockAddr
          (ConnectionHandlerTrace
             NodeToNodeVersion NodeToNodeVersionData))
  -> Tracer IO (ServerTrace SockAddr)
  -> Tracer IO (InboundGovernorTrace SockAddr)
  -> Tracer
       IO
       (ConnectionManagerTrace
          LocalAddress
          (ConnectionHandlerTrace
             NodeToClientVersion NodeToClientVersionData))
  -> Tracer IO (ServerTrace LocalAddress)
  -> Tracer IO (InboundGovernorTrace LocalAddress)
  -> DiffusionTracers
mkDiffusionTracersP2P
  a1 a2 a3 a4 a5 a6 a7 a8 a9
  a10 a11 a12 a13 a14 a15 a16
  a17 a18 a19 =
    DiffusionTracers
    $ Common.DiffusionTracers
      a1 a2 a3 a4 a5 a6
    $ Right
    $ P2P.DiffusionTracersExtra
      a7 a8 a9 a10 a11 a12
      a13 a14 a15 a16 a17
      a18 a19

-- | runDataDiffusion for either P2P or Non-P2P node
--
runDataDiffusion
  :: DiffusionTracers
  -> DiffusionArguments IO
  -> DiffusionApplications
       RemoteAddress
       LocalAddress
       NodeToNodeVersionData
       NodeToClientVersionData
       IO
  -> IO ()
runDataDiffusion
  (DiffusionTracers
    tr@Common.DiffusionTracers
       { dtExtra })
  (DiffusionArguments
    dargs@Common.DiffusionArguments
       { daExtra })
  (DiffusionApplications
    dapps@Common.DiffusionApplications
       { dapExtra }) =
  case (dtExtra, daExtra, dapExtra) of
    (Left t, Left da, Left dapp)    ->
      NonP2P.runDataDiffusion
        (tr { dtExtra = t})
        (dargs { daExtra = da })
        (dapps { dapExtra = dapp })
    (Right t, Right da, Right dapp) ->
      void
        $ P2P.runDataDiffusion
            (tr { dtExtra = t})
            (dargs { daExtra = da })
            (dapps { dapExtra = dapp })
    _                               ->
      error "Non-matching arguments, every argument should be on the same side!"
