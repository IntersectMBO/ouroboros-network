{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Tracer
  ( dmqTracer
  , dmqDiffusionTracers
  , NoExtraPeers (..)
  , NoExtraState (..)
  , NoExtraDebugState (..)
  , NoExtraCounters (..)
  , NoExtraFlags (..)
  , NoExtraConfig (..)
  , NoExtraAPI (..)
  , NoExtraChurnArgs (..)
  ) where

import Codec.CBOR.Term (Term)
import Control.Monad.Class.MonadTime
import Control.Tracer

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Functor.Contravariant ((>$<))
-- import Data.Map.Strict qualified as Map
-- import Data.Monoid (Monoid (..))
-- import Data.Semigroup (Semigroup (..))
import Data.Set qualified as Set
import Data.Text qualified as Text

import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection (DebugPeerSelection (..))
-- import Ouroboros.Network.PeerSelection qualified as PeerSelection
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection.Governor.Types qualified as Governor
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers

import DMQ.Configuration
import DMQ.NodeToClient
import DMQ.NodeToNode

data TraceEvent ev = TraceEvent
  { time      :: UTCTime
  , eventType :: String
  , event     :: ev
  }

instance ToJSON ev => ToJSON (TraceEvent ev) where
  toJSON TraceEvent {time, eventType, event} =
    object [ "time"  .= time
           , "type"  .= eventType
           , "event" .= event
           ]

-- | DMQ tracer
dmqTracer :: ToJSON ev
          => Bool
          -> Tracer IO (String, ev)
dmqTracer pretty = contramapM
           (\(eventType, event) -> do
              time <- getCurrentTime
              return $ bool encode encodePretty pretty TraceEvent { time, eventType, event }
           )
       $ Tracer LBS.Char8.putStrLn

-- An orphan instance needed for `Handshake versionNumber Term`
instance ToJSON Term where
  toJSON term = String (Text.pack . show $ term)

data NoExtraPeers = NoExtraPeers
instance Semigroup NoExtraPeers where
  _ <> _ = NoExtraPeers
instance Monoid NoExtraPeers where
  mempty = NoExtraPeers

instance ToJSON NoExtraPeers where
  toJSON _ = Null
  omitField _ = True

instance ToJSON (PublicRootPeers NoExtraPeers RemoteAddress) where
  toJSON prp =
    object [ "kind"              .= String "PublicRootPeers"
           , "ledgerPeers"       .= PublicRootPeers.getLedgerPeers prp
           , "bigLedgerPeers"    .= PublicRootPeers.getBigLedgerPeers prp
           ]

data NoExtraState      = NoExtraState
data NoExtraCounters   = NoExtraCounters   deriving Eq
data NoExtraDebugState = NoExtraDebugState
instance ToJSON NoExtraDebugState where
  toJSON _ = Null
  omitField _ = True
data NoExtraChurnArgs  = NoExtraChurnArgs
data NoExtraAPI        = NoExtraAPI

instance ToJSON (Governor.PeerSelectionCounters NoExtraCounters) where
  toJSON Governor.PeerSelectionCounters {..} =
    object [ "kind" .= String "PeerSelectionCounters"

           , "knownPeers" .= numberOfKnownPeers
           , "rootPeers" .= numberOfRootPeers
           , "coldPeersPromotions" .= numberOfColdPeersPromotions
           , "establishedPeers" .= numberOfEstablishedPeers
           , "warmPeersDemotions" .= numberOfWarmPeersDemotions
           , "warmPeersPromotions" .= numberOfWarmPeersPromotions
           , "activePeers" .= numberOfActivePeers
           , "activePeersDemotions" .= numberOfActivePeersDemotions

           , "knownBigLedgerPeers" .= numberOfKnownBigLedgerPeers
           , "coldBigLedgerPeersPromotions" .= numberOfColdBigLedgerPeersPromotions
           , "establishedBigLedgerPeers" .= numberOfEstablishedBigLedgerPeers
           , "warmBigLedgerPeersDemotions" .= numberOfWarmBigLedgerPeersDemotions
           , "warmBigLedgerPeersPromotions" .= numberOfWarmBigLedgerPeersPromotions
           , "activeBigLedgerPeers" .= numberOfActiveBigLedgerPeers
           , "activeBigLedgerPeersDemotions" .= numberOfActiveBigLedgerPeersDemotions

           , "knownLocalRootPeers" .= numberOfKnownLocalRootPeers
           , "establishedLocalRootPeers" .= numberOfEstablishedLocalRootPeers
           , "warmLocalRootPeersPromotions" .= numberOfWarmLocalRootPeersPromotions
           , "activeLocalRootPeers" .= numberOfActiveLocalRootPeers
           , "activeLocalRootPeersDemotions" .= numberOfActiveLocalRootPeersDemotions

           , "knownNonRootPeers" .= numberOfKnownNonRootPeers
           , "coldNonRootPeersPromotions" .= numberOfColdNonRootPeersPromotions
           , "establishedNonRootPeers" .= numberOfEstablishedNonRootPeers
           , "warmNonRootPeersDemotions" .= numberOfWarmNonRootPeersDemotions
           , "warmNonRootPeersPromotions" .= numberOfWarmNonRootPeersPromotions
           , "activeNonRootPeers" .= numberOfActiveNonRootPeers
           , "activeNonRootPeersDemotions" .= numberOfActiveNonRootPeersDemotions
           ]

instance ToJSON (DebugPeerSelection NoExtraState NoExtraFlags NoExtraPeers RemoteAddress) where
  toJSON (TraceGovernorState blockedAt wakeupAfter st@Governor.PeerSelectionState { Governor.targets }) =
    object [ "kind"        .= String "DebugPeerSelection"
           , "blockedAt"   .= String (Text.pack . show $ blockedAt)
           , "wakeupAfter" .= String (Text.pack . show $ wakeupAfter)
           , "targets"     .= targets
           , "counters"    .= Governor.peerSelectionStateToCounters
                                (const Set.empty)
                                (const NoExtraCounters)
                                st
           ]

dmqDiffusionTracers
  :: forall m.
     Applicative m
  => Configuration
  -> (forall ev. ToJSON ev => Tracer m (String, ev))
  -> Diffusion.Tracers RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
                       LocalAddress  NodeToClientVersion NodeToClientVersionData
                       NoExtraState
                       NoExtraDebugState
                       NoExtraFlags
                       NoExtraPeers
                       NoExtraCounters
                       m
dmqDiffusionTracers
    Configuration {
      dmqcMuxTracer                                  = I muxTracer,
      dmqcChannelTracer                              = I channelTracer,
      dmqcBearerTracer                               = I bearerTracer,
      dmqcHandshakeTracer                            = I handshakeTracer,
      dmqcLocalMuxTracer                             = I localMuxTracer,
      dmqcLocalChannelTracer                         = I localChannelTracer,
      dmqcLocalBearerTracer                          = I localBearerTracer,
      dmqcLocalHandshakeTracer                       = I localHandshakeTracer,
      dmqcDiffusionTracer                            = I diffusionTracer,
      dmqcTraceLocalRootPeersTracer                  = I traceLocalRootPeersTracer,
      dmqcTracePublicRootPeersTracer                 = I tracePublicRootPeersTracer,
      dmqcTraceLedgerPeersTracer                     = I traceLedgerPeersTracer,
      dmqcTracePeerSelectionTracer                   = I tracePeerSelectionTracer,
      dmqcTraceChurnCounters                         = I traceChurnCounters,
      dmqcDebugPeerSelectionInitiatorTracer          = I debugPeerSelectionInitiatorTracer,
      dmqcDebugPeerSelectionInitiatorResponderTracer = I debugPeerSelectionInitiatorResponderTracer,
      dmqcTracePeerSelectionCounters                 = I tracePeerSelectionCounters,
      dmqcPeerSelectionActionsTracer                 = I peerSelectionActionsTracer,
      dmqcConnectionManagerTracer                    = I connectionManagerTracer,
      dmqcConnectionManagerTransitionTracer          = I connectionManagerTransitionTracer,
      dmqcServerTracer                               = I serverTracer,
      dmqcInboundGovernorTracer                      = I inboundGovernorTracer,
      dmqcInboundGovernorTransitionTracer            = I inboundGovernorTransitionTracer,
      dmqcLocalConnectionManagerTracer               = I localConnectionManagerTracer,
      dmqcLocalServerTracer                          = I localServerTracer,
      dmqcLocalInboundGovernorTracer                 = I localInboundGovernorTracer,
      dmqcDnsTracer                                  = I dnsTracer
    }
    tracer
  = Diffusion.Tracers {
    Diffusion.dtMuxTracer                                  = muxTracer
                                                          .- ("Mux",) >$< tracer,
    Diffusion.dtChannelTracer                              = channelTracer
                                                          .- ("Channel",) >$< tracer,
    Diffusion.dtBearerTracer                               = bearerTracer
                                                          .- ("Bearer",) >$< tracer,
    Diffusion.dtHandshakeTracer                            = handshakeTracer
                                                          .- ("Handshake",) >$< tracer,
    Diffusion.dtLocalMuxTracer                             = localMuxTracer
                                                          .- ("LocalMux",) >$< tracer,
    Diffusion.dtLocalChannelTracer                         = localChannelTracer
                                                          .- ("LocalChannel",) >$< tracer,
    Diffusion.dtLocalBearerTracer                          = localBearerTracer
                                                          .- ("LocalBearer",) >$< tracer,
    Diffusion.dtLocalHandshakeTracer                       = localHandshakeTracer
                                                          .- ("LocalHandshake",) >$< tracer,
    Diffusion.dtDiffusionTracer                            = diffusionTracer
                                                          .- ("Diffusion",) >$< tracer,
    Diffusion.dtTraceLocalRootPeersTracer                  = traceLocalRootPeersTracer
                                                          .- ("LocalRootPeers",) >$< tracer,
    Diffusion.dtTracePublicRootPeersTracer                 = tracePublicRootPeersTracer
                                                          .- ("PublicRootPeers",) >$< tracer,
    Diffusion.dtTraceLedgerPeersTracer                     = traceLedgerPeersTracer
                                                          .- ("LedgerPeers",) >$< tracer,
    Diffusion.dtTracePeerSelectionTracer                   = tracePeerSelectionTracer
                                                          .- ("PeerSelection",) >$< tracer,
    Diffusion.dtDebugPeerSelectionInitiatorTracer          = debugPeerSelectionInitiatorTracer
                                                          .- ("DebugPeerSelectionInitiator",) >$< tracer,
    Diffusion.dtDebugPeerSelectionInitiatorResponderTracer = debugPeerSelectionInitiatorResponderTracer
                                                          .- ("DebugPeerSelectionInitiatorResponder",) >$< tracer,
    Diffusion.dtTracePeerSelectionCounters                 = tracePeerSelectionCounters
                                                          .- ("PeerSelectionCounters",) >$< tracer,
    Diffusion.dtTraceChurnCounters                         = traceChurnCounters
                                                          .- ("ChurnCounters",) >$< tracer,
    Diffusion.dtPeerSelectionActionsTracer                 = peerSelectionActionsTracer
                                                          .- ("PeerSelectionActions",) >$< tracer,
    Diffusion.dtConnectionManagerTracer                    = connectionManagerTracer
                                                          .- ("ConnectionManager",) >$< tracer,
    Diffusion.dtConnectionManagerTransitionTracer          = connectionManagerTransitionTracer
                                                          .- ("ConnectionManagerTransition",) >$< tracer,
    Diffusion.dtServerTracer                               = serverTracer
                                                          .- ("Server",) >$< tracer,
    Diffusion.dtInboundGovernorTracer                      = inboundGovernorTracer
                                                          .- ("InboundGovernor",) >$< tracer,
    Diffusion.dtInboundGovernorTransitionTracer            = inboundGovernorTransitionTracer
                                                          .- ("InboundGovernorTransition",) >$< tracer,
    Diffusion.dtDnsTracer                                  = dnsTracer
                                                          .- ("dtDnsTracer",) >$< tracer,
    Diffusion.dtLocalConnectionManagerTracer               = localConnectionManagerTracer
                                                          .- ("dtLocalConnectionManagerTracer",) >$< tracer,
    Diffusion.dtLocalServerTracer                          = localServerTracer
                                                          .- ("dtLocalServerTracer",) >$< tracer,
    Diffusion.dtLocalInboundGovernorTracer                 = localInboundGovernorTracer
                                                          .- ("dtLocalInboundGovernorTracer",) >$< tracer
  }
  where
    (.-) :: Bool
                 -> Tracer m a
                 -> Tracer m a
    True  .- a = a
    False .- _ = nullTracer

    infixl 3 .-
