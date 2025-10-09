{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Tracer
  ( mkCardanoTracer
  , dmqTracer
  , dmqDiffusionTracers
  , WithEventType (..)
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
import "contra-tracer" Control.Tracer

import Data.Aeson
import Data.Aeson.KeyMap (fromList)
import Data.Functor.Contravariant ((>$<))
import Data.Set qualified as Set
import Data.Text qualified as Text

import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection (DebugPeerSelection (..))
import Ouroboros.Network.PeerSelection.Governor.Types qualified as Governor
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.Snocket (RemoteAddress)

import qualified Cardano.Logging as Logging

import DMQ.Configuration
import DMQ.NodeToClient.Version
import DMQ.NodeToNode.Version
import DMQ.Tracer.Translation
import DMQ.Tracer.Types

mkCardanoTracer :: Logging.TraceConfig
                -> IO (Logging.Trace IO WithEventType)
mkCardanoTracer traceConfig = do
  emptyConfigReflection <- Logging.emptyConfigReflection
  stdoutTrace <- Logging.standardTracer
  {-- From: Cardano.Logging.Tracer.Composed
      -- | Construct a tracer according to the requirements for cardano node.
      -- The tracer gets a 'name', which is appended to its namespace.
      -- The tracer has to be an instance of LogFormatting for the display of
      -- messages and an instance of MetaTrace for meta information such as
      -- severity, privacy, details and backends'.
      -- The tracer gets the backends': 'trStdout', 'trForward' and 'mbTrEkg'
      -- as arguments.
      -- The returned tracer needs to be configured with a configuration
      -- before it is used.
      mkCardanoTracer :: forall evt. ( LogFormatting evt , MetaTrace evt)
                      => Trace IO FormattedMessage
                      -> Trace IO FormattedMessage
                      -> Maybe (Trace IO FormattedMessage)
                      -> [Text]
                      -> IO (Trace IO evt)
  --}
  tracer <- Logging.mkCardanoTracer
                  stdoutTrace
                  mempty
                  Nothing
                  [] -- ["DMQ"]
  {-- From: Cardano.Logging.Configuration
      -- | Call this function at initialisation, and later for reconfiguration.
      -- Config reflection is used to optimise the tracers and has to collect
      -- information about the tracers. Although it is possible to give more
      -- then one tracer of the same time, it is not a common case to do this.
      configureTracers :: forall a m. (MetaTrace a ,  MonadIO m)
                       => ConfigReflection
                       -> TraceConfig
                       -> [Trace m a]
                       -> m ()
  --}
  Logging.configureTracers
    emptyConfigReflection
    traceConfig
    [tracer]
  return tracer

-- | DMQ tracer
dmqTracer :: (Logging.Trace IO WithEventType)
          -> Tracer IO WithEventType
dmqTracer cardanoTracer = contramapM
           (\wet@(WithEventType _ _) -> do
              Logging.traceWith cardanoTracer wet
           )
       $ Tracer (\_ -> return ())

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
  -> (Tracer m WithEventType)
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
                                                          .- WithEventType "Mux" >$< tracer,
    Diffusion.dtChannelTracer                              = channelTracer
                                                          .- WithEventType "Channel" >$< tracer,
    Diffusion.dtBearerTracer                               = bearerTracer
                                                          .- WithEventType "Bearer" >$< tracer,
    Diffusion.dtHandshakeTracer                            = handshakeTracer
                                                          .- WithEventType "Handshake" >$< tracer,
    Diffusion.dtLocalMuxTracer                             = localMuxTracer
                                                          .- WithEventType "LocalMux" >$< tracer,
    Diffusion.dtLocalChannelTracer                         = localChannelTracer
                                                          .- WithEventType "LocalChannel" >$< tracer,
    Diffusion.dtLocalBearerTracer                          = localBearerTracer
                                                          .- WithEventType "LocalBearer" >$< tracer,
    Diffusion.dtLocalHandshakeTracer                       = localHandshakeTracer
                                                          .- WithEventType "LocalHandshake" >$< tracer,
    Diffusion.dtDiffusionTracer                            = diffusionTracer
                                                          .- WithEventType "Diffusion" >$< tracer,
    Diffusion.dtTraceLocalRootPeersTracer                  = traceLocalRootPeersTracer
                                                          .- WithEventType "LocalRootPeers" >$< tracer,
    Diffusion.dtTracePublicRootPeersTracer                 = tracePublicRootPeersTracer
                                                          .- WithEventType "PublicRootPeers" >$< tracer,
    Diffusion.dtTraceLedgerPeersTracer                     = traceLedgerPeersTracer
                                                          .- WithEventType "LedgerPeers" >$< tracer,
    Diffusion.dtTracePeerSelectionTracer                   = tracePeerSelectionTracer
                                                          .- WithEventType "PeerSelection" >$< tracer,
    Diffusion.dtDebugPeerSelectionInitiatorTracer          = debugPeerSelectionInitiatorTracer
                                                          .- WithEventType "DebugPeerSelectionInitiator" >$< tracer,
    Diffusion.dtDebugPeerSelectionInitiatorResponderTracer = debugPeerSelectionInitiatorResponderTracer
                                                          .- WithEventType "DebugPeerSelectionInitiatorResponder" >$< tracer,
    Diffusion.dtTracePeerSelectionCounters                 = tracePeerSelectionCounters
                                                          .- WithEventType "PeerSelectionCounters" >$< tracer,
    Diffusion.dtTraceChurnCounters                         = traceChurnCounters
                                                          .- WithEventType "ChurnCounters" >$< tracer,
    Diffusion.dtPeerSelectionActionsTracer                 = peerSelectionActionsTracer
                                                          .- WithEventType "PeerSelectionActions" >$< tracer,
    Diffusion.dtConnectionManagerTracer                    = connectionManagerTracer
                                                          .- WithEventType "ConnectionManager" >$< tracer,
    Diffusion.dtConnectionManagerTransitionTracer          = connectionManagerTransitionTracer
                                                          .- WithEventType "ConnectionManagerTransition" >$< tracer,
    Diffusion.dtServerTracer                               = serverTracer
                                                          .- WithEventType "Server" >$< tracer,
    Diffusion.dtInboundGovernorTracer                      = inboundGovernorTracer
                                                          .- WithEventType "InboundGovernor" >$< tracer,
    Diffusion.dtInboundGovernorTransitionTracer            = inboundGovernorTransitionTracer
                                                          .- WithEventType "InboundGovernorTransition" >$< tracer,
    Diffusion.dtDnsTracer                                  = dnsTracer
                                                          .- WithEventType "dtDnsTracer" >$< tracer,
    Diffusion.dtLocalConnectionManagerTracer               = localConnectionManagerTracer
                                                          .- WithEventType "dtLocalConnectionManagerTracer" >$< tracer,
    Diffusion.dtLocalServerTracer                          = localServerTracer
                                                          .- WithEventType "dtLocalServerTracer" >$< tracer,
    Diffusion.dtLocalInboundGovernorTracer                 = localInboundGovernorTracer
                                                          .- WithEventType "dtLocalInboundGovernorTracer" >$< tracer
  }
  where
    (.-) :: Bool -> Tracer m a -> Tracer m a
    True  .- a = a
    False .- _ = nullTracer
    infixl 3 .-
