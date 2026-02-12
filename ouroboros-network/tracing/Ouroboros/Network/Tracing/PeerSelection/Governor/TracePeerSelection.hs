{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2026-02-11, 85869e9dd21d9dac7c4381418346e97259c3303b).

{- TODO: All references to package "cardano-diffusion" were removed.
--       See all the TODO annotations.
import           "cardano-diffusion" -- "cardano-diffusion:???"
  Cardano.Network.PeerSelection.Governor.Monitor
    ( ExtraTrace (TraceLedgerStateJudgementChanged, TraceUseBootstrapPeersChanged)
    )
--}

--------------------------------------------------------------------------------

module Ouroboros.Network.Tracing.PeerSelection.Governor.TracePeerSelection () where

--------------------------------------------------------------------------------

---------
-- base -
---------
import Control.Exception (fromException)
import Data.Bifunctor (first)
import Data.Foldable (toList)
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (ToJSON, ToJSONKey, Value (String), toJSON,
           toJSONList, (.=))
-----------------------
-- Package: "network" -
-----------------------
import "network" Network.Socket (SockAddr)
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
import "ouroboros-network" Ouroboros.Network.PeerSelection.Governor.Types
           (DebugPeerSelectionState (..), DemotionTimeoutException,
           TracePeerSelection (..))
import "ouroboros-network" Ouroboros.Network.PeerSelection.PublicRootPeers
           (PublicRootPeers)
import "ouroboros-network" Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import "ouroboros-network" Ouroboros.Network.Protocol.PeerSharing.Type
           (PeerSharingAmount (PeerSharingAmount))
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging
---------
-- Self -
---------
import Ouroboros.Network.Tracing.PeerSelection.Governor.Utils
           (peerSelectionTargetsToObject)

--------------------------------------------------------------------------------
-- PeerSelection Tracer
--------------------------------------------------------------------------------

{-- TODO: Before "cardano-diffusion" removal:
instance LogFormatting (TracePeerSelection Cardano.DebugPeerSelectionState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers SockAddr) SockAddr) where
 -- TODO: That later changed in f550a6eb503cc81807419795ab2360e6042ce9d5:
instance LogFormatting CardanoTracePeerSelection where
--}
instance ( Ord ntnAddr
         , Show extraDebugState
         , Show extraFlags
         , Show extraPeers
         , Show extraTrace
         , Show ntnAddr
         , ToJSON extraFlags
         , ToJSON ntnAddr
         , ToJSON (PublicRootPeers extraPeers ntnAddr)
         , ToJSONKey ntnAddr
         )
      => LogFormatting (TracePeerSelection extraDebugState extraFlags extraPeers extraTrace ntnAddr) where
  forMachine _dtal (TraceLocalRootPeersChanged lrp lrp') =
    mconcat [ "kind" .= String "LocalRootPeersChanged"
             , "previous" .= toJSON lrp
             , "current" .= toJSON lrp'
             ]
  forMachine _dtal (TraceTargetsChanged pst) =
    mconcat [ "kind" .= String "TargetsChanged"
             , "previous" .= toJSON pst
{-- TODO: Field was removed here but not in cardano-node master
 -- See: ouroboros-network/changelog.d/20260122_220351_coot_ouroboros_churn_fix.md
             , "current" .= toJSON pst'
--}
             ]
  forMachine _dtal (TracePublicRootsRequest tRootPeers nRootPeers) =
    mconcat [ "kind" .= String "PublicRootsRequest"
             , "targetNumberOfRootPeers" .= tRootPeers
             , "numberOfRootPeers" .= nRootPeers
             ]
  forMachine _dtal (TracePublicRootsResults res group dt) =
    mconcat [ "kind" .= String "PublicRootsResults"
             , "result" .= toJSON res
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TracePublicRootsFailure err group dt) =
    mconcat [ "kind" .= String "PublicRootsFailure"
             , "reason" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TraceForgetColdPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "ForgetColdPeers"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceBigLedgerPeersRequest tRootPeers nRootPeers) =
    mconcat [ "kind" .= String "BigLedgerPeersRequest"
             , "targetNumberOfBigLedgerPeers" .= tRootPeers
             , "numberOfBigLedgerPeers" .= nRootPeers
             ]
  forMachine _dtal (TraceBigLedgerPeersResults res group dt) =
    mconcat [ "kind" .= String "BigLedgerPeersResults"
             , "result" .= toJSONList (toList res)
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TraceBigLedgerPeersFailure err group dt) =
    mconcat [ "kind" .= String "BigLedgerPeersFailure"
             , "reason" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TraceForgetBigLedgerPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "ForgetColdBigLedgerPeers"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePeerShareRequests targetKnown actualKnown (PeerSharingAmount numRequested) aps sps) =
    mconcat [ "kind" .= String "PeerShareRequests"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "numRequested" .= numRequested
             , "availablePeers" .= toJSONList (toList aps)
             , "selectedPeers" .= toJSONList (toList sps)
             ]
  forMachine _dtal (TracePeerShareResults res) =
    mconcat [ "kind" .= String "PeerShareResults"
             , "result" .= toJSONList (map (first show <$>) res)
             ]
  forMachine _dtal (TracePeerShareResultsFiltered res) =
    mconcat [ "kind" .= String "PeerShareResultsFiltered"
             , "result" .= toJSONList res
             ]
  forMachine _dtal (TracePromoteColdPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "PromoteColdPeers"
             , "targetEstablished" .= targetKnown
             , "actualEstablished" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdLocalPeers tLocalEst sp) =
    mconcat [ "kind" .= String "PromoteColdLocalPeers"
             , "targetLocalEstablished" .= tLocalEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  {-- TODO: `forgotten` added here but not in cardano-node master
   -- See: f9ae41a8962174e4f7d7e93d81f70f1c88cef263
  --}
  forMachine _dtal (TracePromoteColdFailed tEst aEst p d err forgotten) =
    mconcat [ "kind" .= String "PromoteColdFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "delay" .= toJSON d
             , "reason" .= show err
             , "forgotten" .= forgotten
             ]
  forMachine _dtal (TracePromoteColdDone tEst aEst p) =
    mconcat [ "kind" .= String "PromoteColdDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteColdBigLedgerPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "PromoteColdBigLedgerPeers"
             , "targetEstablished" .= targetKnown
             , "actualEstablished" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  {-- TODO: `forgotten` added here but not in cardano-node master
   -- See: f9ae41a8962174e4f7d7e93d81f70f1c88cef263
  --}
  forMachine _dtal (TracePromoteColdBigLedgerPeerFailed tEst aEst p d err forgotten) =
    mconcat [ "kind" .= String "PromoteColdBigLedgerPeerFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "delay" .= toJSON d
             , "reason" .= show err
             , "forgotten" .= forgotten
             ]
  forMachine _dtal (TracePromoteColdBigLedgerPeerDone tEst aEst p) =
    mconcat [ "kind" .= String "PromoteColdBigLedgerPeerDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteWarmPeers tActive aActive sp) =
    mconcat [ "kind" .= String "PromoteWarmPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmLocalPeers taa sp) =
    mconcat [ "kind" .= String "PromoteWarmLocalPeers"
             , "targetActualActive" .= toJSONList taa
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmFailed tActive aActive p err) =
    mconcat [ "kind" .= String "PromoteWarmFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TracePromoteWarmDone tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteWarmAborted tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmAborted"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteWarmBigLedgerPeers tActive aActive sp) =
    mconcat [ "kind" .= String "PromoteWarmBigLedgerPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmBigLedgerPeerFailed tActive aActive p err) =
    mconcat [ "kind" .= String "PromoteWarmBigLedgerPeerFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TracePromoteWarmBigLedgerPeerDone tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmBigLedgerPeerDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteWarmBigLedgerPeerAborted tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmBigLedgerPeerAborted"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteWarmPeers tEst aEst sp) =
    mconcat [ "kind" .= String "DemoteWarmPeers"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteWarmFailed tEst aEst p err) =
    mconcat [ "kind" .= String "DemoteWarmFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteWarmDone tEst aEst p) =
    mconcat [ "kind" .= String "DemoteWarmDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteWarmBigLedgerPeers tEst aEst sp) =
    mconcat [ "kind" .= String "DemoteWarmBigLedgerPeers"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteWarmBigLedgerPeerFailed tEst aEst p err) =
    mconcat [ "kind" .= String "DemoteWarmBigLedgerPeerFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteWarmBigLedgerPeerDone tEst aEst p) =
    mconcat [ "kind" .= String "DemoteWarmBigLedgerPeerDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteHotPeers tActive aActive sp) =
    mconcat [ "kind" .= String "DemoteHotPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteLocalHotPeers taa sp) =
    mconcat [ "kind" .= String "DemoteLocalHotPeers"
             , "targetActualActive" .= toJSONList taa
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteHotFailed tActive aActive p err) =
    mconcat [ "kind" .= String "DemoteHotFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteHotDone tActive aActive p) =
    mconcat [ "kind" .= String "DemoteHotDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteHotBigLedgerPeers tActive aActive sp) =
    mconcat [ "kind" .= String "DemoteHotBigLedgerPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteHotBigLedgerPeerFailed tActive aActive p err) =
    mconcat [ "kind" .= String "DemoteHotBigLedgerPeerFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteHotBigLedgerPeerDone tActive aActive p) =
    mconcat [ "kind" .= String "DemoteHotBigLedgerPeerDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteAsynchronous"
             , "state" .= toJSON msp
             ]
  forMachine _dtal (TraceDemoteLocalAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteLocalAsynchronous"
             , "state" .= toJSON msp
             ]
  forMachine _dtal (TraceDemoteBigLedgerPeersAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteBigLedgerPeerAsynchronous"
             , "state" .= toJSON msp
             ]
  forMachine _dtal TraceGovernorWakeup =
    mconcat [ "kind" .= String "GovernorWakeup"
             ]
  forMachine _dtal (TraceChurnWait dt) =
    mconcat [ "kind" .= String "ChurnWait"
             , "diffTime" .= toJSON dt
             ]
  forMachine _dtal (TracePickInboundPeers targetNumberOfKnownPeers numberOfKnownPeers selected available) =
    mconcat [ "kind" .= String "PickInboundPeers"
            , "targetKnown" .= targetNumberOfKnownPeers
            , "actualKnown" .= numberOfKnownPeers
            , "selected" .= selected
            , "available" .= available
            ]
{-- TODO: Before "cardano-diffusion" removal:
  forMachine _dtal (TraceLedgerStateJudgementChanged new) =
    mconcat [ "kind" .= String "LedgerStateJudgementChanged"
            , "new" .= show new ]
--}
  forMachine _dtal TraceOnlyBootstrapPeers =
    mconcat [ "kind" .= String "LedgerStateJudgementChanged" ]
{-- TODO: Before "cardano-diffusion" removal:
  forMachine _dtal (TraceUseBootstrapPeersChanged ubp) =
    mconcat [ "kind" .= String "UseBootstrapPeersChanged"
            , "useBootstrapPeers" .= toJSON ubp ]
--}
  forMachine _dtal TraceBootstrapPeersFlagChangedWhilstInSensitiveState =
    mconcat [ "kind" .= String "BootstrapPeersFlagChangedWhilstInSensitiveState"
            ]
  forMachine _dtal (TraceVerifyPeerSnapshot result) =
    mconcat [ "kind" .= String "VerifyPeerSnapshot"
            , "result" .= toJSON result ]
  forMachine _dtal (TraceOutboundGovernorCriticalFailure err) =
    mconcat [ "kind" .= String "OutboundGovernorCriticalFailure"
            , "reason" .= show err
            ]
  forMachine _dtal (TraceChurnAction duration action counter) =
    mconcat [ "kind" .= String "ChurnAction"
            , "action" .= show action
            , "counter" .= counter
            , "duration" .= duration
            ]
  forMachine _dtal (TraceChurnTimeout duration action counter) =
    mconcat [ "kind" .= String "ChurnTimeout"
            , "action" .= show action
            , "counter" .= counter
            , "duration" .= duration
            ]
  forMachine _dtal (TraceDebugState mtime ds) =
    mconcat [ "kind" .= String "DebugState"
            , "monotonicTime" .= show mtime
            , "targets" .= peerSelectionTargetsToObject (dpssTargets ds)
            , "localRootPeers" .= dpssLocalRootPeers ds
            , "publicRootPeers" .= dpssPublicRootPeers ds
            , "knownPeers" .= KnownPeers.allPeers (dpssKnownPeers ds)
            , "establishedPeers" .= dpssEstablishedPeers ds
            , "activePeers" .= dpssActivePeers ds
            , "publicRootBackoffs" .= dpssPublicRootBackoffs ds
            , "publicRootRetryTime" .= dpssPublicRootRetryTime ds
            , "bigLedgerPeerBackoffs" .= dpssBigLedgerPeerBackoffs ds
            , "bigLedgerPeerRetryTime" .= dpssBigLedgerPeerRetryTime ds
            , "inProgressBigLedgerPeersReq" .= dpssInProgressBigLedgerPeersReq ds
            , "inProgressPeerShareReqs" .= dpssInProgressPeerShareReqs ds
            , "inProgressPromoteCold" .= dpssInProgressPromoteCold ds
            , "inProgressPromoteWarm" .= dpssInProgressPromoteWarm ds
            , "inProgressDemoteWarm" .= dpssInProgressDemoteWarm ds
            , "inProgressDemoteHot" .= dpssInProgressDemoteHot ds
            , "inProgressDemoteToCold" .= dpssInProgressDemoteToCold ds
            , "upstreamyness" .= dpssUpstreamyness ds
            , "fetchynessBlocks" .= dpssFetchynessBlocks ds
            ]
{-- TODO:
    Pattern match(es) are non-exhaustive
    In an equation for ‘forMachine’:
        Patterns of type ‘DetailLevel’,
                         ‘TracePeerSelection
                            extraDebugState
                            extraFlags
                            extraPeers
                            extraTrace
                            ntnAddr’ not matched:
            _ (ExtraTrace _)
    |
107 |   forMachine _dtal (TraceLocalRootPeersChanged lrp lrp') =
--}
  forMachine _ _ = mempty

  forHuman = pack . show

  asMetrics (TraceChurnAction duration action _) =
    [ DoubleM ("peerSelection.churn" <> pack (show action) <> ".duration")
              (realToFrac duration)
    ]
  asMetrics _ = []

{-- TODO: `extraTrace` was added here but no in cardano-node master:
-- instance MetaTrace (TracePeerSelection extraDebugState extraFlags extraPeers SockAddr) where
--}
instance MetaTrace (TracePeerSelection extraDebugState extraFlags extraPeers extraTrace SockAddr) where
    namespaceFor TraceLocalRootPeersChanged {} =
      Namespace [] ["LocalRootPeersChanged"]
    namespaceFor TraceTargetsChanged {}        =
      Namespace [] ["TargetsChanged"]
    namespaceFor TracePublicRootsRequest {}    =
      Namespace [] ["PublicRootsRequest"]
    namespaceFor TracePublicRootsResults {}    =
      Namespace [] ["PublicRootsResults"]
    namespaceFor TracePublicRootsFailure {}    =
      Namespace [] ["PublicRootsFailure"]
    namespaceFor TraceForgetColdPeers {}       =
      Namespace [] ["ForgetColdPeers"]
    namespaceFor TraceBigLedgerPeersRequest {}    =
      Namespace [] ["BigLedgerPeersRequest"]
    namespaceFor TraceBigLedgerPeersResults {}    =
      Namespace [] ["BigLedgerPeersResults"]
    namespaceFor TraceBigLedgerPeersFailure {}    =
      Namespace [] ["BigLedgerPeersFailure"]
    namespaceFor TraceForgetBigLedgerPeers {}       =
      Namespace [] ["ForgetBigLedgerPeers"]
    namespaceFor TracePeerShareRequests {}     =
      Namespace [] ["PeerShareRequests"]
    namespaceFor TracePeerShareResults {}      =
      Namespace [] ["PeerShareResults"]
    namespaceFor TracePeerShareResultsFiltered {} =
      Namespace [] ["PeerShareResultsFiltered"]
    namespaceFor TracePickInboundPeers {} =
      Namespace [] ["PickInboundPeers"]
    namespaceFor TracePromoteColdPeers {}      =
      Namespace [] ["PromoteColdPeers"]
    namespaceFor TracePromoteColdLocalPeers {} =
      Namespace [] ["PromoteColdLocalPeers"]
    namespaceFor TracePromoteColdFailed {}     =
      Namespace [] ["PromoteColdFailed"]
    namespaceFor TracePromoteColdDone {}       =
      Namespace [] ["PromoteColdDone"]
    namespaceFor TracePromoteColdBigLedgerPeers {}      =
      Namespace [] ["PromoteColdBigLedgerPeers"]
    namespaceFor TracePromoteColdBigLedgerPeerFailed {}     =
      Namespace [] ["PromoteColdBigLedgerPeerFailed"]
    namespaceFor TracePromoteColdBigLedgerPeerDone {}       =
      Namespace [] ["PromoteColdBigLedgerPeerDone"]
    namespaceFor TracePromoteWarmPeers {}      =
      Namespace [] ["PromoteWarmPeers"]
    namespaceFor TracePromoteWarmLocalPeers {} =
      Namespace [] ["PromoteWarmLocalPeers"]
    namespaceFor TracePromoteWarmFailed {}     =
      Namespace [] ["PromoteWarmFailed"]
    namespaceFor TracePromoteWarmDone {}       =
      Namespace [] ["PromoteWarmDone"]
    namespaceFor TracePromoteWarmAborted {}    =
      Namespace [] ["PromoteWarmAborted"]
    namespaceFor TracePromoteWarmBigLedgerPeers {}      =
      Namespace [] ["PromoteWarmBigLedgerPeers"]
    namespaceFor TracePromoteWarmBigLedgerPeerFailed {}     =
      Namespace [] ["PromoteWarmBigLedgerPeerFailed"]
    namespaceFor TracePromoteWarmBigLedgerPeerDone {}       =
      Namespace [] ["PromoteWarmBigLedgerPeerDone"]
    namespaceFor TracePromoteWarmBigLedgerPeerAborted {}    =
      Namespace [] ["PromoteWarmBigLedgerPeerAborted"]
    namespaceFor TraceDemoteWarmPeers {}       =
      Namespace [] ["DemoteWarmPeers"]
    namespaceFor (TraceDemoteWarmFailed _ _ _ e) =
      case fromException e :: Maybe DemotionTimeoutException of
        Just _  -> Namespace [] ["DemoteWarmFailed", "CoolingToColdTimeout"]
        Nothing -> Namespace [] ["DemoteWarmFailed"]
    namespaceFor TraceDemoteWarmDone {}        =
      Namespace [] ["DemoteWarmDone"]
    namespaceFor TraceDemoteWarmBigLedgerPeers {}       =
      Namespace [] ["DemoteWarmBigLedgerPeers"]
    namespaceFor (TraceDemoteWarmBigLedgerPeerFailed _ _ _ e) =
      case fromException e :: Maybe DemotionTimeoutException of
        Just _  -> Namespace [] ["DemoteWarmBigLedgerPeerFailed", "CoolingToColdTimeout"]
        Nothing -> Namespace [] ["DemoteWarmBigLedgerPeerFailed"]
    namespaceFor TraceDemoteWarmBigLedgerPeerDone {}        =
      Namespace [] ["DemoteWarmBigLedgerPeerDone"]
    namespaceFor TraceDemoteHotPeers {}        =
      Namespace [] ["DemoteHotPeers"]
    namespaceFor TraceDemoteLocalHotPeers {}   =
      Namespace [] ["DemoteLocalHotPeers"]
    namespaceFor (TraceDemoteHotFailed _ _ _ e)  =
      case fromException e :: Maybe DemotionTimeoutException of
        Just _  -> Namespace [] ["DemoteHotFailed", "CoolingToColdTimeout"]
        Nothing -> Namespace [] ["DemoteHotFailed"]
    namespaceFor TraceDemoteHotDone {}         =
      Namespace [] ["DemoteHotDone"]
    namespaceFor TraceDemoteHotBigLedgerPeers {}        =
      Namespace [] ["DemoteHotBigLedgerPeers"]
    namespaceFor (TraceDemoteHotBigLedgerPeerFailed _ _ _ e)  =
      case fromException e :: Maybe DemotionTimeoutException of
        Just _  -> Namespace [] ["DemoteHotBigLedgerPeerFailed", "CoolingToColdTimeout"]
        Nothing -> Namespace [] ["DemoteHotBigLedgerPeerFailed"]
    namespaceFor TraceDemoteHotBigLedgerPeerDone {}         =
      Namespace [] ["DemoteHotBigLedgerPeerDone"]
    namespaceFor TraceDemoteAsynchronous {}    =
      Namespace [] ["DemoteAsynchronous"]
    namespaceFor TraceDemoteLocalAsynchronous {} =
      Namespace [] ["DemoteLocalAsynchronous"]
    namespaceFor TraceDemoteBigLedgerPeersAsynchronous {} =
      Namespace [] ["DemoteBigLedgerPeersAsynchronous"]
    namespaceFor TraceGovernorWakeup {}        =
      Namespace [] ["GovernorWakeup"]
    namespaceFor TraceChurnWait {}             =
      Namespace [] ["ChurnWait"]
{-- TODO: Before "cardano-diffusion" removal:
    namespaceFor TraceLedgerStateJudgementChanged {} =
      Namespace [] ["LedgerStateJudgementChanged"]
--}
    namespaceFor TraceOnlyBootstrapPeers {} =
      Namespace [] ["OnlyBootstrapPeers"]
{-- TODO: Before "cardano-diffusion" removal:
    namespaceFor TraceUseBootstrapPeersChanged {} =
      Namespace [] ["UseBootstrapPeersChanged"]
--}
    namespaceFor TraceVerifyPeerSnapshot {} =
      Namespace [] ["VerifyPeerSnapshot"]
    namespaceFor TraceBootstrapPeersFlagChangedWhilstInSensitiveState =
      Namespace [] ["BootstrapPeersFlagChangedWhilstInSensitiveState"]
    namespaceFor TraceOutboundGovernorCriticalFailure {} =
      Namespace [] ["OutboundGovernorCriticalFailure"]
    namespaceFor TraceChurnAction {} =
      Namespace [] ["ChurnAction"]
    namespaceFor TraceChurnTimeout {} =
      Namespace [] ["ChurnTimeout"]
    namespaceFor TraceDebugState {} =
      Namespace [] ["DebugState"]
{-- TODO:
       >     Pattern match(es) are non-exhaustive
       >     In an equation for ‘namespaceFor’:
       >         Patterns of type ‘TracePeerSelection
       >                             extraDebugState
       >                             extraFlags
       >                             extraPeers
       >                             extraTrace
       >                             SockAddr’ not matched:
       >             ExtraTrace _
       >     |
       > 467 |     namespaceFor TraceLocalRootPeersChanged {} =
--}
    namespaceFor _ =
      Namespace [] []

    severityFor (Namespace [] ["LocalRootPeersChanged"]) _ = Just Notice
    severityFor (Namespace [] ["TargetsChanged"]) _ = Just Notice
    severityFor (Namespace [] ["PublicRootsRequest"]) _ = Just Info
    severityFor (Namespace [] ["PublicRootsResults"]) _ = Just Info
    severityFor (Namespace [] ["PublicRootsFailure"]) _ = Just Error
    severityFor (Namespace [] ["ForgetColdPeers"]) _ = Just Info
    severityFor (Namespace [] ["BigLedgerPeersRequest"]) _ = Just Info
    severityFor (Namespace [] ["BigLedgerPeersResults"]) _ = Just Info
    severityFor (Namespace [] ["BigLedgerPeersFailure"]) _ = Just Info
    severityFor (Namespace [] ["ForgetBigLedgerPeers"]) _ = Just Info
    severityFor (Namespace [] ["PeerShareRequests"]) _ = Just Debug
    severityFor (Namespace [] ["PeerShareResults"]) _ = Just Debug
    severityFor (Namespace [] ["PeerShareResultsFiltered"]) _ = Just Info
    severityFor (Namespace [] ["PickInboundPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdLocalPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdFailed"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdDone"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdBigLedgerPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdBigLedgerPeerFailed"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdBigLedgerPeerDone"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmLocalPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmFailed"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmDone"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmAborted"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmBigLedgerPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmBigLedgerPeerFailed"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmBigLedgerPeerDone"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmBigLedgerPeerAborted"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmPeers"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmFailed"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmFailed", "CoolingToColdTimeout"]) _ = Just Error
    severityFor (Namespace [] ["DemoteWarmDone"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmBigLedgerPeers"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmBigLedgerPeerFailed"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmBigLedgerPeerFailed", "CoolingToColdTimeout"]) _ = Just Error
    severityFor (Namespace [] ["DemoteWarmBigLedgerPeerDone"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotPeers"]) _ = Just Info
    severityFor (Namespace [] ["DemoteLocalHotPeers"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotFailed"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotFailed", "CoolingToColdTimeout"]) _ = Just Error
    severityFor (Namespace [] ["DemoteHotDone"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotBigLedgerPeers"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotBigLedgerPeerFailed"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotBigLedgerPeerFailed", "CoolingToColdTimeout"]) _ = Just Error
    severityFor (Namespace [] ["DemoteHotBigLedgerPeerDone"]) _ = Just Info
    severityFor (Namespace [] ["DemoteAsynchronous"]) _ = Just Info
    severityFor (Namespace [] ["DemoteLocalAsynchronous"]) _ = Just Warning
    severityFor (Namespace [] ["DemoteBigLedgerPeersAsynchronous"]) _ = Just Info
    severityFor (Namespace [] ["GovernorWakeup"]) _ = Just Info
    severityFor (Namespace [] ["ChurnWait"]) _ = Just Info
    severityFor (Namespace [] ["LedgerStateJudgementChanged"]) _ = Just Info
    severityFor (Namespace [] ["OnlyBootstrapPeers"]) _ = Just Info
    severityFor (Namespace [] ["UseBootstrapPeersChanged"]) _ = Just Notice
    severityFor (Namespace [] ["VerifyPeerSnapshot"]) _ = Just Error
    severityFor (Namespace [] ["BootstrapPeersFlagChangedWhilstInSensitiveState"]) _ = Just Warning
    severityFor (Namespace [] ["OutboundGovernorCriticalFailure"]) _ = Just Error
    severityFor (Namespace [] ["ChurnAction"]) _ = Just Info
    severityFor (Namespace [] ["ChurnTimeout"]) _ = Just Notice
    severityFor (Namespace [] ["DebugState"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace [] ["LocalRootPeersChanged"]) = Just  ""
    documentFor (Namespace [] ["TargetsChanged"]) = Just  ""
    documentFor (Namespace [] ["PublicRootsRequest"]) = Just  ""
    documentFor (Namespace [] ["PublicRootsResults"]) = Just  ""
    documentFor (Namespace [] ["PublicRootsFailure"]) = Just  ""
    documentFor (Namespace [] ["PeerShareRequests"]) = Just $ mconcat
      [ "target known peers, actual known peers, peers available for gossip,"
      , " peers selected for gossip"
      ]
    documentFor (Namespace [] ["PeerShareResults"]) = Just  ""
    documentFor (Namespace [] ["ForgetColdPeers"]) = Just
      "target known peers, actual known peers, selected peers"
    documentFor (Namespace [] ["PromoteColdPeers"]) = Just
      "target established, actual established, selected peers"
    documentFor (Namespace [] ["PromoteColdLocalPeers"]) = Just
      "target local established, actual local established, selected peers"
    documentFor (Namespace [] ["PromoteColdFailed"]) = Just $ mconcat
      [ "target established, actual established, peer, delay until next"
      , " promotion, reason"
      ]
    documentFor (Namespace [] ["PromoteColdDone"]) = Just
      "target active, actual active, selected peers"
    documentFor (Namespace [] ["PromoteWarmPeers"]) = Just
      "target active, actual active, selected peers"
    documentFor (Namespace [] ["PromoteWarmLocalPeers"]) = Just
      "local per-group (target active, actual active), selected peers"
    documentFor (Namespace [] ["PromoteWarmFailed"]) = Just
      "target active, actual active, peer, reason"
    documentFor (Namespace [] ["PromoteWarmDone"]) = Just
      "target active, actual active, peer"
    documentFor (Namespace [] ["PromoteWarmAborted"]) = Just ""
    documentFor (Namespace [] ["DemoteWarmPeers"]) = Just
      "target established, actual established, selected peers"
    documentFor (Namespace [] ["DemoteWarmFailed"]) = Just
      "target established, actual established, peer, reason"
    documentFor (Namespace [] ["DemoteWarmFailed", "CoolingToColdTimeout"]) =
      Just "Impossible asynchronous demotion timeout"
    documentFor (Namespace [] ["DemoteWarmBigLedgerPeerFailed", "CoolingToColdTimeout"]) =
      Just "Impossible asynchronous demotion timeout"
    documentFor (Namespace [] ["DemoteWarmDone"]) = Just
      "target established, actual established, peer"
    documentFor (Namespace [] ["DemoteHotPeers"]) = Just
      "target active, actual active, selected peers"
    documentFor (Namespace [] ["DemoteLocalHotPeers"]) = Just
      "local per-group (target active, actual active), selected peers"
    documentFor (Namespace [] ["DemoteHotFailed"]) = Just
      "target active, actual active, peer, reason"
    documentFor (Namespace [] ["DemoteHotFailed", "CoolingToColdTimeout"]) =
      Just "Impossible asynchronous demotion timeout"
    documentFor (Namespace [] ["DemoteHotBigLedgerPeerFailed", "CoolingToColdTimeout"]) =
      Just "Impossible asynchronous demotion timeout"
    documentFor (Namespace [] ["DemoteHotDone"]) = Just
      "target active, actual active, peer"
    documentFor (Namespace [] ["DemoteAsynchronous"]) = Just  ""
    documentFor (Namespace [] ["DemoteLocalAsynchronous"]) = Just  ""
    documentFor (Namespace [] ["GovernorWakeup"]) = Just  ""
    documentFor (Namespace [] ["ChurnWait"]) = Just  ""
    documentFor (Namespace [] ["PickInboundPeers"]) = Just
      "An inbound connection was added to known set of outbound governor"
    documentFor (Namespace [] ["OutboundGovernorCriticalFailure"]) = Just
      "Outbound Governor was killed unexpectedly"
    documentFor (Namespace [] ["DebugState"]) = Just
      "peer selection internal state"
    documentFor (Namespace [] ["VerifyPeerSnapshot"]) = Just
      "Verification outcome of big ledger peer snapshot"
    documentFor _ = Nothing

    metricsDocFor (Namespace [] ["ChurnAction"]) =
     [ ("peerSelection.churn.DecreasedActivePeers.duration", "")
     , ("peerSelection.churn.DecreasedActiveBigLedgerPeers.duration", "")
     , ("peerSelection.churn.DecreasedEstablishedPeers.duration", "")
     , ("peerSelection.churn.DecreasedEstablishedBigLedgerPeers.duration", "")
     , ("peerSelection.churn.DecreasedKnownPeers.duration", "")
     , ("peerSelection.churn.DecreasedKnownBigLedgerPeers.duration", "")
     ]
    metricsDocFor _ = []

    allNamespaces = [
        Namespace [] ["LocalRootPeersChanged"]
      , Namespace [] ["TargetsChanged"]
      , Namespace [] ["PublicRootsRequest"]
      , Namespace [] ["PublicRootsResults"]
      , Namespace [] ["PublicRootsFailure"]
      , Namespace [] ["ForgetColdPeers"]
      , Namespace [] ["BigLedgerPeersRequest"]
      , Namespace [] ["BigLedgerPeersResults"]
      , Namespace [] ["BigLedgerPeersFailure"]
      , Namespace [] ["ForgetBigLedgerPeers"]
      , Namespace [] ["PeerShareRequests"]
      , Namespace [] ["PeerShareResults"]
      , Namespace [] ["PeerShareResultsFiltered"]
      , Namespace [] ["PickInboundPeers"]
      , Namespace [] ["PromoteColdPeers"]
      , Namespace [] ["PromoteColdLocalPeers"]
      , Namespace [] ["PromoteColdFailed"]
      , Namespace [] ["PromoteColdDone"]
      , Namespace [] ["PromoteColdBigLedgerPeers"]
      , Namespace [] ["PromoteColdBigLedgerPeerFailed"]
      , Namespace [] ["PromoteColdBigLedgerPeerDone"]
      , Namespace [] ["PromoteWarmPeers"]
      , Namespace [] ["PromoteWarmLocalPeers"]
      , Namespace [] ["PromoteWarmFailed"]
      , Namespace [] ["PromoteWarmDone"]
      , Namespace [] ["PromoteWarmAborted"]
      , Namespace [] ["PromoteWarmBigLedgerPeers"]
      , Namespace [] ["PromoteWarmBigLedgerPeerFailed"]
      , Namespace [] ["PromoteWarmBigLedgerPeerDone"]
      , Namespace [] ["PromoteWarmBigLedgerPeerAborted"]
      , Namespace [] ["DemoteWarmPeers"]
      , Namespace [] ["DemoteWarmFailed"]
      , Namespace [] ["DemoteWarmFailed", "CoolingToColdTimeout"]
      , Namespace [] ["DemoteWarmDone"]
      , Namespace [] ["DemoteWarmBigLedgerPeers"]
      , Namespace [] ["DemoteWarmBigLedgerPeerFailed"]
      , Namespace [] ["DemoteWarmBigLedgerPeerFailed", "CoolingToColdTimeout"]
      , Namespace [] ["DemoteWarmBigLedgerPeerDone"]
      , Namespace [] ["DemoteHotPeers"]
      , Namespace [] ["DemoteLocalHotPeers"]
      , Namespace [] ["DemoteHotFailed"]
      , Namespace [] ["DemoteHotFailed", "CoolingToColdTimeout"]
      , Namespace [] ["DemoteHotDone"]
      , Namespace [] ["DemoteHotBigLedgerPeers"]
      , Namespace [] ["DemoteHotBigLedgerPeerFailed"]
      , Namespace [] ["DemoteHotBigLedgerPeerFailed", "CoolingToColdTimeout"]
      , Namespace [] ["DemoteHotBigLedgerPeerDone"]
      , Namespace [] ["DemoteAsynchronous"]
      , Namespace [] ["DemoteLocalAsynchronous"]
      , Namespace [] ["DemoteBigLedgerPeersAsynchronous"]
      , Namespace [] ["GovernorWakeup"]
      , Namespace [] ["ChurnWait"]
      , Namespace [] ["ChurnAction"]
      , Namespace [] ["ChurnTimeout"]
      , Namespace [] ["LedgerStateJudgementChanged"]
      , Namespace [] ["OnlyBootstrapPeers"]
      , Namespace [] ["BootstrapPeersFlagChangedWhilstInSensitiveState"]
      , Namespace [] ["UseBootstrapPeersChanged"]
      , Namespace [] ["VerifyPeerSnapshot"]
      , Namespace [] ["OutboundGovernorCriticalFailure"]
      , Namespace [] ["DebugState"]
      ]

