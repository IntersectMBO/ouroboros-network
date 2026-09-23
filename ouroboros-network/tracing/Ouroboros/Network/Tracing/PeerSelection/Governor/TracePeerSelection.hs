{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Tracing.PeerSelection.Governor.TracePeerSelection (JSONField (..)) where

--------------------------------------------------------------------------------


import Control.Exception (fromException)
import Data.Aeson (ToJSON, ToJSONKey, Value (String), object, toJSON,
           toJSONList, (.=))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)

import Cardano.Logging
import Ouroboros.Network.Diffusion.Types
import Ouroboros.Network.OrphanInstances (JSONField (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (DebugPeerSelectionState (..), DemotionTimeoutException,
           SupportsPeerSelectionState (..), TracePeerSelection (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.Protocol.PeerSharing.Type
           (PeerSharingAmount (PeerSharingAmount))
import Ouroboros.Network.Tracing.PeerSelection.Governor.Utils
           (peerSelectionTargetsToObject)

--------------------------------------------------------------------------------
-- PeerSelection Tracer
--------------------------------------------------------------------------------

instance ( Show extraDebugState
         , Show extraFlags
         , ToJSON extraFlags
         , JSONField extraFlags
         , ToJSON ntnAddr
         , ToJSON (PublicRootPeers extraPeers ntnAddr)
         , ToJSONKey ntnAddr
         , SupportsPeerSelectionState extraPeers ntnAddr
         , LogFormatting (ToExtraTrace extraPeers)
         ) => LogFormatting (TracePeerSelection extraDebugState extraFlags
                               extraPeers ntnAddr) where
  forMachine _dtal (TraceLocalRootPeersChanged lrp lrp') =
    mconcat [ "kind" .= String "LocalRootPeersChanged"
             , "previous" .= toJSON lrp
             , "current" .= toJSON lrp'
             ]
  forMachine _dtal (TraceTargetsChanged pst) =
    mconcat [ "kind" .= String "TargetsChanged"
             , "current" .= toJSON pst
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
  forMachine _dtal (TraceDemoteHotPeers tActive aActive sp scores) =
    mconcat [ "kind" .= String "DemoteHotPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             , "scores" .= scoresToJSON scores
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
  forMachine _dtal (TraceDemoteHotBigLedgerPeers tActive aActive sp scores) =
    mconcat [ "kind" .= String "DemoteHotBigLedgerPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             , "scores" .= scoresToJSON scores
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
  forMachine _dtal (TraceForgottenPeers peers) =
    mconcat [ "kind" .= String "ForgottenPeers"
            , "peers" .= peers
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
  forMachine _dtal TraceOnlyBootstrapPeers =
    mconcat [ "kind" .= String "LedgerStateJudgementChanged" ]
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

  forMachine dtal (ExtraTrace tr) = forMachine dtal tr

  forHuman (ExtraTrace tr) = pack . show $ tr
  forHuman tr              = pack . show $ tr

  asMetrics (TraceChurnAction duration action _) =
    [ DoubleM ("peerSelection.churn" <> pack (show action) <> ".duration")
              (realToFrac duration)
    ]
  asMetrics (TraceDemoteHotPeers _ _ demoted scores) =
    hotDemotionMetrics "peerSelection.churn.hot." demoted scores
  asMetrics (TraceDemoteHotBigLedgerPeers _ _ demoted scores) =
    hotDemotionMetrics "peerSelection.churn.bigLedger." demoted scores
  asMetrics _ = []


instance MetaTrace (ToExtraTrace extraPeers)
      => MetaTrace (TracePeerSelection extraDebugState extraFlags
                          extraPeers ntnAddr) where
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
    namespaceFor TraceForgottenPeers {} =
      Namespace [] ["TraceForgottenPeers"]
    namespaceFor TraceGovernorWakeup {}        =
      Namespace [] ["GovernorWakeup"]
    namespaceFor TraceChurnWait {}             =
      Namespace [] ["ChurnWait"]
    namespaceFor TraceOnlyBootstrapPeers {} =
      Namespace [] ["OnlyBootstrapPeers"]
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
    namespaceFor (ExtraTrace et) =
      nsCast $ namespaceFor et

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
    severityFor ns tr = case tr of
      Just (ExtraTrace et) -> severityFor (nsCast ns :: Namespace (ToExtraTrace extraPeers)) (Just et)
      Just _ -> Nothing
      Nothing -> severityFor (nsCast ns :: Namespace (ToExtraTrace extraPeers)) Nothing

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
      "target active, actual active, selected peers, scores of the peers available to demote"
    documentFor (Namespace [] ["DemoteHotBigLedgerPeers"]) = Just
      "target active big ledger peers, actual active big ledger peers, selected peers, scores of the peers available to demote"
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
    documentFor ns = documentFor (nsCast ns :: Namespace (ToExtraTrace extraPeers))

    metricsDocFor (Namespace [] ["ChurnAction"]) =
     [ ("peerSelection.churn.DecreasedActivePeers.duration", "")
     , ("peerSelection.churn.DecreasedActiveBigLedgerPeers.duration", "")
     , ("peerSelection.churn.DecreasedEstablishedPeers.duration", "")
     , ("peerSelection.churn.DecreasedEstablishedBigLedgerPeers.duration", "")
     , ("peerSelection.churn.DecreasedKnownPeers.duration", "")
     , ("peerSelection.churn.DecreasedKnownBigLedgerPeers.duration", "")
     ]
    metricsDocFor (Namespace [] ["DemoteHotPeers"]) =
      hotDemotionMetricsDoc "peerSelection.churn.hot." "hot peers"
    metricsDocFor (Namespace [] ["DemoteHotBigLedgerPeers"]) =
      hotDemotionMetricsDoc "peerSelection.churn.bigLedger." "hot big ledger peers"
    metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace (ToExtraTrace extraPeers))

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
      , Namespace [] ["OnlyBootstrapPeers"]
      , Namespace [] ["BootstrapPeersFlagChangedWhilstInSensitiveState"]
      , Namespace [] ["VerifyPeerSnapshot"]
      , Namespace [] ["OutboundGovernorCriticalFailure"]
      , Namespace [] ["DebugState"]
      ] ++ map nsCast (allNamespaces :: [Namespace (ToExtraTrace extraPeers)])


-- satisfy superclass constraints for plain-Ouroboros instantiation

instance LogFormatting (ToExtraTrace (NoExtraPeers peeraddr)) where
  forMachine _dtal _ =
    mconcat [ "kind" .= String "ExtraTrace"
            , "error" .= String "impossible ouroboros-network error: Unexpected trace for NoExtraPeers"
            ]

  forHuman _ = "impossible ouroboros-network error: Unexpected ExtraTrace tag for NoExtraPeers"

instance MetaTrace (ToExtraTrace (NoExtraPeers peeraddr)) where
  namespaceFor _ = Namespace [] ["Error"]
  severityFor _ _ = Just Error
  documentFor _ = Just "This should just never happen"
  allNamespaces = [Namespace [] ["Error"]]


instance LogFormatting (ViewExtraPeers (NoExtraPeers peeraddr)) where
  forMachine _dtal _ = mempty

instance MetaTrace (ViewExtraPeers (NoExtraPeers peeraddr)) where
  namespaceFor _ = Namespace [] ["Counters"]
  severityFor _ _ = Nothing
  documentFor _ = Nothing
  allNamespaces = [Namespace [] ["Counters"]]


--------------------------------------------------------------------------------
-- Hot demotion metrics
--------------------------------------------------------------------------------

-- | Gauges describing a hot demotion decision, derived from the scores of the
-- peers which were available to demote and the set which was picked:
--
-- * @demotedTopScore@: the highest score among the demoted peers, i.e. the
--   score a peer had to beat in order to stay hot;
-- * @retainedBottomScore@, @retainedMedianScore@, @retainedTopScore@: the
--   lowest, (lower) median and highest score among the peers which stayed
--   hot; the top one is the incumbent an attacker would have to out-score;
-- * @zeroScorers@: how many of the available peers had no score at all;
-- * @scoreSum@: the sum of the scores of all available peers;
-- * @eligiblePeers@: how many peers were available to demote, so that the
--   counts above can be read as fractions;
-- * @retainedGiniPermille@: the Gini coefficient of the retained peers'
--   scores, in per-mille, a scale-free measure of how concentrated the
--   scores are among the peers which stay hot;
-- * @topDSharePermille@: the share of the total score held by as many of
--   the strongest peers as were demoted, in per-mille.
--
-- The order statistics and the two ratios are omitted when undefined.
--
hotDemotionMetrics :: Ord peeraddr
                   => Text
                   -- ^ metric name prefix
                   -> Set peeraddr
                   -- ^ demoted peers
                   -> Map peeraddr Int
                   -- ^ scores of all the peers available to demote
                   -> [Metric]
hotDemotionMetrics prefix demoted scores =
       [ IntM (prefix <> "demotedTopScore") (fromIntegral score)
       | Just score <- [maximumMay demotedScores] ]
    ++ [ IntM (prefix <> "retainedBottomScore") (fromIntegral score)
       | Just score <- [minimumMay retainedScores] ]
    ++ [ IntM (prefix <> "retainedMedianScore") (fromIntegral score)
       | Just score <- [medianMay retainedScores] ]
    ++ [ IntM (prefix <> "retainedTopScore") (fromIntegral score)
       | Just score <- [maximumMay retainedScores] ]
    ++ [ IntM (prefix <> "zeroScorers")
              (fromIntegral (Map.size (Map.filter (== 0) scores)))
       , IntM (prefix <> "scoreSum") (fromIntegral (sum scores))
       , IntM (prefix <> "eligiblePeers") (fromIntegral (Map.size scores))
       ]
    ++ [ IntM (prefix <> "retainedGiniPermille") g
       | Just g <- [giniPermille (Map.elems retainedScores)] ]
    ++ [ IntM (prefix <> "topDSharePermille") s
       | Just s <- [topSharePermille (Set.size demoted) (Map.elems scores)] ]
  where
    demotedScores  = Map.restrictKeys scores demoted
    retainedScores = Map.withoutKeys scores demoted
    -- Gini coefficient, sorted-order formula, undefined for an empty or all-zero set
    giniPermille xs
      | null xs || total == 0 = Nothing
      | otherwise = Just (round (1000 * num / (fromIntegral n * total)) :: Integer)
      where
        sorted = sort xs
        n      = length xs
        total  = fromIntegral (sum xs) :: Double
        num    = sum [ fromIntegral ((2 * i - n - 1) * x) | (i, x) <- zip [1 ..] sorted ] :: Double
    -- share of the total score held by the d highest scores
    topSharePermille d xs
      | d <= 0 || null xs || total == 0 = Nothing
      | otherwise = Just (round (1000 * top / total) :: Integer)
      where
        total = fromIntegral (sum xs) :: Double
        top   = fromIntegral (sum (take d (sortOn negate xs))) :: Double
    maximumMay m
      | Map.null m = Nothing
      | otherwise  = Just (maximum m)
    minimumMay m
      | Map.null m = Nothing
      | otherwise  = Just (minimum m)
    -- the lower median
    medianMay m
      | Map.null m = Nothing
      | otherwise  = Just (sort (Map.elems m) !! ((Map.size m - 1) `div` 2))

-- | Documentation of the gauges produced by 'hotDemotionMetrics'.
--
hotDemotionMetricsDoc :: Text
                      -- ^ metric name prefix
                      -> Text
                      -- ^ what the peers are, e.g. @"hot peers"@
                      -> [(Text, Text)]
hotDemotionMetricsDoc prefix what =
    [ ( prefix <> "demotedTopScore"
      , "highest demotion score among the " <> what <> " demoted to warm; "
        <> "the score a peer had to beat to stay hot" )
    , ( prefix <> "retainedBottomScore"
      , "lowest demotion score among the " <> what <> " which stayed hot" )
    , ( prefix <> "retainedMedianScore"
      , "median demotion score among the " <> what <> " which stayed hot" )
    , ( prefix <> "retainedTopScore"
      , "highest demotion score among the " <> what <> " which stayed hot; "
        <> "the incumbent an attacker would have to out-score" )
    , ( prefix <> "zeroScorers"
      , "number of " <> what <> " without any demotion score" )
    , ( prefix <> "scoreSum"
      , "sum of the demotion scores of all " <> what )
    , ( prefix <> "eligiblePeers"
      , "number of " <> what <> " which were available to demote" )
    , ( prefix <> "retainedGiniPermille"
      , "Gini coefficient, in per-mille, of the demotion scores of the "
        <> what <> " which stayed hot" )
    , ( prefix <> "topDSharePermille"
      , "share, in per-mille, of all demotion scores held by as many of the "
        <> "strongest " <> what <> " as were demoted" )
    ]

-- | Render the scores of the peers available to demote.
--
scoresToJSON :: ToJSON peeraddr => Map peeraddr Int -> Value
scoresToJSON scores =
    toJSONList [ object [ "peer" .= peer, "score" .= score ]
               | (peer, score) <- Map.toList scores ]
