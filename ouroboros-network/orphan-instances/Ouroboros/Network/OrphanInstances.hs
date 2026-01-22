{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan JSON instances for Ouroboros.Network types.
--
module Ouroboros.Network.OrphanInstances
  ( networkTopologyFromJSON
  , localRootPeersGroupsFromJSON
  , networkTopologyToJSON
  , localRootPeersGroupsToJSON
  , peerSelectionTargetsToObject
  ) where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (Exception (..))
import Control.Monad (zipWithM)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser, listValue)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.IP (fromHostAddress, fromHostAddress6)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text

import Network.Mux.Trace qualified as Mux
import Network.Mux.Types qualified as Mux
import Network.Socket (SockAddr (..))

import Network.TypedProtocol.Codec (AnyMessage (..))

import Ouroboros.Network.Protocol.Handshake (HandshakeException (..),
           HandshakeProtocolError (..), RefuseReason (..))
import Ouroboros.Network.Protocol.Handshake.Type (Handshake, Message (..))
import Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KeepAlive
import Ouroboros.Network.Protocol.Limits
           (ProtocolLimitFailure (ExceededSizeLimit, ExceededTimeLimit))
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as LocalTxSubmission
import Ouroboros.Network.Protocol.PeerSharing.Type qualified as PeerSharing
import Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as Tx

import Ouroboros.Network.Block (SlotNo (SlotNo))
import Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.Core as ConnMgr
import Ouroboros.Network.ConnectionManager.State (ConnMap (..),
           ConnStateId (..), LocalAddr (..))
import Ouroboros.Network.ConnectionManager.Types (AbstractState (..),
           ConnectionManagerCounters (..), MaybeUnknown (..),
           OperationResult (..), Provenance (..))
import Ouroboros.Network.ConnectionManager.Types qualified as ConnMgr
import Ouroboros.Network.DeltaQ (GSV (GSV),
           PeerGSV (PeerGSV, inboundGSV, outboundGSV))
import Ouroboros.Network.Diffusion.Topology (LocalRootPeersGroup (..),
           LocalRootPeersGroups (..), LocalRoots (..), NetworkTopology (..),
           PublicRootPeers (..), RootConfig (..))
import Ouroboros.Network.Diffusion.Types (DiffusionTracer (..))
import Ouroboros.Network.DiffusionMode
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.ExitPolicy (RepromoteDelay (repromoteDelay))
import Ouroboros.Network.InboundGovernor qualified as InboundGovernor
import Ouroboros.Network.InboundGovernor.State (RemoteSt)
import Ouroboros.Network.InboundGovernor.State qualified as InboundGovernor
import Ouroboros.Network.Mux (MiniProtocolNum (..))
import Ouroboros.Network.PeerSelection hiding (PublicRootPeers)
import Ouroboros.Network.PeerSelection.Churn
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.Server qualified as Server
import Ouroboros.Network.Server.RateLimiting (AcceptConnectionsPolicyTrace (..),
           AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (LocalAddress (..), RemoteAddress)
import Ouroboros.Network.TxSubmission.Inbound.V2 (ProcessedTxCount (..),
           TraceTxLogic (..), TraceTxSubmissionInbound (..))
import Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))

-- Helper function for ToJSON instances with a "kind" field
kindObject :: Text -> [Pair] -> Value
kindObject k fields = object $ ("kind" .= String k) : fields

-- FromJSON Instances

instance FromJSON RootConfig where
  parseJSON = withObject "RootConfig" $ \o ->
                RootConfig
                  <$> o .:  "accessPoints"
                  <*> o .:? "advertise" .!= DoNotAdvertisePeer

instance ToJSON RootConfig where
  toJSON ra = object
    [ "accessPoints" .= rootAccessPoints ra
    , "advertise"    .= rootAdvertise ra
    ]

instance FromJSON DiffusionMode where
  parseJSON = withText "DiffusionMode" $ \str ->
    case str of
      "InitiatorOnly"         -> pure InitiatorOnlyDiffusionMode
      "InitiatorAndResponder" -> pure InitiatorAndResponderDiffusionMode
      _ -> fail "Parsing NodeDiffusionMode failed: can be either 'InitiatorOnly' or 'InitiatorAndResponder'"

instance ToJSON DiffusionMode where
  toJSON InitiatorOnlyDiffusionMode         = "InitiatorOnly"
  toJSON InitiatorAndResponderDiffusionMode = "InitiatorAndResponder"

-- | Does not use the 'FromJSON' instance of 'RootConfig', so that
-- 'accessPoints', 'advertise', 'valency' and 'warmValency' fields are attached
-- to the same object.
localRootPeersGroupFromJSON
  :: (Object -> Parser extraFlags)
  -> Object
  -> Parser (LocalRootPeersGroup extraFlags)
localRootPeersGroupFromJSON parseExtraFlags o = do
    hv@(HotValency v) <- o .: "valency"
                     <|> o .: "hotValency"
    accessPoint <- o .:  "accessPoints"
    ad <- o .:? "advertise" .!= DoNotAdvertisePeer
    behindFirewall <- o .:? "behindFirewall" .!= False
    LocalRootPeersGroup
      (LocalRoots
        (RootConfig accessPoint ad)
        (bool Outbound Inbound behindFirewall)
      )
      <$> pure hv
      <*> o .:? "warmValency" .!= WarmValency v
      <*> o .:? "diffusionMode" .!= InitiatorAndResponderDiffusionMode
      <*> parseExtraFlags o

localRootPeersGroupToJSON :: (extraFlags -> Maybe (Key, Value))
                          -- ^ if `Nothing` is returned the `extraFlags` are
                          -- not encoded in the JSON value
                          -> LocalRootPeersGroup extraFlags
                          -> Value
localRootPeersGroupToJSON extraFlagsToJSON lrpg@LocalRootPeersGroup {extraFlags} =
    Object $
         ("accessPoints"   .?= rootAccessPoints (rootConfig . localRoots $ lrpg))
      <> ("advertise"      .?= rootAdvertise (rootConfig . localRoots  $ lrpg))
      <> ("behindFirewall" .?= case provenance (localRoots lrpg) of
                                 Outbound -> False
                                 Inbound  -> True)
      <> ("hotValency"     .?= hotValency lrpg)
      <> ("warmValency"    .?= warmValency lrpg)
      <> foldMap (uncurry (.?=)) (extraFlagsToJSON extraFlags)
      <> ("diffusionMode"  .?= rootDiffusionMode lrpg)

localRootPeersGroupsFromJSON
  :: (Object -> Parser extraFlags)
  -> Value
  -> Parser (LocalRootPeersGroups extraFlags)
localRootPeersGroupsFromJSON parseExtraFlags =
  withArray "[]" $ \a ->
      fmap LocalRootPeersGroups
    . zipWithM
        (parseIndexedJSON $ withObject "LocalRootPeersGroup" (localRootPeersGroupFromJSON parseExtraFlags))
        [0..]
    . toList
    $ a

localRootPeersGroupsToJSON
  :: (extraFlags -> Maybe (Key, Value))
  -> LocalRootPeersGroups extraFlags
  -> Value
localRootPeersGroupsToJSON extraFlagsToJSON LocalRootPeersGroups {groups} =
  listValue (localRootPeersGroupToJSON extraFlagsToJSON) groups

instance FromJSON PublicRootPeers where
  parseJSON = fmap PublicRootPeers . parseJSON

instance ToJSON PublicRootPeers where
  toJSON = toJSON . publicRoots

networkTopologyFromJSON
  :: (Value -> Parser (LocalRootPeersGroups extraFlags))
  -> (Object -> Parser extraConfig)
  -> Value
  -> Parser (NetworkTopology extraConfig extraFlags)
networkTopologyFromJSON parseLocalRoots parseExtraConfig =
  withObject "NetworkTopology" $ \o ->
              NetworkTopology <$> (o .: "localRoots" >>= parseLocalRoots)
                              <*> o .:  "publicRoots"
                              <*> o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers
                              <*> o .:? "peerSnapshotFile"
                              <*> parseExtraConfig o

networkTopologyToJSON
  :: (extraConfig -> Maybe (Key, Value))
  -> (extraFlags  -> Maybe (Key, Value))
  -> NetworkTopology extraConfig extraFlags
  -> Value
networkTopologyToJSON
  extraConfigToJSON
  extraFlagsToJSON
  NetworkTopology {
    localRootPeersGroups,
    publicRootPeers,
    useLedgerPeers,
    peerSnapshotPath,
    extraConfig
  } =
    Object $
         (explicitToFieldOmit (const False) id "localRoots" (localRootPeersGroupsToJSON extraFlagsToJSON localRootPeersGroups))
      <> ("publicRoots"        .?= publicRootPeers)
      <> ("useLedgerAfterSlot" .?= useLedgerPeers)
      <> ("peerSnapshotFile"   .?= peerSnapshotPath)
      <> foldMap (uncurry (.?=)) (extraConfigToJSON extraConfig)

instance FromJSON PeerSharing where
  parseJSON = withBool "PeerSharing" $ \b ->
    pure $ if b then PeerSharingEnabled
                else PeerSharingDisabled

instance ToJSON PeerSharing where
  toJSON PeerSharingEnabled  = Bool True
  toJSON PeerSharingDisabled = Bool False

instance FromJSON UseLedgerPeers where
  parseJSON = \case
    Number slot -> return $
      case compare slot 0 of
        GT -> UseLedgerPeers (After (SlotNo (floor slot)))
        EQ -> UseLedgerPeers Always
        LT -> DontUseLedgerPeers
    invalid -> fail $ "Parsing of slot number failed due to type mismatch. Encountered: " <> show invalid

instance ToJSON UseLedgerPeers where
  toJSON = \case
    DontUseLedgerPeers                -> Number (-1)
    UseLedgerPeers Always             -> Number 0
    UseLedgerPeers (After (SlotNo s)) -> Number (fromIntegral s)

instance FromJSON AssociationMode where
  parseJSON = withText "AssociationMode" $ \case
    "LocalRootsOnly" -> pure LocalRootsOnly
    "Unrestricted"   -> pure Unrestricted
    _                -> fail "Invalid JSON for AssociationMode"

instance ToJSON AssociationMode where
  toJSON LocalRootsOnly = String "LocalRootsOnly"
  toJSON Unrestricted   = String "Unrestricted"

instance FromJSON HotValency where
  parseJSON v = HotValency <$> parseJSON v

instance ToJSON HotValency where
  toJSON (HotValency v) = toJSON v

instance FromJSON WarmValency where
  parseJSON v = WarmValency <$> parseJSON v

instance ToJSON WarmValency where
  toJSON (WarmValency v) = toJSON v

instance ToJSON AcceptedConnectionsLimit where
  toJSON AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit
          , acceptedConnectionsSoftLimit
          , acceptedConnectionsDelay
          } =
    object [ "AcceptedConnectionsLimit" .=
      object [ "hardLimit" .=
                  toJSON acceptedConnectionsHardLimit
             , "softLimit" .=
                  toJSON acceptedConnectionsSoftLimit
             , "delay" .=
                  toJSON acceptedConnectionsDelay
             ]
           ]

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"

-- This instance is quite verbose, especially if a MiniProtocolNum is included
-- as a field, e.g.
--
-- > { miniProtocolNum = { kind: "MiniProtocolNum", num: 2 }, ... }
--
-- TODO: can we change it?
--
instance ToJSON MiniProtocolNum where
  toJSON (MiniProtocolNum w) = kindObject "MiniProtocolNum" [ "num" .= w ]

instance ToJSON addr => ToJSON (OperationResult addr) where
  toJSON = \case
    UnsupportedState as     -> kindObject "UnsupportedState" [ "unsupportedState" .= toJSON as ]
    OperationSuccess addr   -> kindObject "OperationSuccess" [ "operationSuccess" .= toJSON addr ]
    TerminatedConnection as -> kindObject "TerminatedConnection" [ "terminatedConnection" .= toJSON as ]

instance ToJSON RemoteSt where
  toJSON = String . pack . show

instance ToJSON state => ToJSON (MaybeUnknown state) where
  toJSON = \case
    Known st -> object [ "state" .= toJSON st, "type" .= String "known" ]
    Race st  -> object [ "state" .= toJSON st, "type" .= String "race" ]
    Unknown  -> object [ "type" .= String "unknown" ]

instance ToJSON addr => ToJSON (LocalAddr addr) where
  toJSON (LocalAddr addr) = toJSON addr
  toJSON UnknownLocalAddr = Null

instance ToJSON ConnStateId where
  toJSON (ConnStateId connStateId) = toJSON connStateId

instance ToJSON ProtocolLimitFailure where
  toJSON = \case
    ExceededSizeLimit tok -> kindObject "ProtocolLimitFailure" [ "agency" .= show tok ]
    ExceededTimeLimit tok -> kindObject "ProtocolLimitFailure" [ "agency" .= show tok ]

instance ToJSON vNumber => ToJSON (RefuseReason vNumber) where
  toJSON = \case
    VersionMismatch vNumber tags -> kindObject "VersionMismatch"
      [ "versionNumbers" .= vNumber
      , "unknownVersionNumbers" .= toJSONList tags
      ]
    HandshakeDecodeError vNumber t -> kindObject "HandshakeDecodeError"
      [ "versionNumber" .= vNumber
      , "decodeError" .= String t
      ]
    Refused vNumber t -> kindObject "Refused"
      [ "versionNumber" .= vNumber
      , "text" .= String t
      ]

instance ToJSON vNumber => ToJSON (HandshakeProtocolError vNumber) where
  toJSON = \case
    HandshakeError rvNumber          -> kindObject "HandshakeError" [ "reason" .= rvNumber ]
    NotRecognisedVersion vNumber     -> kindObject "NotRecognisedVersion" [ "versionNumber" .= vNumber ]
    InvalidServerSelection vNumber t -> kindObject "InvalidServerSelection"
      [ "versionNumber" .= vNumber
      , "reason" .= String (pack $ show t)
      ]
    QueryNotSupported -> kindObject "QueryNotSupported" []

instance Show vNumber => ToJSON (HandshakeException vNumber) where
  toJSON = \case
    HandshakeProtocolLimit plf -> kindObject "HandshakeProtocolLimit" [ "handshakeProtocolLimit" .= toJSON plf ]
    HandshakeProtocolError err -> kindObject "HandshakeProtocolError" [ "reason" .= show err ]

instance ToJSON AbstractState where
  toJSON = \case
    UnknownConnectionSt          -> kindObject "UnknownConnectionSt" []
    ReservedOutboundSt           -> kindObject "ReservedOutboundSt" []
    UnnegotiatedSt provenance    -> kindObject "UnnegotiatedSt"
      [ "provenance" .= String (pack . show $ provenance) ]
    InboundIdleSt dataFlow       -> kindObject "InboundIdleSt"
      [ "dataFlow" .= String (pack . show $ dataFlow) ]
    InboundSt dataFlow           -> kindObject "InboundSt"
      [ "dataFlow" .= String (pack . show $ dataFlow) ]
    OutboundUniSt                -> kindObject "OutboundUniSt" []
    OutboundDupSt timeoutExpired -> kindObject "OutboundDupSt"
      [ "timeoutState" .= String (pack . show $ timeoutExpired) ]
    OutboundIdleSt dataFlow      -> kindObject "OutboundIdleSt"
      [ "dataFlow" .= String (pack . show $ dataFlow) ]
    DuplexSt                     -> kindObject "DuplexSt" []
    WaitRemoteIdleSt             -> kindObject "WaitRemoteIdleSt" []
    TerminatingSt                -> kindObject "TerminatingSt" []
    TerminatedSt                 -> kindObject "TerminatedSt" []

instance ToJSON KnownPeerInfo where
  toJSON (KnownPeerInfo
            nKnownPeerFailCount
            nKnownPeerTepid
            nKnownPeerSharing
            nKnownPeerAdvertise
            nKnownSuccessfulConnection
         ) = kindObject "KnownPeerInfo"
           [ "failCount"            .= nKnownPeerFailCount
           , "tepid"                .= nKnownPeerTepid
           , "peerSharing"          .= nKnownPeerSharing
           , "peerAdvertise"        .= nKnownPeerAdvertise
           , "successfulConnection" .= nKnownSuccessfulConnection
           ]

instance ToJSON PeerStatus where
  toJSON = String . pack . show

instance (ToJSON extraFlags, ToJSONKey peerAddr, ToJSON peerAddr, Ord peerAddr)
  => ToJSON (LocalRootPeers extraFlags peerAddr) where
  toJSON lrp = kindObject "LocalRootPeers"
    [ "groups" .= toJSONList (LocalRootPeers.toGroups lrp) ]


peerSelectionTargetsToJSONHelper :: PeerSelectionTargets -> [Pair]
peerSelectionTargetsToJSONHelper
  PeerSelectionTargets {
    targetNumberOfRootPeers,
    targetNumberOfKnownPeers,
    targetNumberOfEstablishedPeers,
    targetNumberOfActivePeers,
    targetNumberOfKnownBigLedgerPeers,
    targetNumberOfEstablishedBigLedgerPeers,
    targetNumberOfActiveBigLedgerPeers
  }
  =
  [ "targetRootPeers"                 .= targetNumberOfRootPeers
  , "targetKnownPeers"                .= targetNumberOfKnownPeers
  , "targetEstablishedPeers"          .= targetNumberOfEstablishedPeers
  , "targetActivePeers"               .= targetNumberOfActivePeers
  , "targetKnownBigLedgerPeers"       .= targetNumberOfKnownBigLedgerPeers
  , "targetEstablishedBigLedgerPeers" .= targetNumberOfEstablishedBigLedgerPeers
  , "targetActiveBigLedgerPeers"      .= targetNumberOfActiveBigLedgerPeers
  ]

instance ToJSON PeerSelectionTargets where
  toJSON targets = kindObject "PeerSelectionTargets"
                 $ peerSelectionTargetsToJSONHelper targets

instance ToJSON RepromoteDelay where
  toJSON = toJSON . repromoteDelay

instance ToJSON addr => ToJSON (PeerSharingResult addr) where
  toJSON = \case
    PeerSharingResult addrs     -> toJSONList addrs
    PeerSharingNotRegisteredYet -> String "PeerSharingNotRegisteredYet"

instance ToJSON extraFlags => ToJSON (LocalRootConfig extraFlags) where
  toJSON LocalRootConfig { peerAdvertise, extraLocalRootFlags, LocalRootPeers.diffusionMode } =
    object
      [ "peerAdvertise" .= peerAdvertise
      , "diffusionMode" .= show diffusionMode
      , "extraFlags"    .= extraLocalRootFlags
      ]

instance ToJSON RemoteAddress where
  toJSON = \case
    SockAddrInet port addr ->
      let ip = fromHostAddress addr in
      object [ "address" .= show ip
             , "port"    .= show port
             ]
    SockAddrInet6 port _ addr _ ->
      let ip = fromHostAddress6 addr in
      object [ "address" .= show ip
             , "port"    .= show port
             ]
    SockAddrUnix path ->
      object [ "socketPath" .= show path ]

instance (ToJSON peer, ToJSON point) => ToJSON (Mux.TraceLabelPeer peer (FetchDecision [point])) where
  toJSON (Mux.TraceLabelPeer peer decision) = object
    [ "peer"     .= peer
    , "decision" .=
         case decision of
           Left decline -> object [ "declined" .= String (pack . show $ decline) ]
           Right points -> toJSON points
    ]

instance ToJSON PeerGSV where
  toJSON PeerGSV { outboundGSV = GSV outboundG _ _, inboundGSV = GSV inboundG _ _ } =
    object ["G" .= (realToFrac (outboundG + inboundG) :: Double)]

instance ToJSON LocalAddress where
  toJSON (LocalAddress path) = String (pack path)

instance ToJSON peerAddr => ToJSON (ConnectionId peerAddr) where
  toJSON ConnectionId { localAddress, remoteAddress } = object
    [ "localAddress"  .= toJSON localAddress
    , "remoteAddress" .= toJSON remoteAddress
    ]

instance ToJSON ConnectionManagerCounters where
  toJSON ConnectionManagerCounters
    { fullDuplexConns
    , duplexConns
    , unidirectionalConns
    , inboundConns
    , outboundConns
    } = object
      [ "fullDuplex"     .= toJSON fullDuplexConns
      , "duplex"         .= toJSON duplexConns
      , "unidirectional" .= toJSON unidirectionalConns
      , "inbound"        .= inboundConns
      , "outbound"       .= outboundConns
      ]

instance ToJSONKey RelayAccessPoint where

instance ToJSONKey RemoteAddress where

instance ToJSONKey LocalAddress where

instance (ToJSON addr, ToJSONKey addr) => ToJSONKey (ConnectionId addr) where

instance ToJSON PortNumber where
  toJSON = toJSON . fromIntegral @_ @Int

instance (ToJSON peer, ToJSON a) => ToJSON (Mux.WithBearer peer a) where
  toJSON (Mux.WithBearer peer ev) =
    object [ "bearer" .= peer
           , "event"  .= ev
           ]

instance ToJSON Mux.MiniProtocolDir where
  toJSON miniProtocolDir = String (pack . show $ miniProtocolDir)

instance ToJSON (Mux.MiniProtocolInfo mode) where
  toJSON Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum miniProtocolNum,
            Mux.miniProtocolDir,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits { Mux.maximumIngressQueue },
            Mux.miniProtocolCapability
         }
    = object
      [ "miniProtocolNum"        .= miniProtocolNum
      , "miniProtocolDir"        .= show miniProtocolDir
      , "maximumIngressQueue"    .= maximumIngressQueue
      , "miniProtocolCapability" .= miniProtocolCapability
      ]

instance ToJSON Mux.Trace where
  toJSON = \case
    Mux.TraceState state ->
      object [ "type"  .= String "State"
             , "state" .= String (pack . show $ state)
             ]
    Mux.TraceCleanExit miniProtocolNum miniProtocolDir ->
      object [ "type" .= String "CleanExit"
             , "miniProtocolNum" .= miniProtocolNum
             , "miniProtocolDir" .= miniProtocolDir
             ]
    Mux.TraceExceptionExit miniProtocolNum miniProtocolDir err ->
      object [ "type" .= String "ExceptionExit"
             , "miniProtocolNum" .= miniProtocolNum
             , "miniProtocolDir" .= miniProtocolDir
             , "error"           .= String (pack . displayException $ err)
             ]
    Mux.TraceStartEagerly miniProtocolNum miniProtocolDir ->
      object [ "type" .= String "StartEagerly"
             , "miniProtocolNum" .= miniProtocolNum
             , "miniProtocolDir" .= miniProtocolDir
             ]
    Mux.TraceStartOnDemand miniProtocolNum miniProtocolDir ->
      object [ "type" .= String "StartOnDemand"
             , "miniProtocolNum" .= miniProtocolNum
             , "miniProtocolDir" .= miniProtocolDir
             ]
    Mux.TraceStartOnDemandAny miniProtocolNum miniProtocolDir ->
      object [ "type" .= String "StartOnDemandAny"
             , "miniProtocolNum" .= miniProtocolNum
             , "miniProtocolDir" .= miniProtocolDir
             ]
    Mux.TraceStartedOnDemand miniProtocolNum miniProtocolDir ->
      object [ "type" .= String "StartedOnDemand"
             , "miniProtocolNum" .= miniProtocolNum
             , "miniProtocolDir" .= miniProtocolDir
             ]
    Mux.TraceTerminating miniProtocolNum miniProtocolDir ->
      object [ "type" .= String "Terminating"
             , "miniProtocolNum" .= miniProtocolNum
             , "miniProtocolDir" .= miniProtocolDir
             ]
    Mux.TraceNewMux infos ->
      object [ "type" .= String "NewMux"
             , "miniProtocolInfos" .= infos
             ]
    Mux.TraceStarting ->
      object [ "type" .= String "Starting"
             ]
    Mux.TraceStopping ->
      object [ "type" .= String "Stopping"
             ]
    Mux.TraceStopped ->
      object [ "type" .= String "Stopped"
             ]


instance ToJSON Mux.ChannelTrace where
  toJSON = \case
    Mux.TraceChannelRecvStart miniProtocolNum ->
      object [ "type" .= String "RecvStart"
             , "miniProtocolNum" .= miniProtocolNum
             ]
    Mux.TraceChannelRecvEnd miniProtocolNum len ->
      object [ "type" .= String "RecvEnd"
             , "miniProtocolNum" .= miniProtocolNum
             , "length" .= len
             ]
    Mux.TraceChannelSendStart miniProtocolNum len ->
      object [ "type" .= String "SendStart"
             , "miniProtocolNum" .= miniProtocolNum
             , "length" .= len
             ]
    Mux.TraceChannelSendEnd miniProtocolNum ->
      object [ "type" .= String "SendEnd"
             , "miniProtocolNum" .= miniProtocolNum
             ]


-- TODO
instance ToJSON Mux.BearerTrace where
  toJSON ev = String (pack . show $ ev)

instance ToJSON (AnyMessage ps) => ToJSON (TraceSendRecv ps) where
  toJSON = \case
    TraceSendMsg msg ->
      object [ "type"    .= String "SendMsg"
             , "message" .= msg
             ]
    TraceRecvMsg msg ->
      object [ "type"    .= String "RecvMsg"
             , "message" .= msg
             ]

instance (ToJSON version, ToJSONKey version, ToJSON params) => ToJSON (AnyMessage (Handshake version params)) where
  toJSON = \case
    AnyMessage (MsgProposeVersions versionDict) ->
      object [ "type" .= String "MsgProposeVersions"
             , "versions" .= versionDict
             ]
    AnyMessage (MsgReplyVersions versionDict) ->
      object [ "type" .= String "MsgReplyVersions"
             , "versions" .= versionDict
             ]

    AnyMessage (MsgQueryReply versionDict) ->
      object [ "type" .= String "MsgQueryVersions"
             , "versions" .= versionDict
             ]
    AnyMessage (MsgAcceptVersion version params) ->
      object [ "type" .= String "MsgAcceptVersion"
             , "version" .= version
             , "params" .= params
             ]
    AnyMessage (MsgRefuse reason) ->
      object [ "type" .= String "MsgRefuse"
             , "reason" .= reason
             ]

instance (ToJSON localAddress, ToJSON remoteAddress) => ToJSON (DiffusionTracer localAddress remoteAddress) where
  toJSON (RunServer sockAddr) = object
    [ "kind" .= String "RunServer"
    , "addresses" .= sockAddr
    ]

  toJSON (RunLocalServer localAddress) = object
    [ "kind" .= String "RunLocalServer"
    , "addresses" .= localAddress
    ]
  toJSON (UsingSystemdSocket localAddress) = object
    [ "kind" .= String "UsingSystemdSocket"
    , "address" .= localAddress
    ]

  toJSON (CreateSystemdSocketForSnocketPath localAddress) = object
    [ "kind" .= String "CreateSystemdSocketForSnocketaddress"
    , "address" .= localAddress
    ]
  toJSON (CreatedLocalSocket localAddress) = object
    [ "kind" .= String "CreatedLocalSocket"
    , "address" .= localAddress
    ]
  toJSON (ConfiguringLocalSocket localAddress socket) = object
    [ "kind" .= String "ConfiguringLocalSocket"
    , "address" .= localAddress
    , "socket" .= String (pack (show socket))
    ]
  toJSON (ListeningLocalSocket localAddress socket) = object
    [ "kind" .= String "ListeningLocalSocket"
    , "address" .= localAddress
    , "socket" .= String (pack (show socket))
    ]
  toJSON (LocalSocketUp localAddress fd) = object
    [ "kind" .= String "LocalSocketUp"
    , "address" .= localAddress
    , "socket" .= String (pack (show fd))
    ]
  toJSON (CreatingServerSocket sockAddr) = object
    [ "kind" .= String "CreatingServerSocket"
    , "address" .= sockAddr
    ]
  toJSON (ListeningServerSocket sockAddr) = object
    [ "kind" .= String "ListeningServerSocket"
    , "address" .= sockAddr
    ]
  toJSON (ServerSocketUp sockAddr) = object
    [ "kind" .= String "ServerSocketUp"
    , "address" .= sockAddr
    ]
  toJSON (ConfiguringServerSocket sockAddr) = object
    [ "kind" .= String "ConfiguringServerSocket"
    , "address" .= sockAddr
    ]
  toJSON (UnsupportedLocalSystemdSocket addr) = object
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "address" .= addr
    ]
  toJSON UnsupportedReadySocketCase = object
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  toJSON (DiffusionErrored exception) = object
    [ "kind" .= String "DiffusionErrored"
    , "error" .= String (pack (show exception))
    ]
  toJSON (SystemdSocketConfiguration config) = object
    [ "kind" .= String "SystemdSocketConfiguration"
    , "message" .= String (pack (show config))
    ]


instance (ToJSON extraFlags, ToJSON peerAddr, ToJSONKey peerAddr) => ToJSON (TraceLocalRootPeers extraFlags peerAddr) where
  toJSON (TraceLocalRootDomains groups) =
    object [ "kind" .= String "LocalRootDomains"
           , "localRootDomains" .= groups
           ]
  toJSON (TraceLocalRootWaiting d dt) =
    object [ "kind" .= String "LocalRootWaiting"
           , "domainAddress" .= d
           , "diffTime" .= show dt
           ]
  toJSON (TraceLocalRootGroups groups) =
    object [ "kind" .= String "LocalRootGroups"
           , "localRootGroups" .= groups
           ]
  toJSON (TraceLocalRootFailure d dexception) =
    object [ "kind" .= String "LocalRootFailure"
           , "domainAddress" .= d
           , "reason" .= show dexception
           ]
  toJSON (TraceLocalRootError d dexception) =
    object [ "kind" .= String "LocalRootError"
           , "domainAddress" .= String (Text.decodeUtf8With Text.ignore d)
           , "reason" .= show dexception
           ]
  toJSON (TraceLocalRootReconfigured _ _) =
    object [ "kind" .= String "LocalRootReconfigured"
           ]
  toJSON (TraceLocalRootDNSMap dnsMap) =
    object
      [ "kind" .= String "TraceLocalRootDNSMap"
      , "dnsMap" .= dnsMap
      ]

instance ToJSON TracePublicRootPeers where
  toJSON (TracePublicRootRelayAccessPoint relays) =
    object [ "kind" .= String "PublicRootRelayAddresses"
           , "relayAddresses" .= relays
           ]
  toJSON (TracePublicRootDomains domains) =
    object [ "kind" .= String "PublicRootDomains"
           , "domainAddresses" .= domains
           ]

instance ToJSON TraceLedgerPeers where
  toJSON (PickedBigLedgerPeer addr _ackStake stake) =
    object
      [ "kind" .= String "PickedBigLedgerPeer"
      , "address" .= addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  toJSON (PickedBigLedgerPeers (NumberOfPeers n) addrs) =
    object
      [ "kind" .= String "PickedBigLedgerPeers"
      , "desiredCount" .= n
      , "count" .= length addrs
      , "addresses" .= addrs
      ]
  toJSON (PickedLedgerPeer addr _ackStake stake) =
    object
      [ "kind" .= String "PickedLedgerPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      ]
  toJSON (PickedLedgerPeers (NumberOfPeers n) addrs) =
    object
      [ "kind" .= String "PickedLedgerPeers"
      , "desiredCount" .= n
      , "count" .= length addrs
      , "addresses" .= addrs
      ]
  toJSON (FetchingNewLedgerState cnt bigCnt) =
    object
      [ "kind" .= String "FetchingNewLedgerState"
      , "numberOfLedgerPeers" .= cnt
      , "numberOfBigLedgerPeers" .= bigCnt
      ]
  toJSON DisabledLedgerPeers =
    object
      [ "kind" .= String "DisabledLedgerPeers"
      ]
  toJSON (TraceUseLedgerPeers ulp) =
    object
      [ "kind" .= String "UseLedgerPeers"
      , "useLedgerPeers" .= ulp
      ]
  toJSON WaitingOnRequest =
    object
      [ "kind" .= String "WaitingOnRequest"
      ]
  toJSON (RequestForPeers (NumberOfPeers np)) =
    object
      [ "kind" .= String "RequestForPeers"
      , "numberOfPeers" .= np
      ]
  toJSON (ReusingLedgerState cnt age) =
    object
      [ "kind" .= String "ReusingLedgerState"
      , "numberOfPools" .= cnt
      , "ledgerStateAge" .= age
      ]
  toJSON FallingBackToPublicRootPeers =
    object
      [ "kind" .= String "FallingBackToBootstrapPeers"
      ]
  toJSON (NotEnoughLedgerPeers (NumberOfPeers target) numOfLedgerPeers) =
    object
      [ "kind" .= String "NotEnoughLedgerPeers"
      , "target" .= target
      , "numOfLedgerPeers" .= numOfLedgerPeers
      ]
  toJSON (NotEnoughBigLedgerPeers (NumberOfPeers target) numOfBigLedgerPeers) =
    object
      [ "kind" .= String "NotEnoughBigLedgerPeers"
      , "target" .= target
      , "numOfBigLedgerPeers" .= numOfBigLedgerPeers
      ]
  toJSON (TraceLedgerPeersDomains daps) =
    object
      [ "kind" .= String "TraceLedgerPeersDomains"
      , "domainAccessPoints" .= daps
      ]
  toJSON UsingBigLedgerPeerSnapshot =
    object
      [ "kind" .= String "UsingBigLedgerPeerSnapshot"
      ]

instance ToJSON Time where
  toJSON = String . pack . show

instance ( ToJSON extraDebugState
         , ToJSON extraFlags
         , ToJSON extraPeers
         , ToJSON extraTracer
         , ToJSON peerAddr
         , ToJSONKey peerAddr
         , Ord peerAddr
         , ToJSON (PublicRootPeers.PublicRootPeers extraPeers peerAddr)
         )
       => ToJSON (TracePeerSelection extraDebugState extraFlags extraPeers extraTracer peerAddr) where
  toJSON (TraceLocalRootPeersChanged lrp lrp') =
    object [ "kind" .= String "LocalRootPeersChanged"
           , "previous" .= lrp
           , "current" .= lrp'
           ]
  toJSON (TraceTargetsChanged pst) =
      kindObject "TargetsChanged"
    $ peerSelectionTargetsToJSONHelper pst
  toJSON (TracePublicRootsRequest tRootPeers nRootPeers) =
    object [ "kind" .= String "PublicRootsRequest"
           , "targetNumberOfRootPeers" .= tRootPeers
           , "numberOfRootPeers" .= nRootPeers
           ]
  toJSON (TracePublicRootsResults res group dt) =
    object [ "kind" .= String "PublicRootsResults"
           , "result" .= res
           , "group" .= group
           , "diffTime" .= dt
           ]
  toJSON (TracePublicRootsFailure err group dt) =
    object [ "kind" .= String "PublicRootsFailure"
           , "reason" .= show err
           , "group" .= group
           , "diffTime" .= dt
           ]
  toJSON (TraceBigLedgerPeersRequest tBigLedgerPeers nBigLedgerPeers) =
    object [ "kind" .= String "BigLedgerPeersRequest"
           , "targetNumberOfBigLedgerPeers" .= tBigLedgerPeers
           , "numberOfBigLedgerPeers" .= nBigLedgerPeers
           ]
  toJSON (TraceBigLedgerPeersResults res group dt) =
    object [ "kind" .= String "BigLedgerPeersResults"
           , "result" .= toList res
           , "group" .= group
           , "diffTime" .= dt
           ]
  toJSON (TraceBigLedgerPeersFailure err group dt) =
    object [ "kind" .= String "BigLedgerPeersFailure"
           , "reason" .= show err
           , "group" .= group
           , "diffTime" .= dt
           ]
  toJSON (TraceForgetBigLedgerPeers targetKnown actualKnown sp) =
    object [ "kind" .= String "ForgetBigLedgerPeers"
           , "targetKnown" .= targetKnown
           , "actualKnown" .= actualKnown
           , "selectedPeers" .= toList sp
           ]
  toJSON (TracePeerShareRequests targetKnown actualKnown (PeerSharingAmount numRequested) aps sps) =
    object [ "kind" .= String "PeerShareRequests"
           , "targetKnown" .= targetKnown
           , "actualKnown" .= actualKnown
           , "numRequested" .= numRequested
           , "availablePeers" .= toList aps
           , "selectedPeers" .= toList sps
           ]
  toJSON (TracePeerShareResults res) =
    object [ "kind" .= String "PeerShareResults"
           , "result" .= map ( first show <$> ) res
           ]
  toJSON (TracePeerShareResultsFiltered res) =
    object [ "kind" .= String "PeerShareResultsFiltered"
           , "result" .= res
           ]
  toJSON (TraceForgetColdPeers targetKnown actualKnown sp) =
    object [ "kind" .= String "ForgetColdPeers"
           , "targetKnown" .= targetKnown
           , "actualKnown" .= actualKnown
           , "selectedPeers" .= toList sp
           ]
  toJSON (TracePromoteColdPeers targetKnown actualKnown sp) =
    object [ "kind" .= String "PromoteColdPeers"
           , "targetEstablished" .= targetKnown
           , "actualEstablished" .= actualKnown
           , "selectedPeers" .= toList sp
           ]
  toJSON (TracePromoteColdLocalPeers tLocalEst sp) =
    object [ "kind" .= String "PromoteColdLocalPeers"
           , "targetLocalEstablished" .= tLocalEst
           , "selectedPeers" .= toList sp
           ]
  toJSON (TracePromoteColdFailed tEst aEst p d err forgotten) =
    object [ "kind" .= String "PromoteColdFailed"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "peer" .= p
           , "delay" .= d
           , "reason" .= show err
           , "reason" .= show forgotten
           ]
  toJSON (TracePromoteColdDone tEst aEst p) =
    object [ "kind" .= String "PromoteColdDone"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "peer" .= p
           ]
  toJSON (TracePromoteColdBigLedgerPeers targetKnown actualKnown sp) =
    object [ "kind" .= String "PromoteColdBigLedgerPeers"
           , "targetEstablished" .= targetKnown
           , "actualEstablished" .= actualKnown
           , "selectedPeers" .= toList sp
           ]
  toJSON (TracePromoteColdBigLedgerPeerFailed tEst aEst p d err forgotten) =
    object [ "kind" .= String "PromoteColdBigLedgerPeerFailed"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "peer" .= p
           , "delay" .= d
           , "reason" .= show err
           , "forgotten" .= show forgotten
           ]
  toJSON (TracePromoteColdBigLedgerPeerDone tEst aEst p) =
    object [ "kind" .= String "PromoteColdBigLedgerPeerDone"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "peer" .= p
           ]
  toJSON (TracePromoteWarmPeers tActive aActive sp) =
    object [ "kind" .= String "PromoteWarmPeers"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "selectedPeers" .= toList sp
           ]
  toJSON (TracePromoteWarmLocalPeers taa sp) =
    object [ "kind" .= String "PromoteWarmLocalPeers"
           , "targetActualActive" .= taa
           , "selectedPeers" .= toList sp
           ]
  toJSON (TracePromoteWarmFailed tActive aActive p err) =
    object [ "kind" .= String "PromoteWarmFailed"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           , "reason" .= show err
           ]
  toJSON (TracePromoteWarmDone tActive aActive p) =
    object [ "kind" .= String "PromoteWarmDone"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           ]
  toJSON (TracePromoteWarmAborted tActive aActive p) =
    object [ "kind" .= String "PromoteWarmAborted"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           ]
  toJSON (TracePromoteWarmBigLedgerPeers tActive aActive sp) =
    object [ "kind" .= String "PromoteWarmBigLedgerPeers"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "selectedPeers" .= toList sp
           ]
  toJSON (TracePromoteWarmBigLedgerPeerFailed tActive aActive p err) =
    object [ "kind" .= String "PromoteWarmBigLedgerPeerFailed"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           , "reason" .= show err
           ]
  toJSON (TracePromoteWarmBigLedgerPeerDone tActive aActive p) =
    object [ "kind" .= String "PromoteWarmBigLedgerPeerDone"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           ]
  toJSON (TracePromoteWarmBigLedgerPeerAborted tActive aActive p) =
    object [ "kind" .= String "PromoteWarmBigLedgerPeerAborted"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           ]
  toJSON (TraceDemoteWarmPeers tEst aEst sp) =
    object [ "kind" .= String "DemoteWarmPeers"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "selectedPeers" .= toList sp
           ]
  toJSON (TraceDemoteWarmFailed tEst aEst p err) =
    object [ "kind" .= String "DemoteWarmFailed"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "peer" .= p
           , "reason" .= show err
           ]
  toJSON (TraceDemoteWarmDone tEst aEst p) =
    object [ "kind" .= String "DemoteWarmDone"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "peer" .= p
           ]
  toJSON (TraceDemoteWarmBigLedgerPeers tEst aEst sp) =
    object [ "kind" .= String "DemoteWarmBigLedgerPeers"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "selectedPeers" .= toList sp
           ]
  toJSON (TraceDemoteWarmBigLedgerPeerFailed tEst aEst p err) =
    object [ "kind" .= String "DemoteWarmBigLedgerPeerFailed"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "peer" .= p
           , "reason" .= show err
           ]
  toJSON (TraceDemoteWarmBigLedgerPeerDone tEst aEst p) =
    object [ "kind" .= String "DemoteWarmBigLedgerPeerDone"
           , "targetEstablished" .= tEst
           , "actualEstablished" .= aEst
           , "peer" .= p
           ]
  toJSON (TraceDemoteHotPeers tActive aActive sp) =
    object [ "kind" .= String "DemoteHotPeers"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "selectedPeers" .= toList sp
           ]
  toJSON (TraceDemoteLocalHotPeers taa sp) =
    object [ "kind" .= String "DemoteLocalHotPeers"
           , "targetActualActive" .= taa
           , "selectedPeers" .= toList sp
           ]
  toJSON (TraceDemoteHotFailed tActive aActive p err) =
    object [ "kind" .= String "DemoteHotFailed"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           , "reason" .= show err
           ]
  toJSON (TraceDemoteHotDone tActive aActive p) =
    object [ "kind" .= String "DemoteHotDone"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           ]
  toJSON (TraceDemoteHotBigLedgerPeers tActive aActive sp) =
    object [ "kind" .= String "DemoteHotBigLedgerPeers"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "selectedPeers" .= toList sp
           ]
  toJSON (TraceDemoteHotBigLedgerPeerFailed tActive aActive p err) =
    object [ "kind" .= String "DemoteHotBigLedgerPeerFailed"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           , "reason" .= show err
           ]
  toJSON (TraceDemoteHotBigLedgerPeerDone tActive aActive p) =
    object [ "kind" .= String "DemoteHotBigLedgerPeerDone"
           , "targetActive" .= tActive
           , "actualActive" .= aActive
           , "peer" .= p
           ]
  toJSON (TraceDemoteAsynchronous msp) =
    object [ "kind" .= String "DemoteAsynchronous"
           , "state" .= msp
           ]
  toJSON (TraceDemoteLocalAsynchronous msp) =
    object [ "kind" .= String "DemoteLocalAsynchronous"
           , "state" .= msp
           ]
  toJSON (TraceDemoteBigLedgerPeersAsynchronous msp) =
    object [ "kind" .= String "DemoteBigLedgerPeersAsynchronous"
           , "state" .= msp
           ]
  toJSON TraceGovernorWakeup =
    object [ "kind" .= String "GovernorWakeup"
           ]
  toJSON (TraceChurnWait dt) =
    object [ "kind" .= String "ChurnWait"
           , "diffTime" .= dt
           ]
  toJSON (TracePickInboundPeers targetNumberOfKnownPeers numberOfKnownPeers selected available) =
    object [ "kind" .= String "PickInboundPeers"
           , "targetKnown" .= targetNumberOfKnownPeers
           , "actualKnown" .= numberOfKnownPeers
           , "selected" .= selected
           , "available" .= available
           ]
  toJSON (ExtraTrace extraTrace) =
    toJSON extraTrace
  toJSON TraceOnlyBootstrapPeers =
    object [ "kind" .= String "OnlyBootstrapPeers" ]
  toJSON TraceBootstrapPeersFlagChangedWhilstInSensitiveState =
    object [ "kind" .= String "BootstrapPeersFlagChangedWhilstInSensitiveState"
           ]
  toJSON (TraceVerifyPeerSnapshot result) =
    object [ "kind" .= String "VerifyPeerSnapshot"
           , "result" .= result ]
  toJSON (TraceOutboundGovernorCriticalFailure err) =
    object [ "kind" .= String "OutboundGovernorCriticalFailure"
           , "reason" .= show err
           ]
  toJSON (TraceChurnAction duration action counter) =
    object [ "kind" .= String "ChurnAction"
           , "action" .= show action
           , "counter" .= counter
           , "duration" .= duration
           ]
  toJSON (TraceChurnTimeout duration action counter) =
    object [ "kind" .= String "ChurnTimeout"
           , "action" .= show action
           , "counter" .= counter
           , "duration" .= duration
           ]
  toJSON (TraceDebugState mtime ds) =
   object [ "kind" .= String "DebugState"
          , "monotonicTime" .= mtime
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
          , "ledgerStateJudgement" .= dpssExtraState ds
          , "associationMode" .= dpssAssociationMode ds
          ]

peerSelectionTargetsToObject :: PeerSelectionTargets -> Value
peerSelectionTargetsToObject
  PeerSelectionTargets { targetNumberOfRootPeers,
                         targetNumberOfKnownPeers,
                         targetNumberOfEstablishedPeers,
                         targetNumberOfActivePeers,
                         targetNumberOfKnownBigLedgerPeers,
                         targetNumberOfEstablishedBigLedgerPeers,
                         targetNumberOfActiveBigLedgerPeers
                       } =
    object
      [ "roots" .= targetNumberOfRootPeers
       , "knownPeers" .= targetNumberOfKnownPeers
       , "established" .= targetNumberOfEstablishedPeers
       , "active" .= targetNumberOfActivePeers
       , "knownBigLedgerPeers" .= targetNumberOfKnownBigLedgerPeers
       , "establishedBigLedgerPeers" .= targetNumberOfEstablishedBigLedgerPeers
       , "activeBigLedgerPeers" .= targetNumberOfActiveBigLedgerPeers
       ]

instance ToJSON ChurnCounters where
  toJSON (ChurnCounter action cnt) =
    object [ "action"  .= show action
           , "counter" .= cnt
           ]

instance (ToJSON peerAddr, Show peerAddr, Show versionNumber)
      => ToJSON (PeerSelectionActionsTrace peerAddr versionNumber) where
  toJSON (PeerStatusChanged ps) =
    object [ "kind" .= String "PeerStatusChanged"
           , "peerStatusChangeType" .= show ps
           ]
  toJSON (PeerStatusChangeFailure ps f) =
    object [ "kind" .= String "PeerStatusChangeFailure"
           , "peerStatusChangeType" .= String (pack . show $ ps)
           , "reason" .= show f
           ]
  toJSON (PeerMonitoringError connId s) =
    object [ "kind" .= String "PeerMonitoringError"
            , "connectionId" .= connId
            , "reason" .= show s
            ]
  toJSON (PeerMonitoringResult connId wf) =
    object [ "kind" .= String "PeerMonitoringResult"
            , "connectionId" .= toJSON connId
            , "withProtocolTemp" .= show wf
            ]
  toJSON (AcquireConnectionError exception) =
    object [ "kind" .= String "AcquireConnectionError"
           , "error" .= displayException exception
           ]
  toJSON (PeerHotDuration connId duration) =
    object [ "kind" .= String "PeerHotDuration"
           , "connId" .= connId
           , "duration" .= duration
           ]

instance (Show versionNumber, ToJSON versionNumber, ToJSON agreedOptions)
  => ToJSON (ConnectionHandlerTrace versionNumber agreedOptions) where
  toJSON (TrHandshakeSuccess versionNumber agreedOptions) =
    object
      [ "kind" .= String "HandshakeSuccess"
      , "versionNumber" .= versionNumber
      , "agreedOptions" .= agreedOptions
      ]
  toJSON (TrHandshakeQuery vMap) =
    object
      [ "kind" .= String "HandshakeQuery"
      , "versions" .= toJSON ((\(k,v) -> object [
          "versionNumber" .= k
        , "options" .= v
        ]) <$> Map.toList vMap)
      ]
  toJSON (TrHandshakeClientError err) =
    object
      [ "kind" .= String "HandshakeClientError"
      , "reason" .= err
      ]
  toJSON (TrHandshakeServerError err) =
    object
      [ "kind" .= String "HandshakeServerError"
      , "reason" .= err
      ]
  toJSON (TrConnectionHandlerError e err cerr) =
    object
      [ "kind" .= String "Error"
      , "context" .= show e
      , "reason"  .= show err
      , "command" .= show cerr
      ]

instance (Show addr, Show versionNumber, Show agreedOptions,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions)
      => ToJSON (ConnMgr.Trace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  toJSON = \case
    TrInboundConnectionNotFound peerAddr ->
      object
        [ "kind" .= String "InboundConnectionNotFound"
        , "remoteAddress" .= peerAddr
        ]
    TrIncludeConnection prov peerAddr ->
      object
        [ "kind" .= String "IncludeConnection"
        , "remoteAddress" .= peerAddr
        , "provenance" .= String (pack . show $ prov)
        ]
    TrReleaseConnection prov connId ->
      object
        [ "kind" .= String "UnregisterConnection"
        , "remoteAddress" .= toJSON connId
        , "provenance" .= String (pack . show $ prov)
        ]
    TrConnect (Just localAddress) remoteAddress diffusionMode ->
      object
        [ "kind" .= String "Connect"
        , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
        , "diffusionMode" .= toJSON diffusionMode
        ]
    TrConnect Nothing remoteAddress diffusionMode ->
      object
        [ "kind" .= String "Connect"
        , "remoteAddress" .= remoteAddress
        , "diffusionMode" .= toJSON diffusionMode
        ]
    TrConnectError (Just localAddress) remoteAddress err ->
      object
        [ "kind" .= String "ConnectError"
        , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
        , "reason" .= String (pack . show $ err)
        ]
    TrConnectError Nothing remoteAddress err ->
      object
        [ "kind" .= String "ConnectError"
        , "remoteAddress" .= remoteAddress
        , "reason" .= String (pack . show $ err)
        ]
    TrTerminatingConnection prov connId ->
      object
        [ "kind" .= String "TerminatingConnection"
        , "provenance" .= String (pack . show $ prov)
        , "connectionId" .= toJSON connId
        ]
    TrTerminatedConnection prov remoteAddress ->
      object
        [ "kind" .= String "TerminatedConnection"
        , "provenance" .= String (pack . show $ prov)
        , "remoteAddress" .= remoteAddress
        ]
    TrConnectionHandler connId a ->
      object
        [ "kind" .= String "ConnectionHandler"
        , "connectionId" .= connId
        , "connectionHandler" .= a
        ]
    TrShutdown ->
      object
        [ "kind" .= String "Shutdown"
        ]
    TrConnectionExists prov remoteAddress inState ->
      object
        [ "kind" .= String "ConnectionExists"
        , "provenance" .= String (pack . show $ prov)
        , "remoteAddress" .= remoteAddress
        , "state" .= toJSON inState
        ]
    TrForbiddenConnection connId ->
      object
        [ "kind" .= String "ForbiddenConnection"
        , "connectionId" .= toJSON connId
        ]
    TrConnectionFailure connId ->
      object
        [ "kind" .= String "ConnectionFailure"
        , "connectionId" .= toJSON connId
        ]
    TrConnectionNotFound prov remoteAddress ->
      object
        [ "kind" .= String "ConnectionNotFound"
        , "remoteAddress" .= remoteAddress
        , "provenance" .= String (pack . show $ prov)
        ]
    TrForbiddenOperation remoteAddress connState ->
      object
        [ "kind" .= String "ForbiddenOperation"
        , "remoteAddress" .= remoteAddress
        , "connectionState" .= connState
        ]
    TrPruneConnections pruningSet numberPruned chosenPeers ->
      object
        [ "kind" .= String "PruneConnections"
        , "prunedPeers" .= toJSON pruningSet
        , "numberPrunedPeers" .= toJSON numberPruned
        , "choiceSet" .= toJSON (toJSON `Set.map` chosenPeers)
        ]
    TrConnectionCleanup connId ->
      object
        [ "kind" .= String "ConnectionCleanup"
        , "connectionId" .= toJSON connId
        ]
    TrConnectionTimeWait connId ->
      object
        [ "kind" .= String "ConnectionTimeWait"
        , "connectionId" .= toJSON connId
        ]
    TrConnectionTimeWaitDone connId ->
      object
        [ "kind" .= String "ConnectionTimeWaitDone"
        , "connectionId" .= toJSON connId
        ]
    TrConnectionManagerCounters cmCounters ->
      object
        [ "kind"  .= String "ConnectionManagerCounters"
        , "state" .= toJSON cmCounters
        ]
    TrState cmState ->
      object
        [ "kind"  .= String "ConnectionManagerState"
        , "state" .= listValue (\(remoteAddr, inner) ->
                                       object
                                         [ "connections" .=
                                           listValue (\(localAddr, connState) ->
                                              object
                                                [ "localAddress" .= localAddr
                                                , "state" .= toJSON connState
                                                ]
                                           )
                                           (Map.toList inner)
                                         , "remoteAddress" .= toJSON remoteAddr
                                         ]
                               )
                               (Map.toList (getConnMap cmState))
        ]
    ConnMgr.TrUnexpectedlyFalseAssertion info ->
      object
        [ "kind" .= String "UnexpectedlyFalseAssertion"
        , "info" .= String (pack . show $ info)
        ]

instance (Show addr, ToJSON addr)
      => ToJSON (ConnMgr.AbstractTransitionTrace addr) where
    toJSON (ConnMgr.TransitionTrace addr tr) =
      object
        [ "kind"    .= String "ConnectionManagerTransition"
        , "address" .= addr
        , "from"    .= ConnMgr.fromState tr
        , "to"      .= ConnMgr.toState tr
        ]


instance ToJSON AcceptConnectionsPolicyTrace where
  toJSON (ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
    object [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
           , "delay" .= show delay
           , "numberOfConnection" .= show numOfConnections
           ]
  toJSON (ServerTraceAcceptConnectionHardLimit softLimit) =
    object [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
           , "softLimit" .= show softLimit
           ]
  toJSON (ServerTraceAcceptConnectionResume numOfConnections) =
    object [ "kind" .= String "ServerTraceAcceptConnectionResume"
           , "numberOfConnection" .= show numOfConnections
           ]

instance (Show addr, ToJSON addr)
      => ToJSON (Server.Trace addr) where
  toJSON (Server.TrAcceptConnection connId)     =
    object [ "kind" .= String "AcceptConnection"
           , "connectionId" .= connId
           ]
  toJSON (Server.TrAcceptError exception)         =
    object [ "kind" .= String "AcceptError"
           , "error" .= show exception
           ]
  toJSON (Server.TrAcceptPolicyTrace policyTrace) =
    object [ "kind" .= String "AcceptPolicyServer.Trace"
           , "policy" .= policyTrace
           ]
  toJSON (Server.TrServerStarted peerAddrs)       =
    object [ "kind" .= String "AcceptPolicyServer.Trace"
           , "addresses" .= peerAddrs
           ]
  toJSON Server.TrServerStopped                   =
    object [ "kind" .= String "ServerStopped"
           ]
  toJSON (Server.TrServerError exception)         =
    object [ "kind" .= String "ServerError"
           , "reason" .= show exception
           ]

instance (ToJSON addr, ToJSONKey addr, Show addr)
      => ToJSON (InboundGovernor.Trace addr) where
  toJSON (InboundGovernor.TrNewConnection p connId)            =
    object [ "kind" .= String "NewConnection"
           , "provenance" .= show p
           , "connectionId" .= toJSON connId
           ]
  toJSON (InboundGovernor.TrResponderRestarted connId m)       =
    object [ "kind" .= String "ResponderStarted"
           , "connectionId" .= toJSON connId
           , "miniProtocolNum" .= toJSON m
           ]
  toJSON (InboundGovernor.TrResponderStartFailure connId m s)  =
    object [ "kind" .= String "ResponderStartFailure"
           , "connectionId" .= toJSON connId
           , "miniProtocolNum" .= toJSON m
           , "reason" .= show s
           ]
  toJSON (InboundGovernor.TrResponderErrored connId m s)       =
    object [ "kind" .= String "ResponderErrored"
           , "connectionId" .= toJSON connId
           , "miniProtocolNum" .= toJSON m
           , "reason" .= show s
           ]
  toJSON (InboundGovernor.TrResponderStarted connId m)         =
    object [ "kind" .= String "ResponderStarted"
           , "connectionId" .= toJSON connId
           , "miniProtocolNum" .= toJSON m
           ]
  toJSON (InboundGovernor.TrResponderTerminated connId m)      =
    object [ "kind" .= String "ResponderTerminated"
           , "connectionId" .= toJSON connId
           , "miniProtocolNum" .= toJSON m
           ]
  toJSON (InboundGovernor.TrPromotedToWarmRemote connId opRes) =
    object [ "kind" .= String "PromotedToWarmRemote"
           , "connectionId" .= toJSON connId
           , "result" .= toJSON opRes
           ]
  toJSON (InboundGovernor.TrPromotedToHotRemote connId)        =
    object [ "kind" .= String "PromotedToHotRemote"
           , "connectionId" .= toJSON connId
           ]
  toJSON (InboundGovernor.TrDemotedToColdRemote connId od)     =
    object [ "kind" .= String "DemotedToColdRemote"
           , "connectionId" .= toJSON connId
           , "result" .= show od
           ]
  toJSON (InboundGovernor.TrDemotedToWarmRemote connId)     =
    object [ "kind" .= String "DemotedToWarmRemote"
           , "connectionId" .= toJSON connId
           ]
  toJSON (InboundGovernor.TrWaitIdleRemote connId opRes) =
    object [ "kind" .= String "WaitIdleRemote"
           , "connectionId" .= toJSON connId
           , "result" .= toJSON opRes
           ]
  toJSON (InboundGovernor.TrMuxCleanExit connId)               =
    object [ "kind" .= String "MuxCleanExit"
           , "connectionId" .= toJSON connId
           ]
  toJSON (InboundGovernor.TrMuxErrored connId s)               =
    object [ "kind" .= String "MuxErrored"
           , "connectionId" .= toJSON connId
           , "reason" .= show s
           ]
  toJSON (InboundGovernor.TrInboundGovernorCounters counters) =
    object [ "kind" .= String "InboundGovernorCounters"
           , "idlePeers" .= InboundGovernor.idlePeersRemote counters
           , "coldPeers" .= InboundGovernor.coldPeersRemote counters
           , "warmPeers" .= InboundGovernor.warmPeersRemote counters
           , "hotPeers" .= InboundGovernor.hotPeersRemote counters
           ]
  toJSON (InboundGovernor.TrRemoteState st) =
    object [ "kind" .= String "RemoteState"
           , "remoteSt" .= st
           ]
  toJSON (InboundGovernor.TrUnexpectedlyFalseAssertion info) =
    object [ "kind" .= String "UnexpectedlyFalseAssertion"
           , "remoteSt" .= String (pack . show $ info)
           ]
  toJSON (InboundGovernor.TrInboundGovernorError err) =
    object [ "kind" .= String "InboundGovernorError"
           , "remoteSt" .= String (pack . show $ err)
           ]
  toJSON (InboundGovernor.TrMaturedConnections matured fresh) =
    object [ "kind" .= String "MaturedConnections"
           , "matured" .= toJSON matured
           , "fresh" .= toJSON fresh
           ]
  toJSON (InboundGovernor.TrInactive fresh) =
    object [ "kind" .= String "Inactive"
           , "fresh" .= toJSON fresh
           ]

instance ToJSON addr
      => ToJSON (Server.RemoteTransitionTrace addr) where
    toJSON (ConnMgr.TransitionTrace addr tr) =
      object [ "kind"    .= String "InboundGovernorTransition"
             , "address" .= addr
             , "from"    .= ConnMgr.fromState tr
             , "to"      .= ConnMgr.toState tr
             ]

instance ToJSON DNSTrace where
  toJSON (DNSLookupResult peerKind domain Nothing results) =
    object [ "type" .= String "DNSLookupResult"
           , "peer" .= show peerKind
           , "domain" .= String (Text.decodeUtf8With Text.ignore domain)
           , "results" .= (\(ip, port, ttl) -> (show ip, port, ttl))
                          `map` results
           ]
  toJSON (DNSLookupResult peerKind domain (Just srvDomain) results) =
    object [ "type" .= String "DNSLookupResult"
           , "peer" .= show peerKind
           , "domain" .= String (Text.decodeUtf8With Text.ignore domain)
           , "srvDomain" .= String (Text.decodeUtf8With Text.ignore srvDomain)
           , "results" .= (\(ip, port, ttl) -> (show ip, port, ttl))
                          `map` results
           ]
  toJSON (DNSLookupError peerKind Nothing domain err) =
    object [ "type" .= String "DNSLookupError"
           , "peer" .= show peerKind
           , "domain" .= String (Text.decodeUtf8With Text.ignore domain)
           , "error" .= displayException err
           ]
  toJSON (DNSLookupError peerKind (Just lookupType) domain err) =
    object [ "type" .= String "DNSLookupError"
           , "lookupType" .= show lookupType
           , "peer" .= show peerKind
           , "domain" .= String (Text.decodeUtf8With Text.ignore domain)
           , "error" .= displayException err
           ]
  toJSON (SRVLookupResult peerKind domain results) =
    object [ "type" .= String "SRVLookupResult"
           , "peer" .= show peerKind
           , "domain" .= String (Text.decodeUtf8With Text.ignore domain)
           , "results" .= (\(domain', a, b, c, d) -> (show domain', a, b, c, d))
                          `map` results
           ]
  toJSON (SRVLookupError peerKind domain) =
    object [ "type" .= String "SRVLookupError"
           , "peer" .= show peerKind
           , "domain" .= String (Text.decodeUtf8With Text.ignore domain)
           ]

instance (Show txid, Show tx)
      => ToJSON (AnyMessage (TxSubmission2 txid tx)) where
  toJSON (AnyMessageAndAgency stok Tx.MsgInit) =
    object
      [ "kind" .= String "MsgInit"
      , "agency" .= String (pack $ show stok)
      ]
  toJSON (AnyMessageAndAgency stok (Tx.MsgRequestTxs txids)) =
    object
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= (show <$> toList txids)
      ]
  toJSON (AnyMessageAndAgency stok (Tx.MsgReplyTxs txs)) =
    object
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= (show <$> toList txs)
      ]
  toJSON (AnyMessageAndAgency stok (Tx.MsgRequestTxIds blocking numToAcknowledge numToRequest)) =
    object
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      , "numToAcknowledge" .= Tx.getNumTxIdsToAck numToAcknowledge
      , "numToRequest" .= Tx.getNumTxIdsToReq numToRequest
      , "blocking" .= (case blocking of { Tx.SingBlocking -> True; Tx.SingNonBlocking -> False } :: Bool)
      ]
  toJSON (AnyMessageAndAgency stok (Tx.MsgReplyTxIds txids)) =
    object
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= (show <$> toList txids)
      ]
  toJSON (AnyMessageAndAgency stok Tx.MsgDone) =
    object
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance ToJSON (AnyMessage KeepAlive) where
  toJSON (AnyMessageAndAgency stok (KeepAlive.MsgKeepAlive cookie)) =
    object
      [ "kind" .= String "MsgKeepAlive"
      , "agency" .= String (pack $ show stok)
      , "cookie" .= KeepAlive.unCookie cookie
      ]
  toJSON (AnyMessageAndAgency stok (KeepAlive.MsgKeepAliveResponse cookie)) =
    object
      [ "kind" .= String "MsgKeepAliveResponse"
      , "agency" .= String (pack $ show stok)
      , "cookie" .= KeepAlive.unCookie cookie
      ]
  toJSON (AnyMessageAndAgency stok KeepAlive.MsgDone) =
    object
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance ToJSON peerAddr => ToJSON (AnyMessage (PeerSharing.PeerSharing peerAddr)) where
  toJSON (AnyMessageAndAgency stok (PeerSharing.MsgShareRequest num)) =
    object
      [ "kind" .= String "MsgShareRequest"
      , "agency" .= String (pack $ show stok)
      , "amount" .= PeerSharing.getAmount num
      ]
  toJSON (AnyMessageAndAgency stok (PeerSharing.MsgSharePeers peers)) =
    object
      [ "kind" .= String "MsgSharePeers"
      , "agency" .= String (pack $ show stok)
      , "peers" .= peers
      ]
  toJSON (AnyMessageAndAgency stok PeerSharing.MsgDone) =
    object
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance (ToJSON tx, ToJSON reason) => ToJSON (AnyMessage (LocalTxSubmission tx reason)) where
  toJSON (AnyMessageAndAgency stok (LocalTxSubmission.MsgSubmitTx tx)) =
    object
      [ "kind" .= String "MsgSubmitTx"
      , "agency" .= String (pack $ show stok)
      , "tx" .= tx
      ]
  toJSON (AnyMessageAndAgency stok LocalTxSubmission.MsgAcceptTx) =
    object
      [ "kind" .= String "MsgAcceptTx"
      , "agency" .= String (pack $ show stok)
      ]
  toJSON (AnyMessageAndAgency stok (LocalTxSubmission.MsgRejectTx reason)) =
    object
      [ "kind" .= String "MsgRejectTx"
      , "agency" .= String (pack $ show stok)
      , "reason" .= reason
      ]
  toJSON (AnyMessageAndAgency stok LocalTxSubmission.MsgDone) =
    object
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance ( ToJSON txid
         , ToJSON tx
         , Show txid
         , Show tx
         )
       => ToJSON (TraceTxSubmissionInbound txid tx) where
  toJSON (TraceTxSubmissionCollected count) =
    object
      [ "kind" .= String "TxSubmissionCollected"
      , "count" .= toJSON count
      ]
  toJSON (TraceTxSubmissionProcessed processed) =
    object
      [ "kind" .= String "TxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  toJSON (TraceTxInboundCanRequestMoreTxs count) =
    object
      [ "kind" .= String "TxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  toJSON (TraceTxInboundCannotRequestMoreTxs count) =
    object
      [ "kind" .= String "TxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]
  toJSON (TraceTxInboundAddedToMempool txids diffTime) =
    object
      [ "kind" .= String "TxInboundAddedToMempool"
      , "txids" .= toJSON txids
      , "time" .= diffTime
      ]
  toJSON (TraceTxInboundRejectedFromMempool txids diffTime) =
    object
      [ "kind" .= String "TxInboundRejectedFromMempool"
      , "txids" .= toJSON txids
      , "time" .= diffTime
      ]
  toJSON (TraceTxInboundError err) =
    object
      [ "kind" .= String "TxInboundError"
      , "error" .= String (pack $ show err)
      ]
  toJSON TraceTxInboundTerminated =
    object
      [ "kind" .= String "TxInboundTerminated"
      ]
  toJSON (TraceTxInboundDecision decision) =
    object
      [ "kind" .= String "TxInboundDecision"
      -- TODO: this is too verbose, it will show full tx's
      , "decision" .= String (pack $ show decision)
      ]

-- TODO: in cardano-node in the `coot/tx-submission-10.5` branch there's
-- a better instance.
instance ( Show addr
         , Show txid
         , Show tx
         )
       => ToJSON (TraceTxLogic addr txid tx) where
  toJSON (TraceSharedTxState tag st) =
    object [ "kind" .= String "SharedTxState"
           , "tag"  .= String (pack tag)
           , "sharedTxState" .= String (pack . show $ st)
           ]
  toJSON (TraceTxDecisions decisions) =
    object [ "kind"      .= String "TxDecisions"
           , "decisions" .= String (pack . show $ decisions)
           ]


-- TODO: we need verbosity levels, this instance should be removed once
-- `dmq-node` has a production tracer.
instance (ToJSON txid, ToJSON tx)
      => ToJSON (TraceTxSubmissionOutbound txid tx) where
  toJSON (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    object
      [ "kind"  .= String "TxSubmissionOutboundRecvMsgRequestTxs"
      , "txids" .= txids
      ]
  toJSON (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    object
      [ "kind" .= String "TxSubmissionOutboundSendMsgReplyTxs"
      , "txs"  .= txs
      ]
  toJSON (TraceControlMessage controlMessage) =
    object
      [ "kind" .= String "ControlMessage"
      , "controlMessage" .= String (pack $ show controlMessage)
      ]
