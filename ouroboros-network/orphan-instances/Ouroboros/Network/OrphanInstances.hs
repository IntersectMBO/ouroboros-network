{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase        #-}

module Ouroboros.Network.OrphanInstances where

import Cardano.Network.NodeToClient (LocalAddress (..), ProtocolLimitFailure)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (zipWithM)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser, listValue)
import Data.Foldable (toList)
import Data.IP (fromHostAddress, fromHostAddress6)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.Mux.Trace (TraceLabelPeer (..))
import Network.Socket (PortNumber, SockAddr (..))
import Ouroboros.Network.Block (SlotNo (SlotNo))
import Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.State (ConnStateId (..),
           LocalAddr (..))
import Ouroboros.Network.ConnectionManager.Types (AbstractState (..),
           ConnectionManagerCounters (..), MaybeUnknown (..),
           OperationResult (..))
import Ouroboros.Network.DeltaQ (GSV (GSV),
           PeerGSV (PeerGSV, inboundGSV, outboundGSV))
import Ouroboros.Network.Diffusion.Topology (LocalRootPeersGroup (..),
           LocalRootPeersGroups (..), NetworkTopology (..),
           PublicRootPeers (..), RootConfig (..))
import Ouroboros.Network.ExitPolicy (RepromoteDelay (repromoteDelay))
import Ouroboros.Network.InboundGovernor.State (RemoteSt)
import Ouroboros.Network.Mux (MiniProtocolNum (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Governor.Types (AssociationMode (..),
           PeerSelectionTargets (..), PeerSharingResult (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (AfterSlot (After, Always), UseLedgerPeers (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import Ouroboros.Network.PeerSelection.State.KnownPeers
           (KnownPeerInfo (KnownPeerInfo))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers
           (HotValency (HotValency),
           LocalRootConfig (LocalRootConfig, peerAdvertise), LocalRootPeers,
           WarmValency (WarmValency))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LRP
import Ouroboros.Network.PeerSelection.Types (PeerStatus)
import Ouroboros.Network.Protocol.Handshake (HandshakeException (..),
           HandshakeProtocolError (..), RefuseReason (..))
import Ouroboros.Network.Protocol.Limits
           (ProtocolLimitFailure (ExceededSizeLimit, ExceededTimeLimit))
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (RemoteAddress)

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
  toJSON = String . pack . show

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
    LocalRootPeersGroup
      <$> parseJSON (Object o)
      <*> pure hv
      <*> o .:? "warmValency" .!= WarmValency v
      <*> (fromMaybe InitiatorAndResponderDiffusionMode
            <$> o .:? "diffusionMode")
      <*> parseExtraFlags o

localRootPeersGroupToJSON :: (extraFlags -> Maybe (Key, Value))
                          -- ^ if `Nothing` is returned the `extraFlags` are
                          -- not encoded in the JSON value
                          -> LocalRootPeersGroup extraFlags
localRootPeersGroupToJSON extraFlagsToJSON lrpg =
    Object $
         ("accessPoints"  .?= rootAccessPoints (localRoots lrpg))
      <> ("advertise"     .?= rootAdvertise (localRoots lrpg))
      <> ("hotValency"    .?= hotValency lrpg)
      <> ("warmValency"   .?= warmValency lrpg)
      <> (case mv of
            Nothing     -> mempty
            Just (k, v) -> k .?= v)
      <> ("diffusionMode" .?= rootDiffusionMode lrpg)
  where
    mv = extraFlagsToJSON (extraFlags lrpg)

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
                              <*> o .: "publicRoots"
                              <*> o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers
                              <*> o .: "peerSnapshotFile"
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
      <> (case mv of
            Nothing    -> mempty
            Just (k,v) -> k .?= v)
  where
    mv = extraConfigToJSON extraConfig

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

instance Show vNumber => ToJSON (RefuseReason vNumber) where
  toJSON = \case
    VersionMismatch vNumber tags -> kindObject "VersionMismatch"
      [ "versionNumber" .= show vNumber
      , "tags" .= toJSONList tags
      ]
    HandshakeDecodeError vNumber t -> kindObject "HandshakeDecodeError"
      [ "versionNumber" .= show vNumber
      , "text" .= String (pack $ show t)
      ]
    Refused vNumber t -> kindObject "Refused"
      [ "versionNumber" .= show vNumber
      , "text" .= String (pack $ show t)
      ]

instance Show vNumber => ToJSON (HandshakeProtocolError vNumber) where
  toJSON = \case
    HandshakeError rvNumber          -> kindObject "HandshakeError" [ "reason" .= toJSON rvNumber ]
    NotRecognisedVersion vNumber     -> kindObject "NotRecognisedVersion" [ "versionNumber" .= show vNumber ]
    InvalidServerSelection vNumber t -> kindObject "InvalidServerSelection"
      [ "versionNumber" .= show vNumber
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

instance (ToJSON extraFlags, ToJSONKey peerAddr, ToJSON peerAddr, Ord peerAddr, Show peerAddr)
  => ToJSON (LocalRootPeers extraFlags peerAddr) where
  toJSON lrp = kindObject "LocalRootPeers"
    [ "groups" .= toJSONList (LocalRootPeers.toGroups lrp) ]

instance ToJSON PeerSelectionTargets where
  toJSON (PeerSelectionTargets
            nRootPeers
            nKnownPeers
            nEstablishedPeers
            nActivePeers
            nKnownBigLedgerPeers
            nEstablishedBigLedgerPeers
            nActiveBigLedgerPeers
         ) = kindObject "PeerSelectionTargets"
           [ "targetRootPeers"                 .= nRootPeers
           , "targetKnownPeers"                .= nKnownPeers
           , "targetEstablishedPeers"          .= nEstablishedPeers
           , "targetActivePeers"               .= nActivePeers
           , "targetKnownBigLedgerPeers"       .= nKnownBigLedgerPeers
           , "targetEstablishedBigLedgerPeers" .= nEstablishedBigLedgerPeers
           , "targetActiveBigLedgerPeers"      .= nActiveBigLedgerPeers
           ]

instance ToJSON RepromoteDelay where
  toJSON = toJSON . repromoteDelay

instance ToJSON addr => ToJSON (PeerSharingResult addr) where
  toJSON = \case
    PeerSharingResult addrs     -> toJSONList addrs
    PeerSharingNotRegisteredYet -> String "PeerSharingNotRegisteredYet"

instance ToJSON extraFlags => ToJSON (LocalRootConfig extraFlags) where
  toJSON LocalRootConfig { peerAdvertise, LRP.extraFlags, LRP.diffusionMode } =
    object
      [ "peerAdvertise" .= peerAdvertise
      , "diffusionMode" .= show diffusionMode
      , "extraFlags"    .= extraFlags
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

instance (ToJSON peer, ToJSON point) => ToJSON (TraceLabelPeer peer (FetchDecision [point])) where
  toJSON (TraceLabelPeer peer decision) = object
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
