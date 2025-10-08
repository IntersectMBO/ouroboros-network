module Cardano.Network.Protocol.PeerSharing.Codec
  ( codecPeerSharing
  , Codec.codecPeerSharingId
  , Codec.byteLimitsPeerSharing
  , Codec.timeLimitsPeerSharing
  ) where

import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Network.TypedProtocol.Codec

import Cardano.Network.NodeToNode.Version

import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)
import Ouroboros.Network.Protocol.PeerSharing.Codec qualified as Codec
import Ouroboros.Network.Protocol.PeerSharing.Type
import Ouroboros.Network.Snocket (RemoteAddress)

codecPeerSharing :: NodeToNodeVersion
                 -> Codec (PeerSharing RemoteAddress)
                         CBOR.DeserialiseFailure IO BL.ByteString
codecPeerSharing ntnVersion =
  Codec.codecPeerSharing (encodeRemoteAddress ntnVersion)
                         (decodeRemoteAddress ntnVersion)

