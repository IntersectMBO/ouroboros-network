module Ouroboros.Network.Protocol.PeerSharing.Codec.CDDL where

import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Network.Socket (SockAddr (..))
import Network.TypedProtocol.Codec
import Ouroboros.Network.NodeToNode.Version qualified as NodeToNode
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)
import Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Type

peerSharingCodec :: NodeToNode.Version
                 -> Codec (PeerSharing SockAddr)
                         CBOR.DeserialiseFailure IO BL.ByteString
peerSharingCodec ntnVersion =
  codecPeerSharing (encodeRemoteAddress ntnVersion)
                   (decodeRemoteAddress ntnVersion)

