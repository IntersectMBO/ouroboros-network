module Ouroboros.Network.Protocol.PeerSharing.Codec.CDDL where

import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Network.Socket (SockAddr (..))
import Network.TypedProtocol.Codec
import Ouroboros.Network.NodeToNode.Version
import Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Type
import Ouroboros.Network.RemoteAddress.Codec (decodeRemoteAddress,
           encodeRemoteAddress)

peerSharingCodec :: NodeToNodeVersion
                 -> Codec (PeerSharing SockAddr)
                         CBOR.DeserialiseFailure IO BL.ByteString
peerSharingCodec _ntnVersion =
  codecPeerSharing encodeRemoteAddress
                   decodeRemoteAddress

