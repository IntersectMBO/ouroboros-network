{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Ouroboros.Network.Protocol.PeerSharing.Client where

import Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..), PeerRole (..))
import Ouroboros.Network.Protocol.PeerSharing.Type (ClientHasAgency (..),
           Message (..), NobodyHasAgency (..), PeerSharing (..),
           PeerSharingAmount, ServerHasAgency (..))

data PeerSharingClient peerAddress m a where
  SendMsgShareRequest
    :: PeerSharingAmount
    -> ([peerAddress] -> m (PeerSharingClient peerAddress m a))
    -> PeerSharingClient peerAddress m a

  SendMsgDone
    :: m a -> PeerSharingClient peerAddress m a

-- | Interpret a particular client action sequence into the client side of the
-- 'PeerSharing' protocol.
--
peerSharingClientPeer :: Monad m
                      => PeerSharingClient peerAddress m a
                      -> Peer (PeerSharing peerAddress) AsClient StIdle m a
peerSharingClientPeer (SendMsgShareRequest amount k) =
  -- Send MsgShareRequest message
  Yield (ClientAgency TokIdle) (MsgShareRequest amount) $
    -- Wait for the reply (notice the agency proofs)
    Await (ServerAgency TokBusy) $ \(MsgSharePeers resp) ->
      -- We have our reply. We might want to perform some action with it so we
      -- run the continuation to handle t he response.
      Effect $ peerSharingClientPeer <$> k resp
peerSharingClientPeer (SendMsgDone result) =
    -- Perform some finishing action
    -- Perform a transition to the 'StDone' state
    Effect $ Yield (ClientAgency TokIdle) MsgDone . Done TokDone <$> result
