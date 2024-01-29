{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.PeerSharing.Examples where

import Data.Word (Word8)
import Ouroboros.Network.Protocol.PeerSharing.Client (PeerSharingClient (..))
import Ouroboros.Network.Protocol.PeerSharing.Server (PeerSharingServer (..))
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import Test.QuickCheck.Function (Fun, applyFun)


-- | A client which collects answers whenever it receives
-- 'MsgSharePeers' and returns the result.
--
peerSharingClientCollect :: forall peer m . Monad m
                         => [PeerSharingAmount]
                         -> PeerSharingClient peer m [peer]
peerSharingClientCollect = go []
  where
    go :: [peer] -> [PeerSharingAmount] -> PeerSharingClient peer m [peer]
    go acc []    = SendMsgDone (pure acc)
    go acc (h:t) = SendMsgShareRequest h (\r -> return (go (r ++ acc) t))


-- | A server which counts number received of 'MsgPeerShareRequest'.
--
peerSharingServerReplicate :: forall m . Monad m
                           => Fun Word8 Int
                           -> PeerSharingServer Int m
peerSharingServerReplicate f = go 0
  where
    go :: Int -> PeerSharingServer Int m
    go n =
      PeerSharingServer
        { recvMsgShareRequest = \ (PeerSharingAmount amount) -> do
            let r = replicate (applyFun f amount) n
            return (r, go (n + 1))
        }
