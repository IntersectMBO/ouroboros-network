{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.Protocol.LocalMsgSubmission.Main where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client

import DMQ.Protocol.LocalMsgSubmission.Type

newtype LocalMsgSubmissionClient tg reject m a = LocalMsgSubmissionClient {
      runLocalTxSubmissionClient :: m (LocalMsgClientStIdle tg reject m a)
    }

-- | The client side of the local transaction submission protocol.
--
-- The peer in the client role submits transactions to the peer in the server
-- role.
--
data LocalMsgClientStIdle tx reject m a where

     -- | The client submits a single transaction and waits a reply.
     --
     -- The server replies to inform the client that it has either accepted the
     -- transaction or rejected it. In the rejection case a reason for the
     -- rejection is included.
     --
     SendMsgSubmitTg
       :: tx
       -> (Either reject () -> m (LocalMsgClientStIdle tx reject m a))
       -> LocalMsgClientStIdle tx reject m a

     -- | The client can terminate the protocol.
     --
     SendMsgDone
       :: a -> LocalMsgClientStIdle tx reject m a


-- | A non-pipelined 'Peer' representing the 'LocalTxSubmissionClient'.
--
localTgSubmissionClientPeer
  :: forall tx reject m a. Monad m
  => LocalMsgSubmissionClient tx reject m a
  -> Client (LocalMsgSubmission tx reject) NonPipelined StIdle m a
localTgSubmissionClientPeer (LocalMsgSubmissionClient client) =
    Effect $ go <$> client
  where
    go :: LocalMsgClientStIdle tx reject m a
       -> Client (LocalMsgSubmission tx reject) NonPipelined StIdle m a
    go (SendMsgSubmitTg tg k) =
      Yield (MsgSubmitTg tg) $
      Await \case
        MsgAcceptTg        -> Effect (go <$> k (Right ()))
        MsgRejectTg reject -> Effect (go <$> k (Left reject))

    go (SendMsgDone a) =
      Yield MsgDone (Done a)
