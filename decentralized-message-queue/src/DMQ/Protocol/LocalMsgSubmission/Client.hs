{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.Protocol.LocalMsgSubmission.Client where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client

import DMQ.Protocol.LocalMsgSubmission.Type

newtype LocalMsgSubmissionClient msg reject m a = LocalMsgSubmissionClient {
      runLocalTxSubmissionClient :: m (LocalMsgClientStIdle msg reject m a)
    }

-- | The client side of the local transaction submission protocol.
--
-- The peer in the client role submits transactions to the peer in the server
-- role.
--
data LocalMsgClientStIdle msg reject m a where

     -- | The client submits a single transaction and waits a reply.
     --
     -- The server replies to inform the client that it has either accepted the
     -- transaction or rejected it. In the rejection case a reason for the
     -- rejection is included.
     --
     SendMsgSubmit
       :: msg
       -> (Either reject () -> m (LocalMsgClientStIdle msg reject m a))
       -> LocalMsgClientStIdle msg reject m a

     -- | The client can terminate the protocol.
     --
     SendMsgDone
       :: a -> LocalMsgClientStIdle msg reject m a


-- | A non-pipelined 'Peer' representing the 'LocalTxSubmissionClient'.
--
localMsgSubmissionClientPeer
  :: forall msg reject m a. Monad m
  => LocalMsgSubmissionClient msg reject m a
  -> Client (LocalMsgSubmission msg reject) NonPipelined StIdle m a
localMsgSubmissionClientPeer (LocalMsgSubmissionClient client) =
    Effect $ go <$> client
  where
    go :: LocalMsgClientStIdle msg reject m a
       -> Client (LocalMsgSubmission msg reject) NonPipelined StIdle m a
    go (SendMsgSubmit msg k) =
      Yield (MsgSubmit msg) $
      Await \case
        MsgAccept        -> Effect (go <$> k (Right ()))
        MsgReject reject -> Effect (go <$> k (Left reject))

    go (SendMsgDone a) =
      Yield MsgDone (Done a)
