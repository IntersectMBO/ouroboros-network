{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.Protocol.LocalMsgSubmission.Server where

import DMQ.Protocol.LocalMsgSubmission.Type
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Server

-- | The server side of the local message submission protocol.
--
-- The client peer submits messages to the server
--
data LocalMsgSubmissionServer msg reject m a =
     LocalMsgSubmissionServer {

       -- | The client has submited a single message and it expects a reply.
       --
       -- The server must reply to inform the client that it has either accepted
       -- the message or rejected it. In the rejection case a reason for the
       -- rejection is included.
       --
       recvMsgSubmit :: msg -> m ( Either reject ()
                                 , LocalMsgSubmissionServer msg reject m a ),

       -- | The client can terminate the protocol.
       --
       recvMsgDone     :: a
     }

-- | A non-pipelined 'Peer' representing the 'LocalMsgSubmissionServer'.
--
localMsgSubmissionServerPeer
  :: forall msg reject m a. Monad m
  => m (LocalMsgSubmissionServer msg reject m a)
  -> Server (LocalMsgSubmission msg reject) NonPipelined StIdle m a
localMsgSubmissionServerPeer server =
    Effect $ go <$> server
  where
    go :: LocalMsgSubmissionServer msg reject m a
       -> Server (LocalMsgSubmission msg reject) NonPipelined StIdle m a
    go LocalMsgSubmissionServer{recvMsgSubmit, recvMsgDone} =
      Await \case
        MsgSubmit msg -> Effect do
          (result, k) <- recvMsgSubmit msg
          return $
            case result of
              Right _ ->
                Yield
                  MsgAccept
                  (go k)
              Left reject ->
                Yield
                  (MsgReject reject)
                  (go k)

        MsgDone -> Done recvMsgDone
