{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the local transaction submission protocol from the point of view
-- of the server.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Ouroboros.Network.Protocol.LocalTxSubmission.Server
  ( -- * Protocol type for the server
    -- | The protocol states from the point of view of the server.
    LocalTxSubmissionServer (..)
    -- * Execution as a typed protocol
  , localTxSubmissionServerPeer
  ) where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type


-- | The server side of the local transaction submission protocol.
--
-- The peer in the client role submits transactions to the peer in the server
-- role.
--
data LocalTxSubmissionServer tx reject m a =
     LocalTxSubmissionServer {

       -- | The client has submited a single transaction and it expects a reply.
       --
       -- The server must reply to inform the client that it has either accepted
       -- the transaction or rejected it. In the rejection case a reason for the
       -- rejection is included.
       --
       recvMsgSubmitTx :: tx -> m ( SubmitResult reject
                                  , LocalTxSubmissionServer tx reject m a ),

       -- | The client can terminate the protocol.
       --
       recvMsgDone     :: a
     }


-- | A non-pipelined 'Peer' representing the 'LocalTxSubmissionServer'.
--
localTxSubmissionServerPeer
  :: forall tx reject m a. Monad m
  => m (LocalTxSubmissionServer tx reject m a)
  -> Peer (LocalTxSubmission tx reject) AsServer StIdle m a
localTxSubmissionServerPeer server =
    Effect $ go <$> server
  where
    go :: LocalTxSubmissionServer tx reject m a
       -> Peer (LocalTxSubmission tx reject) AsServer StIdle m a
    go LocalTxSubmissionServer{recvMsgSubmitTx, recvMsgDone} =
      Await (ClientAgency TokIdle) $ \msg -> case msg of
        MsgSubmitTx tx -> Effect $ do
          (result, k) <- recvMsgSubmitTx tx
          return $
            case result of
              SubmitSuccess ->
                Yield
                  (ServerAgency TokBusy)
                  MsgAcceptTx
                  (go k)
              SubmitFail reject ->
                Yield
                  (ServerAgency TokBusy)
                  (MsgRejectTx reject)
                  (go k)

        MsgDone -> Done TokDone recvMsgDone

