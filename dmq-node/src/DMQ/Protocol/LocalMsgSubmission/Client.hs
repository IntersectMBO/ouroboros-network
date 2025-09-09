{-# LANGUAGE DataKinds #-}

-- | A higher level API to implement clients for local message submission
-- miniprotocol. This aliases the client from the 'LocalTxSubmission' protocol.
--
-- For execution, 'localMsgSubmissionClientPeer' reinterprets this high level
-- description into the underlying typed protocol representation.
--
module DMQ.Protocol.LocalMsgSubmission.Client
  ( -- * Client API types (re-exports)
    LocalMsgSubmissionClient
  , LocalMsgClientStIdle
    -- * Translates the client into a typed protocol
  , localMsgSubmissionClientPeer
  ) where

import DMQ.Protocol.LocalMsgSubmission.Type
import Network.TypedProtocol.Peer.Client
import Ouroboros.Network.Protocol.LocalTxSubmission.Client

-- | Type aliases for the high level client API
--
type LocalMsgSubmissionClient sig = LocalTxSubmissionClient sig SigMempoolFail
type LocalMsgClientStIdle     = LocalTxClientStIdle


-- | A non-pipelined 'Peer' representing the 'LocalMsgSubmissionClient'.
--
-- Translates a high level client description into the underlying typed
-- protocol representation.
--
localMsgSubmissionClientPeer
  :: forall msg m a. Monad m
  => LocalMsgSubmissionClient msg m a
  -> Client (LocalMsgSubmission msg) NonPipelined StIdle m a
localMsgSubmissionClientPeer = localTxSubmissionClientPeer
