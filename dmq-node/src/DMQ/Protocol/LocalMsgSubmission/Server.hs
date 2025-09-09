{-# LANGUAGE DataKinds #-}

-- | A higher level API to implement server for local message submission
-- miniprotocol. This aliases the server from the 'LocalTxSubmission' protocol.
--
-- For execution, 'localMsgSubmissionServerPeer' reinterprets this high level
-- description into the underlying typed protocol representation.
--
module DMQ.Protocol.LocalMsgSubmission.Server
  ( -- * Server API types (re-exports)
    LocalMsgSubmissionServer
    -- * Translates the server into a typed protocol
  , localMsgSubmissionServerPeer
    -- re-exports
  , module LocalTxSubmission
  ) where

import DMQ.Protocol.LocalMsgSubmission.Type
import Network.TypedProtocol.Peer.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Server as LocalTxSubmission

-- | Type aliases for the high level client API
--
type LocalMsgSubmissionServer sig = LocalTxSubmissionServer sig SigMempoolFail


-- | A non-pipelined 'Peer' representing the 'LocalMsgSubmissionServer'.
--
-- Translates a high level server description into the underying typed
-- protocol representation.
--
localMsgSubmissionServerPeer
  :: forall msg m a.
     Monad m
  => m (LocalMsgSubmissionServer msg m a)
  -> Server (LocalMsgSubmission msg) NonPipelined StIdle m a
localMsgSubmissionServerPeer = localTxSubmissionServerPeer
