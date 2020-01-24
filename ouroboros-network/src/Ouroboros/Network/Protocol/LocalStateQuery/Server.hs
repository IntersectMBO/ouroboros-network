{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Server (
      -- * Protocol type for the server
      -- | The protocol states from the point of view of the server.
      LocalStateQueryServer(..)
    , ServerStIdle(..)
    , ServerStAcquiring(..)
    , ServerStAcquired(..)
    , ServerStQuerying(..)

      -- * Execution as a typed protocol
    , localStateQueryServerPeer
    ) where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Block (Point)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type


newtype LocalStateQueryServer block query result m a = LocalStateQueryServer {
      runLocalStateQueryServer :: m (ServerStIdle block query result m a)
    }

-- | In the 'StIdle' protocol state, the server does not have agency. Instead
-- it is waiting for:
--
--  * a request to acquire a state
--  * a termination messge
--
-- It must be prepared to handle either.
--
data ServerStIdle block query result m a = ServerStIdle {
       recvMsgAcquire :: Point block
                      -> m (ServerStAcquiring block query result m a),

       recvMsgDone    :: m a
     }

-- | In the 'StAcquiring' protocol state, the server has agency and must send
-- either:
--
--  * acquired
--  * failure to acquire
--
data ServerStAcquiring block query result m a where
  SendMsgAcquired :: ServerStAcquired  block query result m a
                  -> ServerStAcquiring block query result m a

  SendMsgFailure  :: AcquireFailure
                  -> ServerStIdle      block query result m a
                  -> ServerStAcquiring block query result m a

-- | In the 'StAcquired' protocol state, the server does not have agency. Instead
-- it is waiting for:
--
--  * a query
--  * a request to (re)acquire another state
--  * a release of the current state
--
-- It must be prepared to handle either.
--
data ServerStAcquired block query result m a = ServerStAcquired {
      recvMsgQuery     :: query
                       -> m (ServerStQuerying  block query result m a),

      recvMsgReAcquire :: Point block
                       -> m (ServerStAcquiring block query result m a),

      recvMsgRelease   :: m (ServerStIdle      block query result m a)
    }

-- | In the 'StQuerying' protocol state, the server has agency and must send:
--
--  * a result
--
data ServerStQuerying block query result m a where
  SendMsgResult :: result
                -> ServerStAcquired block query result m a
                -> ServerStQuerying block query result m a

-- | Interpret a 'LocalStateQueryServer' action sequence as a 'Peer' on the server
-- side of the 'LocalStateQuery' protocol.
--
localStateQueryServerPeer
  :: forall block query result m a.
     Monad m
  => LocalStateQueryServer block query result m a
  -> Peer (LocalStateQuery block query result) AsServer StIdle m a
localStateQueryServerPeer (LocalStateQueryServer handler) =
    Effect $ handleStIdle <$> handler
  where
    handleStIdle
      :: ServerStIdle block query result m a
      -> Peer (LocalStateQuery block query result) AsServer StIdle m a
    handleStIdle ServerStIdle{recvMsgAcquire, recvMsgDone} =
      Await (ClientAgency TokIdle) $ \req -> case req of
        MsgAcquire pt -> Effect $
          handleStAcquiring <$> recvMsgAcquire pt
        MsgDone       -> Effect $
          Done TokDone <$> recvMsgDone

    handleStAcquiring
      :: ServerStAcquiring block query result m a
      -> Peer (LocalStateQuery block query result) AsServer StAcquiring m a
    handleStAcquiring req = case req of
      SendMsgAcquired stAcquired    ->
        Yield (ServerAgency TokAcquiring)
              MsgAcquired
              (handleStAcquired stAcquired)
      SendMsgFailure failure stIdle ->
        Yield (ServerAgency TokAcquiring)
              (MsgFailure failure)
              (handleStIdle stIdle)

    handleStAcquired
      :: ServerStAcquired block query result m a
      -> Peer (LocalStateQuery block query result) AsServer StAcquired m a
    handleStAcquired ServerStAcquired{recvMsgQuery, recvMsgReAcquire, recvMsgRelease} =
      Await (ClientAgency TokAcquired) $ \req -> case req of
        MsgQuery query  -> Effect $ handleStQuerying  <$> recvMsgQuery query
        MsgReAcquire pt -> Effect $ handleStAcquiring <$> recvMsgReAcquire pt
        MsgRelease      -> Effect $ handleStIdle      <$> recvMsgRelease

    handleStQuerying
      :: ServerStQuerying block query result m a
      -> Peer (LocalStateQuery block query result) AsServer StQuerying m a
    handleStQuerying (SendMsgResult result stAcquired) =
      Yield (ServerAgency TokQuerying)
            (MsgResult result)
            (handleStAcquired stAcquired)
