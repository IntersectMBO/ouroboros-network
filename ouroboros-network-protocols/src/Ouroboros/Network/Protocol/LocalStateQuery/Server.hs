{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Server
  ( -- * Protocol type for the server
    -- | The protocol states from the point of view of the server.
    LocalStateQueryServer (..)
  , ServerStIdle (..)
  , ServerStAcquiring (..)
  , ServerStAcquired (..)
  , ServerStQuerying (..)
    -- * Execution as a typed protocol
  , localStateQueryServerPeer
  ) where

import           Data.Kind (Type)
import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalStateQuery.Type


newtype LocalStateQueryServer block point (query :: Type -> Type) m a = LocalStateQueryServer {
      runLocalStateQueryServer :: m (ServerStIdle block point query m a)
    }

-- | In the 'StIdle' protocol state, the server does not have agency. Instead
-- it is waiting for:
--
--  * a request to acquire a state
--  * a termination messge
--
-- It must be prepared to handle either.
--
data ServerStIdle block point query m a = ServerStIdle {
       recvMsgAcquire :: Maybe point
                      -> m (ServerStAcquiring block point query m a),

       recvMsgDone    :: m a
     }

-- | In the 'StAcquiring' protocol state, the server has agency and must send
-- either:
--
--  * acquired
--  * failure to acquire
--
data ServerStAcquiring block point query m a where
  SendMsgAcquired :: ServerStAcquired  block point query m a
                  -> ServerStAcquiring block point query m a

  SendMsgFailure  :: AcquireFailure
                  -> ServerStIdle      block point query m a
                  -> ServerStAcquiring block point query m a

-- | In the 'StAcquired' protocol state, the server does not have agency.
-- Instead it is waiting for:
--
--  * a query
--  * a request to (re)acquire another state
--  * a release of the current state
--
-- It must be prepared to handle either.
--
data ServerStAcquired block point query m a = ServerStAcquired {
      recvMsgQuery     :: forall result.
                          query result
                       -> m (ServerStQuerying  block point query m a result),

      recvMsgReAcquire :: Maybe point
                       -> m (ServerStAcquiring block point query m a),

      recvMsgRelease   :: m (ServerStIdle      block point query m a)
    }

-- | In the 'StQuerying' protocol state, the server has agency and must send:
--
--  * a result
--
data ServerStQuerying block point query m a result where
  SendMsgResult :: result
                -> ServerStAcquired block point query m a
                -> ServerStQuerying block point query m a result

-- | Interpret a 'LocalStateQueryServer' action sequence as a 'Peer' on the server
-- side of the 'LocalStateQuery' protocol.
--
localStateQueryServerPeer
  :: forall block point (query :: Type -> Type) m a.
     Monad m
  => LocalStateQueryServer block point query m a
  -> Peer (LocalStateQuery block point query) AsServer StIdle m a
localStateQueryServerPeer (LocalStateQueryServer handler) =
    Effect $ handleStIdle <$> handler
  where
    handleStIdle
      :: ServerStIdle block point query m a
      -> Peer (LocalStateQuery block point query) AsServer StIdle m a
    handleStIdle ServerStIdle{recvMsgAcquire, recvMsgDone} =
      Await (ClientAgency TokIdle) $ \req -> case req of
        MsgAcquire pt -> Effect $
          handleStAcquiring <$> recvMsgAcquire pt
        MsgDone       -> Effect $
          Done TokDone <$> recvMsgDone

    handleStAcquiring
      :: ServerStAcquiring block point query m a
      -> Peer (LocalStateQuery block point query) AsServer StAcquiring m a
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
      :: ServerStAcquired block point query m a
      -> Peer (LocalStateQuery block point query) AsServer StAcquired m a
    handleStAcquired ServerStAcquired{recvMsgQuery, recvMsgReAcquire, recvMsgRelease} =
      Await (ClientAgency TokAcquired) $ \req -> case req of
        MsgQuery query  -> Effect $ handleStQuerying query <$> recvMsgQuery query
        MsgReAcquire pt -> Effect $ handleStAcquiring      <$> recvMsgReAcquire pt
        MsgRelease      -> Effect $ handleStIdle           <$> recvMsgRelease

    handleStQuerying
      :: query result
      -> ServerStQuerying block point query m a result
      -> Peer (LocalStateQuery block point query) AsServer (StQuerying result) m a
    handleStQuerying query (SendMsgResult result stAcquired) =
      Yield (ServerAgency (TokQuerying query))
            (MsgResult query result)
            (handleStAcquired stAcquired)
