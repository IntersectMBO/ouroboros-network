{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Client (
      -- * Protocol type for the client
      -- | The protocol states from the point of view of the client.
      LocalStateQueryClient(..)
    , ClientStIdle(..)
    , ClientStAcquiring(..)
    , ClientStAcquired(..)
    , ClientStQuerying(..)

      -- * Execution as a typed protocol
    , localStateQueryClientPeer
    ) where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Block (Point)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type


newtype LocalStateQueryClient block query result m a = LocalStateQueryClient {
      runLocalStateQueryClient :: ClientStIdle block query result m a
    }

-- | In the 'StIdle' protocol state, the client has agency and must send:
--
--  * a request to acquire a state
--  * a termination messge
--
data ClientStIdle block query result (m :: * -> *) a where
  SendMsgAcquire :: Point block
                 -> ClientStAcquiring block query result m a
                 -> ClientStIdle      block query result m a

  SendMsgDone    :: a
                 -> ClientStIdle      block query result m a

-- | In the 'StAcquiring' protocol state, the client does not have agency.
-- Instead it is waiting for:
--
--  * acquired
--  * failure to acquire
--
-- It must be prepared to handle either.
--
data ClientStAcquiring block query result m a = ClientStAcquiring {
      recvMsgAcquired :: ClientStAcquired block query result m a,

      recvMsgFailure  :: AcquireFailure
                      -> m (ClientStIdle  block query result m a)
    }

-- | In the 'StAcquired' protocol state, the client has agency and must send:
--
--  * a query
--  * a request to (re)acquire another state
--  * a release of the current state
--
data ClientStAcquired block query result m a where
  SendMsgQuery     :: query
                   -> ClientStQuerying block query result m a
                   -> ClientStAcquired block query result m a

  SendMsgReAcquire :: Point block
                   -> ClientStAcquiring block query result m a
                   -> ClientStAcquired  block query result m a

  SendMsgRelease   :: ClientStIdle      block query result m a
                   -> ClientStAcquired  block query result m a

-- | In the 'StQuerying' protocol state, the client does not have agency.
-- Instead it is waiting for:
--
--  * a result
--
data ClientStQuerying block query result m a = ClientStQuerying {
      recvMsgResult :: result -> m (ClientStAcquired block query result m a)
    }

-- | Interpret a 'LocalStateQueryClient' action sequence as a 'Peer' on the
-- client side of the 'LocalStateQuery' protocol.
--
localStateQueryClientPeer
  :: forall block query result m a.
     Monad m
  => LocalStateQueryClient block query result m a
  -> Peer (LocalStateQuery block query result) AsClient StIdle m a
localStateQueryClientPeer (LocalStateQueryClient handler) =
    handleStIdle handler
  where
    handleStIdle
      :: ClientStIdle block query result m a
      -> Peer (LocalStateQuery block query result) AsClient StIdle m a
    handleStIdle req = case req of
      SendMsgAcquire pt stAcquiring ->
        Yield (ClientAgency TokIdle)
              (MsgAcquire pt)
              (handleStAcquiring stAcquiring)
      SendMsgDone a ->
        Yield (ClientAgency TokIdle)
              MsgDone
              (Done TokDone a)

    handleStAcquiring
      :: ClientStAcquiring block query result m a
      -> Peer (LocalStateQuery block query result) AsClient StAcquiring m a
    handleStAcquiring ClientStAcquiring{recvMsgAcquired, recvMsgFailure} =
      Await (ServerAgency TokAcquiring) $ \req -> case req of
        MsgAcquired        -> handleStAcquired recvMsgAcquired
        MsgFailure failure -> Effect $ handleStIdle <$> recvMsgFailure failure

    handleStAcquired
      :: ClientStAcquired block query result m a
      -> Peer (LocalStateQuery block query result) AsClient StAcquired m a
    handleStAcquired req = case req of
      SendMsgQuery query stQuerying ->
        Yield (ClientAgency TokAcquired)
              (MsgQuery query)
              (handleStQuerying stQuerying)
      SendMsgReAcquire pt stAcquiring ->
        Yield (ClientAgency TokAcquired)
              (MsgReAcquire pt)
              (handleStAcquiring stAcquiring)
      SendMsgRelease stIdle ->
        Yield (ClientAgency TokAcquired)
              MsgRelease
              (handleStIdle stIdle)

    handleStQuerying
      :: ClientStQuerying block query result m a
      -> Peer (LocalStateQuery block query result) AsClient StQuerying m a
    handleStQuerying ClientStQuerying{recvMsgResult} =
      Await (ServerAgency TokQuerying) $ \req -> case req of
        MsgResult result -> Effect (handleStAcquired <$> recvMsgResult result)
