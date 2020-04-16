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

      -- * Null local state query client
    , localStateQueryClientNull
    ) where

import           Control.Monad (forever)
import           Control.Monad.Class.MonadTimer
import           Data.Kind (Type)
import           Network.TypedProtocol.Core

import           Ouroboros.Network.Block (Point)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type


newtype LocalStateQueryClient block (query :: Type -> Type) m a = LocalStateQueryClient {
      runLocalStateQueryClient :: m (ClientStIdle block query m a)
    }

localStateQueryClientNull :: MonadTimer m => LocalStateQueryClient block query m a
localStateQueryClientNull =
    LocalStateQueryClient $ forever $ threadDelay 43200 {- day in seconds -}

{-# DEPRECATED localStateQueryClientNull "Use Ouroboros.Network.NodeToClient.localStateQueryPeerNull" #-}

-- | In the 'StIdle' protocol state, the client has agency and must send:
--
--  * a request to acquire a state
--  * a termination messge
--
data ClientStIdle block query (m :: * -> *) a where
  SendMsgAcquire :: Point block
                 -> ClientStAcquiring block query m a
                 -> ClientStIdle      block query m a

  SendMsgDone    :: a
                 -> ClientStIdle      block query m a

-- | In the 'StAcquiring' protocol state, the client does not have agency.
-- Instead it is waiting for:
--
--  * acquired
--  * failure to acquire
--
-- It must be prepared to handle either.
--
data ClientStAcquiring block query m a = ClientStAcquiring {
      recvMsgAcquired :: ClientStAcquired block query m a,

      recvMsgFailure  :: AcquireFailure
                      -> m (ClientStIdle  block query m a)
    }

-- | In the 'StAcquired' protocol state, the client has agency and must send:
--
--  * a query
--  * a request to (re)acquire another state
--  * a release of the current state
--
data ClientStAcquired block query m a where
  SendMsgQuery     :: query result
                   -> ClientStQuerying block query m a result
                   -> ClientStAcquired block query m a

  SendMsgReAcquire :: Point block
                   -> ClientStAcquiring block query m a
                   -> ClientStAcquired  block query m a

  SendMsgRelease   :: ClientStIdle      block query m a
                   -> ClientStAcquired  block query m a

-- | In the 'StQuerying' protocol state, the client does not have agency.
-- Instead it is waiting for:
--
--  * a result
--
data ClientStQuerying block query m a result = ClientStQuerying {
      recvMsgResult :: result -> m (ClientStAcquired block query m a)
    }

-- | Interpret a 'LocalStateQueryClient' action sequence as a 'Peer' on the
-- client side of the 'LocalStateQuery' protocol.
--
localStateQueryClientPeer
  :: forall block (query :: Type -> Type) m a.
     Monad m
  => LocalStateQueryClient block query m a
  -> Peer (LocalStateQuery block query) AsClient StIdle m a
localStateQueryClientPeer (LocalStateQueryClient handler) =
    Effect $ handleStIdle <$> handler
  where
    handleStIdle
      :: ClientStIdle block query m a
      -> Peer (LocalStateQuery block query) AsClient StIdle m a
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
      :: ClientStAcquiring block query m a
      -> Peer (LocalStateQuery block query) AsClient StAcquiring m a
    handleStAcquiring ClientStAcquiring{recvMsgAcquired, recvMsgFailure} =
      Await (ServerAgency TokAcquiring) $ \req -> case req of
        MsgAcquired        -> handleStAcquired recvMsgAcquired
        MsgFailure failure -> Effect $ handleStIdle <$> recvMsgFailure failure

    handleStAcquired
      :: ClientStAcquired block query m a
      -> Peer (LocalStateQuery block query) AsClient StAcquired m a
    handleStAcquired req = case req of
      SendMsgQuery query stQuerying ->
        Yield (ClientAgency TokAcquired)
              (MsgQuery query)
              (handleStQuerying query stQuerying)
      SendMsgReAcquire pt stAcquiring ->
        Yield (ClientAgency TokAcquired)
              (MsgReAcquire pt)
              (handleStAcquiring stAcquiring)
      SendMsgRelease stIdle ->
        Yield (ClientAgency TokAcquired)
              MsgRelease
              (handleStIdle stIdle)

    handleStQuerying
      :: query result
      -> ClientStQuerying block query m a result
      -> Peer (LocalStateQuery block query) AsClient (StQuerying result) m a
    handleStQuerying query ClientStQuerying{recvMsgResult} =
      Await (ServerAgency (TokQuerying query)) $ \req -> case req of
        MsgResult _ result -> Effect (handleStAcquired <$> recvMsgResult result)
