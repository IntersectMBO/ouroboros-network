{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
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

      -- * Utilities
    , mapLocalStateQueryClient
    , mapQueryOnLocalStateQueryClient

    , Some (..)
    ) where

import           Control.Monad (forever)
import           Control.Monad.Class.MonadTimer
import           Data.Kind (Type)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some(..))

newtype LocalStateQueryClient block point (query :: Type -> Type) m a =
    LocalStateQueryClient {
      runLocalStateQueryClient :: m (ClientStIdle block point query m a)
    }

mapQueryOnLocalStateQueryClient :: Monad m => (forall r. query r -> query' r) -> LocalStateQueryClient block point query m a -> LocalStateQueryClient block point query' m a
mapQueryOnLocalStateQueryClient f localStateQueryClient = LocalStateQueryClient
  { runLocalStateQueryClient = mapQueryOnClientStIdle f <$> runLocalStateQueryClient localStateQueryClient
  }

localStateQueryClientNull :: MonadTimer m => LocalStateQueryClient block point query m a
localStateQueryClientNull =
    LocalStateQueryClient $ forever $ threadDelay 43200 {- day in seconds -}

{-# DEPRECATED localStateQueryClientNull "Use Ouroboros.Network.NodeToClient.localStateQueryPeerNull" #-}

-- | In the 'StIdle' protocol state, the client has agency and must send:
--
--  * a request to acquire a state
--  * a termination messge
--
data ClientStIdle block point query (m :: Type -> Type) a where
  SendMsgAcquire :: Maybe point
                 -> ClientStAcquiring block point query m a
                 -> ClientStIdle      block point query m a

  SendMsgDone    :: a
                 -> ClientStIdle      block point query m a

mapQueryOnClientStIdle :: Monad m => (forall r . query r -> query' r) -> ClientStIdle block point query m a -> ClientStIdle block point query' m a
mapQueryOnClientStIdle f client = case client of
  SendMsgAcquire mPoint clientStAcquiring -> SendMsgAcquire mPoint (mapQueryOnClientStAcquiring f clientStAcquiring)
  SendMsgDone a -> SendMsgDone a

-- | In the 'StAcquiring' protocol state, the client does not have agency.
-- Instead it is waiting for:
--
--  * acquired
--  * failure to acquire
--
-- It must be prepared to handle either.
--
data ClientStAcquiring block point query m a = ClientStAcquiring {
      recvMsgAcquired :: m (ClientStAcquired block point query m a),

      recvMsgFailure  :: AcquireFailure
                      -> m (ClientStIdle  block point query m a)
    }

mapQueryOnClientStAcquiring :: Monad m => (forall r . query r -> query' r) -> ClientStAcquiring block point query m a -> ClientStAcquiring block point query' m a
mapQueryOnClientStAcquiring f acquiring = ClientStAcquiring
  { recvMsgAcquired = fmap (mapQueryOnClientStAcquired f) (recvMsgAcquired acquiring)
  , recvMsgFailure = fmap (mapQueryOnClientStIdle f) . recvMsgFailure acquiring
  }

-- | In the 'StAcquired' protocol state, the client has agency and must send:
--
--  * a query
--  * a request to (re)acquire another state
--  * a release of the current state
--
data ClientStAcquired block point query m a where
  SendMsgQuery     :: query result
                   -> ClientStQuerying block point query m a result
                   -> ClientStAcquired block point query m a

  SendMsgReAcquire :: Maybe point
                   -> ClientStAcquiring block point query m a
                   -> ClientStAcquired  block point query m a

  SendMsgRelease   :: m (ClientStIdle   block point query m a)
                   -> ClientStAcquired  block point query m a

mapQueryOnClientStAcquired :: Monad m => (forall r . query r -> query' r) -> ClientStAcquired block point query m a -> ClientStAcquired block point query' m a
mapQueryOnClientStAcquired f clientStAcquired = case clientStAcquired of
  SendMsgQuery queryResult clientStQuerying -> SendMsgQuery (f queryResult) (mapQueryOnClientStQuerying f clientStQuerying)
  SendMsgReAcquire mPoint clientStAcquiring -> SendMsgReAcquire mPoint (mapQueryOnClientStAcquiring f clientStAcquiring)
  SendMsgRelease runClientStIdle -> SendMsgRelease (mapQueryOnClientStIdle f <$> runClientStIdle)


-- | In the 'StQuerying' protocol state, the client does not have agency.
-- Instead it is waiting for:
--
--  * a result
--
data ClientStQuerying block point query m a result = ClientStQuerying {
      recvMsgResult :: result -> m (ClientStAcquired block point query m a)
    }

mapQueryOnClientStQuerying :: Monad m => (forall r. query r -> query' r) -> ClientStQuerying block point query m a result -> ClientStQuerying block point query' m a result
mapQueryOnClientStQuerying f clientStQuerying = ClientStQuerying
  { recvMsgResult = fmap (mapQueryOnClientStAcquired f) . recvMsgResult clientStQuerying
  }

-- | Transform a 'LocalStateQueryClient' by mapping over the query and query
-- result values.
--
-- Note the direction of the individual mapping functions corresponds to
-- whether the types are used as protocol inputs or outputs.
--
mapLocalStateQueryClient :: forall block block' point point' query query' m a.
                            Functor m
                         => (point -> point')
                         -> (forall result. query result -> Some query')
                         -> (forall result result'.
                                    query result
                                 -> query' result'
                                 -> result' -> result)
                         -> LocalStateQueryClient block  point  query  m a
                         -> LocalStateQueryClient block' point' query' m a
mapLocalStateQueryClient fpoint fquery fresult =
    \(LocalStateQueryClient c) -> LocalStateQueryClient (fmap goIdle c)
  where
    goIdle :: ClientStIdle block  point  query  m a
           -> ClientStIdle block' point' query' m a
    goIdle (SendMsgAcquire pt k) =
      SendMsgAcquire (fpoint <$> pt) (goAcquiring k)

    goIdle (SendMsgDone a) = SendMsgDone a

    goAcquiring :: ClientStAcquiring block  point  query  m a
                -> ClientStAcquiring block' point' query' m a
    goAcquiring ClientStAcquiring { recvMsgAcquired, recvMsgFailure } =
      ClientStAcquiring {
        recvMsgAcquired = goAcquired <$> recvMsgAcquired,
        recvMsgFailure  = \failure -> fmap goIdle (recvMsgFailure failure)
      }

    goAcquired :: ClientStAcquired block  point  query  m a
               -> ClientStAcquired block' point' query' m a
    goAcquired (SendMsgQuery     q  k) = case fquery q of
                                           Some q' -> SendMsgQuery q' (goQuerying q q' k)
    goAcquired (SendMsgReAcquire pt k) = SendMsgReAcquire (fpoint <$> pt) (goAcquiring k)
    goAcquired (SendMsgRelease      k) = SendMsgRelease (fmap goIdle k)

    goQuerying :: forall result result'.
                  query  result
               -> query' result'
               -> ClientStQuerying block  point  query  m a result
               -> ClientStQuerying block' point' query' m a result'
    goQuerying q q' ClientStQuerying {recvMsgResult} =
      ClientStQuerying {
        recvMsgResult = \result' ->
          let result :: result
              result = fresult q q' result' in
          fmap goAcquired (recvMsgResult result)
      }


-- | Interpret a 'LocalStateQueryClient' action sequence as a 'Peer' on the
-- client side of the 'LocalStateQuery' protocol.
--
localStateQueryClientPeer
  :: forall block point (query :: Type -> Type) m a.
     Monad m
  => LocalStateQueryClient block point query m a
  -> Peer (LocalStateQuery block point query) AsClient StIdle m a
localStateQueryClientPeer (LocalStateQueryClient handler) =
    Effect $ handleStIdle <$> handler
  where
    handleStIdle
      :: ClientStIdle block point query m a
      -> Peer (LocalStateQuery block point query) AsClient StIdle m a
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
      :: ClientStAcquiring block point query m a
      -> Peer (LocalStateQuery block point query) AsClient StAcquiring m a
    handleStAcquiring ClientStAcquiring{recvMsgAcquired, recvMsgFailure} =
      Await (ServerAgency TokAcquiring) $ \req -> case req of
        MsgAcquired        -> Effect $ handleStAcquired <$> recvMsgAcquired
        MsgFailure failure -> Effect $ handleStIdle <$> recvMsgFailure failure

    handleStAcquired
      :: ClientStAcquired block point query m a
      -> Peer (LocalStateQuery block point query) AsClient StAcquired m a
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
              (Effect (handleStIdle <$> stIdle))

    handleStQuerying
      :: query result
      -> ClientStQuerying block point query m a result
      -> Peer (LocalStateQuery block point query) AsClient (StQuerying result) m a
    handleStQuerying query ClientStQuerying{recvMsgResult} =
      Await (ServerAgency (TokQuerying query)) $ \req -> case req of
        MsgResult _ result -> Effect (handleStAcquired <$> recvMsgResult result)
