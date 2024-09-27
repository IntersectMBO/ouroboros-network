{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.LocalStateQuery.Client
  ( -- * Protocol type for the client
    -- | The protocol states from the point of view of the client.
    LocalStateQueryClient (..)
  , ClientStIdle (..)
  , ClientStAcquiring (..)
  , ClientStAcquired (..)
  , ClientStQuerying (..)
    -- * Execution as a typed protocol
  , localStateQueryClientPeer
    -- * Null local state query client
  , localStateQueryClientNull
    -- * Utilities
  , mapLocalStateQueryClient
  , Some (..)
  ) where

import Control.Monad (forever)
import Control.Monad.Class.MonadTimer
import Data.Kind (Type)

import Network.TypedProtocol.Stateful.Peer.Client

import Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type


newtype LocalStateQueryClient block point (query :: Type -> Type) m a =
    LocalStateQueryClient {
      runLocalStateQueryClient :: m (ClientStIdle block point query m a)
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
  SendMsgAcquire :: Target point
                 -> ClientStAcquiring block point query m a
                 -> ClientStIdle      block point query m a

  SendMsgDone    :: a
                 -> ClientStIdle      block point query m a

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

  SendMsgReAcquire :: Target point
                   -> ClientStAcquiring block point query m a
                   -> ClientStAcquired  block point query m a

  SendMsgRelease   :: m (ClientStIdle   block point query m a)
                   -> ClientStAcquired  block point query m a

-- | In the 'StQuerying' protocol state, the client does not have agency.
-- Instead it is waiting for:
--
--  * a result
--
data ClientStQuerying block point query m a result = ClientStQuerying {
      recvMsgResult :: result -> m (ClientStAcquired block point query m a)
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
    goIdle (SendMsgAcquire tgt k) =
      SendMsgAcquire (fpoint <$> tgt) (goAcquiring k)

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
    goAcquired (SendMsgReAcquire tgt k) = SendMsgReAcquire (fpoint <$> tgt) (goAcquiring k)
    goAcquired (SendMsgRelease       k) = SendMsgRelease (fmap goIdle k)

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
  -> Client (LocalStateQuery block point query) StIdle State m a
localStateQueryClientPeer (LocalStateQueryClient handler) =
    Effect $ handleStIdle <$> handler
  where
    handleStIdle
      :: ClientStIdle block point query m a
      -> Client (LocalStateQuery block point query) StIdle State m a
    handleStIdle req = case req of
      SendMsgAcquire tgt stAcquiring ->
        Yield StateIdle StateAcquiring
              (MsgAcquire tgt)
              (handleStAcquiring stAcquiring)
      SendMsgDone a ->
        Yield StateIdle StateDone MsgDone (Done a)

    handleStAcquiring
      :: ClientStAcquiring block point query m a
      -> Client (LocalStateQuery block point query) StAcquiring State m a
    handleStAcquiring ClientStAcquiring{recvMsgAcquired, recvMsgFailure} =
      Await $ \_ req -> case req of
        MsgAcquired        -> ( Effect $ handleStAcquired <$> recvMsgAcquired
                              , StateAcquired
                              )
        MsgFailure failure -> ( Effect $ handleStIdle <$> recvMsgFailure failure
                              , StateIdle
                              )

    handleStAcquired
      :: ClientStAcquired block point query m a
      -> Client (LocalStateQuery block point query) StAcquired State m a
    handleStAcquired req = case req of
      SendMsgQuery query stQuerying ->
        Yield StateAcquired (StateQuerying query)
              (MsgQuery query)
              (handleStQuerying query stQuerying)
      SendMsgReAcquire tgt stAcquiring ->
        Yield StateAcquired StateAcquiring
              (MsgReAcquire tgt)
              (handleStAcquiring stAcquiring)
      SendMsgRelease stIdle ->
        Yield StateAcquired StateIdle
              MsgRelease
              (Effect (handleStIdle <$> stIdle))

    handleStQuerying
      :: query result
      -> ClientStQuerying block point query m a result
      -> Client (LocalStateQuery block point query) (StQuerying result) State m a
    handleStQuerying _ ClientStQuerying{recvMsgResult} =
      Await $ \_ req -> case req of
        MsgResult result -> ( Effect (handleStAcquired <$> recvMsgResult result)
                            , StateAcquired
                            )
