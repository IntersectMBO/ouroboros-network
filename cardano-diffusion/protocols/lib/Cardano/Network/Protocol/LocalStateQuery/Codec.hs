{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Network.Protocol.LocalStateQuery.Codec
  ( codecLocalStateQuery
  , Codec.codecLocalStateQueryId
  , Codec.Some (..)
  ) where

import Control.Monad.Class.MonadST

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy (ByteString)

import Network.TypedProtocol.Stateful.Codec qualified as Stateful

import Cardano.Network.NodeToClient.Version

import Ouroboros.Network.Protocol.LocalStateQuery.Codec qualified as Codec
import Ouroboros.Network.Protocol.LocalStateQuery.Type

codecLocalStateQuery
  :: forall block point query m.
     ( MonadST m
     , ShowQuery query
     )
  => NodeToClientVersion
     -- ^ eg whether to allow 'ImmutableTip' in @'MsgAcquire'
  -> (point -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s point)
  -> (forall result . query result -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s (Codec.Some query))
  -> (forall result . query result -> result -> CBOR.Encoding)
  -> (forall result . query result -> forall s . CBOR.Decoder s result)
  -> Stateful.Codec (LocalStateQuery block point query) CBOR.DeserialiseFailure State m ByteString
codecLocalStateQuery version =
    Codec.codecLocalStateQuery localStateQueryVersion
  where
    localStateQueryVersion
      | version < NodeToClientV_16
      = Codec.LocalStateQuery_V1
      | otherwise
      = Codec.LocalStateQuery_V2
