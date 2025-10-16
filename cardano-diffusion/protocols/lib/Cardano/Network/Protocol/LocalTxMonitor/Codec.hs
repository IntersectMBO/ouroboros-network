{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Network.Protocol.LocalTxMonitor.Codec
  ( codecLocalTxMonitor
  , Codec.codecLocalTxMonitorId
  ) where

import Control.Monad.Class.MonadST

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy (ByteString)

import Network.TypedProtocol.Codec

import Cardano.Network.NodeToClient.Version

import Ouroboros.Network.Protocol.LocalTxMonitor.Codec qualified as Codec
import Ouroboros.Network.Protocol.LocalTxMonitor.Type

codecLocalTxMonitor ::
     forall txid tx slot m.
     MonadST m
  => NodeToClientVersion
     -- ^ Whether to accept `MsgGetMeasures`
  -> (txid -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s txid)
  -> (tx -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s tx)
  -> (slot -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s slot)
  -> Codec (LocalTxMonitor txid tx slot) CBOR.DeserialiseFailure m ByteString
codecLocalTxMonitor version =
    Codec.codecLocalTxMonitor localTxMonitorVersion
  where
    localTxMonitorVersion
      | version < NodeToClientV_20
      = Codec.LocalTxMonitor_V1
      | otherwise
      = Codec.LocalTxMonitor_V2
