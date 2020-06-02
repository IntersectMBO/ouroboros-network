{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.KeepAlive.Codec
  ( codecKeepAlive
  ) where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)

import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeWord)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeWord)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Type


codecKeepAlive
  :: forall m.
     ( Monad   m
     , MonadST m
     )
  => Codec KeepAlive CBOR.DeserialiseFailure m ByteString
codecKeepAlive = mkCodecCborLazyBS encodeMsg decodeMsg
   where
     encodeMsg :: forall (pr :: PeerRole) st st'.
                  PeerHasAgency pr st
               -> Message KeepAlive st st'
               -> CBOR.Encoding
     encodeMsg (ClientAgency TokClient) MsgKeepAlive         = CBOR.encodeWord 0
     encodeMsg (ServerAgency TokServer) MsgKeepAliveResponse = CBOR.encodeWord 1
     encodeMsg (ClientAgency TokClient) MsgDone              = CBOR.encodeWord 2

     decodeMsg :: forall (pr :: PeerRole) s (st :: KeepAlive).
                  PeerHasAgency pr st
               -> CBOR.Decoder s (SomeMessage st)
     decodeMsg stok = do
       key <- CBOR.decodeWord
       case (stok, key) of
         (ClientAgency TokClient, 0) -> pure (SomeMessage MsgKeepAlive)
         (ServerAgency TokServer, 1) -> pure (SomeMessage MsgKeepAliveResponse)
         (ClientAgency TokClient, 2) -> pure (SomeMessage MsgDone)

         (ClientAgency TokClient, _) ->
           fail ("codecKeepAlive.StClient: unexpected key:" ++ show key)
         (ServerAgency TokServer, _) ->
           fail ("codecKeepAlive.StServer: unexpected key: " ++ show key)
