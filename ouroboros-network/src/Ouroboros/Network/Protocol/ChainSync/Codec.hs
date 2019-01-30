{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Ouroboros.Network.Protocol.ChainSync.Codec
  ( codecChainSync
  ) where

import           Control.Monad.Class.MonadST

import           Network.TypedProtocol.Codec
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type


import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder)

import Data.ByteString.Lazy (ByteString)
import Codec.CBOR.Encoding (encodeListLen, encodeWord)
import Codec.CBOR.Decoding (decodeListLen, decodeWord)
import Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as CBOR


codecChainSync :: forall header point m.
                  (MonadST m, Serialise header, Serialise point)
               => Codec (ChainSync header point)
                        CBOR.DeserialiseFailure m ByteString
codecChainSync =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall (pr :: PeerRole) (st :: ChainSync header point) (st' :: ChainSync header point).
              PeerHasAgency pr st
           -> Message (ChainSync header point) st st'
           -> CBOR.Encoding

    encode (ClientAgency TokIdle) MsgRequestNext =
      encodeListLen 1 <> encodeWord 0

    encode (ServerAgency TokNext{}) MsgAwaitReply =
      encodeListLen 1 <> encodeWord 1

    encode (ServerAgency TokNext{}) (MsgRollForward h p) =
      encodeListLen 3 <> encodeWord 2 <> CBOR.encode h <> CBOR.encode p

    encode (ServerAgency TokNext{}) (MsgRollBackward p1 p2) =
      encodeListLen 3 <> encodeWord 3 <> CBOR.encode p1 <> CBOR.encode p2

    encode (ClientAgency TokIdle) (MsgFindIntersect ps) =
      encodeListLen 2 <> encodeWord 4 <> CBOR.encode ps

    encode (ServerAgency TokIntersect) (MsgIntersectImproved p1 p2) =
      encodeListLen 3 <> encodeWord 5 <> CBOR.encode p1 <> CBOR.encode p2

    encode (ServerAgency TokIntersect) (MsgIntersectUnchanged p) =
      encodeListLen 2 <> encodeWord 6 <> CBOR.encode p

    encode (ClientAgency TokIdle) MsgDone =
      encodeListLen 1 <> encodeWord 7

    decode :: forall (pr :: PeerRole) (st :: ChainSync header point) s.
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- decodeListLen
      key <- decodeWord
      case (key, len, stok) of
        (0, 1, ClientAgency TokIdle) ->
          return (SomeMessage MsgRequestNext)

        (1, 1, ServerAgency (TokNext TokCanAwait)) ->
          return (SomeMessage MsgAwaitReply)

        (2, 3, ServerAgency (TokNext _)) -> do
          h <- CBOR.decode
          p <- CBOR.decode
          return (SomeMessage (MsgRollForward h p))

        (3, 3, ServerAgency (TokNext _)) -> do
          p1 <- CBOR.decode
          p2 <- CBOR.decode
          return (SomeMessage (MsgRollBackward p1 p2))

        (4, 2, ClientAgency TokIdle) -> do
          ps <- CBOR.decode
          return (SomeMessage (MsgFindIntersect ps))

        (5, 3, ServerAgency TokIntersect) -> do
          p1 <- CBOR.decode
          p2 <- CBOR.decode
          return (SomeMessage (MsgIntersectImproved p1 p2))

        (6, 2, ServerAgency TokIntersect) -> do
          p <- CBOR.decode
          return (SomeMessage (MsgIntersectUnchanged p))

        (7, 1, ClientAgency TokIdle) ->
          return (SomeMessage MsgDone)

        _ -> fail ("codecChainSync: unexpected key " ++ show (key, len))

