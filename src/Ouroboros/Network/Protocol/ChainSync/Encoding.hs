{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ChainSync.Encoding where

import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.Untyped (SomeMessage(..))

import Data.Monoid ((<>))
import Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord)
import Codec.CBOR.Decoding (Decoder, decodeListLen, decodeWord)
import Codec.Serialise.Class (Serialise(..))

encodeChainSyncMessage :: (Serialise header, Serialise point)
                       => SomeMessage (ChainSyncMessage header point)
                       -> Encoding
encodeChainSyncMessage (SomeMessage msg) =
  case msg of
    MsgRequestNext            -> encodeListLen 1 <> encodeWord 0
    MsgAwaitReply             -> encodeListLen 1 <> encodeWord 1
    MsgRollForward       h p  -> encodeListLen 3 <> encodeWord 2 <> encode h <> encode p
    MsgRollBackward      p p' -> encodeListLen 3 <> encodeWord 3 <> encode p <> encode p'
    MsgFindIntersect     ps   -> encodeListLen 2 <> encodeWord 4 <> encode ps
    MsgIntersectImproved p p' -> encodeListLen 3 <> encodeWord 5 <> encode p <> encode p'
    MsgIntersectUnchanged  p  -> encodeListLen 2 <> encodeWord 6 <> encode p

decodeChainSyncMessage :: forall header point s.
                          (Serialise header, Serialise point)
                       => Decoder s (SomeMessage (ChainSyncMessage header point))
decodeChainSyncMessage = do
  len <- decodeListLen
  key <- decodeWord
  case key of
    0 -> return (SomeMessage MsgRequestNext)
    1 -> return (SomeMessage MsgAwaitReply)
    2 -> SomeMessage <$> (MsgRollForward        <$> decode <*> decode
                                          :: Decoder s (ChainSyncMessage header point (StNext StCanAwait) StIdle))
    3 -> SomeMessage <$> (MsgRollBackward       <$> decode <*> decode
                                          :: Decoder s (ChainSyncMessage header point (StNext StCanAwait) StIdle))
    4 -> SomeMessage <$> (MsgFindIntersect      <$> decode)
    5 -> SomeMessage <$> (MsgIntersectImproved  <$> decode <*> decode)
    6 -> SomeMessage <$> (MsgIntersectUnchanged <$> decode)

