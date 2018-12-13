{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Network.Framing where

import           Data.ByteString (ByteString)

import           Ouroboros.Network.ByteChannel as ByteChannel
import           Ouroboros.Network.MsgChannel  as MsgChannel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.MonadClass.MonadST



-- TODO: add example framing, trivial, or Karl's format, or CBOR equivalent

data Frame = Frame ConversationId ByteString -- embedded message body
type ConversationId = Int

framingCodec1, framingCodec2 :: MonadST m => Codec ByteString () m Frame
framingCodec1 = undefined
framingCodec2 = undefined


data DecodeState bytes = Initial | Trailing bytes | Ongoing

applyFraming :: forall bytes failure m msg. 
                Monad m
             => Codec bytes failure m msg
             -> ByteChannel bytes m
             -> MsgChannel failure m msg
applyFraming Codec {encode, decode, chunkNull} =
    msgChannel Initial
  where
    msgChannel :: DecodeState bytes -> ByteChannel bytes m -> MsgChannel failure m msg
    msgChannel decodeState channel =
      MsgChannel {
        send = send decodeState channel,
        recv = recv decodeState channel =<< decode
      }

    send decodeState channel msg =
      msgChannel decodeState <$> write channel (encode msg)

    recv :: DecodeState bytes
         -> ByteChannel bytes m
         -> DecodeStep bytes failure m msg
         -> m (RecvResult failure msg (MsgChannel failure m msg))

    recv _ _ (Fail failure) =
      return (RecvFailure failure)

    recv _ channel (Done msg trailing') =
      return (RecvMsg msg (msgChannel decodeState' channel))
      where
        decodeState' | chunkNull trailing' = Initial
                     | otherwise           = Trailing trailing'


    recv (Trailing trailing) channel (Partial k) =
      k (Just trailing) >>= recv Ongoing channel

    recv Initial channel (Partial k) =
      ByteChannel.read channel >>= \res -> case res of
        ByteChannel.ChannelClosed            -> return MsgChannel.ChannelClosed
        ByteChannel.ReadChunk chunk channel' -> k (Just chunk) >>= recv Ongoing channel'

    recv Ongoing channel (Partial k) =
      ByteChannel.read channel >>= \res -> case res of
        ByteChannel.ChannelClosed            -> k Nothing      >>= recv Ongoing channel
        ByteChannel.ReadChunk chunk channel' -> k (Just chunk) >>= recv Ongoing channel'

