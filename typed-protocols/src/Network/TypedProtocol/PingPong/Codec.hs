{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.TypedProtocol.PingPong.Codec where

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.PingPong.Type


codecPingPong
  :: forall m. Monad m
  => Codec PingPong String m String
codecPingPong =
    Codec{encode, decode}
  where
    encode :: PeerHasAgency pr st
           -> Message PingPong st st'
           -> String
    encode (ClientAgency TokIdle) MsgPing = "ping\n"
    encode (ClientAgency TokIdle) MsgDone = "done\n"
    encode (ServerAgency TokBusy) MsgPong = "pong\n"

    decode :: PeerHasAgency pr st
           -> m (DecodeStep String String m (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, str) of
          (ServerAgency TokBusy, "pong") -> DecodeDone (SomeMessage MsgPong) trailing
          (ClientAgency TokIdle, "ping") -> DecodeDone (SomeMessage MsgPing) trailing
          (ClientAgency TokIdle, "done") -> DecodeDone (SomeMessage MsgDone) trailing
          _                              -> DecodeFail ("unexpected message: " ++ str)


decodeTerminatedFrame :: forall m a.
                         Monad m
                      => Char
                      -> (String -> Maybe String -> DecodeStep String String m a)
                      -> m (DecodeStep String String m a)
decodeTerminatedFrame terminator k = go []
  where
    go :: [String] -> m (DecodeStep String String m a)
    go chunks =
      return $ DecodePartial $ \mchunk ->
        case mchunk of
          Nothing    -> return $ DecodeFail "not enough input"
          Just chunk ->
            case break (==terminator) chunk of
              (c, _:c') -> return $ k (concat (reverse (c:chunks)))
                                      (if null c' then Nothing else Just c)
              _         -> go (chunk : chunks)

