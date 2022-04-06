{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.PingPong.Codec where

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.PingPong.Type


codecPingPong
  :: forall m. Monad m
  => Codec PingPong CodecFailure m String
codecPingPong =
    Codec{encode, decode}
  where
    encode :: forall pr (st :: PingPong) (st' :: PingPong)
           .  PeerHasAgency pr st
           -> Message PingPong st st'
           -> String
    encode (ClientAgency TokIdle) MsgPing = "ping\n"
    encode (ClientAgency TokIdle) MsgDone = "done\n"
    encode (ServerAgency TokBusy) MsgPong = "pong\n"

    decode :: forall pr (st :: PingPong)
           .  PeerHasAgency pr st
           -> m (DecodeStep String CodecFailure m (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, str) of
          (ServerAgency TokBusy, "pong") -> DecodeDone (SomeMessage MsgPong) trailing
          (ClientAgency TokIdle, "ping") -> DecodeDone (SomeMessage MsgPing) trailing
          (ClientAgency TokIdle, "done") -> DecodeDone (SomeMessage MsgDone) trailing

          (ServerAgency _      , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)
          (ClientAgency _      , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected client message: " ++ str)


decodeTerminatedFrame :: forall m a.
                         Monad m
                      => Char
                      -> (String -> Maybe String -> DecodeStep String CodecFailure m a)
                      -> m (DecodeStep String CodecFailure m a)
decodeTerminatedFrame terminator k = go []
  where
    go :: [String] -> m (DecodeStep String CodecFailure m a)
    go chunks =
      return $ DecodePartial $ \mchunk ->
        case mchunk of
          Nothing    -> return $ DecodeFail CodecFailureOutOfInput
          Just chunk ->
            case break (==terminator) chunk of
              (c, _:c') -> return $ k (concat (reverse (c:chunks)))
                                      (if null c' then Nothing else Just c)
              _         -> go (chunk : chunks)



codecPingPongId
  :: forall m. Monad m
  => Codec PingPong CodecFailure m (AnyMessage PingPong)
codecPingPongId =
    Codec{encode,decode}
  where
    encode :: forall pr (st :: PingPong) (st' :: PingPong)
           .  PeerHasAgency pr st
           -> Message PingPong st st'
           -> AnyMessage PingPong
    encode _ msg = AnyMessage msg

    decode :: forall pr (st :: PingPong)
           .  PeerHasAgency pr st
           -> m (DecodeStep (AnyMessage PingPong) CodecFailure m (SomeMessage st))
    decode stok =
      pure $ DecodePartial $ \mb ->
        case mb of
          Nothing -> return $ DecodeFail (CodecFailure "expected more data")
          Just (AnyMessage msg) -> return $ 
            case (stok, msg) of
              (ServerAgency TokBusy, MsgPong) ->
                DecodeDone (SomeMessage msg) Nothing
              (ClientAgency TokIdle, MsgPing) ->
                DecodeDone (SomeMessage msg) Nothing
              (ClientAgency TokIdle, MsgDone) ->
                DecodeDone (SomeMessage msg) Nothing
              (ServerAgency _      , _     ) -> DecodeFail failure
                where failure = CodecFailure ("unexpected server message: " ++ show msg)
              (ClientAgency _      , _     ) -> DecodeFail failure
                where failure = CodecFailure ("unexpected client message: " ++ show msg )
