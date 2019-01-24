{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.PingPong.Codec where

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.PingPong.Type

pingPongCodec
  :: forall m. Monad m
  => Codec PingPongState String m String
pingPongCodec = Codec encodeMsg decodeMsg
  where
   encodeMsg :: forall (st :: PingPongState) (st' :: PingPongState). Message st st' -> String
   encodeMsg MsgPing = "ping"
   encodeMsg MsgPong = "pong"
   encodeMsg MsgDone = "done"

   decodeMsg :: forall (st :: PingPongState). StateToken st -> m (DecodeStep String String m (SomeMessage st))
   decodeMsg tok = return $ Partial $ \bytes -> case bytes of
     Nothing  -> return $ Fail "not enough input"
     Just bts -> case decodePingPongMessage tok bts of
       Just msg -> return $ Done msg Nothing
       Nothing  -> return $ Fail "wrong input"

decodePingPongMessage :: forall (st :: PingPongState).
                         StateToken st
                      -> String
                      -> Maybe (SomeMessage st)
decodePingPongMessage TokIdle "ping" = Just (SomeMessage MsgPing)
decodePingPongMessage TokIdle "done" = Just (SomeMessage MsgDone)
decodePingPongMessage TokBusy "pong" = Just (SomeMessage MsgPong)
decodePingPongMessage _       _      = Nothing
