{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}

module Protocol.PingPong.Codec where

import Protocol.Codec

import Protocol.PingPong.Type

pingPongCodec :: Monad m => Codec m String String String PingPongMessage StIdle
pingPongCodec = pingPongCodecIdle

-- | Here is a complete codec for the ping/pong protocol at 'StIdle.
-- The decoder is inverse to the encoder. Codecs at other states are allowed
-- to overlap in their encodings. The encoding of MsgPong, for instance, could
-- be "ping", and there would be no problem, for a user of the ping/pong
-- protocol always knows which state it's at... well, that's true if
--
--   for every transition constructor, if the initial state is fully specialised
--   (monomorphic) then so is the terminal state.
--
-- so we can't have for instance
--
--   TrBad :: Transition 'State anything
--
-- but we can have
--
--   TrGood :: Transition ('Idle param) ('Busy param)
--
--
pingPongCodecIdle :: Monad m => Codec m String String String PingPongMessage StIdle
pingPongCodecIdle = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgPing -> Encoded "ping" pingPongCodecBusy
      MsgDone -> Encoded "done" pingPongCodecDone
  , decode = decodeIdle ""
  }
  where
  decodeIdle acc = Fold $ pure $ Partial $ Response
    { end  = pure $ Left "expected ping or done"
    , more = \strs ->
        let str = mconcat strs
        in  if length acc + length str < 4
            then decodeIdle (acc ++ str)
            else Fold $ pure $ Complete [drop 4 (acc ++ str)] $ pure $ case take 4 (acc ++ str) of
              "ping" -> Right $ Decoded MsgPing pingPongCodecBusy
              "done" -> Right $ Decoded MsgDone pingPongCodecDone
              _      -> Left    "expected ping"
    }

pingPongCodecBusy :: Monad m => Codec m String String String PingPongMessage StBusy
pingPongCodecBusy = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgPong -> Encoded "pong" pingPongCodecIdle
  , decode = decodePong ""
  }
  where
  decodePong acc = Fold $ pure $ Partial $ Response
    { end  = pure $ Left "expected pong"
    , more = \strs ->
        let str = mconcat strs
        in if length acc + length str < 4
           then decodePong (acc ++ str)
           else Fold $ pure $ Complete [drop 4 (acc ++ str)] $ pure $ case take 4 (acc ++ str) of
             "pong" -> Right $ Decoded MsgPong pingPongCodecIdle
             _      -> Left    "expected pong"
    }

pingPongCodecDone :: Monad m => Codec m String String String PingPongMessage StDone
pingPongCodecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Fold $ pure $ Complete [] $ pure $ Left "done"
  }
