{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.PingPong.Codec where

import Data.Text (pack)

import Protocol.Codec

import Protocol.PingPong.Type

pingPongCodec :: Monad m => Codec m String String PingPongMessage 'StIdle
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
pingPongCodecIdle :: Monad m => Codec m String String PingPongMessage 'StIdle
pingPongCodecIdle = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgPing -> Encoded "ping" pingPongCodecBusy
      MsgDone -> Encoded "done" pingPongCodecDone
  , decode = decodeIdle ""
  }
  where
  decodeIdle acc = Decoder $ pure $ Partial $ \mStr -> case mStr of
    Nothing -> Decoder $ pure $ Fail (Just acc) (pack "expected ping or done")
    Just str ->
      if length acc + length str < 4
      then decodeIdle (acc ++ str)
      else case take 4 (acc ++ str) of
        "ping" -> Decoder $ pure $ Done (Just (drop 4 (acc ++ str))) (Decoded MsgPing pingPongCodecBusy)
        "done" -> Decoder $ pure $ Done (Just (drop 4 (acc ++ str))) (Decoded MsgDone pingPongCodecDone)
        _      -> Decoder $ pure $ Fail (Just (drop 4 (acc ++ str))) (pack "expected ping")

pingPongCodecBusy :: Monad m => Codec m String String PingPongMessage 'StBusy
pingPongCodecBusy = Codec
  { encode = Encoder $ \tr -> case tr of
      MsgPong -> Encoded "pong" pingPongCodecIdle
  , decode = decodePong ""
  }
  where
  decodePong acc = Decoder $ pure $ Partial $ \mStr -> case mStr of
    Nothing -> Decoder $ pure $ Fail (Just acc) (pack "expected pong")
    Just str ->
      if length acc + length str < 4
      then decodePong (acc ++ str)
      else case take 4 (acc ++ str) of
        "pong" -> Decoder $ pure $ Done (Just (drop 4 (acc ++ str))) (Decoded MsgPong pingPongCodecIdle)
        _      -> Decoder $ pure $ Fail (Just (drop 4 (acc ++ str))) (pack "expected pong")

pingPongCodecDone :: Monad m => Codec m String String PingPongMessage 'StDone
pingPongCodecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Decoder $ pure $ Fail Nothing (pack "done")
  }
