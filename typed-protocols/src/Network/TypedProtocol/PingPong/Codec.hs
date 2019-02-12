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
    encode :: PeerHasAgency pk st
           -> Message PingPong st st'
           -> String
    encode (ClientAgency TokIdle) MsgPing = "ping\n"
    encode (ClientAgency TokIdle) MsgDone = "done\n"
    encode (ServerAgency TokBusy) MsgPong = "pong\n"

    decode :: PeerHasAgency pk st
           -> m (DecodeStep String String m (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, str) of
          (ServerAgency TokBusy, "pong") -> DecodeDone (SomeMessage MsgPong) trailing
          (ClientAgency TokIdle, "ping") -> DecodeDone (SomeMessage MsgPing) trailing
          (ClientAgency TokIdle, "done") -> DecodeDone (SomeMessage MsgDone) trailing
          _                              -> DecodeFail ("unexpected message: " ++ str)


decodeFrameOfLength :: forall m a.
                       Monad m
                    => Int
                    -> (String -> Maybe String -> DecodeStep String String m a)
                    -> m (DecodeStep String String m a)
decodeFrameOfLength n k = go [] n
  where
    go :: [String] -> Int -> m (DecodeStep String String m a)
    go chunks required =
      return $ DecodePartial $ \mchunk ->
        case mchunk of
          Nothing -> return $ DecodeFail "not enough input"
          Just chunk
            | length chunk >= required
           -> let (c,c') = splitAt required chunk in
              return $ k (concat (reverse (c:chunks)))
                         (if null c' then Nothing else Just c)

            | otherwise
           -> go (chunk : chunks) (required - length chunk)

{-
prop_decodeStepSplitAt n = do
    chan <- fixedInputChannel ["ping", "pong"]
    runDecoder chan Nothing decoder
  where
    decoder = decodeStepSplitAt n Done)
-}

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

