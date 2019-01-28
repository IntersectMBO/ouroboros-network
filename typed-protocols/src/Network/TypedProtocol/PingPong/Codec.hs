{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.TypedProtocol.PingPong.Codec where

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.PingPong.Type

codecPingPongAsClient
  :: forall m. Monad m
  => Codec PingPong AsClient String m String
codecPingPongAsClient =
    Codec{encode, decode}
  where
   encode :: forall (st  :: PingPong)
                    (st' :: PingPong).
             ClientHasAgency st
          -> Message PingPong st st'
          -> String
   encode TokIdle MsgPing = "ping\n"
   encode TokIdle MsgDone = "done\n"

   decode :: forall (st :: PingPong).
             ServerHasAgency st
          -> m (DecodeStep String String m (SomeMessage st))
   decode TokBusy =
     decodeStepConsumeN 5 $ \str trailing ->
       case str of
         "pong\n" -> Done (SomeMessage MsgPong) trailing
         _        -> Fail ("unexpected message: " ++ str)


codecPingPongAsServer
  :: forall m. Monad m
  => Codec PingPong AsServer String m String
codecPingPongAsServer =
    Codec{encode, decode}
  where
   encode :: forall (st  :: PingPong)
                    (st' :: PingPong).
             ServerHasAgency st
          -> Message PingPong st st'
          -> String
   encode TokBusy MsgPong = "pong\n"

   decode :: forall (st :: PingPong).
             ClientHasAgency st
          -> m (DecodeStep String String m (SomeMessage st))
   decode TokIdle =
     decodeStepConsumeN 5 $ \str trailing ->
       case str of
         "ping\n" -> Done (SomeMessage MsgPing) trailing
         "done\n" -> Done (SomeMessage MsgDone) trailing
         _        -> Fail ("unexpected message: " ++ str)

{-
codecPingPong
  :: forall pk m. Monad m
  => Codec PingPong (pk :: PeerKind) String m String
codecPingPong =
    Codec{encode, decode}
  where
   encode :: forall (st  :: PingPong)
                    (st' :: PingPong).
             WeHaveAgency pk st
          -> Message st st'
          -> String
   encode TokIdle MsgPing = "ping\n"
   encode TokIdle MsgDone = "done\n"
   encode TokBusy MsgPong = "pong\n"

   decode :: forall (st :: PingPong).
             TheyHaveAgency pk st
          -> m (DecodeStep String String m (SomeMessage st))
   decode stok =
     decodeStepConsumeN 5 $ \str trailing ->
       case (stok, str) of
         (TokBusy, "pong\n") -> Done (SomeMessage MsgPong) trailing
         (TokIdle, "ping\n") -> Done (SomeMessage MsgPing) trailing
         (TokIdle, "done\n") -> Done (SomeMessage MsgDone) trailing
         _                   -> Fail ("unexpected message: " ++ str)
-}

decodeStepConsumeN :: forall m a.
                       Monad m
                    => Int
                    -> (String -> Maybe String -> DecodeStep String String m a)
                    -> m (DecodeStep String String m a)
decodeStepConsumeN n k = go [] n
  where
    go :: [String] -> Int -> m (DecodeStep String String m a)
    go chunks required =
      return $ Partial $ \mchunk ->
        case mchunk of
          Nothing -> return $ Fail "not enough input"
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
