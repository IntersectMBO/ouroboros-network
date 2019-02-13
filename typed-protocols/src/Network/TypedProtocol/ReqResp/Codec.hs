{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.TypedProtocol.ReqResp.Codec where

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.ReqResp.Type

import           Text.Read (readMaybe)


codecReqResp
  :: forall req resp m.
     (Monad m, Show req, Show resp, Read req, Read resp)
  => Codec (ReqResp req resp) String m String
codecReqResp =
    Codec{encode, decode}
  where
    encode :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (ReqResp req resp) st st'
           -> String
    encode (ClientAgency TokIdle) msg = show msg
    encode (ServerAgency TokBusy) msg = show msg

    decode :: forall (pr :: PeerRole) st.
              PeerHasAgency pr st
           -> m (DecodeStep String String m (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, break (==' ') str) of
          (ClientAgency TokIdle, ("Req", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgReq resp)) trailing
          (ClientAgency TokIdle, ("Done", ""))
            -> DecodeDone (SomeMessage MsgDone) trailing
          (ServerAgency TokBusy, ("Resp", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgResp resp)) trailing
          _ -> DecodeFail ("unexpected message: " ++ str)


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

