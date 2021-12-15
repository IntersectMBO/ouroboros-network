{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Network.TypedProtocol.ReqResp.Codec where

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.PingPong.Codec (decodeTerminatedFrame)
import           Network.TypedProtocol.ReqResp.Type
import           Text.Read (readMaybe)


codecReqResp ::
    forall req resp m
  . (Monad m, Show req, Show resp, Read req, Read resp)
  => Codec (ReqResp req resp) CodecFailure m String
codecReqResp =
    Codec{encode, decode}
  where
    encode :: forall req' resp'
                     (pr  :: PeerRole)
                     (st  :: ReqResp req' resp')
                     (st' :: ReqResp req' resp')
           . (Show (Message (ReqResp req' resp') st st'))
           => PeerHasAgency pr st
           -> Message (ReqResp req' resp') st st'
           -> String
    encode (ClientAgency TokIdle) msg = show msg ++ "\n"
    encode (ServerAgency TokBusy) msg = show msg ++ "\n"

    decode :: forall req' resp' m'
                     (pr :: PeerRole)
                     (st :: ReqResp req' resp')
           .  (Monad m', Read req', Read resp')
           => PeerHasAgency pr st
           -> m' (DecodeStep String CodecFailure m' (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, break (==' ') str) of
          (ClientAgency TokIdle, ("MsgReq", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgReq resp)) trailing
          (ClientAgency TokIdle, ("MsgDone", ""))
            -> DecodeDone (SomeMessage MsgDone) trailing
          (ServerAgency TokBusy, ("MsgResp", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgResp resp)) trailing

          (ServerAgency _      , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)
          (ClientAgency _      , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected client message: " ++ str)

