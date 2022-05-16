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
    encode :: forall (pr  :: PeerRole)
                     (st  :: ReqResp req resp)
                     (st' :: ReqResp req resp)
           .  PeerHasAgency pr st
           -> Message (ReqResp req resp) st st'
           -> String
    encode (ClientAgency TokIdle) msg = show msg ++ "\n"
    encode (ServerAgency TokBusy) msg = show msg ++ "\n"

    decode :: forall (pr :: PeerRole)
                     (st :: ReqResp req resp)
           .  PeerHasAgency pr st
           -> m (DecodeStep String CodecFailure m (SomeMessage st))
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


codecReqRespId ::
    forall req resp m
  . (Monad m, Show req, Show resp)
  => Codec (ReqResp req resp) CodecFailure m (AnyMessage (ReqResp req resp))
codecReqRespId =
    Codec{encode, decode}
  where
    encode :: forall (pr  :: PeerRole)
                     (st  :: ReqResp req resp)
                     (st' :: ReqResp req resp)
           .  PeerHasAgency pr st
           -> Message (ReqResp req resp) st st'
           -> AnyMessage (ReqResp req resp)
    encode _ msg = AnyMessage msg

    decode :: forall (pr :: PeerRole)
                     (st :: ReqResp req resp)
           .  PeerHasAgency pr st
           -> m (DecodeStep (AnyMessage (ReqResp req resp)) CodecFailure m (SomeMessage st))
    decode stok =
      pure $ DecodePartial $ \mb ->
        case mb of
          Nothing -> return $ DecodeFail (CodecFailure "expected more data")
          Just (AnyMessage msg) -> return $
            case (stok, msg) of
              (ClientAgency TokIdle, MsgReq{})
                -> DecodeDone (SomeMessage msg) Nothing
              (ClientAgency TokIdle, MsgDone)
                -> DecodeDone (SomeMessage msg) Nothing
              (ServerAgency TokBusy, MsgResp{})
                -> DecodeDone (SomeMessage msg) Nothing

              (ServerAgency _      , _     ) -> DecodeFail failure
                where failure = CodecFailure ("unexpected server message: " ++ show msg)
              (ClientAgency _      , _     ) -> DecodeFail failure
                where failure = CodecFailure ("unexpected client message: " ++ show msg)

