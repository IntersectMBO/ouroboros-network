{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Network.TypedProtocol.ReqResp.Codec where

import           Data.Singletons

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
                     (st  :: ReqResp req' resp')
                     (st' :: ReqResp req' resp')
           .  ( Show (Message (ReqResp req' resp') st st') )
           => Message (ReqResp req' resp') st st'
           -> String
    encode msg = show msg ++ "\n"

    decode :: forall req' resp' m'
                     (st :: ReqResp req' resp')
           .  (Monad m', SingI st, Read req', Read resp')
           => m' (DecodeStep String CodecFailure m' (SomeMessage st))
    decode =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (sing :: Sing st, break (==' ') str) of
          (SingIdle, ("MsgReq", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgReq resp)) trailing
          (SingIdle, ("MsgDone", ""))
            -> DecodeDone (SomeMessage MsgDone) trailing
          (SingBusy, ("MsgResp", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgResp resp)) trailing

          (_       , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)


codecReqRespId ::
    forall req resp m
  . (Monad m, Show req, Show resp)
  => Codec (ReqResp req resp) CodecFailure m (AnyMessage (ReqResp req resp))
codecReqRespId =
    Codec{encode, decode}
  where
    encode :: forall (st  :: ReqResp req resp)
                     (st' :: ReqResp req resp)
           .  SingI st
           => Message (ReqResp req resp) st st'
           -> AnyMessage (ReqResp req resp)
    encode msg = AnyMessage msg

    decode :: forall (st :: ReqResp req resp)
           .  SingI st
           => m (DecodeStep (AnyMessage (ReqResp req resp)) CodecFailure m (SomeMessage st))
    decode =
      let stok :: Sing st
          stok = sing in
      pure $ DecodePartial $ \mb ->
        case mb of
          Nothing -> return $ DecodeFail (CodecFailure "expected more data")
          Just (AnyMessage msg) -> return $
            case (stok, msg) of
              (SingIdle, MsgReq{})
                -> DecodeDone (SomeMessage msg) Nothing
              (SingIdle, MsgDone)
                -> DecodeDone (SomeMessage msg) Nothing
              (SingBusy, MsgResp{})
                -> DecodeDone (SomeMessage msg) Nothing

              (_      , _     ) -> DecodeFail failure
                where failure = CodecFailure ("unexpected server message: " ++ show msg)

