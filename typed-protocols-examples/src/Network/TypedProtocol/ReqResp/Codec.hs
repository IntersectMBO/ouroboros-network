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
import           Network.TypedProtocol.Core
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
           .  (Monad m', SingI (PeerHasAgency st), Read req', Read resp')
           => m' (DecodeStep String CodecFailure m' (SomeMessage st))
    decode =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (sing :: Sing (PeerHasAgency st), break (==' ') str) of
          (SingClientHasAgency SingIdle, ("MsgReq", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgReq resp)) trailing
          (SingClientHasAgency SingIdle, ("MsgDone", ""))
            -> DecodeDone (SomeMessage MsgDone) trailing
          (SingServerHasAgency SingBusy, ("MsgResp", str'))
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
           .  SingI (PeerHasAgency st)
           => Message (ReqResp req resp) st st'
           -> AnyMessage (ReqResp req resp)
    encode msg = AnyMessage msg

    decode :: forall (st :: ReqResp req resp)
           .  SingI (PeerHasAgency st)
           => m (DecodeStep (AnyMessage (ReqResp req resp)) CodecFailure m (SomeMessage st))
    decode =
      let stok :: Sing (PeerHasAgency st)
          stok = sing in
      pure $ DecodePartial $ \mb ->
        case mb of
          Nothing -> return $ DecodeFail (CodecFailure "expected more data")
          Just (AnyMessage msg) -> return $
            case (stok, msg) of
              (SingClientHasAgency SingIdle, MsgReq{})
                -> DecodeDone (SomeMessage msg) Nothing
              (SingClientHasAgency SingIdle, MsgDone)
                -> DecodeDone (SomeMessage msg) Nothing
              (SingServerHasAgency SingBusy, MsgResp{})
                -> DecodeDone (SomeMessage msg) Nothing

              (SingServerHasAgency _      , _     ) -> DecodeFail failure
                where failure = CodecFailure ("unexpected server message: " ++ show msg)
              (SingClientHasAgency _      , _     ) -> DecodeFail failure
                where failure = CodecFailure ("unexpected client message: " ++ show msg)

