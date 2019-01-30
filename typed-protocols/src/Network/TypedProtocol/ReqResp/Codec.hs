{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.TypedProtocol.ReqResp.Codec where

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.ReqResp.Type

import           Text.Read (readMaybe)


codecReqRespAsClient
  :: forall req resp m.
     (Monad m, Show req, Show resp, Read req, Read resp)
  => Codec (ReqResp req resp) AsClient String m String
codecReqRespAsClient =
    Codec{encode, decode}
  where
   encode :: forall (st  :: ReqResp req resp)
                    (st' :: ReqResp req resp).
             ClientHasAgency st
          -> Message (ReqResp req resp) st st'
          -> String
   encode TokIdle msg = show msg

   decode :: forall (st :: ReqResp req resp).
             ServerHasAgency st
          -> m (DecodeStep String String m (SomeMessage st))
   decode TokBusy =
     decodeTerminatedFrame '\n' $ \str trailing ->
       case break (==' ') str of
         ("Resp", str')
            | Just resp <- readMaybe str'
           -> Done (SomeMessage (MsgResp resp)) trailing
         _ -> Fail ("unexpected message: " ++ str)


codecReqRespAsServer
  :: forall req resp m.
     (Monad m, Show req, Show resp, Read req, Read resp)
  => Codec (ReqResp req resp) AsServer String m String
codecReqRespAsServer =
    Codec{encode, decode}
  where
   encode :: forall (st  :: ReqResp req resp)
                    (st' :: ReqResp req resp).
             ServerHasAgency st
          -> Message (ReqResp req resp) st st'
          -> String
   encode TokBusy msg = show msg

   decode :: forall (st :: ReqResp req resp).
             ClientHasAgency st
          -> m (DecodeStep String String m (SomeMessage st))
   decode TokIdle =
     decodeTerminatedFrame '\n' $ \str trailing ->
       case break (==' ') str of
         ("Req", str')
            | Just resp <- readMaybe str'
           -> Done (SomeMessage (MsgReq resp)) trailing
         ("Done", "")
           -> Done (SomeMessage MsgDone) trailing
         _ -> Fail ("unexpected message: " ++ str)


decodeTerminatedFrame :: forall m a.
                         Monad m
                      => Char
                      -> (String -> Maybe String -> DecodeStep String String m a)
                      -> m (DecodeStep String String m a)
decodeTerminatedFrame terminator k = go []
  where
    go :: [String] -> m (DecodeStep String String m a)
    go chunks =
      return $ Partial $ \mchunk ->
        case mchunk of
          Nothing    -> return $ Fail "not enough input"
          Just chunk ->
            case break (==terminator) chunk of
              (c, _:c') -> return $ k (concat (reverse (c:chunks)))
                                      (if null c' then Nothing else Just c)
              _         -> go (chunk : chunks)

