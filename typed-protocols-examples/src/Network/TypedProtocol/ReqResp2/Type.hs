{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Network.TypedProtocol.ReqResp2.Type where

import           Data.Singletons

import           Network.TypedProtocol.Core


data ReqResp2 req resp where
  StIdle  :: ReqResp2 req resp
  StBusy  :: ReqResp2 req resp
  StBusy' :: ReqResp2 req resp
  StDone  :: ReqResp2 req resp

data SReqResp2 (st :: ReqResp2 req resp) where
    SingIdle  :: SReqResp2 StIdle
    SingBusy  :: SReqResp2 StBusy
    SingBusy' :: SReqResp2 StBusy'
    SingDone  :: SReqResp2 StDone

deriving instance Show (SReqResp2 st)

type instance Sing = SReqResp2
instance SingI StIdle where
    sing = SingIdle
instance SingI StBusy where
    sing = SingBusy
instance SingI StBusy' where
    sing = SingBusy'
instance SingI StDone where
    sing = SingDone


instance Protocol (ReqResp2 req resp) where

  data Message (ReqResp2 req resp) from to where
    MsgReq   :: req  -> Message (ReqResp2 req resp) StIdle  StBusy
    MsgResp  :: resp -> Message (ReqResp2 req resp) StBusy  StIdle

    MsgReq'  :: req  -> Message (ReqResp2 req resp) StIdle  StBusy'
    MsgResp' :: resp -> Message (ReqResp2 req resp) StBusy' StIdle

    MsgDone  ::         Message (ReqResp2 req resp) StIdle  StDone

  type StateAgency StIdle  = ClientAgency
  type StateAgency StBusy  = ServerAgency
  type StateAgency StBusy' = ServerAgency
  type StateAgency StDone  = NobodyAgency


deriving instance (Show req, Show resp)
               => Show (Message (ReqResp2 req resp) from to)

deriving instance (Eq req, Eq resp)
               => Eq (Message (ReqResp2 req resp) from to)

