{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Network.TypedProtocol.ReqResp.Type where

import           Data.Singletons

import           Network.TypedProtocol.Core


data ReqResp req resp where
  StIdle :: ReqResp req resp
  StBusy :: ReqResp req resp
  StDone :: ReqResp req resp

data SReqResp (st :: ReqResp req resp) where
    SingIdle :: SReqResp StIdle
    SingBusy :: SReqResp StBusy
    SingDone :: SReqResp StDone

deriving instance Show (SReqResp st)

type instance Sing = SReqResp
instance SingI StIdle where
    sing = SingIdle
instance SingI StBusy where
    sing = SingBusy
instance SingI StDone where
    sing = SingDone


instance Protocol (ReqResp req resp) where

  data Message (ReqResp req resp) from to where
    MsgReq  :: req  -> Message (ReqResp req resp) StIdle StBusy
    MsgResp :: resp -> Message (ReqResp req resp) StBusy StIdle
    MsgDone ::         Message (ReqResp req resp) StIdle StDone

  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StDone = NobodyAgency


deriving instance (Show req, Show resp)
               => Show (Message (ReqResp req resp) from to)

deriving instance (Eq req, Eq resp)
               => Eq (Message (ReqResp req resp) from to)
