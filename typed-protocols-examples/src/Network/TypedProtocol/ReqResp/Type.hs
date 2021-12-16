{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Network.TypedProtocol.ReqResp.Type where

import           Network.TypedProtocol.Core


data ReqResp req resp where
  StIdle :: ReqResp req resp
  StBusy :: ReqResp req resp
  StDone :: ReqResp req resp

instance Protocol (ReqResp req resp) where

  data Message (ReqResp req resp) from to where
    MsgReq  :: req  -> Message (ReqResp req resp) StIdle StBusy
    MsgResp :: resp -> Message (ReqResp req resp) StBusy StIdle
    MsgDone ::         Message (ReqResp req resp) StIdle StDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokBusy :: ServerHasAgency StBusy

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


deriving instance (Show req, Show resp)
               => Show (Message (ReqResp req resp) from to)

deriving instance (Eq req, Eq resp)
               => Eq (Message (ReqResp req resp) from to)

instance Show (ClientHasAgency (st :: ReqResp req resp)) where
    show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: ReqResp req resp)) where
    show TokBusy = "TokBusy"
