{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}


module Network.TypedProtocol.ReqResp.Type where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Proofs



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


deriving instance (Show req, Show resp)
               => Show (Message (ReqResp req resp) from to)


reqRespAgencyProofs :: AgencyProofs (ReqResp req resp)
reqRespAgencyProofs = AgencyProofs {
    proofByContradiction_ClientAndServerHaveAgency = \TokIdle tok -> case tok of {},
    proofByContradiction_NobodyAndClientHaveAgency = \TokDone tok -> case tok of {},
    proofByContradiction_NobodyAndServerHaveAgency = \TokDone tok -> case tok of {}
  }

