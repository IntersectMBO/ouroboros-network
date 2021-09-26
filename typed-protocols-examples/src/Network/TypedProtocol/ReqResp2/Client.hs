{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}



module Network.TypedProtocol.ReqResp2.Client where

import           Data.Singletons

import           Network.TypedProtocol.ReqResp2.Type

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client



data F st st' where
    F :: !(Sing st)
      -> !(ReflRelativeAgency (StateAgency st)
                              TheyHaveAgency
                             (Relative AsClient (StateAgency st)))
          -- proof that in the state @st@ the server has agency
          -- (it makes @collect@ below a total function)
      ->  F st StIdle


type SingQueue q = SingQueueF F q


reqResp2Client :: forall req resp m.
                  ()
               => [Either req req]
               -> Client (ReqResp2 req resp) 'Pipelined Empty StIdle m [Either resp resp]
reqResp2Client = send SingEmptyF
  where
    -- pipeline all the requests, either through `MsgReq` or `MsgReq'`.
    send :: forall (q :: Queue (ReqResp2 req resp)).
            SingQueue q      -- queue singleton
         -> [Either req req] -- requests to send
         -> Client (ReqResp2 req resp) 'Pipelined q StIdle m [Either resp resp]

    send !q (Left req : reqs) =
      YieldPipelined (MsgReq  req) (send (q |>
                                          F (sing @StBusy) ReflServerAgency) reqs)

    send !q (Right req : reqs) =
      YieldPipelined (MsgReq' req) (send (q |>
                                          F (sing @StBusy') ReflServerAgency) reqs)

    send !q [] = collect q []


    -- collect all the responses
    collect :: SingQueue q        -- queue singleton
            -> [Either resp resp] -- all the responses received so far
            -> Client (ReqResp2 req resp) 'Pipelined q StIdle m [Either resp resp]

    collect SingEmptyF !resps = Yield MsgDone (Done (reverse resps))

    collect (SingConsF (F SingBusy ReflServerAgency) q)  !resps =
      Collect Nothing $ \(MsgResp resp)  ->
        CollectDone (collect q (Left  resp : resps))

    collect (SingConsF (F SingBusy' ReflServerAgency) q) !resps =
      Collect Nothing $ \(MsgResp' resp) ->
        CollectDone (collect q (Right resp : resps))
