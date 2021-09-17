{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
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

import           Network.TypedProtocol.Core hiding (SingQueue (..))
import           Network.TypedProtocol.Peer.Client


-- | A singleton for @'Queue' ('ReqResp2' req resp)@ used by the
-- 'reqResp2Client'.
--
data SingQueue (q :: Queue (ReqResp2 req resp)) where

    SingEmpty :: SingQueue Empty

    SingCons  :: !(Sing st)                                                -- singleton for the state @st@
              -> !(ReflRelativeAgency (StateAgency st)                     -- proof that in the state @st@ the server has agency
                                      TheyHaveAgency                       -- (it makes @collect@ below a total function)
                                     (Relative AsClient (StateAgency st)))
              -> !(SingQueue q)
              -> SingQueue (Tr st StIdle <| q)


snoc :: SingQueue q
     -> Sing st
     -> (ReflRelativeAgency (StateAgency st)
                             TheyHaveAgency
                            (Relative AsClient (StateAgency st)))
     -> SingQueue (q |> Tr st StIdle)

snoc  SingEmpty              st refl = SingCons st  refl   SingEmpty

snoc (SingCons  st' refl' q) st refl = SingCons st' refl' (snoc q st refl)


reqResp2Client :: forall req resp m.
                  ()
               => [Either req req]
               -> Client (ReqResp2 req resp) 'Pipelined Empty StIdle m [Either resp resp]
reqResp2Client = send SingEmpty
  where
    -- pipeline all the requests, either through `MsgReq` or `MsgReq'`.
    send :: forall (q :: Queue (ReqResp2 req resp)).
            SingQueue q      -- queue singleton
         -> [Either req req] -- requests to send
         -> Client (ReqResp2 req resp) 'Pipelined q StIdle m [Either resp resp]

    send !q (Left req : reqs) =
      YieldPipelined (MsgReq  req) (send (snoc q (sing @StBusy)  ReflServerAgency) reqs)

    send !q (Right req : reqs) =
      YieldPipelined (MsgReq' req) (send (snoc q (sing @StBusy') ReflServerAgency) reqs)

    send !q [] = collect q []


    -- collect all the responses
    collect :: SingQueue q        -- queue singleton
            -> [Either resp resp] -- all the responses received so far
            -> Client (ReqResp2 req resp) 'Pipelined q StIdle m [Either resp resp]

    collect SingEmpty !resps = Yield MsgDone (Done (reverse resps))

    collect (SingCons SingBusy ReflServerAgency q)  !resps =
      Collect Nothing $ \(MsgResp resp)  ->
        CollectDone (collect q (Left  resp : resps))

    collect (SingCons SingBusy' ReflServerAgency q) !resps =
      Collect Nothing $ \(MsgResp' resp) ->
        CollectDone (collect q (Right resp : resps))



