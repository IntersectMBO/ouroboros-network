{-# LANGUAGE BangPatterns #-}

-- |
-- req-resp client \/ servers using in mux tests.
--
module Test.Mux.ReqResp where

import           Control.Monad.Class.MonadSTM

import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server


reqRespServerMapAccumL :: MonadSTM m
                       => TMVar m acc
                       -> (acc -> req -> m (acc, resp))
                       -> acc
                       -> ReqRespServer req resp m ()
reqRespServerMapAccumL v f !acc =
    ReqRespServer {
      recvMsgReq  = \req -> do (acc', resp) <- f acc req
                               return (resp, reqRespServerMapAccumL v f acc'),
      recvMsgDone = atomically (putTMVar v acc)
    }

reqRespClientMap :: MonadSTM m
                 => TMVar m [resp]
                 -> [req]
                 -> ReqRespClient req resp m ()
reqRespClientMap v = go []
  where
    go resps []         = SendMsgDone $ do
      atomically $ putTMVar v (reverse resps)
    go resps (req:reqs) =
      SendMsgReq req $ \resp -> do
      return (go (resp:resps) reqs)
