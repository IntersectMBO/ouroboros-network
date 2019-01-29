{-# LANGUAGE BangPatterns        #-}

module Network.TypedProtocol.ReqResp.Examples where

import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Client


-- | A request\/response server instance that computes a 'Data.List.mapAccumL'
-- over the stream of requests.
--
reqRespServerMapAccumL :: Monad m
                       => (acc -> req -> m (acc, resp))
                       -> acc
                       -> ReqRespServer req resp m acc
reqRespServerMapAccumL f !acc =
    ReqRespServer {
      recvMsgReq  = \req -> do (acc', resp) <- f acc req
                               return (resp, reqRespServerMapAccumL f acc'),
      recvMsgDone = acc
    }



-- | An example request\/response client that sends the given list of requests
-- and collects the list of responses.
--
reqRespClientMap :: Monad m
                 => [req]
                 -> ReqRespClient req resp m [resp]
reqRespClientMap = go []
  where
    go resps []         = SendMsgDone (reverse resps)
    go resps (req:reqs) =
      SendMsgReq req $ \resp ->
      return (go (resp:resps) reqs)

