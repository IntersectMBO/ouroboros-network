{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}

module Network.TypedProtocol.ReqResp.Examples where

import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Client

import           Network.TypedProtocol.Pipelined

-- | An example request\/response client which ignores received responses.
--
reqRespClient :: Monad m
              => [req]
              -> ReqRespClient req resp m ()
reqRespClient = go
  where
    go []         = SendMsgDone (pure ())
    go (req:reqs) = SendMsgReq req (\_resp -> return (go reqs))


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
      recvMsgDone = pure acc
    }


-- | An example request\/response client that sends the given list of requests
-- and collects the list of responses.
--
reqRespClientMap :: Monad m
                 => [req]
                 -> ReqRespClient req resp m [resp]
reqRespClientMap = go []
  where
    go resps []         = SendMsgDone (pure $ reverse resps)
    go resps (req:reqs) =
      SendMsgReq req $ \resp ->
      return (go (resp:resps) reqs)

--
-- Pipelined example
--

-- | An example request\/response client that sends the given list of requests
-- and collects the list of responses.
--
-- It is pipelined and tries to collect any replies if they are available.
-- This allows pipelining but keeps it to a minimum, and correspondingly it
-- gives maximum choice to the environment (drivers).
--
-- In theory, with enough and large enough requests and responses, this should
-- be able to saturate any channel of any bandwidth and latency, because it
-- should be able to have both peers send essentially continuously.
--
reqRespClientMapPipelined :: forall req resp m.
                             Monad m
                          => [req]
                          -> ReqRespClientPipelined req resp m [resp]
reqRespClientMapPipelined reqs0 =
    ReqRespClientPipelined (go [] Zero reqs0)
  where
    go :: [resp] -> Nat o -> [req] -> ReqRespSender req resp o resp m [resp]
    go resps Zero reqs =
      case reqs of
        []        -> SendMsgDonePipelined (reverse resps)
        req:reqs' -> sendReq resps Zero req reqs'

    go resps (Succ o) reqs =
      CollectPipelined
        (case reqs of
           []        -> Nothing
           req:reqs' -> Just (sendReq resps (Succ o) req reqs'))
        (\resp -> go (resp:resps) o reqs)

    sendReq :: [resp] -> Nat o -> req -> [req]
            -> ReqRespSender req resp o resp m [resp]
    sendReq resps o req reqs' =
      SendMsgReqPipelined req
        (\resp -> return resp)
        (go resps (Succ o) reqs')
