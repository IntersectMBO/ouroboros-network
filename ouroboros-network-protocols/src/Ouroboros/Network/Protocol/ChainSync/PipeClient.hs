{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Network.Protocol.ChainSync.PipeClient
where

import Ouroboros.Network.Protocol.ChainSync.Client
import Pipes
import Pipes.Core
import Control.Monad (forever)

data ChainSyncResponse block point tip
  = RollForward block tip
  | RollBackward point tip
  | IntersectNotFound tip
  deriving (Show)

data ChainSyncRequest point
  = RequestIntersect [point]
  | RequestNext
  deriving (Show)

example :: ( Monad m
           , p ~ Proxy X () (ChainSyncRequest point) (ChainSyncResponse block point tip) m
           )
        => [point]
        -> (ChainSyncResponse block point tip -> m ())
        -> (ChainSyncClient block point tip p () -> p ())
        -> m ()
example initialPoints handleMutation runClient =
  runEffect
    $ chainSyncClient initialPoints handleMutation
      <<+ chainSyncServer runClient
  
chainSyncClient :: forall block point tip m p
                         . ( Monad m
                           , p ~ Client
                                (ChainSyncRequest point)
                                (ChainSyncResponse block point tip)
                                m
                           )
                        => [point]
                        -> (ChainSyncResponse block point tip -> m ())
                        -> p ()
chainSyncClient known handleResponse = do
  request (RequestIntersect known) >>= lift . handleResponse
  forever $ do
    request RequestNext >>= lift . handleResponse


chainSyncServer :: forall block point tip m p
                         . ( Monad m
                           , p ~ Server
                                (ChainSyncRequest point)
                                (ChainSyncResponse block point tip)
                                m
                           )
                        => (ChainSyncClient block point tip p () -> p ())
                        -> ChainSyncRequest point -> p ()
chainSyncServer runClient rq0 =
  runClient $ ChainSyncClient (handleRequest rq0)
  where
    handleRequest rq = case rq of
        RequestIntersect known ->
          pure . SendMsgFindIntersect known $
            ClientStIntersect
              { recvMsgIntersectFound = \p t -> ChainSyncClient $ do
                  respond (RollBackward p t) >>= handleRequest
              , recvMsgIntersectNotFound = \t -> ChainSyncClient $ do
                  respond (IntersectNotFound t) >>= handleRequest
              }
        RequestNext ->
          pure . SendMsgRequestNext (pure ()) $
            ClientStNext
              { recvMsgRollForward = \h t -> ChainSyncClient $ do
                  respond (RollForward h t) >>= handleRequest
              , recvMsgRollBackward = \p t -> ChainSyncClient $ do
                  respond (RollBackward p t) >>= handleRequest
              }
