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

data ChainSyncResponse block point tip
  = RollForward block tip
  | RollBackward point tip
  | IntersectNotFound tip
  deriving (Show)

-- | Run a chain sync client as a 'Pipe'.
chainSyncClientProducer :: forall block point tip m p
                         . ( Monad m
                           , p ~ Pipe [point] (ChainSyncResponse block point tip) m
                           )
                        => (ChainSyncClient block point tip p () -> p ())
                        -> p ()
chainSyncClientProducer runClient =
  runClient $ ChainSyncClient runProtocol
  where
    runProtocol :: p (ClientStIdle block point tip p ())
    runProtocol = do
      known <- await
      pure . SendMsgFindIntersect known $
        ClientStIntersect
          { recvMsgIntersectFound = \p t -> ChainSyncClient $ do
              yield (RollBackward p t)
              runProducer
          , recvMsgIntersectNotFound = \t -> ChainSyncClient $ do
              yield (IntersectNotFound t)
              runProducer
          }

    runProducer :: p (ClientStIdle block point tip p ())
    runProducer =
      pure . SendMsgRequestNext (pure ()) $
        ClientStNext
          { recvMsgRollForward = \h t -> ChainSyncClient $ do
              yield (RollForward h t)
              runProducer
          , recvMsgRollBackward = \p t -> ChainSyncClient $ do
              yield (RollBackward p t)
              runProducer
          }
