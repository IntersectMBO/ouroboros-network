{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Protocol.ChainSync.PipeClient
where

import Network.TypedProtocol
import Ouroboros.Network.Protocol.ChainSync.Client
import Ouroboros.Network.Protocol.ChainSync.Type
import Pipes

data ChainSyncResponse header point tip
  = RollForward header tip
  | RollBackward point tip
  | IntersectFound point tip
  | IntersectNotFound tip
  deriving (Show)

-- | Run a chain sync client as a 'Producer'.
-- Pass a suitable 'Driver', and a list of known 'point's. The 'Producer' will
-- initialize itself by requesting an intersection, and yielding either
-- 'IntersectFound' or 'IntersectNotFound' to indicate the server's response.
-- After that, it will indefinitely request updates and 'yield' them as
-- 'RollForward' and 'RollBackward', respectively.
chainSyncClientProducer :: forall header point tip m dstate
                         . ( Monad m
                           )
                        => Driver
                            (ChainSync header point tip)
                            AsClient
                            dstate
                            (Producer (ChainSyncResponse header point tip) m)
                        -> [point]
                        -> Producer (ChainSyncResponse header point tip) m dstate
chainSyncClientProducer driver known =
  snd <$> runPeerWithDriver driver peer
  where
    peer = chainSyncClientPeer (ChainSyncClient runPeer)

    runPeer :: Producer
                  (ChainSyncResponse header point tip)
                  m
                  (ClientStIdle header point tip (Producer (ChainSyncResponse header point tip) m) ())
    runPeer =
      pure . SendMsgFindIntersect known $
        ClientStIntersect
          { recvMsgIntersectFound = \p t -> ChainSyncClient $ do
              yield (IntersectFound p t)
              runProducer
          , recvMsgIntersectNotFound = \t -> ChainSyncClient $ do
              yield (IntersectNotFound t)
              runProducer
          }
    runProducer :: Producer
                    (ChainSyncResponse header point tip)
                    m
                    (ClientStIdle header point tip (Producer (ChainSyncResponse header point tip) m) ())
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
