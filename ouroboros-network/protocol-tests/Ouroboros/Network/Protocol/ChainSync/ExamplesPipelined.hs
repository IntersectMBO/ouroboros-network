{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ChainSync.ExamplesPipelined
  ( Client (..)
  , chainSyncClientPipelinedMax
  , chainSyncClientPipelinedMin
  , chainSyncClientPipelinedLowHigh
  ) where

import           Control.Monad.Class.MonadSTM.Strict
import           Data.Word

import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Block (BlockNo, HasHeader (..), Tip (..),
                     blockNo, getTipBlockNo)
import           Ouroboros.Network.MockChain.Chain (Chain (..), Point (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Examples (Client (..))
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision

-- | Pipelined chain sync client which pipelines at most @omax@ requests according to 'MkPipelineDecision' policy.
--
chainSyncClientPipelined
      :: forall header m a.
         ( HasHeader header
         , MonadSTM m
         )
      => MkPipelineDecision
      -> StrictTVar m (Chain header)
      -> Client header (Point header) (Tip header) m a
      -> ChainSyncClientPipelined header (Point header) (Tip header) m a
chainSyncClientPipelined mkPipelineDecision0 chainvar =
    ChainSyncClientPipelined . fmap initialise . getChainPoints
  where
    initialise :: ([Point header], Client header (Point header) (Tip header) m a)
               -> ClientPipelinedStIdle Z header (Point header) (Tip header) m a
    initialise (points, client) =
      SendMsgFindIntersect points $
      -- In this consumer example, we do not care about whether the server
      -- found an intersection or not. If not, we'll just sync from genesis.
      ClientPipelinedStIntersect {
        recvMsgIntersectFound    = \_ srvTip -> do
          cliTipBlockNo <- Chain.headBlockNo <$> atomically (readTVar chainvar)
          pure $ go mkPipelineDecision0 Zero cliTipBlockNo srvTip client,
        recvMsgIntersectNotFound = \  srvTip -> do
          cliTipBlockNo <- Chain.headBlockNo <$> atomically (readTVar chainvar)
          pure $ go mkPipelineDecision0 Zero cliTipBlockNo srvTip client
      }

    -- Drive pipelining by using @mkPipelineDecision@ callback.
    go :: MkPipelineDecision
       -> Nat n
       -> WithOrigin BlockNo
       -- ^ our head
       -> Tip header
       -- ^ head of the server
       -> Client header (Point header) (Tip header) m a
       -> ClientPipelinedStIdle n header (Point header) (Tip header) m a

    go mkPipelineDecision n cliTipBlockNo srvTip client@Client {rollforward, rollbackward} =
      let srvTipBlockNo = getTipBlockNo srvTip in
      case (n, runPipelineDecision mkPipelineDecision n cliTipBlockNo srvTipBlockNo) of
        (_Zero, (Request, mkPipelineDecision')) ->
          SendMsgRequestNext
              clientStNext
              -- We received 'MsgAwaitReplay' and we get a chance to run
              -- a monadic action, in this case we do not take up that
              -- opportunity.
              (pure clientStNext)
            where
              clientStNext = ClientStNext {
                  recvMsgRollForward = \srvHeader srvTip' -> do
                    addBlock srvHeader
                    choice <- rollforward srvHeader
                    pure $ case choice of
                      Left a        -> SendMsgDone a
                      Right client' -> go mkPipelineDecision' n (At (blockNo srvHeader)) srvTip' client',
                  recvMsgRollBackward = \pRollback srvTip' -> do
                    cliTipBlockNo' <- rollback pRollback
                    choice <- rollbackward pRollback srvTip'
                    pure $ case choice of
                      Left a        -> SendMsgDone a
                      Right client' -> go mkPipelineDecision' n cliTipBlockNo' srvTip' client'
                }

        (_, (Pipeline, mkPipelineDecision')) ->
          SendMsgRequestNextPipelined
            (go mkPipelineDecision' (Succ n) cliTipBlockNo srvTip client)

        (Succ n', (CollectOrPipeline, mkPipelineDecision')) ->
          CollectResponse
            -- if there is no message we pipeline next one; it is important we
            -- do not directly loop here, but send something; otherwise we
            -- would just build a busy loop polling the driver's receiving
            -- queue.
            (Just $ pure $ SendMsgRequestNextPipelined $ go mkPipelineDecision' (Succ n) cliTipBlockNo srvTip client)
            ClientStNext {
                recvMsgRollForward = \srvHeader srvTip' -> do
                  addBlock srvHeader
                  choice <- rollforward srvHeader
                  pure $ case choice of
                    Left a        -> collectAndDone n' a
                    Right client' -> go mkPipelineDecision' n' (At (blockNo srvHeader)) srvTip' client',
                recvMsgRollBackward = \pRollback srvTip' -> do
                  cliTipBlockNo' <- rollback pRollback
                  choice <- rollbackward pRollback srvTip'
                  pure $ case choice of
                    Left a        -> collectAndDone n' a
                    Right client' -> go mkPipelineDecision' n' cliTipBlockNo' srvTip' client'
              }

        (Succ n', (Collect, mkPipelineDecision')) ->
          CollectResponse
            Nothing
            ClientStNext {
                recvMsgRollForward = \srvHeader srvTip' -> do
                  addBlock srvHeader
                  choice <- rollforward srvHeader
                  pure $ case choice of
                    Left a        -> collectAndDone n' a
                    Right client' -> go mkPipelineDecision' n' (At (blockNo srvHeader)) srvTip' client',
                recvMsgRollBackward = \pRollback srvTip' -> do
                  cliTipBlockNo' <- rollback pRollback
                  choice <- rollbackward pRollback srvTip'
                  pure $ case choice of
                    Left a        -> collectAndDone n' a
                    Right client' -> go mkPipelineDecision' n' cliTipBlockNo' srvTip' client'
              }


    -- Recursievly collect all outstanding responses, but do nothing with them.
    -- If 'CollectResponse' returns an error when applying
    -- roll forward or roll backward instruction, we collect all the
    -- outstanding responses and send 'MsgDone'.
    collectAndDone :: Nat n
                   -> a
                   -> ClientPipelinedStIdle n header (Point header) (Tip header) m a

    collectAndDone Zero     a = SendMsgDone a

    collectAndDone (Succ n) a = CollectResponse
                                  Nothing
                                  ClientStNext {
                                      recvMsgRollForward  = \_header _point ->
                                        pure $ collectAndDone n a,
                                      recvMsgRollBackward = \_pRollback _pHead ->
                                        pure $ collectAndDone n a
                                    }


    getChainPoints :: Client header (Point header) (Tip header) m a
                   -> m ([Point header], Client header (Point header) (Tip header) m a)
    getChainPoints client = do
      pts <- Chain.selectPoints recentOffsets <$> atomically (readTVar chainvar)
      client' <- points client pts
      pure (pts, client')

    addBlock :: header -> m ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollback :: Point header -> m (WithOrigin BlockNo)
    rollback p = atomically $ do
        chain <- readTVar chainvar
        --TODO: handle rollback failure
        let (Just !chain') = Chain.rollback p chain
        writeTVar chainvar chain'
        pure $ Chain.headBlockNo chain'

-- | Offsets from the head of the chain to select points on the consumer's
-- chain to send to the producer. The specific choice here is fibonacci up
-- to 2160.
--
recentOffsets :: [Int]
recentOffsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]


-- | A pipelined chain-sync client that sends eagerly up @omax@ pipelined
-- request and then collects all responses and starts over again.  This
-- presents maximum pipelining and presents minimum choice to the environment
-- (drivers).
--
-- This client is only useful in tests and reference implementation: it is
-- using mock representation of a chain.
--
-- If @omax@ is equal to 3 the communication will look like (if the client is
-- far from server's tip;  pipelining is presented with long pauses to
-- ilustrate when messages will be collected).
--
-- >
-- >'MsgFindIntersect' ↘ │ ╲    │
-- >                     │  ╲   │
-- >                     │   ╲  │
-- >                     │    ╲ │ ↙ 'MsgIntersectFound' or 'MsgIntersectNotFound'
-- >                     │    ╱ │
-- >                     │   ╱  │
-- >                     │  ╱   │
-- >  'MsgRequestNext' ↘ │ ╱    │
-- >                     │ ╲    │
-- >                     │  ╲   │
-- >                     │   ╲  │
-- >                     │    ╲ │ ↙ 'MsgRollForward' or 'MsgRollBackward'
-- >                     │    ╱ │
-- >  'MsgRequestNext' ↘ │   ╱  │
-- >                     │ ╲╱   │
-- >                     │ ╱╲   │
-- >                     │   ╲  │
-- >                     │    ╲ │ ↙ 'MsgRollForward' or 'MsgRollBackward'
-- >                     │    ╱ │
-- >                     │   ╱  │
-- >  'MsgRequestNext' ↘ │ ╲╱   │
-- >                     │ ╱╲   │
-- >         'Collect' ↙ │╱  ╲  │
-- >  'MsgRequestNext' ↘ │╲     │
-- >                     ┆      ┆
-- >
-- >                     ┆      ┆
-- >         'Collect' ↙ │╱  ╲  │
-- >                     │    ╲ │ ↙ 'MsgRollForward' or 'MsgRollBackward'
-- >                     │    ╱ │
-- >                     │   ╱  │
-- >                     │  ╱   │
-- >         'Collect' ↙ │ ╱    │
-- >                     │      │
-- >
--
chainSyncClientPipelinedMax
      :: forall header m a.
         ( HasHeader header
         , MonadSTM m
         )
      => Word32
      -- ^ maximal number of outstanding requests
      -> StrictTVar m (Chain header)
      -> Client header (Point header) (Tip header) m a
      -> ChainSyncClientPipelined header (Point header) (Tip header) m a
chainSyncClientPipelinedMax omax = chainSyncClientPipelined (constantPipelineDecision $ pipelineDecisionMax omax)

-- | A pipelined chain-sycn client that pipelines at most @omax@ requests and
-- always tries to collect any replies as soon as they are available.   This
-- keeps pipelining to bare minimum, and gives maximum choice to the
-- environment (drivers).
--
-- If @omax@ is equal to 3 the communication will look like (if the client is
-- far from server's tip;  pipelining is presented with long pauses to
-- ilustrate when messages will be collected).
--
-- >
-- >'MsgFindIntersect' ↘ │ ╲    │
-- >                     │  ╲   │
-- >                     │   ╲  │
-- >                     │    ╲ │ ↙ 'MsgIntersectFound' or 'MsgIntersectNotFound'
-- >                     │    ╱ │
-- >                     │   ╱  │
-- >                     │  ╱   │
-- >  'MsgRequestNext' ↘ │ ╱    │
-- >                     │ ╲    │
-- >                     │  ╲   │
-- >                     │   ╲  │
-- >                     │    ╲ │ ↙ 'MsgRollForward' or 'MsgRollBackward'
-- >                     │    ╱ │
-- >  'MsgRequestNext' ↘ │   ╱  │
-- >                     │ ╲╱   │
-- >         'Collect' ↙ │ ╱╲   │
-- >                     │   ╲  │
-- >                     │    ╲ │ ↙ 'MsgRollForward' or 'MsgRollBackward'
-- >                     │    ╱ │
-- >                     │   ╱  │
-- >  'MsgRequestNext' ↘ │ ╲╱   │
-- >                     │ ╱╲   │
-- >         'Collect' ↙ │   ╲  │
-- >                     │    ╲ │ ↙ 'MsgRollForward' or 'MsgRollBackward'
-- >                     │    ╱ │
-- >                     │   ╱  │
-- >                     │  ╱   │
-- >         'Collect' ↙ │ ╱    │
-- >                     │      │
-- >
--
chainSyncClientPipelinedMin
      :: forall header m a.
         ( HasHeader header
         , MonadSTM m
         )
      => Word32
      -- ^ maximal number of outstanding requests
      -> StrictTVar m (Chain header)
      -> Client header (Point header) (Tip header) m a
      -> ChainSyncClientPipelined header (Point header) (Tip header) m a
chainSyncClientPipelinedMin omax = chainSyncClientPipelined (constantPipelineDecision $ pipelineDecisionMin omax)


chainSyncClientPipelinedLowHigh
      :: forall header m a.
         ( HasHeader header
         , MonadSTM m
         )
      => Word32
      -- ^ low mark
      -> Word32
      -- ^ high mark
      -> StrictTVar m (Chain header)
      -> Client header (Point header) (Tip header) m a
      -> ChainSyncClientPipelined header (Point header) (Tip header) m a
chainSyncClientPipelinedLowHigh lowMark highMark = chainSyncClientPipelined (pipelineDecisionLowHighMark lowMark highMark)
