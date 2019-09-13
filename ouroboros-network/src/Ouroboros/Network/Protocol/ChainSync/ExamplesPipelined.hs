{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ChainSync.ExamplesPipelined
    ( Client (..)
    , Tip
    , chainSyncClientPipelinedMax
    , chainSyncClientPipelinedMin
    ) where

import           Control.Monad.Class.MonadSTM.Strict

import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Block (HasHeader (..), BlockNo)
import           Ouroboros.Network.MockChain.Chain (Chain (..), Point (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Examples (Client (..))


-- | Pipeline decision: we can do either one of these:
--
-- * non-pipelined request
-- * pipline a request
-- * collect or pipline, but only when there are pipelined requests
-- * collect, as above, only when tere are pipelined requests
--
-- There might be other useful pipelining scenarios: collect a given number of
-- requests (whihc als can be used to collect all outstanding requests).
--
data PipelineDecision n where
    Request           :: PipelineDecision Z
    Pipeline          :: PipelineDecision n
    CollectOrPipeline :: PipelineDecision (S n)
    Collect           :: PipelineDecision (S n)


-- | Make pipeline decision gets the following arguments:
--
-- * how many requests are not yet collected (in flight or
--   already queued)
-- * block nubmer of client's tip
-- * block nubmer of server's tip
--
-- Client' tip block number and server' tip block number can only be equal
-- (from client's perspective) when both client's and server's tip headers
-- agree.  If they would not agree (server forked), then the server sends
-- 'MsgRollBackward' which rollbacks one block and makes client's tip and
-- server's tip differ.
--
-- In this module we implement two pipelining strategies: 'pipelineDecisionMax'
-- and 'pipelineDecisionMin'.
--
type MkPipelineDecision n = Nat n -> BlockNo -> BlockNo -> PipelineDecision n


-- | Present maximal pipelining of at most @omax@ requests.  Collect responses
-- either when we are at the same block number as the server or when we sent
-- more than @omax@ requests.
--
-- If @omax = 3@ this pipelining strategy will generate a sequence:
-- @
--    Pipeline
--    Pipeline
--    Pipeline
--    Collect
--    Pipeline
--    Collect
--    ....
--    Pipeline
--    Collect
--    Collect
--    Collect
-- @
--
pipelineDecisionMax :: Int -> MkPipelineDecision n
pipelineDecisionMax omax n cliTipBlockNo srvTipBlockNo =
    case n of
      Zero   -- We are at most one block away from the server's tip.
             -- We use equality so that this does not triggered when we are
             -- ahead of the producer, and it wil send us 'MsgRollBackward'.
             | cliTipBlockNo + 1 == srvTipBlockNo
             -> Request

             | otherwise
             -> Pipeline

      Succ{} -- We pipelined some requests and we are now synchronised or we
             -- exceeded pipelineing limit, and thus we should await for
             -- a response.
             --
             -- Note: we add @omax'@ to avoid a deadlock in tests.  This
             -- pielineing strategy collects at this stage a single result,
             -- this causes @n'@ to drop and we will pipeline next message.
             -- This assures that when we aproach the end of the chain we will
             -- collect all outstanding requests without pipelining a request
             | cliTipBlockNo + n' >= srvTipBlockNo || n' >= omax'
             -> Collect

             | otherwise
             -> Pipeline
  where
    n' :: BlockNo
    n' = fromIntegral (int n)

    omax' :: BlockNo
    omax' = fromIntegral omax


-- | Present minimum pipelining of at most @omax@ requests, collect responses
-- eagerly.
--
pipelineDecisionMin :: Int -> MkPipelineDecision n
pipelineDecisionMin omax n cliTipBlockNo srvTipBlockNo =
    case n of
      Zero   -- We are at most one block away from the server's tip.
             -- We use equality so that this does not triggered when we are
             -- ahead of the producer, and it wil send us 'MsgRollBackward'.
             | cliTipBlockNo + 1 == srvTipBlockNo
             -> Request

             | otherwise
             -> Pipeline

      Succ{} -- We pipelined some requests and we are now synchronised or we
             -- exceeded pipelineing limit, and thus we should await for
             -- a response.
             | cliTipBlockNo + n' >= srvTipBlockNo || n' >= omax'
             -> Collect

             | otherwise
             -> CollectOrPipeline
  where
    n' :: BlockNo
    n' = fromIntegral (int n)

    omax' :: BlockNo
    omax' = fromIntegral omax


-- | Type which represents tip of server's chain.
--
type Tip header = (Point header, BlockNo)

-- | Pipelined chain sync client which pipelines at most @omax@ requests according to 'MkPipelineDecision' policy.
--
chainSyncClientPipelined
      :: forall header m a.
         ( HasHeader header
         , MonadSTM m
         )
      => (forall n. MkPipelineDecision n)
      -> StrictTVar m (Chain header)
      -> Client header (Tip header) m a
      -> ChainSyncClientPipelined header (Tip header) m a
chainSyncClientPipelined pipelineDecision chainvar =
    ChainSyncClientPipelined . fmap initialise . getChainPoints
  where
    initialise :: ([Point header], Client header (Tip header) m a)
               -> ClientPipelinedStIdle Z header (Tip header) m a
    initialise (points, client) =
      SendMsgFindIntersect points $
      -- In this consumer example, we do not care about whether the server
      -- found an intersection or not. If not, we'll just sync from genesis.
      ClientPipelinedStIntersect {
        recvMsgIntersectFound    = \_ srvTip -> do
          cliTipBlockNo <- Chain.headBlockNo <$> atomically (readTVar chainvar)
          pure $ go Zero cliTipBlockNo srvTip client,
        recvMsgIntersectNotFound = \  srvTip -> do
          cliTipBlockNo <- Chain.headBlockNo <$> atomically (readTVar chainvar)
          pure $ go Zero cliTipBlockNo srvTip client
      }

    -- Drive pipelining by using @pipelineDecision@ callback.
    go :: Nat n
       -> BlockNo
       -- ^ our head
       -> Tip header
       -- ^ head of the server
       -> Client header (Tip header) m a
       -> ClientPipelinedStIdle n header (Tip header) m a

    go n cliTipBlockNo srvTip@(_, srvTipBlockNo) client@Client {rollforward, rollbackward} =
      case (n, pipelineDecision n cliTipBlockNo srvTipBlockNo) of
        (_Zero, Request) ->
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
                      Right client' -> go n (blockNo srvHeader) srvTip' client',
                  recvMsgRollBackward = \pRollback srvTip' -> do
                    cliTipBlockNo' <- rollback pRollback
                    choice <- rollbackward pRollback srvTip'
                    pure $ case choice of
                      Left a        -> SendMsgDone a
                      Right client' -> go n cliTipBlockNo' srvTip' client'
                }

        (_, Pipeline) ->
          SendMsgRequestNextPipelined
            (go (Succ n) cliTipBlockNo srvTip client)

        (Succ n', CollectOrPipeline) ->
          CollectResponse
            -- if there is no message we pipeline next one; it is important we
            -- do not directly loop here, but send something; otherwise we
            -- would just build a busy loop polling the driver's receiving
            -- queue.
            (Just $ SendMsgRequestNextPipelined $ go (Succ n) cliTipBlockNo srvTip client)
            ClientStNext {
                recvMsgRollForward = \srvHeader srvTip' -> do
                  addBlock srvHeader
                  choice <- rollforward srvHeader
                  pure $ case choice of
                    Left a         -> collectAndDone n' a
                    Right client' -> go n' (blockNo srvHeader) srvTip' client',
                recvMsgRollBackward = \pRollback srvTip' -> do
                  cliTipBlockNo' <- rollback pRollback
                  choice <- rollbackward pRollback srvTip'
                  pure $ case choice of
                    Left a         -> collectAndDone n' a
                    Right client' -> go n' cliTipBlockNo' srvTip' client'
              }

        (Succ n', Collect) ->
          CollectResponse
            Nothing
            ClientStNext {
                recvMsgRollForward = \srvHeader srvTip' -> do
                  addBlock srvHeader
                  choice <- rollforward srvHeader
                  pure $ case choice of
                    Left a         -> collectAndDone n' a
                    Right client' -> go n' (blockNo srvHeader) srvTip' client',
                recvMsgRollBackward = \pRollback srvTip' -> do
                  cliTipBlockNo' <- rollback pRollback
                  choice <- rollbackward pRollback srvTip'
                  pure $ case choice of
                    Left a         -> collectAndDone n' a
                    Right client' -> go n' cliTipBlockNo' srvTip' client'
              }


    -- Recursievly collect all outstanding responses, but do nothing with them.
    -- If 'CollectResponse' returns an error when applying
    -- roll forward or roll backward instruction, we collect all the
    -- outstanding responses and send 'MsgDone'.
    collectAndDone :: Nat n
                   -> a
                   -> ClientPipelinedStIdle n header (Tip header) m a

    collectAndDone Zero     a = SendMsgDone a

    collectAndDone (Succ n) a = CollectResponse
                                  Nothing
                                  ClientStNext {
                                      recvMsgRollForward  = \_header _point ->
                                        pure $ collectAndDone n a,
                                      recvMsgRollBackward = \_pRollback _pHead ->
                                        pure $ collectAndDone n a
                                    }


    getChainPoints :: Client header (Tip header) m a
                   -> m ([Point header], Client header (Tip header) m a)
    getChainPoints client = do
      pts <- Chain.selectPoints recentOffsets <$> atomically (readTVar chainvar)
      client' <- points client pts
      pure (pts, client')

    addBlock :: header -> m ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollback :: Point header -> m BlockNo
    rollback p = atomically $ do
        chain <- readTVar chainvar
        --TODO: handle rollback failure
        let (Just !chain') = Chain.rollback p chain
        writeTVar chainvar chain'
        pure $ Chain.headBlockNo chain'


-- this isn't supposed to be efficient, it's just for the example
int :: Nat n -> Int
int Zero     = 0
int (Succ n) = succ (int n)

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
      => Int
      -- ^ maximal number of outstanding requests
      -> StrictTVar m (Chain header)
      -> Client header (Tip header) m a
      -> ChainSyncClientPipelined header (Tip header) m a
chainSyncClientPipelinedMax omax = chainSyncClientPipelined (pipelineDecisionMax omax)

-- | A pipelined chain-sycn client that piplines at most @omax@ requests and
-- always tries to collect any replies as soon as they are available.   This
-- keeps pipelineing to bare minimum, and gives maximum choice to the
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
      => Int
      -- ^ maximal number of outstanding requests
      -> StrictTVar m (Chain header)
      -> Client header (Tip header) m a
      -> ChainSyncClientPipelined header (Tip header) m a
chainSyncClientPipelinedMin omax = chainSyncClientPipelined (pipelineDecisionMin omax)
