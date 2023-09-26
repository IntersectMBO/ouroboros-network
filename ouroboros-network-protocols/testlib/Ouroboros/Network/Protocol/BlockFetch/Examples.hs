{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Ouroboros.Network.Protocol.BlockFetch.Examples where

import           Control.Exception (assert)
import           Control.Monad (unless)
import           Data.Functor (($>))
import           Data.Maybe (fromMaybe)
import           Data.Type.Queue
import qualified Pipes

import           Control.Concurrent.Class.MonadSTM.Strict

import           Ouroboros.Network.Mock.Chain (Chain, HasHeader, Point)
import qualified Ouroboros.Network.Mock.Chain as Chain

import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Type

constantBlockFetchReceiver
  :: Functor m
  => (block -> m ())   -- ^ handle block
  -> m ()              -- ^ handle `MsgBatchDone`
  -> BlockFetchReceiver block m
constantBlockFetchReceiver onBlock handleBatchDone =
  BlockFetchReceiver {
    handleBlock = \block -> onBlock block $>
                  constantBlockFetchReceiver onBlock handleBatchDone,
    handleBatchDone
  }

-- | A @'BlockFetchClient'@ designed for testing, which accumulates incoming
-- blocks in a @'StrictTVar'@, which is read on termination.
--
-- Returns a list of bodies received from the server, from the newest to
-- oldest.
--
blockFetchClientMap
  :: forall block point m.
     MonadSTM m
  => [ChainRange point]
  -> BlockFetchClient block point m [block]
blockFetchClientMap ranges = BlockFetchClient $ do
  var <- newTVarIO []
  donevar <- newTVarIO (length ranges)
  let blockFetchResponse = BlockFetchResponse {
        handleStartBatch =
          return $ constantBlockFetchReceiver
            (\block -> atomically (modifyTVar var (block :)))
            (atomically (modifyTVar donevar pred)),
        handleNoBlocks = do
          atomically $ modifyTVar donevar pred
          return ()
      }
  goBlockFetch donevar var ranges blockFetchResponse
 where
  goBlockFetch
    :: StrictTVar m Int
    -> StrictTVar m [block]
    -> [ChainRange point]
    -> BlockFetchResponse block m [block]
    -> m (BlockFetchRequest block point m [block])

  goBlockFetch donevar var []       _response = do
    -- wait for all responses to be fulfilled
    atomically $ do
      x <- readTVar donevar
      unless (x <= 0) retry
    SendMsgClientDone <$> atomically (readTVar var)

  goBlockFetch donevar var (r : rs) response  =
    return $ SendMsgRequestRange r response (BlockFetchClient $ goBlockFetch donevar var rs response)

--
-- Pipelined clients of the block-fetch protocol
--

data F st st' where
    FBusy      :: F BFBusy BFIdle
    FStreaming :: F BFStreaming BFIdle

deriving instance Show (F st st')


-- | A pipelined block-fetch client which sends eagerly a list of requests.
-- This presents maximum pipelining and presents minmimum choice to the
-- environment (drivers).
--
-- It returns the interleaving of `ChainRange point` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedMax
  :: forall block point m.
     Monad m
  => [ChainRange point]
  -> BlockFetchClientPipelined block point m [Either (ChainRange point) block]
blockFetchClientPipelinedMax ranges0 =
  BlockFetchClientPipelined (return $ go [] ranges0 SingEmptyF)
 where
  go :: [Either (ChainRange point) block]
     -> [ChainRange point]
     -> SingQueueF F (q :: BFQueue block point)
     -> BlockFetchIdle block point q m [Either (ChainRange point) block]
  go acc (req : reqs) q        = SendMsgRequestRangePipelined
                                   req
                                   (return $ go (Left req : acc) reqs (q |> FBusy))
  go acc []           (SingConsF FBusy q')
                               = CollectStartBatch
                                   Nothing
                                   (return $ go acc [] (SingConsF FStreaming q'))
                                   (return $ go acc [] q')
  go acc []         q@(SingConsF FStreaming q')
                               = CollectBlock
                                   Nothing
                                   (\b -> return $ go (Right b : acc) [] q)
                                   (return $ go acc [] q')

  go acc []           SingEmptyF
                               = SendMsgDonePipelined acc


-- | A pipelined block-fetch client that sends eagerly but always tries to
-- collect any replies as soon as they are available.  This keeps pipelining to
-- bare minimum, and gives maximum choice to the environment (drivers).
--
-- It returns the interleaving of `ChainRange point` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedMin
  :: forall block point m.
     Monad m
  => [ChainRange point]
  -> BlockFetchClientPipelined block point m [Either (ChainRange point) block]
blockFetchClientPipelinedMin ranges0 =
  BlockFetchClientPipelined (return $ go [] ranges0 SingEmptyF)
 where
  go :: [Either (ChainRange point) block]
     -> [ChainRange point]
     -> SingQueueF F q
     -> BlockFetchIdle block point q m
                       [Either (ChainRange point) block]
  go acc reqs         q@(SingConsF FBusy q')
                               = CollectStartBatch
                                   (case reqs of
                                      req : reqs' ->
                                        Just . return $ requestMore acc req reqs' q
                                      [] -> Nothing
                                   )
                                   (return $ go acc reqs (SingConsF FStreaming q'))
                                   (return $ go acc reqs q')

  go acc reqs         q@(SingConsF FStreaming q')
                               = CollectBlock
                                   (case reqs of
                                      req : reqs' ->
                                        Just . return $ requestMore acc req reqs' q
                                      [] -> Nothing
                                   )
                                   (\b -> return $ go (Right b : acc) reqs q)
                                   (return $ go acc reqs q')

  go acc (req : reqs) q@SingEmptyF
                               = requestMore acc req reqs q
  go acc []             SingEmptyF
                               = SendMsgDonePipelined acc

  requestMore :: [Either (ChainRange point) block]
              -> ChainRange point -> [ChainRange point]
              -> SingQueueF F q
              -> BlockFetchIdle block point q m
                                [Either (ChainRange point) block]
  requestMore acc req reqs q = SendMsgRequestRangePipelined
                                req
                                (return $ go (Left req : acc) reqs (q |> FBusy))


-- | A pipelined block-fetch client that sends eagerly up to some maximum limit
-- of outstanding requests. It is also always ready to collect any replies if
-- they are available.  This allows limited pipelining and correspondingly
-- limited choice to the environment (drivers).
--
-- It returns the interleaving of `ChainRange point` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedLimited
  :: forall block point m.
     MonadSTM m
  => Int
  -> [ChainRange point]
  -> BlockFetchClientPipelined block point m [Either (ChainRange point) block]
blockFetchClientPipelinedLimited omax ranges0 =
  BlockFetchClientPipelined (return $ go [] ranges0 SingEmptyF)
 where
  go :: [Either (ChainRange point) block]
     -> [ChainRange point]
     -> SingQueueF F q
     -> BlockFetchIdle block point q m
                       [Either (ChainRange point) block]
  go acc []           (SingConsF FBusy q')
                               = CollectStartBatchSTM
                                   retry
                                   (return $ go acc [] (SingConsF FStreaming q'))
                                   (return $ go acc [] q')

  go acc []         q@(SingConsF FStreaming q')
                               = CollectBlockSTM
                                   retry
                                   (\b -> return $ go (Right b : acc) [] q)
                                   (return $ go acc [] q')

  go acc reqs@(req : reqs') (q@(SingConsF FBusy q'))
                              = CollectStartBatch
                                  (if queueFDepth q  < omax
                                    then Just . return $ requestMore acc req reqs' q
                                    else Nothing)
                                  (return $ go acc reqs (SingConsF FStreaming q'))
                                  (return $ go acc reqs q')

  go acc reqs@(req : reqs') q@(SingConsF FStreaming q')
                              = CollectBlockSTM
                                  (if queueFDepth q < omax
                                    then return . return $ requestMore acc req reqs' q
                                    else retry)
                                  (\b -> return $ go (Right b : acc) reqs q)
                                  (return $ go acc reqs q')

  go acc (req : reqs) SingEmptyF  = requestMore acc req reqs SingEmptyF

  go acc []           SingEmptyF  = SendMsgDonePipelined acc

  requestMore :: [Either (ChainRange point) block]
              -> ChainRange point -> [ChainRange point]
              -> SingQueueF F q
              -> BlockFetchIdle block point q m
                                [Either (ChainRange point) block]
  requestMore acc req reqs q = SendMsgRequestRangePipelined
                                req
                                (return $ go (Left req : acc) reqs (q |> FBusy))

--
-- pipelined interleaving
--

-- | Used to model 'SingQueue F q', as `[PState]` in
-- 'blockFetchPipelineInterleaving'.
--
data PState = PBusy | PStreaming

-- | A reference specification for interleaving of requests and responses
-- with pipelining, where the environment can choose whether a response is
-- available yet.
--
-- This also supports bounded choice where the maximum number of outstanding
-- in-flight responses is limited.
--
blockFetchPipelineInterleaving :: forall req resp.
                                   Int    -- ^ Bound on outstanding responses
                               -> [Bool] -- ^ Pipelining choices
                               -> [req] -> [[resp]] -> [Either req resp]
blockFetchPipelineInterleaving omax cs0 reqs0 resps0 =
    assert (length reqs0 == length resps0) $
    go [] cs0 reqs0 resps0
  where
    go :: [PState] -- pipelining queue
       -> [Bool]
       -> [req]
       -> [[resp]]
       -> [Either req resp]
    go [] cs (req:reqs') resps = Left req
                               : go [PBusy] cs reqs' resps

    go [] _  []          []   = []
    go [] _  []          [rs] = map Right rs
    go [] _  []          _    = error "invariant violation"

    go q@(PBusy : q') (c:cs) reqs resps@(resp:resps')
      | c && length q < omax
      , req : reqs' <- reqs  = Left req
                             : go (q ++ [PBusy]) cs reqs' resps
      | otherwise            =
                  case resp of
                    []  -> go               q'  cs reqs resps' -- 'MsgNoBlocks'
                    _:_ -> go (PStreaming : q') cs reqs resps  -- 'MsgStartBatch'

    go (PBusy : q') [] reqs resps@(resp:resps') =
                  case resp of
                    []  -> go               q'  [] reqs resps' -- 'MsgNoBlocks'
                    _:_ -> go (PStreaming : q') [] reqs resps  -- 'MsgStartBatch'

    go (PBusy : _) _ _ []  = error "invariant violation"

    go q@(PStreaming : q') (c:cs) reqs resps@(resp:resps')
      | c && length q < omax
      , req : reqs' <- reqs  = Left req
                             : go (q ++ [PBusy]) cs reqs' resps
      | otherwise            =
                  case resp of
                    []      -> go               q'  cs reqs resps' -- 'MsgBatchDone'
                    r:rs    -> Right r
                             : go (PStreaming : q') cs reqs (rs:resps') -- 'MsgBlock'

    go (PStreaming : q') [] reqs (resp:resps')
      =           case resp of
                     []     -> go               q'  [] reqs resps' -- 'MsgBatchDone'
                     r:rs   -> Right r
                             : go (PStreaming : q') [] reqs (rs:resps') -- 'MsgBlock'

    go (PStreaming : _) _ _ [] = error "invariant violation"


--
-- Server side of the block-fetch protocol
--

-- | A recursive control data type which encodes a succession of @'ChainRange'
-- block@ requests.
--
newtype RangeRequests m block = RangeRequests {
    runRangeRequest :: ChainRange (Point block)
                    -> Pipes.Producer block m (RangeRequests m block)
  }

-- | A constant @'RangeRequests'@ object.
--
constantRangeRequests
  :: Monad m
  => (ChainRange (Point block) -> Pipes.Producer block m ())
  -> RangeRequests m block
constantRangeRequests f = RangeRequests (\range -> f range $> constantRangeRequests f)

-- | @RangeRequests@ which requests blocks from a chain.  Use @'Functor'@
-- instance of @'RangeRequests'@ to change map @'block'@.
--
rangeRequestsFromChain
  :: ( Monad m
     , HasHeader block
     )
  => Chain block
  -> RangeRequests m block
rangeRequestsFromChain chain = constantRangeRequests $ \(ChainRange from to) ->
  Pipes.each $ fromMaybe [] $ Chain.selectBlockRange chain from to

-- | Construct a @'BlockFetchServer'@ from a @'RangeRequest'@ control data type.
--
blockFetchServer
  :: forall m block.
     Monad m
  => RangeRequests m block
  -> BlockFetchServer block (Point block) m ()
blockFetchServer (RangeRequests rangeRequest) = BlockFetchServer handleRequest ()
 where
  handleRequest
    :: ChainRange (Point block)
    -> m (BlockFetchBlockSender block (Point block) m ())
  handleRequest range = do
    stream <- Pipes.next $ rangeRequest range
    case stream of
      Left rangeRequest'     ->
        return $ SendMsgNoBlocks (return $ blockFetchServer rangeRequest')
      Right (block', stream') ->
        return $ SendMsgStartBatch (sendStream block' stream')

  sendStream
    :: block
    -> Pipes.Producer block m (RangeRequests m block)
    -> m (BlockFetchSendBlocks block (Point block) m ())
  sendStream block stream =
    return $ SendMsgBlock block $ do
      next <- Pipes.next stream
      case next of
        Left rangeRequest' -> return $ SendMsgBatchDone (return $ blockFetchServer rangeRequest')
        Right (block', stream') -> sendStream block' stream'
