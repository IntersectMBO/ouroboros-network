{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Examples where

import Control.Monad (unless)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import qualified Pipes

import Control.Monad.Class.MonadSTM.Strict

import           Network.TypedProtocol.Pipelined

import Ouroboros.Network.MockChain.Chain (Chain, HasHeader)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))
import Ouroboros.Network.Protocol.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Server

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
  :: forall block m. MonadSTM m
  => [ChainRange block]
  -> BlockFetchClient block m [block]
blockFetchClientMap ranges = BlockFetchClient $ do
  var <- newTVarM []
  donevar <- newTVarM (length ranges)
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
    -> [ChainRange block]
    -> BlockFetchResponse block m [block]
    -> m (BlockFetchRequest block m [block])

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

-- | A pipelined block-fetch client which sends eagerly a list of requests.
-- This presents maximum pipelining and presents minmimum choice to the
-- environment (drivers).
--
-- It returns the interleaving of `ChainRange block` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedMax
  :: forall block m.  Monad m
  => [ChainRange block]
  -> BlockFetchClientPipelined block m [Either (ChainRange block) [block]]
blockFetchClientPipelinedMax ranges0 =
  BlockFetchClientPipelined (go [] ranges0 Zero)
 where
  go :: [Either (ChainRange block) [block]] -> [ChainRange block] -> Nat o
     -> BlockFetchSender o [block] block m [Either (ChainRange block) [block]]
  go acc (req : reqs) o        = SendMsgRequestRangePipelined
                                    req
                                    []
                                    (\mBlock c -> case mBlock of
                                        Nothing -> return c
                                        Just b  -> return (b : c))
                                    (go (Left req : acc) reqs (Succ o))
  go acc []           (Succ o) = CollectBlocksPipelined
                                    Nothing    
                                    (\bs -> go (Right bs : acc) [] o)
  go acc []           Zero     = SendMsgDonePipelined acc


-- | A pipelined block-fetch client that sends eagerly but always tries to
-- collect any replies as soon as they are available.  This keeps pipelining to
-- bare minimum, and gives maximum choice to the environment (drivers).
--
-- It returns the interleaving of `ChainRange block` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedMin
  :: forall block m.  Monad m
  => [ChainRange block]
  -> BlockFetchClientPipelined block m [Either (ChainRange block) [block]]
blockFetchClientPipelinedMin ranges0 =
  BlockFetchClientPipelined (go [] ranges0 Zero)
 where
  go :: [Either (ChainRange block) [block]]
     -> [ChainRange block]
     -> Nat n
     -> BlockFetchSender n [block] block m
                         [Either (ChainRange block) [block]]
  go acc []           (Succ n) = CollectBlocksPipelined
                                  Nothing
                                  (\bs -> go (Right bs : acc) [] n)
  go acc (req : reqs) (Succ n) = CollectBlocksPipelined
                                  (Just $ requestMore acc req reqs (Succ n))
                                  (\bs -> go (Right bs : acc) (req : reqs) n)
  go acc (req : reqs) Zero     = requestMore acc req reqs Zero
  go acc []           Zero     = SendMsgDonePipelined acc

  requestMore :: [Either (ChainRange block) [block]]
              -> ChainRange block -> [ChainRange block]
              -> Nat n
              -> BlockFetchSender n [block] block m
                                  [Either (ChainRange block) [block]]
  requestMore acc req reqs n = SendMsgRequestRangePipelined
                                req
                                []
                                (\mBlock c -> case mBlock of
                                  Nothing -> return c
                                  Just b  -> return (b : c))
                                (go (Left req : acc) reqs (Succ n))

-- | A pipelined block-fetch client that sends eagerly up to some maximum limit
-- of outstanding requests. It is also always ready to collect any replies if
-- they are available.  This allows limited pipelining and correspondingly
-- limited choice to the environment (drivers).
--
-- It returns the interleaving of `ChainRange block` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedLimited
  :: forall block m. Monad m
  => Int
  -> [ChainRange block]
  -> BlockFetchClientPipelined block m [Either (ChainRange block) [block]]
blockFetchClientPipelinedLimited omax ranges0 =
  BlockFetchClientPipelined (go [] ranges0 Zero)
 where
  go :: [Either (ChainRange block) [block]]
     -> [ChainRange block]
     -> Nat n
     -> BlockFetchSender n [block] block m
                         [Either (ChainRange block) [block]]
  go acc []              (Succ n) = CollectBlocksPipelined
                                      Nothing
                                      (\bs -> go (Right bs : acc) [] n)

  go acc rs@(req : reqs) (Succ n) = CollectBlocksPipelined
                                      (if int (Succ n) < omax
                                        then Just $ requestMore acc req reqs (Succ n)
                                        else Nothing)
                                      (\bs -> go (Right bs : acc) rs n)

  go acc (req : reqs) Zero        = requestMore acc req reqs Zero

  go acc []           Zero        = SendMsgDonePipelined acc

  requestMore :: [Either (ChainRange block) [block]]
              -> ChainRange block -> [ChainRange block]
              -> Nat n
              -> BlockFetchSender n [block] block m
                                  [Either (ChainRange block) [block]]
  requestMore acc req reqs n = SendMsgRequestRangePipelined
                                req
                                []
                                (\mBlock c -> case mBlock of
                                  Nothing -> return c
                                  Just b  -> return (b : c))
                                (go (Left req : acc) reqs (Succ n))

  -- this isn't supposed to be efficient, it's just for the example
  int :: Nat n -> Int
  int Zero     = 0
  int (Succ n) = succ (int n)


--
-- Server side of the block-fetch protocol
--

-- | A recursive control data type which encodes a succession of @'ChainRange'
-- block@ requests.
--
newtype RangeRequests m block = RangeRequests {
    runRangeRequest :: ChainRange block
                    -> Pipes.Producer block m (RangeRequests m block)
  }

-- | A constant @'RangeRequests'@ object.
--
constantRangeRequests
  :: Monad m
  => (ChainRange block -> Pipes.Producer block m ())
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
  -> BlockFetchServer block m ()
blockFetchServer (RangeRequests rangeRequest) = BlockFetchServer handleRequest ()
 where
  handleRequest
    :: ChainRange block
    -> m (BlockFetchBlockSender block m ())
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
    -> m (BlockFetchSendBlocks block m ())
  sendStream block stream =
    return $ SendMsgBlock block $ do
      next <- Pipes.next stream
      case next of
        Left rangeRequest' -> return $ SendMsgBatchDone (return $ blockFetchServer rangeRequest')
        Right (block', stream') -> sendStream block' stream'
