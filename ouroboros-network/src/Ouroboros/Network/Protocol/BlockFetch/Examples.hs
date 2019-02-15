{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Examples where

import Control.Monad (unless)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Pipes ((~>))
import qualified Pipes

import Control.Monad.Class.MonadSTM (MonadSTM (..))

import           Network.TypedProtocol.Pipelined

import Ouroboros.Network.Chain (Chain, HasHeader)
import qualified Ouroboros.Network.Chain as Chain

import Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))
import Ouroboros.Network.Protocol.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Server

constantBlockFetchReceiver
  :: Functor m
  => (body -> m ())    -- ^ handle body
  -> m ()              -- ^ handle `MsgBatchDone`
  -> BlockFetchReceiver header body m
constantBlockFetchReceiver onBlock handleBatchDone = BlockFetchReceiver {
    handleBlock = \block -> onBlock block $> constantBlockFetchReceiver onBlock handleBatchDone,
    handleBatchDone
  }

-- | A @'BlockFetchClient'@ designed for testing, which accumulates incoming
-- blocks in a @'TVar'@, which is read on termination.
--
-- Returns a list of bodies received from the server, from the newest to
-- oldest.
--
blockFetchClientMap
  :: forall header body m. MonadSTM m
  => [ChainRange header]
  -> BlockFetchClient header body m [body]
blockFetchClientMap ranges = BlockFetchClient $ do
  var <- newTVarM []
  donevar <- newTVarM (length ranges)
  let blockFetchResponse = BlockFetchResponse {
        handleStartBatch =
          return $ constantBlockFetchReceiver
            (\body -> atomically (modifyTVar var (body :)))
            (atomically (modifyTVar donevar pred)),
        handleNoBlocks = do
          atomically $ modifyTVar donevar pred
          return ()
      }
  goBlockFetch donevar var ranges blockFetchResponse
 where
  goBlockFetch
    :: TVar m Int
    -> TVar m [body]
    -> [ChainRange header]
    -> BlockFetchResponse header body m [body]
    -> m (BlockFetchRequest header body m [body])

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
-- It returns the interleaving of `ChainRange header` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedMax
  :: forall header body m.  Monad m
  => [ChainRange header]
  -> BlockFetchClientPipelined header body [body] m [Either (ChainRange header) [body]]
blockFetchClientPipelinedMax ranges0 =
  BlockFetchClientPipelined (go [] ranges0 Zero)
 where
  go :: [Either (ChainRange header) [body]] -> [ChainRange header] -> Nat o
     -> BlockFetchSender o header body [body] m [Either (ChainRange header) [body]]
  go acc (req : reqs) o        = SendMsgRequestRangePipelined
                                    req
                                    []
                                    (\mBody c -> case mBody of
                                        Nothing -> return c
                                        Just b  -> return (b : c))
                                    (go (Left req : acc) reqs (Succ o))
  go acc []           (Succ o) = CollectBlocksPipelined
                                    Nothing    
                                    (\bs -> go (Right bs : acc) [] o)
  go acc []           Zero     = SendMsgDonePipelined acc


-- | A piplined block-fetch client that sends eagerly but always tries to
-- collect any replies as soon as they are available.  This keeps pipelining to
-- bare minimum, and gives maximum choice to the environment (drivers).
--
-- It returns the interleaving of `ChainRange header` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedMin
  :: forall header body m.  Monad m
  => [ChainRange header]
  -> BlockFetchClientPipelined header body [body] m [Either (ChainRange header) [body]]
blockFetchClientPipelinedMin ranges0 =
  BlockFetchClientPipelined (go [] ranges0 Zero)
 where
  go :: [Either (ChainRange header) [body]]
     -> [ChainRange header]
     -> Nat n
     -> BlockFetchSender n header body [body] m [Either (ChainRange header) [body]]
  go acc []           (Succ n) = CollectBlocksPipelined
                                  Nothing
                                  (\bs -> go (Right bs : acc) [] n)
  go acc (req : reqs) (Succ n) = CollectBlocksPipelined
                                  (Just $ requestMore acc req reqs (Succ n))
                                  (\bs -> go (Right bs : acc) (req : reqs) n)
  go acc (req : reqs) Zero     = requestMore acc req reqs Zero
  go acc []           Zero     = SendMsgDonePipelined acc

  requestMore :: [Either (ChainRange header) [body]]
              -> ChainRange header -> [ChainRange header]
              -> Nat n
              -> BlockFetchSender n header body [body] m [Either (ChainRange header) [body]]
  requestMore acc req reqs n = SendMsgRequestRangePipelined
                                req
                                []
                                (\mBody c -> case mBody of
                                  Nothing -> return c
                                  Just b  -> return (b : c))
                                (go (Left req : acc) reqs (Succ n))

-- | A pipelined block-fetch client that sends eagerly up to some maximum limit
-- of outstanding requests. It is also always ready to collect any replies if
-- they are available.  This allows limited pipelining and correspondingly
-- limited choice to the environment (drivers).
--
-- It returns the interleaving of `ChainRange header` requests and list of
-- received block bodies in the order from newest to oldest (received block
-- bodies are also ordered in this way).
--
blockFetchClientPipelinedLimited
  :: forall header body m. Monad m
  => Int
  -> [ChainRange header]
  -> BlockFetchClientPipelined header body [body] m [Either (ChainRange header) [body]]
blockFetchClientPipelinedLimited omax ranges0 =
  BlockFetchClientPipelined (go [] ranges0 Zero)
 where
  go :: [Either (ChainRange header) [body]]
     -> [ChainRange header]
     -> Nat n
     -> BlockFetchSender n header body [body] m [Either (ChainRange header) [body]]
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

  requestMore :: [Either (ChainRange header) [body]]
              -> ChainRange header -> [ChainRange header]
              -> Nat n
              -> BlockFetchSender n header body [body] m [Either (ChainRange header) [body]]
  requestMore acc req reqs n = SendMsgRequestRangePipelined
                                req
                                []
                                (\mBody c -> case mBody of
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
-- header@ requests.
--
newtype RangeRequests header m body = RangeRequests {
    runRangeRequest :: ChainRange header -> Pipes.Producer body m (RangeRequests header m body)
  }

instance Monad m => Functor (RangeRequests header m) where
  -- Replace @Pipe.yield@s with @Pipes.yield . f@), and recurse over the return
  -- value of the @Pipe.Producer@.
  fmap f (RangeRequests g) = RangeRequests $ (fmap . fmap) (fmap f) $ g ~> (Pipes.yield . f)

-- | A constant @'RangeRequests'@ object.
--
constantRangeRequests
  :: Monad m
  => (ChainRange header -> Pipes.Producer body m ())
  -> RangeRequests header m body
constantRangeRequests f = RangeRequests (\range -> f range $> constantRangeRequests f)

-- | @RangeRequests@ which requests blocks from a chain.  Use @'Functor'@
-- instance of @'RangeRequests'@ to change map @'block'@.
--
rangeRequestsFromChain
  :: ( Monad m
     , HasHeader block
     )
  => Chain block
  -> RangeRequests block m block
rangeRequestsFromChain chain = constantRangeRequests $ \(ChainRange from to) ->
  Pipes.each $ fromMaybe [] $ Chain.selectBlockRange chain from to

-- | Construct a @'BlockFetchServer'@ from a @'RangeRequest'@ control data type.
--
blockFetchServer
  :: forall m header body.
     Monad m
  => RangeRequests header m body
  -> BlockFetchServer header body m ()
blockFetchServer (RangeRequests rangeRequest) = BlockFetchServer handleRequest ()
 where
  handleRequest
    :: ChainRange header
    -> m (BlockFetchBlockSender header body m ())
  handleRequest range = do
    stream <- Pipes.next $ rangeRequest range
    case stream of
      Left rangeRequest'     ->
        return $ SendMsgNoBlocks (return $ blockFetchServer rangeRequest')
      Right (body', stream') ->
        return $ SendMsgStartBatch (sendStream body' stream')

  sendStream
    :: body
    -> Pipes.Producer body m (RangeRequests header m body)
    -> m (BlockFetchSendBlocks header body m ())
  sendStream body stream =
    return $ SendMsgBlock body $ do
      next <- Pipes.next stream
      case next of
        Left rangeRequest' -> return $ SendMsgBatchDone (return $ blockFetchServer rangeRequest')
        Right (body', stream') -> sendStream body' stream'
