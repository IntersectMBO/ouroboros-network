{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Examples where

import Control.Monad (unless)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Pipes ((~>))
import qualified Pipes

import Control.Monad.Class.MonadSTM (MonadSTM (..))

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
        handleStartBatch = do
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
    -> m (BlockFetchSender header body m ())
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
