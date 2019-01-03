{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.ChainSyncExamples (
    chainSyncClientExample
  , Client (..)
  , pureClient
  , chainSyncServerExample
  ) where

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.Chain (Chain (..), Point (..), ChainUpdate(..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ChainProducerState, ReaderId)
import qualified Ouroboros.Network.ChainProducerState as ChainProducerState
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server

data Client header m t = Client
  { rollbackward :: Point header -> Point header -> m (Either t (Client header m t))
  , rollforward  :: header -> m (Either t (Client header m t))
  , points       :: [Point header] -> m (Client header m t)
  }

-- | A client which doesn't do anything and never ends. Used with
-- 'chainSyncClientExample', the TVar m (Chain header) will be updated but
-- nothing further will happen.
pureClient :: Applicative m => Client header m x
pureClient = Client
  { rollbackward = \_ _ -> pure (Right pureClient)
  , rollforward  = \_ -> pure (Right pureClient)
  , points       = \_ -> pure pureClient
  }

-- | An instance of the client side of the chain sync protocol that
-- consumes into a 'Chain' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
chainSyncClientExample :: forall header m a.
                          (HasHeader header, MonadSTM m)
                       => TVar m (Chain header)
                       -> Client header m a
                       -> ChainSyncClient header (Point header) m a
chainSyncClientExample chainvar client = ChainSyncClient $
    initialise <$> getChainPoints
  where
    initialise :: ([Point header], Client header m a) -> ClientStIdle header (Point header) m a
    initialise (points, client') =
      SendMsgFindIntersect points $
      -- In this consumer example, we do not care about whether the server
      -- found an intersection or not. If not, we'll just sync from genesis.
      --
      -- Alternative policies here include:
      --  iteratively finding the best intersection
      --  rejecting the server if there is no intersection in the last K blocks
      --
      ClientStIntersect {
        recvMsgIntersectImproved  = \_ _ -> ChainSyncClient (return (requestNext client')),
        recvMsgIntersectUnchanged = \  _ -> ChainSyncClient (return (requestNext client'))
      }

    requestNext :: Client header m a -> ClientStIdle header (Point header) m a
    requestNext client' =
      SendMsgRequestNext
        (handleNext client')
        -- We received a wait message, and we have the opportunity to do
        -- something. In this example we don't take up that opportunity.
        (return (handleNext client'))

    handleNext :: Client header m a -> ClientStNext header (Point header) m a
    handleNext client' =
      ClientStNext {
        recvMsgRollForward  = \header _pHead -> ChainSyncClient $ do
          addBlock header
          choice <- rollforward client' header
          pure $ case choice of
            Left a -> SendMsgDone a
            Right client'' -> requestNext client''

      , recvMsgRollBackward = \pIntersect pHead -> ChainSyncClient $ do
          rollback pIntersect
          choice <- rollbackward client' pIntersect pHead
          pure $ case choice of
            Left a -> SendMsgDone a
            Right client'' -> requestNext client''
      }

    getChainPoints :: m ([Point header], Client header m a)
    getChainPoints = do
      pts <- Chain.selectPoints recentOffsets <$> atomically (readTVar chainvar)
      client' <- points client pts
      pure (pts, client')

    addBlock :: header -> m ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollback :: Point header -> m ()
    rollback p = atomically $ do
        chain <- readTVar chainvar
        --TODO: handle rollback failure
        let (Just !chain') = Chain.rollback p chain
        writeTVar chainvar chain'

-- | Offsets from the head of the chain to select points on the consumer's
-- chain to send to the producer. The specific choice here is fibonacci up
-- to 2160.
--
recentOffsets :: [Int]
recentOffsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]


-- | An instance of the server side of the chain sync protocol that reads from
-- a pure 'ChainProducerState' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
chainSyncServerExample :: forall header m a.
                          (HasHeader header, MonadSTM m)
                       => a
                       -> TVar m (ChainProducerState header)
                       -> ChainSyncServer header (Point header) m a
chainSyncServerExample recvMsgDoneClient chainvar = ChainSyncServer $
    idle <$> newReader
  where
    idle :: ReaderId -> ServerStIdle header (Point header) m a
    idle r =
      ServerStIdle {
        recvMsgRequestNext   = handleRequestNext r,
        recvMsgFindIntersect = handleFindIntersect r,
        recvMsgDoneClient
      }

    idle' :: ReaderId -> ChainSyncServer header (Point header) m a
    idle' = ChainSyncServer . pure . idle

    handleRequestNext :: ReaderId
                      -> m (Either (ServerStNext header (Point header) m a)
                                (m (ServerStNext header (Point header) m a)))
    handleRequestNext r = do
      mupdate <- tryReadChainUpdate r
      case mupdate of
        Just update -> return (Left  (sendNext r update))
        Nothing     -> return (Right (sendNext r <$> readChainUpdate r))
                       -- Reader is at the head, have to block and wait for
                       -- the producer's state to change.

    sendNext :: ReaderId
             -> (Point header, ChainUpdate header)
             -> ServerStNext header (Point header) m a
    sendNext r (tip, AddBlock b) = SendMsgRollForward  b tip (idle' r)
    sendNext r (tip, RollBack p) = SendMsgRollBackward p tip (idle' r)

    handleFindIntersect :: ReaderId
                        -> [Point header]
                        -> m (ServerStIntersect header (Point header) m a)
    handleFindIntersect r points = do
      -- TODO: guard number of points
      -- Find the first point that is on our chain
      changed <- improveReadPoint r points
      case changed of
        (Just pt, tip) -> return $ SendMsgIntersectImproved  pt tip (idle' r)
        (Nothing, tip) -> return $ SendMsgIntersectUnchanged    tip (idle' r)

    newReader :: m ReaderId
    newReader = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = ChainProducerState.initReader Chain.genesisPoint cps
      writeTVar chainvar cps'
      return rid

    improveReadPoint :: ReaderId -> [Point header] -> m (Maybe (Point header), Point header)
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.findFirstPoint points cps of
          Nothing     -> pure (Nothing, Chain.headPoint (ChainProducerState.chainState cps))
          Just ipoint -> do
            let !cps' = ChainProducerState.updateReader rid ipoint cps
            writeTVar chainvar cps'
            pure (Just ipoint, Chain.headPoint (ChainProducerState.chainState cps'))

    tryReadChainUpdate :: ReaderId -> m (Maybe (Point header, ChainUpdate header))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing -> return Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return $ Just (Chain.headPoint (ChainProducerState.chainState cps'), u)

    readChainUpdate :: ReaderId -> m (Point header, ChainUpdate header)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return (Chain.headPoint (ChainProducerState.chainState cps'), u)

