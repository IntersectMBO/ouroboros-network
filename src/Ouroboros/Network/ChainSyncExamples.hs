{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.ChainSyncExamples (
    chainSyncClientExample
  , chainSyncServerExample
  ) where

import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.Chain (Chain (..), Point (..), ChainUpdate(..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ChainProducerState, ReaderId)
import qualified Ouroboros.Network.ChainProducerState as ChainProducerState
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server


-- | An instance of the client side of the chain sync protocol that
-- consumes into a 'Chain' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
chainSyncClientExample :: forall header m stm a.
                          (HasHeader header, MonadSTM m stm)
                       => TVar m (Chain header)
                       -> m (ChainSyncClient header (Point header) m a)
chainSyncClientExample chainvar =
    initialise <$> getChainPoints
  where
    initialise :: [Point header] -> ClientStIdle header (Point header) m a
    initialise points =
      SendMsgFindIntersect points $
      -- In this consumer example, we do not care about whether the server
      -- found an intersection or not. If not, we'll just sync from genesis.
      --
      -- Alternative policies here include:
      --  iteratively finding the best intersection
      --  rejecting the server if there is no intersection in the last K blocks
      --
      ClientStIntersect {
        recvMsgIntersectImproved  = \_ _ -> return requestNext,
        recvMsgIntersectUnchanged = \  _ -> return requestNext
      }

    requestNext :: ClientStIdle header (Point header) m a
    requestNext =
      SendMsgRequestNext
        handleNext
        -- We received a wait message, and we have the opportunity to do
        -- something. In this example we don't take up that opportunity.
        (return handleNext)

    handleNext :: ClientStNext header (Point header) m a
    handleNext =
      ClientStNext {
        recvMsgRollForward  = \header _pHead -> do
          addBlock header
          return requestNext

      , recvMsgRollBackward = \pIntersect _pHead -> do
          rollback pIntersect
          return requestNext 
      }

    getChainPoints :: m [Point header]
    getChainPoints =
        Chain.selectPoints recentOffsets <$> atomically (readTVar chainvar)

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
chainSyncServerExample :: forall header m stm a.
                          (HasHeader header, MonadSTM m stm)
                       => TVar m (ChainProducerState header)
                       -> m (ChainSyncServer header (Point header) m a)
chainSyncServerExample chainvar =
    idle <$> newReader
  where
    idle :: ReaderId -> ServerStIdle header (Point header) m a
    idle r =
      ServerStIdle {
        recvMsgRequestNext   = handleRequestNext r,
        recvMsgFindIntersect = handleFindIntersect r
      }

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
             -> ChainUpdate header
             -> ServerStNext header (Point header) m a
    sendNext r (AddBlock b) = SendMsgRollForward  b undefined (idle r)
    sendNext r (RollBack p) = SendMsgRollBackward p undefined (idle r)

    handleFindIntersect :: ReaderId
                        -> [Point header]
                        -> m (ServerStIntersect header (Point header) m a)
    handleFindIntersect r points = do
      -- TODO: guard number of points
      -- Find the first point that is on our chain
      changed <- improveReadPoint r points
      case changed of
        Just (pt, tip) -> return $ SendMsgIntersectImproved pt tip     (idle r)
        Nothing        -> return $ SendMsgIntersectUnchanged undefined (idle r)

    newReader :: m ReaderId
    newReader = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = ChainProducerState.initReader Chain.genesisPoint cps
      writeTVar chainvar cps'
      return rid

    improveReadPoint :: ReaderId -> [Point header] -> m (Maybe (Point header, Point header))
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.findFirstPoint points cps of
          Nothing     -> return Nothing
          Just ipoint -> do
            let !cps' = ChainProducerState.updateReader rid ipoint cps
            writeTVar chainvar cps'
            return (Just (ipoint, Chain.headPoint (ChainProducerState.chainState cps')))

    tryReadChainUpdate :: ReaderId -> m (Maybe (ChainUpdate header))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing -> return Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return $ Just u

    readChainUpdate :: ReaderId -> m (ChainUpdate header)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return u

