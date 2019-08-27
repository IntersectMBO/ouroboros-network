{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Protocol.ChainSync.Examples (
    chainSyncClientExample
  , Client (..)
  , pureClient
  , chainSyncServerExample
  ) where

import           Control.Monad.Class.MonadSTM.Strict

import           Ouroboros.Network.Block (BlockNo, HasHeader (..), castPoint, genesisPoint)
import           Ouroboros.Network.MockChain.Chain (Chain (..), ChainUpdate (..),
                     Point (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.MockChain.ProducerState (ChainProducerState,
                     ReaderId)
import qualified Ouroboros.Network.MockChain.ProducerState as ChainProducerState
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server

data Client header tip m t = Client
  { rollbackward :: Point header -> tip -> m (Either t (Client header tip m t))
  , rollforward  :: header -> m (Either t (Client header tip m t))
  , points       :: [Point header] -> m (Client header tip m t)
  }

-- | A client which doesn't do anything and never ends. Used with
-- 'chainSyncClientExample', the StrictTVar m (Chain header) will be updated but
-- nothing further will happen.
pureClient :: Applicative m => Client header tip m void
pureClient = Client
  { rollbackward = \_ _ -> pure (Right pureClient)
  , rollforward  = \_ -> pure (Right pureClient)
  , points       = \_ -> pure pureClient
  }

-- | An instance of the client side of the chain sync protocol that
-- consumes into a 'Chain' stored in a 'StrictTVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
chainSyncClientExample :: forall header tip m a.
                          (HasHeader header, MonadSTM m)
                       => StrictTVar m (Chain header)
                       -> Client header tip m a
                       -> ChainSyncClient header tip m a
chainSyncClientExample chainvar client = ChainSyncClient $
    initialise <$> getChainPoints
  where
    initialise :: ([Point header], Client header tip m a)
               -> ClientStIdle header tip m a
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
        recvMsgIntersectFound    = \_ _ -> ChainSyncClient (return (requestNext client')),
        recvMsgIntersectNotFound = \  _ -> ChainSyncClient (return (requestNext client'))
      }

    requestNext :: Client header tip m a
                -> ClientStIdle header tip m a
    requestNext client' =
      SendMsgRequestNext
        (handleNext client')
        -- We received a wait message, and we have the opportunity to do
        -- something. In this example we don't take up that opportunity.
        (return (handleNext client'))

    handleNext :: Client header tip m a
               -> ClientStNext header tip m a
    handleNext client' =
      ClientStNext {
        recvMsgRollForward  = \header _tip -> ChainSyncClient $ do
          addBlock header
          choice <- rollforward client' header
          pure $ case choice of
            Left a         -> SendMsgDone a
            Right client'' -> requestNext client''

      , recvMsgRollBackward = \pIntersect tip -> ChainSyncClient $ do
          rollback pIntersect
          choice <- rollbackward client' pIntersect tip
          pure $ case choice of
            Left a         -> SendMsgDone a
            Right client'' -> requestNext client''
      }

    getChainPoints :: m ([Point header], Client header tip m a)
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
-- a pure 'ChainProducerState' stored in a 'StrictTVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
chainSyncServerExample :: forall header m a.
                          ( HasHeader header
                          , MonadSTM m
                          -- , HeaderHash header ~ HeaderHash blk
                          )
                       => a
                       -> StrictTVar m (ChainProducerState header)
                       -> ChainSyncServer header (Point header, BlockNo) m a
chainSyncServerExample recvMsgDoneClient chainvar = ChainSyncServer $
    idle <$> newReader
  where
    idle :: ReaderId -> ServerStIdle header (Point header, BlockNo) m a
    idle r =
      ServerStIdle {
        recvMsgRequestNext   = handleRequestNext r,
        recvMsgFindIntersect = handleFindIntersect r,
        recvMsgDoneClient    = pure recvMsgDoneClient
      }

    idle' :: ReaderId -> ChainSyncServer header (Point header, BlockNo) m a
    idle' = ChainSyncServer . pure . idle

    handleRequestNext :: ReaderId
                      -> m (Either (ServerStNext header (Point header, BlockNo) m a)
                                (m (ServerStNext header (Point header, BlockNo) m a)))
    handleRequestNext r = do
      mupdate <- tryReadChainUpdate r
      case mupdate of
        Just update -> return (Left  (sendNext r update))
        Nothing     -> return (Right (sendNext r <$> readChainUpdate r))
                       -- Reader is at the head, have to block and wait for
                       -- the producer's state to change.

    sendNext :: ReaderId
             -> (Point header, BlockNo, ChainUpdate header header)
             -> ServerStNext header (Point header, BlockNo) m a
    sendNext r (tip, blkNo, AddBlock b) = SendMsgRollForward  b             (tip, blkNo) (idle' r)
    sendNext r (tip, blkNo, RollBack p) = SendMsgRollBackward (castPoint p) (tip, blkNo) (idle' r)

    handleFindIntersect :: ReaderId
                        -> [Point header]
                        -> m (ServerStIntersect header (Point header, BlockNo) m a)
    handleFindIntersect r points = do
      -- TODO: guard number of points
      -- Find the first point that is on our chain
      changed <- improveReadPoint r points
      case changed of
        (Just pt, tip, blkNo) -> return $ SendMsgIntersectFound     pt (tip, blkNo) (idle' r)
        (Nothing, tip, blkNo) -> return $ SendMsgIntersectNotFound     (tip, blkNo) (idle' r)

    newReader :: m ReaderId
    newReader = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = ChainProducerState.initReader genesisPoint cps
      writeTVar chainvar cps'
      return rid

    improveReadPoint :: ReaderId
                     -> [Point header]
                     -> m (Maybe (Point header), Point header, BlockNo)
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.findFirstPoint (map castPoint points) cps of
          Nothing     -> let chain = ChainProducerState.chainState cps
                         in pure ( Nothing
                                 , castPoint (Chain.headPoint chain)
                                 , Chain.headBlockNo chain
                                 )
          Just ipoint -> do
            let !cps' = ChainProducerState.updateReader rid ipoint cps
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            pure ( Just (castPoint ipoint)
                 , castPoint (Chain.headPoint chain)
                 , Chain.headBlockNo chain
                 )

    tryReadChainUpdate :: ReaderId
                       -> m (Maybe (Point header, BlockNo, ChainUpdate header header))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing -> return Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            return $ Just ( castPoint (Chain.headPoint chain)
                          , Chain.headBlockNo chain
                          , u)

    readChainUpdate :: ReaderId -> m (Point header, BlockNo, ChainUpdate header header)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            return (castPoint (Chain.headPoint chain)
                   , Chain.headBlockNo chain
                   , u
                   )
