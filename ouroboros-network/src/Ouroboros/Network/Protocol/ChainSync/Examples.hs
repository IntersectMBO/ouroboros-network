{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Network.Protocol.ChainSync.Examples (
    chainSyncClientExample
  , Client (..)
  , pureClient
  , controlledClient
  , Tip(..)
  , chainSyncServerExample
  ) where

import           Control.Monad.Class.MonadSTM.Strict

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash, Tip (..),
                     castPoint, castTip, genesisPoint)
import           Ouroboros.Network.MockChain.Chain (Chain (..),
                     ChainUpdate (..), Point (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.MockChain.ProducerState (ChainProducerState,
                     FollowerId)
import qualified Ouroboros.Network.MockChain.ProducerState as ChainProducerState
import           Ouroboros.Network.Mux (ControlMessageSTM, ControlMessage (..))
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server

data Client header point tip m t = Client
  { rollbackward :: point -> tip -> m (Either t (Client header point tip m t))
  , rollforward  :: header -> m (Either t (Client header point tip m t))
  , points       :: [point] -> m (Client header point tip m t)
  }

-- | A client which doesn't do anything and never ends. Used with
-- 'chainSyncClientExample', the StrictTVar m (Chain header) will be updated but
-- nothing further will happen.
pureClient :: Applicative m => Client header point tip m void
pureClient = Client
  { rollbackward = \_ _ -> pure (Right pureClient)
  , rollforward  = \_ -> pure (Right pureClient)
  , points       = \_ -> pure pureClient
  }

controlledClient :: MonadSTM m
                 => ControlMessageSTM m
                 -> Client header point tip m ()
controlledClient controlMessageSTM = go
  where
    go = Client
      { rollbackward = \_ _ -> do
          ctrl <- atomically controlMessageSTM
          case ctrl of
            Continue  -> pure (Right go)
            Quiesce   -> error "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
            Terminate -> pure (Left ())
      , rollforward = \_ -> do
          ctrl <- atomically controlMessageSTM
          case ctrl of
            Continue  -> pure (Right go)
            Quiesce   -> error "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
            Terminate -> pure (Left ())
      , points = \_ -> pure go
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
                       -> Client header (Point header) tip m a
                       -> ChainSyncClient header (Point header) tip m a
chainSyncClientExample chainvar client = ChainSyncClient $
    initialise <$> getChainPoints
  where
    initialise :: ([Point header], Client header (Point header) tip m a)
               -> ClientStIdle header (Point header) tip m a
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

    requestNext :: Client header (Point header) tip m a
                -> ClientStIdle header (Point header) tip m a
    requestNext client' =
      SendMsgRequestNext
        (handleNext client')
        -- We received a wait message, and we have the opportunity to do
        -- something. In this example we don't take up that opportunity.
        (return (handleNext client'))

    handleNext :: Client header (Point header) tip m a
               -> ClientStNext header (Point header) tip m a
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

    getChainPoints :: m ([Point header], Client header (Point header) tip m a)
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
        let !chain' = case Chain.rollback p chain of
              Just a -> a
              Nothing -> error "out of scope rollback"
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
chainSyncServerExample :: forall blk header m a.
                          ( HasHeader header
                          , MonadSTM m
                          , HeaderHash header ~ HeaderHash blk
                          )
                       => a
                       -> StrictTVar m (ChainProducerState header)
                       -> ChainSyncServer header (Point blk) (Tip blk) m a
chainSyncServerExample recvMsgDoneClient chainvar = ChainSyncServer $
    idle <$> newFollower
  where
    idle :: FollowerId -> ServerStIdle header (Point blk) (Tip blk) m a
    idle r =
      ServerStIdle {
        recvMsgRequestNext   = handleRequestNext r,
        recvMsgFindIntersect = handleFindIntersect r,
        recvMsgDoneClient    = pure recvMsgDoneClient
      }

    idle' :: FollowerId -> ChainSyncServer header (Point blk) (Tip blk) m a
    idle' = ChainSyncServer . pure . idle

    handleRequestNext :: FollowerId
                      -> m (Either (ServerStNext header (Point blk) (Tip blk) m a)
                                (m (ServerStNext header (Point blk) (Tip blk) m a)))
    handleRequestNext r = do
      mupdate <- tryReadChainUpdate r
      case mupdate of
        Just update -> return (Left  (sendNext r update))
        Nothing     -> return (Right (sendNext r <$> readChainUpdate r))
                       -- Follower is at the head, have to block and wait for
                       -- the producer's state to change.

    sendNext :: FollowerId
             -> (Tip blk, ChainUpdate header header)
             -> ServerStNext header (Point blk) (Tip blk) m a
    sendNext r (tip, AddBlock b) = SendMsgRollForward  b             tip (idle' r)
    sendNext r (tip, RollBack p) = SendMsgRollBackward (castPoint p) tip (idle' r)

    handleFindIntersect :: FollowerId
                        -> [Point blk]
                        -> m (ServerStIntersect header (Point blk) (Tip blk) m a)
    handleFindIntersect r points = do
      -- TODO: guard number of points
      -- Find the first point that is on our chain
      changed <- improveReadPoint r points
      case changed of
        (Just pt, tip) -> return $ SendMsgIntersectFound     pt tip (idle' r)
        (Nothing, tip) -> return $ SendMsgIntersectNotFound     tip (idle' r)

    newFollower :: m FollowerId
    newFollower = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = ChainProducerState.initFollower genesisPoint cps
      writeTVar chainvar cps'
      return rid

    improveReadPoint :: FollowerId
                     -> [Point blk]
                     -> m (Maybe (Point blk), Tip blk)
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.findFirstPoint (map castPoint points) cps of
          Nothing     -> let chain = ChainProducerState.chainState cps
                         in return (Nothing, castTip (Chain.headTip chain))
          Just ipoint -> do
            let !cps' = ChainProducerState.updateFollower rid ipoint cps
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            return (Just (castPoint ipoint), castTip (Chain.headTip chain))

    tryReadChainUpdate :: FollowerId
                       -> m (Maybe (Tip blk, ChainUpdate header header))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.followerInstruction rid cps of
          Nothing -> return Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            return $ Just (castTip (Chain.headTip chain), u)

    readChainUpdate :: FollowerId -> m (Tip blk, ChainUpdate header header)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.followerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            return (castTip (Chain.headTip chain), u)
