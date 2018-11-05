{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.ConsumersAndProducers
  ( ConsumerHandlers
  , ProducerHandlers
  , exampleConsumer
  , exampleProducer
  )where

import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.Chain (Chain (..), ChainUpdate (..),
                     Point (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ChainProducerState (..),
                     ReaderId)
import qualified Ouroboros.Network.ChainProducerState as ChainProducerState
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.ProtocolInterfaces



-- | An instance of the consumer side of the protocol interface that
-- consumes into a 'Chain' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
exampleConsumer :: forall block m stm.
                   ( HasHeader block
                   , MonadSTM m stm
                   )
                => TVar m (Chain block)
                -> ConsumerHandlers block m
exampleConsumer chainvar =
    ConsumerHandlers {..}
  where
    getChainPoints :: m [Point block]
    getChainPoints =
        Chain.selectPoints recentOffsets <$> atomically (readTVar chainvar)

    addBlock :: block -> m ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollbackTo :: Point block -> m ()
    rollbackTo p = atomically $ do
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

-- | An instance of the producer side of the protocol interface that reads from
-- a pure 'ChainProducerState' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
exampleProducer
  :: forall block m stm.
     ( HasHeader block
     , MonadSTM m stm
     )
  => TVar m (ChainProducerState block)
  -> ProducerHandlers block m ReaderId
exampleProducer chainvar =
    ProducerHandlers {..}
  where
    newReader :: m ReaderId
    newReader = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = ChainProducerState.initReader Chain.genesisPoint cps
      writeTVar chainvar cps'
      return rid

    improveReadPoint :: ReaderId -> [Point block] -> m (Maybe (Point block, Point block))
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.findFirstPoint points cps of
          Nothing     -> return Nothing
          Just ipoint -> do
            let !cps' = ChainProducerState.updateReader rid ipoint cps
            writeTVar chainvar cps'
            return (Just (ipoint, Chain.headPoint (ChainProducerState.chainState cps')))

    tryReadChainUpdate :: ReaderId -> m (Maybe (ChainUpdate block))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing -> pure Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return $ pure u

    readChainUpdate :: ReaderId -> m (ChainUpdate block)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return u
