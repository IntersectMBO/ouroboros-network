{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.ConsumersAndProducers
  ( -- * Chain consumer protocol handlers
    ConsumerHandlers
  , ProducerHandlers
  , exampleConsumer
  , exampleProducer
    -- * Block layer of chain consumer protocol handlers
  , BlockConsumerHandlers
  , BlockProducerHandlers
  , exampleBlockConsumer
  , exampleBlockProducer
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Network.Block (HasHeader (..), StandardHash)
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
exampleConsumer :: forall block m.
                   ( HasHeader block
                   , MonadSTM m
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
  :: forall block m.
     ( HasHeader block
     , MonadSTM m
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
          Nothing -> return Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return $ Just u

    readChainUpdate :: ReaderId -> m (ChainUpdate block)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return u

-- |
-- An instance of the consumer side of the block layer responsible for
-- requesting block bodies (@'BlockBody'@).
exampleBlockConsumer
  :: forall m blockHeader blockBody.
     ( MonadSTM m
     , StandardHash blockHeader
     )
  => TVar m (Map (Point blockHeader) (Promise blockBody))
  -> (blockHeader -> blockBody -> Bool)
  -> BlockConsumerHandlers blockHeader blockBody m
exampleBlockConsumer blockStorage verifyBlockBody = BlockConsumerHandlers
    { putBlock = \point -> atomically . modifyTVar' blockStorage . Map.insert point
    , verifyBlockBody
    }

-- |
-- An instance of the producer side of the block layer responsible for
-- diffusion of block bodies (@'BlockBody'@).
exampleBlockProducer
  :: forall m blockHeader blockBody.
     ( MonadSTM m
     , StandardHash blockHeader
     )
  => TVar m (Map (Point blockHeader) (Promise blockBody))
  -> BlockProducerHandlers blockHeader blockBody m
exampleBlockProducer blockStorage = BlockProducerHandlers
    { getBlock = atomically . getBlock_
    , awaitBlock
    }
    where
    getBlock_ :: Point blockHeader -> Tr m (Maybe (Promise blockBody))
    getBlock_ p = Map.lookup p <$> readTVar blockStorage

    awaitBlock :: Point blockHeader -> m (Maybe blockBody)
    awaitBlock p = atomically $ do
        mbb <- getBlock_ p
        case mbb of
            Just (Fullfilled bb) -> return (Just bb)
            Just Awaiting        -> retry
            Nothing              -> return Nothing
