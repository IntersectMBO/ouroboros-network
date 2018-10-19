{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module ConsumersAndProducers
  ( ConsumerHandlers
  , ProducerHandlers
  , exampleConsumer
  , exampleProducer
  )where

import           Block (HasHeader (..))
import           Chain
                   ( Chain (..), ChainUpdate (..), Point (..) )
import qualified Chain
                   ( selectPoints, addBlock, rollback, genesisPoint )
import           ChainProducerState
                   ( ChainProducerState(..), ReaderId )
import qualified ChainProducerState
                   ( initReader, updateReader, readerInstruction
                   , findFirstPoint )
import           MonadClass
import           ProtocolInterfaces
import           Ouroboros



-- | An instance of the the consumer side of the protocol interface that
-- consumes into a 'Chain' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realisic chain representation.
--
exampleConsumer :: forall p block m stm.
                   ( KnownOuroborosProtocol p
                   , Eq (block p)
                   , HasHeader block
                   , MonadSTM m stm
                   )
                => TVar m (Chain (block p))
                -> ConsumerHandlers (block p) m
exampleConsumer chainvar =
    ConsumerHandlers {..}
  where
    getChainPoints :: m [Point]
    getChainPoints =
        Chain.selectPoints recentOffsets <$> atomically (readTVar chainvar)

    addBlock :: block p -> m ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollbackTo :: Point -> m ()
    rollbackTo p = atomically $ do
        chain <- readTVar chainvar
        --TODO: handle rollback failure
        let (Just !chain') = Chain.rollback p chain
        writeTVar chainvar chain'

-- | Offsets from the head of the chain to select points on the consumer's
-- chain to send to the producer. The specific choice here is fibbonaci up
-- to 2160.
--
recentOffsets :: [Int]
recentOffsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]

-- | An instance of the producer side of the protocol interface that reads from
-- a pure 'ChainProducerState' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realisic chain representation.
--
exampleProducer
  :: forall p block m stm.
     ( HasHeader block
     , Eq (block p)
     , MonadSTM m stm
     )
  => TVar m (ChainProducerState (block p))
  -> ProducerHandlers (block p) m ReaderId
exampleProducer chainvar =
    ProducerHandlers {..}
  where
    newReader :: m ReaderId
    newReader = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = ChainProducerState.initReader Chain.genesisPoint cps
      writeTVar chainvar cps'
      return rid

    improveReadPoint :: ReaderId -> [Point] -> m (Maybe Point)
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.findFirstPoint points cps of
          Nothing     -> return Nothing
          Just ipoint -> do
            let !cps' = ChainProducerState.updateReader rid ipoint cps
            writeTVar chainvar cps'
            return (Just ipoint)

    tryReadChainUpdate :: ReaderId -> m (Maybe (ChainUpdate (block p)))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing -> return Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return $ Just u

    readChainUpdate :: ReaderId -> m (ChainUpdate (block p))
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.readerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return u
