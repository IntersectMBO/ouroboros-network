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

import           Control.Monad

import           Block (HasHeader (..))
import           Chain
                   ( Chain (..), ChainUpdate (..), Point (..)
                   , blockPoint, findIntersection )
import qualified Chain
import           ChainProducerState
                   ( ChainProducerState(..), ReaderId
                   , initReader, updateReader, readerInstruction )
import           MonadClass
import           ProtocolInterfaces



-- | An instance of the the consumer side of the protocol interface that
-- consumes into a 'Chain' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realisic chain representation.
--
exampleConsumer :: forall block m stm.
                   ( Eq block
                   , HasHeader block
                   , MonadSay m
                   , MonadSTM m stm
                   )
                => TVar m (Chain block)
                -> ConsumerHandlers block m
exampleConsumer chainvar =
    ConsumerHandlers {..}
  where
    getChainPoints :: m [Point]
    getChainPoints = atomically $ do
        chain <- readTVar chainvar
        -- TODO: improve point selection function, move it elsewhere
        let ps = map blockPoint $ Chain.toList $ chain
        return ps

    addBlock :: block -> m ()
    addBlock b = void $ atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        when (Chain.headPoint chain /= Chain.headPoint chain')
          $ writeTVar chainvar chain'

    rollbackTo :: Point -> m ()
    rollbackTo p = atomically $ do
        chain <- readTVar chainvar
        --TODO: handle rollback failure
        let (Just !chain') = Chain.rollback p chain
        when (Chain.headPoint chain /= Chain.headPoint chain')
          $ writeTVar chainvar chain'



-- | An instance of the producer side of the protocol interface that reads from
-- a pure 'ChainProducerState' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realisic chain representation.
--
exampleProducer
  :: forall block m stm.
     ( HasHeader block
     , Eq block
     , MonadSay m
     , MonadSTM m stm
     )
  => TVar m (ChainProducerState block)
  -> ProducerHandlers block m ReaderId
exampleProducer chainvar =
    ProducerHandlers {..}
  where
    findIntersectionRange :: [Point] -> m (Maybe Point)
    findIntersectionRange points = do
      ChainProducerState {chainState} <- atomically $ readTVar chainvar
      return $! findIntersection chainState points

    establishReaderState :: Point -> m ReaderId
    establishReaderState ipoint = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = initReader ipoint cps
      when (cps /= cps')
        $ writeTVar chainvar cps'
      return rid

    updateReaderState :: ReaderId -> Point -> m ()
    updateReaderState rid ipoint =
      atomically $ do
        cps <- readTVar chainvar
        let !cps' = updateReader rid ipoint cps
        when (cps /= cps')
          $ writeTVar chainvar cps'

    tryReadChainUpdate :: ReaderId -> m (Maybe (ChainUpdate block))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case readerInstruction rid cps of
          Nothing -> return Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return $ Just u

    readChainUpdate :: ReaderId -> m (ChainUpdate block)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case readerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return u

