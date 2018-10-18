{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
module ProtocolInterfaces (
    ConsumerHandlers(..)
  , ProducerHandlers(..)
  , liftConsumerHandlers
  , liftProducerHandlers
  ) where

import Chain (ChainUpdate (..), Point (..))


-- | The interface used on the consumer side of the chain consumer protocol
-- to update a local chain to synchronise it with the producer chain.
--
data ConsumerHandlers block m = ConsumerHandlers {
       getChainPoints :: m [Point],
       -- ^ get a list of points from which the producer will pick the common
       -- intersection point.  The reference implementation just sends whole
       -- chain.
       addBlock       :: block -> m (),
       -- ^ apply received @'block'@ to its chain.  The reference implementation
       -- is using @'Chain.addBlock'@ to update its internal state.
       rollbackTo     :: Point -> m ()
       -- ^ rollback to a given point.  The reference implementation is using
       -- @'Chain.rollback'@ to update its internal state.
     }


-- | The interface used on the producer side of the chain consumer protocol
-- to query and read the producers chain.
--
data ProducerHandlers block m r = ProducerHandlers {
       newReader             :: m r,
       -- ^ allocate new reader state.  The reference implementation is using
       -- @'ChainProducerState.initReader'@ to mutate its internal state.
       improveReadPoint      :: r -> [Point] -> m (Maybe Point),
       -- ^ find the most optimal intersection between received list of
       -- @'Point'@s and producers chain.
       tryReadChainUpdate    :: r -> m (Maybe (ChainUpdate block)),
       -- ^ compute chain update for a given reader.  This is a non blocking
       -- operation.
       readChainUpdate       :: r -> m (ChainUpdate block)
       -- ^ compute chain update for a given reader.  This maybe block, awaiting
       -- for changes of its internal state (producer's chain).
     }

liftConsumerHandlers :: (forall a. m a -> m' a)
                     -> ConsumerHandlers block m
                     -> ConsumerHandlers block m'
liftConsumerHandlers lift ConsumerHandlers {
                            getChainPoints,
                            addBlock,
                            rollbackTo
                          } =
    ConsumerHandlers {
      getChainPoints = lift getChainPoints,
      addBlock       = \b -> lift (addBlock b),
      rollbackTo     = \p -> lift (rollbackTo p)
    }


liftProducerHandlers :: (forall a. m a -> m' a)
                     -> ProducerHandlers block m  r
                     -> ProducerHandlers block m' r
liftProducerHandlers lift ProducerHandlers {
                            newReader,
                            improveReadPoint,
                            tryReadChainUpdate,
                            readChainUpdate
                          } =
    ProducerHandlers {
      newReader          = lift newReader,
      improveReadPoint   = \r ps -> lift (improveReadPoint r ps),
      tryReadChainUpdate = \r -> lift (tryReadChainUpdate r),
      readChainUpdate    = \r -> lift (readChainUpdate r)
    }

