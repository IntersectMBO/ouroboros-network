{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
module Ouroboros.Network.ProtocolInterfaces (
    -- Chain consumer / producer
    ConsumerHandlers(..)
  , ProducerHandlers(..)
  , liftConsumerHandlers
  , liftProducerHandlers
    -- Block layer consumer / producer
  , BlockProducerHandlers(..)
  , BlockConsumerHandlers(..)
    -- * Auxiliary types
  , Promise(..)
  ) where

import           Ouroboros.Network.Chain (ChainUpdate (..), Point (..))


-- | The interface used on the consumer side of the chain consumer protocol
-- to update a local chain to synchronise it with the producer chain.
--
data ConsumerHandlers block m = ConsumerHandlers {
       getChainPoints :: m [Point block],
       -- ^ get a list of points from which the producer will pick the common
       -- intersection point.  The reference implementation just sends whole
       -- chain.
       addBlock       :: block -> m (),
       -- ^ apply received @'block'@ to its chain.  The reference implementation
       -- is using @'Chain.addBlock'@ to update its internal state.
       rollbackTo     :: Point block -> m ()
       -- ^ rollback to a given point.  The reference implementation is using
       -- @'Chain.rollback'@ to update its internal state.
     }


-- | The interface used on the producer side of the chain consumer protocol
-- to query and read the producers chain.
--
data ProducerHandlers block m r = ProducerHandlers {
       newReader          :: m r,
       -- ^ allocate new reader state.  The reference implementation is using
       -- @'ChainProducerState.initReader'@ to mutate its internal state.
       improveReadPoint   :: r -> [Point block] -> m (Maybe (Point block, Point block)),
       -- ^ find the most optimal intersection between received list of
       -- @'Point'@s and producers chain.  The second point is the current tip.
       tryReadChainUpdate :: r -> m (Maybe (ChainUpdate block)),
       -- ^ compute chain update for a given reader.  This is a non blocking
       -- operation.
       readChainUpdate    :: r -> m (ChainUpdate block)
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

data Promise b
    = Fullfilled b
    | Awaiting
    deriving (Eq, Show)

-- |
-- The interface usef by block layer of the producer side of the chain consumer
-- protocol.
--
data BlockProducerHandlers blockHeader blockBody m = BlockProducerHandlers {
      getBlock :: Point blockHeader -> m (Maybe (Promise blockBody)),
      -- ^ get block from the block storage layer.  It is non blocking.
      -- @'Promise'@ indicates that a block has been requested and the node is
      -- awaiting it.
      awaitBlock :: Point blockHeader -> m (Maybe blockBody)
      -- ^ blocking operation of getting a block.  It will block until a block is
      -- received from a peer.
    }

-- |
-- The interface used by block layer of the consumer side of the chain consumer
-- protocol.
--
-- TODO: add block validation
data BlockConsumerHandlers blockHeader blockBody m = BlockConsumerHandlers {
      putBlock :: Point blockHeader -> Promise blockBody -> m (),
      -- ^ store a block body received from the network
      verifyBlockBody :: blockHeader -> blockBody -> Bool
      -- ^ verify block body
    }
