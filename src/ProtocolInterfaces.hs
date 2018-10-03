
module ProtocolInterfaces (
    ConsumerHandlers(..)
  , ProducerHandlers(..)
  ) where

import Chain (ChainUpdate (..), Point (..))


-- | The interface used on the consumer side of the chain consumer protocol
-- to update a local chain to syncronise it with the producer chain.
--
data ConsumerHandlers block m = ConsumerHandlers {
       getChainPoints :: m [Point],
       addBlock       :: block -> m (),
       rollbackTo     :: Point -> m ()
     }


-- | The interface used on the producer side of the chain consumer protocol
-- to query and read the producers chain.
--
data ProducerHandlers block m r = ProducerHandlers {
       findIntersectionRange :: [Point] -> m (Maybe Point),
       establishReaderState  :: Point -> m r,
       updateReaderState     :: r -> Point -> m (),
       tryReadChainUpdate    :: r -> m (Maybe (ChainUpdate block)),
       readChainUpdate       :: r -> m (ChainUpdate block)
     }

