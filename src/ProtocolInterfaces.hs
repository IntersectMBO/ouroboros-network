
module ProtocolInterfaces (
    ConsumerHandlers(..)
  , ProducerHandlers(..)
  ) where

import Chain (ChainUpdate (..), Point (..))


data ConsumerHandlers block m = ConsumerHandlers {
       getChainPoints :: m [Point],
       addBlock       :: block -> m (),
       rollbackTo     :: Point -> m ()
     }


data ProducerHandlers block m r = ProducerHandlers {
       findIntersectionRange :: [Point] -> m (Maybe Point),
       establishReaderState  :: Point -> m r,
       updateReaderState     :: r -> Point -> m (),
       tryReadChainUpdate    :: r -> m (Maybe (ChainUpdate block)),
       readChainUpdate       :: r -> m (ChainUpdate block)
     }

