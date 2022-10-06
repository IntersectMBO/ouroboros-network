module Cardano.KESAgent.DirectSerialise
where

import Foreign.Ptr
import Foreign.C.Types

-- | Direct deserialization from raw memory.
--
-- @directDeserialise f@ should allocate a new value of type 'a', and
-- call @f@ with a pointer to the raw memory to be filled. @f@ may be called
-- multiple times, for data structures that store their data in multiple
-- non-contiguous blocks of memory.
--
-- The order in which memory blocks are visited matters.
class DirectDeserialise a where
  directDeserialise :: (Ptr CChar -> CSize -> IO ()) -> IO a

-- | Direct read access to a data structure's raw memory.
--
-- @directSerialise f x@ should call @f@ to expose the raw memory underyling
-- @x@. For data types that store their data in multiple non-contiguous blocks
-- of memory, @f@ may be called multiple times, once for each block.
--
-- The order in which memory blocks are visited matters.
class DirectSerialise a where
  directSerialise :: (Ptr CChar -> CSize -> IO ()) -> a -> IO ()
