{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Ouroboros.Storage.VolatileDB.API
  ( VolatileDB(..)
  , withDB

  , module Ouroboros.Storage.VolatileDB.Types
  ) where

import Control.Monad.Catch (MonadMask, bracket)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)

import GHC.Stack (HasCallStack)

import Ouroboros.Storage.VolatileDB.Types

-- | Open the database using the given function, perform the given action
-- using the database, and closes the database using its 'closeDB' function,
-- in case of success or when an exception was raised.
withDB :: (HasCallStack, MonadMask m)
       => m (VolatileDB blockId m)
          -- ^ How to open the database
       -> (VolatileDB blockId m -> m a)
          -- ^ Action to perform using the database
       -> m a
withDB openDB = bracket openDB closeDB

data VolatileDB blockId m = VolatileDB {
      closeDB  :: HasCallStack => m ()
    , isOpenDB :: HasCallStack => m Bool
    , reOpenDB :: HasCallStack => m ()
    , getBlock :: HasCallStack => blockId -> m (Maybe ByteString)
    , putBlock :: HasCallStack => blockId -> Builder -> m ()
    , garbageCollect :: HasCallStack => Slot -> m ()
}
