{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Ouroboros.Storage.VolatileDB.API
  ( VolatileDB(..)
  , withDB

  , module Ouroboros.Storage.VolatileDB.Types
  ) where

import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Lazy (ByteString)
import           Data.Set (Set)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..))

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.VolatileDB.Types

-- | Open the database using the given function, perform the given action
-- using the database, and closes the database using its 'closeDB' function,
-- in case of success or when an exception was raised.
withDB :: (HasCallStack, MonadThrow m)
       => m (VolatileDB blockId m)
          -- ^ How to open the database
       -> (VolatileDB blockId m -> m a)
          -- ^ Action to perform using the database
       -> m a
withDB openDB = bracket openDB closeDB

data VolatileDB blockId m = VolatileDB {
      closeDB        :: HasCallStack => m ()
    , isOpenDB       :: HasCallStack => m Bool
    , reOpenDB       :: HasCallStack => m ()
    , getBlock       :: HasCallStack => blockId -> m (Maybe ByteString)
    , putBlock       :: HasCallStack => BlockInfo blockId -> Builder -> m ()
    , getBlockIds    :: HasCallStack => m [blockId]
      -- | Return a function that returns the successors of the block with the
      -- given @blockId@ where 'Nothing' stands for Genesis.
      --
      -- This function will return a non-empty set for any block of which a
      -- predecessor has been added to the VolatileDB and will return an empty
      -- set if no successors for the given block have been added to the
      -- VolatileDB (yet).
      --
      -- Note that it is not required that the given block has been added to
      -- the VolatileDB.
    , getSuccessors  :: HasCallStack => STM m (Maybe blockId -> Set blockId)
      -- | Return a function that returns the predecessor of the block with
      -- the given @blockId@. In case the predecessor is Genesis, 'Nothing' is
      -- returned.
      --
      -- PRECONDITION: the block must be a member of the VolatileDB, you can
      -- use 'getIsMember' to check this.
    , getPredecessor :: HasCallStack => STM m (blockId -> Maybe blockId)
    , garbageCollect :: HasCallStack => SlotNo -> m ()
    , getIsMember    :: HasCallStack => STM m (blockId -> Bool)
      -- | Return the highest slot number ever stored by the VolatileDB.
    , getMaxSlotNo   :: HasCallStack => STM m MaxSlotNo
} deriving NoUnexpectedThunks via OnlyCheckIsWHNF "VolatileDB" (VolatileDB blockId m)
