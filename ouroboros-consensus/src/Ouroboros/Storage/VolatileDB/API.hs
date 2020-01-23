{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module Ouroboros.Storage.VolatileDB.API
  ( VolatileDB(..)
  , withDB

  , module Ouroboros.Storage.VolatileDB.Types
  ) where

import           Data.ByteString.Builder (Builder)
import           Data.Set (Set)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..))

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common (BlockComponent (..), DB (..))
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
      closeDB           :: HasCallStack => m ()
    , isOpenDB          :: HasCallStack => m Bool
    , reOpenDB          :: HasCallStack => m ()
    , getBlockComponent :: forall b. HasCallStack
                        => BlockComponent (VolatileDB blockId m) b
                        -> blockId
                        -> m (Maybe b)
    , putBlock          :: HasCallStack => BlockInfo blockId -> Builder -> m ()
    , getBlockIds       :: HasCallStack => m [blockId]
      -- | Return a function that returns the successors of the block with the
      -- given @blockId@.
      --
      -- This function will return a non-empty set for any block of which a
      -- predecessor has been added to the VolatileDB and will return an empty
      -- set if no successors for the given block have been added to the
      -- VolatileDB (yet).
      --
      -- Note that it is not required that the given block has been added to
      -- the VolatileDB.
    , getSuccessors     :: HasCallStack => STM m (WithOrigin blockId -> Set blockId)
      -- | Return a function that returns the predecessor of the block with
      -- the given @blockId@.
      --
      -- PRECONDITION: the block must be a member of the VolatileDB, you can
      -- use 'getIsMember' to check this.
    , getPredecessor    :: HasCallStack => STM m (blockId -> WithOrigin blockId)
      -- | Try to remove all blocks with a slot number less than the given
      -- one.
      --
      -- = Context
      --
      -- When the current chain changes, blocks older than @k@, i.e., blocks
      -- that are followed by @k@ blocks or more, become /immutable/. Whenever
      -- this happens, we schedule a garbage collection on the VolatileDB that
      -- will try to remove blocks older than the most recent immutable block,
      -- as such blocks will never be adopted. There's no point in storing
      -- them anymore.
      --
      -- = Block number vs slot number
      --
      -- While we typically talk in terms of /block numbers/ when discussing
      -- immutability, i.e., /@k@ blocks/, we use /slot number/ for garbage
      -- collection. We schedule a garbage collection for blocks with a /slot
      -- number/ less than the slot number of the immutable block, as opposed
      -- to the block number. The reason for this is that the VolatileDB is
      -- not aware of block numbers, only of slot numbers.
      --
      -- By using slot numbers for garbage collection, we might not /yet/ have
      -- garbage collected some blocks that could never be adopted again and
      -- that we would have garbage collected when using block numbers. This
      -- is harmless. The opposite direction is more important and
      -- problematic: garbage collecting a block that we might want to adopt
      -- after all. Say we have mistakenly garbage collected such a block, in
      -- that case the following would be true:
      --
      -- 1. The block has a slot number older than the immutable block's slot
      --    number: otherwise we wouldn't have mistakenly garbage collected
      --    it.
      --
      -- 2. The block has a block number greater than the immutable block's
      --    block number: otherwise we wouldn't want to adopt it, as it would
      --    have been older than @k@.
      --
      -- 3. The block is a part of a fork fitting on the immutable block. As
      --    we cannot roll back this block, all forks we could ever adopt
      --    would have to go through this block.
      --
      -- As slot numbers grow monotonically within a chain, all forks starting
      -- after the immutable block will only contain blocks with slot numbers
      -- greater (or equal to in case of EBBs) than the immutable block's slot
      -- number. This directly contradicts (1), so we will /never/ garbage
      -- collect a block that we might still want to adopt.
      --
      -- = Less than vs. less than or equal to
      --
      -- Note that we remove blocks with a slot number /less than/ the given
      -- slot number, but not /equal to/ it. In practice, this off-by-one
      -- difference will not matter in terms of disk space usage, because as
      -- soon as the chain grows again by at least one block, those blocks
      -- will be removed anyway. The reason for @<@ opposed to @<=@ is to
      -- avoid issues with /EBBs/, which have the same slot number as the
      -- block after it.
    , garbageCollect    :: HasCallStack => SlotNo -> m ()
    , getIsMember       :: HasCallStack => STM m (blockId -> Bool)
      -- | Return the highest slot number ever stored by the VolatileDB.
    , getMaxSlotNo      :: HasCallStack => STM m MaxSlotNo
} deriving NoUnexpectedThunks via OnlyCheckIsWHNF "VolatileDB" (VolatileDB blockId m)


instance DB (VolatileDB blockId m) where
  -- The VolatileDB doesn't have the ability to parse blocks and headers, it
  -- only returns raw blocks and headers.
  type DBBlock      (VolatileDB blockId m) = ()
  type DBHeader     (VolatileDB blockId m) = ()
  type DBHeaderHash (VolatileDB blockId m) = blockId
