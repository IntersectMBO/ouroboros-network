{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Ouroboros.Consensus.Storage.VolatileDB.API (
    -- * API
    VolatileDB (..)
    -- * Types
  , BlockInfo (..)
    -- * Derived functionality
  , withDB
  , getIsMember
  , getPredecessor
  , getKnownBlock
  , getKnownHeader
  , getKnownBlockComponent
  ) where

import           Data.Maybe (isJust)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..))

import           Ouroboros.Network.Block (MaxSlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common (BlockComponent (..),
                     DB (..))

import           Ouroboros.Consensus.Storage.VolatileDB.Error

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

data VolatileDB m blk = VolatileDB {
      -- | Close the VolatileDB.
      --
      -- NOTE: idempotent after a manual closure, but not after an automatic
      -- closure in case of an 'UnexpectedError'. In that case, closing it
      -- again will cause a 'ClosedDBError' wrapping the original
      -- 'UnexpectedError' to be thrown.
      closeDB             :: HasCallStack => m ()
      -- | Return the request block component for the block with the given
      -- hash. When not in the VolatileDB, 'Nothing' is returned.
    , getBlockComponent   :: forall b. HasCallStack
                          => BlockComponent (VolatileDB m blk) b
                          -> HeaderHash blk
                          -> m (Maybe b)
      -- | Store the given block in the VolatileDB.
      --
      -- Returns after the block has been written to disk.
    , putBlock            :: HasCallStack => blk -> m ()
      -- | Return a function that returns the successors of the block with the
      -- given hash.
      --
      -- This function will return a non-empty set for any block of which a
      -- predecessor has been added to the VolatileDB and will return an empty
      -- set if no successors for the given block have been added to the
      -- VolatileDB (yet).
      --
      -- Note that it is not required that the given block has been added to
      -- the VolatileDB.
    , filterByPredecessor :: HasCallStack => STM m (ChainHash blk -> Set (HeaderHash blk))
      -- | Return a function that returns the 'BlockInfo' of the block with
      -- the given hash or 'Nothing' if the block is not found in the
      -- VolatileDB.
    , getBlockInfo        :: HasCallStack => STM m (HeaderHash blk -> Maybe (BlockInfo blk))
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
    , garbageCollect      :: HasCallStack => SlotNo -> m ()
      -- | Return the highest slot number ever stored by the VolatileDB.
    , getMaxSlotNo        :: HasCallStack => STM m MaxSlotNo
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "VolatileDB" (VolatileDB m blk)

{-------------------------------------------------------------------------------
  Parameterisation
-------------------------------------------------------------------------------}

instance DB (VolatileDB m blk) where
  type DBBlock      (VolatileDB m blk) = blk
  type DBHeader     (VolatileDB m blk) = Header blk
  type DBHeaderHash (VolatileDB m blk) = HeaderHash blk
  type DBNestedCtxt (VolatileDB m blk) = SomeBlock (NestedCtxt Header) blk

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- | The information that the user has to provide for each new block.
data BlockInfo blk = BlockInfo {
      biHash         :: !(HeaderHash blk)
    , biSlotNo       :: !SlotNo
    , biBlockNo      :: !BlockNo
    , biPrevHash     :: !(ChainHash blk)
    , biIsEBB        :: !IsEBB
    , biHeaderOffset :: !Word16
    , biHeaderSize   :: !Word16
    }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | Open the database using the given function, perform the given action
-- using the database, and closes the database using its 'closeDB' function,
-- in case of success or when an exception was raised.
withDB :: (HasCallStack, MonadThrow m)
       => m (VolatileDB m blk)
          -- ^ How to open the database
       -> (VolatileDB m blk -> m a)
          -- ^ Action to perform using the database
       -> m a
withDB openDB = bracket openDB closeDB

getIsMember ::
     Functor (STM m)
  => VolatileDB m blk
  -> STM m (HeaderHash blk -> Bool)
getIsMember = fmap (isJust .) . getBlockInfo

getPredecessor ::
     Functor (STM m)
  => VolatileDB m blk
  -> STM m (HeaderHash blk -> Maybe (ChainHash blk))
getPredecessor = fmap (fmap biPrevHash .) . getBlockInfo

getKnownBlock ::
     (MonadThrow m, HasHeader blk)
  => VolatileDB m blk
  -> HeaderHash blk
  -> m blk
getKnownBlock db = getKnownBlockComponent db GetBlock

getKnownHeader ::
     (MonadThrow m, HasHeader blk)
  => VolatileDB m blk
  -> HeaderHash blk
  -> m (Header blk)
getKnownHeader db = getKnownBlockComponent db GetHeader

getKnownBlockComponent ::
    (MonadThrow m, HasHeader blk)
  => VolatileDB m blk
  -> BlockComponent (VolatileDB m blk) b
  -> HeaderHash blk
  -> m b
getKnownBlockComponent db blockComponent hash = do
    mBlock <- mustExist db hash <$>
      getBlockComponent db blockComponent hash
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

mustExist ::
     forall proxy blk b. (StandardHash blk, Typeable blk)
  => proxy blk
  -> HeaderHash blk
  -> Maybe b
  -> Either VolatileDBError b
mustExist _ hash = \case
    Nothing -> Left  $ UnexpectedError $ MissingBlock (Proxy @blk) hash
    Just b  -> Right $ b
