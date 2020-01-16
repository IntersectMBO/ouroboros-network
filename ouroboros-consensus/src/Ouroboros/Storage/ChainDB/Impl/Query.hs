{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Queries
module Ouroboros.Storage.ChainDB.Impl.Query
  ( -- * Queries
    getCurrentChain
  , getCurrentLedger
  , getPastLedger
  , getTipBlock
  , getTipHeader
  , getTipPoint
  , getTipBlockNo
  , getBlockComponent
  , getIsFetched
  , getIsInvalidBlock
  , getMaxSlotNo
    -- * Low-level queries
  , getAnyKnownBlock
  , getAnyKnownBlockComponent
  , getAnyBlockComponent
  ) where

import           Control.Monad (join)
import qualified Data.Map.Strict as Map
import           Data.Typeable

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo, ChainHash (..), HasHeader,
                     HeaderHash, MaxSlotNo, Point, StandardHash, castPoint,
                     genesisPoint, maxSlotNoFromWithOrigin, pointHash,
                     pointSlot)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (WithFingerprint)

import           Ouroboros.Storage.ChainDB.API (BlockComponent (..), ChainDB,
                     ChainDbError (..), ChainDbFailure (..),
                     InvalidBlockReason)
import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.Impl.Types
import           Ouroboros.Storage.ChainDB.Impl.VolDB (VolDB)
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

-- | Return the last @k@ headers.
--
-- While the in-memory fragment ('cdbChain') might temporarily be longer than
-- @k@ (until the background thread has copied those blocks to the
-- ImmutableDB), this function will never return a fragment longer than @k@.
--
-- The anchor point of the returned fragment will be the most recent
-- \"immutable\" block, i.e. a block that cannot be rolled back. In
-- ChainDB.md, we call this block @i@.
--
-- Note that the returned fragment may be shorter than @k@ in case the whole
-- chain itself is shorter than @k@ or in case the VolatileDB was corrupted.
-- In the latter case, we don't take blocks already in the ImmutableDB into
-- account, as we know they /must/ have been \"immutable\" at some point, and,
-- therefore, /must/ still be \"immutable\".
getCurrentChain
  :: forall m blk.
     ( IOLike m
     , HasHeader (Header blk)
     , OuroborosTag (BlockProtocol blk)
     )
  => ChainDbEnv m blk
  -> STM m (AnchoredFragment (Header blk))
getCurrentChain CDB{..} =
    AF.anchorNewest k <$> readTVar cdbChain
  where
    SecurityParam k = protocolSecurityParam cdbNodeConfig

getCurrentLedger :: IOLike m => ChainDbEnv m blk -> STM m (ExtLedgerState blk)
getCurrentLedger CDB{..} = LgrDB.getCurrentState cdbLgrDB

getPastLedger :: (IOLike m, UpdateLedger blk)
              => ChainDbEnv m blk -> Point blk -> m (Maybe (ExtLedgerState blk))
getPastLedger CDB{..} = LgrDB.getPastState cdbLgrDB

getTipBlock
  :: forall m blk. (IOLike m, HasHeader blk, HasHeader (Header blk))
  => ChainDbEnv m blk
  -> m (Maybe blk)
getTipBlock cdb@CDB{..} = do
    tipPoint <- atomically $ getTipPoint cdb
    if tipPoint == genesisPoint
      then return Nothing
      else Just <$> getAnyKnownBlock cdbImmDB cdbVolDB tipPoint

getTipHeader
  :: forall m blk.
     ( IOLike m
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbEnv m blk
  -> m (Maybe (Header blk))
getTipHeader CDB{..} = do
    anchorOrHdr <- AF.head <$> atomically (readTVar cdbChain)
    case anchorOrHdr of
      Right hdr
        -> return $ Just hdr
      Left anchor
        | anchor == genesisPoint
        -> return Nothing
        | otherwise
          -- In this case, the fragment is empty but the anchor point is not
          -- genesis. It must be that the VolatileDB got emptied and that our
          -- current tip is now the tip of the ImmutableDB.

          -- Note that we can't use 'getBlockAtTip' because a block might have
          -- been appended to the ImmutableDB since we obtained 'anchorOrHdr'.
        -> fmap Just . anchorMustBeThere =<<
           ImmDB.getBlockComponentWithPoint cdbImmDB GetHeader (castPoint anchor)
  where
    anchorMustBeThere :: Maybe a -> a
    anchorMustBeThere Nothing  = error "block at tip of ImmutableDB missing"
    anchorMustBeThere (Just a) = a

getTipPoint
  :: forall m blk. (IOLike m, HasHeader (Header blk))
  => ChainDbEnv m blk -> STM m (Point blk)
getTipPoint CDB{..} =
    (castPoint . AF.headPoint) <$> readTVar cdbChain

getTipBlockNo
  :: forall m blk. (IOLike m, HasHeader (Header blk))
  => ChainDbEnv m blk -> STM m BlockNo
getTipBlockNo CDB{..} = do
    mbTipBlockNo <- AF.headBlockNo <$> readTVar cdbChain
    -- If the current chain is empty, then look at 'cdbImmBlockNo', see its
    -- invariant.
    maybe (readTVar cdbImmBlockNo) return mbTipBlockNo

getBlockComponent
  :: forall m blk b. (MonadCatch m , HasHeader blk)
  => ChainDbEnv m blk
  -> BlockComponent (ChainDB m blk) b
  -> Point blk -> m (Maybe b)
getBlockComponent CDB{..} = getAnyBlockComponent cdbImmDB cdbVolDB

getIsFetched
  :: forall m blk. IOLike m
  => ChainDbEnv m blk -> STM m (Point blk -> Bool)
getIsFetched CDB{..} = basedOnHash <$> VolDB.getIsMember cdbVolDB
  where
    -- The volatile DB indexes by hash only, not by points. However, it should
    -- not be possible to have two points with the same hash but different
    -- slot numbers.
    basedOnHash :: (HeaderHash blk -> Bool) -> Point blk -> Bool
    basedOnHash f p =
        case pointHash p of
          BlockHash hash -> f hash
          GenesisHash    -> False

getIsInvalidBlock
  :: forall m blk. (IOLike m, HasHeader blk)
  => ChainDbEnv m blk
  -> STM m (WithFingerprint (HeaderHash blk -> Maybe (InvalidBlockReason blk)))
getIsInvalidBlock CDB{..} =
  fmap (fmap (fmap invalidBlockReason) . flip Map.lookup) <$> readTVar cdbInvalid

getMaxSlotNo
  :: forall m blk. (IOLike m, HasHeader (Header blk))
  => ChainDbEnv m blk -> STM m MaxSlotNo
getMaxSlotNo CDB{..} = do
    -- Note that we need to look at both the current chain and the VolatileDB
    -- in all cases (even when the VolatileDB is not empty), because the
    -- VolatileDB might have been corrupted.
    --
    -- For example, imagine the VolatileDB has been corrupted so that it only
    -- contains block 9'. The ImmutableDB contains blocks 1-10. The max slot
    -- of the current chain will be 10 (being the anchor point of the empty
    -- current chain), while the max slot of the VolatileDB will be 9.
    curChainMaxSlotNo <- maxSlotNoFromWithOrigin . AF.headSlot
                     <$> readTVar cdbChain
    volDBMaxSlotNo    <- VolDB.getMaxSlotNo cdbVolDB
    return $ curChainMaxSlotNo `max` volDBMaxSlotNo

{-------------------------------------------------------------------------------
  Unifying interface over the immutable DB and volatile DB, but independent
  of the ledger DB. These functions therefore do not require the entire
  Chain DB to have been initialized.
-------------------------------------------------------------------------------}

-- | Variant of 'getAnyBlockComponent' instantiated with 'GetBlock'.
getAnyKnownBlock
  :: forall m blk. (MonadCatch m, HasHeader blk)
  => ImmDB m blk
  -> VolDB m blk
  -> Point blk
  -> m blk
getAnyKnownBlock immDB volDB = join . getAnyKnownBlockComponent immDB volDB GetBlock

-- | Wrapper around 'getAnyBlockComponent' for blocks we know should exist.
--
-- If the block does not exist, this indicates disk failure.
getAnyKnownBlockComponent
  :: forall m blk b. (MonadCatch m, HasHeader blk)
  => ImmDB m blk
  -> VolDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> Point blk
  -> m b
getAnyKnownBlockComponent immDB volDB blockComponent p = do
    mBlock <- mustExist p <$> getAnyBlockComponent immDB volDB blockComponent p
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

-- | Get a block component from either the immutable DB or volatile DB.
--
-- Returns 'Nothing' if the 'Point' is unknown.
-- Throws 'NoGenesisBlockException' if the 'Point' refers to the genesis block.
getAnyBlockComponent
  :: forall m blk b. (MonadCatch m, HasHeader blk)
  => ImmDB m blk
  -> VolDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> Point blk
  -> m (Maybe b)
getAnyBlockComponent immDB volDB blockComponent p =
    case pointHash p of
      GenesisHash    -> throwM NoGenesisBlock
      BlockHash hash -> do
        -- Note: to determine whether a block is in the ImmutableDB, we can
        -- look at the slot of its tip, which we'll call @immTipSlot@. If the
        -- slot of the requested point > @immTipSlot@, then the block will not
        -- be in the ImmutableDB but in the VolatileDB. However, there is a
        -- race condition here: if between the time we got @immTipSlot@ and
        -- the time we look up the block in the VolatileDB the block was moved
        -- from the VolatileDB to the ImmutableDB, and it was deleted from the
        -- VolatileDB, we won't find the block, even though it is in the
        -- ChainDB.
        --
        -- Therefore, we first query the VolatileDB and if the block is not in
        -- it, then we can get @immTipSlot@ and compare it to the slot of the
        -- requested point. If the slot <= @immTipSlot@ it /must/ be in the
        -- ImmutableDB (no race condition here).
        mbVolB <- VolDB.getBlockComponent volDB blockComponent hash
        case mbVolB of
          Just b -> return $ Just b
          Nothing    -> do
            -- ImmDB will throw an exception if we ask for a block past the tip
            immTipSlot <- ImmDB.getSlotNoAtTip immDB
            if pointSlot p > immTipSlot
              -- It's not supposed to be in the ImmutableDB and the VolatileDB
              -- didn't contain it, so return 'Nothing'.
              then return Nothing
              else ImmDB.getBlockComponentWithPoint immDB blockComponent p

mustExist :: (Typeable blk, StandardHash blk)
          => Point blk -> Maybe b -> Either ChainDbFailure b
mustExist p Nothing  = Left  $ ChainDbMissingBlock p
mustExist _ (Just b) = Right $ b
