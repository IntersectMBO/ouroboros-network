{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Queries
module Ouroboros.Storage.ChainDB.Impl.Query
  ( -- * Queries
    getCurrentChain
  , getCurrentLedger
  , getTipBlock
  , getTipHeader
  , getTipPoint
  , getBlock
  , getIsFetched
  , getIsInvalidBlock
    -- * Low-level queries
  , getAnyKnownBlock
  ) where

import qualified Data.Map.Strict as Map

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (ChainHash (..), HasHeader, HeaderHash,
                     Point, castPoint, genesisPoint, pointHash, pointSlot)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure (..))

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
     ( MonadSTM m
     , HasHeader (Header blk)
     , OuroborosTag (BlockProtocol blk)
     )
  => ChainDbEnv m blk
  -> STM m (AnchoredFragment (Header blk))
getCurrentChain CDB{..} =
    AF.anchorNewest k <$> readTVar cdbChain
  where
    SecurityParam k = protocolSecurityParam cdbNodeConfig

getCurrentLedger :: MonadSTM m => ChainDbEnv m blk -> STM m (ExtLedgerState blk)
getCurrentLedger CDB{..} = LgrDB.getCurrentState cdbLgrDB

getTipBlock
  :: forall m blk.
     ( MonadCatch m
     , MonadSTM m
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbEnv m blk
  -> m (Maybe blk)
getTipBlock cdb@CDB{..} = do
    tipPoint <- atomically $ getTipPoint cdb
    if tipPoint == genesisPoint
      then return Nothing
      else Just <$> getAnyKnownBlock cdbImmDB cdbVolDB tipPoint

getTipHeader
  :: forall m blk.
     ( MonadCatch m
     , MonadSTM   m
     , GetHeader blk
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
        -> anchorMustBeThere <$>
           ImmDB.getBlockWithPoint cdbImmDB (castPoint anchor)
  where
    anchorMustBeThere :: Maybe blk -> Maybe (Header blk)
    anchorMustBeThere Nothing    = error "block at tip of ImmutableDB missing"
    anchorMustBeThere (Just blk) = Just (getHeader blk)

getTipPoint
  :: forall m blk. (MonadSTM m, HasHeader (Header blk))
  => ChainDbEnv m blk -> STM m (Point blk)
getTipPoint CDB{..} =
    (castPoint . AF.headPoint) <$> readTVar cdbChain

getBlock
  :: forall m blk. (MonadCatch m , HasHeader blk)
  => ChainDbEnv m blk
  -> Point blk -> m (Maybe blk)
getBlock CDB{..} = getAnyBlock cdbImmDB cdbVolDB

getIsFetched
  :: forall m blk. MonadSTM m
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
  :: forall m blk. (MonadSTM m, HasHeader blk)
  => ChainDbEnv m blk -> STM m (HeaderHash blk -> Bool)
getIsInvalidBlock CDB{..} = flip Map.member <$> readTVar cdbInvalid

{-------------------------------------------------------------------------------
  Unifying interface over the immutable DB and volatile DB, but independent
  of the ledger DB. These functions therefore do not require the entire
  Chain DB to have been initialized.
-------------------------------------------------------------------------------}

-- | Wrapper around 'getAnyBlock' for blocks we know should exist
--
-- If the block does not exist, this indicates disk failure.
getAnyKnownBlock
  :: forall m blk. (MonadCatch m, HasHeader blk)
  => ImmDB m blk
  -> VolDB m blk
  -> Point blk
  -> m blk
getAnyKnownBlock immDB volDB p = do
    mBlock <- mustExist p <$> getAnyBlock immDB volDB p
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

-- | Get a block from either the immutable DB or volatile DB
--
-- Returns 'Nothing' if the block is unknown.
-- Throws 'NoGenesisBlockException' if the 'Point' refers to the genesis block.
getAnyBlock
  :: forall m blk. (MonadCatch m, HasHeader blk)
  => ImmDB m blk
  -> VolDB m blk
  -> Point blk
  -> m (Maybe blk)
getAnyBlock immDB volDB p = case pointHash p of
    GenesisHash    -> throwM $ NoGenesisBlock @blk
    BlockHash hash -> do
      -- Note: to determine whether a block is in the ImmutableDB, we can look
      -- at the slot of its tip, which we'll call @immTipSlot@. If the slot of
      -- the requested point > @immTipSlot@, then the block will not be in the
      -- ImmutableDB but in the VolatileDB. However, there is a race condition
      -- here: if between the time we got @immTipSlot@ and the time we look up
      -- the block in the VolatileDB the block was moved from the VolatileDB
      -- to the ImmutableDB, and it was deleted from the VolatileDB, we won't
      -- find the block, even though it is in the ChainDB.
      --
      -- Therefore, we first query the VolatileDB and if the block is not in
      -- it, then we can get @immTipSlot@ and compare it to the slot of the
      -- requested point. If the slot <= @immTipSlot@ it /must/ be in the
      -- ImmutableDB (no race condition here).
      mbVolBlock <- VolDB.getBlock volDB hash
      case mbVolBlock of
        Just block -> return $ Just block
        Nothing    -> do
          -- ImmDB will throw an exception if we ask for a block past the tip
          immTipSlot <- ImmDB.getSlotNoAtTip immDB
          if pointSlot p > immTipSlot
            -- It's not supposed to be in the ImmutableDB and the VolatileDB
            -- didn't contain it, so return 'Nothing'.
            then return Nothing
            else ImmDB.getBlockWithPoint immDB p

mustExist :: Point blk -> Maybe blk -> Either (ChainDbFailure blk) blk
mustExist p Nothing  = Left  $ ChainDbMissingBlock p
mustExist _ (Just b) = Right $ b
