{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Queries
module Ouroboros.Consensus.Storage.ChainDB.Impl.Query
  ( -- * Queries
    getCurrentChain
  , getCurrentLedger
  , getPastLedger
  , getHeaderStateHistory
  , getTipBlock
  , getTipHeader
  , getTipPoint
  , getBlockComponent
  , getIsFetched
  , getIsValid
  , getIsInvalidBlock
  , getMaxSlotNo
    -- * Low-level queries
  , getAnyKnownBlock
  , getAnyKnownBlockComponent
  , getAnyBlockComponent
    -- * Auxiliary
  , projVolatileDbBlockComponent
  ) where

import           Control.Monad (join)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (MaxSlotNo, maxSlotNoFromWithOrigin)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderStateHistory (HeaderStateHistory)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (WithFingerprint (..))

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     ChainDB, ChainDbFailure (..), InvalidBlockReason)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (ImmDB,
                     ImmDbSerialiseConstraints)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import           Ouroboros.Consensus.Storage.VolatileDB (VolatileDB)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

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
     , ConsensusProtocol (BlockProtocol blk)
     )
  => ChainDbEnv m blk
  -> STM m (AnchoredFragment (Header blk))
getCurrentChain CDB{..} =
    AF.anchorNewest k <$> readTVar cdbChain
  where
    SecurityParam k = configSecurityParam cdbTopLevelConfig

getCurrentLedger :: IOLike m => ChainDbEnv m blk -> STM m (ExtLedgerState blk)
getCurrentLedger CDB{..} = LgrDB.getCurrentState cdbLgrDB

getPastLedger ::
     (HasHeader blk, IOLike m)
  => ChainDbEnv m blk -> Point blk -> STM m (Maybe (ExtLedgerState blk))
getPastLedger CDB{..} = LgrDB.getPastState cdbLgrDB

getHeaderStateHistory ::
     IOLike m
  => ChainDbEnv m blk -> STM m (HeaderStateHistory blk)
getHeaderStateHistory CDB{..} = LgrDB.getHeaderStateHistory cdbLgrDB

getTipBlock
  :: forall m blk.
     ( IOLike m
     , HasHeader blk
     , HasHeader (Header blk)
     , ImmDbSerialiseConstraints blk
     )
  => ChainDbEnv m blk
  -> m (Maybe blk)
getTipBlock cdb@CDB{..} = do
    tipPoint <- atomically $ getTipPoint cdb
    case pointToWithOriginRealPoint tipPoint of
      Origin      -> return Nothing
      NotOrigin p -> Just <$> getAnyKnownBlock cdbImmDB cdbVolatileDB p

getTipHeader
  :: forall m blk.
     ( IOLike m
     , HasHeader blk
     , HasHeader (Header blk)
     , ImmDbSerialiseConstraints blk
     )
  => ChainDbEnv m blk
  -> m (Maybe (Header blk))
getTipHeader CDB{..} = do
    anchorOrHdr <- AF.head <$> atomically (readTVar cdbChain)
    case anchorOrHdr of
      Right hdr   -> return $ Just hdr
      Left anchor ->
        case pointToWithOriginRealPoint (castPoint (AF.anchorToPoint anchor)) of
          Origin      -> return Nothing
          NotOrigin p ->
            -- In this case, the fragment is empty but the anchor point is not
            -- genesis. It must be that the VolatileDB got emptied and that our
            -- current tip is now the tip of the ImmutableDB.

            -- Note that we can't use 'getBlockAtTip' because a block might have
            -- been appended to the ImmutableDB since we obtained 'anchorOrHdr'.
            fmap Just . anchorMustBeThere =<<
              ImmDB.getBlockComponentWithPoint cdbImmDB GetHeader p
  where
    anchorMustBeThere :: Maybe a -> a
    anchorMustBeThere Nothing  = error "block at tip of ImmutableDB missing"
    anchorMustBeThere (Just a) = a

getTipPoint
  :: forall m blk. (IOLike m, HasHeader (Header blk))
  => ChainDbEnv m blk -> STM m (Point blk)
getTipPoint CDB{..} =
    (castPoint . AF.headPoint) <$> readTVar cdbChain

getBlockComponent
  :: forall m blk b.
     ( MonadCatch m
     , HasHeader blk
     , ImmDbSerialiseConstraints blk
     )
  => ChainDbEnv m blk
  -> BlockComponent (ChainDB m blk) b
  -> RealPoint blk -> m (Maybe b)
getBlockComponent CDB{..} = getAnyBlockComponent cdbImmDB cdbVolatileDB

getIsFetched
  :: forall m blk. IOLike m
  => ChainDbEnv m blk -> STM m (Point blk -> Bool)
getIsFetched CDB{..} = basedOnHash <$> VolatileDB.getIsMember cdbVolatileDB
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

getIsValid
  :: forall m blk. (IOLike m, HasHeader blk)
  => ChainDbEnv m blk
  -> STM m (RealPoint blk -> Maybe Bool)
getIsValid CDB{..} = do
    prevApplied <- LgrDB.getPrevApplied cdbLgrDB
    invalid     <- forgetFingerprint <$> readTVar cdbInvalid
    return $ \pt@(RealPoint _ hash) ->
      -- Blocks from the future that were valid according to the ledger but
      -- that exceeded the max clock skew will be in 'prevApplied' *and*
      -- 'invalid'. So we first check 'invalid' before 'prevApplied'. See
      -- #2413.
      if | Map.member hash invalid   -> Just False
         | Set.member pt prevApplied -> Just True
         | otherwise                 -> Nothing

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
    volatileDbMaxSlotNo    <- VolatileDB.getMaxSlotNo cdbVolatileDB
    return $ curChainMaxSlotNo `max` volatileDbMaxSlotNo

{-------------------------------------------------------------------------------
  Unifying interface over the immutable DB and volatile DB, but independent
  of the ledger DB. These functions therefore do not require the entire
  Chain DB to have been initialized.
-------------------------------------------------------------------------------}

-- | Variant of 'getAnyBlockComponent' instantiated with 'GetBlock'.
getAnyKnownBlock
  :: forall m blk.
     ( MonadCatch m
     , HasHeader blk
     , ImmDbSerialiseConstraints blk
     )
  => ImmDB m blk
  -> VolatileDB m blk
  -> RealPoint blk
  -> m blk
getAnyKnownBlock immDB volatileDB =
    join . getAnyKnownBlockComponent immDB volatileDB GetBlock

-- | Wrapper around 'getAnyBlockComponent' for blocks we know should exist.
--
-- If the block does not exist, this indicates disk failure.
getAnyKnownBlockComponent
  :: forall m blk b.
     ( MonadCatch m
     , HasHeader blk
     , ImmDbSerialiseConstraints blk
     )
  => ImmDB m blk
  -> VolatileDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> RealPoint blk
  -> m b
getAnyKnownBlockComponent immDB volatileDB blockComponent p = do
    mBlock <- mustExist p <$> getAnyBlockComponent immDB volatileDB blockComponent p
    case mBlock of
      Right b  -> return b
      Left err -> throwM err

-- | Get a block component from either the immutable DB or volatile DB.
--
-- Returns 'Nothing' if the 'Point' is unknown.
-- Throws 'NoGenesisBlockException' if the 'Point' refers to the genesis block.
getAnyBlockComponent
  :: forall m blk b.
     ( MonadCatch m
     , HasHeader blk
     , ImmDbSerialiseConstraints blk
     )
  => ImmDB m blk
  -> VolatileDB m blk
  -> BlockComponent (ChainDB m blk) b
  -> RealPoint blk
  -> m (Maybe b)
getAnyBlockComponent immDB volatileDB blockComponent p = do
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
    mbVolatileB <- VolatileDB.getBlockComponent
                     volatileDB
                     (projVolatileDbBlockComponent blockComponent)
                     hash
    case mbVolatileB of
      Just b -> return $ Just b
      Nothing    -> do
        -- ImmDB will throw an exception if we ask for a block past the tip
        immTipSlot <- ImmDB.getSlotNoAtTip immDB
        if NotOrigin (realPointSlot p) > immTipSlot
          -- It's not supposed to be in the ImmutableDB and the VolatileDB
          -- didn't contain it, so return 'Nothing'.
          then return Nothing
          else ImmDB.getBlockComponentWithPoint immDB blockComponent p
  where
    hash = realPointHash p

-- TODO will be gone when #2264 is done
projVolatileDbBlockComponent ::
     forall m blk b. Monad m
  => BlockComponent (ChainDB    m blk) b
  -> BlockComponent (VolatileDB m blk) b
projVolatileDbBlockComponent = go
  where
    go ::
         BlockComponent (ChainDB    m blk) b'
      -> BlockComponent (VolatileDB m blk) b'
    go = \case
      GetRawBlock   -> GetRawBlock
      GetRawHeader  -> GetRawHeader
      GetHash       -> GetHash
      GetSlot       -> GetSlot
      GetIsEBB      -> GetIsEBB
      GetBlockSize  -> GetBlockSize
      GetHeaderSize -> GetHeaderSize
      GetNestedCtxt -> GetNestedCtxt
      GetPure a     -> GetPure a
      GetApply f bc -> GetApply (go f) (go bc)
      -- The ImmutableDB isn't aware of the @blk@ type and thus the ChainDB
      -- must decode the blocks when translating from a ChainDB
      -- 'BlockComponent' to an ImmutableDB 'BlockComponent'. As decoding can
      -- fail, the return type of these two constructors is monadic. The
      -- VolatileDB /is/ aware of the @blk@ type and can decode them when
      -- getting the 'BlockComponent', so no decoding needs to be done in the
      -- translation, this means that these return types are not monadic.
      -- Hence the need to make them monadic here. This will be gone when
      -- #2264 is done.
      GetBlock      -> return <$> GetBlock
      GetHeader     -> return <$> GetHeader

mustExist :: (Typeable blk, StandardHash blk)
          => RealPoint blk -> Maybe b -> Either ChainDbFailure b
mustExist p Nothing  = Left  $ ChainDbMissingBlock p
mustExist _ (Just b) = Right $ b
