{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

-- | Model for the 'ImmutableDB' based on a chain.
--
-- The chain is just a list of slots that can be unfilled (@Nothing@) or
-- filled (@Just ByteString@).
module Test.Ouroboros.Storage.ImmutableDB.Model
  ( DBModel(..)
  , dbmCurrentEpoch
  , dbmBlobs
  , initDBModel
  , IteratorModel
  , openDBModel
  , simulateCorruptions
  , rollBackToTip
  ) where

import           Control.Monad (when)
import           Control.Monad.State.Strict (MonadState, get, gets, modify, put)

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Function ((&))
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word (Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Util (lastMaybe)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API.Types (FsPath)
import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Util
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Test.Ouroboros.Storage.ImmutableDB.TestBlock

data DBModel hash = DBModel
  { dbmChain        :: [Maybe ByteString]
    -- ^ 'Nothing' when a slot is empty
  , dbmTip          :: ImmTip
  , dbmEBBs         :: Map EpochNo (hash, ByteString)
    -- ^ The EBB for each 'EpochNo'
  , dbmEpochInfo    :: EpochInfo Identity
  , dbmIterators    :: Map IteratorID (IteratorModel hash)
  , dbmNextIterator :: BaseIteratorID
  } deriving (Show, Generic)

initDBModel :: EpochSize -- ^ We assume fixed epoch size
            -> DBModel hash
initDBModel epochSize = DBModel
  { dbmChain        = []
  , dbmTip          = TipGen
  , dbmEBBs         = Map.empty
  , dbmEpochInfo    = fixedSizeEpochInfo epochSize
  , dbmIterators    = Map.empty
  , dbmNextIterator = initialIteratorID
  }

dbmCurrentEpoch :: DBModel hash -> EpochNo
dbmCurrentEpoch dbm@DBModel{..} =
    case dbmTip of
      TipGen            -> EpochNo 0
      Tip (Right slot ) -> slotToEpoch dbm slot
      Tip (Left epoch') -> epoch'

dbmBlobs :: DBModel hash
         -> Map (EpochSlot, SlotNo) (Either (hash, ByteString) ByteString)
dbmBlobs dbm@DBModel {..} = foldr add ebbs (zip (map SlotNo [0..]) dbmChain)
  where
    add (_,    Nothing)   = id
    add (slot, Just blob) =
      Map.insert (slotToEpochSlot dbm slot, slot) (Right blob)

    ebbs = dbmEBBs
         & Map.map Left
         & Map.mapKeysMonotonic (`EpochSlot` 0)
         & Map.mapKeysMonotonic (\epochSlot ->
             (epochSlot, epochSlotToSlot dbm epochSlot))

-- | Model for an 'Iterator'.
--
-- An iterator is open iff its is present in 'dbmIterators'.
--
-- The model of an iterator is just the list of 'IteratorResult's it streams
-- over. Advancing the iterator will yield the first one and should drop it
-- from the model.
newtype IteratorModel hash = IteratorModel [IteratorResult hash ByteString]
  deriving (Show, Eq, Generic)


{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

openDBModel :: (MonadState (DBModel hash) m, Eq hash)
            => ErrorHandling ImmutableDBError m
            -> (EpochNo -> EpochSize)
            -> (DBModel hash, ImmutableDB hash m)
openDBModel err getEpochSize = (dbModel, db)
  where
    dbModel = initDBModel (getEpochSize 0)
    db = ImmutableDB
      { closeDB           = return ()
      , isOpen            = return True
      , reopen            = \_ -> return ()
      , deleteAfter       = deleteAfterModel
      , getTip            = getTipModel
      , getBinaryBlob     = getBinaryBlobModel     err
      , getEBB            = getEBBModel            err
      , appendBinaryBlob  = appendBinaryBlobModel  err
      , appendEBB         = appendEBBModel         err
      , streamBinaryBlobs = streamBinaryBlobsModel err
      , immutableDBErr    = err
      }

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

lookupEpochSize :: DBModel hash -> EpochNo -> EpochSize
lookupEpochSize DBModel {..} = runIdentity . epochInfoSize dbmEpochInfo

epochSlotToSlot :: DBModel hash -> EpochSlot -> SlotNo
epochSlotToSlot DBModel {..} = runIdentity . epochInfoAbsolute dbmEpochInfo

slotToEpochSlot :: DBModel hash -> SlotNo -> EpochSlot
slotToEpochSlot DBModel {..} = runIdentity . epochInfoBlockRelative dbmEpochInfo

slotToEpoch :: DBModel hash -> SlotNo -> EpochNo
slotToEpoch DBModel {..} = runIdentity . epochInfoEpoch dbmEpochInfo

lookupBySlot :: HasCallStack => SlotNo -> [Maybe b] -> Maybe b
lookupBySlot (SlotNo i) = go i
  where
    go 0 (blob:_)  = blob
    go n (_:blobs) = go (n - 1) blobs
    go _ []        = error "lookupBySlot: index out of bounds"

-- | Rolls back the chain so that the given 'Tip' is the new tip.
--
-- The user is responsible for giving a valid 'Tip', i.e. a tip that points to
-- a filled slot or an existing EBB (Genesis is always valid). This function
-- will not truncate to the last filled slot or EBB itself.
rollBackToTip :: ImmTip -> DBModel hash -> DBModel hash
rollBackToTip tip dbm@DBModel {..} = case tip of
    TipGen    -> (initDBModel firstEpochSize)
        { dbmNextIterator = dbmNextIterator }
      where
        firstEpochSize = lookupEpochSize dbm 0

    Tip (Left epoch) -> dbm
        { dbmChain = rolledBackChain
        , dbmEBBs  = ebbsUpToEpoch epoch
        , dbmTip   = tip
        }
      where
        firstSlotAfter  = epochSlotToSlot dbm (EpochSlot epoch 1)
        rolledBackChain = dbmChain
                        & zip [0..]
                        & takeWhile ((< firstSlotAfter) . fst)
                        & map snd

    Tip (Right slot)
      | slot >= fromIntegral (length dbmChain) -> dbm
      | otherwise                              -> dbm
        { dbmChain = rolledBackChain
        , dbmEBBs  = ebbsUpToEpoch epoch
        , dbmTip   = tip
        }
      where
        EpochSlot epoch _ = slotToEpochSlot dbm slot
        rolledBackChain   = dbmChain
                          & zip [0..]
                          & takeWhile ((<= slot) . fst)
                          & map snd
  where
    ebbsUpToEpoch epoch =
      Map.filterWithKey (\ebbEpoch _ -> ebbEpoch <= epoch) dbmEBBs

-- | Return the filled 'EpochSlot's of the given 'EpochNo' stored in the model.
epochSlotsInEpoch :: DBModel hash -> EpochNo -> [EpochSlot]
epochSlotsInEpoch dbm epoch =
    filter ((== epoch) . _epoch) $
    map (fst . fst) $
    Map.toAscList $ dbmBlobs dbm

-- | Return the filled 'EpochSlot's (including EBBs) before, in, and after the
-- given 'EpochNo'.
filledEpochSlots :: DBModel hash
                 -> EpochNo
                 -> ([EpochSlot], [EpochSlot], [EpochSlot])
filledEpochSlots dbm epoch = (lt, eq, gt)
  where
    increasingEpochSlots = map (fst . fst) $ Map.toAscList $ dbmBlobs dbm
    (lt, geq) = span ((< epoch)      . _epoch) increasingEpochSlots
    (eq, gt)  = span ((< succ epoch) . _epoch) geq


-- | List all 'Tip's that point to a filled slot or an existing EBB in the
-- model, including 'TipGenesis'. The tips will be sorted from old to recent.
tips :: DBModel hash -> NonEmpty ImmTip
tips dbm = TipGen NE.:| tipsAfter dbm TipGen

-- | List all 'Tip's that point to a filled slot or an existing EBB in the
-- model that are after the given 'Tip'. The tips will be sorted from old to
-- recent.
tipsAfter :: DBModel hash -> ImmTip -> [ImmTip]
tipsAfter dbm tip = map toTip $ dropWhile isBeforeTip blobLocations
  where
    blobLocations :: [(EpochSlot, SlotNo)]
    blobLocations = Map.keys $ dbmBlobs dbm
    isBeforeTip :: (EpochSlot, SlotNo) -> Bool
    isBeforeTip (epochSlot, slot) = case tip of
      TipGen            -> False
      Tip (Left epoch)  -> epochSlot < EpochSlot epoch 0
      Tip (Right slot') -> slot      < slot'
    toTip :: (EpochSlot, SlotNo) -> ImmTip
    toTip (EpochSlot epoch 0, _)    = Tip (Left epoch)
    toTip (_                , slot) = Tip (Right slot)


{------------------------------------------------------------------------------
  Simulation corruptions and restoring afterwards
------------------------------------------------------------------------------}


-- | Simulate the following: close the database, apply the corruptions to the
-- respective files, and restore to the last valid epoch.
--
-- The returned chain will be a prefix of the given chain.
--
-- The 'FsPath's must correspond to index or epoch files that a real database,
-- which is in sync with the given model, would have created on disk.
simulateCorruptions :: Corruptions -> DBModel hash -> DBModel hash
simulateCorruptions corrs dbm = rollBack rbp dbm
  where
    -- Take the minimal 'RollBackPoint', which is the earliest.
    rbp = minimum $
      fmap (\(c, f) -> findCorruptionRollBackPoint c f dbm) corrs

data RollBackPoint
  = DontRollBack
    -- ^ No roll back needed.
  | RollBackToGenesis
    -- ^ Roll back to genesis, removing all slots.
  | RollBackToEpochSlot EpochSlot
    -- ^ Roll back to the 'EpochSlot', keeping it as the last relative slot.
  deriving (Eq, Show, Generic)

-- | The earlier 'RollBackPoint' < the later 'RollBackPoint'.
instance Ord RollBackPoint where
  compare r1 r2 = case (r1, r2) of
    (RollBackToGenesis, RollBackToGenesis)             -> EQ
    (RollBackToGenesis, _)                             -> LT
    (_, RollBackToGenesis)                             -> GT
    (DontRollBack, DontRollBack)                       -> EQ
    (_,            DontRollBack)                       -> LT
    (DontRollBack, _)                                  -> GT
    (RollBackToEpochSlot es1, RollBackToEpochSlot es2) -> compare es1 es2

rollBack :: RollBackPoint -> DBModel hash -> DBModel hash
rollBack rbp dbm = case rbp of
    DontRollBack                            ->                                  dbm
    RollBackToGenesis                       -> rollBackToTip TipGen             dbm
    RollBackToEpochSlot (EpochSlot epoch 0) -> rollBackToTip (Tip (Left epoch)) dbm
    RollBackToEpochSlot epochSlot           -> rollBackToTip (Tip (Right slot)) dbm
      where
        slot = epochSlotToSlot dbm epochSlot

findCorruptionRollBackPoint :: FileCorruption -> FsPath -> DBModel hash
                            -> RollBackPoint
findCorruptionRollBackPoint corr file dbm =
    case lastMaybe file >>= parseDBFile of
      Just ("epoch", epoch) -> findEpochCorruptionRollBackPoint corr epoch dbm
      Just ("index", epoch) -> findIndexCorruptionRollBackPoint corr epoch dbm
      _                     -> error "Invalid file to corrupt"

findEpochDropLastBytesRollBackPoint :: Word64 -> EpochNo -> DBModel hash
                                    -> RollBackPoint
findEpochDropLastBytesRollBackPoint n epoch dbm
    | null epochSlots
      -- If the file is empty, no corruption happened, and we don't have to
      -- roll back
    = DontRollBack
    | lastValidFilledSlotIndex validBytes < 0
      -- When there are no more filled slots in the epoch file, roll back to
      -- the last filled slot before the epoch.
    = rollbackToLastFilledSlotBefore epoch dbm
    | otherwise
    = RollBackToEpochSlot (epochSlots !! lastValidFilledSlotIndex validBytes)
  where
    totalBytes = fromIntegral testBlockSize * fromIntegral (length epochSlots)
    validBytes :: Word64
    validBytes
      | n >= totalBytes
      = 0
      | otherwise
      = totalBytes - n
    epochSlots = epochSlotsInEpoch dbm epoch
    lastValidFilledSlotIndex :: Word64 -> Int
    lastValidFilledSlotIndex offset =
      (fromIntegral offset `quot` fromIntegral testBlockSize) - 1

findEpochCorruptionRollBackPoint :: FileCorruption -> EpochNo -> DBModel hash
                                 -> RollBackPoint
findEpochCorruptionRollBackPoint corr epoch dbm = case corr of
    DeleteFile      -> rollbackToLastFilledSlotBefore epoch dbm
    DropLastBytes n -> findEpochDropLastBytesRollBackPoint n epoch dbm

rollbackToLastFilledSlotBefore :: EpochNo -> DBModel hash -> RollBackPoint
rollbackToLastFilledSlotBefore epoch dbm = case lastMaybe beforeEpoch of
    Just lastFilledSlotBefore -> RollBackToEpochSlot lastFilledSlotBefore
    Nothing                   -> RollBackToGenesis
  where
    (beforeEpoch, _, _) = filledEpochSlots dbm epoch

findIndexCorruptionRollBackPoint :: FileCorruption -> EpochNo -> DBModel hash
                                 -> RollBackPoint
findIndexCorruptionRollBackPoint _corr epoch dbm
    | Just lastFilledSlotOfEpoch     <- lastMaybe inEpoch
    = if isLastSlotOfEpoch lastFilledSlotOfEpoch
        -- If the last slot is filled, the complete index file can be
        -- recovered from the epoch file
      then DontRollBack
        -- Otherwise roll back to the last filled slot.
      else RollBackToEpochSlot lastFilledSlotOfEpoch
    | Just lastFilledSlotBeforeEpoch <- lastMaybe beforeEpoch
    = RollBackToEpochSlot lastFilledSlotBeforeEpoch
    | otherwise  -- there are no filled slots, roll back to genesis
    = RollBackToGenesis
  where
    (beforeEpoch, inEpoch, _afterEpoch) = filledEpochSlots dbm epoch
    isLastSlotOfEpoch (EpochSlot epoch' relSlot) =
      relSlot == maxRelativeSlot (lookupEpochSize dbm epoch')


{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

getTipModel :: MonadState (DBModel hash) m => m ImmTip
getTipModel = gets dbmTip

deleteAfterModel :: (MonadState (DBModel hash) m) => ImmTip -> m ()
deleteAfterModel tip =
    -- First roll back to the given tip (which is not guaranteed to be
    -- valid/exist!), then roll back to the last valid remaining tip.
    modify $ rollBackToLastValidTip . rollBackToTip tip
  where
    rollBackToLastValidTip dbm = rollBackToTip (NE.last (tips dbm)) dbm

getBinaryBlobModel :: (HasCallStack, MonadState (DBModel hash) m)
                   => ErrorHandling ImmutableDBError m
                   -> SlotNo
                   -> m (Maybe ByteString)
getBinaryBlobModel err slot = do
    DBModel {..} <- get

    -- Check that the slot is not in the future
    let inTheFuture = case dbmTip of
          TipGen               -> True
          Tip (Right lastSlot) -> slot > lastSlot
          Tip (Left  _ebb)     -> slot >= fromIntegral (length dbmChain)

    when inTheFuture $
      throwUserError err $ ReadFutureSlotError slot dbmTip

    return $ lookupBySlot slot dbmChain

getEBBModel :: (HasCallStack, MonadState (DBModel hash) m)
            => ErrorHandling ImmutableDBError m
            -> EpochNo
            -> m (Maybe (hash, ByteString))
getEBBModel err epoch = do
    dbm@DBModel {..} <- get
    let currentEpoch = dbmCurrentEpoch dbm
        inTheFuture  = epoch > currentEpoch || dbmTip == TipGen

    when inTheFuture $
      throwUserError err $ ReadFutureEBBError epoch currentEpoch

    return $ Map.lookup epoch dbmEBBs

appendBinaryBlobModel :: (HasCallStack, MonadState (DBModel hash) m)
                      => ErrorHandling ImmutableDBError m
                      -> SlotNo
                      -> Builder
                      -> m ()
appendBinaryBlobModel err slot bld = do
    dbm@DBModel {..} <- get

    -- Check that we're not appending to the past
    let inThePast = case dbmTip of
          Tip (Right lastSlot) -> slot <= lastSlot
          Tip (Left _)         -> slot < fromIntegral (length dbmChain)
          TipGen               -> False

    when inThePast $
      throwUserError err $ AppendToSlotInThePastError slot dbmTip

    let blob  = BL.toStrict $ BS.toLazyByteString bld
        toPad = fromIntegral (unSlotNo slot) - length dbmChain

    -- TODO snoc list?
    put dbm
      { dbmChain = dbmChain ++ replicate toPad Nothing ++ [Just blob]
      , dbmTip   = Tip (Right slot)
      }

appendEBBModel :: (MonadState (DBModel hash) m)
               => ErrorHandling ImmutableDBError m
               -> EpochNo
               -> hash
               -> Builder
               -> m ()
appendEBBModel err epoch hash bld = do
    dbm@DBModel {..} <- get

    -- Check that we're not appending to the past
    let currentEpoch = dbmCurrentEpoch dbm
        inThePast    = epoch <= currentEpoch && dbmTip /= TipGen

    when inThePast $
      throwUserError err $ AppendToEBBInThePastError epoch currentEpoch

    let blob         = BL.toStrict $ BS.toLazyByteString bld
        ebbEpochSlot = EpochSlot epoch 0
        ebbSlot      = epochSlotToSlot dbm ebbEpochSlot
        toPad        = fromIntegral (unSlotNo ebbSlot) - length dbmChain

    put dbm
      { dbmChain = dbmChain ++ replicate toPad Nothing
      , dbmTip   = Tip (Left epoch)
      , dbmEBBs  = Map.insert epoch (hash, blob) dbmEBBs
      }

streamBinaryBlobsModel :: forall m hash.
                          (MonadState (DBModel hash) m, Eq hash)
                       => ErrorHandling ImmutableDBError m
                       -> Maybe (SlotNo, hash)
                       -> Maybe (SlotNo, hash)
                       -> m (Iterator hash m ByteString)
streamBinaryBlobsModel err mbStart mbEnd = do
    dbm@DBModel {..} <- get

    validateIteratorRange err (generalizeEpochInfo dbmEpochInfo) dbmTip mbStart mbEnd

    let results = iteratorResults dbm
        itm     = IteratorModel results
        itID    = BaseIteratorID dbmNextIterator
    put dbm
      { dbmNextIterator = succ dbmNextIterator
      , dbmIterators    = Map.insert itID itm dbmIterators
      }
    return Iterator
      { iteratorNext  = iteratorNextModel  itID
      , iteratorPeek  = iteratorPeekModel  itID
      , iteratorClose = iteratorCloseModel itID
      , iteratorID    = itID
      }
  where
    iteratorResults dbm@DBModel {..} =
        map snd $ takeUntilEnd $ dropUntilStart $
        map toIteratorResult $ Map.toAscList $ dbmBlobs dbm
      where
        toIteratorResult (k@(EpochSlot epoch _, slot), v) = case v of
          Left (hash, blob) -> (k, IteratorEBB epoch hash blob)
          Right blob        -> (k, IteratorResult slot blob)

        dropUntilStart = case mbStart of
          Nothing                -> id
          Just (startSlot, hash) -> dropWhile $ \((_, slot), res) -> if
            | slot < startSlot   -> True
            | slot == startSlot  -> case res of
              -- If an EBB is stored in the slot, we'll encounter the EBB
              -- before we'd encounter a block at the same slot. If the hash
              -- is that of the EBB, stop dropping because we want to include
              -- it in the results.
              IteratorEBB _ hash' _ | hash' == hash -> False
              -- If the hash doesn't match that of the EBB, it must be the
              -- hash of the block (which we can't verify!). Drop the EBB
              -- because we should start at the next block.
                                    | otherwise     -> True
              -- If the slot is that of a regular block, stop dropping and
              -- include the block in the results, regardless the hash.
              _ -> False
            | otherwise -> False
        takeUntilEnd = case mbEnd of
            Nothing              -> id
            Just (endSlot, hash) -> go
              where
                go [] = []
                go (x@((_, slot), res):xs)
                  | slot < endSlot  = x : go xs
                  | slot == endSlot = case res of
                    -- If an EBB is stored in the slot, we'll encounter the
                    -- EBB before we'd encounter a block at the same slot. If
                    -- the hash is that of the EBB, we want to include it and
                    -- stop.
                    IteratorEBB _ hash' _ | hash' == hash -> [x]
                    -- If the hash doesn't match that of the EBB, it must be
                    -- the hash of the next block at the same slot (which we
                    -- can't verify!), so include the EBB and continue so we
                    -- can include the next block.
                                          | otherwise     -> x : go xs
                    -- If the slot is that of a regular block, include it and
                    -- stop. It can't be that the hash was of the EBB because,
                    -- then we would have already stopped.
                    _ -> [x]
                  | otherwise = []

iteratorNextModel :: MonadState (DBModel hash) m
                  => IteratorID
                  -> m (IteratorResult hash ByteString)
iteratorNextModel itID = do
    dbm@DBModel {..} <- get
    case Map.lookup itID dbmIterators of
      Nothing                         -> return IteratorExhausted
      Just (IteratorModel [])         -> do
        iteratorCloseModel itID
        return IteratorExhausted
      Just (IteratorModel (res:ress)) -> do
        put dbm
          { dbmIterators = Map.insert itID (IteratorModel ress) dbmIterators
          }
        return res

iteratorPeekModel :: MonadState (DBModel hash) m
                  => IteratorID
                  -> m (IteratorResult hash ByteString)
iteratorPeekModel itID = do
    DBModel {..} <- get
    case Map.lookup itID dbmIterators of
      Nothing                      -> return IteratorExhausted
      Just (IteratorModel [])      -> return IteratorExhausted
      Just (IteratorModel (res:_)) -> return res

iteratorCloseModel :: MonadState (DBModel hash) m
                   => IteratorID -> m ()
iteratorCloseModel itID = modify $ \dbm@DBModel {..} ->
    dbm { dbmIterators = Map.delete itID dbmIterators }
