{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- | Model for the 'ImmutableDB' based on a chain.
--
-- The chain is just a list of slots that can be unfilled (@Nothing@) or
-- filled (@Just ByteString@).
module Test.Ouroboros.Storage.ImmutableDB.Model
  ( DBModel(..)
  , dbmBlobs
  , initDBModel
  , IteratorModel
  , openDBModel
  , getLastBlobLocation
  , simulateCorruptions
  , slotToEpochSlot
  ) where

import           Control.Monad (when)
import           Control.Monad.State (MonadState, get, gets, modify, put)

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List (dropWhileEnd)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import           Data.Word (Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, withFrozenCallStack)

import           Ouroboros.Consensus.Util (lastMaybe)

import           Ouroboros.Network.Block (Slot (..))

import           Ouroboros.Storage.FS.API.Types (FsPath)
import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.CumulEpochSizes (CumulEpochSizes,
                     EpochSlot (..))
import qualified Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CES
import           Ouroboros.Storage.ImmutableDB.Util
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Test.Ouroboros.Storage.ImmutableDB.TestBlock


data DBModel = DBModel
  { dbmChain           :: [Maybe ByteString]
    -- ^ 'Nothing' when a slot is empty
  , dbmNextSlot        :: Slot
    -- ^ The number of the next available slot
  , dbmCumulEpochSizes :: CumulEpochSizes
  , dbmIterators       :: Map IteratorID IteratorModel
  , dbmNextIterator    :: IteratorID
  } deriving (Show, Generic)

initDBModel :: EpochSize -- ^ The size of the first epoch
            -> DBModel
initDBModel firstEpochSize = DBModel
  { dbmChain           = []
  , dbmNextSlot        = 0
  , dbmCumulEpochSizes = CES.singleton firstEpochSize
  , dbmIterators       = Map.empty
  , dbmNextIterator    = initialIteratorId
  }

dbmBlobs :: HasCallStack => DBModel -> Map EpochSlot ByteString
dbmBlobs dbm@DBModel {..} = foldr add Map.empty (zip (map Slot [0..]) dbmChain)
  where
    add (_,    Nothing)   = id
    add (slot, Just blob) = Map.insert (slotToEpochSlot dbm slot) blob


-- | Model for an 'Iterator'.
--
-- The first 'Slot' = return the next filled slot (doesn't have to exist) >=
-- this 'Slot'. The second 'Slot': last filled slot to return (inclusive).
--
-- An iterator is open iff its is present in 'dbmIterators'
newtype IteratorModel = IteratorModel (Slot, Slot)
  deriving (Show, Eq, Generic)




{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

openDBModel :: MonadState DBModel m
            => ErrorHandling ImmutableDBError m
            -> (Epoch -> EpochSize)
            -> (DBModel, ImmutableDB m)
openDBModel err getEpochSize = (dbModel, db)
  where
    dbModel = initDBModel (getEpochSize 0)
    db = ImmutableDB
      { closeDB           = return ()
      , isOpen            = return True
      , reopen            = const reopenModel -- No recovery
      , getNextSlot       = getNextSlotModel
      , getBinaryBlob     = getBinaryBlobModel     err
      , appendBinaryBlob  = appendBinaryBlobModel  err getEpochSize
      , streamBinaryBlobs = streamBinaryBlobsModel err
      , immutableDBErr    = err
      }

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

impossible :: HasCallStack => String -> a
impossible msg = withFrozenCallStack (error ("Impossible: " ++ msg))

mustBeJust :: HasCallStack => String -> Maybe a -> a
mustBeJust msg = fromMaybe (impossible msg)

lookupEpochSize :: HasCallStack => DBModel -> Epoch -> EpochSize
lookupEpochSize DBModel {..} epoch =
    mustBeJust msg $ CES.epochSize dbmCumulEpochSizes epoch
  where
    msg = "epoch (" <> show epoch <> ") size unknown (" <>
          show dbmCumulEpochSizes <> ")"

epochSlotToSlot :: HasCallStack => DBModel -> EpochSlot -> Slot
epochSlotToSlot DBModel {..} epochSlot =
    mustBeJust msg $ CES.epochSlotToSlot dbmCumulEpochSizes epochSlot
  where
    msg = "epochSlot (" <> show epochSlot <>
          ") could not be converted to a slot"

slotToEpochSlot :: HasCallStack => DBModel -> Slot -> EpochSlot
slotToEpochSlot DBModel {..} slot =
    mustBeJust msg $ CES.slotToEpochSlot dbmCumulEpochSizes slot
  where
    msg = "slot (" <> show slot <>
          ") could not be converted to an epochSlot (" <>
          show dbmCumulEpochSizes <> ")"

lookupBySlot :: HasCallStack => Slot -> [Maybe b] -> Maybe b
lookupBySlot (Slot i) = go i
  where
    go 0 (blob:_)  = blob
    go n (_:blobs) = go (n - 1) blobs
    go _ []        = error "lookupBySlot: index out of bounds"

getLastBlobLocation :: DBModel -> Maybe Slot
getLastBlobLocation DBModel {..} =
    lastMaybe $ zipWith const [0..] $ dropWhileEnd isNothing dbmChain

-- | Rolls back the chain to the given 'Slot', i.e. the given 'Slot' will be
-- the new head of the chain.
rollBackToSlot :: HasCallStack => Slot -> DBModel -> DBModel
rollBackToSlot slot dbm@DBModel {..} =
    dbm { dbmChain           = rolledBackChain
        , dbmNextSlot        = newNextSlot
        , dbmCumulEpochSizes = dbmCumulEpochSizes'
        }
  where
    dbmCumulEpochSizes' = CES.rollBackToEpoch dbmCumulEpochSizes epoch
    EpochSlot epoch _ = slotToEpochSlot dbm slot
    rolledBackChain = map snd $ takeWhile ((<= slot) . fst) $ zip [0..] dbmChain
    newNextSlot = fromIntegral $ length rolledBackChain

-- | Rolls back the chain to the start of the given 'Epoch'. The next epoch
-- slot will be the first of the given 'Epoch'.
rollBackToEpochStart :: HasCallStack => Epoch -> DBModel -> DBModel
rollBackToEpochStart epoch dbm@DBModel {..} =
    dbm { dbmChain           = rolledBackChain
        , dbmNextSlot        = newNextSlot
        , dbmCumulEpochSizes = CES.rollBackToEpoch dbmCumulEpochSizes epoch
        }
  where
    -- All slots >= @slot@ should be removed from the end of the chain
    slot = epochSlotToSlot dbm (EpochSlot epoch 0)
    rolledBackChain =
      map snd $
      takeWhile ((< slot) . fst) $
      zip [0..] dbmChain
    newNextSlot = fromIntegral $ length rolledBackChain

-- | Rolls back the chain to the slot that corresponds to the given
-- 'EpochSlot'.
rollBackToEpochSlot :: HasCallStack => EpochSlot -> DBModel -> DBModel
rollBackToEpochSlot epochSlot dbm =
    rollBackToSlot (epochSlotToSlot dbm epochSlot) dbm

-- | Return the filled 'EpochSlot's of the given 'Epoch' stored in the model.
epochSlotsInEpoch :: HasCallStack => DBModel -> Epoch -> [EpochSlot]
epochSlotsInEpoch dbm epoch =
    filter ((== epoch) . _epoch) $
    map fst $
    Map.toAscList $ dbmBlobs dbm

-- | Return the filled 'EpochSlot's before, in, and after the given 'Epoch'.
filledEpochSlots :: HasCallStack
                 => DBModel -> Epoch -> ([EpochSlot], [EpochSlot], [EpochSlot])
filledEpochSlots dbm epoch = (lt, eq, gt)
  where
    increasingEpochSlots = map fst $ Map.toAscList $ dbmBlobs dbm
    (lt, geq) = span ((< epoch)      . _epoch) increasingEpochSlots
    (eq, gt)  = span ((< succ epoch) . _epoch) geq


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
simulateCorruptions :: Corruptions -> DBModel -> DBModel
simulateCorruptions corrs dbm = rollBack rbp dbm
  where
    -- Take the minimal 'RollBackPoint', which is the earliest.
    rbp = minimum $
      fmap (\(c, f) -> findCorruptionRollBackPoint c f dbm) corrs

data RollBackPoint
  = DontRollBack
    -- ^ No roll back needed.
  | RollBackToEpochStart Epoch
    -- ^ Roll back to the start of the 'Epoch', removing all relative slots
    -- of the epoch.
  | RollBackToEpochSlot  EpochSlot
    -- ^ Roll back to the 'EpochSlot', keeping it as the last relative slot.
  deriving (Eq, Show, Generic)

-- | The earlier 'RollBackPoint' < the later 'RollBackPoint'.
instance Ord RollBackPoint where
  compare r1 r2 = case (r1, r2) of
    (DontRollBack, DontRollBack) -> EQ
    (_,            DontRollBack) -> LT
    (DontRollBack, _)            -> GT
    (RollBackToEpochStart e1, RollBackToEpochStart e2) -> compare e1 e2
    (RollBackToEpochStart e1, RollBackToEpochSlot (EpochSlot e2 _))
      | e1 <= e2  -> LT
      | otherwise -> GT
    (RollBackToEpochSlot (EpochSlot e1 _), RollBackToEpochStart e2)
      | e1 < e2   -> LT
      | otherwise -> GT
    (RollBackToEpochSlot es1, RollBackToEpochSlot es2) -> compare es1 es2

rollBack :: RollBackPoint -> DBModel -> DBModel
rollBack rbp = case rbp of
    DontRollBack                   -> id
    RollBackToEpochStart epoch     -> rollBackToEpochStart epoch
    RollBackToEpochSlot  epochSlot -> rollBackToEpochSlot  epochSlot

findCorruptionRollBackPoint :: FileCorruption -> FsPath -> DBModel
                            -> RollBackPoint
findCorruptionRollBackPoint corr file dbm =
    case lastMaybe file >>= parseDBFile of
      Just ("epoch", epoch) -> findEpochCorruptionRollBackPoint corr epoch dbm
      Just ("index", epoch) -> findIndexCorruptionRollBackPoint corr epoch dbm
      _                     -> error "Invalid file to corrupt"

findEpochDropLastBytesRollBackPoint :: Word64 -> Epoch -> DBModel
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
    totalBytes = fromIntegral $ testBlockSize * length epochSlots
    validBytes :: Word64
    validBytes
      | n >= totalBytes
      = 0
      | otherwise
      = totalBytes - n
    epochSlots = epochSlotsInEpoch dbm epoch
    lastValidFilledSlotIndex offset =
      (fromIntegral offset `quot` testBlockSize) - 1

findEpochCorruptionRollBackPoint :: FileCorruption -> Epoch -> DBModel
                                 -> RollBackPoint
findEpochCorruptionRollBackPoint corr epoch dbm = case corr of
    DeleteFile      -> rollbackToLastFilledSlotBefore epoch dbm

    DropLastBytes n -> findEpochDropLastBytesRollBackPoint n epoch dbm

rollbackToLastFilledSlotBefore :: Epoch -> DBModel -> RollBackPoint
rollbackToLastFilledSlotBefore epoch dbm = case lastMaybe beforeEpoch of
    Just lastFilledSlotBefore -> RollBackToEpochSlot lastFilledSlotBefore
    Nothing                   -> RollBackToEpochStart 0
  where
    (beforeEpoch, _, _) = filledEpochSlots dbm epoch

findIndexCorruptionRollBackPoint :: FileCorruption -> Epoch -> DBModel
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
    = RollBackToEpochStart 0
  where
    (beforeEpoch, inEpoch, _afterEpoch) = filledEpochSlots dbm epoch
    isLastSlotOfEpoch (EpochSlot epoch' relSlot) =
      relSlot == CES.lastRelativeSlot (lookupEpochSize dbm epoch')


{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

reopenModel :: MonadState DBModel m
            => Maybe TruncateFrom -> m (Maybe Slot)
reopenModel Nothing                    = gets getLastBlobLocation
reopenModel (Just (TruncateFrom 0))    = do
    modify $ rollBackToEpochStart 0
    return Nothing
reopenModel (Just (TruncateFrom slot)) = do
    modify $ rollBackToSlot (slot - 1)
    lastBlobLocation <- gets getLastBlobLocation
    -- Truncate empty trailing slots
    modify $ case lastBlobLocation of
      Nothing             -> rollBackToEpochStart 0
      Just lastFilledSlot -> rollBackToSlot lastFilledSlot
    return lastBlobLocation

getNextSlotModel :: (HasCallStack, MonadState DBModel m)
                 => m Slot
getNextSlotModel = gets dbmNextSlot

getBinaryBlobModel :: (HasCallStack, MonadState DBModel m)
                   => ErrorHandling ImmutableDBError m
                   -> Slot
                   -> m (Maybe ByteString)
getBinaryBlobModel err slot = do
    DBModel {..} <- get
    when (slot >= dbmNextSlot) $ throwUserError err $
      ReadFutureSlotError slot dbmNextSlot

    return $ lookupBySlot slot dbmChain


appendBinaryBlobModel :: (HasCallStack, MonadState DBModel m)
                      => ErrorHandling ImmutableDBError m
                      -> (Epoch -> EpochSize)
                      -> Slot
                      -> Builder
                      -> m ()
appendBinaryBlobModel err getEpochSize slot bld = do
    dbm@DBModel {..} <- get

    when (slot < dbmNextSlot) $
      throwUserError err $ AppendToSlotInThePastError slot dbmNextSlot

    let blob = BL.toStrict $ BS.toLazyByteString bld
        toPad = fromIntegral (slot - dbmNextSlot)

    -- TODO snoc list?
    put dbm
      { dbmChain           = dbmChain ++ replicate toPad Nothing ++ [Just blob]
      , dbmNextSlot        = slot + 1
      , dbmCumulEpochSizes = addMissingEpochSizes dbmCumulEpochSizes
      }
  where
    addMissingEpochSizes ces
      | slot <= CES.maxSlot ces
      = ces
      | otherwise
      = addMissingEpochSizes
      $ CES.snoc ces (getEpochSize (succ (CES.lastEpoch ces)))

streamBinaryBlobsModel :: (HasCallStack, MonadState DBModel m)
                       => ErrorHandling ImmutableDBError m
                       -> Maybe Slot
                       -> Maybe Slot
                       -> m (Iterator m)
streamBinaryBlobsModel err mbStart mbEnd = do
    dbm@DBModel {..} <- get
    validateIteratorRange err dbmNextSlot mbStart mbEnd

    case length $ dropWhileEnd isNothing dbmChain of
      -- Empty database, return an empty iterator
      0 -> do
        put dbm { dbmNextIterator = succ dbmNextIterator }
        return Iterator
          { iteratorNext  = return IteratorExhausted
          , iteratorClose = return ()
          , iteratorID    = dbmNextIterator
          }
      n -> do
        let currentSlot = fromIntegral n - 1
            start = fromMaybe 0           mbStart
            end   = fromMaybe currentSlot mbEnd
            itm   = IteratorModel (start, end)
            itID  = dbmNextIterator
        put dbm
          { dbmNextIterator = succ dbmNextIterator
          , dbmIterators    = Map.insert dbmNextIterator itm dbmIterators
          }
        return Iterator
          { iteratorNext  = iteratorNextModel  itID
          , iteratorClose = iteratorCloseModel itID
          , iteratorID    = itID
          }


iteratorNextModel :: (HasCallStack, MonadState DBModel m)
                  => IteratorID
                  -> m IteratorResult
iteratorNextModel itID = do
    dbm@DBModel {..} <- get
    case Map.lookup itID dbmIterators of
      Nothing -> return IteratorExhausted
      Just (IteratorModel (minNext, end)) -> do
        let mbBlob = listToMaybe
                   $ takeWhile ((<= end) . fst)
                   $ dropWhile ((< minNext) . fst)
                   $ mapMaybe (\(slot, mbBlob') -> (slot,) <$> mbBlob')
                   $ zip [0..] dbmChain
        case mbBlob of
          Nothing -> do
            iteratorCloseModel itID
            return IteratorExhausted
          Just (next, blob) -> do
            if next > end
              then iteratorCloseModel itID
              else
                let next' = succ next
                    itm'  = IteratorModel (next', end)
                in put dbm { dbmIterators = Map.insert itID itm' dbmIterators }
            return $ IteratorResult next blob

iteratorCloseModel :: MonadState DBModel m
                   => IteratorID -> m ()
iteratorCloseModel itID = modify $ \dbm@DBModel {..} ->
    dbm { dbmIterators = Map.delete itID dbmIterators }
