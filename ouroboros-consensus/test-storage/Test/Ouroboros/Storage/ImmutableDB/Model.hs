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
  , lookupNextEpochSlot
  , getLastBlobLocation
  , simulateCorruptions
  ) where

import           Control.Monad (when)
import           Control.Monad.State (MonadState, get, gets, put, modify)

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List (genericReplicate, dropWhileEnd)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe, fromMaybe, mapMaybe, isNothing)
import           Data.Word (Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, withFrozenCallStack)

import           Ouroboros.Consensus.Util (lastMaybe)

import           Ouroboros.Network.Block (Slot(..))

import           Ouroboros.Storage.FS.API.Types (FsPath)
import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Util hiding (lookupEpochSize)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Test.Ouroboros.Storage.ImmutableDB.TestBlock
import           Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes (CumulEpochSizes)
import qualified Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CES


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

dbmBlobs :: DBModel -> Map EpochSlot ByteString
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
            -> EpochSize
            -> (DBModel, ImmutableDB m)
openDBModel err firstEpochSize = (dbModel, db)
  where
    dbModel = initDBModel firstEpochSize
    db = ImmutableDB
      { closeDB           = return ()
      , isOpen            = return True
      , reopen            = const reopenModel -- No recovery
      , getNextEpochSlot  = getNextEpochSlotModel
      , getBinaryBlob     = getBinaryBlobModel     err
      , appendBinaryBlob  = appendBinaryBlobModel  err
      , startNewEpoch     = startNewEpochModel
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

lookupNextEpochSlot :: HasCallStack => DBModel -> EpochSlot
lookupNextEpochSlot dbm@DBModel {..} = case slotToEpochSlot dbm dbmNextSlot of
    -- TODO imitates the Impl
    EpochSlot epoch 0 | epoch == currentEpoch + 1
      -> EpochSlot currentEpoch (fromIntegral epochSize)
    epochSlot -> epochSlot
  where
    currentEpoch = CES.lastEpoch dbmCumulEpochSizes
    epochSize    = lookupEpochSize dbm currentEpoch

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

getLastBlobLocation :: DBModel -> Maybe EpochSlot
getLastBlobLocation dbm@DBModel {..} =
    fmap (slotToEpochSlot dbm) $
    lastMaybe $ zipWith const [0..] $ dropWhileEnd isNothing dbmChain

-- | Rolls back the chain to the given 'Slot'.
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

-- | Return the filled 'EpochSlot' of the given 'Epoch' stored in the model.
epochSlotsInEpoch :: DBModel -> Epoch -> [EpochSlot]
epochSlotsInEpoch dbm epoch =
    filter ((== epoch) . _epoch) $
    map fst $
    Map.toAscList $ dbmBlobs dbm

-- | Return the last filled 'EpochSlot' of the given 'Epoch' stored in the
-- model.
lastFilledEpochSlotOf :: DBModel -> Epoch -> Maybe EpochSlot
lastFilledEpochSlotOf dbm epoch = lastMaybe $ epochSlotsInEpoch dbm epoch


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
    rbp = foldr1 min $
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
      -- If the file is empty, we don't have to roll back.
    = DontRollBack
    | lastValidFilledSlotIndex validBytes < 0
      -- When we corrupted all blocks, we should roll back to the start of
      -- the epoch, but if the first block did not start at slot 0, the
      -- index will let us restore to the unfilled slot before the first
      -- block.
    = RollBackToEpochStart epoch
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
    DeleteFile      -> RollBackToEpochStart epoch

    DropLastBytes n -> findEpochDropLastBytesRollBackPoint n epoch dbm

findIndexCorruptionRollBackPoint :: FileCorruption -> Epoch -> DBModel
                                 -> RollBackPoint
findIndexCorruptionRollBackPoint _corr epoch dbm =
    case lastFilledEpochSlotOf dbm epoch of
      Just lastFilledEpochSlotOfEpoch
        | isLastSlotOfEpoch lastFilledEpochSlotOfEpoch
          -- If the last slot is filled, the complete index file can be
          -- recovered from the epoch file
        -> DontRollBack
        | otherwise
          -- Otherwise roll back to the last filled slot, unless we can roll
          -- back to a later slot based on the padded slots of the index file.
        -> RollBackToEpochSlot lastFilledEpochSlotOfEpoch

      -- If there are no slots in the epoch, Find out the last valid slot in
      -- the index file and roll back to that.
      Nothing -> RollBackToEpochStart epoch
  where
    isLastSlotOfEpoch (EpochSlot epoch' relSlot) =
      getRelativeSlot relSlot + 1 == lookupEpochSize dbm epoch'


{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

reopenModel :: MonadState DBModel m => m (Maybe EpochSlot)
reopenModel = gets getLastBlobLocation


getNextEpochSlotModel :: (HasCallStack, MonadState DBModel m)
                      => m EpochSlot
getNextEpochSlotModel = gets lookupNextEpochSlot

getBinaryBlobModel :: (HasCallStack, MonadState DBModel m)
                   => ErrorHandling ImmutableDBError m
                   -> EpochSlot
                   -> m (Maybe ByteString)
getBinaryBlobModel err readEpochSlot = do
    dbm@DBModel {..} <- get
    let nextEpochSlot = lookupNextEpochSlot dbm
    when (readEpochSlot >= nextEpochSlot) $ throwUserError err $
      ReadFutureSlotError readEpochSlot nextEpochSlot

    let EpochSlot epoch relativeSlot = readEpochSlot
        -- We already know that 'readEpochSlot' does not lie in the future, so
        -- the size of the corresponding epoch must be known.
        epochSize = lookupEpochSize dbm epoch
    when (getRelativeSlot relativeSlot >= epochSize) $ throwUserError err $
      SlotGreaterThanEpochSizeError relativeSlot epoch epochSize

    let slot = epochSlotToSlot dbm readEpochSlot

    return $ lookupBySlot slot dbmChain


appendBinaryBlobModel :: (HasCallStack, MonadState DBModel m)
                      => ErrorHandling ImmutableDBError m
                      -> RelativeSlot
                      -> Builder
                      -> m ()
appendBinaryBlobModel err relSlot bld = do
    dbm@DBModel {..} <- get
    let EpochSlot currentEpoch nextRelSlot = lookupNextEpochSlot dbm

    when (relSlot < nextRelSlot) $
      throwUserError err $ AppendToSlotInThePastError relSlot nextRelSlot

    let epochSize = lookupEpochSize dbm currentEpoch
    when (getRelativeSlot relSlot >= epochSize) $ throwUserError err $
      SlotGreaterThanEpochSizeError relSlot currentEpoch epochSize

    let appendEpochSlot = EpochSlot currentEpoch relSlot
        appendSlot = epochSlotToSlot dbm appendEpochSlot
        blob = BL.toStrict $ BS.toLazyByteString bld
        toPad = fromIntegral (appendSlot - dbmNextSlot)

    -- TODO snoc list?
    put dbm
      { dbmChain    = dbmChain ++ (replicate toPad Nothing ++ [Just blob])
      , dbmNextSlot = appendSlot + 1
      }

startNewEpochModel :: MonadState DBModel m
                   => EpochSize
                   -> m Epoch
startNewEpochModel nextEpochSize = do
    dbm@DBModel {..} <- get
    let toPad = CES.maxSlot dbmCumulEpochSizes + 1 - dbmNextSlot
        newEpoch = CES.lastEpoch dbmCumulEpochSizes + 1

    put dbm
      { dbmChain = dbmChain ++ genericReplicate toPad Nothing
        -- We use 'genericReplicate' instead of the regular 'replicate', which
        -- takes an 'Int', because when converting from a 'Slot' (~= 'Word')
        -- to an 'Int', it could underflow. While this scenario is not very
        -- realistic, it can turn up in the StateMachine tests, in which case
        -- we want it to fail with a better error than the error thrown by
        -- 'fromEnum'.
      , dbmNextSlot = dbmNextSlot + fromIntegral toPad
      , dbmCumulEpochSizes = CES.snoc dbmCumulEpochSizes nextEpochSize }
    return newEpoch

streamBinaryBlobsModel :: (HasCallStack, MonadState DBModel m)
                       => ErrorHandling ImmutableDBError m
                       -> Maybe EpochSlot
                       -> Maybe EpochSlot
                       -> m (Iterator m)
streamBinaryBlobsModel err mbStart mbEnd = do
    dbm@DBModel {..} <- get
    let nextEpochSlot = lookupNextEpochSlot dbm
        getEpochSize epoch = return $ lookupEpochSize dbm epoch
    validateIteratorRange err nextEpochSlot getEpochSize mbStart mbEnd

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
            start = maybe 0 (epochSlotToSlot dbm) mbStart
            end   = maybe currentSlot (epochSlotToSlot dbm) mbEnd
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
            return $ IteratorResult (slotToEpochSlot dbm next) blob

iteratorCloseModel :: MonadState DBModel m
                   => IteratorID -> m ()
iteratorCloseModel itID = modify $ \dbm@DBModel {..} ->
    dbm { dbmIterators = Map.delete itID dbmIterators }
