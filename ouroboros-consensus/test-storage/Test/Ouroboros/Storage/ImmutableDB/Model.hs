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
  , dbmBlobs
  , initDBModel
  , IteratorModel
  , openDBModel
  , simulateCorruptions
  , rollBackToTip
  ) where

import           Control.Monad (when)
import           Control.Monad.State.Strict (MonadState, execState, get, gets,
                     modify, put, runState)

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Function ((&))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, withFrozenCallStack)

import           Ouroboros.Consensus.Util (lastMaybe)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.FS.API.Types (FsPath)
import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.CumulEpochSizes (CumulEpochSizes,
                     EpochSlot (..))
import qualified Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CES
import           Ouroboros.Storage.ImmutableDB.Util
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Test.Ouroboros.Storage.ImmutableDB.TestBlock

data DBModel hash = DBModel
  { dbmChain           :: [Maybe ByteString]
    -- ^ 'Nothing' when a slot is empty
  , dbmTip             :: Tip
  , dbmEBBs            :: Map EpochNo (hash, ByteString)
    -- ^ The EBB for each 'EpochNo'
  , dbmCumulEpochSizes :: CumulEpochSizes
  , dbmIterators       :: Map IteratorID (IteratorModel hash)
  , dbmNextIterator    :: IteratorID
  } deriving (Show, Generic)

initDBModel :: EpochSize -- ^ The size of the first epoch
            -> DBModel hash
initDBModel firstEpochSize = DBModel
  { dbmChain           = []
  , dbmTip             = TipGenesis
  , dbmEBBs            = Map.empty
  , dbmCumulEpochSizes = CES.singleton firstEpochSize
  , dbmIterators       = Map.empty
  , dbmNextIterator    = initialIteratorID
  }

dbmBlobs :: HasCallStack
         => DBModel hash
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
newtype IteratorModel hash = IteratorModel [IteratorResult hash]
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
      , appendBinaryBlob  = appendBinaryBlobModel  err getEpochSize
      , appendEBB         = appendEBBModel         err getEpochSize
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

lookupEpochSize :: HasCallStack => DBModel hash -> EpochNo -> EpochSize
lookupEpochSize DBModel {..} epoch =
    mustBeJust msg $ CES.epochSize dbmCumulEpochSizes epoch
  where
    msg = "epoch (" <> show epoch <> ") size unknown (" <>
          show dbmCumulEpochSizes <> ")"

epochSlotToSlot :: HasCallStack => DBModel hash -> EpochSlot -> SlotNo
epochSlotToSlot DBModel {..} epochSlot =
    mustBeJust msg $ CES.epochSlotToSlot dbmCumulEpochSizes epochSlot
  where
    msg = "epochSlot (" <> show epochSlot <>
          ") could not be converted to a slot"

slotToEpochSlot :: HasCallStack => DBModel hash -> SlotNo -> EpochSlot
slotToEpochSlot DBModel {..} slot =
    mustBeJust msg $ CES.slotToEpochSlot dbmCumulEpochSizes slot
  where
    msg = "slot (" <> show slot <>
          ") could not be converted to an epochSlot (" <>
          show dbmCumulEpochSizes <> ")"

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
rollBackToTip :: HasCallStack => Tip -> DBModel hash -> DBModel hash
rollBackToTip tip dbm@DBModel {..} = case tip of
    TipGenesis    -> (initDBModel firstEpochSize)
        { dbmNextIterator = dbmNextIterator }
      where
        firstEpochSize = lookupEpochSize dbm 0

    TipEBB epoch
      | epoch > CES.lastEpoch dbmCumulEpochSizes -> dbm
      | otherwise                                -> dbm
        { dbmChain           = rolledBackChain
        , dbmEBBs            = ebbsUpToEpoch epoch
        , dbmTip             = tip
        , dbmCumulEpochSizes = CES.rollBackToEpoch dbmCumulEpochSizes epoch
        }
      where
        firstSlotAfter  = epochSlotToSlot dbm (EpochSlot epoch 1)
        rolledBackChain = dbmChain
                        & zip [0..]
                        & takeWhile ((< firstSlotAfter) . fst)
                        & map snd

    TipBlock slot
      | slot >= fromIntegral (length dbmChain) -> dbm
      | otherwise                              -> dbm
        { dbmChain           = rolledBackChain
        , dbmEBBs            = ebbsUpToEpoch epoch
        , dbmTip             = tip
        , dbmCumulEpochSizes = CES.rollBackToEpoch dbmCumulEpochSizes epoch
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
epochSlotsInEpoch :: HasCallStack => DBModel hash -> EpochNo -> [EpochSlot]
epochSlotsInEpoch dbm epoch =
    filter ((== epoch) . _epoch) $
    map (fst . fst) $
    Map.toAscList $ dbmBlobs dbm

-- | Return the filled 'EpochSlot's (including EBBs) before, in, and after the
-- given 'EpochNo'.
filledEpochSlots :: HasCallStack
                 => DBModel hash
                 -> EpochNo
                 -> ([EpochSlot], [EpochSlot], [EpochSlot])
filledEpochSlots dbm epoch = (lt, eq, gt)
  where
    increasingEpochSlots = map (fst . fst) $ Map.toAscList $ dbmBlobs dbm
    (lt, geq) = span ((< epoch)      . _epoch) increasingEpochSlots
    (eq, gt)  = span ((< succ epoch) . _epoch) geq


-- | List all 'Tip's that point to a filled slot or an existing EBB in the
-- model, including 'TipGenesis'. The tips will be sorted from old to recent.
tips :: DBModel hash -> NonEmpty Tip
tips dbm = TipGenesis NE.:| tipsAfter dbm TipGenesis

-- | List all 'Tip's that point to a filled slot or an existing EBB in the
-- model that are after the given 'Tip'. The tips will be sorted from old to
-- recent.
tipsAfter :: DBModel hash -> Tip -> [Tip]
tipsAfter dbm tip = map toTip $ dropWhile isBeforeTip blobLocations
  where
    blobLocations :: [(EpochSlot, SlotNo)]
    blobLocations = Map.keys $ dbmBlobs dbm
    isBeforeTip :: (EpochSlot, SlotNo) -> Bool
    isBeforeTip (epochSlot, slot) = case tip of
      TipGenesis     -> False
      TipEBB epoch   -> epochSlot < EpochSlot epoch 0
      TipBlock slot' -> slot      < slot'
    toTip :: (EpochSlot, SlotNo) -> Tip
    toTip (EpochSlot epoch 0, _)    = TipEBB epoch
    toTip (_                , slot) = TipBlock slot


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
    DontRollBack                            ->                               dbm
    RollBackToGenesis                       -> rollBackToTip TipGenesis      dbm
    RollBackToEpochSlot (EpochSlot epoch 0) -> rollBackToTip (TipEBB epoch)  dbm
    RollBackToEpochSlot epochSlot           -> rollBackToTip (TipBlock slot) dbm
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
      relSlot == CES.lastRelativeSlot (lookupEpochSize dbm epoch')


{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

getTipModel :: MonadState (DBModel hash) m => m Tip
getTipModel = gets dbmTip

deleteAfterModel :: (HasCallStack, MonadState (DBModel hash) m) => Tip -> m ()
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
          TipGenesis        -> True
          TipBlock lastSlot -> slot > lastSlot
          TipEBB _          -> slot >= fromIntegral (length dbmChain)

    when inTheFuture $
      throwUserError err $ ReadFutureSlotError slot dbmTip

    return $ lookupBySlot slot dbmChain

getEBBModel :: (HasCallStack, MonadState (DBModel hash) m)
            => ErrorHandling ImmutableDBError m
            -> EpochNo
            -> m (Maybe (hash, ByteString))
getEBBModel err epoch = do
    DBModel {..} <- get
    let currentEpoch = CES.lastEpoch dbmCumulEpochSizes
        inTheFuture  = case dbmTip of
          TipGenesis -> True
          TipBlock _ -> epoch > currentEpoch
          TipEBB _   -> epoch > currentEpoch

    when inTheFuture $
      throwUserError err $ ReadFutureEBBError epoch currentEpoch

    return $ Map.lookup epoch dbmEBBs

appendBinaryBlobModel :: (HasCallStack, MonadState (DBModel hash) m)
                      => ErrorHandling ImmutableDBError m
                      -> (EpochNo -> EpochSize)
                      -> SlotNo
                      -> Builder
                      -> m ()
appendBinaryBlobModel err getEpochSize slot bld = do
    dbm@DBModel {..} <- get

    -- Check that we're not appending to the past
    let inThePast = case dbmTip of
          TipBlock lastSlot -> slot <= lastSlot
          TipEBB _          -> slot < fromIntegral (length dbmChain)
          TipGenesis        -> False

    when inThePast $
      throwUserError err $ AppendToSlotInThePastError slot dbmTip

    let blob  = BL.toStrict $ BS.toLazyByteString bld
        toPad = fromIntegral (unSlotNo slot) - length dbmChain
        ces'  = addMissingEpochSizes dbmCumulEpochSizes

    -- TODO snoc list?
    put dbm
      { dbmChain           = dbmChain ++ replicate toPad Nothing ++ [Just blob]
      , dbmTip             = TipBlock slot
      , dbmCumulEpochSizes = ces'
      }
  where
    -- | Add 'EpochSize's to 'CumulEpochSizes' until the 'CES.maxSlot' is
    -- greater or equal to the 'SlotNo'.
    addMissingEpochSizes :: CumulEpochSizes -> CumulEpochSizes
    addMissingEpochSizes = execState $
       CES.getNewEpochSizesUntilM done (return . getEpochSize)
      where
        done :: CumulEpochSizes -> Maybe ()
        done ces | slot <= CES.maxSlot ces = Just ()
                 | otherwise               = Nothing


appendEBBModel :: (HasCallStack, MonadState (DBModel hash) m)
               => ErrorHandling ImmutableDBError m
               -> (EpochNo -> EpochSize)
               -> EpochNo
               -> hash
               -> Builder
               -> m ()
appendEBBModel err getEpochSize epoch hash bld = do
    dbm@DBModel {..} <- get
    let currentEpoch = CES.lastEpoch dbmCumulEpochSizes

    -- Check that we're not appending to the past
    let inThePast = case dbmTip of
          -- There is already a block in this epoch, so the EBB can no
          -- longer be appended in this epoch
          TipBlock _ -> epoch <= currentEpoch
          -- There is already an EBB in this epoch
          TipEBB _   -> epoch <= currentEpoch
          TipGenesis -> False

    when inThePast $
      throwUserError err $ AppendToEBBInThePastError epoch currentEpoch

    let blob            = BL.toStrict $ BS.toLazyByteString bld
        ebbEpochSlot    = EpochSlot epoch 0
        (ebbSlot, ces') = addMissingEpochSizes ebbEpochSlot dbmCumulEpochSizes
        toPad           = fromIntegral (unSlotNo ebbSlot) - length dbmChain

    put dbm
      { dbmChain           = dbmChain ++ replicate toPad Nothing
      , dbmTip             = TipEBB epoch
      , dbmCumulEpochSizes = ces'
      , dbmEBBs            = Map.insert epoch (hash, blob) dbmEBBs
      }
  where
    addMissingEpochSizes :: EpochSlot -> CumulEpochSizes
                         -> (SlotNo, CumulEpochSizes)
    addMissingEpochSizes epochSlot = runState $ CES.getNewEpochSizesUntilM
      (`CES.epochSlotToSlot` epochSlot)
      (return . getEpochSize)


streamBinaryBlobsModel :: forall m hash.
                          (HasCallStack, MonadState (DBModel hash) m, Eq hash)
                       => ErrorHandling ImmutableDBError m
                       -> Maybe (SlotNo, hash)
                       -> Maybe (SlotNo, hash)
                       -> m (Iterator hash m)
streamBinaryBlobsModel err mbStart mbEnd = do
    dbm@DBModel {..} <- get

    validateIteratorRange err dbmCumulEpochSizes dbmTip mbStart mbEnd

    let results = iteratorResults dbm
        itm     = IteratorModel results
        itID    = dbmNextIterator
    put dbm
      { dbmNextIterator = succ dbmNextIterator
      , dbmIterators    = Map.insert dbmNextIterator itm dbmIterators
      }
    return Iterator
      { iteratorNext  = iteratorNextModel  itID
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
                  -> m (IteratorResult hash)
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

iteratorCloseModel :: MonadState (DBModel hash) m
                   => IteratorID -> m ()
iteratorCloseModel itID = modify $ \dbm@DBModel {..} ->
    dbm { dbmIterators = Map.delete itID dbmIterators }
