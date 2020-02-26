{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

-- | Model for the 'ImmutableDB' based on a chain.
--
-- The chain is just a list of slots that can be unfilled (@Nothing@) or
-- filled (@Just ByteString@).
module Test.Ouroboros.Storage.ImmutableDB.Model
  ( DBModel(..)
  , InSlot(..)
  , dbmTip
  , dbmRegular
  , dbmEBBs
  , dbmCurrentEpoch
  , dbmBlobs
  , dbmTipBlock
  , dbmBlockList
  , initDBModel
  , IteratorId
  , IteratorModel
  , simulateCorruptions
  , tips
  , closeAllIterators
   -- * ImmutableDB implementation
  , getTipModel
  , reopenModel
  , reopenInThePastModel
  , deleteAfterModel
  , getBlockComponentModel
  , getEBBComponentModel
  , getBlockOrEBBComponentModel
  , appendBlockModel
  , appendEBBModel
  , streamModel
  , iteratorNextModel
  , iteratorPeekModel
  , iteratorHasNextModel
  , iteratorCloseModel
  ) where

import           Control.Monad (when)
import           Control.Monad.Except (MonadError, throwError)

import           Data.Bifunctor (first)
import           Data.ByteString.Builder (Builder, toLazyByteString)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Function ((&))
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Proxy
import qualified Data.Text as Text
import           Data.Word (Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, callStack, popCallStack)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util (lastMaybe, repeatedly)
import           Ouroboros.Consensus.Util.RedundantConstraints

import           Ouroboros.Network.Block (BlockNo, SlotNo (..))

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath, fsPathSplit)
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDB,
                     IteratorResult (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.ChunkInfo
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util (parseDBFile,
                     validateIteratorRange)
import           Ouroboros.Consensus.Storage.ImmutableDB.Layout
import           Ouroboros.Consensus.Storage.ImmutableDB.Types
import           Ouroboros.Consensus.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Consensus.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.TestBlock

data InSlot hash =
    -- | This slot contains only a regular block
    InSlotBlock
      (TipInfo hash SlotNo, BinaryInfo ByteString)

    -- | This slot contains only an EBB
  | InSlotEBB
      (TipInfo hash EpochNo, BinaryInfo ByteString)

    -- | This slot contains an EBB /and/ a regular block
    --
    -- NOTE: EBBs shares
    --
    -- o a block number with their predecessor
    -- o a slot number with their successor
    --
    -- So within the same /slot/, the EBB comes /first/.
  | InSlotBoth
      (TipInfo hash EpochNo, BinaryInfo ByteString)
      (TipInfo hash SlotNo , BinaryInfo ByteString)
  deriving (Show, Generic)

data DBModel hash = DBModel
  { dbmSlots        :: Map SlotNo (InSlot hash)
  , dbmChunkInfo    :: ChunkInfo
  , dbmIterators    :: Map IteratorId (IteratorModel hash)
  , dbmNextIterator :: IteratorId
  } deriving (Show, Generic)

initDBModel :: EpochSize -- ^ We assume fixed epoch size
            -> DBModel hash
initDBModel epochSize = DBModel
  { dbmSlots        = Map.empty
  , dbmChunkInfo    = simpleChunkInfo (unEpochSize epochSize)
  , dbmIterators    = Map.empty
  , dbmNextIterator = 0
  }

insertInSlot :: forall hash. HasCallStack
             => SlotNo
             -> TipInfo hash BlockOrEBB
             -> BinaryInfo ByteString
             -> Map SlotNo (InSlot hash)
             -> Map SlotNo (InSlot hash)
insertInSlot slot info xs =
    Map.alter (Just . go (forgetTipInfo info)) slot
  where
    go :: BlockOrEBB -> Maybe (InSlot hash) -> InSlot hash
    go (Block s) Nothing                  = InSlotBlock    (const s <$> info, xs)
    go (EBB   e) Nothing                  = InSlotEBB      (const e <$> info, xs)
    go (Block s) (Just (InSlotEBB   ebb)) = InSlotBoth ebb (const s <$> info, xs)
    go (EBB   _) (Just (InSlotBlock _  )) = error "insertInSlot: EBB after block"
    go _ _                                = error "insertInSlot: slot already filled"

{-------------------------------------------------------------------------------
  Derived values
-------------------------------------------------------------------------------}

dbmTip :: DBModel hash -> ImmTipWithInfo hash
dbmTip DBModel{..} =
    case Map.lookupMax dbmSlots of
      Nothing              -> TipGen
      Just (_slot, inSlot) -> Tip $
        case inSlot of
          InSlotBlock     (tip, _bytes) -> Block <$> tip
          InSlotEBB       (tip, _bytes) -> EBB   <$> tip
          InSlotBoth _ebb (tip, _bytes) -> Block <$> tip

dbmEBBs :: forall hash.
           DBModel hash -> Map EpochNo (hash, BinaryInfo ByteString)
dbmEBBs =
    Map.fromList . mapMaybe (containsEBB . snd) . Map.toList . dbmSlots
  where
    containsEBB :: InSlot hash
                -> Maybe (EpochNo, (hash, BinaryInfo ByteString))
    containsEBB (InSlotBlock _)                  = Nothing
    containsEBB (InSlotEBB  (tip, bytes))        = Just $ swizzle tip bytes
    containsEBB (InSlotBoth (tip, bytes) _block) = Just $ swizzle tip bytes

    swizzle :: TipInfo hash EpochNo
            -> BinaryInfo ByteString
            -> (EpochNo, (hash, BinaryInfo ByteString))
    swizzle info bytes = (forgetTipInfo info, (tipInfoHash info, bytes))

dbmCurrentEpoch :: DBModel hash -> EpochNo
dbmCurrentEpoch dbm =
    case forgetTipInfo <$> dbmTip dbm of
      TipGen           -> EpochNo 0
      Tip (Block slot) -> slotToEpoch dbm slot
      Tip (EBB epoch') -> epoch'

-- | The chain containing the regular blocks /only/
--
-- Returns all slots from old to new, with 'Nothing' representing empty slots.
-- May end on a 'Nothing' if the chain ends on an EBB.
dbmRegular :: DBModel hash -> [Maybe (hash, BinaryInfo ByteString)]
dbmRegular = expand (SlotNo 0) . Map.toList . dbmSlots
  where
    expand :: SlotNo -- Slot number we expect to see next
           -> [(SlotNo, InSlot hash)]
           -> [Maybe (hash, BinaryInfo ByteString)]
    expand _ []                = []
    expand s ((s', inSlot):ss) = concat [
        replicate skipped Nothing
      , case inSlot of
          InSlotBlock (info, bytes) ->
            Just (tipInfoHash info, bytes) : expand (succ s') ss
          InSlotBoth _ebb (info, bytes) ->
            Just (tipInfoHash info, bytes) : expand (succ s') ss
          InSlotEBB _ ->
            -- EBBs share a slot number with their successor
            expand s' ss
      ]
      where
        skipped :: Int
        skipped = fromIntegral (unSlotNo s' - unSlotNo s)

-- | All blobs in the DB
--
-- 'Left' values denote EBBs.
dbmBlobs :: DBModel hash
         -> Map (EpochSlot, SlotNo)
                (Either (hash, BinaryInfo ByteString)
                        (hash, BinaryInfo ByteString))
dbmBlobs dbm = repeatedly insert (Map.toList $ dbmSlots dbm) Map.empty
  where
    insert (slot, inSlot) =
      case inSlot of
        InSlotBlock regular     -> insertRegular slot regular
        InSlotEBB   ebb         -> insertEBB     slot ebb
        InSlotBoth  ebb regular -> insertRegular slot regular
                                 . insertEBB     slot ebb

    insertRegular slot (info, xs) =
        Map.insert (slotToEpochSlot dbm slot, slot)
                   (Right (tipInfoHash info, xs))

    insertEBB slot (info, xs) =
        Map.insert (epochNoToEpochSlot (forgetTipInfo info), slot)
                   (Left (tipInfoHash info, xs))

-- TODO #1151
dbmTipBlock :: DBModel hash -> Maybe TestBlock
dbmTipBlock dbm = testBlockFromLazyByteString <$> case forgetTipInfo <$> dbmTip dbm of
    TipGen            -> Nothing
    Tip (Block _slot) -> Just $ binaryBlob $ snd $ mustBeJust $ last $ dbmRegular dbm
    Tip (EBB epoch)   -> Just $ binaryBlob $ snd $ dbmEBBs dbm Map.! epoch
  where
    mustBeJust = fromMaybe (error "chain ends with an empty slot")

dbmBlockList :: DBModel hash -> [ByteString]
dbmBlockList = fmap toBlob . Map.elems . dbmBlobs
  where
    toBlob (Left  (_hash, binaryInfo)) = binaryBlob binaryInfo
    toBlob (Right (_hash, binaryInfo)) = binaryBlob binaryInfo

type IteratorId = Int

-- | Model for an 'Iterator'.
--
-- An iterator is open iff its is present in 'dbmIterators'.
--
-- The model of an iterator is just the list of 'IteratorResult's it streams
-- over. Advancing the iterator will yield the first one and should drop it
-- from the model.
newtype IteratorModel hash = IteratorModel [IterRes hash]
  deriving (Show, Eq, Generic)

-- | Short hand. We store @Either EpochNo SlotNo@ and @hash@ to implement
-- 'iteratorHasNext'
type IterRes hash = (Either EpochNo SlotNo, hash, BinaryInfo ByteString)

{-------------------------------------------------------------------------------
  Slot conversions
-------------------------------------------------------------------------------}

epochNoToEpochSlot :: EpochNo -> EpochSlot
epochNoToEpochSlot = (`EpochSlot` 0)

epochNoToSlot :: DBModel hash -> EpochNo -> SlotNo
epochNoToSlot dbm = epochSlotToSlot dbm . epochNoToEpochSlot

slotForBlockOrEBB :: DBModel hash -> BlockOrEBB -> SlotNo
slotForBlockOrEBB dbm (EBB  epoch) = epochNoToSlot dbm epoch
slotForBlockOrEBB _   (Block slot) = slot

slotToEpoch :: DBModel hash -> SlotNo -> EpochNo
slotToEpoch DBModel {..} = runIdentity . epochInfoEpoch dbmChunkInfo

epochSlotToSlot :: DBModel hash -> EpochSlot -> SlotNo
epochSlotToSlot DBModel {..} = runIdentity . epochInfoAbsolute dbmChunkInfo

slotToEpochSlot :: DBModel hash -> SlotNo -> EpochSlot
slotToEpochSlot DBModel {..} = runIdentity . epochInfoBlockRelative dbmChunkInfo

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

throwUserError :: (MonadError ImmutableDBError m, HasCallStack)
               => UserError -> m a
throwUserError e = throwError $ UserError e (popCallStack callStack)

lookupEpochSize :: DBModel hash -> EpochNo -> EpochSize
lookupEpochSize DBModel {..} = runIdentity . epochInfoSize dbmChunkInfo

lookupBySlot :: HasCallStack => SlotNo -> [Maybe b] -> Maybe b
lookupBySlot (SlotNo i) = go i
  where
    go 0 (blob:_)  = blob
    go n (_:blobs) = go (n - 1) blobs
    go _ []        = error ("lookupBySlot: index out of bounds: " <> show i)

-- | Rolls back the chain so that the given 'Tip' is the new tip.
--
-- The user is responsible for giving a valid 'Tip', i.e. a tip that points to
-- a filled slot or an existing EBB (Genesis is always valid). This function
-- will not truncate to the last filled slot or EBB itself.
rollBackToTip :: forall hash. Show hash
              => ImmTip -> DBModel hash -> DBModel hash
rollBackToTip tip dbm@DBModel {..} = case tip of
    TipGen ->
        (initDBModel firstEpochSize) { dbmNextIterator = dbmNextIterator }
      where
        firstEpochSize = lookupEpochSize dbm 0

    Tip (EBB epoch) ->
        dbm { dbmSlots = Map.update deleteRegular (epochNoToSlot dbm epoch)
                       $ Map.filterWithKey shouldKeep
                       $ dbmSlots
            }
      where
        shouldKeep slot _inSlot = slot <= epochNoToSlot dbm epoch

        deleteRegular :: InSlot hash -> Maybe (InSlot hash)
        deleteRegular (InSlotEBB  ebb)   = Just $ InSlotEBB ebb
        deleteRegular (InSlotBoth ebb _) = Just $ InSlotEBB ebb
        deleteRegular (InSlotBlock _)    = Nothing

    Tip (Block slot) ->
        dbm { dbmSlots = Map.filterWithKey shouldKeep $ dbmSlots }
      where
        shouldKeep slot' _inSlot = slot' <= slot
  where
    _ = keepRedundantConstraint (Proxy @(Show hash))

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

properTips :: DBModel hash -> [TipInfo hash BlockOrEBB]
properTips = concatMap go . Map.elems . dbmSlots
  where
    go :: InSlot hash -> [TipInfo hash BlockOrEBB]
    go (InSlotBlock (reg, _))          = [ Block <$> reg ]
    go (InSlotEBB   (ebb, _))          = [ EBB   <$> ebb ]
    go (InSlotBoth  (ebb, _) (reg, _)) = [ EBB   <$> ebb
                                         , Block <$> reg ]

-- | List all 'Tip's that point to a filled slot or an existing EBB in the
-- model, including 'TipGenesis'. The tips will be sorted from old to recent.
tips :: DBModel hash -> NonEmpty (ImmTipWithInfo hash)
tips dbm = TipGen NE.:| map Tip (properTips dbm)

-- | Return the blobs in the given 'EpochNo', in order.
blobsInEpoch :: DBModel hash -> EpochNo -> [ByteString]
blobsInEpoch dbm epoch =
    maybe id (:) mbEBBBlob       $
    map (binaryBlob . snd)       $
    mapMaybe snd                 $
    takeWhile ((== epoch) . fst) $
    dropWhile ((/= epoch) . fst) $
    zip (map (slotToEpoch dbm . SlotNo) [0..]) (dbmRegular dbm)
  where
    mbEBBBlob = binaryBlob . snd <$> Map.lookup epoch (dbmEBBs dbm)

closeAllIterators :: DBModel hash -> DBModel hash
closeAllIterators dbm = dbm { dbmIterators = mempty }

{------------------------------------------------------------------------------
  Simulation corruptions and restoring afterwards
------------------------------------------------------------------------------}

-- | Simulate the following: close the database, apply the corruptions to the
-- respective files, and restore to the last valid epoch.
--
-- The resulting chain will be a prefix of the given chain.
--
-- The 'FsPath's must correspond to index or epoch files that a real database,
-- which is in sync with the given model, would have created on disk.
--
-- Returns the new tip.
simulateCorruptions
  :: Show hash
  => Corruptions -> DBModel hash -> (ImmTipWithInfo hash, DBModel hash)
simulateCorruptions corrs dbm = (dbmTip dbm', dbm')
  where
    dbm' = closeAllIterators $ rollBack rbp dbm
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

rollBack :: Show hash => RollBackPoint -> DBModel hash -> DBModel hash
rollBack rbp dbm = case rbp of
    DontRollBack                            ->                                  dbm
    RollBackToGenesis                       -> rollBackToTip TipGen             dbm
    RollBackToEpochSlot (EpochSlot epoch 0) -> rollBackToTip (Tip (EBB epoch))  dbm
    RollBackToEpochSlot epochSlot           -> rollBackToTip (Tip (Block slot)) dbm
      where
        slot = epochSlotToSlot dbm epochSlot

findCorruptionRollBackPoint :: FileCorruption -> FsPath -> DBModel hash
                            -> RollBackPoint
findCorruptionRollBackPoint corr file dbm =
    case (Text.unpack . snd <$> fsPathSplit file) >>= parseDBFile of
      Just ("epoch",     epoch)  -> findEpochCorruptionRollBackPoint corr epoch dbm
      -- Index files are always recoverable
      Just ("primary",   _epoch) -> DontRollBack
      Just ("secondary", _epoch) -> DontRollBack
      _                          -> error "Invalid file to corrupt"

findEpochRollBackPoint :: Word64 -- ^ The number of valid bytes in the epoch,
                                 -- the corruption happens at the first byte
                                 -- after it.
                       -> EpochNo -> DBModel hash -> RollBackPoint
findEpochRollBackPoint validBytes epoch dbm
    | null epochSlots
      -- If the file is empty, no corruption happened, and we don't have to
      -- roll back
    = DontRollBack
    | Just lastValidEpochSlot <- mbLastValidEpochSlot
    = RollBackToEpochSlot lastValidEpochSlot
    | otherwise
      -- When there are no more filled slots in the epoch file, roll back to
      -- the last filled slot before the epoch.
    = rollbackToLastFilledSlotBefore epoch dbm
  where
    blobs = blobsInEpoch dbm epoch

    epochSlots = epochSlotsInEpoch dbm epoch

    mbLastValidEpochSlot :: Maybe EpochSlot
    mbLastValidEpochSlot = go 0 Nothing (zip epochSlots blobs)
      where
        go :: Word64 -> Maybe EpochSlot -> [(EpochSlot, ByteString)]
           -> Maybe EpochSlot
        go curOffset lastValid = \case
          [] -> lastValid
          (epochSlot, blob):rest
              | curOffset + blobSize <= validBytes
              -> go (curOffset + blobSize) (Just epochSlot) rest
              | otherwise
              -> lastValid
            where
              blobSize = fromIntegral $ Lazy.length blob

findEpochCorruptionRollBackPoint :: FileCorruption -> EpochNo -> DBModel hash
                                 -> RollBackPoint
findEpochCorruptionRollBackPoint corr epoch dbm = case corr of
    DeleteFile      -> rollbackToLastFilledSlotBefore    epoch dbm

    DropLastBytes n -> findEpochRollBackPoint validBytes epoch dbm
      where
        validBytes | n >= totalBytes = 0
                   | otherwise       = totalBytes - n

    Corrupt n       -> findEpochRollBackPoint validBytes epoch dbm
      where
        validBytes = n `mod` totalBytes
  where
    blobs = blobsInEpoch dbm epoch
    totalBytes = fromIntegral $ sum (map Lazy.length blobs)

rollbackToLastFilledSlotBefore :: EpochNo -> DBModel hash -> RollBackPoint
rollbackToLastFilledSlotBefore epoch dbm = case lastMaybe beforeEpoch of
    Just lastFilledSlotBefore -> RollBackToEpochSlot lastFilledSlotBefore
    Nothing                   -> RollBackToGenesis
  where
    (beforeEpoch, _, _) = filledEpochSlots dbm epoch

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

getTipModel :: DBModel hash -> ImmTipWithInfo hash
getTipModel = dbmTip

-- | Close all open iterators and return the current tip
reopenModel :: DBModel hash -> (ImmTipWithInfo hash, DBModel hash)
reopenModel dbm = (dbmTip dbm, closeAllIterators dbm)

-- | Close all open iterators, truncate all blocks > the given slot, and
-- return the current tip.
reopenInThePastModel :: forall hash. Show hash
                     => SlotNo  -- ^ Current slot
                     -> DBModel hash
                     -> (ImmTipWithInfo hash, DBModel hash)
reopenInThePastModel curSlot dbm = (dbmTip dbm', dbm')
  where
    tipsInThePast :: [EpochSlot]
    tipsInThePast =
      [ slotToEpochSlot dbm slot
      | tip <- properTips dbm
      , let slot = slotForBlockOrEBB dbm (forgetTipInfo tip)
      , slot <= curSlot
      ]

    rollBackPoint = case lastMaybe tipsInThePast of
      Nothing        -> RollBackToGenesis
      Just epochSlot -> RollBackToEpochSlot epochSlot

    dbm' = rollBack rollBackPoint $ closeAllIterators dbm

deleteAfterModel :: Show hash
                 => ImmTipWithInfo hash -> DBModel hash -> DBModel hash
deleteAfterModel tip =
      rollBackToTip (forgetTipInfo <$> tip)
    . closeAllIterators

extractBlockComponent
  :: hash
  -> SlotNo
  -> Maybe EpochNo -- ^ Is an EBB
  -> BinaryInfo ByteString
  -> BlockComponent (ImmutableDB hash m) b
  -> b
extractBlockComponent hash slot mbEpoch binaryInfo = \case
    GetBlock      -> ()
    GetRawBlock   -> binaryBlob binaryInfo
    GetHeader     -> ()
    GetRawHeader  -> extractHeader binaryInfo
    GetHash       -> hash
    GetSlot       -> slot
    GetIsEBB      -> case mbEpoch of
      Nothing       -> IsNotEBB
      Just _epochNo -> IsEBB
    GetBlockSize  -> fromIntegral $ Lazy.length $ binaryBlob binaryInfo
    GetHeaderSize -> headerSize binaryInfo
    GetPure a     -> a
    GetApply f bc ->
      extractBlockComponent hash slot mbEpoch binaryInfo f $
      extractBlockComponent hash slot mbEpoch binaryInfo bc

getBlockComponentModel
  :: HasCallStack
  => BlockComponent (ImmutableDB hash m) b
  -> SlotNo
  -> DBModel hash
  -> Either ImmutableDBError (Maybe b)
getBlockComponentModel blockComponent slot dbm@DBModel{..} = do
    -- Check that the slot is not in the future
    let inTheFuture = case forgetTipInfo <$> dbmTip dbm of
          TipGen               -> True
          Tip (Block lastSlot) -> slot > lastSlot
          Tip (EBB  _ebb)      -> slot >= fromIntegral (length (dbmRegular dbm))

    when inTheFuture $
      throwUserError $ ReadFutureSlotError slot (forgetTipInfo <$> dbmTip dbm)

    return $ case lookupBySlot slot (dbmRegular dbm) of
      Nothing                 -> Nothing
      Just (hash, binaryInfo) -> Just $
        extractBlockComponent hash slot Nothing binaryInfo blockComponent

getEBBComponentModel
  :: HasCallStack
  => BlockComponent (ImmutableDB hash m) b
  -> EpochNo
  -> DBModel hash
  -> Either ImmutableDBError (Maybe b)
getEBBComponentModel blockComponent epoch dbm@DBModel {..} = do
    let currentEpoch = dbmCurrentEpoch dbm
        inTheFuture  = epoch > currentEpoch ||
          case dbmTip dbm of
            TipGen -> True
            Tip _  -> False

    when inTheFuture $
      throwUserError $ ReadFutureEBBError epoch currentEpoch

    return $ case Map.lookup epoch (dbmEBBs dbm) of
      Nothing                 -> Nothing
      Just (hash, binaryInfo) -> Just $
          extractBlockComponent hash slot (Just epoch) binaryInfo blockComponent
        where
          slot = epochNoToSlot dbm epoch

getBlockOrEBBComponentModel
  :: (HasCallStack, Eq hash)
  => BlockComponent (ImmutableDB hash m) b
  -> SlotNo
  -> hash
  -> DBModel hash
  -> Either ImmutableDBError (Maybe b)
getBlockOrEBBComponentModel blockComponent slot hash dbm = do
    -- Check that the slot is not in the future
    let inTheFuture = case forgetTipInfo <$> dbmTip dbm of
          TipGen               -> True
          Tip (Block lastSlot) -> slot > lastSlot
          Tip (EBB   epoch)    -> slot > epochNoToSlot dbm epoch

    when inTheFuture $
      throwUserError $ ReadFutureSlotError slot (forgetTipInfo <$> dbmTip dbm)

    let (epoch, couldBeEBB) = case slotToEpochSlot dbm slot of
          EpochSlot e 1 -> (e, True)
          EpochSlot e _ -> (e, False)

    -- The chain can be too short if there's an EBB at the tip
    return $ case lookupBySlotMaybe slot of
      Just (hash', binaryInfo)
        | hash' == hash
        -> Just $ extractBlockComponent hash slot Nothing binaryInfo blockComponent
      -- Fall back to EBB
      _ | couldBeEBB
        , Just (hash', binaryInfo) <- Map.lookup epoch (dbmEBBs dbm)
        , hash' == hash
        -> Just $ extractBlockComponent hash slot (Just epoch) binaryInfo blockComponent
        | otherwise
        -> Nothing
  where
    -- Return 'Nothing' when the chain is too short. In contrast to
    -- 'lookupBySlot', which would throw an error.
    lookupBySlotMaybe (SlotNo i')
      | let i = fromIntegral i'
      , i < length (dbmRegular dbm)
      = dbmRegular dbm !! i
      | otherwise
      = Nothing

appendBlockModel
  :: forall hash. (HasCallStack, Show hash)
  => SlotNo
  -> BlockNo
  -> hash
  -> BinaryInfo Builder
  -> DBModel hash
  -> Either ImmutableDBError (DBModel hash)
appendBlockModel slot block hash binaryInfo dbm@DBModel {..} = do
    -- Check that we're not appending to the past
    let inThePast = case forgetTipInfo <$> dbmTip dbm of
          Tip (Block lastSlot) -> slot <= lastSlot
          Tip (EBB _)          -> slot < fromIntegral (length (dbmRegular dbm))
          TipGen               -> False

    when inThePast $
      throwUserError $ AppendToSlotInThePastError slot (forgetTipInfo <$> dbmTip dbm)

    let binaryInfo' = toLazyByteString <$> binaryInfo
        tipInfo     = TipInfo hash (Block slot) block
    return dbm { dbmSlots = insertInSlot slot tipInfo binaryInfo' dbmSlots }
  where
    _ = keepRedundantConstraint (Proxy @(Show hash))

appendEBBModel
  :: EpochNo
  -> BlockNo
  -> hash
  -> BinaryInfo Builder
  -> DBModel hash
  -> Either ImmutableDBError (DBModel hash)
appendEBBModel epoch block hash binaryInfo dbm@DBModel {..} = do
    -- Check that we're not appending to the past
    let currentEpoch = dbmCurrentEpoch dbm
        inThePast    = epoch <= currentEpoch && case dbmTip dbm of
          TipGen -> False
          Tip _  -> True

    when inThePast $
      throwUserError $ AppendToEBBInThePastError epoch currentEpoch

    let binaryInfo' = toLazyByteString <$> binaryInfo
        ebbSlot     = epochNoToSlot dbm epoch
        tipInfo     = TipInfo hash (EBB epoch) block

    return dbm { dbmSlots = insertInSlot ebbSlot tipInfo binaryInfo' dbmSlots }

streamModel
  :: forall hash. (Eq hash, HasCallStack)
  => Maybe (SlotNo, hash)
  -> Maybe (SlotNo, hash)
  -> DBModel hash
  -> Either ImmutableDBError
            (Either (WrongBoundError hash)
                    (IteratorId, DBModel hash))
streamModel mbStart mbEnd dbm@DBModel {..} = swizzle $ do
    validateIteratorRange err dbmChunkInfo
      (forgetTipInfo <$> dbmTip dbm) mbStart mbEnd

    -- The real implementation checks the end bound first, so we do the
    -- same to get the same errors
    mbEnd'   <- mapM (liftRight . checkBound) mbEnd
    mbStart' <- mapM (liftRight . checkBound) mbStart

    -- 'validateIteratorRange', which doesn't know about hashes, can't
    -- detect that streaming from the regular block to the EBB in the same
    -- slot is invalid, as the EBB comes before the regular block. Here,
    -- we do know about the hashes and 'EpochSlot's.
    case (mbStart', mbEnd') of
      (Just start, Just end)
        | start > end
        -> liftLeft $ throwUserError $ InvalidIteratorRangeError
             (epochSlotToSlot dbm start) (epochSlotToSlot dbm start)
      _ -> return ()

    let results = iteratorResults mbStart' mbEnd'
        itm     = IteratorModel results
        itId    = dbmNextIterator
        dbm'    = dbm
          { dbmNextIterator = succ dbmNextIterator
          , dbmIterators    = Map.insert itId itm dbmIterators
          }
    return (itId, dbm')
  where
    blobs = dbmBlobs dbm

    err :: ErrorHandling
             ImmutableDBError
             (Either (Either ImmutableDBError (WrongBoundError hash)))
    err = EH.embed Left (\case { Left e -> Just e ; Right _ -> Nothing }) EH.monadError

    liftLeft :: Either ImmutableDBError a
             -> Either (Either ImmutableDBError (WrongBoundError hash)) a
    liftLeft  = first Left
    liftRight :: Either (WrongBoundError hash) a
              -> Either (Either ImmutableDBError (WrongBoundError hash)) a
    liftRight = first Right

    swizzle :: Either (Either ImmutableDBError (WrongBoundError hash)) a
            -> Either ImmutableDBError (Either (WrongBoundError hash) a)
    swizzle (Left (Left e))  = Left e
    swizzle (Left (Right e)) = Right (Left e)
    swizzle (Right a)        = Right (Right a)

    checkBound
      :: (SlotNo, hash) -> Either (WrongBoundError hash) EpochSlot
    checkBound (slotNo, hash) = case slotToEpochSlot dbm slotNo of
      EpochSlot epoch 1 ->
        case (Map.lookup (EpochSlot epoch 0, slotNo) blobs,
              Map.lookup (EpochSlot epoch 1, slotNo) blobs) of
          (Nothing, Nothing)
            -> Left $ EmptySlotError slotNo
          (Just res1, _)
            | either fst fst res1 == hash
            -> return $ EpochSlot epoch 0
          (_, Just res2)
            | either fst fst res2 == hash
            -> return $ EpochSlot epoch 1
          (mbRes1, mbRes2)
            -> Left $ WrongHashError slotNo hash $ NE.fromList $
               map (either fst fst) $ catMaybes [mbRes1, mbRes2]
      epochSlot         ->
        case Map.lookup (epochSlot, slotNo) blobs of
          Nothing  -> Left $ EmptySlotError slotNo
          Just res
              | hash' == hash -> return epochSlot
              | otherwise     -> Left $ WrongHashError slotNo hash (hash' NE.:| [])
            where
              hash' = either fst fst res

    iteratorResults
      :: Maybe EpochSlot -> Maybe EpochSlot
      -> [IterRes hash]
    iteratorResults mbStart' mbEnd' =
        blobs
      & Map.toAscList
      & map toIterRes
      & dropUntilStart mbStart'
      & takeUntilEnd mbEnd'
      & map snd

    toIterRes
      :: ((EpochSlot, SlotNo),
          Either (hash, BinaryInfo ByteString)
                 (hash, BinaryInfo ByteString))
      -> ((EpochSlot, SlotNo), IterRes hash)
    toIterRes (k@(EpochSlot epoch _, slot), v) = case v of
      Left  (hash, bi) -> (k, (Left epoch, hash, bi))
      Right (hash, bi) -> (k, (Right slot, hash, bi))

    dropUntilStart
      :: Maybe EpochSlot
      -> [((EpochSlot, SlotNo), a)]
      -> [((EpochSlot, SlotNo), a)]
    dropUntilStart = \case
        Nothing    -> id
        Just start -> dropWhile ((< start) . fst . fst)

    takeUntilEnd
      :: Maybe EpochSlot
      -> [((EpochSlot, SlotNo), a)]
      -> [((EpochSlot, SlotNo), a)]
    takeUntilEnd = \case
        Nothing  -> id
        Just end -> takeWhile ((<= end) . fst . fst)

iteratorNextModel
  :: IteratorId
  -> BlockComponent (ImmutableDB hash m) b
  -> DBModel hash
  -> (IteratorResult b, DBModel hash)
iteratorNextModel itId blockComponent dbm@DBModel {..} =
    case Map.lookup itId dbmIterators of
      Nothing ->
          (IteratorExhausted, dbm)
      Just (IteratorModel []) ->
          (IteratorExhausted, iteratorCloseModel itId dbm)
      Just (IteratorModel ((epochOrSlot, hash, bi):ress)) ->
          (res, dbm')
        where
          dbm' = dbm
            { dbmIterators = Map.insert itId (IteratorModel ress) dbmIterators
            }
          res = IteratorResult $
            extractBlockComponent hash slot mbEpochNo bi blockComponent
          (slot, mbEpochNo) = case epochOrSlot of
            Left epoch  -> (epochNoToSlot dbm epoch, Just epoch)
            Right slot' -> (slot', Nothing)

iteratorPeekModel
  :: IteratorId
  -> BlockComponent (ImmutableDB hash m) b
  -> DBModel hash
  -> IteratorResult b
iteratorPeekModel itId blockComponent dbm@DBModel { dbmIterators } =
    case Map.lookup itId dbmIterators of
      Nothing                      -> IteratorExhausted
      Just (IteratorModel [])      -> IteratorExhausted
      Just (IteratorModel ((epochOrSlot, hash, bi):_)) ->
          IteratorResult $
            extractBlockComponent hash slot mbEpochNo bi blockComponent
        where
          (slot, mbEpochNo) = case epochOrSlot of
            Left epoch  -> (epochNoToSlot dbm epoch, Just epoch)
            Right slot' -> (slot', Nothing)

iteratorHasNextModel :: IteratorId
                     -> DBModel hash
                     -> Maybe (Either EpochNo SlotNo, hash)
iteratorHasNextModel itId DBModel { dbmIterators } =
    case Map.lookup itId dbmIterators of
      Nothing                                         -> Nothing
      Just (IteratorModel [])                         -> Nothing
      Just (IteratorModel ((epochOrSlot, hash, _):_)) -> Just (epochOrSlot, hash)

iteratorCloseModel :: IteratorId -> DBModel hash -> DBModel hash
iteratorCloseModel itId dbm@DBModel { dbmIterators } =
    dbm { dbmIterators = Map.delete itId dbmIterators }
