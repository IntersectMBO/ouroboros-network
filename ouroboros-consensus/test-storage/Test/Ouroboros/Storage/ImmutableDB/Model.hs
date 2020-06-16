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
  , dbmCurrentChunk
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
  , streamAllModel
  , iteratorNextModel
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

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util (lastMaybe, repeatedly)
import           Ouroboros.Consensus.Util.RedundantConstraints

import           Ouroboros.Network.Block (BlockNo, SlotNo (..))

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath, fsPathSplit)
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDB,
                     IteratorResult (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (unsafeChunkNoToEpochNo, unsafeEpochNoToChunkNo)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util (parseDBFile,
                     validateIteratorRange)
import           Ouroboros.Consensus.Storage.ImmutableDB.Types

import           Test.Ouroboros.Storage.TestBlock hiding (EBB)

data InSlot hash =
    -- | This slot contains only a regular block
    InSlotBlock
      (TipInfo hash SlotNo, ByteString, BinaryBlockInfo)

    -- | This slot contains only an EBB
  | InSlotEBB
      (TipInfo hash EpochNo, ByteString, BinaryBlockInfo)

    -- | This slot contains an EBB /and/ a regular block
    --
    -- NOTE: EBBs shares
    --
    -- o a block number with their predecessor
    -- o a slot number with their successor
    --
    -- So within the same /slot/, the EBB comes /first/.
  | InSlotBoth
      (TipInfo hash EpochNo, ByteString, BinaryBlockInfo)
      (TipInfo hash SlotNo , ByteString, BinaryBlockInfo)
  deriving (Show, Generic)

data DBModel hash = DBModel
  { dbmSlots        :: Map SlotNo (InSlot hash)
  , dbmChunkInfo    :: ChunkInfo
  , dbmIterators    :: Map IteratorId (IteratorModel hash)
  , dbmNextIterator :: IteratorId
  , dbmPrefixLen    :: PrefixLen
  } deriving (Show, Generic)

initDBModel :: ChunkInfo -> PrefixLen -> DBModel hash
initDBModel chunkInfo prefixLen = DBModel
  { dbmSlots        = Map.empty
  , dbmChunkInfo    = chunkInfo
  , dbmIterators    = Map.empty
  , dbmNextIterator = 0
  , dbmPrefixLen    = prefixLen
  }

insertInSlot :: forall hash. HasCallStack
             => SlotNo
             -> TipInfo hash BlockOrEBB
             -> ByteString
             -> BinaryBlockInfo
             -> Map SlotNo (InSlot hash)
             -> Map SlotNo (InSlot hash)
insertInSlot slot info bytes binfo =
    Map.alter (Just . go (forgetTipInfo info)) slot
  where
    go :: BlockOrEBB -> Maybe (InSlot hash) -> InSlot hash
    go (Block s) Nothing                  = InSlotBlock    (const s <$> info, bytes, binfo)
    go (EBB   e) Nothing                  = InSlotEBB      (const e <$> info, bytes, binfo)
    go (Block s) (Just (InSlotEBB   ebb)) = InSlotBoth ebb (const s <$> info, bytes, binfo)
    go (EBB   _) (Just (InSlotBlock _  )) = error "insertInSlot: EBB after block"
    go _ _                                = error "insertInSlot: slot already filled"

{-------------------------------------------------------------------------------
  Derived values
-------------------------------------------------------------------------------}

dbmTip :: DBModel hash -> ImmTipWithInfo hash
dbmTip DBModel{..} =
    case Map.lookupMax dbmSlots of
      Nothing              -> Origin
      Just (_slot, inSlot) -> At $
        case inSlot of
          InSlotBlock     (tip, _bytes, _binfo) -> Block <$> tip
          InSlotEBB       (tip, _bytes, _binfo) -> EBB   <$> tip
          InSlotBoth _ebb (tip, _bytes, _binfo) -> Block <$> tip

dbmEBBs :: forall hash.
           DBModel hash -> Map ChunkNo (hash, ByteString, BinaryBlockInfo)
dbmEBBs =
    Map.fromList . mapMaybe (containsEBB . snd) . Map.toList . dbmSlots
  where
    containsEBB :: InSlot hash
                -> Maybe (ChunkNo, (hash, ByteString, BinaryBlockInfo))
    containsEBB (InSlotBlock _)                  = Nothing
    containsEBB (InSlotEBB  (tip, bytes, binfo))        = Just $ swizzle tip bytes binfo
    containsEBB (InSlotBoth (tip, bytes, binfo) _block) = Just $ swizzle tip bytes binfo

    swizzle :: TipInfo hash EpochNo
            -> ByteString
            -> BinaryBlockInfo
            -> (ChunkNo, (hash, ByteString, BinaryBlockInfo))
    swizzle info bytes binfo = (
          unsafeEpochNoToChunkNo $ forgetTipInfo info
        , (tipInfoHash info, bytes, binfo)
        )

dbmCurrentChunk :: DBModel hash -> ChunkNo
dbmCurrentChunk dbm =
    case forgetTipInfo <$> dbmTip dbm of
      Origin          -> firstChunkNo
      At (Block slot) -> chunkIndexOfSlot' dbm slot
      At (EBB epoch') -> unsafeEpochNoToChunkNo epoch'

-- | The chain containing the regular blocks /only/
--
-- Returns all slots from old to new, with 'Nothing' representing empty slots.
-- May end on a 'Nothing' if the chain ends on an EBB.
dbmRegular :: DBModel hash -> [Maybe (hash, ByteString, BinaryBlockInfo)]
dbmRegular = expand (SlotNo 0) . Map.toList . dbmSlots
  where
    expand :: SlotNo -- Slot number we expect to see next
           -> [(SlotNo, InSlot hash)]
           -> [Maybe (hash, ByteString, BinaryBlockInfo)]
    expand _ []                = []
    expand s ((s', inSlot):ss) = concat [
        replicate skipped Nothing
      , case inSlot of
          InSlotBlock (info, bytes, binfo) ->
            Just (tipInfoHash info, bytes, binfo) : expand (succ s') ss
          InSlotBoth _ebb (info, bytes, binfo) ->
            Just (tipInfoHash info, bytes, binfo) : expand (succ s') ss
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
         -> Map (ChunkSlot, SlotNo)
                (Either (hash, ByteString, BinaryBlockInfo)
                        (hash, ByteString, BinaryBlockInfo))
dbmBlobs dbm = repeatedly insert (Map.toList $ dbmSlots dbm) Map.empty
  where
    insert (slot, inSlot) =
      case inSlot of
        InSlotBlock regular     -> insertRegular slot regular
        InSlotEBB   ebb         -> insertEBB     slot ebb
        InSlotBoth  ebb regular -> insertRegular slot regular
                                 . insertEBB     slot ebb

    insertRegular slot (info, bytes, binfo) =
        Map.insert (chunkSlotForRegularBlock' dbm slot, slot)
                   (Right (tipInfoHash info, bytes, binfo))

    insertEBB slot (info, bytes, binfo) =
        Map.insert (chunkSlotForBoundaryBlock' dbm (forgetTipInfo info), slot)
                   (Left (tipInfoHash info, bytes, binfo))

-- TODO #1151
dbmTipBlock :: DBModel hash -> Maybe TestBlock
dbmTipBlock dbm = testBlockFromLazyByteString <$> case forgetTipInfo <$> dbmTip dbm of
    Origin           -> Nothing
    At (Block _slot) -> Just $ getBytes $ mustBeJust $ last $ dbmRegular dbm
    At (EBB epoch)   -> Just $ getBytes $ dbmEBBs dbm Map.! (unsafeEpochNoToChunkNo epoch)
  where
    mustBeJust = fromMaybe (error "chain ends with an empty slot")

    getBytes :: (hash, ByteString, BinaryBlockInfo) -> ByteString
    getBytes (_, bytes, _) = bytes

dbmBlockList :: DBModel hash -> [ByteString]
dbmBlockList = fmap getBytes . Map.elems . dbmBlobs
  where
    getBytes (Left  (_hash, bytes, _binfo)) = bytes
    getBytes (Right (_hash, bytes, _binfo)) = bytes

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

-- | Short hand. We store @Either EpochNo SlotNo@ (to distinguish regular blocks
-- from EBBs) and @hash@ to implement 'iteratorHasNext'.
type IterRes hash = (Either EpochNo SlotNo, hash, ByteString, BinaryBlockInfo)

{-------------------------------------------------------------------------------
  Convenience: lift slot conversions to 'DBModel'
-------------------------------------------------------------------------------}

slotNoOfBlockOrEBB' :: DBModel hash -> BlockOrEBB -> SlotNo
slotNoOfBlockOrEBB' = slotNoOfBlockOrEBB . dbmChunkInfo

slotNoOfEBB' :: HasCallStack => DBModel hash -> EpochNo -> SlotNo
slotNoOfEBB' = slotNoOfEBB . dbmChunkInfo

chunkIndexOfSlot' :: DBModel hash -> SlotNo -> ChunkNo
chunkIndexOfSlot' = chunkIndexOfSlot . dbmChunkInfo

chunkSlotToSlot' :: DBModel hash -> ChunkSlot -> SlotNo
chunkSlotToSlot' = chunkSlotToSlot . dbmChunkInfo

chunkSlotForUnknownBlock' :: DBModel hash
                          -> SlotNo
                          -> (ChunkNo, Maybe ChunkSlot, ChunkSlot)
chunkSlotForUnknownBlock' = chunkSlotForUnknownBlock . dbmChunkInfo

chunkSlotForRegularBlock' :: DBModel hash -> SlotNo -> ChunkSlot
chunkSlotForRegularBlock' = chunkSlotForRegularBlock . dbmChunkInfo

chunkSlotForBoundaryBlock' :: DBModel hash -> EpochNo -> ChunkSlot
chunkSlotForBoundaryBlock' = chunkSlotForBoundaryBlock . dbmChunkInfo

chunkSlotForBlockOrEBB' :: DBModel hash -> BlockOrEBB -> ChunkSlot
chunkSlotForBlockOrEBB' = chunkSlotForBlockOrEBB . dbmChunkInfo

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

throwUserError :: (MonadError ImmutableDBError m, HasCallStack)
               => UserError -> m a
throwUserError e = throwError $ UserError e (popCallStack callStack)

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
    Origin ->
        (initDBModel dbmChunkInfo dbmPrefixLen) {
            dbmNextIterator = dbmNextIterator
          }

    At (EBB epoch) ->
        dbm { dbmSlots = Map.update deleteRegular (slotNoOfEBB' dbm epoch)
                       $ Map.filterWithKey shouldKeep
                       $ dbmSlots
            }
      where
        shouldKeep slot _inSlot = slot <= slotNoOfEBB' dbm epoch

        deleteRegular :: InSlot hash -> Maybe (InSlot hash)
        deleteRegular (InSlotEBB  ebb)   = Just $ InSlotEBB ebb
        deleteRegular (InSlotBoth ebb _) = Just $ InSlotEBB ebb
        deleteRegular (InSlotBlock _)    = Nothing

    At (Block slot) ->
        dbm { dbmSlots = Map.filterWithKey shouldKeep $ dbmSlots }
      where
        shouldKeep slot' _inSlot = slot' <= slot
  where
    _ = keepRedundantConstraint (Proxy @(Show hash))

-- | Return the filled 'ChunkSlot's of the given 'ChunkNo' stored in the model.
chunkSlotsInChunk :: DBModel hash -> ChunkNo -> [ChunkSlot]
chunkSlotsInChunk dbm epoch =
    filter ((== epoch) . chunkIndex) $
    map (fst . fst) $
    Map.toAscList $ dbmBlobs dbm

-- | Return the filled 'ChunkSlot's (including EBBs) before, in, and after the
-- given 'ChunkNo'.
filledChunkSlots :: DBModel hash
                 -> ChunkNo
                 -> ([ChunkSlot], [ChunkSlot], [ChunkSlot])
filledChunkSlots dbm chunk = (lt, eq, gt)
  where
    increasingChunkSlots = map (fst . fst) $ Map.toAscList $ dbmBlobs dbm
    (lt, geq) = span ((< chunk)             . chunkIndex) increasingChunkSlots
    (eq, gt)  = span ((< nextChunkNo chunk) . chunkIndex) geq

properTips :: DBModel hash -> [TipInfo hash BlockOrEBB]
properTips = concatMap go . Map.elems . dbmSlots
  where
    go :: InSlot hash -> [TipInfo hash BlockOrEBB]
    go (InSlotBlock (reg, _, _))             = [ Block <$> reg ]
    go (InSlotEBB   (ebb, _, _))             = [ EBB   <$> ebb ]
    go (InSlotBoth  (ebb, _, _) (reg, _, _)) = [ EBB   <$> ebb
                                               , Block <$> reg ]

-- | List all 'Tip's that point to a filled slot or an existing EBB in the
-- model, including 'TipGenesis'. The tips will be sorted from old to recent.
tips :: DBModel hash -> NonEmpty (ImmTipWithInfo hash)
tips dbm = Origin NE.:| map At (properTips dbm)

-- | Return the blobs in the given 'EpochNo', in order.
blobsInChunk :: DBModel hash -> ChunkNo -> [ByteString]
blobsInChunk dbm chunk =
    maybe id (:) mbEBBBlob       $
    map getBytes                 $
    mapMaybe snd                 $
    takeWhile ((== chunk) . fst) $
    dropWhile ((/= chunk) . fst) $
    zip (map (chunkIndexOfSlot' dbm . SlotNo) [0..]) (dbmRegular dbm)
  where
    mbEBBBlob = getBytes <$> Map.lookup chunk (dbmEBBs dbm)

    getBytes (_, bytes, _) = bytes

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
  | RollBackToChunkSlot ChunkSlot
    -- ^ Roll back to the 'ChunkSlot', keeping it as the last relative slot.
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
    (RollBackToChunkSlot es1, RollBackToChunkSlot es2) -> compare es1 es2

rollBack :: Show hash => RollBackPoint -> DBModel hash -> DBModel hash
rollBack rbp dbm = case rbp of
    DontRollBack ->
      dbm
    RollBackToGenesis ->
      rollBackToTip Origin dbm
    RollBackToChunkSlot epochSlot@(ChunkSlot chunk relSlot) ->
      case relativeSlotIsEBB relSlot of
        IsEBB ->
            rollBackToTip (At (EBB epoch)) dbm
          where
            epoch = unsafeChunkNoToEpochNo chunk
        IsNotEBB ->
           rollBackToTip (At (Block slot)) dbm
          where
            slot = chunkSlotToSlot' dbm epochSlot

findCorruptionRollBackPoint :: FileCorruption -> FsPath -> DBModel hash
                            -> RollBackPoint
findCorruptionRollBackPoint corr file dbm =
    case (Text.unpack . snd <$> fsPathSplit file) >>= parseDBFile of
      Just ("chunk",      chunk) -> findChunkCorruptionRollBackPoint corr chunk dbm
      -- Index files are always recoverable
      Just ("primary",   _chunk) -> DontRollBack
      Just ("secondary", _chunk) -> DontRollBack
      _                          -> error "Invalid file to corrupt"

findChunkRollBackPoint :: Word64 -- ^ The number of valid bytes in the chunk,
                                 -- the corruption happens at the first byte
                                 -- after it.
                       -> ChunkNo -> DBModel hash -> RollBackPoint
findChunkRollBackPoint validBytes chunk dbm
    | null chunkSlots
      -- If the file is empty, no corruption happened, and we don't have to
      -- roll back
    = DontRollBack
    | Just lastValidEpochSlot <- mbLastValidEpochSlot
    = RollBackToChunkSlot lastValidEpochSlot
    | otherwise
      -- When there are no more filled slots in the epoch file, roll back to
      -- the last filled slot before the epoch.
    = rollbackToLastFilledSlotBefore chunk dbm
  where
    blobs = blobsInChunk dbm chunk

    chunkSlots = chunkSlotsInChunk dbm chunk

    mbLastValidEpochSlot :: Maybe ChunkSlot
    mbLastValidEpochSlot = go 0 Nothing (zip chunkSlots blobs)
      where
        go :: Word64 -> Maybe ChunkSlot -> [(ChunkSlot, ByteString)]
           -> Maybe ChunkSlot
        go curOffset lastValid = \case
          [] -> lastValid
          (epochSlot, blob):rest
              | curOffset + blobSize <= validBytes
              -> go (curOffset + blobSize) (Just epochSlot) rest
              | otherwise
              -> lastValid
            where
              blobSize = fromIntegral $ Lazy.length blob

findChunkCorruptionRollBackPoint :: FileCorruption -> ChunkNo -> DBModel hash
                                 -> RollBackPoint
findChunkCorruptionRollBackPoint corr chunk dbm = case corr of
    DeleteFile      -> rollbackToLastFilledSlotBefore    chunk dbm

    DropLastBytes n -> findChunkRollBackPoint validBytes chunk dbm
      where
        validBytes | n >= totalBytes = 0
                   | otherwise       = totalBytes - n

    Corrupt n       -> findChunkRollBackPoint validBytes chunk dbm
      where
        validBytes = n `mod` totalBytes
  where
    blobs = blobsInChunk dbm chunk
    totalBytes = fromIntegral $ sum (map Lazy.length blobs)

rollbackToLastFilledSlotBefore :: ChunkNo -> DBModel hash -> RollBackPoint
rollbackToLastFilledSlotBefore chunk dbm = case lastMaybe beforeEpoch of
    Just lastFilledSlotBefore -> RollBackToChunkSlot lastFilledSlotBefore
    Nothing                   -> RollBackToGenesis
  where
    (beforeEpoch, _, _) = filledChunkSlots dbm chunk

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
    tipsInThePast :: [ChunkSlot]
    tipsInThePast =
      [ chunkSlotForBlockOrEBB' dbm (forgetTipInfo tip)
      | tip <- properTips dbm
      , let slot = slotNoOfBlockOrEBB' dbm (forgetTipInfo tip)
      , slot <= curSlot
      ]

    rollBackPoint = case lastMaybe tipsInThePast of
      Nothing        -> RollBackToGenesis
      Just epochSlot -> RollBackToChunkSlot epochSlot

    dbm' = rollBack rollBackPoint $ closeAllIterators dbm

deleteAfterModel :: Show hash
                 => ImmTipWithInfo hash -> DBModel hash -> DBModel hash
deleteAfterModel tip =
      rollBackToTip (forgetTipInfo <$> tip)
    . closeAllIterators

extractBlockComponent
  :: PrefixLen
  -> hash
  -> SlotNo
  -> IsEBB
  -> ByteString
  -> BinaryBlockInfo
  -> BlockComponent (ImmutableDB hash m) b
  -> b
extractBlockComponent prefixLen hash slot isEBB bytes binfo = \case
    GetBlock      -> ()
    GetRawBlock   -> bytes
    GetHeader     -> ()
    GetRawHeader  -> extractHeader binfo bytes
    GetHash       -> hash
    GetSlot       -> slot
    GetIsEBB      -> isEBB
    GetBlockSize  -> fromIntegral $ Lazy.length bytes
    GetHeaderSize -> headerSize binfo
    GetNestedCtxt -> takePrefix prefixLen bytes
    GetPure a     -> a
    GetApply f bc ->
      extractBlockComponent prefixLen hash slot isEBB bytes binfo f $
      extractBlockComponent prefixLen hash slot isEBB bytes binfo bc

getBlockComponentModel
  :: HasCallStack
  => BlockComponent (ImmutableDB hash m) b
  -> SlotNo
  -> DBModel hash
  -> Either ImmutableDBError (Maybe b)
getBlockComponentModel blockComponent slot dbm@DBModel{..} = do
    -- Check that the slot is not in the future
    let inTheFuture = case forgetTipInfo <$> dbmTip dbm of
          Origin              -> True
          At (Block lastSlot) -> slot > lastSlot
          At (EBB  _ebb)      -> slot >= fromIntegral (length (dbmRegular dbm))

    when inTheFuture $
      throwUserError $ ReadFutureSlotError slot (forgetTipInfo <$> dbmTip dbm)

    return $ case lookupBySlot slot (dbmRegular dbm) of
      Nothing                   -> Nothing
      Just (hash, bytes, binfo) -> Just $
        extractBlockComponent
          dbmPrefixLen
          hash
          slot
          IsNotEBB
          bytes
          binfo
          blockComponent

getEBBComponentModel
  :: HasCallStack
  => BlockComponent (ImmutableDB hash m) b
  -> EpochNo
  -> DBModel hash
  -> Either ImmutableDBError (Maybe b)
getEBBComponentModel blockComponent epoch dbm@DBModel {..} = do
    let currentEpoch = dbmCurrentChunk dbm
        chunk        = unsafeEpochNoToChunkNo epoch
        inTheFuture  = chunk > currentEpoch ||
          case dbmTip dbm of
            Origin -> True
            At _   -> False

    when inTheFuture $
      throwUserError $ ReadFutureEBBError epoch currentEpoch

    return $ case Map.lookup chunk (dbmEBBs dbm) of
      Nothing                   -> Nothing
      Just (hash, bytes, binfo) -> Just $
          extractBlockComponent
            dbmPrefixLen
            hash
            slot
            IsEBB
            bytes
            binfo
            blockComponent
        where
          slot = slotNoOfEBB' dbm epoch

getBlockOrEBBComponentModel
  :: (HasCallStack, Eq hash)
  => BlockComponent (ImmutableDB hash m) b
  -> SlotNo
  -> hash
  -> DBModel hash
  -> Either ImmutableDBError (Maybe b)
getBlockOrEBBComponentModel blockComponent slot hash dbm@DBModel { dbmPrefixLen } = do
    -- Check that the slot is not in the future
    let inTheFuture = case forgetTipInfo <$> dbmTip dbm of
          Origin              -> True
          At (Block lastSlot) -> slot > lastSlot
          At (EBB   epoch)    -> slot > slotNoOfEBB' dbm epoch

    when inTheFuture $
      throwUserError $ ReadFutureSlotError slot (forgetTipInfo <$> dbmTip dbm)

    let (chunk, mIfBoundary, _ifRegular) = chunkSlotForUnknownBlock' dbm slot

    -- The chain can be too short if there's an EBB at the tip
    return $ case lookupBySlotMaybe slot of
      Just (hash', bytes, binfo)
        | hash' == hash
        -> Just $ extractBlockComponent dbmPrefixLen hash slot IsNotEBB bytes binfo blockComponent
      -- Fall back to EBB
      _ | Just _ifBoundary <- mIfBoundary
        , Just (hash', bytes, binfo) <- Map.lookup chunk (dbmEBBs dbm)
        , hash' == hash
        -> Just $ extractBlockComponent dbmPrefixLen hash slot IsEBB bytes binfo blockComponent
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
  -> Builder
  -> BinaryBlockInfo
  -> DBModel hash
  -> Either ImmutableDBError (DBModel hash)
appendBlockModel slot block hash builder binfo dbm@DBModel {..} = do
    -- Check that we're not appending to the past
    let inThePast = case forgetTipInfo <$> dbmTip dbm of
          At (Block lastSlot) -> slot <= lastSlot
          At (EBB _)          -> slot < fromIntegral (length (dbmRegular dbm))
          Origin              -> False

    when inThePast $
      throwUserError $ AppendToSlotInThePastError slot (forgetTipInfo <$> dbmTip dbm)

    let bytes   = toLazyByteString builder
        tipInfo = TipInfo hash (Block slot) block
    return dbm { dbmSlots = insertInSlot slot tipInfo bytes binfo dbmSlots }
  where
    _ = keepRedundantConstraint (Proxy @(Show hash))

appendEBBModel
  :: forall hash. (Show hash, HasCallStack)
  => EpochNo
  -> BlockNo
  -> hash
  -> Builder
  -> BinaryBlockInfo
  -> DBModel hash
  -> Either ImmutableDBError (DBModel hash)
appendEBBModel epoch block hash builder binfo dbm@DBModel {..} = do
    -- Check that we're not appending to the past
    let currentChunk = dbmCurrentChunk dbm
        chunk        = unsafeEpochNoToChunkNo epoch
        inThePast    = chunk <= currentChunk && case dbmTip dbm of
          Origin -> False
          At _   -> True

    when inThePast $
      throwUserError $ AppendToEBBInThePastError epoch currentChunk

    let bytes   = toLazyByteString builder
        ebbSlot = slotNoOfEBB' dbm epoch
        tipInfo = TipInfo hash (EBB epoch) block

    return dbm { dbmSlots = insertInSlot ebbSlot tipInfo bytes binfo dbmSlots }
  where
    _ = keepRedundantConstraint (Proxy @(Show hash))

streamModel
  :: forall hash. (Eq hash, HasCallStack)
  => Maybe (SlotNo, hash)
  -> Maybe (SlotNo, hash)
  -> DBModel hash
  -> Either ImmutableDBError
            (Either (WrongBoundError hash)
                    (IteratorId, DBModel hash))
streamModel mbStart mbEnd dbm@DBModel {..} = swizzle $ do
    liftLeft $ runIdentity $
      validateIteratorRange dbmChunkInfo (forgetTipInfo <$> dbmTip dbm)
        mbStart mbEnd

    -- The real implementation checks the end bound first, so we do the
    -- same to get the same errors
    mbEnd'   <- mapM (liftRight . checkBound) mbEnd
    mbStart' <- mapM (liftRight . checkBound) mbStart

    -- 'validateIteratorRange', which doesn't know about hashes, can't
    -- detect that streaming from the regular block to the EBB in the same
    -- slot is invalid, as the EBB comes before the regular block. Here,
    -- we do know about the hashes and 'ChunkSlot's.
    case (mbStart', mbEnd') of
      (Just start, Just end)
        | start > end
        -> liftLeft $ throwUserError $ InvalidIteratorRangeError
             (chunkSlotToSlot' dbm start) (chunkSlotToSlot' dbm start)
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
      :: (SlotNo, hash) -> Either (WrongBoundError hash) ChunkSlot
    checkBound (slotNo, hash) =
      let (_chunk, mIfBoundary, ifRegular) = chunkSlotForUnknownBlock' dbm slotNo in
      case mIfBoundary of
        Just ifBoundary ->
          case (Map.lookup (ifBoundary, slotNo) blobs,
                Map.lookup (ifRegular , slotNo) blobs) of
            (Nothing, Nothing)
              -> Left $ EmptySlotError slotNo
            (Just res1, _)
              | getHash res1 == hash
              -> return $ ifBoundary
            (_, Just res2)
              | getHash res2 == hash
              -> return $ ifRegular
            (mbRes1, mbRes2)
              -> Left $ WrongHashError slotNo hash $ NE.fromList $
                 map getHash $ catMaybes [mbRes1, mbRes2]
        Nothing ->
          case Map.lookup (ifRegular, slotNo) blobs of
            Nothing  -> Left $ EmptySlotError slotNo
            Just res
                | hash' == hash -> return ifRegular
                | otherwise     -> Left $ WrongHashError slotNo hash (hash' NE.:| [])
              where
                hash' = getHash res

    getHash :: Either
                 (hash, ByteString, BinaryBlockInfo)
                 (hash, ByteString, BinaryBlockInfo)
            -> hash
    getHash (Left  (hash, _, _)) = hash
    getHash (Right (hash, _, _)) = hash

    iteratorResults
      :: Maybe ChunkSlot -> Maybe ChunkSlot
      -> [IterRes hash]
    iteratorResults mbStart' mbEnd' =
        blobs
      & Map.toAscList
      & map toIterRes
      & dropUntilStart mbStart'
      & takeUntilEnd mbEnd'
      & map snd

    toIterRes
      :: ((ChunkSlot, SlotNo),
          Either (hash, ByteString, BinaryBlockInfo)
                 (hash, ByteString, BinaryBlockInfo))
      -> ((ChunkSlot, SlotNo), IterRes hash)
    toIterRes (k@(ChunkSlot epoch _, slot), v) = case v of
      Left  (hash, bytes, binfo) -> (k, (Left (unsafeChunkNoToEpochNo epoch), hash, bytes, binfo))
      Right (hash, bytes, binfo) -> (k, (Right slot, hash, bytes, binfo))

    dropUntilStart
      :: Maybe ChunkSlot
      -> [((ChunkSlot, SlotNo), a)]
      -> [((ChunkSlot, SlotNo), a)]
    dropUntilStart = \case
        Nothing    -> id
        Just start -> dropWhile ((< start) . fst . fst)

    takeUntilEnd
      :: Maybe ChunkSlot
      -> [((ChunkSlot, SlotNo), a)]
      -> [((ChunkSlot, SlotNo), a)]
    takeUntilEnd = \case
        Nothing  -> id
        Just end -> takeWhile ((<= end) . fst . fst)

streamAllModel
  :: forall m hash b.
     BlockComponent (ImmutableDB hash m) b
  -> DBModel hash
  -> [b]
streamAllModel blockComponent dbm@DBModel { dbmPrefixLen } =
      map toBlockComponent
    . Map.toAscList
    . dbmBlobs
    $ dbm
  where
    toBlockComponent
      :: ((ChunkSlot, SlotNo),
          Either (hash, ByteString, BinaryBlockInfo) (hash, ByteString, BinaryBlockInfo))
      -> b
    toBlockComponent ((_chunkSlot, slotNo), ebbOrBlock) =
        extractBlockComponent dbmPrefixLen hash slotNo isEBB bytes binfo blockComponent
      where
        (isEBB, hash, bytes, binfo) = case ebbOrBlock of
          Left  (h, b, bi) -> (IsEBB,    h, b, bi)
          Right (h, b, bi) -> (IsNotEBB, h, b, bi)

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

      Just (IteratorModel ((epochOrSlot, hash, bytes, binfo):ress)) ->
          (res, dbm')
        where
          dbm' = dbm
            { dbmIterators = Map.insert itId (IteratorModel ress) dbmIterators
            }

          res = IteratorResult $
            extractBlockComponent dbmPrefixLen hash slot isEBB bytes binfo blockComponent

          (slot, isEBB) = case epochOrSlot of
            Left epoch  -> (slotNoOfEBB' dbm epoch, IsEBB)
            Right slot' -> (slot', IsNotEBB)

iteratorHasNextModel :: IteratorId
                     -> DBModel hash
                     -> Maybe (Either EpochNo SlotNo, hash)
iteratorHasNextModel itId DBModel { dbmIterators } =
    case Map.lookup itId dbmIterators of
      Nothing                                            -> Nothing
      Just (IteratorModel [])                            -> Nothing
      Just (IteratorModel ((epochOrSlot, hash, _, _):_)) -> Just (epochOrSlot, hash)

iteratorCloseModel :: IteratorId -> DBModel hash -> DBModel hash
iteratorCloseModel itId dbm@DBModel { dbmIterators } =
    dbm { dbmIterators = Map.delete itId dbmIterators }
