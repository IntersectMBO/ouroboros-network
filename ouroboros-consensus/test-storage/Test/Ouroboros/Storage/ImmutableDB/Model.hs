{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
  , dbmTipBlock
  , initDBModel
  , IteratorModel
  , openDBModel
  , simulateCorruptions
  , rollBackToTip
  , tips
  ) where

import           Control.Monad (when)
import           Control.Monad.Except (lift, liftEither, runExceptT)
import           Control.Monad.State.Strict (MonadState, get, gets, modify, put)

import           Data.ByteString.Builder (Builder, toLazyByteString)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Function ((&))
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import qualified Data.Text as Text
import           Data.Word (Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util (lastMaybe)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API.Types (FsPath, fsPathSplit)
import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl (Internal (..))
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Test.Ouroboros.Storage.TestBlock

data DBModel hash = DBModel
  { dbmChain        :: [Maybe (hash, BinaryInfo ByteString)]
    -- ^ 'Nothing' when a slot is empty
  , dbmTip          :: ImmTipWithHash hash
  , dbmEBBs         :: Map EpochNo (hash, BinaryInfo ByteString)
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
    case forgetHash <$> dbmTip of
      TipGen           -> EpochNo 0
      Tip (Block slot) -> slotToEpoch dbm slot
      Tip (EBB epoch') -> epoch'

dbmBlobs :: DBModel hash
         -> Map (EpochSlot, SlotNo)
                (Either (hash, BinaryInfo ByteString)
                        (hash, BinaryInfo ByteString))
dbmBlobs dbm@DBModel {..} = foldr add ebbs (zip (map SlotNo [0..]) dbmChain)
  where
    add (_,    Nothing)                = id
    add (slot, Just hashAndBinaryInfo) =
      Map.insert (slotToEpochSlot dbm slot, slot) (Right hashAndBinaryInfo)

    ebbs = dbmEBBs
         & Map.map Left
         & Map.mapKeysMonotonic (`EpochSlot` 0)
         & Map.mapKeysMonotonic (\epochSlot ->
             (epochSlot, epochSlotToSlot dbm epochSlot))

-- TODO #1151
dbmTipBlock :: DBModel hash -> Maybe TestBlock
dbmTipBlock dbm = testBlockFromLazyByteString <$> case forgetHash <$> dbmTip dbm of
    TipGen            -> Nothing
    Tip (Block _slot) -> Just $ binaryBlob $ snd $ mustBeJust $ last $ dbmChain dbm
    Tip (EBB epoch)   -> Just $ binaryBlob $ snd $ dbmEBBs dbm Map.! epoch
  where
    mustBeJust = fromMaybe (error "chain ends with an empty slot")

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

{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

openDBModel :: (MonadState (DBModel hash) m, Eq hash)
            => ErrorHandling ImmutableDBError m
            -> (EpochNo -> EpochSize)
            -> (DBModel hash, ImmutableDB hash m, Internal hash m)
openDBModel err getEpochSize = (dbModel, db, internal)
  where
    dbModel = initDBModel (getEpochSize 0)
    db = ImmutableDB
      { closeDB                = return ()
      , isOpen                 = return True
      , reopen                 = \_ -> return ()
      , getTip                 = getTipModel
      , getBlockComponent      = getBlockComponentModel      err
      , getEBBComponent        = getEBBComponentModel        err
      , getBlockOrEBBComponent = getBlockOrEBBComponentModel err
      , appendBlock            = appendBlockModel            err
      , appendEBB              = appendEBBModel              err
      , stream                 = streamModel                 err
      , immutableDBErr         = err
      }
    internal = Internal
      { deleteAfter = deleteAfterModel
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
    go _ []        = error ("lookupBySlot: index out of bounds: " <> show i)

-- | Rolls back the chain so that the given 'Tip' is the new tip.
--
-- The user is responsible for giving a valid 'Tip', i.e. a tip that points to
-- a filled slot or an existing EBB (Genesis is always valid). This function
-- will not truncate to the last filled slot or EBB itself.
rollBackToTip :: forall hash. ImmTip -> DBModel hash -> DBModel hash
rollBackToTip tip dbm@DBModel {..} = case tip of
    TipGen    -> (initDBModel firstEpochSize)
        { dbmNextIterator = dbmNextIterator }
      where
        firstEpochSize = lookupEpochSize dbm 0

    Tip (EBB epoch) -> dbm
        { dbmChain = rolledBackChain
        , dbmEBBs  = ebbsUpToEpoch epoch
        , dbmTip   = addHash tip
        }
      where
        firstSlotAfter  = epochSlotToSlot dbm (EpochSlot epoch 1)
        rolledBackChain = dbmChain
                        & zip [0..]
                        & takeWhile ((< firstSlotAfter) . fst)
                        & map snd

    Tip (Block slot)
      | slot >= fromIntegral (length dbmChain) -> dbm
      | otherwise                              -> dbm
        { dbmChain = rolledBackChain
        , dbmEBBs  = ebbsUpToEpoch epoch
        , dbmTip   = addHash tip
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

    addHash :: ImmTip -> ImmTipWithHash hash
    addHash = fmap $ \case
      EBB   epoch ->
        WithHash (fst $ dbmEBBs Map.! epoch) (EBB epoch)
      Block slot  ->
        WithHash
          (fst $ fromJust $ dbmChain !! fromIntegral (unSlotNo slot))
          (Block slot)

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
      Tip (EBB epoch)   -> epochSlot < EpochSlot epoch 0
      Tip (Block slot') -> slot      < slot'
    toTip :: (EpochSlot, SlotNo) -> ImmTip
    toTip (EpochSlot epoch 0, _)    = Tip (EBB epoch)
    toTip (_                , slot) = Tip (Block slot)

-- | Return the blobs in the given 'EpochNo', in order.
blobsInEpoch :: DBModel hash -> EpochNo -> [ByteString]
blobsInEpoch dbm@DBModel {..} epoch =
    maybe id (:) mbEBBBlob       $
    map (binaryBlob . snd)       $
    mapMaybe snd                 $
    takeWhile ((== epoch) . fst) $
    dropWhile ((/= epoch) . fst) $
    zip (map (slotToEpoch dbm . SlotNo) [0..]) dbmChain
  where
    mbEBBBlob = binaryBlob . snd <$> Map.lookup epoch dbmEBBs

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

getTipModel :: MonadState (DBModel hash) m => m (ImmTipWithHash hash)
getTipModel = gets dbmTip

deleteAfterModel :: (MonadState (DBModel hash) m) => ImmTip -> m ()
deleteAfterModel tip =
    -- First roll back to the given tip (which is not guaranteed to be
    -- valid/exist!), then roll back to the last valid remaining tip.
    modify $ rollBackToLastValidTip . rollBackToTip tip
  where
    rollBackToLastValidTip dbm = rollBackToTip (NE.last (tips dbm)) dbm

extractHeader :: BinaryInfo ByteString -> ByteString
extractHeader BinaryInfo { binaryBlob, headerOffset, headerSize } =
    Lazy.take (fromIntegral headerSize) $
    Lazy.drop (fromIntegral headerOffset) binaryBlob

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
  :: (HasCallStack, MonadState (DBModel hash) m)
  => ErrorHandling ImmutableDBError m
  -> BlockComponent (ImmutableDB hash m) b
  -> SlotNo
  -> m (Maybe b)
getBlockComponentModel err blockComponent slot = do
    DBModel { dbmTip, dbmChain } <- get

    -- Check that the slot is not in the future
    let inTheFuture = case forgetHash <$> dbmTip of
          TipGen               -> True
          Tip (Block lastSlot) -> slot > lastSlot
          Tip (EBB  _ebb)      -> slot >= fromIntegral (length dbmChain)

    when inTheFuture $
      throwUserError err $ ReadFutureSlotError slot (forgetHash <$> dbmTip)

    return $ case lookupBySlot slot dbmChain of
      Nothing                 -> Nothing
      Just (hash, binaryInfo) -> Just $
        extractBlockComponent hash slot Nothing binaryInfo blockComponent

getEBBComponentModel
  :: (HasCallStack, MonadState (DBModel hash) m)
  => ErrorHandling ImmutableDBError m
  -> BlockComponent (ImmutableDB hash m) b
  -> EpochNo
  -> m (Maybe b)
getEBBComponentModel err blockComponent epoch = do
    dbm@DBModel {..} <- get
    let currentEpoch = dbmCurrentEpoch dbm
        inTheFuture  = epoch > currentEpoch ||
          case dbmTip of
            TipGen -> True
            Tip _  -> False

    when inTheFuture $
      throwUserError err $ ReadFutureEBBError epoch currentEpoch

    return $ case Map.lookup epoch dbmEBBs of
      Nothing                 -> Nothing
      Just (hash, binaryInfo) -> Just $
          extractBlockComponent hash slot (Just epoch) binaryInfo blockComponent
        where
          slot = epochSlotToSlot dbm (EpochSlot epoch 0)

getBlockOrEBBComponentModel
  :: (HasCallStack, MonadState (DBModel hash) m, Eq hash)
  => ErrorHandling ImmutableDBError m
  -> BlockComponent (ImmutableDB hash m) b
  -> SlotNo
  -> hash
  -> m (Maybe b)
getBlockOrEBBComponentModel err blockComponent = \slot hash -> do
    dbm@DBModel { dbmTip, dbmChain, dbmEBBs } <- get

    -- Check that the slot is not in the future
    let inTheFuture = case forgetHash <$> dbmTip of
          TipGen               -> True
          Tip (Block lastSlot) -> slot > lastSlot
          Tip (EBB   epoch)    -> slot > epochSlotToSlot dbm (EpochSlot epoch 0)

    when inTheFuture $
      throwUserError err $ ReadFutureSlotError slot (forgetHash <$> dbmTip)

    let (epoch, couldBeEBB) = case slotToEpochSlot dbm slot of
          EpochSlot e 1 -> (e, True)
          EpochSlot e _ -> (e, False)

    -- The chain can be too short if there's an EBB at the tip
    return $ case lookupBySlotMaybe slot dbmChain of
      Just (hash', binaryInfo)
        | hash' == hash
        -> Just $ extractBlockComponent hash slot Nothing binaryInfo blockComponent
      -- Fall back to EBB
      _ | couldBeEBB
        , Just (hash', binaryInfo) <- Map.lookup epoch dbmEBBs
        , hash' == hash
        -> Just $ extractBlockComponent hash slot (Just epoch) binaryInfo blockComponent
        | otherwise
        -> Nothing
  where
    -- Return 'Nothing' when the chain is too short. In contrast to
    -- 'lookupBySlot', which would throw an error.
    lookupBySlotMaybe (SlotNo i') dbmChain
      | let i = fromIntegral i'
      , i < length dbmChain
      = dbmChain !! i
      | otherwise
      = Nothing

appendBlockModel :: (HasCallStack, MonadState (DBModel hash) m)
                 => ErrorHandling ImmutableDBError m
                 -> SlotNo
                 -> hash
                 -> BinaryInfo Builder
                 -> m ()
appendBlockModel err slot hash binaryInfo = do
    dbm@DBModel {..} <- get

    -- Check that we're not appending to the past
    let inThePast = case forgetHash <$> dbmTip of
          Tip (Block lastSlot) -> slot <= lastSlot
          Tip (EBB _)          -> slot < fromIntegral (length dbmChain)
          TipGen               -> False

    when inThePast $
      throwUserError err $ AppendToSlotInThePastError slot (forgetHash <$> dbmTip)

    let binaryInfo' = toLazyByteString <$> binaryInfo
        toPad       = fromIntegral (unSlotNo slot) - length dbmChain

    -- TODO snoc list?
    put dbm
      { dbmChain = dbmChain ++ replicate toPad Nothing ++ [Just (hash, binaryInfo')]
      , dbmTip   = Tip (WithHash hash (Block slot))
      }

appendEBBModel :: (MonadState (DBModel hash) m)
               => ErrorHandling ImmutableDBError m
               -> EpochNo
               -> hash
               -> BinaryInfo Builder
               -> m ()
appendEBBModel err epoch hash binaryInfo = do
    dbm@DBModel {..} <- get

    -- Check that we're not appending to the past
    let currentEpoch = dbmCurrentEpoch dbm
        inThePast    = epoch <= currentEpoch && case dbmTip of
          TipGen -> False
          Tip _  -> True

    when inThePast $
      throwUserError err $ AppendToEBBInThePastError epoch currentEpoch

    let binaryInfo'  = toLazyByteString <$> binaryInfo
        ebbEpochSlot = EpochSlot epoch 0
        ebbSlot      = epochSlotToSlot dbm ebbEpochSlot
        toPad        = fromIntegral (unSlotNo ebbSlot) - length dbmChain

    put dbm
      { dbmChain = dbmChain ++ replicate toPad Nothing
      , dbmTip   = Tip (WithHash hash (EBB epoch))
      , dbmEBBs  = Map.insert epoch (hash, binaryInfo') dbmEBBs
      }

streamModel
  :: forall m hash b. (MonadState (DBModel hash) m, Eq hash)
  => ErrorHandling ImmutableDBError m
  -> BlockComponent (ImmutableDB hash m) b
  -> Maybe (SlotNo, hash)
  -> Maybe (SlotNo, hash)
  -> m (Either (WrongBoundError hash)
               (Iterator hash m b))
streamModel err blockComponent mbStart mbEnd = do
    dbm@DBModel {..} <- get
    validateIteratorRange err (generalizeEpochInfo dbmEpochInfo)
      (forgetHash <$> dbmTip) mbStart mbEnd
    go dbm >>= \case
      Left  e       -> return $ Left e
      Right results -> Right <$> do
        let itm  = IteratorModel results
            itID = BaseIteratorID dbmNextIterator
        put dbm
          { dbmNextIterator = succ dbmNextIterator
          , dbmIterators    = Map.insert itID itm dbmIterators
          }
        return Iterator
          { iteratorNext    = iteratorNextModel    itID blockComponent
          , iteratorPeek    = iteratorPeekModel    itID blockComponent
          , iteratorHasNext = iteratorHasNextModel itID
          , iteratorClose   = iteratorCloseModel   itID
          , iteratorID      = itID
          }
  where
    go
      :: DBModel hash
      -> m (Either (WrongBoundError hash) [IterRes hash])
    go dbm = runExceptT $ do
        -- The real implementation checks the end bound first, so we do the
        -- same to get the same errors
        mbEnd'   <- liftEither $ mapM checkBound mbEnd
        mbStart' <- liftEither $ mapM checkBound mbStart

        -- 'validateIteratorRange', which doesn't know about hashes, can't
        -- detect that streaming from the regular block to the EBB in the same
        -- slot is invalid, as the EBB comes before the regular block. Here,
        -- we do know about the hashes and 'EpochSlot's.
        case (mbStart', mbEnd') of
          (Just start, Just end)
            | start > end
            -> lift $ throwUserError err $ InvalidIteratorRangeError
                 (epochSlotToSlot dbm start) (epochSlotToSlot dbm start)
          _ -> return ()

        return $ iteratorResults mbStart' mbEnd'
      where
        blobs = dbmBlobs dbm

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

iteratorNextModel :: MonadState (DBModel hash) m
                  => IteratorID
                  -> BlockComponent (ImmutableDB hash m) b
                  -> m (IteratorResult b)
iteratorNextModel itID blockComponent = do
    dbm@DBModel {..} <- get
    case Map.lookup itID dbmIterators of
      Nothing                         -> return IteratorExhausted
      Just (IteratorModel [])         -> do
        iteratorCloseModel itID
        return IteratorExhausted
      Just (IteratorModel ((epochOrSlot, hash, bi):ress)) -> do
          put dbm
            { dbmIterators = Map.insert itID (IteratorModel ress) dbmIterators
            }
          return $ IteratorResult $
            extractBlockComponent hash slot mbEpochNo bi blockComponent
        where
          (slot, mbEpochNo) = case epochOrSlot of
            Left epoch  -> (epochSlotToSlot dbm (EpochSlot epoch 0), Just epoch)
            Right slot' -> (slot', Nothing)

iteratorPeekModel :: MonadState (DBModel hash) m
                  => IteratorID
                  -> BlockComponent (ImmutableDB hash m) b
                  -> m (IteratorResult b)
iteratorPeekModel itID blockComponent = do
    dbm@DBModel {..} <- get
    case Map.lookup itID dbmIterators of
      Nothing                      -> return IteratorExhausted
      Just (IteratorModel [])      -> return IteratorExhausted
      Just (IteratorModel ((epochOrSlot, hash, bi):_)) -> return $
          IteratorResult $
            extractBlockComponent hash slot mbEpochNo bi blockComponent
        where
          (slot, mbEpochNo) = case epochOrSlot of
            Left epoch  -> (epochSlotToSlot dbm (EpochSlot epoch 0), Just epoch)
            Right slot' -> (slot', Nothing)

iteratorHasNextModel :: MonadState (DBModel hash) m
                     => IteratorID
                     -> m (Maybe (Either EpochNo SlotNo, hash))
iteratorHasNextModel itID = do
    DBModel {..} <- get
    case Map.lookup itID dbmIterators of
      Nothing                                         -> return Nothing
      Just (IteratorModel [])                         -> return Nothing
      Just (IteratorModel ((epochOrSlot, hash, _):_)) -> return $ Just (epochOrSlot, hash)

iteratorCloseModel :: MonadState (DBModel hash) m
                   => IteratorID -> m ()
iteratorCloseModel itID = modify $ \dbm@DBModel {..} ->
    dbm { dbmIterators = Map.delete itID dbmIterators }
