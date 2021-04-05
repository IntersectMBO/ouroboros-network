{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Model for the 'ImmutableDB'.
module Test.Ouroboros.Storage.ImmutableDB.Model (
    DBModel (..)
  , InSlot (..)
  , IteratorId
  , IteratorModel
  , closeAllIterators
  , dbmBlocks
  , dbmCurrentChunk
  , dbmTip
  , dbmTipBlock
  , initDBModel
  , simulateCorruptions
  , tips
    -- * ImmutableDB implementation
  , appendBlockModel
  , deleteAfterModel
  , getBlockComponentModel
  , getTipModel
  , iteratorCloseModel
  , iteratorHasNextModel
  , iteratorNextModel
  , reopenModel
  , streamAllModel
  , streamModel
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (unless, when)
import           Control.Monad.Except (MonadError, throwError)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lazy
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Word (Word64)

import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util (lastMaybe, takeUntil)
import           Ouroboros.Consensus.Util.CallStack

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath, fsPathSplit)
import           Ouroboros.Consensus.Storage.ImmutableDB.API hiding
                     (throwApiMisuse)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util (parseDBFile)
import           Ouroboros.Consensus.Storage.Serialisation

import           Test.Ouroboros.Storage.TestBlock hiding (EBB)

data InSlot blk =
    -- | This slot contains only a regular block
    InSlotBlock blk

    -- | This slot contains only an EBB
  | InSlotEBB blk

    -- | This slot contains an EBB /and/ a regular block
    --
    -- NOTE: EBBs shares
    --
    -- o a block number with their predecessor
    -- o a slot number with their successor
    --
    -- So within the same /slot/, the EBB comes /first/.
  | InSlotBoth blk blk
  deriving (Show, Generic)

data DBModel blk = DBModel {
      dbmChunkInfo    :: ChunkInfo
    , dbmCodecConfig  :: CodecConfig blk
    , dbmIterators    :: Map IteratorId (IteratorModel blk)
    , dbmNextIterator :: IteratorId
    , dbmSlots        :: Map SlotNo (InSlot blk)
    }
  deriving (Generic)

deriving instance (Show (CodecConfig blk), StandardHash blk, Show blk) => Show (DBModel blk)

initDBModel :: ChunkInfo -> CodecConfig blk -> DBModel blk
initDBModel chunkInfo codecConfig = DBModel {
      dbmChunkInfo    = chunkInfo
    , dbmCodecConfig  = codecConfig
    , dbmIterators    = Map.empty
    , dbmNextIterator = 0
    , dbmSlots        = Map.empty
    }

insertInSlot ::
     forall blk. (HasHeader blk, GetHeader blk, HasCallStack)
  => blk
  -> Map SlotNo (InSlot blk)
  -> Map SlotNo (InSlot blk)
insertInSlot blk = Map.alter (Just . ins (blockToIsEBB blk)) (blockSlot blk)
  where
    ins :: IsEBB -> Maybe (InSlot blk) -> InSlot blk
    ins IsNotEBB Nothing                  = InSlotBlock    blk
    ins IsEBB    Nothing                  = InSlotEBB      blk
    ins IsNotEBB (Just (InSlotEBB   ebb)) = InSlotBoth ebb blk
    ins IsEBB    (Just (InSlotBlock _  )) = error "insertInSlot: EBB after block"
    ins _ _                               = error "insertInSlot: slot already filled"

{-------------------------------------------------------------------------------
  Derived values
-------------------------------------------------------------------------------}

dbmTipBlock :: DBModel blk -> WithOrigin blk
dbmTipBlock DBModel { dbmSlots } =
    case Map.lookupMax dbmSlots of
      Nothing              -> Origin
      Just (_slot, inSlot) -> NotOrigin $
        case inSlot of
          InSlotBlock     blk -> blk
          InSlotEBB       blk -> blk
          InSlotBoth _ebb blk -> blk

dbmTip :: (HasHeader blk, GetHeader blk) => DBModel blk -> WithOrigin (Tip blk)
dbmTip = fmap blockToTip . dbmTipBlock

-- | Return a list of blocks in the same order as they appear on the \"virtual\"
-- chain in the ImmutableDB.
dbmBlocks :: DBModel blk -> [blk]
dbmBlocks = concatMap inSlotToBlks . Map.elems . dbmSlots
  where
    inSlotToBlks :: InSlot blk -> [blk]
    inSlotToBlks = \case
        InSlotBlock     blk -> [blk]
        InSlotEBB   ebb     -> [ebb]
        InSlotBoth  ebb blk -> [ebb, blk]

dbmCurrentChunk :: HasHeader blk => DBModel blk -> ChunkNo
dbmCurrentChunk dbm@DBModel { dbmChunkInfo } =
    case blockSlot <$> dbmTipBlock dbm of
      Origin         -> firstChunkNo
      NotOrigin slot -> chunkIndexOfSlot dbmChunkInfo slot

type IteratorId = Int

-- | Model for an 'Iterator'.
--
-- An iterator is open iff its is present in 'dbmIterators'.
--
-- The model of an iterator is just the list of blocks it streams over.
-- Advancing the iterator will yield the first one and should drop it from the
-- model.
newtype IteratorModel blk = IteratorModel [blk]
  deriving (Show, Eq, Generic)

{------------------------------------------------------------------------------
  Helpers
------------------------------------------------------------------------------}

throwApiMisuse ::
     (MonadError (ImmutableDBError blk) m, HasCallStack)
  => ApiMisuse blk -> m a
throwApiMisuse e = throwError $ ApiMisuse e prettyCallStack

computeBlockSize :: EncodeDisk blk blk => CodecConfig blk -> blk -> Word64
computeBlockSize ccfg =
      fromIntegral
    . Lazy.length
    . CBOR.toLazyByteString
    . encodeDisk ccfg

lookupBlock ::
     (HasHeader blk, GetHeader blk)
  => RealPoint blk
  -> DBModel blk
  -> Either (MissingBlock blk) blk
lookupBlock pt@(RealPoint slot hash) dbm@DBModel { dbmSlots } =
    case Map.lookup slot dbmSlots of
      Just (InSlotBlock blk)
        | blockHash blk == hash
        -> return blk
        | otherwise
        -> throwError $ WrongHash pt (NE.fromList [blockHash blk])
      Just (InSlotEBB ebb)
        | blockHash ebb == hash
        -> return ebb
        | otherwise
        -> throwError $ WrongHash pt (NE.fromList [blockHash ebb])
      Just (InSlotBoth ebb blk)
        | blockHash ebb == hash
        -> return ebb
        | blockHash blk == hash
        -> return blk
        | otherwise
        -> throwError $ WrongHash pt (NE.fromList [blockHash ebb, blockHash blk])
      Nothing
        | NotOrigin slot > (tipSlotNo <$> dbmTip dbm)
        -> throwError $ NewerThanTip pt (tipToPoint (dbmTip dbm))
        | otherwise
        -> throwError $ EmptySlot pt

-- | Rolls back the chain so that the given 'Tip' is the new tip.
--
-- PRECONDITION: the given tip must correspond to a block in the model
rollBackToTip ::
     forall blk. (HasHeader blk, GetHeader blk)
  => WithOrigin (Tip blk) -> DBModel blk -> DBModel blk
rollBackToTip tip dbm@DBModel { dbmSlots } =
    dbm { dbmSlots = Map.mapMaybe shouldKeep dbmSlots }
  where
    -- Note: we're not checking hashes, we rely on the precondition
    blockNewerThanTip :: blk -> Bool
    blockNewerThanTip blk =
        NotOrigin (CompareTip (blockToTip blk)) > (CompareTip <$> tip)

    shouldKeep :: InSlot blk -> Maybe (InSlot blk)
    shouldKeep = \case
        InSlotEBB ebb
          | blockNewerThanTip ebb
          -> Nothing
          | otherwise
          -> Just $ InSlotEBB ebb
        InSlotBlock blk
          | blockNewerThanTip blk
          -> Nothing
          | otherwise
          -> Just $ InSlotBlock blk
        InSlotBoth ebb blk
          | blockNewerThanTip ebb, blockNewerThanTip blk
          -> Nothing
          | blockNewerThanTip blk
          -> Just $ InSlotEBB ebb
          | otherwise
          -> Just $ InSlotBoth ebb blk

-- | Return the blocks before, in, and after the given 'ChunkNo'.
blocksBeforeInAfterChunk ::
     forall blk. HasHeader blk
  => ChunkNo
  -> DBModel blk
  -> ([blk], [blk], [blk])
blocksBeforeInAfterChunk chunk dbm@DBModel { dbmChunkInfo } =
    (map fst lt, map fst eq, map fst gt)
  where
    blocksWithChunks :: [(blk, ChunkNo)]
    blocksWithChunks =
        [ (blk, chunkIndexOfSlot dbmChunkInfo (blockSlot blk))
        | blk <- dbmBlocks dbm
        ]

    (lt, geq) = span ((< chunk)             . snd) blocksWithChunks
    (eq, gt)  = span ((< nextChunkNo chunk) . snd) geq

-- | Return the blocks in the given 'ChunkNo', in order.
blocksInChunk :: HasHeader blk => ChunkNo -> DBModel blk -> [blk]
blocksInChunk chunk dbm = eq
  where
    (_lt, eq, _gt) = blocksBeforeInAfterChunk chunk dbm

properTips :: (HasHeader blk, GetHeader blk) => DBModel blk -> [Tip blk]
properTips = map blockToTip . concatMap go . Map.elems . dbmSlots
  where
    go :: InSlot blk -> [blk]
    go (InSlotBlock blk)     = [blk]
    go (InSlotEBB   ebb)     = [ebb]
    go (InSlotBoth  ebb blk) = [ebb, blk]

-- | List all 'Tip's that point to a filled slot or an existing EBB in the
-- model, including 'Origin'. The tips will be sorted from old to recent.
tips ::
     (HasHeader blk, GetHeader blk)
  => DBModel blk
  -> NonEmpty (WithOrigin (Tip blk))
tips dbm = Origin NE.:| map NotOrigin (properTips dbm)

closeAllIterators :: DBModel blk -> DBModel blk
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
simulateCorruptions ::
     (HasHeader blk, GetHeader blk, EncodeDisk blk blk)
  => Corruptions
  -> DBModel blk
  -> (WithOrigin (Tip blk), DBModel blk)
simulateCorruptions corrs dbm = (dbmTip dbm', dbm')
  where
    dbm' = closeAllIterators $ rollBack rbp dbm
    -- Take the minimal 'RollBackPoint', which is the earliest.
    rbp = minimum $
      fmap (\(c, f) -> findCorruptionRollBackPoint c f dbm) corrs

data RollBackPoint blk
  = DontRollBack
    -- ^ No roll back needed.
  | RollBackToTip (WithOrigin (Tip blk))
    -- ^ Roll back to the tip, keeping it as the last block. When 'Origin',
    -- truncate all blocks.
  deriving (Eq, Show, Generic)

-- | The earlier 'RollBackPoint' < the later 'RollBackPoint'.
instance StandardHash blk => Ord (RollBackPoint blk) where
  compare r1 r2 = case (r1, r2) of
    (DontRollBack, DontRollBack)         -> EQ
    (_,            DontRollBack)         -> LT
    (DontRollBack, _)                    -> GT
    (RollBackToTip t1, RollBackToTip t2) ->
      compare (CompareTip <$> t1) (CompareTip <$> t2)

rollBack ::
    (HasHeader blk, GetHeader blk)
  => RollBackPoint blk -> DBModel blk -> DBModel blk
rollBack rbp dbm = case rbp of
    DontRollBack      -> dbm
    RollBackToTip tip -> rollBackToTip tip dbm

findCorruptionRollBackPoint ::
     (HasHeader blk, GetHeader blk, EncodeDisk blk blk)
  => FileCorruption
  -> FsPath
  -> DBModel blk
  -> RollBackPoint blk
findCorruptionRollBackPoint corr file dbm =
    case (Text.unpack . snd <$> fsPathSplit file) >>= parseDBFile of
      Just ("chunk",      chunk) -> findCorruptionRollBackForChunk corr chunk dbm
      -- Index files are always recoverable
      Just ("primary",   _chunk) -> DontRollBack
      Just ("secondary", _chunk) -> DontRollBack
      _                          -> error "Invalid file to corrupt"

findCorruptionRollBackForChunk ::
     (HasHeader blk, GetHeader blk, EncodeDisk blk blk)
  => FileCorruption
  -> ChunkNo
  -> DBModel blk
  -> RollBackPoint blk
findCorruptionRollBackForChunk corr chunk dbm = case corr of
    DeleteFile      -> rollbackToLastFilledSlotBefore    chunk dbm

    DropLastBytes n -> findRollBackPointForOffsetInChunk validBytes chunk dbm
      where
        validBytes | n >= totalBytes = 0
                   | otherwise       = totalBytes - n

    Corrupt n       -> findRollBackPointForOffsetInChunk validBytes chunk dbm
      where
        validBytes = n `mod` totalBytes
  where
    totalBytes :: Word64
    totalBytes =
          sum
        . map (computeBlockSize (dbmCodecConfig dbm))
        . blocksInChunk chunk
        $ dbm

findRollBackPointForOffsetInChunk ::
     forall blk. (HasHeader blk, GetHeader blk, EncodeDisk blk blk)
  => Word64  -- ^ The number of valid bytes in the chunk, the corruption happens
             -- at the first byte after it.
  -> ChunkNo -> DBModel blk -> RollBackPoint blk
findRollBackPointForOffsetInChunk validBytes chunk dbm@DBModel { dbmCodecConfig }
    | null blocksInThisChunk
      -- If the file is empty, no corruption happened, and we don't have to
      -- roll back
    = DontRollBack
    | Just lastValidTip <- mbLastValidTip
    = RollBackToTip (NotOrigin lastValidTip)
    | otherwise
      -- When there are no more filled slots in the epoch file, roll back to
      -- the last filled slot before the epoch.
    = rollbackToLastFilledSlotBefore chunk dbm
  where
    blocksInThisChunk :: [blk]
    blocksInThisChunk = blocksInChunk chunk dbm

    mbLastValidTip :: Maybe (Tip blk)
    mbLastValidTip = go 0 Nothing blocksInThisChunk
      where
        go :: Word64 -> Maybe (Tip blk) -> [blk] -> Maybe (Tip blk)
        go curOffset lastValid = \case
          [] -> lastValid
          blk:blks
              | let blockSize = computeBlockSize dbmCodecConfig blk
              , curOffset + blockSize <= validBytes
              -> go (curOffset + blockSize) (Just (blockToTip blk)) blks
              | otherwise
              -> lastValid

rollbackToLastFilledSlotBefore ::
     (HasHeader blk, GetHeader blk)
  => ChunkNo -> DBModel blk -> RollBackPoint blk
rollbackToLastFilledSlotBefore chunk dbm = case lastMaybe beforeChunk of
    Nothing              -> RollBackToTip Origin
    Just lastBlockBefore ->
      RollBackToTip (NotOrigin (blockToTip lastBlockBefore))
  where
    (beforeChunk, _, _) = blocksBeforeInAfterChunk chunk dbm

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

getTipModel ::
     (HasHeader blk, GetHeader blk)
  => DBModel blk -> WithOrigin (Tip blk)
getTipModel = dbmTip

-- | Close all open iterators and return the current tip
reopenModel ::
     (HasHeader blk, GetHeader blk)
  => DBModel blk -> (WithOrigin (Tip blk), DBModel blk)
reopenModel dbm = (dbmTip dbm, closeAllIterators dbm)

deleteAfterModel ::
     (HasHeader blk, GetHeader blk)
  => WithOrigin (Tip blk) -> DBModel blk -> DBModel blk
deleteAfterModel tip = rollBackToTip tip . closeAllIterators

extractBlockComponent ::
     forall blk b.
     ( HasHeader blk
     , GetHeader blk
     , EncodeDisk blk blk
     , HasNestedContent Header blk
     , EncodeDiskDep (NestedCtxt Header) blk
     )
  => CodecConfig blk
  -> blk
  -> BlockComponent blk b
  -> b
extractBlockComponent ccfg blk = \case
    GetVerifiedBlock -> blk  -- We don't verify
    GetBlock         -> blk
    GetRawBlock      -> rawBlk
    GetHeader        -> getHeader blk
    GetRawHeader     -> rawHdr
    GetHash          -> blockHash blk
    GetSlot          -> blockSlot blk
    GetIsEBB         -> blockToIsEBB blk
    GetBlockSize     -> fromIntegral $ Lazy.length rawBlk
    GetHeaderSize    -> fromIntegral $ Lazy.length rawHdr
    GetNestedCtxt    -> nestedCtxt
    GetPure a        -> a
    GetApply f bc    ->
      extractBlockComponent ccfg blk f $
      extractBlockComponent ccfg blk bc
  where
    rawBlk :: Lazy.ByteString
    rawBlk = CBOR.toLazyByteString $ encodeDisk ccfg blk

    rawHdr :: Lazy.ByteString
    nestedCtxt :: SomeSecond (NestedCtxt Header) blk
    (nestedCtxt, rawHdr) = case unnest (getHeader blk) of
        DepPair ctxt h ->
          ( SomeSecond ctxt
          , CBOR.toLazyByteString $ encodeDiskDep ccfg ctxt h
          )

getBlockComponentModel ::
     ( HasHeader blk
     , GetHeader blk
     , EncodeDisk blk blk
     , HasNestedContent Header blk
     , EncodeDiskDep (NestedCtxt Header) blk
     )
  => BlockComponent blk b
  -> RealPoint blk
  -> DBModel blk
  -> Either (MissingBlock blk) b
getBlockComponentModel blockComponent pt dbm =
    flip (extractBlockComponent ccfg) blockComponent
      <$> lookupBlock pt dbm
  where
    DBModel { dbmCodecConfig = ccfg } = dbm

appendBlockModel ::
     forall blk. (HasHeader blk, GetHeader blk, HasCallStack)
  => blk
  -> DBModel blk
  -> Either (ImmutableDBError blk) (DBModel blk)
appendBlockModel blk dbm@DBModel { dbmSlots } = do
    -- Check that we're not appending to the past
    let inThePast =
          NotOrigin (CompareTip blockTip) <= (CompareTip <$> dbmTip dbm)

    when inThePast $
      throwApiMisuse $
        AppendBlockNotNewerThanTipError
          (blockRealPoint blk)
          (tipToPoint (dbmTip dbm))

    return dbm { dbmSlots = insertInSlot blk dbmSlots }
  where
    blockTip = blockToTip blk

streamModel ::
     forall blk. (HasHeader blk, GetHeader blk, HasCallStack)
  => StreamFrom blk
  -> StreamTo   blk
  -> DBModel blk
  -> Either (ImmutableDBError blk)
            (Either (MissingBlock blk)
                    (IteratorId, DBModel blk))
streamModel from to dbm = swizzle $ do
    unless (validBounds from to) $
      liftLeft $ throwApiMisuse $ InvalidIteratorRangeError from to

    -- The real implementation checks the end bound first, so we do the
    -- same to get the same errors
    toTip <- liftRight $ blockToTip <$> case to of
      StreamToInclusive pt -> lookupBlock pt dbm

    fromTip <- liftRight $ fmap blockToTip <$> case from of
      StreamFromInclusive pt -> NotOrigin <$> lookupBlock pt dbm
      StreamFromExclusive pt -> case pointToWithOriginRealPoint pt of
        Origin        -> return Origin
        NotOrigin pt' -> NotOrigin <$> lookupBlock pt' dbm

    --  validBounds can't check based on the points that we're not trying to
    --  stream from the regular block to the EBB in the same slot, so do that
    --  now, like in the real implementation.
    when ((CompareTip <$> fromTip) > NotOrigin (CompareTip toTip)) $
      liftLeft $ throwApiMisuse $ InvalidIteratorRangeError from to

    let blks =
            applyUpperBound to
          . applyLowerBound from
          . dbmBlocks
          $ dbm
        itm  = IteratorModel blks
        itId = dbmNextIterator
        dbm' = dbm {
            dbmNextIterator = succ dbmNextIterator
          , dbmIterators    = Map.insert itId itm dbmIterators
          }
    return (itId, dbm')
  where
    DBModel { dbmNextIterator, dbmIterators } = dbm

    liftLeft  = first Left
    liftRight = first Right

    swizzle :: Either (Either (ImmutableDBError blk) (MissingBlock blk)) a
            -> Either (ImmutableDBError blk) (Either (MissingBlock blk) a)
    swizzle (Left (Left e))  = Left e
    swizzle (Left (Right e)) = Right (Left e)
    swizzle (Right a)        = Right (Right a)

    applyLowerBound :: StreamFrom blk -> [blk] -> [blk]
    applyLowerBound = \case
        StreamFromExclusive pt -> case pointToWithOriginRealPoint pt of
          Origin        -> id
          NotOrigin pt' -> drop 1 . dropWhile ((/= pt') . blockRealPoint)
        StreamFromInclusive pt -> dropWhile ((/= pt) . blockRealPoint)

    applyUpperBound :: StreamTo blk -> [blk] -> [blk]
    applyUpperBound (StreamToInclusive pt) =
        takeUntil ((== pt) . blockRealPoint)

streamAllModel ::
     ( HasHeader blk
     , GetHeader blk
     , EncodeDisk blk blk
     , HasNestedContent Header blk
     , EncodeDiskDep (NestedCtxt Header) blk
     )
  => BlockComponent blk b
  -> DBModel blk
  -> [b]
streamAllModel blockComponent dbm@DBModel { dbmCodecConfig = ccfg } =
      map (flip (extractBlockComponent ccfg) blockComponent)
    . dbmBlocks
    $ dbm

iteratorNextModel ::
     ( HasHeader blk
     , GetHeader blk
     , EncodeDisk blk blk
     , HasNestedContent Header blk
     , EncodeDiskDep (NestedCtxt Header) blk
     )
  => IteratorId
  -> BlockComponent blk b
  -> DBModel blk
  -> (IteratorResult b, DBModel blk)
iteratorNextModel itId blockComponent dbm =
    case Map.lookup itId dbmIterators of
      Nothing ->
          (IteratorExhausted, dbm)

      Just (IteratorModel []) ->
          (IteratorExhausted, iteratorCloseModel itId dbm)

      Just (IteratorModel (blk:blks)) ->
          (IteratorResult (extractBlockComponent ccfg blk blockComponent), dbm')
        where
          dbm' = dbm {
              dbmIterators = Map.insert itId (IteratorModel blks) dbmIterators
            }
  where
    DBModel { dbmIterators, dbmCodecConfig = ccfg } = dbm

iteratorHasNextModel ::
     HasHeader blk
  => IteratorId
  -> DBModel blk
  -> Maybe (RealPoint blk)
iteratorHasNextModel itId DBModel { dbmIterators } =
    case Map.lookup itId dbmIterators of
      Nothing                      -> Nothing
      Just (IteratorModel [])      -> Nothing
      Just (IteratorModel (blk:_)) -> Just $ blockRealPoint blk

iteratorCloseModel :: IteratorId -> DBModel blk -> DBModel blk
iteratorCloseModel itId dbm@DBModel { dbmIterators } =
    dbm { dbmIterators = Map.delete itId dbmIterators }
