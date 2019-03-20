{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Byron.Proxy.DB where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR

import Control.Exception (Exception)
import Control.Lens ((^.))
import qualified Control.Lens as Lens (to)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Trans.Class (lift)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Conduit (ConduitT)
import qualified Data.Conduit as Conduit
import Data.Text (Text)
import Data.Word (Word64)

import qualified Pos.Binary as CSL (decode, serializeBuilder)
import qualified Pos.Chain.Block as CSL
import qualified Pos.Core.Slotting as CSL

import Ouroboros.Byron.Proxy.Index (Index, IndexWrite)
import qualified Ouroboros.Byron.Proxy.Index as Index
import Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr)
import Ouroboros.Storage.ImmutableDB.API (ImmutableDB, EpochNo (..), SlotNo (..))
import qualified Ouroboros.Storage.ImmutableDB.API as Immutable
import Ouroboros.Storage.ImmutableDB.Util (cborEpochFileParser')
import Ouroboros.Storage.FS.API (HasFS)

-- | Run a Decoder and fail if it does not consume all of the input bytes.
decodeFull
  :: (forall s . CBOR.Decoder s t)
  -> Lazy.ByteString
  -> Either CBOR.DeserialiseFailure t
decodeFull decoder lbs = case CBOR.deserialiseFromBytesWithSize decoder lbs of
  Left failure -> Left failure
  Right (bs, offset, t) ->
    if Lazy.null bs
    then Right t
    else Left (CBOR.DeserialiseFailure offset "decodeFull: did not consume all input")

-- | Parser for opening the `ImmutableDB`. It uses a CBOR decoder which decodes
-- a `Block` and returns its `Slot`, which is `epoch * epochSlots` in
-- case it's a genesis/EBB.
--
-- It does not check consistency of genesis/EBBs: that if they appear alongside
-- a main block, the main block is for slot 0 of the same epoch as the EBB
epochFileParser
  :: ( MonadST m, MonadThrow m )
  => CSL.SlotCount
  -> HasFS m h
  -> Immutable.EpochFileParser ReadIncrementalErr CSL.HeaderHash m (Word, Immutable.SlotNo)
epochFileParser epochSlots hasFS =
  fmap (\(w, block) -> (w, takeSlot block))
       (cborEpochFileParser' hasFS decoder hashOfEBB)
  where
  takeSlot = blockHeaderSlot epochSlots . CSL.getBlockHeader
  -- Must use the `Bi` typeclass to get a block decoder.
  decoder :: forall s . CBOR.Decoder s CSL.Block
  decoder = CSL.decode
  hashOfEBB :: CSL.Block -> Maybe CSL.HeaderHash
  hashOfEBB block = case block of
    -- Left is for EBBs.
    Left _  -> Just $ CSL.headerHash block
    Right _ -> Nothing

-- | The slot number relative to genesis of a block header.
-- EBBs get `epoch * slots_in_epoch`, so they share a slot with the first block
-- of that epoch. This includes the actual genesis block (at 0).
blockHeaderSlot :: CSL.SlotCount -> CSL.BlockHeader -> Slot
blockHeaderSlot epochSlots blkHeader = case blkHeader of
  CSL.BlockHeaderMain mBlkHeader -> mBlkHeader ^.
      CSL.gbhConsensus
    . CSL.mcdSlot
    -- FlatSlotId ~ Word64
    -- so we `fromIntegral` it (hopefully it fits!)
    . Lens.to (fromIntegral . CSL.flattenSlotId epochSlots)
  CSL.BlockHeaderGenesis gBlkHeader -> gBlkHeader ^.
      CSL.gbhConsensus
    . CSL.gcdEpoch
    . Lens.to CSL.getEpochIndex
    . Lens.to (ebbSlot . fromIntegral)
  where
  ebbSlot = \ei -> ei * fromIntegral epochSlots

-- | An iterator type quite like `Ouroboros.Storage.ImmutableDB.API.Iterator`,
-- but without the `IteratorID`, and with explicit iterator tails in the
-- `Next` constructors.
data IteratorResource m = IteratorResource
  { closeIterator :: m ()
  , iterator      :: Iterator m
  }

newtype Iterator m = Iterator
  { next :: m (Next m)
  }

data Next m where
  Done      :: Next m
  NextBlock :: Slot                   -> ByteString -> Iterator m -> Next m
  -- | For EBBs, the `Slot` is the same as the first block of that epoch.
  NextEBB   :: Slot -> CSL.HeaderHash -> ByteString -> Iterator m -> Next m

data DBRead where
  ReadEBB   :: Slot -> CSL.HeaderHash -> ByteString -> DBRead
  ReadBlock :: Slot ->                   ByteString -> DBRead

dbBytes :: DBRead -> ByteString
dbBytes term = case term of
  ReadEBB _ _ bytes -> bytes
  ReadBlock _ bytes -> bytes

data Tip where
  TipGenesis :: Tip
  TipEBB     :: Slot -> CSL.HeaderHash -> ByteString -> Tip
  TipBlock   :: Slot                   -> ByteString -> Tip

-- | Make an `Iterator` from an `ImmutableDB` `Iterator`.
fromImmutableDBIterator
  :: ( Monad m )
  => CSL.SlotCount
  -> Immutable.Iterator CSL.HeaderHash m
  -> Iterator m
fromImmutableDBIterator epochSlots idbIterator = Iterator $ do
  idbNext <- Immutable.iteratorNext idbIterator
  case idbNext of
    Immutable.IteratorExhausted                  -> pure Done
    Immutable.IteratorResult    slot       bytes -> pure $ NextBlock slot      bytes recurse
    Immutable.IteratorEBB       epoch hash bytes -> pure $ NextEBB   slot hash bytes recurse
      where
      slot = fromIntegral (epochSlots * fromIntegral epoch)
  where
  -- The `ImmutableDB` `Iterator` is 100% effectful; getting its "tail" is
  -- just the very same recursive call.
  recurse = fromImmutableDBIterator epochSlots idbIterator

-- | Make a conduit which yields every result of an iterator. Caller is
-- responsible for resource bracketing.
conduitFromIterator
  :: ( Monad m )
  => Iterator m
  -> ConduitT () DBRead m ()
conduitFromIterator iterator = do
  step <- lift $ next iterator
  case step of
    Done -> pure ()
    NextBlock slot bytes iterator' -> do
      Conduit.yield $ ReadBlock slot bytes
      conduitFromIterator iterator'
    NextEBB slot hash bytes iterator' -> do
      Conduit.yield $ ReadEBB slot hash bytes
      conduitFromIterator iterator'

data Point where
  FromGenesis :: Point
  FromHash    :: !CSL.HeaderHash -> Point
  FromPoint   :: !SlotNo -> !CSL.HeaderHash -> Point

-- | Unified database interface which supports lookup by hash or by slot.
-- It also gives `Slot` for EBBs, whereas the `ImmutableDB` would give `Epoch`.
data DB m = DB
  { -- | Append 0 or more blocks within the continuation.
    appendBlocks :: forall t . (DBAppend m -> m t) -> m t
    -- | Get an `ImmutableDB`-style `Iterator` from a given slot inclusive.
    --
    -- NB: caller is responsible for `close`ing the iterator, so `bracket` it
    -- or use `ResourceT` or similar. In any case, `readFrom` must be treated
    -- as a resource-acquisition thing, so when `m ~ IO` you must mask/restore
    -- appropriately.
    --
    -- If `Nothing` is given for the `Slot`, the index will be used to find the
    -- start point. Giving a `Just slot` saves this bit of work.
  , readFrom     :: Point -> m (IteratorResource m)
    -- | Gives enough information to retrieve what is the tip at the time of
    -- reading. Crucially, that includes whether it's an EBB or a main block,
    -- so the user can figure out how to decode the bytes given.
  , readTip      :: m Tip
  }

data DBAppend m = DBAppend
  { appendBlock :: CSL.Block -> m ()
  }

data DBError where
  IndexInconsistent :: Text -> DBError
  deriving (Show)

instance Exception DBError

-- | Blocks are written to the database in the cardano-sl `Block` `Bi` instance
-- encoding. In theory it would be better to encode `MainBlock` for non-EBB
-- and `GenesisBlock` for EBB, but then the epoch file parser would be more
-- difficult to define.
mkDB
  :: ( Monad m )
  => (forall x . DBError -> m x)
  -> CSL.SlotCount -- ^ Number of slots per epoch
                   -- (must never change in the chain stored here)
  -> Index m
  -> ImmutableDB CSL.HeaderHash m
  -> DB m
mkDB err epochSlots idx idb = DB
  { appendBlocks = appendBlocksImpl err epochSlots idx idb
  , readFrom     = readFromImpl err epochSlots idx idb
  , readTip      = readTipImpl err epochSlots idx idb
  }

appendBlocksImpl
  :: ( Monad m )
  => (forall x . DBError -> m x)
  -> CSL.SlotCount
  -> Index m
  -> ImmutableDB CSL.HeaderHash m
  -> (DBAppend m -> m t)
  -> m t
appendBlocksImpl err epochSlots idx idb k =
  Index.indexWrite idx $ \iwrite ->
    k (dbAppendImpl err epochSlots iwrite idb)

-- | Append one block, assumed to be within an `Index.indexWrite`.
-- It sets the block as the new tip in the index, then appends it to the
-- `ImmutableDB`, doing something slightly different depending on whether
-- it's an EBB or a main block.
dbAppendImpl
  :: ( Monad m )
  => (forall x . DBError -> m x)
  -> CSL.SlotCount
  -> IndexWrite m
  -> ImmutableDB CSL.HeaderHash m
  -> DBAppend m
dbAppendImpl err epochSlots iwrite idb = DBAppend $ \cslBlock ->
  -- Must serialise as a `Block` rather than a `MainBlock` or `GenesisBlock`,
  -- because the epoch file parser needs to be able to discriminate them.
  let builder = CSL.serializeBuilder cslBlock
  in  case cslBlock of 
        Left ebb -> do
          -- Write the index first, so that if something goes wrong with the
          -- `appendEBB` to the `ImmutableDB`, the transaction will quit and the
          -- index/db will remain consistent.
          let hash = CSL.headerHash ebb
              epoch = ebbEpoch ebb
          Index.updateTip iwrite hash epoch Index.EBBSlot
          Immutable.appendEBB idb epoch hash builder
        Right blk -> do
          let hash = CSL.headerHash blk
              (epoch, Slot wslot) = blockEpochAndRelativeSlot blk
              slot = Slot (fromIntegral epoch * fromIntegral epochSlots + wslot)
          Index.updateTip iwrite hash epoch (Index.RealSlot wslot)
          Immutable.appendBinaryBlob idb slot builder

-- | Stream from a given point, using the index to determine the start point
-- in case the `Point` is a header hash.
--
-- This is a resource-acquiring function and so must be masked/bracketed
-- appropriately.
readFromImpl
  :: forall m .
     ( Monad m )
  => (forall x . DBError -> m x)
  -> CSL.SlotCount
  -> Index m
  -> ImmutableDB CSL.HeaderHash m
  -> Point
  -> m (IteratorResource m)
readFromImpl err epochSlots idx idb point = case point of
  FromGenesis -> iteratorFromSlot Nothing
  -- Slot is given: just take an iterator from the `ImmutableDB`.
  FromPoint slot hash -> iteratorFromSlot (Just (slot, hash))
  -- No slot given: look up the slot using the index and then go from there.
  -- If it's not in the index, give an empty iterator.
  FromHash hash -> do
    idxItem <- Index.indexRead idx (Index.ByHash hash)
    case idxItem of
      -- If the hash is not in the database, it's not an error, we just give an
      -- empty stream.
      Nothing -> pure $ IteratorResource
        { closeIterator = pure ()
        , iterator      = Iterator (pure Done)
        }
      Just (_, epoch, indexSlot) ->
        let slot = indexToSlot epochSlots epoch indexSlot
        in  iteratorFromSlot (Just (slot, hash))
  where
  iteratorFromSlot :: Maybe (Slot, CSL.HeaderHash) -> m (IteratorResource m)
  iteratorFromSlot mStartPoint = do
    idbIterator <- Immutable.streamBinaryBlobs idb mStartPoint Nothing
    pure $ IteratorResource
      { closeIterator = Immutable.iteratorClose idbIterator
      , iterator      = fromImmutableDBIterator epochSlots idbIterator
      }

-- |
--
-- Ignores the index, goes straight to the `ImmutableDB`.
--
-- FIXME
-- The result given back may not be the tip, since there's no transactional
-- guarantee: we check for the tip epoch/slot, then read that point, but
-- there may have been write(s) in the interim.
--
-- TODO index doesn't need to be able to get tip anymore, so drop that part.
readTipImpl
  :: ( Monad m )
  => (forall x . DBError -> m x)
  -> CSL.SlotCount
  -> Index m
  -> ImmutableDB CSL.HeaderHash m
  -> m Tip
readTipImpl err epochSlots idx idb = do
  tip <- Immutable.getTip idb
  case tip of
    -- Empty DB. Hm...
    Immutable.TipGenesis   -> pure TipGenesis
    Immutable.TipEBB epoch -> do
      mItem <- Immutable.getEBB idb epoch
      case mItem of
        -- FIXME ImmutableDB bug
        Nothing -> error "ImmutableDB bug ebb"
        Just (hash, bytes) -> pure $ TipEBB slot hash bytes
          where
          slot = fromIntegral (epochSlots * fromIntegral epoch)
    Immutable.TipBlock slot -> do
      mItem <- Immutable.getBinaryBlob idb slot
      case mItem of
        -- FIXME ImmutableDB bug
        Nothing -> error "ImmutableDB bug main block"
        Just bytes -> pure $ TipBlock slot bytes

-- | The `Epoch` of an EBB.
ebbEpoch :: CSL.GenesisBlock -> EpochNo
ebbEpoch ebb = ebb ^.
    CSL.gbHeader
  . CSL.gbhConsensus
  . CSL.gcdEpoch
  . Lens.to (EpochNo . fromIntegral . CSL.getEpochIndex)

blockEpochAndRelativeSlot :: CSL.MainBlock -> (Epoch, SlotNo)
blockEpochAndRelativeSlot blk = blk ^.
    CSL.gbHeader
  . CSL.gbhConsensus
  . CSL.mcdSlot
  . Lens.to (\sl ->
      ( EpochNo (fromIntegral (CSL.getEpochIndex (CSL.siEpoch sl)))
      , SlotNo  (fromIntegral (CSL.getSlotIndex  (CSL.siSlot  sl)))
      )
    )

-- | True if the `EpochSlot` is for an EBB (or genesis).
isEbbSlot :: Index.IndexSlot -> Bool
isEbbSlot idxSlot = case idxSlot of
  Index.EBBSlot    -> True
  Index.RealSlot _ -> False

-- | Put the EBBs for epoch `n` at the same slot (relative to genesis) as the
-- first block of epoch `n`.
indexToSlot :: CSL.SlotCount -> EpochNo -> Index.IndexSlot -> SlotNo
indexToSlot epochSlots (EpochNo epoch) idxSlot = SlotNo sl
  where
  sl = (fromIntegral epochSlots * epoch) + relative
  relative :: Word64
  relative = case idxSlot of
    Index.EBBSlot    -> 0
    Index.RealSlot w -> w
