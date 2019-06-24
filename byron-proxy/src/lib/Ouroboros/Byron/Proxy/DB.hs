{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Byron.Proxy.DB where

import qualified Codec.CBOR.Decoding as CBOR

import Control.Exception (Exception)
import Control.Lens ((^.))
import qualified Control.Lens as Lens (to)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Trans.Class (lift)
import Control.Tracer (Tracer, traceWith)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder (byteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Conduit (ConduitT)
import qualified Data.Conduit as Conduit
import Data.Text (Text)
import Data.Word (Word64)

import Cardano.Binary (Annotated (..), ByteSpan, slice)
import Cardano.Crypto.Hashing (AbstractHash (..), unsafeAbstractHash)
import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano

import qualified Pos.Binary as CSL (serializeBuilder)
import qualified Pos.Chain.Block as CSL
import qualified Pos.Core.Slotting as CSL
import qualified Pos.Crypto.Hashing as Legacy (AbstractHash (..))

import Ouroboros.Byron.Proxy.Index.Types (Index, IndexWrite)
import qualified Ouroboros.Byron.Proxy.Index.Types as Index
import Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Storage.Common (EpochNo (..))
import qualified Ouroboros.Storage.Common as Immutable
import Ouroboros.Storage.ImmutableDB.API (ImmutableDB)
import qualified Ouroboros.Storage.ImmutableDB.API as Immutable
import Ouroboros.Storage.ImmutableDB.Util (cborEpochFileParser')
import Ouroboros.Storage.FS.API (HasFS)

-- TODO: Move these functions to a compatibility module
coerceHashToLegacy :: Cardano.HeaderHash -> CSL.HeaderHash
coerceHashToLegacy (AbstractHash digest) = Legacy.AbstractHash digest

coerceHashFromLegacy :: CSL.HeaderHash -> Cardano.HeaderHash
coerceHashFromLegacy (Legacy.AbstractHash digest) = AbstractHash digest

-- | Parser for opening the `ImmutableDB`. It uses a CBOR decoder which decodes
-- a `Block` and returns its `Slot`, which is `epoch * epochSlots` in
-- case it's a genesis/EBB.
--
-- It does not check consistency of genesis/EBBs: that if they appear alongside
-- a main block, the main block is for slot 0 of the same epoch as the EBB
epochFileParser
  :: ( MonadST m, MonadThrow m )
  => Cardano.EpochSlots
  -> HasFS m h
  -> Immutable.EpochFileParser ReadIncrementalErr Cardano.HeaderHash m (Word64, Immutable.SlotNo)
epochFileParser epochSlots hasFS =
  fmap (\(w, ablock) -> (w, takeSlot ablock))
       (cborEpochFileParser' hasFS decoder hashOfEBB)
  where
  takeSlot :: Cardano.ABlockOrBoundary a -> SlotNo
  takeSlot blk = case blk of
    Cardano.ABOBBlock    blk -> SlotNo $ Cardano.unSlotNumber (Cardano.blockSlot blk)
    Cardano.ABOBBoundary ebb -> SlotNo $ Cardano.boundaryEpoch ebb * Cardano.unEpochSlots epochSlots
  decoder :: forall s . CBOR.Decoder s (Cardano.ABlockOrBoundary ByteSpan)
  decoder = Cardano.fromCBORABlockOrBoundary epochSlots
  hashOfEBB :: Lazy.ByteString -> Cardano.ABlockOrBoundary ByteSpan -> Maybe Cardano.HeaderHash
  hashOfEBB bytes blk = case blk of
    Cardano.ABOBBlock    _   -> Nothing
    Cardano.ABOBBoundary ebb -> Just hash
      where
      -- TODO make a function for this
      -- TODO fix the toStrict/fromStrict marshalling?
      -- Cardano.wrapBoundaryBytes should work on lazy bytestrings: if you
      -- have a strict, getting a lazy is cheap, if you have a lazy,
      -- going to strict is not.
      hash = unsafeAbstractHash
        . Lazy.fromStrict
        . Cardano.wrapBoundaryBytes
        . Lazy.toStrict
        . Cardano.boundaryHeaderBytes
        $ ebbWithBytes
      ebbWithBytes = fmap (slice bytes) ebb

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
  NextBlock :: SlotNo                       -> Lazy.ByteString -> Iterator m -> Next m
  -- | For EBBs, the `Slot` is the same as the first block of that epoch.
  NextEBB   :: SlotNo -> Cardano.HeaderHash -> Lazy.ByteString -> Iterator m -> Next m

data DBRead where
  ReadEBB   :: SlotNo -> Cardano.HeaderHash -> Lazy.ByteString -> DBRead
  ReadBlock :: SlotNo ->                       Lazy.ByteString -> DBRead

dbBytes :: DBRead -> ByteString
dbBytes term = Lazy.toStrict $ case term of
  ReadEBB _ _ bytes -> bytes
  ReadBlock _ bytes -> bytes

data Tip where
  TipGenesis :: Tip
  TipEBB     :: SlotNo -> Cardano.HeaderHash -> Lazy.ByteString -> Tip
  TipBlock   :: SlotNo                       -> Lazy.ByteString -> Tip

-- | Make an `Iterator` from an `ImmutableDB` `Iterator`.
fromImmutableDBIterator
  :: ( Monad m )
  => Cardano.EpochSlots
  -> Immutable.Iterator Cardano.HeaderHash m Lazy.ByteString
  -> Iterator m
fromImmutableDBIterator epochSlots idbIterator = Iterator $ do
  idbNext <- Immutable.iteratorNext idbIterator
  case idbNext of
    Immutable.IteratorExhausted                  -> pure Done
    Immutable.IteratorResult    slot       bytes -> pure $ NextBlock slot      bytes recurse
    Immutable.IteratorEBB       epoch hash bytes -> pure $ NextEBB   slot hash bytes recurse
      where
      slot = SlotNo $ Cardano.unEpochSlots epochSlots * unEpochNo epoch
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
  -- CSL hash is used because hash-based indexing is for legacy support.
  FromHash    :: !CSL.HeaderHash -> Point
  FromPoint   :: !SlotNo -> !Cardano.HeaderHash -> Point

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

-- | The DB supports writes of legacy cardano-sl blocks, or new cardano-ledger
-- block or boundary blocks. Boundary blocks in the new style must be annotated
-- with ByteString because they do not otherwise hold enough information to
-- re-serialise.
data BlockToWrite where
  LegacyBlockToWrite  :: !CSL.Block -> BlockToWrite
  -- TODO something wrong here? Couldn't we have defined the types in such a
  -- way that we'd write
  --   Annotated Cardano.ABlockOrBoundary ByteString
  -- and have the annotation part be fed through to all of the sub parts?
  CardanoBlockToWrite :: !(Annotated (Cardano.ABlockOrBoundary ByteString) ByteString) -> BlockToWrite

data DBAppend m = DBAppend
  { appendBlock :: BlockToWrite -> m ()
  }

data DBError where
  IndexInconsistent :: Text -> DBError
  deriving (Show)

instance Exception DBError

data DBTrace where
  DBWrite :: !SlotNo -> DBTrace

-- | Blocks are written to the database in the cardano-sl `Block` `Bi` instance
-- encoding. In theory it would be better to encode `MainBlock` for non-EBB
-- and `GenesisBlock` for EBB, but then the epoch file parser would be more
-- difficult to define.
mkDB
  :: ( Monad m )
  => (forall x . DBError -> m x)
  -> Tracer m DBTrace
  -> Cardano.EpochSlots -- ^ Number of slots per epoch
                   -- (must never change in the chain stored here)
  -> Index m
  -> ImmutableDB Cardano.HeaderHash m
  -> DB m
mkDB err tracer epochSlots idx idb = DB
  { appendBlocks = appendBlocksImpl err tracer epochSlots idx idb
  , readFrom     = readFromImpl err epochSlots idx idb
  , readTip      = readTipImpl err epochSlots idx idb
  }

appendBlocksImpl
  :: ( Monad m )
  => (forall x . DBError -> m x)
  -> Tracer m DBTrace
  -> Cardano.EpochSlots
  -> Index m
  -> ImmutableDB Cardano.HeaderHash m
  -> (DBAppend m -> m t)
  -> m t
appendBlocksImpl err tracer epochSlots idx idb k =
  Index.indexWrite idx $ \iwrite ->
    k (dbAppendImpl err tracer epochSlots iwrite idb)

-- | Append one block, assumed to be within an `Index.indexWrite`.
-- It sets the block as the new tip in the index, then appends it to the
-- `ImmutableDB`, doing something slightly different depending on whether
-- it's an EBB or a main block.
dbAppendImpl
  :: ( Monad m )
  => (forall x . DBError -> m x)
  -> Tracer m DBTrace
  -> Cardano.EpochSlots
  -> IndexWrite m
  -> ImmutableDB Cardano.HeaderHash m
  -> DBAppend m
dbAppendImpl err tracer epochSlots iwrite idb = DBAppend $ \blockToWrite -> do
  -- Must serialise as a `Block` rather than a `MainBlock` or `GenesisBlock`,
  -- because the epoch file parser needs to be able to discriminate them.
  let builder = case blockToWrite of
        LegacyBlockToWrite  cslBlk -> CSL.serializeBuilder cslBlk
        -- The annotation is assumed to be the bytes from which it was decoded.
        CardanoBlockToWrite (Annotated _ bytes) ->
          Builder.byteString bytes
  slotNo <- case blockToWrite of
    LegacyBlockToWrite b@(Left ebb) -> do
      -- Write the index first, so that if something goes wrong with the
      -- `appendEBB` to the `ImmutableDB`, the transaction will quit and the
      -- index/db will remain consistent.
      let hash = CSL.headerHash ebb
          epoch = ebbEpoch ebb
          slot  = SlotNo (unEpochNo epoch * Cardano.unEpochSlots epochSlots)
      Index.updateTip iwrite hash epoch Index.EBBSlot
      Immutable.appendEBB idb epoch (coerceHashFromLegacy hash) builder
      pure slot
    LegacyBlockToWrite b@(Right blk) -> do
      let hash = CSL.headerHash blk
          (epoch, SlotNo wslot) = blockEpochAndRelativeSlot blk
          slot = SlotNo $ unEpochNo epoch * Cardano.unEpochSlots epochSlots + wslot
      Index.updateTip iwrite hash epoch (Index.RealSlot wslot)
      Immutable.appendBinaryBlob idb slot builder
      pure slot
    CardanoBlockToWrite (Annotated (Cardano.ABOBBlock blk) _) -> do
      let hash = Cardano.blockHashAnnotated blk
          slotNumber = Cardano.blockSlot blk
          slot = Cardano.unSlotNumber slotNumber
          Cardano.EpochNumber epoch = Cardano.slotNumberEpoch epochSlots slotNumber
      Index.updateTip iwrite (coerceHashToLegacy hash) (EpochNo epoch) (Index.RealSlot slot)
      Immutable.appendBinaryBlob idb (SlotNo slot) builder
      pure (SlotNo slot)
    CardanoBlockToWrite (Annotated (Cardano.ABOBBoundary ebb) _) -> do
      let hash = unsafeAbstractHash
            . Lazy.fromStrict
            . Cardano.wrapBoundaryBytes
            . Cardano.boundaryHeaderBytes
            $ ebb
          epoch = EpochNo (Cardano.boundaryEpoch ebb)
          slot  = SlotNo (unEpochNo epoch * Cardano.unEpochSlots epochSlots)
      Index.updateTip iwrite (coerceHashToLegacy hash) epoch Index.EBBSlot
      Immutable.appendEBB idb epoch hash builder
      pure slot
  traceWith tracer (DBWrite slotNo)

-- | Stream from a given point, using the index to determine the start point
-- in case the `Point` is a header hash.
--
-- This is a resource-acquiring function and so must be masked/bracketed
-- appropriately.
readFromImpl
  :: forall m .
     ( Monad m )
  => (forall x . DBError -> m x)
  -> Cardano.EpochSlots
  -> Index m
  -> ImmutableDB Cardano.HeaderHash m
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
        in  iteratorFromSlot (Just (slot, coerceHashFromLegacy hash))
  where
  iteratorFromSlot :: Maybe (SlotNo, Cardano.HeaderHash) -> m (IteratorResource m)
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
  -> Cardano.EpochSlots
  -> Index m
  -> ImmutableDB Cardano.HeaderHash m
  -> m Tip
readTipImpl err epochSlots idx idb = do
  tip <- Immutable.getTip idb
  case tip of
    -- Empty DB. Hm...
    Immutable.TipGen -> pure TipGenesis
    Immutable.Tip (Left epoch) -> do
      mItem <- Immutable.getEBB idb epoch
      case mItem of
        -- FIXME ImmutableDB bug
        Nothing -> error "ImmutableDB bug ebb"
        Just (hash, bytes) -> pure $ TipEBB slot hash bytes
          where
          slot = SlotNo $ Cardano.unEpochSlots epochSlots * unEpochNo epoch
    Immutable.Tip (Right slot) -> do
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

blockEpochAndRelativeSlot :: CSL.MainBlock -> (EpochNo, SlotNo)
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
indexToSlot :: Cardano.EpochSlots -> EpochNo -> Index.IndexSlot -> SlotNo
indexToSlot epochSlots (EpochNo epoch) idxSlot = SlotNo sl
  where
  sl = (Cardano.unEpochSlots epochSlots * epoch) + relative
  relative :: Word64
  relative = case idxSlot of
    Index.EBBSlot    -> 0
    Index.RealSlot w -> w
