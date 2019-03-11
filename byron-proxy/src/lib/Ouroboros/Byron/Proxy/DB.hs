{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Byron.Proxy.DB where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR

import Control.Exception (Exception, bracket)
import Control.Lens ((^.))
import qualified Control.Lens as Lens (to)
import Control.Monad (unless)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString, fromStrict, null)
import Data.ByteString.Builder (Builder)
import Data.Conduit (ConduitT)
import qualified Data.Conduit as Conduit
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)

import qualified Pos.Binary as CSL (decode, deserialize', serialize')
import qualified Pos.Chain.Block as CSL
import qualified Pos.Core.Slotting as CSL

import Ouroboros.Byron.Proxy.Index (Index, IndexWrite)
import qualified Ouroboros.Byron.Proxy.Index as Index
import Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr)
import Ouroboros.Storage.ImmutableDB.API (ImmutableDB, Epoch (..), Slot (..))
import qualified Ouroboros.Storage.ImmutableDB.API as Immutable
import Ouroboros.Storage.ImmutableDB.Util (cborEpochFileParser')
import Ouroboros.Storage.FS.API (HasFS)

-- Combines an `ImmutableDB` and an `Index` into a system for appending and
-- reading cardano-sl `Block`s, with special care for epoch-boundary-blocks
-- (EBBs) and the genesis block, which are packed alongside the first block of
-- an epoch (if it exists).
--
-- Slots in the immutable DB are filled with tagged serialized blocks. There
-- are 3 possibilities:
--   - An EBB
--   - A main block
--   - An EBB followed by a main block.
-- This is expressed by the `Block` type in this module.
--
-- Since there is at most one append per slot in the `ImmutableDB`, the
-- EBBs cannot be written immediately, because the next block may also be
-- for that slot. The DB abstraction here takes care of this, holding the
-- most-recently-appended EBB in memory until the next block is appended, at
-- which point that EBB is appended to the proper spot, possibly alongside
-- the next block if it's for the first slot of that epoch.
--
-- The DB does not allow for concurrent writes, but does admit concurrent
-- reads (alongside other reads and alongside writes).
--
-- For reading, the unwritten EBB, if there is one, will not be given by the
-- conduit. It will be given only after it has been put into the ImmutableDB.
-- It will, however, be given as the tip by `readTip`. There is therefore
-- always a tip-of-chain, even if the actual on-disk database is empty.
--
-- Serialized blocks are read in a streaming style, by giving a start point,
-- which is either a header hash (to support Byron style) or a slot (the new
-- style). Conduit is used because the cardano-sl library also uses it. To
-- make it work with the `ImmutableDB` iterator API, we have to use the
-- `ResourceT` thing. Hopefully it all works out.

-- | The type which is written to a slot in the ImmutableDB. It can be a block,
-- an EBB, or both (EBBs do not necessarily take a whole slot).
data Block ebb blk where
  EBB   :: ebb -> Block ebb blk
  Block :: blk -> Block ebb blk
  Both  :: ebb -> blk -> Block ebb blk

-- | Use an 8-bit word tag to discriminate between the 3 constructors of
-- `Block`. If it's `Both`, the EBB encoding is appended after the block.
--
-- TODO do we need to use CBOR lists, or is this ok?
encode
  :: (ebb -> CBOR.Encoding)
  -> (blk -> CBOR.Encoding)
  -> (Block ebb blk -> CBOR.Encoding)
encode encEbb encBlk block = case block of
  EBB   ebb     -> CBOR.encodeWord8 0 <> encEbb ebb
  Block blk     -> CBOR.encodeWord8 1 <> encBlk blk
  Both  ebb blk -> CBOR.encodeWord8 2 <> encEbb ebb <> encBlk blk

decode
  :: CBOR.Decoder s ebb
  -> CBOR.Decoder s blk
  -> CBOR.Decoder s (Block ebb blk)
decode decEbb decBlk = do
  tag <- CBOR.decodeWord8
  case tag of
    0 -> EBB   <$> decEbb
    1 -> Block <$> decBlk
    2 -> Both  <$> decEbb <*> decBlk
    _ -> fail "expected 8-bit word tag"

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

-- | Decode a `Block ByteString ByteString`. The `ByteString`(s) are encoded
-- genesis or main blocks.
decodeSerialisedBlock
  :: Lazy.ByteString
  -> Either CBOR.DeserialiseFailure (Block ByteString ByteString)
decodeSerialisedBlock = decodeFull decoder
  where
  decoder :: CBOR.Decoder s (Block ByteString ByteString)
  decoder = decode CBOR.decodeBytes CBOR.decodeBytes

-- | Encode a `Block ByteString ByteString` where the bytes are serialized
-- genesis or main blocks.
encodeSerialisedBlock :: Block ByteString ByteString -> Builder
encodeSerialisedBlock = CBOR.toBuilder . encoding
  where
  encoding = encode CBOR.encodeBytes CBOR.encodeBytes

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
  -> Immutable.EpochFileParser ReadIncrementalErr m (Word, Immutable.Slot)
epochFileParser epochSlots hasFS = cborEpochFileParser' hasFS decoder
  where
  decoder :: forall s . CBOR.Decoder s Immutable.Slot
  decoder = do
    -- Each piece is a DB.Block, with the encoded GenesisBlock (EBB) or
    -- MainBlock in it. This decoder reads the whole thing, then decodes
    -- those bytes to determine the slot.
    blkBytes <- decode CBOR.decodeBytes CBOR.decodeBytes
    -- Make a cardano-sl `Block` from the `Block` by favouring the main block,
    -- if it's there.
    blk :: CSL.Block <- case blkBytes of
      EBB   bs    -> Left  <$> CSL.deserialize' bs
      Block bs    -> Right <$> CSL.deserialize' bs
      Both  _  bs -> Right <$> CSL.deserialize' bs
    pure (blockHeaderSlot epochSlots (CSL.getBlockHeader blk))

-- | Identifies a block in the DB: by slot or by header hash.
data Point where
  ByHash :: CSL.HeaderHash -> Point
  BySlot :: Slot           -> Point

-- | Unified database interface which supports lookup by hash or by slot.
data DB m = DB
  { -- | Append 0 or more blocks within the continuation.
    appendBlocks :: forall t . (DBAppend m -> m t) -> m t
    -- | Stream serialized blocks from a given point.
    -- Using `BySlot` for a slot which is divisible by the epoch size will
    -- begin at an EBB.
  , readFrom     :: Point -> ConduitT () ByteString m ()
  , readTip      :: m CSL.Block
  }

data DBAppend m = DBAppend
  { appendBlock :: CSL.Block -> m ()
  }

data UnwrittenEBB where
  NoUnwrittenEBB :: UnwrittenEBB
  -- | The serialised EBB is held along with the epoch in which it lives (0
  -- for genesis)
  -- Its serialisation is stored in order to save on potential re-serialisations
  -- from reads.
  -- We also need the hash so that we can put it into the index when the time
  -- comes.
  --
  -- But, we also need the `GenesisBlock` itself in order to support `readTip`.
  -- TODO knock out the `Epoch`, since it's cheaply derived from the
  -- `CSL.GenesisBlock`
  UnwrittenEBB
    :: !CSL.HeaderHash
    -> !Epoch
    -> !ByteString
    -> !CSL.GenesisBlock
    -> UnwrittenEBB

data DBError where
  IndexInconsistent :: Text -> DBError
  MalformedBlock    :: CBOR.DeserialiseFailure -> DBError
  deriving (Show)

instance Exception DBError

withDB
  :: (forall x . DBError -> IO x)
  -> CSL.GenesisBlock
  -- ^ The actual genesis block (not an EBB). Used in case the ImmtuableDB
  -- is empty (iff the Index is empty), in which case it will be written to
  -- slot 0 on the first append of a later block.
  -> CSL.SlotCount
  -- ^ Must know how many slots are in an epoch, and this must be constant for
  -- all epochs for which there are blocks in the DB.
  -> Index IO
  -> ImmutableDB IO
  -> (DB IO -> IO t)
  -> IO t
withDB err genesis epochSlots idx idb = bracket
  (openDB err genesis epochSlots idx idb)
  (const (pure ()))

openDB
  :: (forall x . DBError -> IO x)
  -> CSL.GenesisBlock
  -> CSL.SlotCount
  -> Index IO
  -> ImmutableDB IO
  -> IO (DB IO)
openDB err genesis epochSlots idx idb = do
  dbTip <- Index.indexRead idx Index.Tip
  -- The genesis block is the unwritten EBB only if the database is empty.
  -- If we put it as the unwritten EBB and it weren't empty, it would be
  -- written at the first write and we'd get an inconsistent database.
  let unwrittenEBB = case dbTip of
        Just _  -> NoUnwrittenEBB
        Nothing -> UnwrittenEBB (CSL.headerHash genesis)
                                (ebbEpoch genesis)
                                (CSL.serialize' genesis)
                                genesis
  unwrittenEBBRef <- newIORef unwrittenEBB
  pure $ DB
    { appendBlocks = \k -> appendBlocksImpl err epochSlots unwrittenEBBRef idx idb k
    , readFrom     = readFromImpl err epochSlots unwrittenEBBRef idx idb
    , readTip      = readTipImpl err epochSlots unwrittenEBBRef idx idb
    }

appendBlocksImpl
  :: (forall x . DBError -> IO x)
  -> CSL.SlotCount
  -> IORef UnwrittenEBB
  -> Index IO
  -> ImmutableDB IO
  -> (DBAppend IO -> IO t)
  -> IO t
appendBlocksImpl err epochSlots unwrittenEBBRef idx idb k =
  Index.indexWrite idx $ \iwrite ->
    k (dbAppendImpl err epochSlots unwrittenEBBRef iwrite idb)

-- | Append one block. This is the most complicated part.
dbAppendImpl
  :: (forall x . DBError -> IO x)
  -> CSL.SlotCount
  -> IORef UnwrittenEBB
  -> IndexWrite IO
  -> ImmutableDB IO
  -> DBAppend IO
dbAppendImpl err epochSlots unwrittenEBBRef iwrite idb = DBAppend $ \cslBlock -> case cslBlock of
  -- It's an EBB: we _always_ put it into the unwritten EBB IORef. If something
  -- is already there, we'll commit it to the index and immutable DB.
  -- No consistency check is done; we leave that to the immutable DB.
  Left ebb -> do
    prevUnwrittenEBB <- atomicModifyIORef' unwrittenEBBRef $ \it ->
      let hh = CSL.headerHash ebb
          epoch = ebbEpoch ebb
          bytes = CSL.serialize' ebb
      in  (UnwrittenEBB hh epoch bytes ebb, it)
    case prevUnwrittenEBB of
      -- There was nothing before, so we're done. Next write will see that
      -- there's an unwritten EBB and write it.
      NoUnwrittenEBB -> pure ()
      -- Very rare and weird case: we get 2 EBBs in a row.
      UnwrittenEBB hh epoch bytes _ -> do
        Index.updateTip iwrite hh epoch Index.EBBSlot
        -- The slot in the immutable DB for EBBs is the same as the slot for
        -- the first block of this epoch.
        let slot = fromIntegral (epochSlots * fromIntegral epoch)
        Immutable.appendBinaryBlob idb slot (encodeSerialisedBlock (EBB bytes))
  Right mainBlk -> do
    prevUnwrittenEBB <- atomicModifyIORef' unwrittenEBBRef $ \it ->
      (NoUnwrittenEBB, it)
    case prevUnwrittenEBB of
      NoUnwrittenEBB -> do
        let hh = CSL.headerHash mainBlk
            (epoch, Slot wslot) = blockEpochAndSlot mainBlk
            bytes = CSL.serialize' mainBlk
            -- wslot is relative to epoch; for the ImmutableDB we need relative
            -- to genesis
            slot = Slot (fromIntegral epoch * fromIntegral epochSlots + wslot)
        Index.updateTip iwrite hh epoch (Index.RealSlot wslot)
        Immutable.appendBinaryBlob idb slot (encodeSerialisedBlock (Block bytes))
      UnwrittenEBB hh epoch bytes _ -> do
        let hh' = CSL.headerHash mainBlk
            (epoch', Slot wslot) = blockEpochAndSlot mainBlk
            bytes' = CSL.serialize' mainBlk
            -- wslot is relative to epoch; for the ImmutableDB we need relative
            -- to genesis
            slot = Slot (fromIntegral epoch' * fromIntegral epochSlots + wslot)
        Index.updateTip iwrite hh  epoch  Index.EBBSlot
        Index.updateTip iwrite hh' epoch' (Index.RealSlot wslot)
        Immutable.appendBinaryBlob idb slot (encodeSerialisedBlock (Both bytes bytes'))

-- | Stream from a given point, using the index to determine the start point
-- in case the `Point` is a header hash.
readFromImpl
  :: (forall x . DBError -> IO x)
  -> CSL.SlotCount
  -> IORef UnwrittenEBB
  -> Index IO
  -> ImmutableDB IO
  -> Point
  -> ConduitT () ByteString IO ()
readFromImpl err epochSlots unwrittenEBBRef idx idb point = do
  -- First, check whether the point is for the unwritten tip.
  -- TODO is it right to do this though? We do it for `readTipImpl`.
  -- If we read it here, surely we should also check it at the end of
  -- streaming. But we can't read at the end of the stream, because there's a
  -- possibility we miss a block. Between the final read of the stream and the
  -- read of the unwritten tip, a block and then an EBB could be written, and
  -- the intermediate block would be missed.
  unwrittenEBB <- lift $ readIORef unwrittenEBBRef
  case (unwrittenEBB, point) of
    -- If the point is a slot corresponding to the EBB then we'll yield it
    -- and stop.
    (UnwrittenEBB _ (Epoch epoch) bs _, BySlot slot) ->
      if slot == fromIntegral (epochSlots * fromIntegral epoch)
      then Conduit.yield bs
      else streamFromSlot False slot
    (_, BySlot slot) -> streamFromSlot False slot
    (_, ByHash hh)   -> streamFromHash hh
  where

  -- Use an `ImmutableDB` iterator to stream serialised blocks. The
  -- iterator must be bracketed, so we have to use a `ResourceT`.
  -- Surely there is a better way?
  streamWithIterator
    :: Bool -- ^ True means skip the EBB if the iterator is at a block that
            -- includes an EBB and a main block. Also set to False on recursive
            -- calls.
    -> Immutable.Iterator IO
    -> ConduitT () ByteString (ResourceT IO) ()
  streamWithIterator skipEbb iter = do
    next <- lift . lift $ Immutable.iteratorNext iter
    case next of
      Immutable.IteratorExhausted -> pure ()
      -- Must decode the `Block` wrapper.
      -- The bytes could be an EBB and main block packed together.
      Immutable.IteratorResult _ bytes -> case decodeSerialisedBlock (Lazy.fromStrict bytes) of
        Left cborError -> lift . lift $ err $ MalformedBlock cborError
        Right (EBB bytes) -> do
          Conduit.yield bytes
          streamWithIterator False iter
        Right (Block bytes) -> do
          Conduit.yield bytes
          streamWithIterator False iter
        Right (Both bytesEbb bytesBlock) -> do
          unless skipEbb (Conduit.yield bytesEbb)
          Conduit.yield bytesBlock
          streamWithIterator False iter

  streamFromSlot :: Bool -> Slot -> ConduitT () ByteString IO ()
  streamFromSlot skipEbb sl = Conduit.transPipe runResourceT $ Conduit.bracketP
    (Immutable.streamBinaryBlobs idb (Just sl) Nothing)
    Immutable.iteratorClose
    (streamWithIterator skipEbb)

  -- Streaming from a hash is done by finding the `Slot` of the start point
  -- then using `streamFromSlot`.
  -- NB: we don't even need the forward links in the sqlite database.
  streamFromHash :: CSL.HeaderHash -> ConduitT () ByteString IO ()
  streamFromHash hh = do
    idxItem <- lift $ Index.indexRead idx (Index.ByHash hh)
    case idxItem of
      -- If the hash is not in the database, it's not an error, we just give an
      -- empty stream.
      Nothing -> pure ()
      -- If the hash is in the database, we now know the slot, but we must take
      -- care in case it's 0 modulo the slots per epoch: if it's not for an
      -- EBB (relative slot 0) then we have to ensure that `streamFromHash`
      -- does not yield the EBB which also lives at that slot. That's what
      -- the `Bool` parameter is for.
      Just (_, epoch, indexSlot) -> streamFromSlot
        (not (isEbbSlot indexSlot))
        (indexToSlot epochSlots epoch indexSlot)

readTipImpl
  :: (forall x . DBError -> IO x)
  -> CSL.SlotCount
  -> IORef UnwrittenEBB
  -> Index IO
  -> ImmutableDB IO
  -> IO CSL.Block
readTipImpl err epochSlots unwrittenEBBRef idx idb = do
  unwritten <- readIORef unwrittenEBBRef
  case unwritten of
    UnwrittenEBB _ _ _ ebb -> pure (Left ebb)
    NoUnwrittenEBB -> Index.indexRead idx Index.Tip >>= \mTip -> case mTip of
      -- If this happens, it's a bug in this module.
      Nothing -> error "readTipImpl: empty index and no unwritten tip"
      -- The index gives an EpochSlot, from which we can determine the actual
      -- slot by
      --   epoch * epochSlots + (relativeSlot `mod` epochSlots)
      -- since EBBs have epochSlots as their relativeSlot, we get 0 for these
      -- and for the first main block.
      Just (_, epoch, indexSlot) -> do
        mBs <- Immutable.getBinaryBlob idb (indexToSlot epochSlots epoch indexSlot)
        case mBs of
          Nothing -> err $ IndexInconsistent "missing tip"
          -- `epochSlots` and the relative slot from the index tells us whether
          -- we're looking for an EBB or a main block.
          Just bytes -> case decodeSerialisedBlock (Lazy.fromStrict bytes) of
            Left cborError -> err (MalformedBlock cborError)
            Right block -> case (isEbbSlot indexSlot, block) of
              (True,  Block _)    -> err $ IndexInconsistent "missing EBB"
              (False, EBB _)      -> err $ IndexInconsistent "missing main block"
              (True,  Both ebbBytes _) -> case decodeFull CSL.decode (Lazy.fromStrict ebbBytes) of
                Left cborError -> err $ MalformedBlock cborError
                Right ebb       -> pure $ Left ebb
              (False, Both _ blkBytes) -> case decodeFull CSL.decode (Lazy.fromStrict blkBytes) of
                Left cborError -> err $ MalformedBlock cborError
                Right blk      -> pure $ Right blk
              (True,  EBB ebbBytes)    -> case decodeFull CSL.decode (Lazy.fromStrict ebbBytes) of
                Left cborError -> err $ MalformedBlock cborError
                Right ebb      -> pure $ Left ebb
              (False, Block blkBytes)  -> case decodeFull CSL.decode (Lazy.fromStrict blkBytes) of
                Left cborError -> err $ MalformedBlock cborError
                Right blk      -> pure $ Right blk

-- | The `Epoch` of an EBB.
ebbEpoch :: CSL.GenesisBlock -> Epoch
ebbEpoch ebb = ebb ^.
    CSL.gbHeader
  . CSL.gbhConsensus
  . CSL.gcdEpoch
  . Lens.to (Epoch . fromIntegral . CSL.getEpochIndex)

blockEpochAndSlot :: CSL.MainBlock -> (Epoch, Slot)
blockEpochAndSlot blk = blk ^.
    CSL.gbHeader
  . CSL.gbhConsensus
  . CSL.mcdSlot
  . Lens.to (\sl ->
      ( Epoch (fromIntegral (CSL.getEpochIndex (CSL.siEpoch sl)))
      , Slot  (fromIntegral (CSL.getSlotIndex  (CSL.siSlot  sl)))
      )
    )

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

-- | True if the `EpochSlot` is for an EBB (or genesis).
isEbbSlot :: Index.IndexSlot -> Bool
isEbbSlot idxSlot = case idxSlot of
  Index.EBBSlot    -> True
  Index.RealSlot _ -> False

-- | Put the EBBs for epoch `n` at the same slot (relative to genesis) as the
-- first block of epoch `n`.
indexToSlot :: CSL.SlotCount -> Epoch -> Index.IndexSlot -> Slot
indexToSlot epochSlots (Epoch epoch) idxSlot = Slot sl
  where
  sl = (fromIntegral epochSlots * epoch) + relative
  relative :: Word
  relative = case idxSlot of
    Index.EBBSlot    -> 0
    Index.RealSlot w -> w
