{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary (
    BlockOffset (..)
  , BlockSize (..)
  , Entry (..)
  , HeaderOffset (..)
  , HeaderSize (..)
  , appendEntry
  , entrySize
  , readAllEntries
  , readEntries
  , readEntry
  , truncateToEntry
  , writeAllEntries
  ) where

import           Control.Exception (assert)
import           Control.Monad (forM)
import           Data.Binary (Binary (..), Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor.Identity (Identity (..))
import           Data.Typeable (Typeable)
import           Data.Word
import           Foreign.Storable (Storable (sizeOf))
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block hiding (headerHash)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.CRC

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
                     (BlockOrEBB (..), WithBlockSize (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
                     (fsPathSecondaryIndexFile, runGet, runGetWithUnconsumed)

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

newtype BlockOffset = BlockOffset { unBlockOffset :: Word64 }
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, Real, Integral, Num, Storable, NoThunks)

instance Binary BlockOffset where
  get = BlockOffset <$> Get.getWord64be
  put = Put.putWord64be . unBlockOffset

newtype HeaderOffset = HeaderOffset { unHeaderOffset :: Word16 }
  deriving stock   (Show)
  deriving newtype (Eq, Storable, NoThunks)

instance Binary HeaderOffset where
  get = HeaderOffset <$> Get.getWord16be
  put = Put.putWord16be . unHeaderOffset

newtype HeaderSize = HeaderSize { unHeaderSize :: Word16 }
  deriving stock   (Show)
  deriving newtype (Eq, Storable, NoThunks)

instance Binary HeaderSize where
  get = HeaderSize <$> Get.getWord16be
  put = Put.putWord16be . unHeaderSize

getBlockOrEBB :: IsEBB -> Get BlockOrEBB
getBlockOrEBB IsEBB    = EBB   . EpochNo <$> Get.getWord64be
getBlockOrEBB IsNotEBB = Block . SlotNo  <$> Get.getWord64be

putBlockOrEBB :: BlockOrEBB -> Put
putBlockOrEBB blockOrEBB = Put.putWord64be $ case blockOrEBB of
    Block slotNo  -> unSlotNo slotNo
    EBB   epochNo -> unEpochNo epochNo

{------------------------------------------------------------------------------
  Entry
------------------------------------------------------------------------------}

data Entry blk = Entry {
      blockOffset  :: !BlockOffset
    , headerOffset :: !HeaderOffset
    , headerSize   :: !HeaderSize
    , checksum     :: !CRC
    , headerHash   :: !(HeaderHash blk)
    , blockOrEBB   :: !BlockOrEBB
    }
  deriving (Generic)

deriving instance StandardHash blk => Eq       (Entry blk)
deriving instance StandardHash blk => Show     (Entry blk)
deriving instance StandardHash blk => NoThunks (Entry blk)

getEntry :: forall blk. ConvertRawHash blk => IsEBB -> Get (Entry blk)
getEntry isEBB = do
    blockOffset  <- get
    headerOffset <- get
    headerSize   <- get
    checksum     <- CRC <$> Get.getWord32be
    headerHash   <- getHash pb
    blockOrEBB   <- getBlockOrEBB isEBB
    return Entry {..}
  where
    pb :: Proxy blk
    pb = Proxy

putEntry :: forall blk. ConvertRawHash blk => Entry blk -> Put
putEntry Entry {..} = mconcat [
      put                blockOffset
    , put                headerOffset
    , put                headerSize
    , Put.putWord32be    (getCRC checksum)
    , putHash         pb headerHash
    , putBlockOrEBB      blockOrEBB
    ]
  where
    pb :: Proxy blk
    pb = Proxy

entrySize :: ConvertRawHash blk => Proxy blk -> Word32
entrySize pb =
    size 8 "blockOffset"  blockOffset
  + size 2 "headerOffset" headerOffset
  + size 2 "headerSize"   headerSize
  + size 4 "checksum"     checksum
  + hashSize pb
  + 8 -- blockOrEBB
  where
    size :: Storable a => Word32 -> String -> (Entry blk -> a) -> Word32
    size expected name field = assert (expected == actual) actual
      where
        actual = fromIntegral (sizeOf (field (error name)))

data BlockSize
  = BlockSize Word32
  | LastEntry
    -- ^ In case of the last entry, we don't have any entry and thus block
    -- offset after it that we can use to calculate the size of the block.
  deriving (Eq, Show, Generic, NoThunks)

-- | Read the entry at the given 'SecondaryOffset'. Interpret it as an EBB
-- depending on the given 'IsEBB'.
readEntry
  :: forall m blk h.
     ( HasCallStack
     , ConvertRawHash blk
     , MonadThrow m
     , StandardHash blk
     , Typeable blk
     )
  => HasFS m h
  -> ChunkNo
  -> IsEBB
  -> SecondaryOffset
  -> m (Entry blk, BlockSize)
readEntry hasFS chunk isEBB slotOffset = runIdentity <$>
    readEntries hasFS chunk (Identity (isEBB, slotOffset))

-- | Same as 'readEntry', but for multiple entries.
--
-- NOTE: only use this for a few entries, as we will seek (@pread@) for each
-- entry. Use 'readAllEntries' if you want to read all entries in the
-- secondary index file.
readEntries
  :: forall m blk h t.
     ( HasCallStack
     , ConvertRawHash blk
     , MonadThrow m
     , StandardHash blk
     , Typeable blk
     , Traversable t
     )
  => HasFS m h
  -> ChunkNo
  -> t (IsEBB, SecondaryOffset)
  -> m (t (Entry blk, BlockSize))
readEntries hasFS chunk toRead =
    withFile hasFS secondaryIndexFile ReadMode $ \sHnd -> do
      -- TODO can we avoid this call to 'hGetSize'?
      size <- hGetSize sHnd
      forM toRead $ \(isEBB, slotOffset) -> do
        let offset = AbsOffset (fromIntegral slotOffset)
            -- Is there another entry after the entry we need to read so that
            -- we can read its 'blockOffset' that will allow us to calculate
            -- the size of the block.
            anotherEntryAfter = size >=
              unAbsOffset offset + nbBytes + nbBlockOffsetBytes
        if anotherEntryAfter then do
          (entry, nextBlockOffset) <-
            hGetExactlyAt hasFS sHnd (nbBytes + nbBlockOffsetBytes) offset >>=
            runGet (Proxy @blk) secondaryIndexFile
              ((,) <$> getEntry isEBB <*> get)
          let blockSize = fromIntegral $
                unBlockOffset nextBlockOffset -
                unBlockOffset (blockOffset entry)
          return (entry, BlockSize blockSize)
        else do
          entry <- hGetExactlyAt hasFS sHnd nbBytes offset >>=
            runGet (Proxy @blk) secondaryIndexFile (getEntry isEBB)
          return (entry, LastEntry)
  where
    secondaryIndexFile = fsPathSecondaryIndexFile chunk
    nbBytes            = fromIntegral $ entrySize (Proxy @blk)
    nbBlockOffsetBytes = fromIntegral (sizeOf (blockOffset (error "blockOffset")))
    HasFS { hGetSize } = hasFS

-- | Read all entries in a secondary index file, starting from the given
-- 'SecondaryOffset' until the stop condition is true or until the end of the
-- file is reached. The entry for which the stop condition is true will be the
-- last in the returned list of entries.
readAllEntries
  :: forall m blk h.
     ( HasCallStack
     , ConvertRawHash blk
     , MonadThrow m
     , StandardHash blk
     , Typeable blk
     )
  => HasFS m h
  -> SecondaryOffset      -- ^ Start from this offset
  -> ChunkNo
  -> (Entry blk -> Bool)  -- ^ Stop condition: stop after this entry
  -> Word64               -- ^ The size of the chunk file, used to compute
                          -- the size of the last block.
  -> IsEBB                -- ^ Is the first entry to read an EBB?
  -> m [WithBlockSize (Entry blk)]
readAllEntries hasFS secondaryOffset chunk stopAfter chunkFileSize = \isEBB ->
    withFile hasFS secondaryIndexFile ReadMode $ \sHnd -> do
      bl <- hGetAllAt hasFS sHnd (AbsOffset (fromIntegral secondaryOffset))
      go isEBB bl [] Nothing
  where
    secondaryIndexFile = fsPathSecondaryIndexFile chunk

    go :: IsEBB  -- ^ Interpret the next entry as an EBB?
       -> Lazy.ByteString
       -> [WithBlockSize (Entry blk)]  -- ^ Accumulator
       -> Maybe (Entry blk)
          -- ^ The previous entry we read. We can only add it to the
          -- accumulator when we know its block size, which we compute based
          -- on the next entry's offset.
       -> m [WithBlockSize (Entry blk)]
    go isEBB bl acc mbPrevEntry
      | Lazy.null bl = return $ reverse $
        (addBlockSize chunkFileSize <$> mbPrevEntry) `consMaybe` acc
      | otherwise    = do
        (remaining, entry) <-
          runGetWithUnconsumed (Proxy @blk) secondaryIndexFile (getEntry isEBB) bl
        let offsetAfterPrevBlock = unBlockOffset (blockOffset entry)
            acc' = (addBlockSize offsetAfterPrevBlock <$> mbPrevEntry)
              `consMaybe` acc
        if stopAfter entry then

          if Lazy.null remaining then
            return $ reverse $ addBlockSize chunkFileSize entry : acc'
          else do
            -- Read the next blockOffset so we can compute the size of the
            -- last block we read.
            --
            -- We know @remaining@ is not empty, so it contains at least the
            -- next entry (unless the file is invalid) and definitely the
            -- next entry's block offset.
            (_, nextBlockOffset) <-
              runGetWithUnconsumed (Proxy @blk) secondaryIndexFile get remaining
            return $ reverse $ addBlockSize nextBlockOffset entry : acc'

        else
          -- Pass 'IsNotEBB' because there can only be one EBB and that must
          -- be the first one in the file.
          go IsNotEBB remaining acc' (Just entry)

    -- | Add the block size to an entry, it is computed by subtracting the
    -- entry's block offset from the offset after the entry's block, i.e.,
    -- where the next block starts.
    addBlockSize :: Word64 -> Entry blk -> WithBlockSize (Entry blk)
    addBlockSize offsetAfter entry = WithBlockSize size entry
      where
        size = fromIntegral $ offsetAfter - unBlockOffset (blockOffset entry)

    consMaybe :: Maybe a -> [a] -> [a]
    consMaybe = maybe id (:)

appendEntry
  :: forall m blk h. (HasCallStack, ConvertRawHash blk, MonadThrow m)
  => HasFS m h
  -> Handle h
  -> Entry blk
  -> m Word64
     -- ^ The number of bytes written
appendEntry hasFS sHnd entry = do
    bytesWritten <- hPut hasFS sHnd $ Put.execPut $ putEntry entry
    return $
      assert (bytesWritten == fromIntegral (entrySize (Proxy @blk))) bytesWritten

-- | Remove all entries after the entry at the given 'SecondaryOffset'. That
-- entry will now be the last entry in the secondary index file.
truncateToEntry
  :: forall m blk h. (HasCallStack, ConvertRawHash blk, MonadThrow m)
  => Proxy blk
  -> HasFS m h
  -> ChunkNo
  -> SecondaryOffset
  -> m ()
truncateToEntry pb hasFS chunk secondaryOffset =
    withFile hasFS secondaryIndexFile (AppendMode AllowExisting) $ \sHnd ->
      hTruncate sHnd offset
  where
    secondaryIndexFile  = fsPathSecondaryIndexFile chunk
    HasFS { hTruncate } = hasFS
    offset              = fromIntegral (secondaryOffset + entrySize pb)

writeAllEntries
  :: forall m blk h. (HasCallStack, ConvertRawHash blk, MonadThrow m)
  => HasFS m h
  -> ChunkNo
  -> [Entry blk]
  -> m ()
writeAllEntries hasFS chunk entries =
    withFile hasFS secondaryIndexFile (AppendMode AllowExisting) $ \sHnd -> do
      -- First truncate the file, otherwise we might leave some old contents
      -- at the end if the new contents are smaller than the previous contents
      hTruncate sHnd 0
      mapM_ (appendEntry hasFS sHnd) entries
  where
    secondaryIndexFile  = fsPathSecondaryIndexFile chunk
    HasFS { hTruncate } = hasFS

{------------------------------------------------------------------------------
  Binary functions
------------------------------------------------------------------------------}

getHash :: ConvertRawHash blk => Proxy blk -> Get (HeaderHash blk)
getHash pb = do
    bytes <- Get.getByteString (fromIntegral (hashSize pb))
    return $! fromRawHash pb bytes

putHash :: ConvertRawHash blk => Proxy blk -> HeaderHash blk -> Put
putHash pb = Put.putShortByteString . toShortRawHash pb
