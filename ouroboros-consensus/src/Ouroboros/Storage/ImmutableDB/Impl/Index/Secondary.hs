{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary
  ( Entry (..)
  , entrySize
  , BlockOffset (..)
  , HeaderOffset (..)
  , HeaderSize (..)
  , BlockSize (..)
  , readEntry
  , readEntries
  , readAllEntries
  , appendEntry
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
import           Data.Word
import           Foreign.Storable (Storable (sizeOf))
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.Block (IsEBB (..))

import           Ouroboros.Storage.Common (EpochNo (..))
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.CRC
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import           Ouroboros.Storage.ImmutableDB.Impl.Util (renderFile, runGet,
                     runGetWithUnconsumed)
import           Ouroboros.Storage.ImmutableDB.Types (BlockOrEBB (..),
                     HashInfo (..), ImmutableDBError (..), WithBlockSize (..))

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- TODO best place for these?

newtype BlockOffset = BlockOffset { unBlockOffset :: Word64 }
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, Real, Integral, Num, Storable, NoUnexpectedThunks)

instance Binary BlockOffset where
  get = BlockOffset <$> Get.getWord64be
  put = Put.putWord64be . unBlockOffset

newtype HeaderOffset = HeaderOffset { unHeaderOffset :: Word16 }
  deriving stock   (Show)
  deriving newtype (Eq, Storable, NoUnexpectedThunks)

instance Binary HeaderOffset where
  get = HeaderOffset <$> Get.getWord16be
  put = Put.putWord16be . unHeaderOffset

newtype HeaderSize = HeaderSize { unHeaderSize :: Word16 }
  deriving stock   (Show)
  deriving newtype (Eq, Storable, NoUnexpectedThunks)

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

data Entry hash = Entry
  { blockOffset  :: !BlockOffset
  , headerOffset :: !HeaderOffset
  , headerSize   :: !HeaderSize
  , checksum     :: !CRC
  , headerHash   :: !hash
  , blockOrEBB   :: !BlockOrEBB
  } deriving (Eq, Show, Functor, Generic, NoUnexpectedThunks)

getEntry :: IsEBB -> Get hash -> Get (Entry hash)
getEntry isEBB getHash = do
    blockOffset  <- get
    headerOffset <- get
    headerSize   <- get
    checksum     <- CRC <$> Get.getWord32be
    headerHash   <- getHash
    blockOrEBB   <- getBlockOrEBB isEBB
    return Entry { blockOffset, headerOffset, headerSize, checksum, headerHash, blockOrEBB }

putEntry :: (hash -> Put) -> Entry hash -> Put
putEntry putHash Entry { blockOffset, headerOffset, headerSize, checksum = CRC crc, headerHash, blockOrEBB } =
    put     blockOffset
 <> put     headerOffset
 <> put     headerSize
 <> Put.putWord32be crc
 <> putHash headerHash
 <> putBlockOrEBB blockOrEBB

entrySize
  :: Word32 -- ^ Hash size in bytes
  -> Word32
entrySize hashSize =
    size 8 "blockOffset"  blockOffset
  + size 2 "headerOffset" headerOffset
  + size 2 "headerSize"   headerSize
  + size 4 "checksum"     checksum
  + hashSize
  + 8 -- blockOrEBB
  where
    size :: Storable a => Word32 -> String -> (Entry hash -> a) -> Word32
    size expected name field = assert (expected == actual) actual
      where
        actual = fromIntegral (sizeOf (field (error name)))

data BlockSize
  = BlockSize Word32
  | LastEntry
    -- ^ In case of the last entry, we don't have any entry and thus block
    -- offset after it that we can use to calculate the size of the block.
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

-- | Read the entry at the given 'SecondaryOffset'. Interpret it as an EBB
-- depending on the given 'IsEBB'.
readEntry
  :: forall m hash h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> EpochNo
  -> IsEBB
  -> SecondaryOffset
  -> m (Entry hash, BlockSize)
readEntry hasFS err hashInfo epoch isEBB slotOffset = runIdentity <$>
    readEntries hasFS err hashInfo epoch (Identity (isEBB, slotOffset))

-- | Same as 'readEntry', but for multiple entries.
--
-- NOTE: only use this for a few entries, as we will seek (@pread@) for each
-- entry. Use 'readAllEntries' if you want to read all entries in the
-- secondary index file.
readEntries
  :: forall m hash h t. (HasCallStack, MonadThrow m, Traversable t)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> EpochNo
  -> t (IsEBB, SecondaryOffset)
  -> m (t (Entry hash, BlockSize))
readEntries hasFS err HashInfo { hashSize, getHash } epoch toRead =
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
            runGet err secondaryIndexFile
              ((,) <$> getEntry isEBB getHash <*> get)
          let blockSize = fromIntegral $
                unBlockOffset nextBlockOffset -
                unBlockOffset (blockOffset entry)
          return (entry, BlockSize blockSize)
        else do
          entry <- hGetExactlyAt hasFS sHnd nbBytes offset >>=
            runGet err secondaryIndexFile (getEntry isEBB getHash)
          return (entry, LastEntry)
  where
    secondaryIndexFile = renderFile "secondary" epoch
    nbBytes            = fromIntegral $ entrySize hashSize
    nbBlockOffsetBytes = fromIntegral (sizeOf (blockOffset (error "blockOffset")))
    HasFS { hGetSize } = hasFS

-- | Read all entries in a secondary index file, starting from the given
-- 'SecondaryOffset' until the stop condition is true or until the end of the
-- file is reached. The entry for which the stop condition is true will be the
-- last in the returned list of entries.
readAllEntries
  :: forall m hash h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> SecondaryOffset       -- ^ Start from this offset
  -> EpochNo
  -> (Entry hash -> Bool)  -- ^ Stop condition: stop after this entry
  -> Word64                -- ^ The size of the epoch file, used to compute
                           -- the size of the last block.
  -> IsEBB                 -- ^ Is the first entry to read an EBB?
  -> m [WithBlockSize (Entry hash)]
readAllEntries hasFS err HashInfo { getHash } secondaryOffset epoch stopAfter
               epochFileSize = \isEBB ->
    withFile hasFS secondaryIndexFile ReadMode $ \sHnd -> do
      bl <- hGetAllAt hasFS sHnd (AbsOffset (fromIntegral secondaryOffset))
      go isEBB bl [] Nothing
  where
    secondaryIndexFile = renderFile "secondary" epoch

    go :: IsEBB  -- ^ Interpret the next entry as an EBB?
       -> Lazy.ByteString
       -> [WithBlockSize (Entry hash)]  -- ^ Accumulator
       -> Maybe (Entry hash)
          -- ^ The previous entry we read. We can only add it to the
          -- accumulator when we know its block size, which we compute based
          -- on the next entry's offset.
       -> m [WithBlockSize (Entry hash)]
    go isEBB bl acc mbPrevEntry
      | Lazy.null bl = return $ reverse $
        (addBlockSize epochFileSize <$> mbPrevEntry) `consMaybe` acc
      | otherwise    = do
        (remaining, entry) <- runGetWithUnconsumed err secondaryIndexFile
          (getEntry isEBB getHash) bl
        let offsetAfterPrevBlock = unBlockOffset (blockOffset entry)
            acc' = (addBlockSize offsetAfterPrevBlock <$> mbPrevEntry)
              `consMaybe` acc
        if stopAfter entry then

          if Lazy.null remaining then
            return $ reverse $ addBlockSize epochFileSize entry : acc'
          else do
            -- Read the next blockOffset so we can compute the size of the
            -- last block we read.
            --
            -- We know @remaining@ is not empty, so it contains at least the
            -- next entry (unless the file is invalid) and definitely the
            -- next entry's block offset.
            (_, nextBlockOffset) <-
              runGetWithUnconsumed err secondaryIndexFile get remaining
            return $ reverse $ addBlockSize nextBlockOffset entry : acc'

        else
          -- Pass 'IsNotEBB' because there can only be one EBB and that must
          -- be the first one in the file.
          go IsNotEBB remaining acc' (Just entry)

    -- | Add the block size to an entry, it is computed by subtracting the
    -- entry's block offset from the offset after the entry's block, i.e.,
    -- where the next block starts.
    addBlockSize :: Word64 -> Entry hash -> WithBlockSize (Entry hash)
    addBlockSize offsetAfter entry = WithBlockSize size entry
      where
        size = fromIntegral $ offsetAfter - unBlockOffset (blockOffset entry)

    consMaybe :: Maybe a -> [a] -> [a]
    consMaybe = maybe id (:)

appendEntry
  :: forall m hash h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> Handle h
  -> HashInfo hash
  -> Entry hash
  -> m Word64
     -- ^ The number of bytes written
appendEntry hasFS sHnd HashInfo { putHash, hashSize } entry = do
    bytesWritten <- hPut hasFS sHnd $ Put.execPut $ putEntry putHash entry
    return $
      assert (bytesWritten == fromIntegral (entrySize hashSize)) bytesWritten

-- | Remove all entries after the entry at the given 'SecondaryOffset'. That
-- entry will now be the last entry in the secondary index file.
truncateToEntry
  :: forall m hash h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> HashInfo hash
  -> EpochNo
  -> SecondaryOffset
  -> m ()
truncateToEntry hasFS HashInfo { hashSize } epoch secondaryOffset =
    withFile hasFS secondaryIndexFile (AppendMode AllowExisting) $ \sHnd ->
      hTruncate sHnd offset
  where
    secondaryIndexFile  = renderFile "secondary" epoch
    HasFS { hTruncate } = hasFS
    offset              = fromIntegral (secondaryOffset + entrySize hashSize)

writeAllEntries
  :: forall m hash h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> HashInfo hash
  -> EpochNo
  -> [Entry hash]
  -> m ()
writeAllEntries hasFS hashInfo epoch entries =
    withFile hasFS secondaryIndexFile (AppendMode AllowExisting) $ \sHnd -> do
      -- First truncate the file, otherwise we might leave some old contents
      -- at the end if the new contents are smaller than the previous contents
      hTruncate sHnd 0
      mapM_ (appendEntry hasFS sHnd hashInfo) entries
  where
    secondaryIndexFile  = renderFile "secondary" epoch
    HasFS { hTruncate } = hasFS
