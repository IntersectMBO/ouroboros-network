{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Support for CRC
module Ouroboros.Consensus.Storage.FS.CRC (
    -- * Wrap digest functionality
    CRC (..)
  , computeCRC
  , initCRC
  , updateCRC
    -- * File system functions with CRC functionality
  , hGetAllAtCRC
  , hGetExactlyAtCRC
  , hPutAllCRC
  ) where

import           Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Coerce
import qualified Data.Digest.CRC32 as Digest
import           Data.Word
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GHC.Stack
import           NoThunks.Class (NoThunks)

import           Control.Monad.Class.MonadThrow


import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types (AbsOffset (..))

{-------------------------------------------------------------------------------
  Wrap functionality from digest
-------------------------------------------------------------------------------}

newtype CRC = CRC { getCRC :: Word32 }
  deriving (Eq, Show, Generic, NoThunks, Storable)

initCRC :: CRC
initCRC = CRC $ Digest.crc32 ([] :: [Word8])

updateCRC :: forall a. Digest.CRC32 a => a -> CRC -> CRC
updateCRC = coerce (flip (Digest.crc32Update :: Word32 -> a -> Word32))

computeCRC :: forall a. Digest.CRC32 a => a -> CRC
computeCRC = coerce (Digest.crc32 :: a -> Word32)

{-------------------------------------------------------------------------------
  File system functions that compute CRCs
-------------------------------------------------------------------------------}

-- | Variation on 'hPutAll' that also computes a CRC
hPutAllCRC :: forall m h. (HasCallStack, Monad m)
           => HasFS m h
           -> Handle h
           -> BL.ByteString
           -> m (Word64, CRC)
hPutAllCRC hasFS h = foldM putChunk (0, initCRC) . BL.toChunks
  where
    putChunk :: (Word64, CRC) -> BS.ByteString -> m (Word64, CRC)
    putChunk (written, crc) chunk = do
      chunkSize <- hPutAllStrict hasFS h chunk
      let !written' = written + chunkSize
          !crc'     = updateCRC chunk crc
      return (written', crc')

-- | Variation on 'hGetExactlyAt' that also computes a CRC
hGetExactlyAtCRC :: forall m h. (HasCallStack, MonadThrow m)
                 => HasFS m h
                 -> Handle h
                 -> Word64    -- ^ The number of bytes to read.
                 -> AbsOffset -- ^ The offset at which to read.
                 -> m (BL.ByteString, CRC)
hGetExactlyAtCRC hasFS h n offset = do
    -- TODO Interleave reading with computing the CRC. Better cache locality
    -- and fits better with incremental parsing, when we add support for that.
    bs <- hGetExactlyAt hasFS h n offset
    let !crc = computeCRC bs
    return (bs, crc)

-- | Variation on 'hGetAllAt' that also computes a CRC
hGetAllAtCRC :: forall m h. Monad m
             => HasFS m h
             -> Handle h
             -> AbsOffset -- ^ The offset at which to read.
             -> m (BL.ByteString, CRC)
hGetAllAtCRC hasFS h offset = do
    -- TODO Interleave reading with computing the CRC. Better cache locality
    -- and fits better with incremental parsing, when we add support for that.
    bs <- hGetAllAt hasFS h offset
    let !crc = computeCRC bs
    return (bs, crc)
