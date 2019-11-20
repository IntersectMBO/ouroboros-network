{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Support for CRC
module Ouroboros.Storage.FS.CRC (
    -- * Wrap digest functionality
    CRC(..)
  , initCRC
  , updateCRC
  , computeCRC
    -- * File system functions with CRC functionality
  , hPutAllCRC
  , hPutCRC
  ) where

import           Control.Monad (foldM)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Coerce
import qualified Data.Digest.CRC32 as Digest
import           Data.Word
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Storage.FS.API

{-------------------------------------------------------------------------------
  Wrap functionality from digest
-------------------------------------------------------------------------------}

newtype CRC = CRC Word32
  deriving (Eq, Show, Generic, NoUnexpectedThunks, Storable)

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

-- | Variation on 'hPut' that also computes a CRC
hPutCRC :: forall m h. (HasCallStack, Monad m)
        => HasFS m h
        -> Handle h
        -> Builder
        -> m (Word64, CRC)
hPutCRC hasFS g = hPutAllCRC hasFS g . BS.toLazyByteString
