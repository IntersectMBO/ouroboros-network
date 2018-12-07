{-# LANGUAGE BangPatterns #-}
module Ouroboros.Storage.Util (
    encodeIndexEntry
  , decodeIndexEntry
  , decodeIndexEntryAt
  ) where

import           Control.Exception (assert)

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Word

-- | Encodes each index entry into a fixed-size sequence of bytes. In particular,
-- the first parameter is an 'Integer' representing the offset within the epoch
-- file. However, using a single 'Word32' is enough to represents up to 4GB
-- epoch files on disk, which is more than enough to be safe during the loss
-- of precision deriving from the cast \"Integer -> Word32\".
encodeIndexEntry :: Word64
                 -- ^ The current offset within the epoch file.
                 -> BS.Builder
encodeIndexEntry = BS.word64BE

-- | Decodes the entry of an index file.
-- Precondition: the input 'ByteString' @must be@ 8 bytes long.
decodeIndexEntry :: BS.ByteString
                 -- ^ A binary blob
                 -> Word64
decodeIndexEntry = readWord64BE 0

decodeIndexEntryAt :: Int
                   -- ^ Start reading from the given index
                   -> BS.ByteString
                   -- ^ A binary blob
                   -> Word64
decodeIndexEntryAt = readWord64BE

readWord64BE :: Int -> BS.ByteString -> Word64
readWord64BE !i bs =
    assert (i >= 0 && i+7 <= BS.length bs - 1) $
    fromIntegral (BS.unsafeIndex bs (i + 0)) `shiftL` 56
  + fromIntegral (BS.unsafeIndex bs (i + 1)) `shiftL` 48
  + fromIntegral (BS.unsafeIndex bs (i + 2)) `shiftL` 40
  + fromIntegral (BS.unsafeIndex bs (i + 3)) `shiftL` 32
  + fromIntegral (BS.unsafeIndex bs (i + 4)) `shiftL` 24
  + fromIntegral (BS.unsafeIndex bs (i + 5)) `shiftL` 16
  + fromIntegral (BS.unsafeIndex bs (i + 6)) `shiftL` 8
  + fromIntegral (BS.unsafeIndex bs (i + 7))
