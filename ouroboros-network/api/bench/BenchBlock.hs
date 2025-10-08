{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A simple block for benchmarking 'AnchoredFragment's.
module BenchBlock where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Builder
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BS
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Network.Block

-- | A 32 byte hash, just like for Cardano mainnet.
newtype BenchHash = BenchHash { getBenchHash :: ShortByteString }
  deriving stock (Generic)
  deriving newtype (Eq, Ord)
  deriving anyclass (NoThunks)

instance Show BenchHash where
  show (BenchHash h) =
    "BenchHash " <> show (toLazyByteString (byteStringHex (BS.fromShort h)))

data BenchBlock = BenchBlock {
    benchSlotNo  :: !SlotNo,
    benchBlockNo :: !BlockNo,
    benchHash    :: !BenchHash
  }
  deriving anyclass StandardHash

type instance HeaderHash BenchBlock = BenchHash

instance HasHeader BenchBlock where
  getHeaderFields b = HeaderFields {
      headerFieldSlot    = benchSlotNo b,
      headerFieldBlockNo = benchBlockNo b,
      headerFieldHash    = benchHash b
    }

mkBenchBlock ::
     SlotNo
  -> BlockNo
  -> Word64 -- ^ Fork number, to get different hashes despite same slot/block number.
  -> BenchBlock
mkBenchBlock benchSlotNo benchBlockNo forkNo = BenchBlock {
     benchSlotNo,
     benchBlockNo,
     -- Cardano uses Blake2b (256), while we use SHA256 here to keep the
     -- dependency footprint small; it doesn't matter as this isn't what is
     -- benchmarked here.
     benchHash = BenchHash $ BS.toShort $ SHA256.hashlazy $ toLazyByteString $
          word64LE (unSlotNo benchSlotNo)
       <> word64LE (unBlockNo benchBlockNo)
       <> word64LE forkNo
   }
