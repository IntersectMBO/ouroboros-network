-- | QuickCheck generators for 'NetworkMagic', shared by the `node-to-node` and
-- `node-to-client` version data codec tests.
--
module Cardano.Network.Version.TestUtils
  ( genNetworkMagic
  , OutOfRangeMagic (..)
  , outOfWord32Range
  ) where

import Data.Int (Int32)
import Data.Word (Word32)

import Ouroboros.Network.Magic (NetworkMagic (..))

import Test.QuickCheck


-- | Generate a 'NetworkMagic', biased towards the boundaries at which its
-- representation changes.
--
-- `cborg` hands an unsigned integer back as `CBOR.TInt` when it fits `Int` and
-- as `CBOR.TInteger` otherwise; on 32bit platforms that switch happens at
-- `maxBound :: Int32`.  `maxBound :: Word32` is where the decoder's range
-- check kicks in.  Uniform sampling reaches either neighbourhood with
-- negligible probability, so we draw from around each boundary as well.
--
genNetworkMagic :: Gen NetworkMagic
genNetworkMagic =
    NetworkMagic <$>
      frequency
        [ (1, chooseBoundedIntegral (minBound, maxBound))
        , (1, nearBoundary)
        ]
  where
    nearBoundary = do
      boundary <- elements [ toInteger (minBound :: Word32)
                           , toInteger (maxBound :: Int32)
                           , toInteger (maxBound :: Word32)
                           ]
      offset <- choose (-4, 4)
      return . fromInteger
             . min (toInteger (maxBound :: Word32))
             . max (toInteger (minBound :: Word32))
             $ boundary + offset


-- | An 'Integer' outside the 'Word32' range, biased towards the boundaries.
--
-- Note that `minBound :: Int32` is not a boundary of 'genNetworkMagic': a
-- 'NetworkMagic' is a 'Word32', so no negative value is in its domain.  It is
-- a boundary of the /decoder/, which reads an 'Integer', and it is the value
-- a buggy peer is most likely to send: `fromIntegral (0x80000000 :: Word32)`
-- is exactly `minBound :: Int32`, so a 32bit peer encoding that magic through
-- `Word32 -> Int` wraps onto it.  A round-trip property cannot reach any of
-- this, because our encoder never emits a negative term.
--
newtype OutOfRangeMagic = OutOfRangeMagic Integer
  deriving Show

instance Arbitrary OutOfRangeMagic where
    arbitrary = OutOfRangeMagic <$> oneof
        [ below (toInteger (minBound :: Word32))
        , around (toInteger (minBound :: Int32))
        , above (toInteger (maxBound :: Word32))
        ]
      where
        below  b = (b -) <$> choose (1, 4)
        above  b = (b +) <$> choose (1, 4)
        -- every offset here is still comfortably negative
        around b = (b +) <$> choose (-4, 4)

    -- Only shrink to values which are still out of range: an in-range magic is
    -- one the decoder is meant to accept, so it is not a counterexample and
    -- would misreport the failure.  'shrinkIntegral' moves towards zero from
    -- either side, so filtering leaves `-1` and `0x100000000` as the two fixed
    -- points - the simplest witness on each side of the range.
    shrink (OutOfRangeMagic x) =
      [ OutOfRangeMagic x'
      | x' <- shrinkIntegral x
      , outOfWord32Range x'
      ]


outOfWord32Range :: Integer -> Bool
outOfWord32Range x = x < toInteger (minBound :: Word32)
                  || x > toInteger (maxBound :: Word32)
