{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Test.Util.Corruption
  ( Corruption (..)
  , detectCorruption
  , applyCorruption
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy

import           Test.QuickCheck


newtype Corruption = Corruption Word
  deriving stock   (Show)
  deriving newtype (Arbitrary)

-- | Increment (overflow if necessary) the byte at position @i@ in the
-- bytestring, where @i = n `mod` length bs@.
--
-- If the bytestring is empty, return it unmodified.
applyCorruption :: Corruption -> Lazy.ByteString -> Lazy.ByteString
applyCorruption (Corruption n) bs
    | Lazy.null bs
    = bs
    | otherwise
    = before <> Lazy.cons (Lazy.head atAfter + 1) (Lazy.tail atAfter)
  where
    offset = fromIntegral n `mod` Lazy.length bs
    (before, atAfter) = Lazy.splitAt offset bs

-- | Serialise @a@, apply the given corruption, deserialise it, when that
-- fails, the corruption was detected. When deserialising the corrupted
-- bytestring succeeds, pass the deserialised value to the integrity checking
-- function. If that function returns 'False', the corruption was detected, if
-- it returns 'True', the corruption was not detected and the test fails.
detectCorruption
  :: Show a
  => (a -> Encoding)
  -> (forall s. Decoder s (Lazy.ByteString -> a))
  -> (a -> Bool)
     -- ^ Integrity check that should detect the corruption. Return 'False'
     -- when corrupt.
  -> a
  -> Corruption
  -> Property
detectCorruption enc dec isValid a cor = case deserialiseFromBytes dec bs of
    Right (_, mkA')
        | not (isValid a')
        -> label "corruption detected" $ property True
        | otherwise
        -> label "corruption not detected" $
           counterexample ("Corruption not detected: " <> show a') False
      where
        a' = mkA' bs
    Left _ -> label "corruption detected by decoder" $ property True
  where
    bs = applyCorruption cor $ toLazyByteString (enc a)
