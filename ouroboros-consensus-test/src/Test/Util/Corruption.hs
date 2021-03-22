{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Test.Util.Corruption (
    Corruption (..)
  , applyCorruption
  , detectCorruption
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Term (Term)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (deserialise)
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
detectCorruption enc dec isValid a cor =
    case deserialiseFromBytes dec corruptBytes of
      Right (leftover, mkA')
          | not (Lazy.null leftover)
          -> label "corruption detected by decoder" $ property True
          | not (isValid a')
          -> label "corruption detected" $ property True
          | otherwise
          -> counterexample
               ("Corruption not detected: " <> show a')
           $ counterexample
               ("Original bytes: " <> show origBytes)
           $ counterexample
               ("Corrupt bytes: " <> show corruptBytes)
           $ counterexample
               ("Original CBOR: " <> show (deserialise origBytes :: Term))
           $ counterexample
               ("Corrupt CBOR: " <> show (deserialise corruptBytes :: Term))
             False
        where
          a' = mkA' corruptBytes
      Left _ -> label "corruption detected by decoder" $ property True
  where
    origBytes    = toLazyByteString (enc a)
    corruptBytes = applyCorruption cor origBytes
