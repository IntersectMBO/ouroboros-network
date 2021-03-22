{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}
module Test.Consensus.Util.Versioned (tests) where

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (DeserialiseFailure (..), Serialise (..))
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Util.Versioned

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Versioned"
    [ testCase "version0" test_version0
    , testCase "version1" test_version1
    , testCase "version2" test_version2
    , testCase "unknown"  test_unknown
    ]

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

-- Plan:
--
-- Version2 is the current version, i.e., the type we'll try to decode. We
-- test how we handle decoding encodings of previous versions.
--
-- Version0 is missing 'field2', which is a field that cannot be
-- reconstructed, so we should fail with 'IncompatibleVersion'.
--
-- Version1 has 'field2' but misses 'field3', but we can reconstruct 'field3'
-- by adding 'field1' to 'field2'. So we should be able to decode a 'Version2'
-- from an encoding of 'Version1' using 'Migrate'.

data Version0 = Version0
    { field1 :: Int
    }
  deriving (Eq, Show, Generic, Serialise)

data Version1 = Version1
    { field1 :: Int
    , field2 :: Int
      -- ^ Let's say this field cannot be reconstructed from nothing
    }
  deriving (Eq, Show, Generic, Serialise)

data Version2 = Version2
    { field1 :: Int
    , field2 :: Int
    , field3 :: Int
      -- ^ This field is the sum of 'field1' and 'field2'
    }
  deriving (Eq, Show, Generic, Serialise)

version0 :: Version0
version0 = Version0 1

version1 :: Version1
version1 = Version1 1 100

version2 :: Version2
version2 = Version2 1 100 101

decodeLatestVersion
  :: [(VersionNumber, VersionDecoder Version2)]
decodeLatestVersion =
    [ (0, Incompatible "missing field 1")
    , (1, Migrate (Decode decode) $ \(Version1 a b) ->
        return $ Version2 a b (a + b))
    , (2, Decode decode)
    ]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

test_decodeVersioned
  :: Serialise a
  => [(VersionNumber, VersionDecoder b)]
  -> VersionNumber
  -> a
  -> Either String (Versioned b)
test_decodeVersioned decs vn a =
    case deserialiseFromBytes
           (decodeVersioned decs)
           (toLazyByteString (encodeVersion vn (encode a))) of
      Left  (DeserialiseFailure _offset msg) -> Left  msg
      Right (_unconsumed, b)                 -> Right b

test_version0 :: Assertion
test_version0 =
    test_decodeVersioned decodeLatestVersion 0 version0
      @?= Left "IncompatibleVersion 0 \"missing field 1\""

test_version1 :: Assertion
test_version1 =
    test_decodeVersioned decodeLatestVersion 1 version1
      @?= Right (Versioned 1 version2)

test_version2 :: Assertion
test_version2 =
    test_decodeVersioned decodeLatestVersion 2 version2
      @?= Right (Versioned 2 version2)

test_unknown :: Assertion
test_unknown =
    test_decodeVersioned decodeLatestVersion 12 True
      @?= Left "UnknownVersion 12"
