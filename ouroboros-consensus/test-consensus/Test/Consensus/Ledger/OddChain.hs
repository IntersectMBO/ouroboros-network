{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Consensus.Ledger.OddChain (tests) where

import           Test.QuickCheck (Property, counterexample, (===), withMaxSuccess
                   , Gen, Arbitrary
                   , arbitrary, shrink
                   , Positive (Positive)
                   , oneof, vectorOf, elements
                   )
import           Test.Tasty (testGroup, TestTree)
import           Test.Tasty.QuickCheck (testProperty)

import           Data.Word (Word64)
import qualified Data.ByteString.Base16.Lazy as Base16



import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C
import           Test.Util.Orphans.Arbitrary ()

import           Cardano.Binary (ToCBOR, toCBOR, FromCBOR, fromCBOR)
import           Cardano.Crypto.DSIGN.Class (SignKeyDSIGN)
import           Cardano.Crypto.DSIGN.Mock (mockSign, MockDSIGN
                   , SignKeyDSIGN (SignKeyMockDSIGN)
                   , VerKeyDSIGN (VerKeyMockDSIGN)
                   )
import qualified Cardano.Crypto.Hash.Class as Cardano.Crypto

import           Ouroboros.Network.Block (ChainHash (GenesisHash, BlockHash), HeaderHash)
import           Ouroboros.Consensus.Ledger.OddChain (Tx (Tx), SignedPart
                   , Header (OddHeader), oddBlockSignedPart, oddBlockSignature
                   , OddBlock (OddBlock), oddBlockHeader, oddBlockPayload
                   , mkSignedPart
                   , Phase (Decrease, Increase)
                   , LedgerState (LedgerState), stCurrentSlot, stPrevHash, phase
                   , OddError
                   , OddTxError (NotOdd, OddBut)
                   , OutOfBoundError (NotDecreasing, NotIncreasing)
                   )

import qualified Cardano.Crypto.Hash.Class as Crypto.Hash
import           Cardano.Crypto.Hash.Short (ShortHash)

tests :: TestTree
tests = testGroup "Odd Chain"
  [ testGroup "Serialisation roundtrips"
      [ testRoundtrip @OddBlock               "Block"
      , testRoundtrip @(Header OddBlock)      "Block header"
      , testRoundtrip @SignedPart             "Signed part"
      , testRoundtrip @(ChainHash OddBlock)   "Chain hash"
      , testRoundtrip @Tx                     "Tx"
      , testRoundtrip @Phase                  "Phase"
      , testRoundtrip @(LedgerState OddBlock) "LedgerState"
      , testRoundtrip @OddTxError             "Tx Error"
      , testRoundtrip @OutOfBoundError        "Out of bound error"
      ]
  ]


testRoundtrip
  :: forall a
   . ( ToCBOR a
     , FromCBOR a
     , Arbitrary a
     , Eq a
     , Show a
     )
  => String -> TestTree
testRoundtrip label = testProperty ("roundtrip " <> label) $ prop_roundtrip @a

prop_roundtrip
  :: ( ToCBOR a
     , FromCBOR a
     , Arbitrary a
     , Eq a
     , Show a
     )
  => a -> Property
prop_roundtrip = withMaxSuccess 10000 . roundtrip toCBOR fromCBOR

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

roundtrip :: (Eq a, Show a)
          => (a -> Encoding)
          -> (forall s. Decoder s a)
          -> a
          -> Property
roundtrip enc dec = roundtrip' enc (const <$> dec)

-- | Roundtrip property for values annotated with their serialized form
--
-- NOTE: Suppose @a@ consists of a pair of the unannotated value @a'@ and some
-- 'Lazy.ByteString'. The roundtrip property will fail if that
-- 'Lazy.ByteString' encoding is not equal to @enc a'@. One way in which this
-- might happen is if the annotation is not canonical CBOR, but @enc@ does
-- produce canonical CBOR.
roundtrip' :: (Eq a, Show a)
           => (a -> Encoding)  -- ^ @enc@
           -> (forall s. Decoder s (Lazy.ByteString -> a))
           -> a
           -> Property
roundtrip' enc dec a
  = counterexample (show $ Base16.encode bs)
  $ case deserialiseFromBytes dec bs of
    Right (bs', a')
      | Lazy.null bs'
      -> a === a' bs
      | otherwise
      -> counterexample ("left-over bytes: " <> show bs') False
    Left e
      -> counterexample (show e) False
  where
    bs = toLazyByteString (enc a)

--------------------------------------------------------------------------------
-- Arbitrary instances and generators
--------------------------------------------------------------------------------

instance Arbitrary OddBlock where
  arbitrary = do
    payload <- arbitrary
    hdr     <- arbitraryHeader payload
    pure $! OddBlock
          { oddBlockHeader = hdr
          , oddBlockPayload = payload
          }

  shrink _ = [] -- TODO: define this properly. At least we should shrink the transactions.

deriving via Positive Int instance Arbitrary Tx
deriving via Positive Int instance Arbitrary (SignKeyDSIGN MockDSIGN)
deriving via Positive Int instance Arbitrary (VerKeyDSIGN MockDSIGN)

arbitraryHeader :: [Tx] -> Gen (Header OddBlock)
arbitraryHeader payload = do
  signedPart <- arbitrarySignedPart payload
  aSKey      <- arbitrary
  pure $! mkHeader signedPart aSKey

arbitrarySignedPart :: [Tx] -> Gen SignedPart
arbitrarySignedPart payload = do
  aVKey     <- arbitrary
  aPrevHash <- arbitrary
  aBlockNo  <- arbitrary
  aSlotNo   <- arbitrary
  pure $! mkSignedPart aVKey aPrevHash aBlockNo aSlotNo payload

instance Arbitrary SignedPart where
  arbitrary = do
    payload <- arbitrary
    arbitrarySignedPart payload

instance Arbitrary (Header OddBlock) where
  arbitrary = arbitrary >>= arbitraryHeader

instance Arbitrary (ChainHash OddBlock) where
  arbitrary = oneof [ pure GenesisHash
                    , BlockHash . Cardano.Crypto.UnsafeHash <$> arbitraryBSOfLength4
                    ]
    where
      -- Quick and dirty hack to get 4 bytes hashes. This shouldn't be done in
      -- this way since it has an implicit coupling on the number of bytes used
      -- by `ShortHash`.
      arbitraryBSOfLength4 = do
        str <- vectorOf 4 arbitrary
        pure $! C.pack str

  shrink GenesisHash = []
  shrink _           = [GenesisHash] -- TODO: define properly


mkHeader :: SignedPart -> SignKeyDSIGN MockDSIGN -> Header OddBlock
mkHeader signedPart sKey
  = OddHeader
  { oddBlockSignedPart = signedPart
  , oddBlockSignature  = mockSign signedPart sKey
  }

instance Arbitrary (Crypto.Hash.Hash ShortHash Lazy.ByteString) where
  arbitrary = Crypto.Hash.hash <$> arbitrary

instance Arbitrary Phase where
  arbitrary = oneof [ Increase <$> arbitrary
                    , Decrease <$> arbitrary
                    ]

instance Arbitrary (LedgerState OddBlock) where
  arbitrary = do
    aSlot  <- arbitrary
    aHash  <- arbitrary
    aPhase <- arbitrary
    pure $! LedgerState
           { stCurrentSlot = aSlot
           , stPrevHash = aHash
           , phase = aPhase
           }

instance Arbitrary OddTxError where
  arbitrary =
    oneof [ NotOdd <$> arbitrary <*> arbitrary
          , OddBut <$> arbitrary <*> arbitrary
          ]

instance Arbitrary OutOfBoundError where
  arbitrary = elements [NotDecreasing, NotIncreasing]
