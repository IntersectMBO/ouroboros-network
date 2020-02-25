{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Consensus.Ledger.OddChain (tests) where

import           Control.Monad.Except (Except, runExcept)
import           Test.QuickCheck (Property, counterexample, withMaxSuccess
                   , Gen, Arbitrary
                   , arbitrary, shrink
                   , Positive (Positive)
                   , oneof, vectorOf, elements, suchThat
                   , (===), (.&&.)
                   )
import           Test.Tasty (testGroup, TestTree)
import           Test.Tasty.QuickCheck (testProperty)
import           Control.Arrow (second)
import           Codec.Serialise (serialise, deserialise)

import           Data.Word (Word64)
import qualified Data.ByteString.Base16.Lazy as Base16

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes, DeserialiseFailure)
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

import           Ouroboros.Network.Block (ChainHash (GenesisHash, BlockHash), HeaderHash, pattern BlockPoint)
import           Ouroboros.Consensus.Ledger.OddChain (Tx (Tx), SignedPart, GenTx (OddTx)
                   , Header (OddHeader), oddBlockSignedPart, oddBlockSignature
                   , OddBlock (OddBlock), oddBlockHeader, oddBlockPayload
                   , mkSignedPart, errors, stLastApplied
                   , Phase (Decrease, Increase)
                   , LedgerState (LedgerState), phase
                   , OddError
                   , OddTxError (NotOdd, OddBut)
                   , OutOfBoundError (NotDecreasing, NotIncreasing)
                   , LedgerConfig (OddConfig), slotsPerEpoch, cfgNodeStartTime
                   , Hash
                   )
import           Ouroboros.Storage.Common (BinaryInfo (BinaryInfo),
                     binaryBlob, headerOffset,
                     headerSize)
import           Ouroboros.Consensus.Node.Run.Abstract (nodeEncodeBlockWithInfo)
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Block (getHeader)

import qualified Cardano.Crypto.Hash.Class as Crypto.Hash
import           Cardano.Crypto.Hash.Short (ShortHash)

import           Ouroboros.Consensus.Ledger.Abstract (applyLedgerBlock
                   , TickedLedgerState, tickedLedgerState
                   , applyChainTick
                   , LedgerConfig
                   , BlockProtocol
                   )
import           Ouroboros.Network.Block (blockSlot)
import           Ouroboros.Consensus.Mempool.API (applyTx, GenTx)

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
  , testGroup "Ledger properties"
      [ testProperty "Mempool safety"  prop_mempool
      , testProperty "Header encoding" prop_block_header_encoding
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
  $ checkRoundTrip bs (deserialiseFromBytes dec bs) a
  where
    bs = toLazyByteString (enc a)

checkRoundTrip
  :: (Eq a, Show a)
  => Lazy.ByteString
  -> Either DeserialiseFailure (Lazy.ByteString, Lazy.ByteString -> a)
  -> a
  -> Property
checkRoundTrip byteString deserialisationResult originalData =
  case deserialisationResult of
    Right (leftOverBytes, fa)
      | Lazy.null leftOverBytes
      -> originalData === fa byteString
      | otherwise
      -> counterexample ("left-over bytes: " <> show leftOverBytes) False
    Left e
      -> counterexample (show e) False
--------------------------------------------------------------------------------
-- Ledger properties
--------------------------------------------------------------------------------

-- | This is a property that every ledger should satisfy:
--
-- Applying a sequence of transactions should result in the same state as
-- applying those transactions in a block.
--
-- Given a sequence of transactions @[tx0 .. txn]@, the mempool validates these
-- transactions starting in a given state @s0@, resulting in a sequence of
-- intermediate states:
--
-- > s0 -- tx0 --> s1 -- tx1 --> .. --> sn -- txn --> sn+1
--
--
-- These transactions can end up in different blocks. So it is important that
-- when bundling a sub-sequence of transactions @[tx0 .. txj]@, @j <= n@ the
-- resulting state of applying a block with this sub-sequence of transactions
-- is the same as @sj+1@.
--
-- TODO: This should be better explained.
-- TODO: This should be made more generic.
--
prop_mempool
  :: LedgerConfig OddBlock
  -> OddBlock
  -> LedgerState OddBlock
  -> Property
prop_mempool cfg blk@OddBlock { oddBlockPayload } st
  = withMaxSuccess 10000
  $ counterexample (show $ tickedLedgerState tickedSt)
  $ case blockApplicationResult of
      ([], _) -> blockApplicationResult     === payloadApplicationResult
      _       -> fst blockApplicationResult === fst payloadApplicationResult
  where
    tickedSt = applyChainTick cfg slot st
    blockApplicationResult :: ([OddTxError], Phase)
    blockApplicationResult
      = second phase
      $ case runExcept $ applyLedgerBlock cfg blk st of
          Left err  -> (errors err, tickedLedgerState tickedSt)
          Right st' -> ([]        , st')

    payloadApplicationResult :: ([OddTxError], Phase)
    payloadApplicationResult
      = second phase
      $ second tickedLedgerState
      $ foldl (flip apply) ([], tickedSt) oddBlockPayload

    slot = blockSlot blk
    apply
      :: Tx
      -> ([OddTxError], TickedLedgerState OddBlock)
      -> ([OddTxError], TickedLedgerState OddBlock)
    apply tx (errs, st') =
      case runExcept $ applyTx cfg (OddTx tx) st' of
        Left err   -> (err: errs, st' )
        Right st'' -> (     errs, st'')

-- | The implementation 'nodeEncodeBlockWithInfo' should correctly encode the
-- block, and specify the header offset and size.
prop_block_header_encoding
  :: OddBlock
  -> Property
prop_block_header_encoding blk
  =    withMaxSuccess 10000
  $    checkRoundTrip byteString blockDeserialisationResult blk
  .&&. checkRoundTrip headerByteString headerDeserialisationResult (getHeader blk)
  where
    BinaryInfo { binaryBlob, headerOffset, headerSize } = nodeEncodeBlockWithInfo undefined blk
    byteString = toLazyByteString binaryBlob
    blockDeserialisationResult = deserialiseFromBytes (const <$> fromCBOR) byteString
    headerByteString = Lazy.take
                           (fromIntegral headerSize)
                           (Lazy.drop (fromIntegral headerOffset) byteString)
    headerDeserialisationResult = deserialiseFromBytes (const <$> fromCBOR) headerByteString

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

  shrink GenesisHash = []
  shrink _           = [GenesisHash] -- TODO: define properly

-- Quick and dirty hack to get 4 bytes hashes. This shouldn't be done in
-- this way since it has an implicit coupling on the number of bytes used
-- by `ShortHash`.
arbitraryBSOfLength4 ::  Gen Crypto.Hash.ByteString
arbitraryBSOfLength4 = do
  str <- vectorOf 4 arbitrary
  pure $! C.pack str

instance Arbitrary (Hash (Header OddBlock)) where
  arbitrary = Cardano.Crypto.UnsafeHash <$> arbitraryBSOfLength4


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
           { stLastApplied = BlockPoint aSlot aHash
           , phase = aPhase
           }

instance Arbitrary OddTxError where
  arbitrary =
    oneof [ NotOdd <$> arbitrary <*> arbitrary
          , OddBut <$> arbitrary <*> arbitrary
          ]

instance Arbitrary OutOfBoundError where
  arbitrary = elements [NotDecreasing, NotIncreasing]

instance Arbitrary (LedgerConfig OddBlock) where
  arbitrary = do
    aNumberOfSlots <- arbitrary `suchThat` (/= 0) -- Zero slots per epoch aren't allowed.
    aStartTime     <- arbitrary
    pure $!
      OddConfig
      { slotsPerEpoch = aNumberOfSlots
      , cfgNodeStartTime = aStartTime
      }
