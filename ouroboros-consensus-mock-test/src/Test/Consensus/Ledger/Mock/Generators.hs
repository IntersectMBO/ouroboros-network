{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Ledger.Mock.Generators () where

import           Codec.Serialise (Serialise, encode, serialise)
import           Control.Monad
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable

import           Cardano.Crypto.DSIGN
import           Cardano.Crypto.Hash

import           Test.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util (hashFromBytesE)

import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Block.BFT
import qualified Ouroboros.Consensus.Mock.Ledger.State as L
import qualified Ouroboros.Consensus.Mock.Ledger.UTxO as L
import           Ouroboros.Consensus.Mock.Node.Serialisation ()

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.Slotting.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

{-------------------------------------------------------------------------------
  General instances
-------------------------------------------------------------------------------}

instance Arbitrary (HeaderHash blk) => Arbitrary (ChainHash blk) where
  arbitrary = oneof [
        return GenesisHash
      , BlockHash <$> arbitrary
      ]

instance Arbitrary (HeaderHash blk) => Arbitrary (Point blk) where
  arbitrary = oneof [
        return GenesisPoint
      , BlockPoint <$> arbitrary <*> arbitrary
      ]

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- These generators blindly create random values, so the block will not be
-- valid, but this does not matter for serialisation tests.

instance (SimpleCrypto c, Arbitrary ext, Serialise ext)
      => Arbitrary (SimpleBlock c ext) where
  arbitrary = do
    simpleStdHeader <- arbitrary
    body            <- arbitrary
    ext             <- arbitrary
    let simpleStdHeader' = simpleStdHeader {
            -- Fill in the right body size, because we rely on this in the
            -- serialisation tests
            simpleBodySize = fromIntegral $ Lazy.length $ serialise body
          }
        hdr = mkSimpleHeader encode simpleStdHeader' ext
    return $ SimpleBlock hdr body

instance (SimpleCrypto c, Arbitrary ext, Serialise ext, Typeable ext)
      => Arbitrary (Header (SimpleBlock c ext)) where
  arbitrary = getHeader <$> arbitrary

instance (HashAlgorithm (SimpleHash c), Arbitrary ext, Serialise ext)
      => Arbitrary (SimpleStdHeader c ext) where
  arbitrary = SimpleStdHeader
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary

instance Arbitrary SimpleBody where
  arbitrary = SimpleBody <$> listOf arbitrary

instance Arbitrary (SomeSecond (NestedCtxt Header) (SimpleBlock c ext)) where
  arbitrary = return $ SomeSecond indexIsTrivial

instance Arbitrary (SomeSecond Query (SimpleBlock c ext)) where
  arbitrary = return $ SomeSecond QueryLedgerTip

instance (SimpleCrypto c, Typeable ext) => Arbitrary (SomeResult (SimpleBlock c ext)) where
  arbitrary = SomeResult QueryLedgerTip <$> arbitrary

instance Arbitrary (LedgerState (SimpleBlock c ext)) where
  arbitrary = SimpleLedgerState <$> arbitrary

instance HashAlgorithm (SimpleHash c) => Arbitrary (AnnTip (SimpleBlock c ext)) where
  arbitrary = do
      annTipSlotNo  <- SlotNo  <$> arbitrary
      annTipBlockNo <- BlockNo <$> arbitrary
      annTipInfo    <- arbitrary
      return AnnTip{..}

instance Arbitrary (GenTx (SimpleBlock c ext)) where
  arbitrary = do
      simpleGenTx   <- arbitrary
      simpleGenTxId <- arbitrary
      return SimpleGenTx{..}

instance Arbitrary (TxId (GenTx (SimpleBlock c ext))) where
  arbitrary = SimpleGenTxId <$> arbitrary

{-------------------------------------------------------------------------------
  Ledger

  TODO: This is /very/ minimal right now.
-------------------------------------------------------------------------------}

instance Arbitrary L.Tx where
  arbitrary = L.Tx L.DoNotExpire
         <$> pure mempty  -- For simplicity
         <*> arbitrary

instance Arbitrary L.Addr where
  arbitrary = elements ["a", "b", "c"]

instance Arbitrary (L.MockState blk) where
  arbitrary = return $ L.MockState {
        mockUtxo      = Map.empty
      , mockConfirmed = Set.empty
      , mockTip       = GenesisPoint
      }

instance Arbitrary (HeaderHash blk) => Arbitrary (L.MockError blk) where
  arbitrary = oneof [
      L.MockExpired <$> arbitrary <*> arbitrary
    -- , MockUtxOError <$> arbitrary -- TODO
    , L.MockInvalidHash <$> arbitrary <*> arbitrary
    ]

{-------------------------------------------------------------------------------
  Per protocol
-------------------------------------------------------------------------------}

instance Arbitrary (SimpleBftExt c BftMockCrypto) where
  arbitrary = do
      simpleBftExt <- arbitrary
      return SimpleBftExt{..}

instance Arbitrary (BftFields BftMockCrypto toSign) where
  arbitrary = do
      bftSignature <- SignedDSIGN <$>
                        (SigMockDSIGN <$> arbitrary <*> arbitrary)
      return BftFields{..}

instance HashAlgorithm h => Arbitrary (Hash h a) where
  arbitrary =
      hashFromBytesE . Strict.pack <$>
        replicateM (fromIntegral (sizeHash (Proxy @h))) arbitrary
