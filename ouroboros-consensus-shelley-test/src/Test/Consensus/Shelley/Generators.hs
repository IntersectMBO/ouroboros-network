{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Generators (
    SomeResult (..)
  ) where

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Ouroboros.Network.Block (BlockNo (..), pattern BlockPoint,
                     Point, SlotNo (..), mkSerialised)
import           Ouroboros.Network.Point (WithOrigin (..), withOrigin)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.History (LedgerViewHistory)
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import           Generic.Random (genericArbitraryU)
import           Test.QuickCheck hiding (Result)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation (SomeResult (..), WithVersion (..))

import           Test.Consensus.Shelley.MockCrypto
import           Test.Shelley.Spec.Ledger.SerializationProperties ()

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

instance HashAlgorithm h => Arbitrary (Block h) where
  arbitrary = mkShelleyBlock <$> arbitrary

instance HashAlgorithm h => Arbitrary (Header (Block h)) where
  arbitrary = getHeader <$> arbitrary

instance Crypto c => Arbitrary (ShelleyHash c) where
  arbitrary = ShelleyHash <$> arbitrary

instance HashAlgorithm h => Arbitrary (GenTx (Block h)) where
  arbitrary = mkShelleyTx <$> arbitrary

instance HashAlgorithm h => Arbitrary (GenTxId (Block h)) where
  arbitrary = ShelleyTxId <$> arbitrary

instance (HashAlgorithm h, forall a. Arbitrary (Hash h a))
      => Arbitrary (ApplyTxError (TPraosMockCrypto h)) where
  arbitrary = ApplyTxError <$> arbitrary
  shrink (ApplyTxError xs) = [ApplyTxError xs' | xs' <- shrink xs]

instance HashAlgorithm h => Arbitrary (SomeBlock Query (Block h)) where
  arbitrary = oneof
    [ pure $ SomeBlock GetLedgerTip
    , pure $ SomeBlock GetEpochNo
    , SomeBlock . GetNonMyopicMemberRewards <$> arbitrary
    , pure $ SomeBlock GetCurrentPParams
    , pure $ SomeBlock GetProposedPParamsUpdates
    , pure $ SomeBlock GetStakeDistribution
    , pure $ SomeBlock GetCurrentEpochState
    , (\(SomeBlock q) -> SomeBlock (GetCBOR q)) <$> arbitrary
    , SomeBlock . GetFilteredDelegationsAndRewardAccounts <$> arbitrary
    ]

instance HashAlgorithm h => Arbitrary (SomeResult (Block h)) where
  arbitrary = oneof
    [ SomeResult GetLedgerTip <$> arbitrary
    , SomeResult GetEpochNo <$> arbitrary
    , SomeResult <$> (GetNonMyopicMemberRewards <$> arbitrary) <*> arbitrary
    , SomeResult GetCurrentPParams <$> arbitrary
    , SomeResult GetProposedPParamsUpdates <$> arbitrary
    , SomeResult GetStakeDistribution <$> arbitrary
    , SomeResult GetCurrentEpochState <$> arbitrary
    , (\(SomeResult q r) ->
        SomeResult (GetCBOR q) (mkSerialised (encodeShelleyResult q) r)) <$>
      arbitrary
    , SomeResult <$> (GetFilteredDelegationsAndRewardAccounts <$> arbitrary) <*> arbitrary
    ]

instance HashAlgorithm h
      => Arbitrary (NonMyopicMemberRewards (TPraosMockCrypto h)) where
  arbitrary = NonMyopicMemberRewards <$> arbitrary

instance HashAlgorithm h => Arbitrary (Point (Block h)) where
  arbitrary = BlockPoint <$> arbitrary <*> arbitrary

instance HashAlgorithm h => Arbitrary (TPraosState (TPraosMockCrypto h)) where
  arbitrary = do
      steps     <- choose (0, 5)
      startSlot <- frequency
        [ (1, return Origin)
        , (5, At . SlotNo <$> choose (0, 100))
        ]
      initState <- TPraosState.empty startSlot <$> arbitrary
      go steps startSlot initState
    where
      go :: Int
         -> WithOrigin SlotNo
         -> TPraosState (TPraosMockCrypto h)
         -> Gen (TPraosState (TPraosMockCrypto h))
      go steps prevSlot st
        | 0 <- steps = return st
        | otherwise  = do
          let slot = withOrigin (SlotNo 0) succ prevSlot
          newPrtclState <- arbitrary
          go (steps - 1) (At slot) (TPraosState.append slot newPrtclState st)

instance (HashAlgorithm h, forall a. Arbitrary (Hash h a))
      => Arbitrary (History.LedgerViewHistory (TPraosMockCrypto h)) where
  arbitrary = do
      snapshots   <- choose (0, 5)
      startSlot   <- SlotNo <$> choose (0, 100)
      go snapshots startSlot History.empty
    where
      maxRollback = 5

      go :: Int
         -> SlotNo
         -> LedgerViewHistory (TPraosMockCrypto h)
         -> Gen (LedgerViewHistory (TPraosMockCrypto h))
      go snapshots slot hist
        | 0 <- snapshots = return hist
        | otherwise      = do
          ledgerView <- arbitrary
          let hist' = History.snapOld maxRollback slot ledgerView hist
          go (snapshots - 1) (succ slot) hist'

instance (HashAlgorithm h, forall a. Arbitrary (Hash h a))
      => Arbitrary (LedgerState (Block h)) where
  arbitrary = ShelleyLedgerState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance HashAlgorithm h => Arbitrary (AnnTip (Block h)) where
  arbitrary = AnnTip
    <$> arbitrary
    <*> (BlockNo <$> arbitrary)
    <*> arbitrary

instance Arbitrary ShelleyNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ShelleyNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary (SomeBlock (NestedCtxt f) (Block h)) where
  arbitrary = return (SomeBlock indexIsTrivial)

{-------------------------------------------------------------------------------
  Generators for cardano-ledger-specs
-------------------------------------------------------------------------------}

instance Arbitrary (SL.PParams' SL.StrictMaybe) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance HashAlgorithm h
      => Arbitrary (SL.ProposedPPUpdates (TPraosMockCrypto h)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance (HashAlgorithm h, forall a. Arbitrary (Hash h a))
      => Arbitrary (SL.GenDelegs (TPraosMockCrypto h)) where
  arbitrary = (SL.GenDelegs . fmap (uncurry SL.GenDelegPair)) <$> arbitrary

instance (HashAlgorithm h, forall a. Arbitrary (Hash h a))
      => Arbitrary (SL.LedgerView (TPraosMockCrypto h)) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

{-------------------------------------------------------------------------------
  Versioned generators for serialisation
-------------------------------------------------------------------------------}

-- | We only have single version, so no special casing required.
--
-- This blanket orphan instance will have to be replaced with more specific
-- ones, once we introduce a different Shelley version.
instance Arbitrary a => Arbitrary (WithVersion ShelleyNodeToNodeVersion a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary

-- | We only have single version, so no special casing required.
--
-- This blanket orphan instance will have to be replaced with more specific
-- ones, once we introduce a different Shelley version.
instance Arbitrary a => Arbitrary (WithVersion ShelleyNodeToClientVersion a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary
