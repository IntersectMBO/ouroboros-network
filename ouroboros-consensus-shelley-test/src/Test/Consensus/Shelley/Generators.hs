{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Generators (
    SomeResult (..)
  ) where

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

instance Arbitrary Block where
  arbitrary = mkShelleyBlock <$> arbitrary

instance Arbitrary (Header Block) where
  arbitrary = getHeader <$> arbitrary

instance Crypto c => Arbitrary (ShelleyHash c) where
  arbitrary = ShelleyHash <$> arbitrary

instance Arbitrary (GenTx Block) where
  arbitrary = mkShelleyTx <$> arbitrary

instance Arbitrary (GenTxId Block) where
  arbitrary = ShelleyTxId <$> arbitrary

instance Arbitrary (ApplyTxError TPraosMockCrypto) where
  arbitrary = ApplyTxError <$> arbitrary
  shrink (ApplyTxError xs) = [ApplyTxError xs' | xs' <- shrink xs]

instance Arbitrary (SomeBlock Query Block) where
  arbitrary = oneof
    [ pure $ SomeBlock GetLedgerTip
    , pure $ SomeBlock GetEpochNo
    , SomeBlock . GetNonMyopicMemberRewards <$> arbitrary
    , pure $ SomeBlock GetCurrentPParams
    , pure $ SomeBlock GetProposedPParamsUpdates
    , pure $ SomeBlock GetStakeDistribution
    , pure $ SomeBlock GetCurrentLedgerState
    , (\(SomeBlock q) -> SomeBlock (GetCBOR q)) <$> arbitrary
    , SomeBlock . GetFilteredDelegationsAndRewardAccounts <$> arbitrary
    ]

instance Arbitrary (SomeResult Block) where
  arbitrary = oneof
    [ SomeResult GetLedgerTip <$> arbitrary
    , SomeResult GetEpochNo <$> arbitrary
    , SomeResult <$> (GetNonMyopicMemberRewards <$> arbitrary) <*> arbitrary
    , SomeResult GetCurrentPParams <$> arbitrary
    , SomeResult GetProposedPParamsUpdates <$> arbitrary
    , SomeResult GetStakeDistribution <$> arbitrary
    , SomeResult GetCurrentLedgerState <$> arbitrary
    , (\(SomeResult q r) ->
        SomeResult (GetCBOR q) (mkSerialised (encodeShelleyResult q) r)) <$>
      arbitrary
    , SomeResult <$> (GetFilteredDelegationsAndRewardAccounts <$> arbitrary) <*> arbitrary
    ]

instance Arbitrary (NonMyopicMemberRewards TPraosMockCrypto) where
  arbitrary = NonMyopicMemberRewards <$> arbitrary

instance Arbitrary (Point Block) where
  arbitrary = BlockPoint <$> arbitrary <*> arbitrary

instance Arbitrary (TPraosState TPraosMockCrypto) where
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
         -> TPraosState TPraosMockCrypto
         -> Gen (TPraosState TPraosMockCrypto)
      go steps prevSlot st
        | 0 <- steps = return st
        | otherwise  = do
          let slot = withOrigin (SlotNo 0) succ prevSlot
          newPrtclState <- arbitrary
          go (steps - 1) (At slot) (TPraosState.append slot newPrtclState st)

instance Arbitrary (History.LedgerViewHistory TPraosMockCrypto) where
  arbitrary = do
      snapshots   <- choose (0, 5)
      startSlot   <- SlotNo <$> choose (0, 100)
      go snapshots startSlot History.empty
    where
      maxRollback = 5

      go :: Int
         -> SlotNo
         -> LedgerViewHistory TPraosMockCrypto
         -> Gen (LedgerViewHistory TPraosMockCrypto)
      go snapshots slot hist
        | 0 <- snapshots = return hist
        | otherwise      = do
          ledgerView <- arbitrary
          let hist' = History.snapOld maxRollback slot ledgerView hist
          go (snapshots - 1) (succ slot) hist'

instance Arbitrary (LedgerState Block) where
  arbitrary = ShelleyLedgerState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary (AnnTip Block) where
  arbitrary = AnnTip
    <$> arbitrary
    <*> (BlockNo <$> arbitrary)
    <*> arbitrary

instance Arbitrary ShelleyNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ShelleyNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary (SomeBlock (NestedCtxt f) Block) where
  arbitrary = return (SomeBlock indexIsTrivial)

{-------------------------------------------------------------------------------
  Generators for cardano-ledger-specs
-------------------------------------------------------------------------------}

instance Arbitrary (SL.PParams' SL.StrictMaybe) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (SL.ProposedPPUpdates TPraosMockCrypto) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance Arbitrary (SL.GenDelegs TPraosMockCrypto) where
  arbitrary = SL.GenDelegs <$> arbitrary

instance Arbitrary (SL.LedgerView TPraosMockCrypto) where
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
