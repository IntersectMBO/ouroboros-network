{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Generators (
    SomeResult (..)
  ) where

import           Ouroboros.Network.Block (mkSerialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCrypto,
                     TPraosState (..))

import           Generic.Random (genericArbitraryU)
import           Test.QuickCheck hiding (Result)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (SomeResult (..),
                     WithVersion (..))

import           Test.Consensus.Shelley.MockCrypto (CanMock)
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as SL
import           Test.Shelley.Spec.Ledger.Serialisation.Generators (genPParams)

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

instance CanMock era => Arbitrary (ShelleyBlock era) where
  arbitrary = mkShelleyBlock <$> arbitrary

instance CanMock era => Arbitrary (Header (ShelleyBlock era)) where
  arbitrary = getHeader <$> arbitrary

instance SL.Mock c => Arbitrary (ShelleyHash c) where
  arbitrary = ShelleyHash <$> arbitrary

instance CanMock era => Arbitrary (GenTx (ShelleyBlock era)) where
  arbitrary = mkShelleyTx <$> arbitrary

instance CanMock era => Arbitrary (GenTxId (ShelleyBlock era)) where
  arbitrary = ShelleyTxId <$> arbitrary

instance CanMock era => Arbitrary (SL.ApplyTxError era) where
  arbitrary = SL.ApplyTxError <$> arbitrary
  shrink (ApplyTxError xs) = [ApplyTxError xs' | xs' <- shrink xs]

instance CanMock era => Arbitrary (SomeBlock Query (ShelleyBlock era)) where
  arbitrary = oneof
    [ pure $ SomeBlock GetLedgerTip
    , pure $ SomeBlock GetEpochNo
    , SomeBlock . GetNonMyopicMemberRewards <$> arbitrary
    , pure $ SomeBlock GetCurrentPParams
    , pure $ SomeBlock GetProposedPParamsUpdates
    , pure $ SomeBlock GetStakeDistribution
    , pure $ SomeBlock DebugEpochState
    , (\(SomeBlock q) -> SomeBlock (GetCBOR q)) <$> arbitrary
    , SomeBlock . GetFilteredDelegationsAndRewardAccounts <$> arbitrary
    , pure $ SomeBlock GetGenesisConfig
    , pure $ SomeBlock DebugNewEpochState
    ]

instance CanMock era => Arbitrary (SomeResult (ShelleyBlock era)) where
  arbitrary = oneof
    [ SomeResult GetLedgerTip <$> arbitrary
    , SomeResult GetEpochNo <$> arbitrary
    , SomeResult <$> (GetNonMyopicMemberRewards <$> arbitrary) <*> arbitrary
    , SomeResult GetCurrentPParams <$> genPParams (Proxy @era)
    , SomeResult GetProposedPParamsUpdates <$> arbitrary
    , SomeResult GetStakeDistribution <$> arbitrary
    , SomeResult DebugEpochState <$> arbitrary
    , (\(SomeResult q r) ->
        SomeResult (GetCBOR q) (mkSerialised (encodeShelleyResult q) r)) <$>
      arbitrary
    , SomeResult <$> (GetFilteredDelegationsAndRewardAccounts <$> arbitrary) <*> arbitrary
    , SomeResult GetGenesisConfig . compactGenesis <$> arbitrary
    , SomeResult DebugNewEpochState <$> arbitrary
    ]

instance CanMock era => Arbitrary (NonMyopicMemberRewards era) where
  arbitrary = NonMyopicMemberRewards <$> arbitrary

instance CanMock era => Arbitrary (Point (ShelleyBlock era)) where
  arbitrary = BlockPoint <$> arbitrary <*> arbitrary

instance TPraosCrypto c => Arbitrary (TPraosState c) where
  arbitrary = do
      lastSlot <- frequency
        [ (1, return Origin)
        , (5, NotOrigin . SlotNo <$> choose (0, 100))
        ]
      TPraosState lastSlot <$> arbitrary

instance CanMock era => Arbitrary (ShelleyTip era) where
  arbitrary = ShelleyTip
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ShelleyTransition where
  arbitrary = ShelleyTransitionInfo <$> arbitrary

instance CanMock era => Arbitrary (LedgerState (ShelleyBlock era)) where
  arbitrary = ShelleyLedgerState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance CanMock era => Arbitrary (AnnTip (ShelleyBlock era)) where
  arbitrary = AnnTip
    <$> arbitrary
    <*> (BlockNo <$> arbitrary)
    <*> arbitrary

instance Arbitrary ShelleyNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ShelleyNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

instance ShelleyBasedEra era
      => Arbitrary (SomeBlock (NestedCtxt f) (ShelleyBlock era)) where
  arbitrary = return (SomeBlock indexIsTrivial)

{-------------------------------------------------------------------------------
  Generators for cardano-ledger-specs
-------------------------------------------------------------------------------}

instance Arbitrary (SL.PParams' SL.StrictMaybe era) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance TPraosCrypto c => Arbitrary (SL.ChainDepState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

{-------------------------------------------------------------------------------
  Versioned generators for serialisation
-------------------------------------------------------------------------------}

-- | We only have single version, so no special casing required.
--
-- This blanket orphan instance will have to be replaced with more specific
-- ones, once we introduce a different Shelley version.
instance Arbitrary a => Arbitrary (WithVersion ShelleyNodeToNodeVersion a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary

-- | This is @OVERLAPPABLE@ because we have to override the default behaviour
-- for 'Query's.
instance {-# OVERLAPPABLE #-} Arbitrary a
      => Arbitrary (WithVersion ShelleyNodeToClientVersion a) where
  arbitrary = WithVersion <$> arbitrary <*> arbitrary

-- | Some 'Query's are only supported by 'ShelleyNodeToClientVersion2', so we
-- make sure to not generate those queries in combination with
-- 'ShelleyNodeToClientVersion1'.
instance CanMock era
      => Arbitrary (WithVersion ShelleyNodeToClientVersion (SomeBlock Query (ShelleyBlock era))) where
  arbitrary = do
      query@(SomeBlock q) <- arbitrary
      version <- arbitrary `suchThat` querySupportedVersion q
      return $ WithVersion version query
