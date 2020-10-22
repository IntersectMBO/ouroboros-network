{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Generators (
    module Test.Consensus.Byron.Generators
  ) where

import           Data.Coerce
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.SOP.Strict (K (..), NP (..), NS (..), SListI, lengthSList)

import           Test.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (RelativeTime (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Serialisation (Some (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (NonEmpty (..),
                     nonEmptyFromList)

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import           Ouroboros.Consensus.HardFork.Combinator.State

import           Ouroboros.Consensus.Byron.Ledger

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node (CardanoHardForkConstraints)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (WithVersion (..))

import           Test.Consensus.Byron.Generators

import           Test.Consensus.Shelley.Generators
import           Test.Consensus.Shelley.MockCrypto (CanMock)

import           Test.Consensus.Cardano.MockCrypto

{-------------------------------------------------------------------------------
  Disk
-------------------------------------------------------------------------------}

instance Arbitrary (CardanoBlock MockCryptoCompatByron) where
  arbitrary = oneof
    [ BlockByron   <$> arbitrary
    , BlockShelley <$> arbitrary
    , BlockAllegra <$> arbitrary
    , BlockMary    <$> arbitrary
    ]

instance Arbitrary (CardanoHeader MockCryptoCompatByron) where
  arbitrary = getHeader <$> arbitrary

-- TODO if we try to use arbitrary instances for 'SlotNo' and 'EpochNo' here, we
-- hit a conflict, since they exist both in byron generators and shelley
-- generators.
instance Arbitrary History.Bound where
  arbitrary =
      History.Bound <$> (RelativeTime <$> arbitrary)
                    <*> (SlotNo       <$> arbitrary)
                    <*> (EpochNo      <$> arbitrary)

arbitraryHardForkState
  :: forall f c a.
     ( Arbitrary (f ByronBlock)
     , Arbitrary (f (ShelleyBlock (ShelleyEra c)))
     , Coercible a (HardForkState f (CardanoEras c))
     )
  => Proxy f
  -> Gen a
arbitraryHardForkState _ = coerce <$> oneof
    [ TZ
        <$> genCurrent (Proxy @ByronBlock)
    , TS
        <$> (K  <$> genPast)
        <*> (TZ <$> genCurrent (Proxy @(ShelleyBlock (ShelleyEra c))))
    , TS
        <$> (K  <$> genPast)
        <*> (TS
              <$> (K  <$> genPast)
              <*> (TZ <$> genCurrent (Proxy @(ShelleyBlock (AllegraEra c)))))
    , TS
        <$> (K  <$> genPast)
        <*> (TS
              <$> (K  <$> genPast)
              <*> (TS
                    <$> (K  <$> genPast)
                    <*> (TZ <$> genCurrent (Proxy @(ShelleyBlock (MaryEra c))))))
    ]
  where
    genCurrent
      :: forall blk. Arbitrary (f blk)
      => Proxy blk
      -> Gen (Current f blk)
    genCurrent _ = Current <$> arbitrary <*> (arbitrary @(f blk))

    genPast :: Gen Past
    genPast = Past <$> arbitrary <*> arbitrary

instance (c ~ MockCryptoCompatByron, ShelleyBasedEra (ShelleyEra c))
      => Arbitrary (CardanoLedgerState c) where
  arbitrary = arbitraryHardForkState (Proxy @LedgerState)

instance c ~ MockCryptoCompatByron
      => Arbitrary (HardForkChainDepState (CardanoEras c)) where
  arbitrary = arbitraryHardForkState (Proxy @WrapChainDepState)

-- | Forwarding
instance Arbitrary (ChainDepState (BlockProtocol blk))
      => Arbitrary (WrapChainDepState blk) where
  arbitrary = WrapChainDepState <$> arbitrary

instance (CanMock (ShelleyEra c), CardanoHardForkConstraints c)
      => Arbitrary (OneEraHash (CardanoEras c)) where
  arbitrary = OneEraHash <$> oneof
    [ toShortRawHash (Proxy @ByronBlock) <$> arbitrary
    , toShortRawHash (Proxy @(ShelleyBlock (ShelleyEra c))) <$> arbitrary
    , toShortRawHash (Proxy @(ShelleyBlock (AllegraEra c))) <$> arbitrary
    , toShortRawHash (Proxy @(ShelleyBlock (MaryEra    c))) <$> arbitrary
    ]

instance (c ~ MockCryptoCompatByron, ShelleyBasedEra (ShelleyEra c))
      => Arbitrary (AnnTip (CardanoBlock c)) where
  arbitrary = oneof
    [ mapAnnTip TipInfoByron   <$> arbitrary @(AnnTip (ByronBlock))
    , mapAnnTip TipInfoShelley <$> arbitrary @(AnnTip (ShelleyBlock (ShelleyEra c)))
    , mapAnnTip TipInfoAllegra <$> arbitrary @(AnnTip (ShelleyBlock (AllegraEra c)))
    , mapAnnTip TipInfoMary    <$> arbitrary @(AnnTip (ShelleyBlock (MaryEra    c)))
    ]

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

instance CardanoHardForkConstraints c
      => Arbitrary (HardForkNodeToNodeVersion (CardanoEras c)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToNodeVersions (Proxy @(CardanoBlock c))

instance Arbitrary (BlockNodeToNodeVersion blk)
     => Arbitrary (EraNodeToNodeVersion blk) where
  arbitrary = frequency
    [ (1, pure EraNodeToNodeDisabled)
    , (9, EraNodeToNodeEnabled <$> arbitrary)
    ]

arbitraryNodeToNode
  :: ( Arbitrary (WithVersion ByronNodeToNodeVersion byron)
     , Arbitrary shelley
     , Arbitrary allegra
     , Arbitrary mary
     )
  => (byron   -> cardano)
  -> (shelley -> cardano)
  -> (allegra -> cardano)
  -> (mary    -> cardano)
  -> Gen (WithVersion (HardForkNodeToNodeVersion (CardanoEras c)) cardano)
arbitraryNodeToNode injByron injShelley injAllegra injMary = oneof
    -- Byron + HardFork disabled
    [ (\(WithVersion versionByron b) ->
          WithVersion
            (HardForkNodeToNodeDisabled versionByron)
            (injByron b))
        <$> arbitrary
    -- Byron + HardFork enabled.
    -- NOTE: Any value generated by the V1 generator is also fine when using
    -- V2.
    , (\(WithVersion versionByron b) versionShelley versionAllegra versionMary ->
          WithVersion
            (HardForkNodeToNodeEnabled (
                 EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* versionAllegra
              :* versionMary
              :* Nil))
            (injByron b))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Shelley + HardFork enabled
    , (\versionByron (WithVersion versionShelley s) versionAllegra versionMary ->
          WithVersion
            (HardForkNodeToNodeEnabled (
                 EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* versionAllegra
              :* versionMary
              :* Nil))
            (injShelley s))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Allegra + HardFork enabled
    , (\versionByron versionShelley (WithVersion versionAllegra a) versionMary ->
          WithVersion
            (HardForkNodeToNodeEnabled (
                 EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* EraNodeToNodeEnabled versionAllegra
              :* versionMary
              :* Nil))
            (injAllegra a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Mary + HardFork enabled
    , (\versionByron versionShelley versionAllegra (WithVersion versionMary m) ->
          WithVersion
            (HardForkNodeToNodeEnabled (
                 EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* EraNodeToNodeEnabled versionAllegra
              :* EraNodeToNodeEnabled versionMary
              :* Nil))
            (injMary m))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (SomeBlock (NestedCtxt Header) (CardanoBlock c))) where
  arbitrary = arbitraryNodeToNode injByron injShelley injAllegra injMary
    where
      injByron   = mapSomeNestedCtxt NCZ
      injShelley = mapSomeNestedCtxt (NCS . NCZ)
      injAllegra = mapSomeNestedCtxt (NCS . NCS . NCZ)
      injMary    = mapSomeNestedCtxt (NCS . NCS . NCS . NCZ)

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (CardanoBlock c)) where
  arbitrary = arbitraryNodeToNode BlockByron BlockShelley BlockAllegra BlockMary

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (CardanoHeader c)) where
  arbitrary = arbitraryNodeToNode HeaderByron HeaderShelley HeaderAllegra HeaderMary

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (CardanoGenTx c)) where
  arbitrary = arbitraryNodeToNode GenTxByron GenTxShelley GenTxAllegra GenTxMary

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (CardanoGenTxId c)) where
  arbitrary = arbitraryNodeToNode GenTxIdByron GenTxIdShelley GenTxIdAllegra GenTxIdMary

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}

instance CardanoHardForkConstraints c
      => Arbitrary (HardForkNodeToClientVersion (CardanoEras c)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToClientVersions (Proxy @(CardanoBlock c))

newtype HardForkEnabledNodeToClientVersion c = HardForkEnabledNodeToClientVersion {
      getHardForkEnabledNodeToClientVersion :: HardForkNodeToClientVersion (CardanoEras c)
    }

deriving newtype instance CardanoHardForkConstraints c
                       => Eq (HardForkEnabledNodeToClientVersion c)
deriving newtype instance CardanoHardForkConstraints c
                       => Show (HardForkEnabledNodeToClientVersion c)

instance CardanoHardForkConstraints c
      => Arbitrary (HardForkEnabledNodeToClientVersion c) where
  arbitrary =
        elements
      . map HardForkEnabledNodeToClientVersion
      . filter isHardForkNodeToClientEnabled
      . Map.elems
      . supportedNodeToClientVersions
      $ Proxy @(CardanoBlock c)

instance Arbitrary (BlockNodeToClientVersion blk)
     => Arbitrary (EraNodeToClientVersion blk) where
  arbitrary = frequency
    [ (1, pure EraNodeToClientDisabled)
    , (9, EraNodeToClientEnabled <$> arbitrary)
    ]

arbitraryNodeToClient
  :: ( Arbitrary (WithVersion ByronNodeToClientVersion byron)
     , Arbitrary shelley
     , Arbitrary allegra
     , Arbitrary mary
     )
  => (byron   -> cardano)
  -> (shelley -> cardano)
  -> (allegra -> cardano)
  -> (mary    -> cardano)
  -> Gen (WithVersion (HardForkNodeToClientVersion (CardanoEras c)) cardano)
arbitraryNodeToClient injByron injShelley injAllegra injMary = oneof
    -- Byron + HardFork disabled
    [ (\(WithVersion versionByron b) ->
          WithVersion
            (HardForkNodeToClientDisabled versionByron)
            (injByron b))
        <$> arbitrary
    -- Byron + HardFork enabled.
    , (\(WithVersion versionByron b) versionShelley versionAllegra versionMary ->
          WithVersion
            (HardForkNodeToClientEnabled (
                 EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* versionAllegra
              :* versionMary
              :* Nil))
            (injByron b))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Shelley + HardFork enabled
    , (\versionByron (WithVersion versionShelley s) versionAllegra versionMary ->
          WithVersion
            (HardForkNodeToClientEnabled (
                 EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* versionAllegra
              :* versionMary
              :* Nil))
            (injShelley s))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Allegra + HardFork enabled
    , (\versionByron versionShelley (WithVersion versionAllegra a) versionMary ->
          WithVersion
            (HardForkNodeToClientEnabled (
                 EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* EraNodeToClientEnabled versionAllegra
              :* versionMary
              :* Nil))
            (injAllegra a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Mary + HardFork enabled
    , (\versionByron versionShelley versionAllegra (WithVersion versionMary m) ->
          WithVersion
            (HardForkNodeToClientEnabled (
                 EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* EraNodeToClientEnabled versionAllegra
              :* EraNodeToClientEnabled versionMary
              :* Nil))
            (injMary m))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c))
                                (CardanoBlock c)) where
  arbitrary = arbitraryNodeToClient BlockByron BlockShelley BlockAllegra BlockMary

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c))
                                (CardanoGenTx c)) where
  arbitrary = arbitraryNodeToClient GenTxByron GenTxShelley GenTxAllegra GenTxMary

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c))
                                (CardanoApplyTxErr c)) where
  arbitrary = frequency
      [ (8, arbitraryNodeToClient ApplyTxErrByron ApplyTxErrShelley ApplyTxErrAllegra ApplyTxErrMary)
      , (2, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (HardForkApplyTxErrWrongEra <$> arbitrary))
      ]

instance Arbitrary (Some QueryAnytime) where
  arbitrary = return $ Some GetEraStart

instance Arbitrary (Some (QueryHardFork (CardanoEras c))) where
  arbitrary = return $ Some GetInterpreter

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c))
                                (SomeBlock Query (CardanoBlock c))) where
  arbitrary = frequency
      [ (1, arbitraryNodeToClient injByron injShelley injAllegra injMary)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeByron <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeShelley <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeAllegra <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeMary <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injHardFork <$> arbitrary))
      ]
    where
      injByron          (SomeBlock query) = SomeBlock (QueryIfCurrentByron   query)
      injShelley        (SomeBlock query) = SomeBlock (QueryIfCurrentShelley query)
      injAllegra        (SomeBlock query) = SomeBlock (QueryIfCurrentAllegra query)
      injMary           (SomeBlock query) = SomeBlock (QueryIfCurrentMary    query)
      injAnytimeByron   (Some      query) = SomeBlock (QueryAnytimeByron     query)
      injAnytimeShelley (Some      query) = SomeBlock (QueryAnytimeShelley   query)
      injAnytimeAllegra (Some      query) = SomeBlock (QueryAnytimeAllegra   query)
      injAnytimeMary    (Some      query) = SomeBlock (QueryAnytimeMary      query)
      injHardFork       (Some      query) = SomeBlock (QueryHardFork         query)

instance Arbitrary History.EraEnd where
  arbitrary = oneof
      [ History.EraEnd <$> arbitrary
      , return History.EraUnbounded
      ]

instance Arbitrary History.SafeBeforeEpoch where
  arbitrary = oneof
      [ return History.NoLowerBound
      , History.LowerBound . EpochNo <$> arbitrary
      ]

instance Arbitrary History.SafeZone where
  arbitrary = oneof
      [ History.StandardSafeZone <$> arbitrary <*> arbitrary
      , return History.UnsafeIndefiniteSafeZone
      ]

instance Arbitrary History.EraParams where
  arbitrary = History.EraParams
      <$> (EpochSize <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

instance Arbitrary History.EraSummary where
  arbitrary = History.EraSummary
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Arbitrary a, SListI xs) => Arbitrary (NonEmpty xs a) where
  arbitrary = do
      let nbXs = lengthSList (Proxy @xs)
      len <- choose (1, nbXs)
      xs  <- vectorOf len arbitrary
      return $ fromMaybe (error "nonEmptyFromList failed") $ nonEmptyFromList xs

instance Arbitrary (History.Interpreter (CardanoEras c)) where
  arbitrary = History.mkInterpreter . History.Summary <$> arbitrary

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c))
                                (SomeResult (CardanoBlock c))) where
  arbitrary = frequency
      [ (1, arbitraryNodeToClient injByron injShelley injAllegra injMary)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryIfCurrentResultEraMismatch)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultByron)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultShelley)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultAllegra)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultMary)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryHardForkResult)
      ]
    where
      injByron   (SomeResult q r) = SomeResult (QueryIfCurrentByron   q) (QueryResultSuccess r)
      injShelley (SomeResult q r) = SomeResult (QueryIfCurrentShelley q) (QueryResultSuccess r)
      injAllegra (SomeResult q r) = SomeResult (QueryIfCurrentAllegra q) (QueryResultSuccess r)
      injMary    (SomeResult q r) = SomeResult (QueryIfCurrentMary    q) (QueryResultSuccess r)

      -- In practice, when sending a Byron query you'll never get a mismatch
      -- saying that your query is from the Shelley era while the ledger is
      -- from Byron. Only the inverse. We ignore that in this generator, as it
      -- doesn't matter for serialisation purposes, we just generate a random
      -- 'MismatchEraInfo'.
      genQueryIfCurrentResultEraMismatch :: Gen (SomeResult (CardanoBlock c))
      genQueryIfCurrentResultEraMismatch = oneof
          [ (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentByron q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentShelley q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentAllegra q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentMary q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          ]

      genQueryAnytimeResultByron :: Gen (SomeResult (CardanoBlock c))
      genQueryAnytimeResultByron =
          SomeResult (QueryAnytimeByron GetEraStart) <$> arbitrary

      genQueryAnytimeResultShelley :: Gen (SomeResult (CardanoBlock c))
      genQueryAnytimeResultShelley =
          SomeResult (QueryAnytimeShelley GetEraStart) <$> arbitrary

      genQueryAnytimeResultAllegra :: Gen (SomeResult (CardanoBlock c))
      genQueryAnytimeResultAllegra =
          SomeResult (QueryAnytimeAllegra GetEraStart) <$> arbitrary

      genQueryAnytimeResultMary :: Gen (SomeResult (CardanoBlock c))
      genQueryAnytimeResultMary =
          SomeResult (QueryAnytimeMary GetEraStart) <$> arbitrary

      genQueryHardForkResult :: Gen (SomeResult (CardanoBlock c))
      genQueryHardForkResult =
          SomeResult (QueryHardFork GetInterpreter) <$> arbitrary

instance c ~ MockCryptoCompatByron
      => Arbitrary (MismatchEraInfo (CardanoEras c)) where
  arbitrary = MismatchEraInfo <$> elements
      [ ML eraInfoByron       (Z (LedgerEraInfo eraInfoShelley))
      , ML eraInfoByron    (S (Z (LedgerEraInfo eraInfoAllegra)))
      , ML eraInfoByron (S (S (Z (LedgerEraInfo eraInfoMary))))

      , MR (Z  eraInfoShelley)      (LedgerEraInfo eraInfoByron)
      , MS (ML eraInfoShelley    (Z (LedgerEraInfo eraInfoAllegra)))
      , MS (ML eraInfoShelley (S (Z (LedgerEraInfo eraInfoMary))))

      , MR (S  (Z  eraInfoAllegra))  (LedgerEraInfo eraInfoByron)
      , MS (MR (Z  eraInfoAllegra)   (LedgerEraInfo eraInfoShelley))
      , MS (MS (ML eraInfoAllegra (Z (LedgerEraInfo eraInfoMary))))

      , MR (S  (S  (Z eraInfoMary))) (LedgerEraInfo eraInfoByron)
      , MS (MR (S  (Z eraInfoMary))  (LedgerEraInfo eraInfoShelley))
      , MS (MS (MR (Z eraInfoMary)   (LedgerEraInfo eraInfoAllegra)))
      ]
    where
      eraInfoByron   = singleEraInfo (Proxy @ByronBlock)
      eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock (ShelleyEra c)))
      eraInfoAllegra = singleEraInfo (Proxy @(ShelleyBlock (AllegraEra c)))
      eraInfoMary    = singleEraInfo (Proxy @(ShelleyBlock (MaryEra    c)))
