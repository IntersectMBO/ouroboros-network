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
  , toTelescope
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
arbitraryHardForkState p = toTelescope p <$> oneof
    [ Left  <$> genCurrent (Proxy @ByronBlock)
    , Right <$> ((,) <$> genPast
                     <*> genCurrent (Proxy @(ShelleyBlock (ShelleyEra c))))
    ]
  where
    genCurrent
      :: forall blk. Arbitrary (f blk)
      => Proxy blk
      -> Gen (Current f blk)
    genCurrent _ = Current <$> arbitrary <*> (arbitrary @(f blk))

    genPast :: Gen Past
    genPast = Past <$> arbitrary <*> arbitrary

instance (c ~ MockCryptoCompatByron, Era (ShelleyEra c))
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
    ]

instance (c ~ MockCryptoCompatByron, Era (ShelleyEra c))
      => Arbitrary (AnnTip (CardanoBlock c)) where
  arbitrary = oneof
    [ mapAnnTip TipInfoByron   <$> arbitrary @(AnnTip (ByronBlock))
    , mapAnnTip TipInfoShelley <$> arbitrary @(AnnTip (ShelleyBlock (ShelleyEra c)))
    ]

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

instance CardanoHardForkConstraints c
      => Arbitrary (HardForkNodeToNodeVersion (CardanoEras c)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToNodeVersions (Proxy @(CardanoBlock c))

arbitraryNodeToNode
  :: ( Arbitrary (WithVersion ByronNodeToNodeVersion byron)
     , Arbitrary shelley
     )
  => (byron   -> cardano)
  -> (shelley -> cardano)
  -> Gen (WithVersion (HardForkNodeToNodeVersion (CardanoEras c)) cardano)
arbitraryNodeToNode injByron injShelley = oneof
    -- Byron + HardFork disabled
    [ (\(WithVersion versionByron b) ->
          WithVersion
            (HardForkNodeToNodeDisabled versionByron)
            (injByron b))
        <$> arbitrary
    -- Byron + HardFork enabled.
    -- NOTE: Any value generated by the V1 generator is also fine when using
    -- V2.
    , (\(WithVersion versionByron b) versionShelley ->
          WithVersion
            (HardForkNodeToNodeEnabled (
                 EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* Nil))
            (injByron b))
        <$> arbitrary <*> arbitrary
    -- Shelley + HardFork enabled
    , (\versionByron (WithVersion versionShelley s) ->
          WithVersion
            (HardForkNodeToNodeEnabled (
                 EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* Nil))
            (injShelley s))
        <$> arbitrary <*> arbitrary
    ]

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (SomeBlock (NestedCtxt Header) (CardanoBlock c))) where
  arbitrary = arbitraryNodeToNode injByron injShelley
    where
      injByron   = mapSomeNestedCtxt NCZ
      injShelley = mapSomeNestedCtxt (NCS . NCZ)

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (CardanoBlock c)) where
  arbitrary = arbitraryNodeToNode BlockByron BlockShelley

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (CardanoHeader c)) where
  arbitrary = arbitraryNodeToNode HeaderByron HeaderShelley

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (CardanoGenTx c)) where
  arbitrary = arbitraryNodeToNode GenTxByron GenTxShelley

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c))
                                (CardanoGenTxId c)) where
  arbitrary = arbitraryNodeToNode GenTxIdByron GenTxIdShelley

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

arbitraryNodeToClient
  :: ( Arbitrary (WithVersion ByronNodeToClientVersion byron)
     , Arbitrary shelley
     )
  => (byron   -> cardano)
  -> (shelley -> cardano)
  -> Gen (WithVersion (HardForkNodeToClientVersion (CardanoEras c)) cardano)
arbitraryNodeToClient injByron injShelley = oneof
    -- Byron + HardFork disabled
    [ (\(WithVersion versionByron b) ->
          WithVersion
            (HardForkNodeToClientDisabled versionByron)
            (injByron b))
        <$> arbitrary
    -- Byron + HardFork enabled.
    , (\(WithVersion versionByron b) versionShelley ->
          WithVersion
            (HardForkNodeToClientEnabled (
                 EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* Nil))
            (injByron b))
        <$> arbitrary <*> arbitrary
    -- Shelley + HardFork enabled
    , (\versionByron (WithVersion versionShelley s) ->
          WithVersion
            (HardForkNodeToClientEnabled (
                 EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* Nil))
            (injShelley s))
        <$> arbitrary <*> arbitrary
    ]

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c))
                                (CardanoBlock c)) where
  arbitrary = arbitraryNodeToClient BlockByron BlockShelley

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c))
                                (CardanoGenTx c)) where
  arbitrary = arbitraryNodeToClient GenTxByron GenTxShelley

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c))
                                (CardanoApplyTxErr c)) where
  arbitrary = frequency
      [ (8, arbitraryNodeToClient ApplyTxErrByron ApplyTxErrShelley)
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
      [ (1, arbitraryNodeToClient injByron injShelley)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeByron <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeShelley <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injHardFork <$> arbitrary))
      ]
    where
      injByron          (SomeBlock query) = SomeBlock (QueryIfCurrentByron   query)
      injShelley        (SomeBlock query) = SomeBlock (QueryIfCurrentShelley query)
      injAnytimeByron   (Some      query) = SomeBlock (QueryAnytimeByron     query)
      injAnytimeShelley (Some      query) = SomeBlock (QueryAnytimeShelley   query)
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
      [ (1, arbitraryNodeToClient injByron injShelley)
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
              <*> genQueryHardForkResult)
      ]
    where
      injByron   (SomeResult q r) = SomeResult (QueryIfCurrentByron   q) (QueryResultSuccess r)
      injShelley (SomeResult q r) = SomeResult (QueryIfCurrentShelley q) (QueryResultSuccess r)

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
          ]

      genQueryAnytimeResultByron :: Gen (SomeResult (CardanoBlock c))
      genQueryAnytimeResultByron =
          SomeResult (QueryAnytimeByron GetEraStart) <$> arbitrary

      genQueryAnytimeResultShelley :: Gen (SomeResult (CardanoBlock c))
      genQueryAnytimeResultShelley =
          SomeResult (QueryAnytimeShelley GetEraStart) <$> arbitrary

      genQueryHardForkResult :: Gen (SomeResult (CardanoBlock c))
      genQueryHardForkResult =
          SomeResult (QueryHardFork GetInterpreter) <$> arbitrary

instance c ~ MockCryptoCompatByron
      => Arbitrary (MismatchEraInfo (CardanoEras c)) where
  arbitrary = MismatchEraInfo <$> elements
      [ ML eraInfoByron (Z (LedgerEraInfo eraInfoShelley))
      , MR (Z eraInfoShelley) (LedgerEraInfo eraInfoByron)
      ]
    where
      eraInfoByron   = singleEraInfo (Proxy @ByronBlock)
      eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock (ShelleyEra c)))

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

toTelescope :: Coercible a (HardForkState f '[x, y])
            => proxy f
            -> Either (Current f x) (Past, Current f y) -> a
toTelescope _ = coerce . HardForkState . aux
  where
    aux :: Either (Current f x) (Past, Current f y)
        -> Telescope (K Past) (Current f) '[x, y]
    aux (Left fx)        = TZ fx
    aux (Right (gx, fy)) = TS (K gx) (TZ fy)
