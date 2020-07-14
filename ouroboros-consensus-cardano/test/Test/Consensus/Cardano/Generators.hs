{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Generators (
    module Test.Consensus.Byron.Generators
  , toTelescope
  , toTelescope'
  ) where

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)
import           Data.Coerce
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.SOP.Strict (NP (..), NS (..), SListI, lengthSList)

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
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCrypto)

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Cardano.Node ()

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (WithVersion (..))

import           Test.Consensus.Byron.Generators

import           Test.Consensus.Shelley.Generators
import           Test.Consensus.Shelley.MockCrypto

{-------------------------------------------------------------------------------
  Disk
-------------------------------------------------------------------------------}

instance HashAlgorithm h => Arbitrary (CardanoBlock (TPraosMockCrypto h)) where
  arbitrary = oneof
    [ BlockByron   <$> arbitrary
    , BlockShelley <$> arbitrary
    ]

instance HashAlgorithm h => Arbitrary (CardanoHeader (TPraosMockCrypto h)) where
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
  :: forall f sc a.
     ( Arbitrary (f ByronBlock)
     , Arbitrary (f (ShelleyBlock sc))
     , Coercible a (HardForkState_ f f (CardanoEras sc))
     )
  => Proxy f
  -> Gen a
arbitraryHardForkState p = toTelescope' p <$> oneof
    [ Left  <$> genCurrent (Proxy @ByronBlock)
    , Right <$> ((,) <$> genPast    (Proxy @ByronBlock)
                     <*> genCurrent (Proxy @(ShelleyBlock sc)))
    ]
  where
    genCurrent
      :: forall blk. Arbitrary (f blk)
      => Proxy blk
      -> Gen (Current f blk)
    genCurrent _ = Current <$> arbitrary <*> (arbitrary @(f blk))

    genPast
      :: forall blk. Arbitrary (f blk)
      => Proxy blk
      -> Gen (Past f blk)
    genPast _ = Past
      <$> arbitrary
      <*> arbitrary
      <*> oneof
            [ Snapshot <$> arbitrary <*> arbitrary @(f blk)
            , return NoSnapshot
            ]

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h, forall a. Arbitrary (Hash h a))
      => Arbitrary (CardanoLedgerState sc) where
  arbitrary = arbitraryHardForkState (Proxy @LedgerState)

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (HardForkChainDepState (CardanoEras sc)) where
  arbitrary = arbitraryHardForkState (Proxy @WrapChainDepState)

-- | Forwarding
instance Arbitrary (ChainDepState (BlockProtocol blk))
      => Arbitrary (WrapChainDepState blk) where
  arbitrary = WrapChainDepState <$> arbitrary

-- | NOTE: Byron hashes are always 32 bytes, but for Shelley it depends on the
-- crypto: with 'TPraosStandardCrypto' the hash is also 32 bytes, but with
-- 'TPraosMockCrypto' it is 4 bytes ('ShortHash'). As this generator is only
-- used in the roundtrip tests, where we don't care about the length of the
-- hash, this difference is fine.
--
-- When we would store them on disk (specifically, in the secondary index of
-- the ImmutableDB) and try to read them again, this would go wrong, as we
-- require all hashes to have the same length.
instance Crypto sc => Arbitrary (OneEraHash (CardanoEras sc)) where
  arbitrary = OneEraHash <$> oneof
    [ toRawHash (Proxy @ByronBlock) <$> arbitrary
    , toRawHash (Proxy @(ShelleyBlock sc)) <$> arbitrary
    ]

instance HashAlgorithm h => Arbitrary (AnnTip (CardanoBlock (TPraosMockCrypto h))) where
  arbitrary = oneof
    [ mapAnnTip TipInfoByron   <$> arbitrary @(AnnTip (ByronBlock))
    , mapAnnTip TipInfoShelley <$> arbitrary @(AnnTip (ShelleyBlock (TPraosMockCrypto h)))
    ]

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

instance TPraosCrypto sc => Arbitrary (HardForkNodeToNodeVersion (CardanoEras sc)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToNodeVersions (Proxy @(CardanoBlock sc))

arbitraryNodeToNode
  :: ( Arbitrary (WithVersion ByronNodeToNodeVersion byron)
     , Arbitrary shelley
     )
  => (byron   -> cardano)
  -> (shelley -> cardano)
  -> Gen (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc)) cardano)
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


instance sc ~ TPraosMockCrypto h
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (SomeBlock (NestedCtxt Header) (CardanoBlock sc))) where
  arbitrary = arbitraryNodeToNode injByron injShelley
    where
      injByron   = mapSomeNestedCtxt NCZ
      injShelley = mapSomeNestedCtxt (NCS . NCZ)

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (CardanoBlock sc)) where
  arbitrary = arbitraryNodeToNode BlockByron BlockShelley

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (CardanoHeader sc)) where
  arbitrary = arbitraryNodeToNode HeaderByron HeaderShelley

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (CardanoGenTx sc)) where
  arbitrary = arbitraryNodeToNode GenTxByron GenTxShelley

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (CardanoGenTxId sc)) where
  arbitrary = arbitraryNodeToNode GenTxIdByron GenTxIdShelley

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}

instance TPraosCrypto sc => Arbitrary (HardForkNodeToClientVersion (CardanoEras sc)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToClientVersions (Proxy @(CardanoBlock sc))

newtype HardForkEnabledNodeToClientVersion sc = HardForkEnabledNodeToClientVersion {
      getHardForkEnabledNodeToClientVersion :: HardForkNodeToClientVersion (CardanoEras sc)
    }
  deriving newtype (Eq, Show)

instance TPraosCrypto sc => Arbitrary (HardForkEnabledNodeToClientVersion sc) where
  arbitrary =
        elements
      . map HardForkEnabledNodeToClientVersion
      . filter isHardForkNodeToClientEnabled
      . Map.elems
      . supportedNodeToClientVersions
      $ Proxy @(CardanoBlock sc)

arbitraryNodeToClient
  :: ( Arbitrary (WithVersion ByronNodeToClientVersion byron)
     , Arbitrary shelley
     )
  => (byron   -> cardano)
  -> (shelley -> cardano)
  -> Gen (WithVersion (HardForkNodeToClientVersion (CardanoEras sc)) cardano)
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

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (CardanoBlock sc)) where
  arbitrary = arbitraryNodeToClient BlockByron BlockShelley

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (CardanoGenTx sc)) where
  arbitrary = arbitraryNodeToClient GenTxByron GenTxShelley

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h, forall a. Arbitrary (Hash h a))
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (CardanoApplyTxErr sc)) where
  arbitrary = frequency
      [ (8, arbitraryNodeToClient ApplyTxErrByron ApplyTxErrShelley)
      , (2, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (HardForkApplyTxErrWrongEra <$> arbitrary))
      ]

instance Arbitrary (Some QueryAnytime) where
  arbitrary = return $ Some GetEraStart

instance Arbitrary (Some (QueryHardFork (CardanoEras sc))) where
  arbitrary = return $ Some GetInterpreter

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (SomeBlock Query (CardanoBlock sc))) where
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

instance Arbitrary (History.Interpreter (CardanoEras sc)) where
  arbitrary = History.mkInterpreter . History.Summary <$> arbitrary

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (SomeResult (CardanoBlock sc))) where
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
      genQueryIfCurrentResultEraMismatch :: Gen (SomeResult (CardanoBlock sc))
      genQueryIfCurrentResultEraMismatch = oneof
          [ (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentByron q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentShelley q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          ]

      genQueryAnytimeResultByron :: Gen (SomeResult (CardanoBlock sc))
      genQueryAnytimeResultByron =
          SomeResult (QueryAnytimeByron GetEraStart) <$> arbitrary

      genQueryAnytimeResultShelley :: Gen (SomeResult (CardanoBlock sc))
      genQueryAnytimeResultShelley =
          SomeResult (QueryAnytimeShelley GetEraStart) <$> arbitrary

      genQueryHardForkResult :: Gen (SomeResult (CardanoBlock sc))
      genQueryHardForkResult =
          SomeResult (QueryHardFork GetInterpreter) <$> arbitrary

instance (sc ~ TPraosMockCrypto h, HashAlgorithm h)
      => Arbitrary (MismatchEraInfo (CardanoEras sc)) where
  arbitrary = MismatchEraInfo <$> elements
      [ ML eraInfoByron (Z (LedgerEraInfo eraInfoShelley))
      , MR (Z eraInfoShelley) (LedgerEraInfo eraInfoByron)
      ]
    where
      eraInfoByron   = singleEraInfo (Proxy @ByronBlock)
      eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock sc))

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

toTelescope :: Coercible a (HardForkState_ g f '[x, y])
            => proxy g
            -> proxy f
            -> Either (Current f x) (Past g x, Current f y) -> a
toTelescope _ _ = coerce . HardForkState . aux
  where
    aux :: Either (Current f x) (Past g x, Current f y)
        -> Telescope (Past g) (Current f) '[x, y]
    aux (Left fx)        = TZ fx
    aux (Right (gx, fy)) = TS gx (TZ fy)

toTelescope' :: Coercible a (HardForkState f '[x, y])
             => proxy f
             -> Either (Current f x) (Past f x, Current f y) -> a
toTelescope' p = toTelescope p p
