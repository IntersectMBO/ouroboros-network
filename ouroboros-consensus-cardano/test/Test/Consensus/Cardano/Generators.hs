{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Generators (
    module Test.Consensus.Byron.Generators
  ) where

import           Data.Coerce
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import           Data.SOP.Strict (NP (..), NS (..))

import           Test.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (RelativeTime (..))
import           Ouroboros.Consensus.HardFork.History (Bound (..))
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

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
import           Test.Util.Serialisation (WithVersion (..))

import           Test.Consensus.Byron.Generators

import           Test.Consensus.Shelley.Generators
import           Test.Consensus.Shelley.MockCrypto

{-------------------------------------------------------------------------------
  Disk
-------------------------------------------------------------------------------}

instance Arbitrary (CardanoBlock TPraosMockCrypto) where
  arbitrary = oneof
    [ BlockByron   <$> arbitrary
    , BlockShelley <$> arbitrary
    ]

instance Arbitrary (CardanoHeader TPraosMockCrypto) where
  arbitrary = getHeader <$> arbitrary

instance Arbitrary Bound where
  arbitrary = Bound <$> (RelativeTime <$> arbitrary) <*> arbitrary <*> arbitrary

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

instance sc ~ TPraosMockCrypto
      => Arbitrary (CardanoLedgerState sc) where
  arbitrary = arbitraryHardForkState (Proxy @LedgerState)

instance sc ~ TPraosMockCrypto
      => Arbitrary (HardForkConsensusState (CardanoEras sc)) where
  arbitrary = arbitraryHardForkState (Proxy @WrapConsensusState)

-- | Forwarding
instance Arbitrary (ConsensusState (BlockProtocol blk))
      => Arbitrary (WrapConsensusState blk) where
  arbitrary = WrapConsensusState <$> arbitrary

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

instance Arbitrary (AnnTip (CardanoBlock TPraosMockCrypto)) where
  arbitrary = oneof
    [ mapAnnTip TipInfoByron   <$> arbitrary @(AnnTip (ByronBlock))
    , mapAnnTip TipInfoShelley <$> arbitrary @(AnnTip (ShelleyBlock TPraosMockCrypto))
    ]

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

instance TPraosCrypto sc => Arbitrary (HardForkNodeToNodeVersion (CardanoEras sc)) where
  arbitrary =
    elements $ NE.toList $ supportedNodeToNodeVersions (Proxy @(CardanoBlock sc))

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
            (HardForkNodeToNodeDisabled (WrapNodeToNodeVersion versionByron))
            (injByron b))
        <$> arbitrary
    -- Byron + HardFork enabled.
    -- NOTE: Any value generated by the V1 generator is also fine when using
    -- V2.
    , (\(WithVersion versionByron b) versionShelley ->
          WithVersion
            (HardForkNodeToNodeEnabled (
                 WrapNodeToNodeVersion versionByron
              :* WrapNodeToNodeVersion versionShelley
              :* Nil))
            (injByron b))
        <$> arbitrary <*> arbitrary
    -- Shelley + HardFork enabled
    , (\versionByron (WithVersion versionShelley s) ->
          WithVersion
            (HardForkNodeToNodeEnabled (
                 WrapNodeToNodeVersion versionByron
              :* WrapNodeToNodeVersion versionShelley
              :* Nil))
            (injShelley s))
        <$> arbitrary <*> arbitrary
    ]


instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (SomeBlock (NestedCtxt Header) (CardanoBlock sc))) where
  arbitrary = arbitraryNodeToNode injByron injShelley
    where
      injByron   = mapSomeNestedCtxt NCZ
      injShelley = mapSomeNestedCtxt (NCS . NCZ)

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (CardanoBlock sc)) where
  arbitrary = arbitraryNodeToNode BlockByron BlockShelley

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (CardanoHeader sc)) where
  arbitrary = arbitraryNodeToNode HeaderByron HeaderShelley

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (CardanoGenTx sc)) where
  arbitrary = arbitraryNodeToNode GenTxByron GenTxShelley

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras sc))
                                (CardanoGenTxId sc)) where
  arbitrary = arbitraryNodeToNode GenTxIdByron GenTxIdShelley

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}


instance TPraosCrypto sc => Arbitrary (HardForkNodeToClientVersion (CardanoEras sc)) where
  arbitrary =
    elements $ NE.toList $ supportedNodeToClientVersions (Proxy @(CardanoBlock sc))

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
            (HardForkNodeToClientDisabled (WrapNodeToClientVersion versionByron))
            (injByron b))
        <$> arbitrary
    -- Byron + HardFork enabled.
    , (\(WithVersion versionByron b) versionShelley ->
          WithVersion
            (HardForkNodeToClientEnabled (
                 WrapNodeToClientVersion versionByron
              :* WrapNodeToClientVersion versionShelley
              :* Nil))
            (injByron b))
        <$> arbitrary <*> arbitrary
    -- Shelley + HardFork enabled
    , (\versionByron (WithVersion versionShelley s) ->
          WithVersion
            (HardForkNodeToClientEnabled (
                 WrapNodeToClientVersion versionByron
              :* WrapNodeToClientVersion versionShelley
              :* Nil))
            (injShelley s))
        <$> arbitrary <*> arbitrary
    ]

pc :: Proxy (CardanoBlock sc)
pc = Proxy

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (CardanoBlock sc)) where
  arbitrary = arbitraryNodeToClient BlockByron BlockShelley

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (CardanoGenTx sc)) where
  arbitrary = arbitraryNodeToClient GenTxByron GenTxShelley

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (CardanoApplyTxErr sc)) where
  arbitrary = frequency
      [ (8, arbitraryNodeToClient ApplyTxErrByron ApplyTxErrShelley)
      , (2, WithVersion (mostRecentSupportedNodeToClient pc) . HardForkApplyTxErrWrongEra
              <$> arbitrary)
      ]

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (SomeBlock Query (CardanoBlock sc))) where
  arbitrary = arbitraryNodeToClient injByron injShelley
    where
      injByron   (SomeBlock query) = SomeBlock (QueryByron   query)
      injShelley (SomeBlock query) = SomeBlock (QueryShelley query)

instance sc ~ TPraosMockCrypto
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras sc))
                                (SomeResult (CardanoBlock sc))) where
  arbitrary = frequency
      [ (8, arbitraryNodeToClient injByron injShelley)
      , (2, WithVersion (mostRecentSupportedNodeToClient pc) <$> genQueryResultEraMismatch)
      ]
    where
      injByron   (SomeResult q r) = SomeResult (QueryByron   q) (QueryResultSuccess r)
      injShelley (SomeResult q r) = SomeResult (QueryShelley q) (QueryResultSuccess r)

      -- In practice, when sending a Byron query you'll never get a mismatch
      -- saying that your query is from the Shelley era while the ledger is
      -- from Byron. Only the inverse. We ignore that in this generator, as it
      -- doesn't matter for serialisation purposes, we just generate a random
      -- 'MismatchEraInfo'.
      genQueryResultEraMismatch :: Gen (SomeResult (CardanoBlock sc))
      genQueryResultEraMismatch = oneof
          [ (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryByron q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryShelley q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          ]

instance sc ~ TPraosMockCrypto
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
