{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
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
-- | 'Arbitrary' instances intended for serialisation roundtrip tests for
-- 'CardanoBlock' and its related types.
--
-- Because the generated values are only used in serialisation roundtrip tests,
-- they don't need to be valid blocks, transactions, etc.
--
-- We combine the Byron and Shelley-based instances defined elsewhere into
-- Cardano instances by picking randomly from one of the eras.
module Test.Consensus.Cardano.Generators (module Test.Consensus.Byron.Generators) where

import qualified Cardano.Crypto.DSIGN as DSIGN
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import qualified Cardano.Crypto.KES as KES
import           Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Ledger.BaseTypes as SL
import           Cardano.Ledger.Crypto (Crypto (..))
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Proxy
import           Data.SOP.Counting
import           Data.SOP.Index
import           Data.SOP.Strict
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node (CardanoHardForkConstraints)
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Serialisation (Some (..))
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Protocol.TPraos (PraosCrypto, TPraos)
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Block ()
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import           Test.Cardano.Ledger.Conway.Serialisation.Generators ()
import           Test.Consensus.Byron.Generators
import           Test.Consensus.Cardano.MockCrypto
import           Test.Consensus.Protocol.Serialisation.Generators ()
import           Test.Consensus.Shelley.Generators (SomeResult (..))
import           Test.Consensus.Shelley.MockCrypto (CanMock)
import           Test.QuickCheck
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (Coherent (..),
                     WithVersion (..))

{-------------------------------------------------------------------------------
  Disk
-------------------------------------------------------------------------------}

instance ( DSIGN.Signable (DSIGN c1) ~ SignableRepresentation
         , KES.Signable (KES c1) ~ SignableRepresentation
         , VRF.Signable (VRF c1) SL.Seed
         , PraosCrypto c1
         , DSIGN.Signable (DSIGN c2) ~ SignableRepresentation
         , KES.Signable (KES c2) ~ SignableRepresentation
         , VRF.Signable (VRF c2) SL.Seed
         , PraosCrypto c2
         ) => Arbitrary (CardanoBlock c1 c2) where
  arbitrary =
      oneof $ catMaybes $ hcollapse generators
    where
      generators ::
        NP
          (K (Maybe (Gen (CardanoBlock c1 c2))))
          (CardanoEras c1 c2)
      generators =
            mk BlockByron
         :* mk BlockShelley
         :* mk BlockAllegra
         :* mk BlockMary
         :* mk BlockAlonzo
         :* mk BlockBabbage
         :* mk BlockConway
         :* Nil

      mk ::
           forall a x. Arbitrary a
        => (a -> CardanoBlock c1 c2)
        -> K (Maybe (Gen (CardanoBlock c1 c2))) x
      mk f = K $ Just $ f <$> arbitrary

instance ( DSIGN.Signable (DSIGN c1) ~ SignableRepresentation
         , KES.Signable (KES c1) ~ SignableRepresentation
         , VRF.Signable (VRF c1) SL.Seed
         , PraosCrypto c1
         , DSIGN.Signable (DSIGN c2) ~ SignableRepresentation
         , KES.Signable (KES c2) ~ SignableRepresentation
         , VRF.Signable (VRF c2) SL.Seed
         , PraosCrypto c2
         ) => Arbitrary (Coherent (CardanoBlock c1 c2)) where
  arbitrary =
      fmap Coherent $ oneof $ catMaybes $ hcollapse generators
    where
      generators ::
        NP
          (K (Maybe (Gen (CardanoBlock c1 c2))))
          (CardanoEras c1 c2)
      generators =
            mk BlockByron
         :* mk BlockShelley
         :* mk BlockAllegra
         :* mk BlockMary
         :* mk BlockAlonzo
         :* mk BlockBabbage
         :* mk BlockConway
         :* Nil

      mk ::
           forall a x. Arbitrary (Coherent a)
        => (a -> CardanoBlock c1 c2)
        -> K (Maybe (Gen (CardanoBlock c1 c2))) x
      mk f = K $ Just $ f . getCoherent <$> arbitrary

instance ( DSIGN.Signable (DSIGN c1) ~ SignableRepresentation
         , KES.Signable (KES c1) ~ SignableRepresentation
         , VRF.Signable (VRF c1) SL.Seed
         , HASH c1 ~ Blake2b_256
         , ADDRHASH c1 ~ Blake2b_224
         , DSIGN c1 ~ DSIGN.Ed25519DSIGN
         , PraosCrypto c1
         , DSIGN.Signable (DSIGN c2) ~ SignableRepresentation
         , KES.Signable (KES c2) ~ SignableRepresentation
         , VRF.Signable (VRF c2) SL.Seed
         , HASH c2 ~ Blake2b_256
         , ADDRHASH c2 ~ Blake2b_224
         , DSIGN c2 ~ DSIGN.Ed25519DSIGN
         , PraosCrypto c2
         ) => Arbitrary (CardanoHeader c1 c2) where
 arbitrary =
      oneof $ catMaybes $ hcollapse generators
    where
      generators ::
        NP
          (K (Maybe (Gen (CardanoHeader c1 c2))))
          (CardanoEras c1 c2)
      generators =
            mk HeaderByron
         :* mk HeaderShelley
         :* mk HeaderAllegra
         :* mk HeaderMary
         :* mk HeaderAlonzo
         :* mk HeaderBabbage
         :* mk HeaderConway
         :* Nil

      mk ::
           forall a x. Arbitrary a
        => (a -> CardanoHeader c1 c2)
        -> K (Maybe (Gen (CardanoHeader c1 c2))) x
      mk f = K $ Just $ f <$> arbitrary


instance (CanMock (TPraos c1) (ShelleyEra c1), CardanoHardForkConstraints c1 c2)
      => Arbitrary (OneEraHash (CardanoEras c1 c2)) where
  arbitrary = inj <$> arbitrary
    where
      inj :: NS WrapHeaderHash (CardanoEras c1 c2) -> OneEraHash (CardanoEras c1 c2)
      inj = hcollapse . hcmap proxySingle aux

      aux ::
           forall blk. SingleEraBlock blk
        => WrapHeaderHash blk -> K (OneEraHash (CardanoEras c1 c2)) blk
      aux = K . OneEraHash . toShortRawHash (Proxy @blk) . unwrapHeaderHash

instance (c1 ~ MockCryptoCompatByron, ShelleyBasedEra (ShelleyEra c1), c2 ~ MockCryptoCompatByron, ShelleyBasedEra (ShelleyEra c2))
      => Arbitrary (AnnTip (CardanoBlock c1 c2)) where
  arbitrary = AnnTip
      <$> (SlotNo <$> arbitrary)
      <*> arbitrary
      <*> (OneEraTipInfo <$> arbitrary)

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

instance CardanoHardForkConstraints c1 c2
      => Arbitrary (HardForkNodeToNodeVersion (CardanoEras c1 c2)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToNodeVersions (Proxy @(CardanoBlock c1 c2))

instance Arbitrary (BlockNodeToNodeVersion blk)
     => Arbitrary (EraNodeToNodeVersion blk) where
  arbitrary = frequency
    [ (1, pure EraNodeToNodeDisabled)
    , (9, EraNodeToNodeEnabled <$> arbitrary)
    ]

arbitraryNodeToNode
  :: ( Arbitrary (WithVersion ByronNodeToNodeVersion byron)
     , Arbitrary (WithVersion ShelleyNodeToNodeVersion shelley)
     , Arbitrary (WithVersion ShelleyNodeToNodeVersion allegra)
     , Arbitrary (WithVersion ShelleyNodeToNodeVersion mary)
     , Arbitrary (WithVersion ShelleyNodeToNodeVersion alonzo)
     , Arbitrary (WithVersion ShelleyNodeToNodeVersion babbage)
     , Arbitrary (WithVersion ShelleyNodeToNodeVersion conway)
     )
  => (byron   -> cardano)
  -> (shelley -> cardano)
  -> (allegra -> cardano)
  -> (mary    -> cardano)
  -> (alonzo  -> cardano)
  -> (babbage -> cardano)
  -> (conway  -> cardano)
  -> Gen (WithVersion (HardForkNodeToNodeVersion (CardanoEras c1 c2)) cardano)
arbitraryNodeToNode injByron injShelley injAllegra injMary injAlonzo injBabbage injConway = oneof
    -- Byron + HardFork disabled
    [ (\(WithVersion versionByron b) ->
          WithVersion
            (HardForkNodeToNodeDisabled versionByron)
            (injByron b))
        <$> arbitrary
    -- Byron + HardFork enabled.
    -- NOTE: Any value generated by the V1 generator is also fine when using
    -- V2.
    , (\(WithVersion versionByron b) versionShelley versionAllegra versionMary versionAlonzo versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* versionAllegra
              :* versionMary
              :* versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injByron b))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Shelley + HardFork enable
    , (\versionByron (WithVersion versionShelley s) versionAllegra versionMary versionAlonzo versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* versionAllegra
              :* versionMary
              :* versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injShelley s))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Allegra + HardFork enabled
    , (\versionByron versionShelley (WithVersion versionAllegra a) versionMary versionAlonzo versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* EraNodeToNodeEnabled versionAllegra
              :* versionMary
              :* versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injAllegra a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Mary + HardFork enabled
    , (\versionByron versionShelley versionAllegra (WithVersion versionMary m) versionAlonzo versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* EraNodeToNodeEnabled versionAllegra
              :* EraNodeToNodeEnabled versionMary
              :* versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injMary m))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Alonzo + HardFork enabled
    , (\versionByron versionShelley versionAllegra versionMary (WithVersion versionAlonzo a) versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* EraNodeToNodeEnabled versionAllegra
              :* EraNodeToNodeEnabled versionMary
              :* EraNodeToNodeEnabled versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injAlonzo a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Babbage + HardFork enabled
    , (\versionByron versionShelley versionAllegra versionMary versionAlonzo (WithVersion versionBabbage a) versionConway ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* EraNodeToNodeEnabled versionAllegra
              :* EraNodeToNodeEnabled versionMary
              :* EraNodeToNodeEnabled versionAlonzo
              :* EraNodeToNodeEnabled versionBabbage
              :* versionConway
              :* Nil
              ))
            (injBabbage a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Conway + HardFork enabled
    , (\versionByron versionShelley versionAllegra versionMary versionAlonzo versionBabbage (WithVersion versionConway a) ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionByron
              :* EraNodeToNodeEnabled versionShelley
              :* EraNodeToNodeEnabled versionAllegra
              :* EraNodeToNodeEnabled versionMary
              :* EraNodeToNodeEnabled versionAlonzo
              :* EraNodeToNodeEnabled versionBabbage
              :* EraNodeToNodeEnabled versionConway
              :* Nil
              ))
            (injConway a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c1 c2))
                                (SomeSecond (NestedCtxt Header) (CardanoBlock c1 c2))) where
  arbitrary = arbitraryNodeToNode injByron injShelley injAllegra injMary injAlonzo injBabbage injConway
    where
      injByron   = mapSomeNestedCtxt NCZ
      injShelley = mapSomeNestedCtxt (NCS . NCZ)
      injAllegra = mapSomeNestedCtxt (NCS . NCS . NCZ)
      injMary    = mapSomeNestedCtxt (NCS . NCS . NCS . NCZ)
      injAlonzo  = mapSomeNestedCtxt (NCS . NCS . NCS . NCS . NCZ)
      injBabbage = mapSomeNestedCtxt (NCS . NCS . NCS . NCS . NCS . NCZ)
      injConway  = mapSomeNestedCtxt (NCS . NCS . NCS . NCS . NCS . NCS . NCZ)

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c1 c2))
                                (CardanoBlock c1 c2)) where
  arbitrary = arbitraryNodeToNode BlockByron BlockShelley BlockAllegra BlockMary BlockAlonzo BlockBabbage BlockConway

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c1 c2))
                                (CardanoHeader c1 c2)) where
  arbitrary = arbitraryNodeToNode HeaderByron HeaderShelley HeaderAllegra HeaderMary HeaderAlonzo HeaderBabbage HeaderConway

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c1 c2))
                                (CardanoGenTx c1 c2)) where
  arbitrary = arbitraryNodeToNode GenTxByron GenTxShelley GenTxAllegra GenTxMary GenTxAlonzo GenTxBabbage GenTxConway

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (CardanoEras c1 c2))
                                (CardanoGenTxId c1 c2)) where
  arbitrary = arbitraryNodeToNode GenTxIdByron GenTxIdShelley GenTxIdAllegra GenTxIdMary GenTxIdAlonzo GenTxIdBabbage GenTxIdConway

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}

instance CardanoHardForkConstraints c1 c2
      => Arbitrary (HardForkNodeToClientVersion (CardanoEras c1 c2)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToClientVersions (Proxy @(CardanoBlock c1 c2))

newtype HardForkEnabledNodeToClientVersion c1 c2 = HardForkEnabledNodeToClientVersion {
      getHardForkEnabledNodeToClientVersion :: HardForkNodeToClientVersion (CardanoEras c1 c2)
    }

deriving newtype instance CardanoHardForkConstraints c1 c2
                       => Eq (HardForkEnabledNodeToClientVersion c1 c2)
deriving newtype instance CardanoHardForkConstraints c1 c2
                       => Show (HardForkEnabledNodeToClientVersion c1 c2)

instance CardanoHardForkConstraints c1 c2
      => Arbitrary (HardForkEnabledNodeToClientVersion c1 c2) where
  arbitrary =
        elements
      . map HardForkEnabledNodeToClientVersion
      . filter isHardForkNodeToClientEnabled
      . Map.elems
      . supportedNodeToClientVersions
      $ Proxy @(CardanoBlock c1 c2)

-- | Generate a supported 'HardForkNodeToClientVersion' of which the
-- 'HardForkSpecificNodeToClientVersion' satisfies the given predicate.
--
-- PRECONDITION: 'supportedNodeToClientVersions' must include a version that
-- satisfies this condition.
genWithHardForkSpecificNodeToClientVersion ::
     forall c1 c2. CardanoHardForkConstraints c1 c2
  => (HardForkSpecificNodeToClientVersion -> Bool)
  -> Gen (HardForkNodeToClientVersion (CardanoEras c1 c2))
genWithHardForkSpecificNodeToClientVersion p =
      elements
    . filter p'
    . Map.elems
    . supportedNodeToClientVersions
    $ Proxy @(CardanoBlock c1 c2)
  where
    p' :: HardForkNodeToClientVersion (CardanoEras c1 c2) -> Bool
    p' (HardForkNodeToClientEnabled v _) = p v
    p' HardForkNodeToClientDisabled {} = False

instance Arbitrary (BlockNodeToClientVersion blk)
     => Arbitrary (EraNodeToClientVersion blk) where
  arbitrary = frequency
    [ (1, pure EraNodeToClientDisabled)
    , (9, EraNodeToClientEnabled <$> arbitrary)
    ]

arbitraryNodeToClient
  :: ( Arbitrary (WithVersion ByronNodeToClientVersion   byron)
     , Arbitrary (WithVersion ShelleyNodeToClientVersion shelley)
     , Arbitrary (WithVersion ShelleyNodeToClientVersion allegra)
     , Arbitrary (WithVersion ShelleyNodeToClientVersion mary)
     , Arbitrary (WithVersion ShelleyNodeToClientVersion alonzo)
     , Arbitrary (WithVersion ShelleyNodeToClientVersion babbage)
     , Arbitrary (WithVersion ShelleyNodeToClientVersion conway)
     )
  => (byron   -> cardano)
  -> (shelley -> cardano)
  -> (allegra -> cardano)
  -> (mary    -> cardano)
  -> (alonzo  -> cardano)
  -> (babbage -> cardano)
  -> (conway  -> cardano)
  -> Gen (WithVersion (HardForkNodeToClientVersion (CardanoEras c1 c2)) cardano)
arbitraryNodeToClient injByron injShelley injAllegra injMary injAlonzo injBabbage injConway = oneof
    -- Byron + HardFork disabled
    [ (\(WithVersion versionByron b) ->
          WithVersion
            (HardForkNodeToClientDisabled versionByron)
            (injByron b))
        <$> arbitrary
    -- Byron + HardFork enabled.
    , (\(WithVersion versionByron b) versionShelley versionAllegra versionMary versionAlonzo versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* versionAllegra
              :* versionMary
              :* versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injByron b))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Shelley + HardFork enabled
    , (\versionByron (WithVersion versionShelley s) versionAllegra versionMary versionAlonzo versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* versionAllegra
              :* versionMary
              :* versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injShelley s))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Allegra + HardFork enabled
    , (\versionByron versionShelley (WithVersion versionAllegra a) versionMary versionAlonzo versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* EraNodeToClientEnabled versionAllegra
              :* versionMary
              :* versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injAllegra a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Mary + HardFork enabled
    , (\versionByron versionShelley versionAllegra (WithVersion versionMary m) versionAlonzo versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* EraNodeToClientEnabled versionAllegra
              :* EraNodeToClientEnabled versionMary
              :* versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injMary m))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Alonzo + HardFork enabled
    , (\versionByron versionShelley versionAllegra versionMary (WithVersion versionAlonzo a) versionBabbage versionConway ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* EraNodeToClientEnabled versionAllegra
              :* EraNodeToClientEnabled versionMary
              :* EraNodeToClientEnabled versionAlonzo
              :* versionBabbage
              :* versionConway
              :* Nil
              ))
            (injAlonzo a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Babbage + HardFork enabled
    , (\versionByron versionShelley versionAllegra versionMary versionAlonzo (WithVersion versionBabbage a) versionConway ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* EraNodeToClientEnabled versionAllegra
              :* EraNodeToClientEnabled versionMary
              :* EraNodeToClientEnabled versionAlonzo
              :* EraNodeToClientEnabled versionBabbage
              :* versionConway
              :* Nil
              ))
            (injBabbage a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Conway + HardFork enabled
    , (\versionByron versionShelley versionAllegra versionMary versionAlonzo versionBabbage (WithVersion versionConway a) ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionByron
              :* EraNodeToClientEnabled versionShelley
              :* EraNodeToClientEnabled versionAllegra
              :* EraNodeToClientEnabled versionMary
              :* EraNodeToClientEnabled versionAlonzo
              :* EraNodeToClientEnabled versionBabbage
              :* EraNodeToClientEnabled versionConway
              :* Nil
              ))
            (injConway a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c1 c2))
                                (CardanoBlock c1 c2)) where
  arbitrary = arbitraryNodeToClient BlockByron BlockShelley BlockAllegra BlockMary BlockAlonzo BlockBabbage BlockConway

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c1 c2))
                                (CardanoGenTx c1 c2)) where
  arbitrary = arbitraryNodeToClient GenTxByron GenTxShelley GenTxAllegra GenTxMary GenTxAlonzo GenTxBabbage GenTxConway

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c1 c2))
                                (CardanoApplyTxErr c1 c2)) where
  arbitrary = frequency
      [ (8, arbitraryNodeToClient ApplyTxErrByron ApplyTxErrShelley ApplyTxErrAllegra ApplyTxErrMary ApplyTxErrAlonzo ApplyTxErrBabbage ApplyTxErrConway)
      , (2, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (HardForkApplyTxErrWrongEra <$> arbitrary))
      ]
  shrink = traverse aux
    where
      aux :: CardanoApplyTxErr MockCryptoCompatByron MockCryptoCompatByron
         -> [CardanoApplyTxErr MockCryptoCompatByron MockCryptoCompatByron]
      aux (HardForkApplyTxErrFromEra (OneEraApplyTxErr x)) =
          HardForkApplyTxErrFromEra . OneEraApplyTxErr <$> shrink x
      aux (HardForkApplyTxErrWrongEra x) =
          HardForkApplyTxErrWrongEra <$> shrink x

instance Arbitrary (Some QueryAnytime) where
  arbitrary = return $ Some GetEraStart

instance CardanoHardForkConstraints c1 c2
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c1 c2))
                                (Some (QueryHardFork (CardanoEras c1 c2)))) where
  arbitrary = frequency
      [ (1, do version <- getHardForkEnabledNodeToClientVersion <$> arbitrary
               return $ WithVersion version (Some GetInterpreter))
      , (1, do version <- genWithHardForkSpecificNodeToClientVersion
                            (>= HardForkSpecificNodeToClientVersion2)
               return $ WithVersion version (Some GetCurrentEra))
      ]

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c1 c2))
                                (SomeSecond BlockQuery (CardanoBlock c1 c2))) where
  arbitrary = frequency
      [ (1, arbitraryNodeToClient injByron injShelley injAllegra injMary injAlonzo injBabbage injConway)
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
              <*> (injAnytimeAlonzo <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeBabbage <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeConway <$> arbitrary))
      , (1, fmap injHardFork <$> arbitrary)
      ]
    where
      injByron          (SomeSecond query) = SomeSecond (QueryIfCurrentByron   query)
      injShelley        (SomeSecond query) = SomeSecond (QueryIfCurrentShelley query)
      injAllegra        (SomeSecond query) = SomeSecond (QueryIfCurrentAllegra query)
      injMary           (SomeSecond query) = SomeSecond (QueryIfCurrentMary    query)
      injAlonzo         (SomeSecond query) = SomeSecond (QueryIfCurrentAlonzo  query)
      injBabbage        (SomeSecond query) = SomeSecond (QueryIfCurrentBabbage query)
      injConway         (SomeSecond query) = SomeSecond (QueryIfCurrentConway  query)
      injAnytimeByron   (Some      query)  = SomeSecond (QueryAnytimeByron     query)
      injAnytimeShelley (Some      query)  = SomeSecond (QueryAnytimeShelley   query)
      injAnytimeAllegra (Some      query)  = SomeSecond (QueryAnytimeAllegra   query)
      injAnytimeMary    (Some      query)  = SomeSecond (QueryAnytimeMary      query)
      injAnytimeAlonzo  (Some      query)  = SomeSecond (QueryAnytimeAlonzo    query)
      injAnytimeBabbage (Some      query)  = SomeSecond (QueryAnytimeBabbage   query)
      injAnytimeConway  (Some      query)  = SomeSecond (QueryAnytimeConway    query)
      injHardFork       (Some      query)  = SomeSecond (QueryHardFork         query)

instance Arbitrary History.EraEnd where
  arbitrary = oneof
      [ History.EraEnd <$> arbitrary
      , return History.EraUnbounded
      ]

instance Arbitrary History.SafeZone where
  arbitrary = oneof
      [ History.StandardSafeZone <$> arbitrary
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

instance Arbitrary (History.Interpreter (CardanoEras c1 c2)) where
  arbitrary =
      History.mkInterpreter . History.Summary . enforceInvariant <$> arbitrary
    where
      -- Enforce the invariant that when the last era in the summary is the
      -- final era, it is unbounded. The decoder relies on this.
      enforceInvariant xs
        | length (nonEmptyToList xs) == lengthSList (Proxy @(CardanoEras c1 c2))
        = fixEndBound xs
        | otherwise
        = xs

      fixEndBound ::
           NonEmpty xs History.EraSummary
        -> NonEmpty xs History.EraSummary
      fixEndBound (NonEmptyCons e es) = NonEmptyCons e (fixEndBound es)
      fixEndBound (NonEmptyOne  e)    =
          NonEmptyOne  e { History.eraEnd = History.EraUnbounded }

instance Arbitrary (EraIndex (CardanoEras c1 c2)) where
  arbitrary = do
    let nbEras = lengthSList (Proxy @(CardanoEras c1 c2))
    index <- choose (0, fromIntegral nbEras - 1)
    case nsFromIndex index of
      Nothing -> error $ "nsFromIndex failed for " <> show index
      Just ns -> return $ eraIndexFromNS ns

instance (c1 ~ MockCryptoCompatByron, c2 ~ MockCryptoCompatByron)
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (CardanoEras c1 c2))
                                (SomeResult (CardanoBlock c1 c2))) where
  arbitrary = frequency
      [ (1, arbitraryNodeToClient injByron injShelley injAllegra injMary injAlonzo injBabbage injConway)
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
              <*> genQueryAnytimeResultAlonzo)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultBabbage)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultConway)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryHardForkResult)
      ]
    where
      injByron   (SomeResult q r) = SomeResult (QueryIfCurrentByron   q) (QueryResultSuccess r)
      injShelley (SomeResult q r) = SomeResult (QueryIfCurrentShelley q) (QueryResultSuccess r)
      injAllegra (SomeResult q r) = SomeResult (QueryIfCurrentAllegra q) (QueryResultSuccess r)
      injMary    (SomeResult q r) = SomeResult (QueryIfCurrentMary    q) (QueryResultSuccess r)
      injAlonzo  (SomeResult q r) = SomeResult (QueryIfCurrentAlonzo  q) (QueryResultSuccess r)
      injBabbage (SomeResult q r) = SomeResult (QueryIfCurrentBabbage q) (QueryResultSuccess r)
      injConway  (SomeResult q r) = SomeResult (QueryIfCurrentConway  q) (QueryResultSuccess r)

      -- In practice, when sending a Byron query you'll never get a mismatch
      -- saying that your query is from the Shelley era while the ledger is
      -- from Byron. Only the inverse. We ignore that in this generator, as it
      -- doesn't matter for serialisation purposes, we just generate a random
      -- 'MismatchEraInfo'.
      genQueryIfCurrentResultEraMismatch :: Gen (SomeResult (CardanoBlock c1 c2))
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
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentAlonzo q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentBabbage q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentConway q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          ]

      genQueryAnytimeResultByron :: Gen (SomeResult (CardanoBlock c1 c2))
      genQueryAnytimeResultByron =
          SomeResult (QueryAnytimeByron GetEraStart) <$> arbitrary

      genQueryAnytimeResultShelley :: Gen (SomeResult (CardanoBlock c1 c2))
      genQueryAnytimeResultShelley =
          SomeResult (QueryAnytimeShelley GetEraStart) <$> arbitrary

      genQueryAnytimeResultAllegra :: Gen (SomeResult (CardanoBlock c1 c2))
      genQueryAnytimeResultAllegra =
          SomeResult (QueryAnytimeAllegra GetEraStart) <$> arbitrary

      genQueryAnytimeResultMary :: Gen (SomeResult (CardanoBlock c1 c2))
      genQueryAnytimeResultMary =
          SomeResult (QueryAnytimeMary GetEraStart) <$> arbitrary

      genQueryAnytimeResultAlonzo :: Gen (SomeResult (CardanoBlock c1 c2))
      genQueryAnytimeResultAlonzo =
          SomeResult (QueryAnytimeAlonzo GetEraStart) <$> arbitrary

      genQueryAnytimeResultBabbage :: Gen (SomeResult (CardanoBlock c1 c2))
      genQueryAnytimeResultBabbage =
          SomeResult (QueryAnytimeBabbage GetEraStart) <$> arbitrary

      genQueryAnytimeResultConway :: Gen (SomeResult (CardanoBlock c1 c2))
      genQueryAnytimeResultConway =
          SomeResult (QueryAnytimeConway GetEraStart) <$> arbitrary

      genQueryHardForkResult :: Gen (SomeResult (CardanoBlock c1 c2))
      genQueryHardForkResult = oneof
          [ SomeResult (QueryHardFork GetInterpreter) <$> arbitrary
          , SomeResult (QueryHardFork GetCurrentEra)  <$> arbitrary
          ]
