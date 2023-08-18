{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.KESAgent.Tests.Serialization
where

import Cardano.KESAgent.TextEnvelope
import Cardano.KESAgent.OCert
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.Serialization

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.PinnedSizedBytes (PinnedSizedBytes, psbToByteString)
import Test.Crypto.Instances

import Control.Monad.Class.MonadThrow ( bracket )
import qualified Data.Aeson as JSON
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "serialization"
  [ testGroup "properties"
    [ testGroup "KES"
      [ testSerializable "VerKey" mkVerKeyKES
      ]
    , testGroup "ColdKey"
      [ testSerializable "VerKey" mkColdVerKey
      , testSerializable "SignKey" mkColdSignKey
      ]
    ]
  ]

mkVerKeyKES :: PinnedSizedBytes (SeedSizeKES (KES StandardCrypto)) -> IO (KESVerKey StandardCrypto)
mkVerKeyKES seed = KESVerKey <$> do
  withMLockedSeedFromPSB seed $ \mseed ->
    bracket
      (genKeyKES mseed)
      forgetSignKeyKES
      deriveVerKeyKES

mkColdSignKey :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto)) -> IO (ColdSignKey StandardCrypto)
mkColdSignKey seed = return . ColdSignKey $ genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seed)

mkColdVerKey :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto)) -> IO (ColdVerKey StandardCrypto)
mkColdVerKey seed = ColdVerKey . deriveVerKeyDSIGN . coldSignKey <$> mkColdSignKey seed

testSerializable :: forall a seed.
                    ( HasTextEnvelope a, Eq a, Show a, Arbitrary seed, Show seed )
                 => String
                 -> (seed -> IO a)
                 -> TestTree
testSerializable name makeThing =
  testGroup name
    [ testProperty "round trip TextEnvelope" $ prop_roundtrip_te makeThing
    , testProperty "round trip JSON" $ prop_roundtrip_json makeThing
    ]

prop_roundtrip_te :: forall a seed. (HasTextEnvelope a, Eq a, Show a)
                  => (seed -> IO a)
                  -> seed
                  -> Property
prop_roundtrip_te makeThing seed = ioProperty $ do
  val <- makeThing seed
  return $
    let expected :: Either String a = Right val
        actual = fromTextEnvelope . toTextEnvelope $ val
    in (expected === actual)

prop_roundtrip_json :: forall a seed. (HasTextEnvelope a, Eq a, Show a)
                    => (seed -> IO a)
                    -> seed
                    -> Property
prop_roundtrip_json makeThing seed = ioProperty $ do
  val <- makeThing seed
  return $
    let expected :: Either String a = Right val
        encoded = JSON.encode . toTextEnvelope $ val
        actual = JSON.eitherDecode encoded >>= fromTextEnvelope
    in (expected === actual)
