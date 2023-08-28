{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Tests.Serialization
where

import Cardano.KESAgent.Serialization.TextEnvelope
import Cardano.KESAgent.Serialization.CBOR
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.StandardCrypto

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.PinnedSizedBytes (PinnedSizedBytes, psbToByteString)
import Test.Crypto.Instances

import Control.Monad.Class.MonadThrow ( bracket )
import qualified Data.Aeson as JSON
import Data.Proxy
import Data.Word ( Word64 )
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import System.FilePath ( (</>) )
import System.IO.Temp ( withSystemTempDirectory )

import Paths_kes_agent

tests :: TestTree
tests = testGroup "serialization"
  [ testGroup "fixtures"
    [ testDeserializeFixture "Cold VerKey" "fixtures/cold.vkey" (Proxy @(ColdVerKey (DSIGN StandardCrypto)))
    , testDeserializeFixture "Cold SignKey" "fixtures/cold.skey" (Proxy @(ColdSignKey (DSIGN StandardCrypto)))
    , testDeserializeFixture "KES VerKey" "fixtures/kes.vkey" (Proxy @(KESVerKey (KES StandardCrypto)))
    , testDeserializeFixture "OpCert Counter" "fixtures/opcert.counter" (Proxy @(OpCertCounter StandardCrypto))
    , testDeserializeFixture "OpCert" "fixtures/opcert.cert" (Proxy @(OpCert StandardCrypto))
    ]
  , testGroup "properties"
    [ testGroup "KES"
      [ testSerializable "VerKey" mkVerKeyKES
      ]
    , testGroup "ColdKey"
      [ testSerializable "VerKey" mkColdVerKey
      , testSerializable "SignKey" mkColdSignKey
      ]
    , testGroup "OpCert"
      [ testSerializable "Counter" mkOpCertCounter
      , testSerializable "Cert" mkOpCert
      ]
    ]
  ]

testDeserializeFixture :: forall a. HasTextEnvelope a => String -> FilePath -> Proxy a -> TestTree
testDeserializeFixture label srcFile proxy =
  testCase label $ do
    path <- getDataFileName srcFile
    Just json <- JSON.decodeFileStrict' path
    result <- either error return $ fromTextEnvelope @a json
    return ()

mkVerKeyKES :: PinnedSizedBytes (SeedSizeKES (KES StandardCrypto)) -> IO (KESVerKey (KES StandardCrypto))
mkVerKeyKES seed = KESVerKey <$> do
  withMLockedSeedFromPSB seed $ \mseed ->
    bracket
      (genKeyKES mseed)
      forgetSignKeyKES
      deriveVerKeyKES

mkColdSignKey :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto)) -> IO (ColdSignKey (DSIGN StandardCrypto))
mkColdSignKey seed = return . ColdSignKey $ genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seed)

mkColdVerKey :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto)) -> IO (ColdVerKey (DSIGN StandardCrypto))
mkColdVerKey seed = ColdVerKey . deriveVerKeyDSIGN . coldSignKey <$> mkColdSignKey seed

mkOpCertCounter :: (PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto)), Word64) -> IO (OpCertCounter StandardCrypto)
mkOpCertCounter (seed, count) = do
  ColdVerKey vk <- mkColdVerKey seed
  return $ OpCertCounter count vk

mkOpCert :: (Word64, Word, PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto)), PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))) -> IO (OpCert StandardCrypto)
mkOpCert (n, kesPeriod, seedDSIGN, seedKES) = do
  ColdSignKey coldSignKey <- mkColdSignKey seedDSIGN
  let coldVerKey = deriveVerKeyDSIGN coldSignKey
  KESVerKey kesVerKey <- mkVerKeyKES seedKES
  let ocert = makeOCert kesVerKey n (KESPeriod kesPeriod) coldSignKey
  return $ OpCert ocert coldVerKey

testSerializable :: forall a seed.
                    ( HasTextEnvelope a, Eq a, Show a, Arbitrary seed, Show seed )
                 => String
                 -> (seed -> IO a)
                 -> TestTree
testSerializable name makeThing =
  testGroup name
    [ testProperty "round trip TextEnvelope" $ prop_roundtrip_te makeThing
    , testProperty "round trip JSON" $ prop_roundtrip_json makeThing
    , testProperty "round trip bytestring" $ prop_roundtrip_bytestring makeThing
    , testProperty "round trip file" $ prop_roundtrip_file makeThing
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

prop_roundtrip_bytestring :: forall a seed. (HasTextEnvelope a, Eq a, Show a)
                          => (seed -> IO a)
                          -> seed
                          -> Property
prop_roundtrip_bytestring makeThing seed = ioProperty $ do
  val <- makeThing seed
  return $
    let expected :: Either String a = Right val
        encoded = encodeTextEnvelope val
        actual = decodeTextEnvelope encoded
    in (expected === actual)

prop_roundtrip_file :: forall a seed. (HasTextEnvelope a, Eq a, Show a)
                    => (seed -> IO a)
                    -> seed
                    -> Property
prop_roundtrip_file makeThing seed = ioProperty $ do
  val <- makeThing seed
  actual <- withSystemTempDirectory "kes-agent-tests" $ \dirname -> do
    let filename = dirname </> "serialized.something"
    encodeTextEnvelopeFile filename val
    decodeTextEnvelopeFile filename
  return $
    let expected :: Either String a = Right val
    in (expected === actual)
