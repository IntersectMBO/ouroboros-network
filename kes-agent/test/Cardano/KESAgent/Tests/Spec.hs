{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}

module Cardano.KESAgent.Tests.Spec
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.Driver
import Cardano.KESAgent.Protocols.Control.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.Spec

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.PinnedSizedBytes (PinnedSizedBytes, psbToByteString)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Ouroboros.Network.RawBearer
import Test.Crypto.Instances

import Control.Concurrent.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.TChan
import Control.Monad (forM_)
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow ( MonadThrow, bracket )
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Int
import Data.Proxy
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Typeable
import Data.Word
import System.FilePath ( (</>) )
import System.IO.Temp ( withSystemTempDirectory )
import Test.Crypto.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Foreign.Marshal (copyBytes, allocaBytes)
import Foreign.Ptr (castPtr)
import GHC.TypeNats (KnownNat, natVal)

data SomeRecord =
  SomeRecord
    { field1 :: Word8
    , field2 :: Word8
    }
    deriving (Show, Read, Eq, Ord, Typeable)

instance Arbitrary SomeRecord where
  arbitrary = SomeRecord <$> arbitrary <*> arbitrary
  shrink (SomeRecord a b) = do
    a' <- shrink a
    b' <- shrink b
    [SomeRecord a' b, SomeRecord a b']

$(deriveSer ''SomeRecord)

data SomeEnum
  = Value0
  | Value1
  | Value2
  | Value3
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

instance Arbitrary SomeEnum where
  arbitrary = toEnum . (`mod` 4) . abs <$> arbitrary
  shrink = \case
    Value0 -> []
    x -> [Value0 .. pred x]

deriving via (ViaEnum SomeEnum)
  instance HasSerInfo SomeEnum

deriving via (ViaEnum SomeEnum)
  instance ( forall x y. Coercible x y => Coercible (m x) (m y)
           , MonadThrow m
           , MonadST m
           )
           => IsSerItem m SomeEnum

tests :: TestTree
tests = testGroup "Spec ser"
  [ testSpec (Proxy @()) "()"
  , testSpec (Proxy @Bool) "Bool"
  , testSpec (Proxy @Word8) "Word8"
  , testSpec (Proxy @Word16) "Word16"
  , testSpec (Proxy @Word32) "Word32"
  , testSpec (Proxy @Word64) "Word64"
  , testSpec (Proxy @(Maybe Word64)) "Maybe Word64"
  , testSpec (Proxy @[Word64]) "[Word64]"
  , testSpec (Proxy @Int8) "Int8"
  , testSpec (Proxy @Int16) "Int16"
  , testSpec (Proxy @Int32) "Int32"
  , testSpec (Proxy @Int64) "Int64"
  , testSpec (Proxy @(Maybe Int64)) "Maybe Int64"
  , testSpec (Proxy @[Int64]) "[Int64]"
  , testSpec (Proxy @SomeRecord) "SomeRecord"
  , testSpec (Proxy @SomeEnum) "SomeEnum"
  , testSpecMk "UTCTime" (return . posixSecondsToUTCTime . fromIntegral :: Int -> IO UTCTime)
  , testSpecMk "VerKeyKES" mkVerKeyKES
  , testSpecMk "ColdVerKey" mkColdVerKey
  , testSpecMk "Text" (return . Text.pack)
  , testSpecMk "Sized 10 ByteString" (mkSizedBS @10)
  , testSpecMk "VariableSized ByteString" (return . VariableSized . BS.pack)
  , testSpec (Proxy @SomeRecord) "SomeRecord"
  , testSpecMk "KeyInfo" mkKeyInfo
  , testSpecMk "BootstrapInfo" (return . mkBootstrapInfo)
  , testSpecMk "BundleInfo" mkBundleInfo
  , testSpecMk "Maybe BundleInfo" (fmap Just . mkBundleInfo)
  , testSpecMk "Maybe KeyInfo" (fmap Just . mkKeyInfo)
  , testSpecMk "[BootstrapInfo]" (return . map mkBootstrapInfo)
  , testSpecMk "Nothing :: Maybe BundleInfo" (\(i :: Int) -> return (Nothing :: Maybe (BundleInfo StandardCrypto)))
  , testSpecMk "AgentInfo" mkAgentInfo
  , testSpecMk "Command" (return . mkEnum :: Int -> IO Command)
  , testSpecMk "RecvResult" (return . mkEnum :: Int -> IO RecvResult)
  ]

withSignKeyKES :: (SignKeyKES (KES StandardCrypto) -> IO a)
               -> PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
               -> IO a
withSignKeyKES action seed = 
  withMLockedSeedFromPSB seed $ \mseed ->
    bracket
      (genKeyKES mseed)
      forgetSignKeyKES
      action

mkSizedBS :: forall n. KnownNat n => (Word8, [Word8]) -> IO (Sized n BS.ByteString)
mkSizedBS (x, xs) =
  return . Sized . BS.pack . take (fromIntegral . natVal $ Proxy @n) . concat . repeat $ (x:xs)

mkBootstrapInfo :: (String, Int) -> BootstrapInfo
mkBootstrapInfo (addr, i) =
  BootstrapInfo (Text.pack addr) (mkEnum i)

mkEnum :: forall a. (Enum a, Bounded a) => Int -> a
mkEnum i =
  toEnum (abs i `mod` (fromEnum (maxBound :: a) + 1))

mkAgentInfo :: ( -- | source data for bundle
                 Maybe
                   ( Word32
                   , Word -- ^ KES period
                   , Word64 -- ^ ocertN
                   , PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
                     -- ^ seed for KES key
                   , PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto))
                     -- ^ seed for cold key
                   )
               , -- | source data for staged key
                 Maybe
                   ( PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
                   )
               , Int -- ^ current time
               , Word -- ^ current KES period
               , [(String, Int)]
               ) 
            -> IO (AgentInfo StandardCrypto)
mkAgentInfo (bundleSeedMay, stagedSeedMay, t, p, connSeeds) = do
  -- bundle <- maybe (return Nothing) (fmap Just . mkBundleInfo) bundleSeedMay
  let bundle = Nothing
  stagedKey <- maybe (return Nothing) (fmap Just . mkKeyInfo) stagedSeedMay
  let bootstrapInfos = map mkBootstrapInfo connSeeds
  let kesPeriod = KESPeriod p

  return
      AgentInfo
        { agentInfoCurrentBundle = bundle
        , agentInfoStagedKey = stagedKey
        , agentInfoCurrentTime = posixSecondsToUTCTime . fromIntegral . abs $ t
        , agentInfoCurrentKESPeriod = kesPeriod
        , agentInfoBootstrapConnections = bootstrapInfos
        }

mkBundleInfo :: ( Word32 -- ^ evolution
                , Word -- ^ KES period
                , Word64 -- ^ ocertN
                , PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
                  -- ^ seed for KES key
                , PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto))
                  -- ^ seed for cold key
                )
              -> IO (BundleInfo StandardCrypto)
mkBundleInfo (e, p, n, seedKES, seedDSIGN) = do
  let kesPeriod = KESPeriod p
  vkHot <- mkVerKeyKES seedKES
  skCold <- mkColdSignKey seedDSIGN
  let ocert = makeOCert vkHot n kesPeriod skCold
  return BundleInfo
    { bundleInfoEvolution = e
    , bundleInfoStartKESPeriod = kesPeriod
    , bundleInfoOCertN = n
    , bundleInfoVK = vkHot
    , bundleInfoSigma = ocertSigma ocert
    }

mkKeyInfo :: PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
          -> IO (KeyInfo StandardCrypto)
mkKeyInfo seed = KeyInfo <$> mkVerKeyKES seed

mkVerKeyKES :: PinnedSizedBytes (SeedSizeKES (KES StandardCrypto))
            -> IO (VerKeyKES (KES StandardCrypto))
mkVerKeyKES = withSignKeyKES deriveVerKeyKES

mkColdSignKey :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto))
              -> IO (SignKeyDSIGN (DSIGN StandardCrypto))
mkColdSignKey seed = return $ genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seed)

mkColdVerKey :: PinnedSizedBytes (SeedSizeDSIGN (DSIGN StandardCrypto))
             -> IO (VerKeyDSIGN (DSIGN StandardCrypto))
mkColdVerKey seed = deriveVerKeyDSIGN <$> mkColdSignKey seed

testSpecMk :: forall a seed.
            ( IsSerItem IO a, Eq a, Show a, Arbitrary seed, Show seed )
         => String
         -> (seed -> IO a)
         -> TestTree
testSpecMk name mkThing =
  testSpecWith name withThing
  where
    withThing :: forall b. (a -> IO b) -> seed -> IO b
    withThing action seed = bracket (mkThing seed) (const $ return ()) action

testSpec :: forall a.
            ( IsSerItem IO a, Eq a, Show a, Arbitrary a )
         => Proxy a
         -> String
         -> TestTree
testSpec _ name =
  testSpecMk name (return :: a -> IO a)

testSpecWith :: forall a seed.
                ( IsSerItem IO a, Eq a, Show a, Arbitrary seed, Show seed )
             => String
             -> (forall b. (a -> IO b) -> seed -> IO b)
             -> TestTree
testSpecWith name withThing =
  testGroup name
    [ testProperty "declared size matches" $ prop_size_match withThing
    , testProperty "round trip IsSer" $ prop_roundtrip_isSer withThing
    ]

mkDummyBearer :: IO (RawBearer IO)
mkDummyBearer = do
  streamVar <- atomically $ newTMVar BS.empty
  return
    RawBearer
      { send = \buf bufsize -> do
          bufBS <- BS.packCStringLen (castPtr buf, bufsize)
          atomically $ do
            streamBS <- takeTMVar streamVar
            let streamBS' = streamBS <> bufBS
            putTMVar streamVar streamBS'
          return bufsize
      , recv = \buf bufsize -> do
          bufBS <- atomically $ do
            streamBS <- takeTMVar streamVar
            let bufBS = BS.take bufsize streamBS
                streamBS' = BS.drop bufsize streamBS
            putTMVar streamVar streamBS'
            return bufBS
          BS.useAsCStringLen
            bufBS
            (\(src, srcSize) -> do
              copyBytes (castPtr buf) (castPtr src) srcSize
              return srcSize
            )
      }

prop_roundtrip_isSer :: forall a seed. (IsSerItem IO a, Eq a, Show a)
                  => (forall b. (a -> IO b) -> seed -> IO b)
                  -> seed
                  -> Property
prop_roundtrip_isSer withThing seed =
  ioProperty $ withThing check seed
  where
    check :: a -> IO Property
    check expected = do
      s <- mkDummyBearer
      sendItem s expected
      actual <- runReadResultT $ receiveItem s
      return (ReadOK expected === actual)

prop_size_match :: forall a seed. (IsSerItem IO a, Eq a, Show a)
                => (forall b. (a -> IO b) -> seed -> IO b)
                -> seed
                -> Property
prop_size_match withThing seed =
  ioProperty $ withThing check seed
  where
    check :: a -> IO Property
    check item = do
      s <- mkDummyBearer
      sendItem s item
      let bufsize = 1024 * 1024
      actual <- allocaBytes bufsize $ \buf -> recv s buf bufsize
      let declared = fieldSize $ info (Proxy @a)
      case knownSize declared of
        Just expected -> return $
          classify True ("known size " ++ show expected ++ "")
          (expected === actual)
        Nothing -> case declared of
          RangeSize (FixedSize a) (FixedSize b) -> return $
            classify True ("size range " ++ show a ++ " .. " ++ show b)
            (actual >= a .&&. actual <= b)
          _ -> return $
            classify True ("variable size (" ++ formatFieldSize declared ++ ")")
            (property True)
