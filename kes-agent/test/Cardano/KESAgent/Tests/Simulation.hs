-- TODO: replace psbFromBytes with something non-deprecated and remove this pragma
{-#OPTIONS_GHC -Wno-deprecations#-}

{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeFamilies #-}
module Cardano.KESAgent.Tests.Simulation
( tests )
where

import Cardano.Crypto.DirectSerialise
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.Ed25519ML
import Cardano.Crypto.DSIGNM.Class
import Cardano.Crypto.Hash.Blake2b
import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.Single
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.Libsodium
import Cardano.Crypto.PinnedSizedBytes
import Cardano.Crypto.Seed
import Cardano.Crypto.SafePinned
import Cardano.Binary (FromCBOR)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (bracket, catch, SomeException)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy
import GHC.TypeLits (KnownNat, natVal)
import System.Socket.Family.Unix
import System.IO
import System.IO.Unsafe
import Test.QuickCheck (Arbitrary (..), vectorOf)
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Printf (printf)
import Data.Word
import Data.Typeable

import Cardano.KESAgent.Agent
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.OCert
import Cardano.KESAgent.ServiceClient
import Cardano.KESAgent.ControlClient
import Cardano.KESAgent.Logging
import Cardano.KESAgent.Tests.Util

tests :: Lock -> TestTree
tests lock =
  testGroup "Simulation"
  [ testProperty "One key through chain" (testOneKeyThroughChain @StandardCrypto Proxy lock)
  ]

testOneKeyThroughChain :: forall c
                        . Crypto c
                       => Typeable c
                       => VersionedProtocol (KESProtocol c)
                       => KESSignAlgorithm IO (KES c)
                       => DirectSerialise (SignKeyKES (KES c))
                       => DirectDeserialise (SignKeyKES (KES c))
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => ContextDSIGN (DSIGN c) ~ ()
                       => Show (SignKeyKES (KES c))
                       => Proxy c
                       -> Lock
                       -> PinnedSizedBytes (SeedSizeKES (KES c))
                       -> Word64
                       -> Word
                       -> Word
                       -> Word
                       -> Property
testOneKeyThroughChain p lock seedPSB certN kesPeriodRaw nodeDelay controlDelay =
  ioProperty . withLock lock . withMLSBFromPSB seedPSB $ \seed -> do
    let seedP = mkSeedFromBytes . psbToByteString $ seedPSB
    hSetBuffering stdout LineBuffering
    expectedSK <- genKeyKES @IO @(KES c) seed
    vkHot <- deriveVerKeyKES expectedSK
    let skCold = genKeyDSIGN @(DSIGN c) seedP
        kesPeriod = KESPeriod kesPeriodRaw
        signable = OCertSignable vkHot certN kesPeriod
        sig = signedDSIGN () signable skCold
        expectedOC = OCert vkHot certN kesPeriod sig
    resultVar <- newEmptyMVar :: IO (MVar (SignKeyKES (KES c), OCert c))
    let tracer :: forall a. Show a => Tracer IO a
        -- Change tracer to Tracer print for debugging
        -- tracer = Tracer print
        tracer = nullTracer

    output <- race
          -- abort 1 second after both clients have started
          (threadDelay $ 1000000 + (fromIntegral $ max controlDelay nodeDelay) + 500)
          (race
            -- run these to "completion"
            (agent tracer `concurrently_`
              node tracer resultVar `concurrently_`
              controlServer tracer (expectedSK, expectedOC))
            -- ...until this one finished
            (watch resultVar)
          )

    case output of
        Left err -> error (show err)
        Right (Left err) -> error (show err)
        Right (Right (resultSK, resultOC)) -> do
          -- Serialize keys so that we can forget them immediately, and only close over
          -- their (non-mlocked) serializations when returning the property.
          -- Serializing KES sign keys like this is normally unsafe, as it violates
          -- mlocking guarantees, but for testing purposes, this is fine.
          expectedSKBS <- rawSerialiseSignKeyKES expectedSK
          resultSKBS <- rawSerialiseSignKeyKES resultSK
          forgetSignKeyKES expectedSK
          forgetSignKeyKES resultSK
          return (expectedSKBS === resultSKBS)

  where
    Just serviceSocketAddr = socketAddressUnixAbstract "KESAgent/service"
    Just controlSocketAddr = socketAddressUnixAbstract "KESAgent/control"
    agent tracer = runAgent p
              AgentOptions { agentServiceSocketAddress = serviceSocketAddr
                           , agentControlSocketAddress = controlSocketAddr
                           }
              tracer
    node tracer mvar = do
      threadDelay (500 + fromIntegral nodeDelay)
      (runServiceClient p
        ServiceClientOptions { serviceClientSocketAddress = serviceSocketAddr }
        (\sk oc -> do
          putMVar mvar (sk, oc)
        ))
        tracer
        `catch` (\(e :: AsyncCancelled) -> return ())
        `catch` (\(e :: SomeException) -> putStrLn $ "NODE: " ++ show e)
    controlServer tracer (sk, oc) = do
      threadDelay (500 + fromIntegral controlDelay)
      (runControlClient1 p
        ControlClientOptions { controlClientSocketAddress = controlSocketAddr }
        sk oc)
        tracer
        `catch` (\(e :: AsyncCancelled) -> return ())
        `catch` (\(e :: SomeException) -> putStrLn $ "CONTROL: " ++ show e)
    watch mvar = do
      takeMVar mvar

-- Show instances for signing keys violate mlocking guarantees, but for testing
-- purposes, this is fine, so we'll declare orphan instances here.
--
instance Show (SignKeyKES (SingleKES Ed25519DSIGNM)) where
  show (SignKeySingleKES (SignKeyEd25519DSIGNM mlsb)) =
    let bytes = BS.unpack $ mlsbToByteString mlsb
        hexstr = concatMap (printf "%02x") bytes
    in "SignKeySingleKES (SignKeyEd25519DSIGNM " ++ hexstr ++ ")"

instance Show (SignKeyKES d) => Show (SignKeyKES (SumKES h d)) where
  show (SignKeySumKES sk r vk0 vk1) = show sk

-- We normally ensure that we avoid naively comparing signing keys by not
-- providing instances, but for tests it is fine, so we provide the orphan
-- instances here.

deriving instance Eq (SignKeyDSIGNM d)
               => Eq (SignKeyKES (SingleKES d))
deriving instance (KESAlgorithm d, SodiumHashAlgorithm h, Eq (SignKeyKES d))
               => Eq (SignKeyKES (SumKES h d))

instance Eq a => Eq (SafePinned a) where
  ap == bp = unsafePerformIO $ do
    interactSafePinned ap $ \a ->
      interactSafePinned bp $ \b ->
        return (a == b)

-- We cannot allow this instance, because it doesn't guarantee timely
-- forgetting of the MLocked memory.
-- Instead, use 'arbitrary' to generate a suitably sized PinnedSizedBytes
-- value, and then mlsbFromPSB or withMLSBFromPSB to convert it to an
-- MLockedSizedBytes value.
--
-- instance KnownNat n => Arbitrary (MLockedSizedBytes n) where
--     arbitrary = unsafePerformIO . mlsbFromByteString . BS.pack <$> vectorOf size arbitrary
--       where
--         size :: Int
--         size = fromInteger (natVal (Proxy :: Proxy n))

mlsbFromPSB :: (KnownNat n) => PinnedSizedBytes n -> IO (MLockedSizedBytes n)
mlsbFromPSB = mlsbFromByteString . psbToByteString

withMLSBFromPSB :: (KnownNat n) => PinnedSizedBytes n -> (MLockedSizedBytes n -> IO a) -> IO a
withMLSBFromPSB psb action =
  bracket
    (mlsbFromPSB psb)
    mlsbFinalize
    action

instance KnownNat n => Arbitrary (PinnedSizedBytes n) where
    arbitrary = psbFromBytes <$> vectorOf size arbitrary
      where
        size :: Int
        size = fromInteger (natVal (Proxy :: Proxy n))
