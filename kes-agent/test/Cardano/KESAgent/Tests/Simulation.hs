-- TODO: replace psbFromBytes with something non-deprecated and remove this pragma
{-#OPTIONS_GHC -Wno-deprecations#-}

{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE DerivingStrategies #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy
import GHC.TypeLits (KnownNat, natVal)
import GHC.Stack (HasCallStack)
import System.Socket.Family.Unix
import System.IO
import System.IO.Unsafe
import Test.QuickCheck (Arbitrary (..), vectorOf)
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Printf (printf)
import Data.Word
import Data.Typeable
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text

import Cardano.KESAgent.Agent
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.OCert
import Cardano.KESAgent.ServiceClient
import Cardano.KESAgent.ControlClient
import Cardano.KESAgent.Driver (DriverTrace (..))
import Cardano.KESAgent.Logging
import Cardano.KESAgent.Evolution
import Cardano.KESAgent.Tests.Util

tests :: Lock -> TestTree
tests lock =
  testGroup "Simulation"
  [ testProperty "Mock" (testOneKeyThroughChain @MockCrypto Proxy lock)
  , testProperty "Standard" (testOneKeyThroughChain @StandardCrypto Proxy lock)
  ]

elaborateTracerLock :: MVar ()
elaborateTracerLock = unsafePerformIO $ newMVar ()
{-#NOINLINE elaborateTracerLock #-}

elaborateTracer :: TracePretty a => Tracer IO a
elaborateTracer = Tracer $ \x -> withMVar elaborateTracerLock $ \() -> do
  t <- getPOSIXTime
  hPutStrLn stdout $ printf "%015.4f %s" (realToFrac t :: Double) (tracePretty x)
  hFlush stdout

class TracePretty a where
  tracePretty :: a -> String

strLength :: String -> Int
strLength = length

instance TracePretty AgentTrace where
  tracePretty (AgentServiceDriverTrace d) = "Agent: ServiceDriver: " ++ tracePretty d
  tracePretty (AgentControlDriverTrace d) = "Agent: ControlDriver: " ++ tracePretty d
  tracePretty (AgentServiceClientConnected addr) = "Agent: ServiceClientConnected"
  tracePretty (AgentServiceClientDisconnected addr) = "Agent: ServiceClientDisconnected"
  tracePretty (AgentServiceSocketError err) = "Agent: ServiceSocketError " ++ (unwords . lines . show $ err)
  tracePretty (AgentControlClientConnected addr) = "Agent: ControlClientConnected"
  tracePretty (AgentControlClientDisconnected addr) = "Agent: ControlClientDisconnected"
  tracePretty (AgentControlSocketError err) = "Agent: ControlSocketError " ++ (unwords . lines . show $ err)
  tracePretty x = "Agent: " ++ (drop (strLength "Agent") (show x))

instance TracePretty ControlClientTrace where
  tracePretty (ControlClientDriverTrace d) = "Control: Driver: " ++ tracePretty d
  tracePretty (ControlClientConnected addr) = "Control: Connected"
  tracePretty x = "Control: " ++ (drop (strLength "ControlClient") (show x))

instance TracePretty ServiceClientTrace where
  tracePretty (ServiceClientDriverTrace d) = "Service: Driver: " ++ tracePretty d
  tracePretty (ServiceClientConnected addr) = "Service: Connected"
  tracePretty x = "Service: " ++ (drop (strLength "ServiceClient") (show x))

instance TracePretty DriverTrace where
  tracePretty x = drop (strLength "Driver") (show x)

data Simulator c =
  Simulator
    { sendKey :: (SignKeyWithPeriodKES (KES c), OCert c) -> IO ()
    , delay :: Int -> IO ()
    , setTime :: NominalDiffTime -> IO ()
    }

newtype PrettyBS = PrettyBS { unPrettyBS :: ByteString }
  deriving newtype Eq

instance Show PrettyBS where
  show = hexShowBS . unPrettyBS

runTestNetwork :: forall c
                . Crypto c
               => Typeable c
               => VersionedProtocol (KESProtocol c)
               => KESSignAlgorithm IO (KES c)
               => ContextKES (KES c) ~ ()
               => DirectSerialise (SignKeyKES (KES c))
               => DirectDeserialise (SignKeyKES (KES c))
               => DSIGN.Signable (DSIGN c) (OCertSignable c)
               => ContextDSIGN (DSIGN c) ~ ()
               => Show (SignKeyWithPeriodKES (KES c))
               => Proxy c
               -> (forall a. (Show a, TracePretty a) => Tracer IO a)
               -> SocketAddress Unix
               -> SocketAddress Unix
               -> Integer
               -> Word
               -> Word
               -> -- | A \"sender\" script, i.e., the control server
                  (Simulator c -> IO ())
               -> -- | A \"receiver\" job, which will receive keys as they
                  -- arrive at the Node.
                  ((SignKeyWithPeriodKES (KES c), OCert c) -> (Property -> IO ()) -> IO ())
               -> IO Property
runTestNetwork p tracer controlAddress serviceAddress genesisTimestamp nodeDelay controlDelay senderScript receiverScript = do
    propertyVar <- newEmptyMVar :: IO (MVar Property)
    timeVar <- newMVar (fromInteger genesisTimestamp)

    let agentOptions = defAgentOptions
                          { agentGenesisTimestamp = genesisTimestamp
                          , agentGetCurrentTime = readMVar timeVar
                          , agentControlSocketAddress = controlAddress
                          , agentServiceSocketAddress = serviceAddress
                          }
        agent :: HasCallStack => Tracer IO AgentTrace -> IO ()
        agent tracer = runAgent p agentOptions tracer

        node :: HasCallStack
             => Tracer IO ServiceClientTrace
             -> MVar Property
             -> IO ()
        node tracer mvar = do
          threadDelay (fromIntegral nodeDelay)
          (runServiceClient p
            ServiceClientOptions { serviceClientSocketAddress = agentServiceSocketAddress agentOptions }
            (\sk oc -> do
              receiverScript (sk, oc) (putMVar mvar)
            ))
            tracer
            `catch` (\(e :: AsyncCancelled) -> return ())
            `catch` (\(e :: SomeException) -> putStrLn $ "NODE: " ++ show e)

        simulator :: Simulator c
        simulator = Simulator
          { sendKey = \(sk, oc) ->
              (runControlClient1 p
                ControlClientOptions { controlClientSocketAddress = agentControlSocketAddress agentOptions }
                sk oc)
                tracer
                `catch` (\(e :: AsyncCancelled) -> return ())
                `catch` (\(e :: SomeException) -> putStrLn $ "CONTROL: " ++ show e)
          , delay = threadDelay
          , setTime = void . swapMVar timeVar
          }

        controlServer :: HasCallStack
                      => Tracer IO ControlClientTrace
                      -> (Simulator c -> IO ())
                      -> IO ()
        controlServer tracer script = do
          threadDelay (fromIntegral controlDelay)
          script simulator

        watch :: HasCallStack => MVar a -> IO a
        watch mvar = do
          takeMVar mvar

    output <- race
          -- abort 1 second after both clients have started
          (threadDelay $ 1000000 + (fromIntegral $ max controlDelay nodeDelay) + 500)
          (race
            -- run these to "completion"
            (agent tracer `concurrently_`
              node tracer propertyVar `concurrently_`
              controlServer tracer senderScript)
            -- ...until this one finishes
            (watch propertyVar)
          )

    case output of
        Left () -> error "TIMEOUT"
        Right (Left err) -> error ("EXCEPTION\n" ++ show err)
        Right (Right prop) -> return prop


testOneKeyThroughChain :: forall c
                        . Crypto c
                       => Typeable c
                       => VersionedProtocol (KESProtocol c)
                       => KESSignAlgorithm IO (KES c)
                       => ContextKES (KES c) ~ ()
                       => DirectSerialise (SignKeyKES (KES c))
                       => DirectDeserialise (SignKeyKES (KES c))
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => ContextDSIGN (DSIGN c) ~ ()
                       => Show (SignKeyWithPeriodKES (KES c))
                       => Proxy c
                       -> Lock
                       -> Word64
                       -> Word64
                       -> PinnedSizedBytes (SeedSizeKES (KES c))
                       -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                       -> Word64
                       -> Word
                       -> Word
                       -> Property
testOneKeyThroughChain p lock controlAddressStr serviceAddressStr seedKESPSB seedDSIGNPSB certN nodeDelay controlDelay =
  ioProperty . withLock lock . withMLSBFromPSB seedKESPSB $ \seedKES -> do
    let seedDSIGNP = mkSeedFromBytes . psbToByteString $ seedDSIGNPSB
        expectedPeriod = 0
    hSetBuffering stdout LineBuffering
    expectedSK <- genKeyKES @IO @(KES c) seedKES
    let expectedSKP = SignKeyWithPeriodKES expectedSK expectedPeriod
    vkHot <- deriveVerKeyKES expectedSK
    let kesPeriod = KESPeriod 0
    let skCold = genKeyDSIGN @(DSIGN c) seedDSIGNP
        expectedOC = makeOCert vkHot certN kesPeriod skCold

    controlAddress <- justOrError $ socketAddressUnixAbstract (encodeUtf8 . Text.pack $ "kes-agent-control:" ++ show controlAddressStr)
    serviceAddress <- justOrError $ socketAddressUnixAbstract (encodeUtf8 . Text.pack $ "kes-agent-service:" ++ show serviceAddressStr)

    prop <- runTestNetwork p nullTracer controlAddress serviceAddress genesisTimestamp nodeDelay controlDelay
      (\sim -> sendKey sim (expectedSKP, expectedOC))
      (\(resultSKP, resultOC) finish -> do
          expectedSKBS <- rawSerialiseSignKeyKES expectedSK
          resultSKBS <- rawSerialiseSignKeyKES (skWithoutPeriodKES resultSKP)
          forgetSignKeyKES (skWithoutPeriodKES resultSKP)
          finish ((PrettyBS expectedSKBS, expectedPeriod) === (PrettyBS resultSKBS, periodKES resultSKP))
      )

    forgetSignKeyKES expectedSK
    return prop

  where
    genesisTimestamp = 1506203091

-- Show instances for signing keys violate mlocking guarantees, but for testing
-- purposes, this is fine, so we'll declare orphan instances here.
--
instance Show (SignKeyKES (SingleKES Ed25519DSIGNM)) where
  show (SignKeySingleKES (SignKeyEd25519DSIGNM mlsb)) =
    let hexstr = hexShowBS $ mlsbToByteString mlsb
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

-- | Utility function, should probably go somewhere else
hexShowBS :: ByteString -> String
hexShowBS = concatMap (printf "%02x") . BS.unpack

justOrError :: Maybe a -> IO a
justOrError Nothing = error "Nothing"
justOrError (Just a) = return a
