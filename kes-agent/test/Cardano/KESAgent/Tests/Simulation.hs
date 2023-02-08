-- TODO: replace psbFromBytes with something non-deprecated and remove this pragma
{-#OPTIONS_GHC -Wno-deprecations#-}

{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE DerivingStrategies #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE NoStarIsType #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE DataKinds #-}
module Cardano.KESAgent.Tests.Simulation
( tests
, Lock, withLock, mkLock
, elaborateTracer
, mvarPrettyTracer
)
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
import Cardano.Crypto.MonadMLock
import Cardano.Crypto.MLockedSeed
import Cardano.Crypto.PinnedSizedBytes
import Cardano.Crypto.Seed
import Cardano.Binary (FromCBOR)
-- import Test.Crypto.Util
import Test.Crypto.Instances
import Ouroboros.Network.Snocket
import Debug.Trace

import Control.Monad.Class.MonadTimer (MonadTimer, MonadDelay, threadDelay, DiffTime)
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadMVar
  ( MonadMVar
  , MVar
  , newMVar
  , newEmptyMVar
  , readMVar
  , putMVar
  , swapMVar
  , takeMVar
  , withMVar
  , modifyMVar_
  )
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadThrow, MonadCatch, bracket, catch, SomeException)
import Control.Monad (forever, void, forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy
import GHC.TypeLits (KnownNat, natVal, type (*))
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
import Data.Time (NominalDiffTime, picosecondsToDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import qualified Data.Text as Text
import Network.Socket
import Foreign.Ptr (castPtr)

import Cardano.KESAgent.Agent
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.OCert
import Cardano.KESAgent.ServiceClient
import Cardano.KESAgent.ControlClient
import Cardano.KESAgent.Driver (DriverTrace (..))
import Cardano.KESAgent.Evolution
import Cardano.KESAgent.RefCounting
import Cardano.KESAgent.DirectBearer

tests :: Lock IO
      -> (forall a. (Show a, TracePretty a) => Tracer IO a)
      -> Snocket IO Socket SockAddr
      -> TestTree
tests lock tracer snocket =
  testGroup "Simulation"
  [ testCrypto @MockCrypto @IO Proxy Proxy lock tracer snocket (SockAddrUnix . ("./local" ++) . show)
  , testCrypto @SingleCrypto @IO Proxy Proxy lock tracer snocket (SockAddrUnix . ("./local" ++) . show)
  , testCrypto @StandardCrypto @IO Proxy Proxy lock tracer snocket (SockAddrUnix . ("./local" ++) . show)
  ]

traceShowTee :: Show a => String -> IO a -> IO a
traceShowTee prefix a = do
  result <- a
  traceShowM result
  return result

instance ToDirectBearer IO Socket where
  toDirectBearer socket =
    DirectBearer
      { send = \buf bufsize ->
          fromIntegral <$> sendBuf socket (castPtr buf) (fromIntegral bufsize)
      , recv = \buf bufsize ->
          fromIntegral <$> recvBuf socket (castPtr buf) (fromIntegral bufsize)
      }

newtype Lock m = Lock { lockToMVar :: MVar m () }

withLock :: MonadMVar m => Lock m -> m a -> m a
withLock (Lock mv) = withMVar mv . const

mkLock :: MonadMVar m => m (Lock m)
mkLock = Lock <$> newMVar ()

testCrypto :: forall c m n fd addr
            . Crypto c
           => Typeable c
           => VersionedProtocol (KESProtocol m c)
           => UnsoundKESSignAlgorithm m (KES c)
           => ContextKES (KES c) ~ ()
           => DirectSerialise m (SignKeyKES (KES c))
           => DirectDeserialise m (SignKeyKES (KES c))
           => DSIGN.Signable (DSIGN c) (OCertSignable c)
           => ContextDSIGN (DSIGN c) ~ ()
           => n ~ 10
           => KnownNat (SeedSizeKES (KES c) * n)
           => Show (SignKeyWithPeriodKES (KES c))
           => Show addr
           => MonadMLock m
           => MonadAsync m
           => MonadMVar m
           => MonadProperty m
           => MonadST m
           => MonadThrow m
           => MonadCatch m
           => MonadFail m
           => MonadTime m
           => MonadTimer m
           => MonadByteStringMemory m
           => ToDirectBearer m fd
           => Proxy c
           -> Proxy m
           -> Lock m
           -> (forall a. (Show a, TracePretty a) => Tracer m a)
           -> Snocket m fd addr
           -> (Word32 -> addr)
           -> TestTree
testCrypto proxyC proxyM lock tracer snocket mkAddr =
  testGroup name
    [ testProperty "one key through chain" (testOneKeyThroughChain proxyC proxyM lock snocket mkAddr)
    , testProperty "concurrent pushes" (testConcurrentPushes proxyC proxyM (Proxy @n) lock snocket mkAddr)
    ]
  where
    name = Text.unpack .
           decodeUtf8 .
           unVersionIdentifier .
           versionIdentifier $ (Proxy @(KESProtocol m c))

{-# NOINLINE elaborateTracerLock #-}
elaborateTracerLock :: MVar IO ()
elaborateTracerLock = unsafePerformIO $ newMVar ()

elaborateTracer :: TracePretty a => Tracer IO a
elaborateTracer = Tracer $ \x -> withMVar elaborateTracerLock $ \() -> do
  t <- getPOSIXTime
  putStrLn $ printf "%015.4f %s" (realToFrac t :: Double) (tracePretty x)
  hFlush stdout

mvarPrettyTracer :: (MonadTime m, MonadMVar m)
                 => MVar m [String]
                 -> (forall a. (Show a, TracePretty a) => Tracer m a)
mvarPrettyTracer var = Tracer $ \x -> modifyMVar_ var $ \strs -> do
  t <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let str = printf "%015.4f %s" (realToFrac t :: Double) (tracePretty x)
  return $ strs ++ [str]
  

class TracePretty a where
  tracePretty :: a -> String

strLength :: String -> Int
strLength = length

instance TracePretty AgentTrace where
  tracePretty (AgentServiceDriverTrace d) = "Agent: ServiceDriver: " ++ tracePretty d
  tracePretty (AgentControlDriverTrace d) = "Agent: ControlDriver: " ++ tracePretty d
  tracePretty (AgentServiceClientConnected) = "Agent: ServiceClientConnected"
  tracePretty (AgentServiceClientDisconnected) = "Agent: ServiceClientDisconnected"
  tracePretty (AgentServiceSocketError e) = "Agent: ServiceSocketError: " ++ e
  tracePretty (AgentControlClientConnected) = "Agent: ControlClientConnected"
  tracePretty (AgentControlClientDisconnected) = "Agent: ControlClientDisconnected"
  tracePretty (AgentControlSocketError e) = "Agent: ControlSocketError: " ++ e
  tracePretty x = "Agent: " ++ (drop (strLength "Agent") (show x))

instance TracePretty ControlClientTrace where
  tracePretty (ControlClientDriverTrace d) = "Control: Driver: " ++ tracePretty d
  tracePretty (ControlClientConnected) = "Control: Connected"
  tracePretty x = "Control: " ++ (drop (strLength "ControlClient") (show x))

instance TracePretty ServiceClientTrace where
  tracePretty (ServiceClientDriverTrace d) = "Service: Driver: " ++ tracePretty d
  tracePretty (ServiceClientConnected) = "Service: Connected"
  tracePretty x = "Service: " ++ (drop (strLength "ServiceClient") (show x))

instance TracePretty DriverTrace where
  tracePretty x = drop (strLength "Driver") (show x)

data Simulator m c =
  Simulator
    { sendKey :: (CRef m (SignKeyWithPeriodKES (KES c)), OCert c) -> m ()
    , delay :: DiffTime -> m ()
    , setTime :: NominalDiffTime -> m ()
    }

newtype PrettyBS = PrettyBS { unPrettyBS :: ByteString }
  deriving newtype Eq

instance Show PrettyBS where
  show = hexShowBS . unPrettyBS

{- HLINT ignore "Eta reduce" -}

runTestNetwork :: forall c m fd addr
                . Crypto c
               => Typeable c
               => VersionedProtocol (KESProtocol m c)
               => KESSignAlgorithm m (KES c)
               => ContextKES (KES c) ~ ()
               => DirectSerialise m (SignKeyKES (KES c))
               => DirectDeserialise m (SignKeyKES (KES c))
               => DSIGN.Signable (DSIGN c) (OCertSignable c)
               => ContextDSIGN (DSIGN c) ~ ()
               => Show (SignKeyWithPeriodKES (KES c))
               => MonadMLock m
               => MonadAsync m
               => MonadMVar m
               => MonadTime m
               => MonadTimer m
               => MonadThrow m
               => MonadCatch m
               => MonadFail m
               => MonadST m
               => MonadByteStringMemory m
               => ToDirectBearer m fd
               => Proxy c
               -> (forall a. (Show a, TracePretty a) => Tracer m a)
               -> Snocket m fd addr
               -> addr
               -> addr
               -> Integer
               -> DiffTime
               -> DiffTime
               -> -- | A \"sender\" script, i.e., the control server
                  (Simulator m c -> m ())
               -> -- | A \"receiver\" job, which will receive keys as they
                  -- arrive at the Node.
                  ((CRef m (SignKeyWithPeriodKES (KES c)), OCert c) -> (Property -> m ()) -> m ())
               -> m Property
runTestNetwork p tracer snocket controlAddress serviceAddress genesisTimestamp nodeDelay controlDelay senderScript receiverScript = do
    propertyVar <- newEmptyMVar :: m (MVar m Property)
    timeVar <- newMVar (fromInteger genesisTimestamp) :: m (MVar m NominalDiffTime)

    let agentOptions  :: AgentOptions m fd addr
        agentOptions = AgentOptions
                          { agentGenesisTimestamp = genesisTimestamp
                          , agentGetCurrentTime = readMVar timeVar
                          , agentControlAddr = controlAddress
                          , agentServiceAddr = serviceAddress
                          , agentSnocket = snocket
                          }
        agent :: HasCallStack => Tracer m AgentTrace -> m ()
        agent tracer = runAgent p agentOptions tracer

        node :: HasCallStack
             => Tracer m ServiceClientTrace
             -> MVar m Property
             -> m ()
        node tracer mvar = do
          threadDelay nodeDelay
          runServiceClient p
            ServiceClientOptions
              { serviceClientSnocket = snocket
              , serviceClientAddress = serviceAddress
              }
            (\sk oc -> do
              receiverScript (sk, oc) (putMVar mvar)
            )
            tracer
            `catch` (\(e :: AsyncCancelled) -> return ())
            `catch` (\(e :: SomeException) -> traceWith tracer $ ServiceClientAbnormalTermination ("NODE: " ++ show e))

        simulator :: Simulator m c
        simulator = Simulator
          { sendKey = \(sk, oc) ->
              runControlClient1 p
                ControlClientOptions
                  { controlClientSnocket = snocket
                  , controlClientAddress = controlAddress
                  }
                sk oc
                tracer
                `catch` (\(e :: AsyncCancelled) -> return ())
                `catch` (\(e :: SomeException) -> traceWith tracer $ ControlClientAbnormalTermination ("CONTROL: " ++ show e))
          , delay = threadDelay
          , setTime = void . swapMVar timeVar
          }

        controlServer :: HasCallStack
                      => Tracer m ControlClientTrace
                      -> (Simulator m c -> m ())
                      -> m ()
        controlServer tracer script = do
          threadDelay controlDelay
          script simulator

        watch :: HasCallStack => MVar m a -> m a
        watch mvar = do
          takeMVar mvar

    output <- race
          -- abort 5 seconds after both clients have started
          (threadDelay $ 5 + max controlDelay nodeDelay)
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


testOneKeyThroughChain :: forall c m fd addr
                        . Crypto c
                       => Typeable c
                       => VersionedProtocol (KESProtocol m c)
                       => UnsoundKESSignAlgorithm m (KES c)
                       => ContextKES (KES c) ~ ()
                       => DirectSerialise m (SignKeyKES (KES c))
                       => DirectDeserialise m (SignKeyKES (KES c))
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => ContextDSIGN (DSIGN c) ~ ()
                       => Show (SignKeyWithPeriodKES (KES c))
                       => MonadProperty m
                       => MonadMVar m
                       => MonadMLock m
                       => MonadByteStringMemory m
                       => MonadST m
                       => MonadThrow m
                       => MonadCatch m
                       => MonadFail m
                       => MonadAsync m
                       => MonadTime m
                       => MonadTimer m
                       => ToDirectBearer m fd
                       => Show addr
                       => Proxy c
                       -> Proxy m
                       -> Lock m
                       -> Snocket m fd addr
                       -> (Word32 -> addr)
                       -> Word32
                       -> Word32
                       -> PinnedSizedBytes (SeedSizeKES (KES c))
                       -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                       -> Integer
                       -> Word64
                       -> Word
                       -> Word
                       -> Property
testOneKeyThroughChain p _proxyM
    lock
    snocket
    mkAddress
    controlAddressSeed
    serviceAddressSeed
    seedKESPSB
    seedDSIGNPSB
    genesisTimestamp
    certN
    nodeDelay
    controlDelay =
  controlAddressSeed /= serviceAddressSeed ==>
  mProperty . withLock lock . withMLockedSeedFromPSB seedKESPSB $ \seedKES -> do
    traceMVar <- newMVar []
    let controlAddress = mkAddress controlAddressSeed
    let serviceAddress = mkAddress serviceAddressSeed
    let seedDSIGNP = mkSeedFromBytes . psbToByteString $ seedDSIGNPSB
        expectedPeriod = 0
    expectedSK <- genKeyKES @m @(KES c) seedKES
    let expectedSKP = SignKeyWithPeriodKES expectedSK expectedPeriod
    expectedSKBS <- rawSerialiseSignKeyKES expectedSK

    withNewCRef (forgetSignKeyKES . skWithoutPeriodKES) expectedSKP $ \expectedSKPVar -> do
      vkHot <- deriveVerKeyKES @m expectedSK
      let kesPeriod = KESPeriod 0
      let skCold = genKeyDSIGN @(DSIGN c) seedDSIGNP
          expectedOC = makeOCert vkHot certN kesPeriod skCold

      prop <- runTestNetwork p (mvarPrettyTracer traceMVar) snocket
        controlAddress
        serviceAddress
        genesisTimestamp
        (picosecondsToDiffTime $ fromIntegral nodeDelay * 1000000)
        (picosecondsToDiffTime $ fromIntegral controlDelay * 1000000)
        (\sim -> sendKey sim (expectedSKPVar, expectedOC))
        (\(resultSKPVar, resultOC) finish -> do
            (resultSKBS, resultPeriod) <- withCRefValue resultSKPVar $ \resultSKP -> do
                            skp <- rawSerialiseSignKeyKES (skWithoutPeriodKES resultSKP)
                            return (skp, periodKES resultSKP)
            finish ((PrettyBS expectedSKBS, expectedPeriod) === (PrettyBS resultSKBS, resultPeriod))
        )
      log <- takeMVar traceMVar
      return $ (counterexample $ unlines log) prop

testConcurrentPushes :: forall c m n fd addr
                      . KnownNat n
                     => Crypto c
                     => Typeable c
                     => VersionedProtocol (KESProtocol m c)
                     => UnsoundKESSignAlgorithm m (KES c)
                     => ContextKES (KES c) ~ ()
                     => DirectSerialise m (SignKeyKES (KES c))
                     => DirectDeserialise m (SignKeyKES (KES c))
                     => DSIGN.Signable (DSIGN c) (OCertSignable c)
                     => ContextDSIGN (DSIGN c) ~ ()
                     => Show (SignKeyWithPeriodKES (KES c))
                     => MonadProperty m
                     => MonadMVar m
                     => MonadMLock m
                     => MonadByteStringMemory m
                     => MonadST m
                     => MonadThrow m
                     => MonadCatch m
                     => MonadFail m
                     => MonadAsync m
                     => MonadTime m
                     => MonadTimer m
                     => ToDirectBearer m fd
                     => Proxy c
                     -> Proxy m
                     -> Proxy n
                     -> Lock m
                     -> Snocket m fd addr
                     -> (Word32 -> addr)
                     -> Word32
                     -> Word32
                     -> PinnedSizedBytes (SeedSizeKES (KES c) * n)
                     -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                     -> Integer
                     -> Word
                     -> Word
                     -> Property
testConcurrentPushes proxyCrypto proxyM proxyN
    lock
    snocket
    mkAddress
    controlAddressSeed
    serviceAddressSeed
    seedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    nodeDelay
    controlDelay =
  controlAddressSeed /= serviceAddressSeed ==>
  mProperty . withLock lock $ do
    traceMVar <- newMVar []
    let controlAddress = mkAddress controlAddressSeed
    let serviceAddress = mkAddress serviceAddressSeed
    let seedDSIGNP = mkSeedFromBytes . psbToByteString $ seedDSIGNPSB
        expectedPeriod = 0
    let seedsKESBS = psbToByteString seedsKESPSB
        seedsKESRaw = map psbFromByteString $ chunksOfBS (fromIntegral $ natVal @(SeedSizeKES (KES c)) Proxy) seedsKESBS
    let skCold = genKeyDSIGN @(DSIGN c) seedDSIGNP

    expectedSKOs <- forM (zip [0..] seedsKESRaw) $ \(certN, seedKESPSB) ->
        withMLockedSeedFromPSB seedKESPSB $ \seedKES -> do
          expectedSK <- genKeyKES @m @(KES c) seedKES
          let expectedSKP = SignKeyWithPeriodKES expectedSK expectedPeriod
          expectedSKPVar <- newCRef (forgetSignKeyKES . skWithoutPeriodKES) expectedSKP
          vkHot <- deriveVerKeyKES expectedSK
          let kesPeriod = KESPeriod 0
              expectedOC = makeOCert vkHot certN kesPeriod skCold
          return (expectedSKPVar, expectedOC)

    expectedSerialized <- forM expectedSKOs $ \(skpVar, oc) -> do
        withCRefValue skpVar $ \skp -> do
            serialized <- rawSerialiseSignKeyKES . skWithoutPeriodKES $ skp
            return (ocertN oc, (PrettyBS serialized, periodKES skp))

    prop <- runTestNetwork proxyCrypto (mvarPrettyTracer traceMVar) snocket
              controlAddress
              serviceAddress
              genesisTimestamp
              (picosecondsToDiffTime $ fromIntegral nodeDelay * 1000000000)
              (picosecondsToDiffTime $ fromIntegral controlDelay * 1000000000)
      (\sim -> mapConcurrently_
                (sendKey sim)
                expectedSKOs)
      (\(resultSKPVar, resultOC) finish -> do
          received <- withCRefValue resultSKPVar $ \resultSKP -> do
                          skp <- rawSerialiseSignKeyKES (skWithoutPeriodKES resultSKP)
                          return $ Just (PrettyBS skp, periodKES resultSKP)
          let sent = lookup (ocertN resultOC) expectedSerialized
          finish (sent === received)
      )

    mapM_ (releaseCRef . fst) expectedSKOs
    log <- takeMVar traceMVar
    return $ (counterexample $ unlines log) prop

-- Show instances for signing keys violate mlocking guarantees, but for testing
-- purposes, this is fine, so we'll declare orphan instances here.
--
instance Show (SignKeyKES (SingleKES Ed25519DSIGNM)) where
  show (SignKeySingleKES (SignKeyEd25519DSIGNM mlsb)) =
    let hexstr = hexShowBS $ mlsbAsByteString mlsb
    in "SignKeySingleKES (SignKeyEd25519DSIGNM " ++ hexstr ++ ")"

instance Show (SignKeyKES d) => Show (SignKeyKES (SumKES h d)) where
  show (SignKeySumKES sk r vk0 vk1) = show sk

-- | Utility function, should probably go somewhere else
hexShowBS :: ByteString -> String
hexShowBS = concatMap (printf "%02x") . BS.unpack

justOrError :: Applicative m => Maybe a -> m a
justOrError Nothing = error "Nothing"
justOrError (Just a) = pure a

chunksOfBS :: Int -> ByteString -> [ByteString]
chunksOfBS _ "" = []
chunksOfBS n bs = BS.take n bs : chunksOfBS n (BS.drop n bs)

class MonadProperty m where
  mProperty :: forall p. Testable p => m p -> Property

instance MonadProperty IO where
  mProperty = ioProperty
