-- TODO: replace psbFromBytes with something non-deprecated and remove this pragma
{-#OPTIONS_GHC -Wno-deprecations#-}

{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DerivingStrategies #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE ImpredicativeTypes #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE NoStarIsType #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE QuantifiedConstraints #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeOperators #-}

module Cardano.KESAgent.Tests.Simulation
( tests
, Lock, withLock, mkLock
, elaborateTracer
, mvarPrettyTracer
)
where

import Control.Monad (forever, void, forM)
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
  , tryTakeMVar
  , withMVar
  , modifyMVar_
  )
import Control.Monad.Class.MonadST (MonadST, withLiftST)
import Control.Monad.Class.MonadThrow (MonadThrow, MonadCatch, bracket, catch, SomeException, catchJust)
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadSay (say)
import Control.Monad.Class.MonadTimer (MonadTimer, MonadDelay, threadDelay, DiffTime)
import Control.Monad.IOSim
import Control.Monad.Primitive (PrimState)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (NominalDiffTime, picosecondsToDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Typeable
import Data.Word
import Debug.Trace
import Foreign.Ptr (castPtr, Ptr)
import Foreign (mallocBytes, free)
import Foreign.ForeignPtr (mallocForeignPtrBytes, finalizeForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.C (CSize)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, natVal, type (*))
import GHC.Types (Type)
import Network.Socket
import System.IO
import System.IO.Unsafe
import System.IOManager
import System.Socket.Family.Unix
import Test.Crypto.Instances
import Test.QuickCheck (Arbitrary (..), vectorOf)
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Printf (printf)
import Simulation.Network.Snocket as SimSnocket
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistErrorType, ioeGetErrorType)
-- import Test.Crypto.Util
import Data.Primitive
import GHC.TypeLits (Nat)

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket
import Ouroboros.Network.Testing.Data.AbsBearerInfo hiding (delay)
import qualified Ouroboros.Network.Testing.Data.AbsBearerInfo as ABI (delay)

import Cardano.Binary (FromCBOR)
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.Ed25519
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.Hash.Blake2b
import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.Single
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.Libsodium
import Cardano.Crypto.Libsodium.MLockedBytes.Internal (MLockedSizedBytes (..))
import Cardano.Crypto.Libsodium.Memory.Internal (MLockedForeignPtr (..))
import Cardano.Crypto.MLockedSeed
import Cardano.Crypto.MonadMLock
import Cardano.Crypto.PinnedSizedBytes (psbFromByteString)
import Cardano.Crypto.Seed

import Cardano.KESAgent.Agent
import Cardano.KESAgent.Classes
import Cardano.KESAgent.ControlClient
import Cardano.KESAgent.Driver (DriverTrace (..))
import Cardano.KESAgent.Evolution
import Cardano.KESAgent.OCert
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.RefCounting
import Cardano.KESAgent.ServiceClient

tests :: Lock IO
      -> (forall a. (Show a, TracePretty a) => Tracer IO a)
      -> IOManager
      -> TestTree
tests lock tracer ioManager =
  testGroup "Simulation"
  [ testCrypto @MockCrypto
      Proxy lock tracer ioManager
  , testCrypto @SingleCrypto
      Proxy lock tracer ioManager
  , testCrypto @StandardCrypto
      Proxy lock tracer ioManager
  ]

traceShowTee :: Show a => String -> IO a -> IO a
traceShowTee prefix a = do
  result <- a
  traceShowM result
  return result

data Lock (m :: Type -> Type) =
  Lock
    { acquireLock :: m ()
    , releaseLock :: () -> m ()
    }

withLock :: MonadCatch m => Lock m -> m a -> m a
withLock lock = bracket (acquireLock lock) (releaseLock lock) . const

mkLock :: MonadMVar m => m (Lock m)
mkLock = do
  var <- newMVar ()
  return $ Lock
    { acquireLock = takeMVar var
    , releaseLock = putMVar var
    }

hoistLock :: forall m n. (forall a. m a -> n a) -> Lock m -> Lock n
hoistLock hoist (Lock acquire release) =
  Lock (hoist acquire) (\() -> hoist (release ()))

testCrypto :: forall c kes
            . kes ~ KES c
           => MonadKES IO c
           => (forall s. MonadKES (IOSim s) c)
           => UnsoundKESSignAlgorithm IO kes
           => (forall s. UnsoundKESSignAlgorithm (IOSim s) kes)
           => DSIGN.Signable (DSIGN c) (OCertSignable c)
           => ContextDSIGN (DSIGN c) ~ ()
           => Show (SignKeyWithPeriodKES (KES c))
           => Proxy c
           -> Lock IO
           -> (forall a. (Show a, TracePretty a) => Tracer IO a)
           -> IOManager
           -> TestTree
testCrypto proxyC lock tracer ioManager =
  testGroup name
    [ testGroup "IO"
      [ testProperty "one key through chain" $
          testOneKeyThroughChainIO proxyC lock ioManager
      , testProperty "concurrent pushes" $
          testConcurrentPushesIO proxyC lock ioManager
      ]
    , testGroup "IOSim"
      [ testProperty "one key through chain" $
          testOneKeyThroughChainIOSim proxyC
      , testProperty "concurrent pushes" $
          testConcurrentPushesIOSim proxyC
      ]
    ]
  where
    ioMkAddr i = SockAddrUnix $ "./local" ++ show i
    
    name = Text.unpack .
           decodeUtf8 .
           unVersionIdentifier .
           versionIdentifier $ (Proxy @(KESProtocol IO c))

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

-- | Bundles the operations a control client can perform
data ControlClientHooks m c =
  ControlClientHooks
    { sendKey :: (CRef m (SignKeyWithPeriodKES (KES c)), OCert c) -> m ()
    , controlClientWait :: DiffTime -> m ()
    }

data NodeHooks m c =
  NodeHooks
    { reportProperty :: Property -> m ()
    , nodeWait :: DiffTime -> m ()
    }

data NodeScript m c =
  NodeScript
    { runNodeScript :: NodeHooks m c -> m ()
    , keyReceived :: NodeHooks m c -> (CRef m (SignKeyWithPeriodKES (KES c)), OCert c) -> m ()
    }

type ControlClientScript m c =
  ControlClientHooks m c -> m ()

newtype PrettyBS = PrettyBS { unPrettyBS :: ByteString }
  deriving newtype Eq

instance Show PrettyBS where
  show = hexShowBS . unPrettyBS

{- HLINT ignore "Eta reduce" -}

runTestNetwork :: forall c m fd addr
                . MonadKES m c
               => MonadNetworking m fd
               => MonadTimer m
               => DSIGN.Signable (DSIGN c) (OCertSignable c)
               => ContextDSIGN (DSIGN c) ~ ()
               => Show (SignKeyWithPeriodKES (KES c))
               => Proxy c
               -> (forall a. (Show a, TracePretty a) => Tracer m a)
               -> Snocket m fd addr
               -> Integer
               -> addr -- | control address
               -> addr -- | service address
               -> DiffTime
                  -- | control clients: socket address, startup delay, script
               -> [(DiffTime, ControlClientScript m c)]
                  -- | nodes: socket address, startup delay, script
               -> [(DiffTime, NodeScript m c)]
               -> m Property
runTestNetwork p tracer snocket genesisTimestamp
               controlAddress serviceAddress
               agentDelay senders receivers = do
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
             -> (DiffTime, NodeScript m c)
             -> m ()
        node tracer mvar (startupDelay, script) = do
          let hooks = NodeHooks
                        { reportProperty = putMVar propertyVar
                        , nodeWait = threadDelay
                        }
          threadDelay startupDelay
          concurrently_
            (runNodeScript script hooks)
            (
              runServiceClient p
                ServiceClientOptions
                  { serviceClientSnocket = snocket
                  , serviceClientAddress = serviceAddress
                  }
                (\sk oc -> do
                  keyReceived script hooks (sk, oc)
                )
                tracer
                `catch` (\(e :: AsyncCancelled) -> return ())
                `catch` (\(e :: SomeException) -> traceWith tracer $ ServiceClientAbnormalTermination ("NODE: " ++ show e))
            )

        nodes tracer mvar = forConcurrently_ receivers $ node tracer mvar
        controlClients tracer = forConcurrently_ senders $ controlClient tracer

        controlClient :: HasCallStack
                      => Tracer m ControlClientTrace
                      -> (DiffTime, ControlClientScript m c)
                      -> m ()
        controlClient tracer (startupDelay, script) = do
          threadDelay startupDelay
          script $ ControlClientHooks
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
            , controlClientWait = threadDelay
            }

        watch :: HasCallStack => MVar m a -> m a
        watch mvar = do
          takeMVar mvar

    output <- race
          -- abort 5 seconds after all clients have started
          (threadDelay $ 5 + maximum (agentDelay : (map fst senders ++ map fst receivers)))
          (race
            -- run these to "completion"
            (agent tracer `concurrently_`
              nodes tracer propertyVar `concurrently_`
              controlClients tracer)
            -- ...until this one finishes
            (watch propertyVar)
          )

    case output of
        Left () -> error "TIMEOUT"
        Right (Left err) -> error ("EXCEPTION\n" ++ show err)
        Right (Right prop) -> return prop


testOneKeyThroughChainIO :: forall c
                          . MonadKES IO c
                       => ContextDSIGN (DSIGN c) ~ ()
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => Show (SignKeyWithPeriodKES (KES c))
                       => UnsoundKESSignAlgorithm IO (KES c)
                       => Proxy c
                       -> Lock IO
                       -> IOManager
                       -> Word32
                       -> Word32
                       -> PinnedSizedBytes (SeedSizeKES (KES c))
                       -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                       -> Integer
                       -> Word64
                       -> Word
                       -> Word
                       -> Word
                       -> Property
testOneKeyThroughChainIO p
    lock
    ioManager
    controlAddressSeed
    serviceAddressSeed
    seedKESPSB
    seedDSIGNPSB
    genesisTimestamp
    certN
    agentDelay
    nodeDelay
    controlDelay =
  controlAddressSeed /= serviceAddressSeed ==>
  ioProperty . withLock lock $ do
      result <- testOneKeyThroughChain
                  p
                  ioSnocket
                  ioMkAddr
                  controlAddressSeed
                  serviceAddressSeed
                  seedKESPSB
                  seedDSIGNPSB
                  genesisTimestamp
                  certN
                  agentDelay
                  nodeDelay
                  controlDelay
      cleanUp controlAddressSeed
      cleanUp serviceAddressSeed
      return result
  where
    ioSnocket = socketSnocket ioManager
    ioMkAddrName i = "./local" ++ show i
    ioMkAddr i = SockAddrUnix $ ioMkAddrName i

    cleanUp seed = do
        catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
                  (removeFile $ ioMkAddrName seed)
                  (\_ -> return ())

testOneKeyThroughChainIOSim :: forall c kes
                             . KES c ~ kes
                            => (forall s. MonadKES (IOSim s) c)
                            => (forall s. UnsoundKESSignAlgorithm (IOSim s) kes)
                            => ContextDSIGN (DSIGN c) ~ ()
                            => DSIGN.Signable (DSIGN c) (OCertSignable c)
                            => Show (SignKeyWithPeriodKES  kes)
                            => (forall s. VersionedProtocol (KESProtocol (IOSim s) c))
                            => Proxy c
                            -> Word32
                            -> Word32
                            -> PinnedSizedBytes (SeedSizeKES (KES c))
                            -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                            -> Integer
                            -> Word64
                            -> Word
                            -> Word
                            -> Word
                            -> Property
testOneKeyThroughChainIOSim p
    controlAddressSeed
    serviceAddressSeed
    seedKESPSB
    seedDSIGNPSB
    genesisTimestamp
    certN
    agentDelay
    nodeDelay
    controlDelay =
  controlAddressSeed /= serviceAddressSeed ==>
  iosimProperty $
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo absNoAttenuation)
      mempty $ \snocket _observe -> do
        testOneKeyThroughChain
          p
          snocket
          (TestAddress . (fromIntegral :: Word32 -> Int))
          controlAddressSeed
          serviceAddressSeed
          seedKESPSB
          seedDSIGNPSB
          genesisTimestamp
          certN
          agentDelay
          nodeDelay
          controlDelay

testOneKeyThroughChain :: forall c m fd addr
                        . MonadKES m c
                       => MonadNetworking m fd
                       => MonadTimer m
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => ContextDSIGN (DSIGN c) ~ ()
                       => Show (SignKeyWithPeriodKES (KES c))
                       => UnsoundKESSignAlgorithm m (KES c)
                       => Proxy c
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
                       -> Word
                       -> m Property
testOneKeyThroughChain p
    snocket
    mkAddress
    controlAddressSeed
    serviceAddressSeed
    seedKESPSB
    seedDSIGNPSB
    genesisTimestamp
    certN
    agentDelay
    nodeDelay
    controlDelay =
  withMLockedSeedFromPSB seedKESPSB $ \seedKES -> do
    traceMVar <- newMVar []

    -- convert quickcheck-generated inputs into things we can use
    let controlAddress = mkAddress controlAddressSeed
    let serviceAddress = mkAddress serviceAddressSeed
    let agentDelayDT = (picosecondsToDiffTime $ fromIntegral agentDelay * 1000000)
    let nodeDelayDT = (picosecondsToDiffTime $ fromIntegral nodeDelay * 1000000)
    let controlDelayDT = (picosecondsToDiffTime $ fromIntegral controlDelay * 1000000)
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

      let controlScript sim = sendKey sim (expectedSKPVar, expectedOC)

      let nodeScript =
            NodeScript
              { keyReceived = \hooks (resultSKPVar, resultOC) -> do
                  (resultSKBS, resultPeriod) <- withCRefValue resultSKPVar $ \resultSKP -> do
                    skp <- rawSerialiseSignKeyKES (skWithoutPeriodKES resultSKP)
                    return (skp, periodKES resultSKP)
                  reportProperty hooks
                    ((PrettyBS expectedSKBS, expectedPeriod) === (PrettyBS resultSKBS, resultPeriod))
              , runNodeScript = const $ return ()
              }

      prop <- runTestNetwork p
        (mvarPrettyTracer traceMVar)
        snocket
        genesisTimestamp
        controlAddress
        serviceAddress
        agentDelayDT
        [(controlDelayDT, controlScript)]
        [(nodeDelayDT, nodeScript)]
      log <- takeMVar traceMVar
      return $ (counterexample $ unlines log) prop

testConcurrentPushesIO :: forall c
                          . MonadKES IO c
                       => ContextDSIGN (DSIGN c) ~ ()
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => Show (SignKeyWithPeriodKES (KES c))
                       => UnsoundKESSignAlgorithm IO (KES c)
                       => Proxy c
                       -> Lock IO
                       -> IOManager
                       -> Word32
                       -> Word32
                       -> [(Word, PinnedSizedBytes (SeedSizeKES (KES c)))]
                       -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                       -> Integer
                       -> Word
                       -> Word
                       -> Property
testConcurrentPushesIO proxyCrypto
    lock
    ioManager
    controlAddressSeed
    serviceAddressSeed
    controlDelaysAndSeedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    agentDelay
    nodeDelay =
  controlAddressSeed /= serviceAddressSeed ==>
  not (null controlDelaysAndSeedsKESPSB) ==>
  ioProperty . withLock lock $ do
      result <- testConcurrentPushes
                  proxyCrypto
                  ioSnocket
                  ioMkAddr
                  controlAddressSeed
                  serviceAddressSeed
                  controlDelaysAndSeedsKESPSB
                  seedDSIGNPSB
                  genesisTimestamp
                  agentDelay
                  nodeDelay
      cleanUp controlAddressSeed
      cleanUp serviceAddressSeed
      return result
  where
    ioSnocket = socketSnocket ioManager
    ioMkAddrName i = "./local" ++ show i
    ioMkAddr i = SockAddrUnix $ ioMkAddrName i

    cleanUp seed = do
        catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
                  (removeFile $ ioMkAddrName seed)
                  (\_ -> return ())

testConcurrentPushesIOSim :: forall c k
                          . (forall s. MonadKES (IOSim s) c)
                         => ContextDSIGN (DSIGN c) ~ ()
                         => DSIGN.Signable (DSIGN c) (OCertSignable c)
                         => Show (SignKeyWithPeriodKES (KES c))
                         => (forall s. UnsoundKESSignAlgorithm (IOSim s) k)
                         => KES c ~ k
                         => Proxy c
                         -> Word32
                         -> Word32
                         -> [(Word, PinnedSizedBytes (SeedSizeKES (KES c)))]
                         -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                         -> Integer
                         -> Word
                         -> Word
                         -> Property
testConcurrentPushesIOSim proxyCrypto
    controlAddressSeed
    serviceAddressSeed
    controlDelaysAndSeedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    agentDelay
    nodeDelay =
  controlAddressSeed /= serviceAddressSeed ==>
  not (null controlDelaysAndSeedsKESPSB) ==>
  iosimProperty $
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo absNoAttenuation)
      mempty $ \snocket _observe -> do
        testConcurrentPushes
            proxyCrypto
            snocket
            (TestAddress . (fromIntegral :: Word32 -> Int))
            controlAddressSeed
            serviceAddressSeed
            controlDelaysAndSeedsKESPSB
            seedDSIGNPSB
            genesisTimestamp
            agentDelay
            nodeDelay

testConcurrentPushes :: forall c m n fd addr
                      . MonadKES m c
                     => MonadNetworking m fd
                     => MonadTimer m
                     => DSIGN.Signable (DSIGN c) (OCertSignable c)
                     => ContextDSIGN (DSIGN c) ~ ()
                     => Show (SignKeyWithPeriodKES (KES c))
                     => UnsoundKESSignAlgorithm m (KES c)
                     => Proxy c
                     -> Snocket m fd addr
                     -> (Word32 -> addr)
                     -> Word32
                     -> Word32
                     -> [(Word, PinnedSizedBytes (SeedSizeKES (KES c)))]
                     -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                     -> Integer
                     -> Word
                     -> Word
                     -> m Property
testConcurrentPushes proxyCrypto
    snocket
    mkAddress
    controlAddressSeed
    serviceAddressSeed
    controlDelaysAndSeedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    agentDelay
    nodeDelay =
  do
    traceMVar <- newMVar []
    let controlAddress = mkAddress controlAddressSeed
    let serviceAddress = mkAddress serviceAddressSeed
    let seedDSIGNP = mkSeedFromBytes . psbToByteString $ seedDSIGNPSB
        expectedPeriod = 0
    let (controlDelays, seedsKESRaw) = unzip controlDelaysAndSeedsKESPSB
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

    let controlScript bundle hooks = sendKey hooks bundle

    let nodeScript =
          NodeScript

            { keyReceived = \hooks (resultSKPVar, resultOC) -> do
                received <- withCRefValue resultSKPVar $ \resultSKP -> do
                                skp <- rawSerialiseSignKeyKES (skWithoutPeriodKES resultSKP)
                                return $ Just (PrettyBS skp, periodKES resultSKP)
                let sent = lookup (ocertN resultOC) expectedSerialized
                reportProperty hooks (sent === received)
            , runNodeScript = const $ return ()
            }

    prop <- runTestNetwork proxyCrypto
      (mvarPrettyTracer traceMVar)
      snocket
      genesisTimestamp
      controlAddress
      serviceAddress
      (delayFromWord agentDelay)
      [ (delayFromWord controlDelay, controlScript sko)
      | (controlDelay, sko) <- zip controlDelays expectedSKOs
      ]
      [(delayFromWord nodeDelay, nodeScript)]

    mapM_ (releaseCRef . fst) expectedSKOs
    log <- takeMVar traceMVar
    return $ (counterexample $ unlines log) prop

-- Show instances for signing keys violate mlocking guarantees, but for testing
-- purposes, this is fine, so we'll declare orphan instances here.
--
instance Show (SignKeyKES (SingleKES Ed25519DSIGN)) where
  show (SignKeySingleKES (SignKeyEd25519DSIGNM mlsb)) =
    let hexstr = hexShowBS $ unsafePerformIO (mlsbToByteString mlsb)
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

iosimProperty :: (forall s . IOSim s Property)
              -> Property
iosimProperty sim =
  let tr = runSimTrace sim
   in case traceResult True tr of
     Left e -> counterexample
                (unlines
                  [ "=== Say Events ==="
                  , unlines (selectTraceEventsSay' tr)
                  , "=== Trace Events ==="
                  , unlines (show `map` traceEvents tr)
                  , "=== Error ==="
                  , show e ++ "\n"
                  ])
                False
     Right prop -> prop

-- TODO: should be defined where 'AbsBearerInfo' is defined, but it also
-- requires access to 'ouroboros-network-framework' where 'BearerInfo' is
-- defined.  This can be fixed by moving `ouroboros-network-testing` to
-- `ouroboros-network-framework:testlib`.
--
toBearerInfo :: AbsBearerInfo -> BearerInfo
toBearerInfo abi =
    BearerInfo {
        biConnectionDelay      = ABI.delay (abiConnectionDelay abi),
        biInboundAttenuation   = attenuation (abiInboundAttenuation abi),
        biOutboundAttenuation  = attenuation (abiOutboundAttenuation abi),
        biInboundWriteFailure  = abiInboundWriteFailure abi,
        biOutboundWriteFailure = abiOutboundWriteFailure abi,
        biAcceptFailures       = (\(errDelay, errType) ->
                                   ( ABI.delay errDelay
                                   , case errType of
                                      AbsIOErrConnectionAborted -> IOErrConnectionAborted
                                      AbsIOErrResourceExhausted -> IOErrResourceExhausted
                                   )
                                 ) <$> abiAcceptFailure abi,
        biSDUSize              = toSduSize (abiSDUSize abi)
      }

instance MonadUnmanagedMemory (IOSim s) where
  zeroMem ptr size = unsafeIOToIOSim $ zeroMem ptr size
  copyMem dst src size = unsafeIOToIOSim $ copyMem dst src size
  allocaBytes size action =
    bracket
      (unsafeIOToIOSim $ mallocBytes size)
      (unsafeIOToIOSim . free)
      action

unsafeIOToIOSim :: IO a -> IOSim s a
unsafeIOToIOSim ioAction = liftST (unsafeIOToST ioAction)

instance MonadPSB (IOSim s) where
  psbUseAsCPtrLen psb action =
    allocaBytes (fromInteger size) $ \ptr -> do
      unsafeIOToIOSim $ do
        psbUseAsCPtrLen psb $ \src _ -> copyMem (castPtr ptr) (castPtr src) (fromInteger size)
      r <- action (castPtr ptr) (fromInteger size)
      unsafeIOToIOSim $ do
        psbUseAsCPtrLen psb $ \src _ -> copyMem (castPtr src) (castPtr ptr) (fromInteger size)
      return r
    where
      size = natVal psb
  psbCreateResultLen initialize = do
    psb <- unsafeIOToIOSim $ psbCreate (\_ -> return ())
    r <- psbUseAsCPtrLen psb initialize
    return (psb, r)

instance MonadByteStringMemory (IOSim s) where
  useByteStringAsCStringLen bs action =
    allocaBytes (fromIntegral size) $ \ptr -> do
      unsafeIOToIOSim $ do
        useByteStringAsCStringLen bs $ \(src, _) -> copyMem (castPtr ptr) (castPtr src) (fromIntegral size)
      r <- action (castPtr ptr, fromIntegral size)
      unsafeIOToIOSim $ do
        useByteStringAsCStringLen bs $ \(src, _) -> copyMem (castPtr src) (castPtr ptr) (fromIntegral size)
      return r
    where
      size = BS.length bs

instance MonadMLock (IOSim s) where
  mlockedMalloc size = do
    say $ "Allocating " ++ show size ++ " mlocked byte(s)"
    liftST . unsafeIOToST . fmap SFP $ mallocForeignPtrBytes (fromIntegral size)
  finalizeMLockedForeignPtr (SFP fp) = do
    say "Finalize"
    liftST . unsafeIOToST $ finalizeForeignPtr fp

  withMLockedForeignPtr (SFP fp) action = do
    result <- action $ unsafeForeignPtrToPtr fp
    liftST . unsafeIOToST $ touchForeignPtr fp
    return result

  traceMLockedForeignPtr mlp =
    withMLockedForeignPtr mlp $ \ptr ->
      say (show ptr)

delayFromWord :: Word -> DiffTime
delayFromWord wordDelay = picosecondsToDiffTime $ fromIntegral wordDelay * 1000000000
