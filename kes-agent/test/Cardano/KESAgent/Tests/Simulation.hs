-- TODO: replace psbFromBytes with something non-deprecated and remove this pragma
{-# OPTIONS_GHC -Wno-deprecations#-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.KESAgent.Tests.Simulation
  ( Lock
  , mkLock
  , mvarPrettyTracer
  , tests
  , withLock
  ) where

import Cardano.KESAgent.Agent
import Cardano.KESAgent.Classes
import Cardano.KESAgent.ControlClient
import Cardano.KESAgent.Driver ( DriverTrace (..) )
import Cardano.KESAgent.Evolution
import Cardano.KESAgent.OCert
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.RefCounting
import Cardano.KESAgent.ServiceClient

import Cardano.Binary ( FromCBOR )
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.DSIGN.Ed25519
import Cardano.Crypto.Hash.Blake2b
import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.Single
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.Libsodium
import Cardano.Crypto.Libsodium.Memory.Internal ( MLockedForeignPtr (..) )
import Cardano.Crypto.Libsodium.MLockedBytes.Internal ( MLockedSizedBytes (..) )
import Cardano.Crypto.Libsodium.MLockedSeed
import Cardano.Crypto.PinnedSizedBytes
  ( PinnedSizedBytes
  , psbFromByteString
  , psbToByteString
  )
import Cardano.Crypto.Seed

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket
import Ouroboros.Network.Testing.Data.AbsBearerInfo hiding ( delay )
import Ouroboros.Network.Testing.Data.AbsBearerInfo qualified as ABI ( delay )

import Control.Monad ( forM, forM_, forever, void, when )
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadMVar
  ( MVar
  , MonadMVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , swapMVar
  , takeMVar
  , tryTakeMVar
  , withMVar
  )
import Control.Monad.Class.MonadST ( MonadST, withLiftST )
import Control.Monad.Class.MonadThrow
  ( MonadCatch
  , MonadThrow
  , SomeException
  , bracket
  , catch
  , catchJust
  , finally
  , throwIO
  )
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer ( MonadDelay, MonadTimer, threadDelay )
import Control.Monad.IOSim
import Control.Monad.Primitive ( PrimState )
import Control.Monad.ST ( ST, stToIO )
import Control.Monad.ST.Unsafe ( unsafeIOToST )
import Control.Tracer ( Tracer (..), nullTracer, traceWith )
import Data.ByteString ( ByteString )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Primitive
import Data.Proxy
import Data.Text qualified as Text
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Time
  ( DiffTime
  , NominalDiffTime
  , diffTimeToPicoseconds
  , picosecondsToDiffTime
  )
import Data.Time.Clock.POSIX ( getPOSIXTime, utcTimeToPOSIXSeconds )
import Data.Typeable
import Data.Word
import Debug.Trace
import Foreign ( free, mallocBytes )
import Foreign.C ( CSize )
import Foreign.ForeignPtr
  ( finalizeForeignPtr
  , mallocForeignPtrBytes
  , touchForeignPtr
  )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Ptr ( Ptr, castPtr )
import GHC.Stack ( HasCallStack )
import GHC.TypeLits ( KnownNat, Nat, natVal, type (*) )
import GHC.Types ( Type )
import Network.Mux.Channel qualified as Mux
import Network.Socket
import Simulation.Network.Snocket as SimSnocket
import System.Directory ( removeFile )
import System.IO
import System.IO.Error ( ioeGetErrorType, isDoesNotExistErrorType )
import System.IO.Unsafe
import System.IOManager
import System.Random
import System.Socket.Family.Unix
import Test.Crypto.Instances
import Test.QuickCheck ( Arbitrary (..), vectorOf )
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Printf ( printf )

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

data Lock (m :: Type -> Type)
  = Lock
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

testCrypto :: forall c kes
            . kes ~ KES c
           => MonadKES IO c
           => (forall s. MonadKES (IOSim s) c)
           => UnsoundKESAlgorithm kes
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
          testOneKeyThroughChainIO proxyC lock
      , testProperty "out-of-order pushes" $
          testOutOfOrderPushesIO proxyC lock
      , testProperty "concurrent pushes" $
          testConcurrentPushesIO proxyC lock
      ]
    , testGroup "IOSim"
      [ testProperty "one key through chain" $
          testOneKeyThroughChainIOSim proxyC
      , testProperty "out-of-order pushes" $
          testOutOfOrderPushesIOSim proxyC
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

mvarPrettyTracer :: (MonadTime m, MonadMVar m)
                 => MVar m [String]
                 -> (forall a. (Show a, TracePretty a) => Tracer m a)
mvarPrettyTracer var = Tracer $ \x -> modifyMVar_ var $ \strs -> do
  t <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let str = printf "%015.4f %s" (realToFrac t :: Double) (tracePretty x)
  return $ strs ++ [str]

mvarStringTracer :: (MonadTime m, MonadMVar m)
                 => MVar m [String]
                 -> Tracer m String
mvarStringTracer var = Tracer $ \x -> modifyMVar_ var $ \strs -> do
  t <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let str = printf "%015.4f %s" (realToFrac t :: Double) x
  return $ strs ++ [str]


class TracePretty a where
  tracePretty :: a -> String

strLength :: String -> Int
strLength = length

instance TracePretty AgentTrace where
  tracePretty (AgentServiceDriverTrace d) = "Agent: ServiceDriver: " ++ tracePretty d
  tracePretty (AgentControlDriverTrace d) = "Agent: ControlDriver: " ++ tracePretty d
  tracePretty (AgentServiceClientConnected a) = "Agent: ServiceClientConnected: " ++ a
  tracePretty (AgentServiceClientDisconnected a) = "Agent: ServiceClientDisconnected: " ++ a
  tracePretty (AgentServiceSocketError e) = "Agent: ServiceSocketError: " ++ e
  tracePretty (AgentControlClientConnected a) = "Agent: ControlClientConnected: " ++ a
  tracePretty (AgentControlClientDisconnected a) = "Agent: ControlClientDisconnected: " ++ a
  tracePretty (AgentControlSocketError e) = "Agent: ControlSocketError: " ++ e
  tracePretty x = "Agent: " ++ drop (strLength "Agent") (show x)

instance TracePretty ControlClientTrace where
  tracePretty (ControlClientDriverTrace d) = "Control: Driver: " ++ tracePretty d
  tracePretty ControlClientConnected = "Control: Connected"
  tracePretty x = "Control: " ++ drop (strLength "ControlClient") (show x)

instance TracePretty ServiceClientTrace where
  tracePretty (ServiceClientDriverTrace d) = "Service: Driver: " ++ tracePretty d
  tracePretty ServiceClientConnected = "Service: Connected"
  tracePretty x = "Service: " ++ drop (strLength "ServiceClient") (show x)

instance TracePretty DriverTrace where
  tracePretty x = drop (strLength "Driver") (show x)

instance TracePretty CRefEvent where
  tracePretty = show

-- | The operations a control client can perform: sending keys, and waiting.
data ControlClientHooks m c
  = ControlClientHooks
      { sendKey :: (CRef m (SignKeyWithPeriodKES (KES c)), OCert c) -> m ()
      , controlClientWait :: Int -> m ()
      }

-- | A control client script is just a monadic action that runs an arbitrary
-- number of actions.
type ControlClientScript m c =
  ControlClientHooks m c -> m ()

-- | The operations a control client can perform: reporting a property (this
-- will be forwarded to the test result), and waiting.
data NodeHooks m c
  = NodeHooks
      { reportProperty :: Property -> m ()
      , nodeWait :: Int -> m ()
      }

-- | A node script actually consists of two parts: the 'runNodeScript' action
-- runs concurrently in the background, allowing the node script to simulate
-- other activity, such as evolving keys, losing keys, etc.; the 'keyReceived'
-- action is invoked every time the node receives a key + opcert bundle.
-- Hook actions can be triggered from either script, as the use case requires.
data NodeScript m c
  = NodeScript
      { runNodeScript :: NodeHooks m c -> m ()
      , keyReceived :: NodeHooks m c -> (CRef m (SignKeyWithPeriodKES (KES c)), OCert c) -> m ()
      }

newtype PrettyBS
  = PrettyBS { unPrettyBS :: ByteString }
  deriving newtype (Eq)

instance Show PrettyBS where
  show = hexShowBS . unPrettyBS

{- HLINT ignore "Eta reduce" -}

class Monad m => WithTestAddress m addr where
  withTestAddress :: (addr -> m a) -> m a

instance WithTestAddress IO SockAddr where
  withTestAddress = withTestAddressIO

withTestAddressIO :: (SockAddr -> IO a) -> IO a
withTestAddressIO a =
  withPoolAddress ioAddressPool $ \i ->
    a (mkAddr i) `finally` cleanup i
  where
    mkAddrName i = "./local" ++ show i

    mkAddr :: Word -> SockAddr
    mkAddr i = SockAddrUnix $ mkAddrName i

    cleanup :: Word -> IO ()
    cleanup seed = do
      -- putStrLn $ "rm " ++ mkAddrName seed
      removeFile (mkAddrName seed) `catch`
        (\e ->
          if isDoesNotExistErrorType (ioeGetErrorType e)
            then do
              putStrLn $ mkAddrName seed ++ " does not exist"
            else do
              throwIO e
        )

withTestAddressIOSim :: forall s a. AddressPool (IOSim s) -> (TestAddress Int -> IOSim s a) -> IOSim s a
withTestAddressIOSim ap a =
  withPoolAddress ap $ \i -> a (TestAddress $ fromIntegral i)

type AddressPool m = MVar m AddressPoolData

data AddressPoolData
  = APD
      { poolBound :: Word
      , poolItems :: [Word]
      }

newAddressPool :: MonadMVar m => m (AddressPool m)
newAddressPool = newMVar (APD 0 [])

allocatePoolAddress :: (MonadMVar m, MonadST m) => AddressPool m -> m Word
allocatePoolAddress pool = do
  apd <- takeMVar pool
  let (taken, apd') = case poolItems apd of
        [] ->
          let next = poolBound apd
          in (next, apd { poolBound = succ next })
        x:xs ->
          (x, apd { poolItems = xs })
  putMVar pool apd'
  return taken

freePoolAddress :: MonadMVar m => AddressPool m -> Word -> m ()
freePoolAddress pool addr =
  modifyMVar_ pool (\apd -> return apd { poolItems = addr : poolItems apd })

withPoolAddress :: (MonadMVar m, MonadST m, MonadCatch m) => AddressPool m -> (Word -> m a) -> m a
withPoolAddress pool =
  bracket (allocatePoolAddress pool) (freePoolAddress pool)

{-# NOINLINE ioAddressPool #-}
ioAddressPool :: AddressPool IO
ioAddressPool = unsafePerformIO newAddressPool

instance Show (FD (IOSim s) (TestAddress Int)) where
  show _ = "???"

-- | Run a test network, consisting of one Agent, and an arbitrary number of scripted
-- control clients and nodes.
-- Control clients and nodes are defined as scripts (see 'NodeScript' and
-- 'ControlClientScript'), each with a startup delay, which allows us to randomize the
-- timing of the network components starting up and doing their things.
runTestNetwork :: forall c m fd addr
                . MonadKES m c
               => Show addr
               => Show fd
               => MonadTimer m
               => DSIGN.Signable (DSIGN c) (OCertSignable c)
               => ContextDSIGN (DSIGN c) ~ ()
               => Show (SignKeyWithPeriodKES (KES c))
               => Proxy c
               -> MakeRawBearer m fd
               -> (forall a. (Show a, TracePretty a) => Tracer m a)
               -> Snocket m fd addr
               -> Integer
               -> (forall a. (addr -> m a) -> m a)
               -> Int
                  -- | control clients: startup delay, script
               -> [(Int, ControlClientScript m c)]
                  -- | nodes: startup delay, script
               -> [(Int, NodeScript m c)]
               -> m Property
runTestNetwork p mrb tracer snocket genesisTimestamp
               withAddress
               agentDelay senders receivers = do
    propertyVar <- newEmptyMVar :: m (MVar m Property)
    timeVar <- newMVar (fromInteger genesisTimestamp) :: m (MVar m NominalDiffTime)
    withAddress $ \controlAddress -> do
      withAddress $ \serviceAddress -> do
        let agentOptions  :: AgentOptions m fd addr
            agentOptions = AgentOptions
                              { agentGenesisTimestamp = genesisTimestamp
                              , agentGetCurrentTime = readMVar timeVar
                              , agentControlAddr = controlAddress
                              , agentServiceAddr = serviceAddress
                              , agentSnocket = snocket
                              }

            -- Run the single agent.
            agent :: HasCallStack => Tracer m AgentTrace -> m ()
            agent tracer = runAgent p mrb agentOptions tracer

            -- Run one node.
            node :: HasCallStack
                 => Tracer m ServiceClientTrace
                 -> MVar m Property
                 -> (Int, NodeScript m c)
                 -> m ()
            node tracer mvar (startupDelay, script) = do
              let hooks = NodeHooks
                            { reportProperty = putMVar propertyVar
                            , nodeWait = threadDelay
                            }
              threadDelay startupDelay
              -- The node consists of two threads: the first runs the main or
              -- background activity, the second runs the networking bits and
              -- calls into 'keyReceived' upon receiving a key from the Agent.
              concurrently_
                (runNodeScript script hooks)
                (
                  runServiceClient p mrb
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

            -- | Run one control client script. The script may perform multiple
            -- interactions with the agent; a new control client will be spun up
            -- for each interaction.
            controlClient :: HasCallStack
                          => Tracer m ControlClientTrace
                          -> (Int, ControlClientScript m c)
                          -> m ()
            controlClient tracer (startupDelay, script) = do
              threadDelay startupDelay
              script $ ControlClientHooks
                { sendKey = \(sk, oc) -> do
                    runControlClient1 p mrb
                      ControlClientOptions
                        { controlClientSnocket = snocket
                        , controlClientAddress = controlAddress
                        , controlClientLocalAddress = Nothing
                        }
                      sk oc
                      tracer
                      `catch` (\(e :: AsyncCancelled) -> return ())
                      `catch` (\(e :: SomeException) -> traceWith tracer $ ControlClientAbnormalTermination ("CONTROL: " ++ show e))
                , controlClientWait = threadDelay
                }

            -- | Run all the nodes.
            nodes tracer mvar = forConcurrently_ receivers $ node tracer mvar

            -- | Run all the control clients.
            controlClients tracer = forConcurrently_ senders $ controlClient tracer

            -- | This is our watchdog; it will terminate as soon as property is
            -- pushed to the shared MVar.
            watch :: HasCallStack => MVar m a -> m a
            watch mvar = do
              takeMVar mvar

        output <- race
                -- run these to "completion"
                ( agent tracer `concurrently_`
                  nodes tracer propertyVar `concurrently_`
                  controlClients tracer
                )
                -- ...until this one finishes
                (watch propertyVar)

        case output of
            Left err   -> error ("EXCEPTION\n" ++ show err)
            Right prop -> return prop

-- | Run a generic test in IOSim
testIOSim :: ( forall s.
               MakeRawBearer (IOSim s) (FD (IOSim s) (TestAddress Int)) ->
               Snocket (IOSim s) (FD (IOSim s) (TestAddress Int)) (TestAddress Int) ->
               (forall a. (TestAddress Int -> IOSim s a) -> IOSim s a) ->
               IOSim s Property
             )
          -> Property
testIOSim test =
  iosimProperty $ do
    ap <- newAddressPool
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo $ absNoAttenuation { abiConnectionDelay = SmallDelay })
      mempty $ \snocket _observe -> do
        test
          (makeFDRawBearer nullTracer)
          snocket
          (withTestAddressIOSim ap)

-- | Run a generic test in IO
testIO :: Lock IO
       -> ( MakeRawBearer IO Socket ->
            Snocket IO Socket SockAddr ->
            (forall a. (SockAddr -> IO a) -> IO a) ->
            IO Property
          )
       -> Property
testIO lock test =
  ioProperty . withIOManager $ \ioManager ->
    withLock lock $ do
      test
        makeSocketRawBearer
        (socketSnocket ioManager)
        withTestAddressIO


testOneKeyThroughChainIO :: forall c
                          . MonadKES IO c
                       => ContextDSIGN (DSIGN c) ~ ()
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => Show (SignKeyWithPeriodKES (KES c))
                       => UnsoundKESAlgorithm (KES c)
                       => Proxy c
                       -> Lock IO
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
    seedKESPSB
    seedDSIGNPSB
    genesisTimestamp
    certN
    agentDelay
    nodeDelay
    controlDelay =
  testIO lock $ testOneKeyThroughChain
        p
        seedKESPSB
        seedDSIGNPSB
        genesisTimestamp
        certN
        agentDelay
        nodeDelay
        controlDelay

testOneKeyThroughChainIOSim :: forall c kes
                             . KES c ~ kes
                            => (forall s. MonadKES (IOSim s) c)
                            => (UnsoundKESAlgorithm kes)
                            => ContextDSIGN (DSIGN c) ~ ()
                            => DSIGN.Signable (DSIGN c) (OCertSignable c)
                            => Show (SignKeyWithPeriodKES  kes)
                            => (forall s. VersionedProtocol (KESProtocol (IOSim s) c))
                            => Proxy c

                            -> PinnedSizedBytes (SeedSizeKES (KES c))
                            -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                            -> Integer
                            -> Word64
                            -> Word
                            -> Word
                            -> Word
                            -> Property
testOneKeyThroughChainIOSim
    p
    seedKESPSB
    seedDSIGNPSB
    genesisTimestamp
    certN
    agentDelay
    nodeDelay
    controlDelay =
  testIOSim $ testOneKeyThroughChain
    p
    seedKESPSB
    seedDSIGNPSB
    genesisTimestamp
    certN
    agentDelay
    nodeDelay
    controlDelay

testOutOfOrderPushesIO :: forall c
                          . MonadKES IO c
                       => ContextDSIGN (DSIGN c) ~ ()
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => Show (SignKeyWithPeriodKES (KES c))
                       => UnsoundKESAlgorithm (KES c)
                       => Proxy c
                       -> Lock IO
                       -> [PinnedSizedBytes (SeedSizeKES (KES c))]
                       -> Int
                       -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                       -> Integer
                       -> Property
testOutOfOrderPushesIO proxyCrypto
    lock
    seedsKESRaw
    oooIndex
    seedDSIGNPSB
    genesisTimestamp =
  (length seedsKESRaw > 1) ==>
  testIO lock $ testOutOfOrderPushes
    proxyCrypto
    seedsKESRaw
    oooIndex
    seedDSIGNPSB
    genesisTimestamp

testOutOfOrderPushesIOSim :: forall c kes
                          . (forall s. MonadKES (IOSim s) c)
                         => ContextDSIGN (DSIGN c) ~ ()
                         => DSIGN.Signable (DSIGN c) (OCertSignable c)
                         => Show (SignKeyWithPeriodKES (KES c))
                         => UnsoundKESAlgorithm kes
                         => KES c ~ kes
                         => Proxy c
                         -> [PinnedSizedBytes (SeedSizeKES (KES c))]
                         -> Int
                         -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                         -> Integer
                         -> Property
testOutOfOrderPushesIOSim proxyCrypto
    seedsKESRaw
    oooIndex
    seedDSIGNPSB
    genesisTimestamp =
  (length seedsKESRaw > 1) ==>
  testIOSim $ testOutOfOrderPushes
    proxyCrypto
    seedsKESRaw
    oooIndex
    seedDSIGNPSB
    genesisTimestamp

testConcurrentPushesIO :: forall c
                          . MonadKES IO c
                       => ContextDSIGN (DSIGN c) ~ ()
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => Show (SignKeyWithPeriodKES (KES c))
                       => UnsoundKESAlgorithm (KES c)
                       => Proxy c
                       -> Lock IO
                       -> [(Word, PinnedSizedBytes (SeedSizeKES (KES c)))]
                       -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                       -> Integer
                       -> Word
                       -> Word
                       -> Property
testConcurrentPushesIO proxyCrypto
    lock
    controlDelaysAndSeedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    agentDelay
    nodeDelay =
  not (null controlDelaysAndSeedsKESPSB) ==>
  testIO lock $ testConcurrentPushes
    proxyCrypto
    controlDelaysAndSeedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    agentDelay
    nodeDelay

testConcurrentPushesIOSim :: forall c kes
                          . (forall s. MonadKES (IOSim s) c)
                         => ContextDSIGN (DSIGN c) ~ ()
                         => DSIGN.Signable (DSIGN c) (OCertSignable c)
                         => Show (SignKeyWithPeriodKES (KES c))
                         => UnsoundKESAlgorithm kes
                         => KES c ~ kes
                         => Proxy c
                         -> [(Word, PinnedSizedBytes (SeedSizeKES (KES c)))]
                         -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                         -> Integer
                         -> Word
                         -> Word
                         -> Property
testConcurrentPushesIOSim proxyCrypto
    controlDelaysAndSeedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    agentDelay
    nodeDelay =
  not (null controlDelaysAndSeedsKESPSB) ==>
  testIOSim $ testConcurrentPushes
    proxyCrypto
    controlDelaysAndSeedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    agentDelay
    nodeDelay

testOneKeyThroughChain :: forall c m fd addr
                        . MonadKES m c
                       => Show addr
                       => Show fd
                       => MonadTimer m
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => ContextDSIGN (DSIGN c) ~ ()
                       => Show (SignKeyWithPeriodKES (KES c))
                       => UnsoundKESAlgorithm (KES c)
                       => Proxy c
                       -> PinnedSizedBytes (SeedSizeKES (KES c))
                       -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                       -> Integer
                       -> Word64
                       -> Word
                       -> Word
                       -> Word
                       -> MakeRawBearer m fd
                       -> Snocket m fd addr
                       -> (forall a. (addr -> m a) -> m a)
                       -> m Property
testOneKeyThroughChain p
    seedKESPSB
    seedDSIGNPSB
    genesisTimestamp
    certN
    agentDelay
    nodeDelay
    controlDelay
    mrb
    snocket
    withAddress =
  withMLockedSeedFromPSB seedKESPSB $ \seedKES -> do
    traceMVar <- newMVar []
    crefTracker <- newCRefTracker
    let crefTracer = crtTracer crefTracker <> mvarPrettyTracer traceMVar
    let withNewCRef = withNewCRefWith crefTracer

    -- convert quickcheck-generated inputs into things we can use
    let seedDSIGNP = mkSeedFromBytes . psbToByteString $ seedDSIGNPSB
        expectedPeriod = 0
    expectedSK <- genKeyKES @(KES c) seedKES
    let expectedSKP = SignKeyWithPeriodKES expectedSK expectedPeriod
    expectedSKBS <- rawSerialiseSignKeyKES expectedSK

    withNewCRef (forgetSignKeyKES . skWithoutPeriodKES) expectedSKP $ \expectedSKPVar -> do
      vkHot <- deriveVerKeyKES expectedSK
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

      prop <- runTestNetwork p mrb
        (mvarPrettyTracer traceMVar)
        snocket
        genesisTimestamp
        withAddress
        (delayFromWord agentDelay)
        [(delayFromWord controlDelay, controlScript)]
        [(delayFromWord nodeDelay, nodeScript)]
      log <- takeMVar traceMVar
      crefCheck <- crtCheck crefTracker
      return $ counterexample (unlines log)
             $ prop .&. crefCheck

testConcurrentPushes :: forall c m n fd addr
                      . MonadKES m c
                     => Show addr
                     => Show fd
                     => MonadTimer m
                     => DSIGN.Signable (DSIGN c) (OCertSignable c)
                     => ContextDSIGN (DSIGN c) ~ ()
                     => Show (SignKeyWithPeriodKES (KES c))
                     => UnsoundKESAlgorithm (KES c)
                     => Proxy c
                     -> [(Word, PinnedSizedBytes (SeedSizeKES (KES c)))]
                     -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                     -> Integer
                     -> Word
                     -> Word
                     -> MakeRawBearer m fd
                     -> Snocket m fd addr
                     -> (forall a. (addr -> m a) -> m a)
                     -> m Property
testConcurrentPushes proxyCrypto
    controlDelaysAndSeedsKESPSB
    seedDSIGNPSB
    genesisTimestamp
    agentDelay
    nodeDelay
    mrb
    snocket
    withAddress =
  do
    traceMVar <- newMVar []
    crefTracker <- newCRefTracker
    let crefTracer = crtTracer crefTracker <> mvarPrettyTracer traceMVar
    let newCRef = newCRefWith crefTracer
    -- Debug.Trace.traceM $ "Concurrent pushes: " ++ show (length controlDelaysAndSeedsKESPSB)
    let seedDSIGNP = mkSeedFromBytes . psbToByteString $ seedDSIGNPSB
        expectedPeriod = 0
    let (controlDelays, seedsKESRaw) = unzip controlDelaysAndSeedsKESPSB
    let skCold = genKeyDSIGN @(DSIGN c) seedDSIGNP
    let strTracer :: Tracer m String = mvarStringTracer traceMVar

    forM_ controlDelays $ \delay -> traceWith strTracer $ "Control delay: " ++ show (delayFromWord delay)
    traceWith strTracer $ "Node delay: " ++ show (delayFromWord nodeDelay)
    traceWith strTracer $ "Agent delay: " ++ show (delayFromWord agentDelay)

    expectedSKOs <- forM (zip [0..] seedsKESRaw) $ \(certN, seedKESPSB) ->
        withMLockedSeedFromPSB seedKESPSB $ \seedKES -> do
          expectedSK <- genKeyKES @(KES c) seedKES
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

    let maxOCertN = fst $ last expectedSerialized

    let controlScript bundle hooks = do
          traceWith strTracer $ "Sending: " ++ (show . ocertN $ snd bundle)
          sendKey hooks bundle

    let nodeScript =
          NodeScript
            { keyReceived = \hooks (resultSKPVar, resultOC) -> do
                received <- withCRefValue resultSKPVar $ \resultSKP -> do
                                skp <- rawSerialiseSignKeyKES (skWithoutPeriodKES resultSKP)
                                return $ Just (PrettyBS skp, periodKES resultSKP)
                let actualOCertN = ocertN resultOC
                traceWith strTracer $ "Received: " ++ show actualOCertN
                let sent = lookup actualOCertN expectedSerialized
                traceWith strTracer $
                    show (ocertN resultOC) ++ "/" ++ show maxOCertN ++
                    (if ocertN resultOC == maxOCertN then "*" else "")
                when (ocertN resultOC == maxOCertN) $ do
                  traceWith strTracer "Received last key"
                  reportProperty hooks (sent === received)
            , runNodeScript = const $ return ()
            }

    traceWith strTracer "Start test network"

    let go = do
          prop <- runTestNetwork proxyCrypto
            mrb
            nullTracer -- (mvarPrettyTracer traceMVar)
            snocket
            genesisTimestamp
            withAddress
            (delayFromWord agentDelay)
            [ (delayFromWord controlDelay + 5000, controlScript sko)
            | (controlDelay, sko) <- zip controlDelays expectedSKOs
            ]
            [(delayFromWord nodeDelay + 5000, nodeScript)]
          traceWith strTracer "Finished test network"
          log <- readMVar traceMVar
          return $ (counterexample $ unlines log) prop

    let cleanup = mapM_ (releaseCRef . fst) expectedSKOs

    let timeout :: m ()
        timeout = do
          threadDelay . delayFromWord $ 5000000 + maximum (agentDelay : nodeDelay : controlDelays)

    result <- race timeout go `finally` cleanup
    case result of
      Left () -> do
        log <- readMVar traceMVar
        return $ counterexample "TIMEOUT"
               $ counterexample (unlines log)
               $ property False
      Right prop -> do
        log <- readMVar traceMVar
        crefCheck <- crtCheck crefTracker
        return $ counterexample (unlines log) 
               $ prop .&. crefCheck


testOutOfOrderPushes :: forall c m n fd addr
                      . MonadKES m c
                     => Show addr
                     => Show fd
                     => MonadTimer m
                     => DSIGN.Signable (DSIGN c) (OCertSignable c)
                     => ContextDSIGN (DSIGN c) ~ ()
                     => Show (SignKeyWithPeriodKES (KES c))
                     => UnsoundKESAlgorithm (KES c)
                     => Proxy c
                     -> [PinnedSizedBytes (SeedSizeKES (KES c))]
                     -> Int
                     -> PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                     -> Integer
                     -> MakeRawBearer m fd
                     -> Snocket m fd addr
                     -> (forall a. (addr -> m a) -> m a)
                     -> m Property
testOutOfOrderPushes proxyCrypto
    seedsKESRaw
    oooIndex
    seedDSIGNPSB
    genesisTimestamp
    mrb
    snocket
    withAddress =
  do
    traceMVar <- newMVar []
    crefTracker <- newCRefTracker
    let crefTracer = crtTracer crefTracker <> mvarPrettyTracer traceMVar
    let newCRef = newCRefWith crefTracer

    let seedDSIGNP = mkSeedFromBytes . psbToByteString $ seedDSIGNPSB
        expectedPeriod = 0
    let skCold = genKeyDSIGN @(DSIGN c) seedDSIGNP
    let strTracer :: Tracer m String = mvarStringTracer traceMVar

    sortedSKOs <- forM (zip [0..] seedsKESRaw) $ \(certN, seedKESPSB) ->
        withMLockedSeedFromPSB seedKESPSB $ \seedKES -> do
          expectedSK <- genKeyKES @(KES c) seedKES
          let expectedSKP = SignKeyWithPeriodKES expectedSK expectedPeriod
          expectedSKPVar <- newCRef (forgetSignKeyKES . skWithoutPeriodKES) expectedSKP
          vkHot <- deriveVerKeyKES expectedSK
          let kesPeriod = KESPeriod 0
              expectedOC = makeOCert vkHot certN kesPeriod skCold
          return (expectedSKPVar, expectedOC)

    -- Shuffle: swap any two adjacent SKs, such that the second one comes first
    let shuffleIndex = abs oooIndex `mod` (length sortedSKOs - 1)
    let shuffledSKOs =
          take shuffleIndex sortedSKOs ++ 
          (take 1 . drop (shuffleIndex + 1)) sortedSKOs ++
          (take 1 . drop shuffleIndex) sortedSKOs ++
          drop (shuffleIndex + 2) sortedSKOs
    let expectedSKOs =
          take shuffleIndex sortedSKOs ++ 
          (take 1 . drop (shuffleIndex + 1)) sortedSKOs ++
          drop (shuffleIndex + 2) sortedSKOs

    shuffledSerialized <- forM shuffledSKOs $ \(skpVar, oc) -> do
        withCRefValue skpVar $ \skp -> do
            serialized <- rawSerialiseSignKeyKES . skWithoutPeriodKES $ skp
            return (ocertN oc, (PrettyBS serialized, periodKES skp))
    expectedSerialized <- forM expectedSKOs $ \(skpVar, oc) -> do
        withCRefValue skpVar $ \skp -> do
            serialized <- rawSerialiseSignKeyKES . skWithoutPeriodKES $ skp
            return (ocertN oc, (PrettyBS serialized, periodKES skp))

    let maxOCertN = ocertN . snd $ last sortedSKOs

    let controlScript hooks = forM_ shuffledSKOs $ \bundle -> do
          threadDelay (delayFromWord 1000)
          traceWith strTracer $ "ControlScript Sending: " ++ (show . ocertN $ snd bundle)
          sendKey hooks bundle

    receivedVar <- newMVar []
    let nodeScript =
          NodeScript
            { keyReceived = \hooks (resultSKPVar, resultOC) -> do
                received <- withCRefValue resultSKPVar $ \resultSKP -> do
                                skp <- rawSerialiseSignKeyKES (skWithoutPeriodKES resultSKP)
                                return (PrettyBS skp, periodKES resultSKP)
                let actualOCertN = ocertN resultOC
                traceWith strTracer $ "NodeScript Received: " ++ show actualOCertN
                let sent = lookup actualOCertN expectedSerialized
                traceWith strTracer $
                    show (ocertN resultOC) ++ "/" ++ show maxOCertN ++
                    (if ocertN resultOC == maxOCertN then "*" else "")
                modifyMVar_ receivedVar $ \xs-> do
                  let actualSerialized = xs ++ [(ocertN resultOC, received)]
                  when (ocertN resultOC == maxOCertN) $ do
                    traceWith strTracer "Exit"
                    reportProperty hooks (actualSerialized === expectedSerialized)
                  return actualSerialized
            , runNodeScript = const $ return ()
            }

    traceWith strTracer "Start test network"

    let go = do
          prop <- runTestNetwork proxyCrypto
            mrb
            (mvarPrettyTracer traceMVar)
            snocket
            genesisTimestamp
            withAddress
            (delayFromWord 0)
            [ (delayFromWord 1000, controlScript)
            ]
            [(delayFromWord 2000, nodeScript)]
          traceWith strTracer "Finished test network"
          actualSerialized <- readMVar receivedVar
          return $ counterexample (show shuffledSerialized)
                 $ map fst actualSerialized === map fst expectedSerialized

    let cleanup = mapM_ (releaseCRef . fst) sortedSKOs

    let timeout :: m ()
        timeout = do
          threadDelay 10000000

    result <- race timeout go `finally` cleanup
    case result of
      Left () -> do
        log <- readMVar traceMVar
        actualSerialized <- readMVar receivedVar
        return $ counterexample "TIMEOUT"
               $ counterexample (unlines log)
               $ counterexample (show shuffledSerialized)
               $ property False
      Right prop -> do
        log <- readMVar traceMVar
        crefCheck <- crtCheck crefTracker
        return $ counterexample (unlines log) 
               $ prop .&. crefCheck

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
justOrError Nothing  = error "Nothing"
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

delayFromWord :: Word -> Int
delayFromWord wordDelay = fromIntegral wordDelay

data CRefTracker m =
  CRefTracker
    { crtResult :: m (Map CRefID CRefCount)
    , crtTracer :: Tracer m CRefEvent
    }

crtCheck :: Monad m => CRefTracker m -> m Property
crtCheck crefTracker = do
    ledger <- crtResult crefTracker
    let nonempty = Map.filter (/= 0) ledger
    return $ counterexample (show nonempty)
           $ Map.null nonempty

newCRefTracker :: MonadMVar m => m (CRefTracker m)
newCRefTracker = do
  trackerVar <- newMVar Map.empty
  return CRefTracker
    { crtResult = readMVar trackerVar
    , crtTracer = Tracer $ \ev ->
        modifyMVar_ trackerVar $
          case ev of
            CRefCreate cid count -> return . Map.insertWith (+) cid 1
            CRefAcquire cid count -> return . Map.insertWith (+) cid 1
            CRefRelease cid count -> return . Map.insertWith (+) cid (-1)
    }

