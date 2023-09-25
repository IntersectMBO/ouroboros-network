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
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Tests.Simulation
  ( Lock
  , mkLock
  , mvarPrettyTracer
  , tests
  , withLock
  ) where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Classes
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.Evolution
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Processes.Agent
import Cardano.KESAgent.Processes.ControlClient
import Cardano.KESAgent.Processes.ServiceClient
import Cardano.KESAgent.Protocols.Control.Peers
import Cardano.KESAgent.Protocols.Control.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting
import Cardano.KESAgent.Serialization.Spec

import Cardano.Binary ( FromCBOR )
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.DSIGN.Ed25519
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.Hash.Blake2b
import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.Single
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.Libsodium
import Cardano.Crypto.Libsodium.MLockedBytes.Internal ( MLockedSizedBytes (..) )
import Cardano.Crypto.Libsodium.MLockedSeed
import Cardano.Crypto.Libsodium.Memory.Internal ( MLockedForeignPtr (..) )
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

import Control.Concurrent.Class.MonadMVar
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
  , tryReadMVar
  , withMVar
  )
import Control.Monad ( forM, forM_, forever, void, when )
import Control.Monad.Class.MonadAsync
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
import Data.Coerce
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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
import Data.Time.Clock.POSIX
  ( getPOSIXTime
  , utcTimeToPOSIXSeconds
  , posixSecondsToUTCTime
  )
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
      -> (forall a. (Show a, Pretty a) => Tracer IO a)
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
           => Crypto c
           => NamedCrypto c
           => Show (SignKeyWithPeriodKES (KES c))
           => HasSerInfo (VerKeyKES (KES c))
           => HasSerInfo (SignKeyKES (KES c))
           => Proxy c
           -> Lock IO
           -> (forall a. (Show a, Pretty a) => Tracer IO a)
           -> IOManager
           -> TestTree
testCrypto proxyC lock tracer ioManager =
  testGroup name
    [ testGroup "IO"
      [ testProperty "one key through chain" $
          testIO lock (testOneKeyThroughChain proxyC)
      ]
    , testGroup "IOSim"
      [ testProperty "one key through chain" $
          testIOSim (testOneKeyThroughChain proxyC)
      ]
    ]
  where
    ioMkAddr i = SockAddrUnix $ "./local" ++ show i

    name = Text.unpack .
           decodeUtf8 .
           unCryptoName .
           cryptoName $ (Proxy @c)

mvarPrettyTracer :: (MonadTime m, MonadMVar m)
                 => MVar m [String]
                 -> (forall a. (Show a, Pretty a) => Tracer m a)
mvarPrettyTracer var = Tracer $ \x -> modifyMVar_ var $ \strs -> do
  t <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let str = printf "%015.4f %s" (realToFrac t :: Double) (pretty x)
  return $ strs ++ [str]

mvarStringTracer :: (MonadTime m, MonadMVar m)
                 => MVar m [String]
                 -> Tracer m String
mvarStringTracer var = Tracer $ \x -> modifyMVar_ var $ \strs -> do
  t <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let str = printf "%015.4f %s" (realToFrac t :: Double) x
  return $ strs ++ [str]


-- | The operations a control client can perform: sending keys, and waiting.
data ControlClientHooks m c
  = ControlClientHooks
      { controlClientExec :: forall a. ControlPeer c m a -> m a
      , controlClientWait :: Int -> m ()
      , controlClientReportProperty :: Property -> m ()
      }

-- | A control client script is just a monadic action that runs an arbitrary
-- number of actions.
type ControlClientScript m c =
  ControlClientHooks m c -> m ()

-- | The operations a control client can perform: reporting a property (this
-- will be forwarded to the test result), and waiting.
data NodeHooks m c
  = NodeHooks
      { nodeReportProperty :: Property -> m ()
      , nodeWait :: Int -> m ()
      , nodeDone :: m ()
      }

-- | A node script actually consists of two parts: the 'runNodeScript' action
-- runs concurrently in the background, allowing the node script to simulate
-- other activity, such as evolving keys, losing keys, etc.; the 'keyReceived'
-- action is invoked every time the node receives a key + opcert bundle.
-- Hook actions can be triggered from either script, as the use case requires.
data NodeScript m c
  = NodeScript
      { runNodeScript :: NodeHooks m c -> m ()
      , keyReceived :: NodeHooks m c -> Bundle m c -> m RecvResult
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
newAddressPool = newMVar (APD 1 [])

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

-- | Run a test network, consisting of one Agent, and an arbitrary number of
-- scripted control clients and nodes.
-- Control clients and nodes are defined as scripts (see 'NodeScript' and
-- 'ControlClientScript'), each with a startup delay, which allows us to
-- randomize the timing of the network components starting up and doing their
-- things.
-- The agent is initialized with a list of seeds, which it will use as a source
-- of "entropy" when generating KES keys. This makes it possible to compare KES
-- keys received by the node against the expected keys.
runTestNetwork :: forall c m fd addr
                . (forall x y. Coercible x y => Coercible (m x) (m y))
               => MonadKES m c
               => Show addr
               => Show fd
               => MonadTimer m
               => DSIGN.Signable (DSIGN c) (OCertSignable c)
               => ContextDSIGN (DSIGN c) ~ ()
               => Crypto c
               => NamedCrypto c
               => Show (SignKeyWithPeriodKES (KES c))
               => HasSerInfo (VerKeyKES (KES c))
               => HasSerInfo (SignKeyKES (KES c))
               => Proxy c
               -> MakeRawBearer m fd
               -> Snocket m fd addr
               -> Integer
               -> (forall a. (addr -> m a) -> m a)
               -> VerKeyDSIGN (DSIGN c)
               -> Int
                  -- | agent KES seeds
               -> [PinnedSizedBytes (SeedSizeKES (KES c))]
                  -- | control clients: startup delay, script
               -> [(Int, ControlClientScript m c)]
                  -- | nodes: startup delay, script
               -> [(Int, NodeScript m c)]
               -> m Property
runTestNetwork p mrb snocket genesisTimestamp
               withAddress coldVK
               agentDelay
               agentKESSeeds
               senders receivers = do
    traceMVar <- newMVar []
    let tracer :: forall a. (Show a, Pretty a) => Tracer m a
        tracer = mvarPrettyTracer traceMVar
    propertyVar <- newMVar (property True)
    doneVar <- newEmptyMVar :: m (MVar m ())
    timeVar <- newMVar (posixSecondsToUTCTime $ fromInteger genesisTimestamp) :: m (MVar m UTCTime)
    result <- withAddress $ \controlAddress -> do
      withAddress $ \serviceAddress -> do
        agentSeedVar <- newMVar agentKESSeeds

        let agentNextSeed :: m (MLockedSeed (SeedSizeKES (KES c)))
            agentNextSeed = do
              seeds <- takeMVar agentSeedVar
              (result, seeds') <- case seeds of
                [] ->
                  return (Nothing, [])
                (seed:seeds') -> do
                  traceWith tracer (PrettyStr $ "Consuming seed: " ++ (show . PrettyBS . psbToByteString $ seed))
                  result <- mlockedSeedFromPSB seed
                  return (Just result, seeds')
              putMVar agentSeedVar seeds'
              case result of
                Just seed -> return seed
                Nothing -> error "Out of entropy"

        let agentOptions  :: AgentOptions m addr c
            agentOptions = AgentOptions
                              { agentEvolutionConfig =
                                    EvolutionConfig
                                      { systemStart = posixSecondsToUTCTime (fromInteger genesisTimestamp)
                                      , slotsPerKESPeriod = 12900
                                      , slotLength = 1
                                      }
                              , agentGetCurrentTime = readMVar timeVar
                              , agentControlAddr = controlAddress
                              , agentServiceAddr = serviceAddress
                              , agentBootstrapAddr = []
                              , agentTracer = tracer
                              , agentColdVerKey = coldVK
                              , agentGenSeed = agentNextSeed
                              }

            -- Run the single agent.
            agent :: HasCallStack => Tracer m AgentTrace -> m ()
            agent tracer = do
              bracket
                (newAgent (Proxy @c) snocket mrb agentOptions)
                finalizeAgent
                runAgent

            reportProperty :: Property -> m ()
            reportProperty prop = modifyMVar_ propertyVar (return . (.&. prop))

            -- Run one node.
            node :: HasCallStack
                 => Tracer m ServiceClientTrace
                 -> (Int, NodeScript m c)
                 -> m ()
            node tracer (startupDelay, script) = do
              let hooks = NodeHooks
                            { nodeReportProperty = reportProperty
                            , nodeDone = putMVar doneVar ()
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
                    (\bundle -> do
                      keyReceived script hooks bundle
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
                { controlClientExec = \peer -> do
                    runControlClient1 peer
                      p mrb
                      ControlClientOptions
                        { controlClientSnocket = snocket
                        , controlClientAddress = controlAddress
                        , controlClientLocalAddress = Nothing
                        , controlClientRetryDelay = 1000
                        , controlClientRetryExponential = True
                        , controlClientRetryAttempts = 10
                        }
                      tracer
                      `catch` (\(e :: AsyncCancelled) -> throwIO e)
                      `catch` (\(e :: SomeException) -> do
                                  traceWith tracer $ ControlClientAbnormalTermination ("CONTROL: " ++ show e)
                                  throwIO e
                              )
                , controlClientWait = threadDelay
                , controlClientReportProperty = reportProperty
                }

            -- | Run all the nodes.
            nodes tracer = forConcurrently_ receivers $ node tracer

            -- | Run all the control clients.
            controlClients tracer = forConcurrently_ senders $ controlClient tracer

            -- | This is our watchdog; it will terminate as soon as a value is
            -- pushed to the shared MVar.
            watch :: HasCallStack => MVar m a -> m a
            watch mvar = do
              takeMVar mvar

        output <- race
                -- run these to "completion"
                ( agent tracer `concurrently_`
                  nodes tracer `concurrently_`
                  controlClients tracer
                )
                -- ...until we get a property, or the thread delay kicks in.
                (threadDelay 1000000 `race` watch doneVar)

        prop <- fromMaybe (counterexample "Interrupted" $ property False) <$>
                  tryReadMVar propertyVar
        case output of
          Left err ->
            error ("EXCEPTION\n" ++ show err)
          Right (Left ()) ->
            return $ counterexample "(KILLED)" $ prop
          Right (Right ()) -> do
            return prop

    log <- takeMVar traceMVar
    return $ counterexample (unlines log) result

-- | Run a generic test in IOSim
testIOSim :: forall params.
             ( forall s.
               MakeRawBearer (IOSim s) (FD (IOSim s) (TestAddress Int)) ->
               Snocket (IOSim s) (FD (IOSim s) (TestAddress Int)) (TestAddress Int) ->
               (forall a. (TestAddress Int -> IOSim s a) -> IOSim s a) ->
               params ->
               IOSim s Property
             )
          -> params
          -> Property
testIOSim test params =
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
          params

-- | Run a generic test in IO
testIO :: Lock IO
       -> ( MakeRawBearer IO Socket ->
            Snocket IO Socket SockAddr ->
            (forall a. (SockAddr -> IO a) -> IO a) ->
            params ->
            IO Property
          )
       -> params
       -> Property
testIO lock test params =
  ioProperty . withIOManager $ \ioManager ->
    withLock lock $ do
      test
        makeSocketRawBearer
        (socketSnocket ioManager)
        withTestAddressIO
        params

newtype OutOfOrderPushesSeeds c =
  OutOfOrderPushesSeeds { outOfOrderPushesSeeds :: [PinnedSizedBytes (SeedSizeKES (KES c))] }
  deriving newtype (Show)

instance (KESAlgorithm (KES c)) => Arbitrary (OutOfOrderPushesSeeds c) where
  arbitrary = resize 5 . fmap OutOfOrderPushesSeeds $ do
    a <- arbitrary
    b <- arbitrary
    xs <- arbitrary
    return $ a:b:xs
  shrink (OutOfOrderPushesSeeds xs) =
    map OutOfOrderPushesSeeds $ filter ((>= 2) . length) (shrink xs)

testOneKeyThroughChain :: forall c m fd addr
                        . (forall x y. Coercible x y => Coercible (m x) (m y))
                       => MonadKES m c
                       => Show addr
                       => Show fd
                       => MonadTimer m
                       => DSIGN.Signable (DSIGN c) (OCertSignable c)
                       => ContextDSIGN (DSIGN c) ~ ()
                       => Crypto c
                       => NamedCrypto c
                       => Show (SignKeyWithPeriodKES (KES c))
                       => UnsoundKESAlgorithm (KES c)
                       => HasSerInfo (VerKeyKES (KES c))
                       => HasSerInfo (SignKeyKES (KES c))
                       => Proxy c
                       -> MakeRawBearer m fd
                       -> Snocket m fd addr
                       -> (forall a. (addr -> m a) -> m a)
                       -> ( PinnedSizedBytes (SeedSizeKES (KES c))
                          , PinnedSizedBytes (SeedSizeDSIGN (DSIGN c))
                          , Integer
                          , Word64
                          , Word
                          , Word
                          , Word
                          )
                       -> m Property
testOneKeyThroughChain
    p mrb snocket withAddress
    ( seedKESPSB
    , seedDSIGNPSB
    , genesisTimestamp
    , certN
    , agentDelay
    , nodeDelay
    , controlDelay
    ) = do
  -- convert quickcheck-generated inputs into things we can use
  let seedDSIGNP = mkSeedFromBytes . psbToByteString $ seedDSIGNPSB
      expectedPeriod = 0

  expectedSK <- withMLockedSeedFromPSB seedKESPSB $ genKeyKES @(KES c)
  expectedSKBS <- rawSerialiseSignKeyKES expectedSK
  let expectedSKP = SignKeyWithPeriodKES expectedSK expectedPeriod

  expectedVK <- deriveVerKeyKES expectedSK
  let expectedVKBS = rawSerialiseVerKeyKES expectedVK

  vkHot <- deriveVerKeyKES expectedSK
  let kesPeriod = KESPeriod 0
  let skCold = genKeyDSIGN @(DSIGN c) seedDSIGNP
      vkCold = deriveVerKeyDSIGN skCold
      expectedOC = makeOCert vkHot certN kesPeriod skCold

  let controlScript hooks = do
        generatedVK <- controlClientExec hooks controlGenKey
        let generatedVKBS = rawSerialiseVerKeyKES <$> generatedVK
        controlClientReportProperty hooks $
          counterexample "Generated vs. expected VK:" $
          (PrettyBS <$> generatedVKBS) === Just (PrettyBS expectedVKBS)
        controlClientExec hooks $ controlInstallKey expectedOC
        return ()

  let nodeScript =
        NodeScript
          { keyReceived = \hooks (Bundle resultSKPVar resultOC) -> do
              (resultSKBS, resultPeriod) <- withCRefValue resultSKPVar $ \resultSKP -> do
                skp <- rawSerialiseSignKeyKES (skWithoutPeriodKES resultSKP)
                return (skp, periodKES resultSKP)
              let expectedPretty = PrettyBS expectedSKBS
                  resultPretty = PrettyBS resultSKBS
              nodeReportProperty hooks
                ((expectedPretty, expectedPeriod) === (resultPretty, resultPeriod))
              nodeDone hooks
              return RecvOK
          , runNodeScript = const $ return ()
          }

  runTestNetwork p mrb
     snocket
     genesisTimestamp
     withAddress
     vkCold
     (delayFromWord agentDelay)
     [seedKESPSB]
     [(delayFromWord controlDelay, controlScript)]
     [(delayFromWord nodeDelay, nodeScript)]

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
  trackerVar <- newMVar (Map.empty :: Map CRefID CRefCount)
  return CRefTracker
    { crtResult = readMVar trackerVar
    , crtTracer = Tracer $ \ev -> do
        (m :: Map CRefID CRefCount) <- takeMVar trackerVar
        (m' :: Map CRefID CRefCount) <- case creType ev of
                CRefCreate -> do
                  when (creCountAfter ev /= 1) (error "Invalid CRef count after creation")
                  case Map.lookup (creID ev) m of
                    Nothing -> return $ Map.insert (creID ev) 1 m
                    Just _ -> error $ "Duplicate CRefID " ++ show (creID ev)
                CRefAcquire -> do
                  case Map.lookup (creID ev) m of
                    Nothing -> error $ "Attempt to acquire nonexistent CRef " ++ show (creID ev)
                    Just v -> do
                      let v' = succ v
                      when (creCountBefore ev /= v) (error $ "CRef count mismatch before acquire; expected " ++ show (creCountBefore ev) ++ ", but found " ++ show v)
                      when (creCountAfter ev /= v') (error $ "CRef count mismatch after acquire; expected " ++ show (creCountAfter ev) ++ ", but found " ++ show v')
                      return $ Map.insert (creID ev) v' m
                CRefRelease -> do
                  case Map.lookup (creID ev) m of
                    Nothing -> error $ "Attempt to release nonexistent CRef " ++ show (creID ev)
                    Just v -> do
                      let v' = pred v
                      when (creCountBefore ev /= v) (error $ "CRef count mismatch before release; expected " ++ show (creCountBefore ev) ++ ", but found " ++ show v)
                      when (creCountAfter ev /= v') (error $ "CRef count mismatch after release; expected " ++ show (creCountAfter ev) ++ ", but found " ++ show v')
                      return $ Map.insert (creID ev) v' m
        putMVar trackerVar m'
    }

mlockedSeedFromPSB :: (MonadST m, KnownNat n) => PinnedSizedBytes n -> m (MLockedSeed n)
mlockedSeedFromPSB = fmap MLockedSeed . mlsbFromPSB

mlsbFromPSB :: (MonadST m, KnownNat n) => PinnedSizedBytes n -> m (MLockedSizedBytes n)
mlsbFromPSB = mlsbFromByteString . psbToByteString
