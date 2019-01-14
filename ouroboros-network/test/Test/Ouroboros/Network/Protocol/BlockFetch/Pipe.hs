{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ouroboros.Network.Protocol.BlockFetch.Pipe
  ( tests
  ) where

import           Control.Arrow ((***))
import           Control.Monad (void, unless)
import           Control.Monad.Free (Free)
import           Control.Monad.ST.Lazy (runST)
import           Data.ByteString (ByteString)
import           Data.Functor (($>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           System.Process (createPipe)

import qualified Pipes

import           Control.Monad.IOSim (SimF)

import           Protocol.Codec
import           Protocol.Channel
import           Protocol.Driver

import           Ouroboros.Network.Chain (Chain (..), HasHeader, Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Serialise hiding (liftST)
import           Ouroboros.Network.Pipe (pipeDuplex)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadProbe

import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Codec.Cbor

import           Ouroboros.Network.Testing.ConcreteBlock
import           Test.Chain (TestBlockChain (..))
import           Test.Ouroboros.Network.Testing.Arbitrary
import           Test.Ouroboros.Network.Testing.Utils (runExperiment, tmvarChannels)
import           Test.Ouroboros.Network.Protocol.BlockFetch.Client

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol.BlockFetch.Pipe"
  [ testProperty "PipeClientDemoIO" (ioProperty . prop_blockFetchClientDemoIO . map (getArbitraryPoint *** getArbitraryPoint))
  , testProperty "ClientDemoST" (prop_blockFetchClientDemoST . map (getArbitraryPoint *** getArbitraryPoint))
  , testProperty "PipeServerDemoIO" (ioProperty . uncurry prop_blockFetchServerDemoIO . fromChains . map getTestBlockChain)
  , testProperty "ServerDemoST" (uncurry prop_blockFetchServerDemoST . fromChains . map getTestBlockChain)
  ]

blockFetchClientDemo_experiment
  :: forall m header.
     ( MonadST m
     , MonadSTM m
     , MonadProbe m
     , HasHeader header
     , Serialise header
     , Eq header
     )
  => Duplex m m Encoding ByteString
  -> Duplex m m Encoding ByteString
  -> [(Point header, Point header)]
  -> Probe m Property
  -> m ()
blockFetchClientDemo_experiment cliChan serChan ranges probe = withLiftST @m $ \liftST -> do
  var     <- atomically $ newTVar []
  doneVar <- atomically $ newTVar False

  let client = blockFetchClientSenderFromProducer (Pipes.each ranges)
      server = constantReceiver (atomically . modifyTVar' var . (:)) (return ())

      cliPeer = blockFetchClientSenderStream client
      serPeer = blockFetchServerReceiverStream server

      codec = hoistCodec liftST blockFetchClientCodec

  fork $ do
    useCodecWithDuplex serChan codec serPeer >>= throwOnUnexpected "server" 
    results <- atomically $ readTVar var
    probeOutput probe (reverse results === ranges)
    atomically $ writeTVar doneVar True

  fork $ void (useCodecWithDuplex cliChan codec cliPeer >>= throwOnUnexpected "client")

  atomically $ do
    done <- readTVar doneVar
    unless done retry

throwOnUnexpected :: (Applicative m, Show fail) => String -> Result fail t -> m t
throwOnUnexpected id_ (Unexpected txt) = error $ id_ ++ " " ++ show txt
throwOnUnexpected _   (Normal t)       = pure t

prop_blockFetchClientDemoST
  :: ( HasHeader header
     , Serialise header
     , Eq header
     )
  => [(Point header, Point header)]
  -> Property
prop_blockFetchClientDemoST ranges =
  runST $ runExperiment $ \probe -> do
    (cliChan, serChan) <- tmvarChannels
    blockFetchClientDemo_experiment @(Free (SimF _)) cliChan serChan ranges probe

prop_blockFetchClientDemoIO
  :: ( HasHeader header
     , Serialise header
     , Eq header
     )
  => [(Point header, Point header)]
  -> IO Property
prop_blockFetchClientDemoIO ranges = runExperiment $ \probe -> do
  (serRead, cliWrite) <- createPipe
  (cliRead, serWrite) <- createPipe
  let cliChan = pipeDuplex cliRead cliWrite
      serChan = pipeDuplex serRead serWrite

  blockFetchClientDemo_experiment cliChan serChan ranges probe

blockFetchServerDemo_experiment
  :: forall m.
     ( MonadST m
     , MonadSTM m
     , MonadProbe m
     )
  => Duplex m m Encoding ByteString
  -> Duplex m m Encoding ByteString
  -> [(Point Block, Point Block)]
  -> ((Point Block, Point Block) -> [(Point Block, BlockBody)])
  -> Probe m Property
  -> m ()
blockFetchServerDemo_experiment cliChan serChan ranges blocks probe = withLiftST @m $ \liftST -> do

  var     <- atomically $ newTVar ranges
  doneVar <- atomically $ newTVar False

  let server :: BlockFetchServerSender (Point Block, BlockBody) m ()
      server = blockFetchServerSender
                (error "blockFetchServerSender: server must be lasy in the return value")
                (readRange var)
                (\range -> return $ Just $ Pipes.each (blocks range))
      client :: BlockFetchClientReceiver (Point Block, BlockBody) m [(Point Block, BlockBody)]
      client = blockFetchClientReceiver

      cliPeer = blockFetchClientReceiverStream client
      serPeer = blockFetchServerStream server

      codec = hoistCodec liftST blockFetchServerCodec

  fork $ void $ useCodecWithDuplex serChan codec serPeer >>= throwOnUnexpected "server"

  fork $ do
    results <- (useCodecWithDuplex cliChan codec cliPeer >>= throwOnUnexpected "client")
    let expected = concatMap blocks ranges
    probeOutput probe (reverse results === expected)
    atomically $ writeTVar doneVar True

  atomically $ do
    done <- readTVar doneVar
    unless done retry

 where
  readRange :: TVar m [range] -> m (StreamElement range)
  readRange v = atomically $ do
    rs <- readTVar v
    case rs of
      []         -> return End
      (r : rest) -> writeTVar v rest $> Element r

prop_blockFetchServerDemoST
  :: [(Point Block, Point Block)]
  -> ((Point Block, Point Block) -> [(Point Block, BlockBody)])
  -> Property
prop_blockFetchServerDemoST ranges blocks =
  runST $ runExperiment $ \probe -> do
    (cliChan, serChan) <- tmvarChannels
    blockFetchServerDemo_experiment cliChan serChan ranges blocks probe

prop_blockFetchServerDemoIO
  :: [(Point Block, Point Block)]
  -> ((Point Block, Point Block) -> [(Point Block, BlockBody)])
  -> IO Property
prop_blockFetchServerDemoIO ranges blocks = runExperiment $ \probe -> do
  (serRead, cliWrite) <- createPipe
  (cliRead, serWrite) <- createPipe
  let cliChan = pipeDuplex cliRead cliWrite
      serChan = pipeDuplex serRead serWrite

  blockFetchServerDemo_experiment cliChan serChan ranges blocks probe

fromChains
  :: [Chain Block]
  -> ( [(Point Block, Point Block)]
     , (Point Block, Point Block) -> [(Point Block, BlockBody)]
     )
fromChains = go ([], Map.empty)
 where
  go :: ( [(Point Block, Point Block)]
        , Map (Point Block, Point Block) [(Point Block, BlockBody)]
        )
     -> [Chain Block]
     -> ( [(Point Block, Point Block)]
        , (Point Block, Point Block) -> [(Point Block, BlockBody)]
        )
  go (ranges, bodies) []         = (ranges, fromMaybe [] . (bodies Map.!?))
  go (ranges, bodies) (chain : rest) = case chain of
    Genesis -> go (ranges, bodies) rest
    _ :> _  ->
      let range   = (Chain.genesisPoint, Chain.headPoint chain)
          bodies' = Chain.foldChain
            (\acc b -> (Chain.blockPoint b, blockBody b) : acc)
            [] chain
      in go (range : ranges, Map.insert range bodies' bodies) rest
