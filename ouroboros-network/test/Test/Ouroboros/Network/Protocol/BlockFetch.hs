{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Ouroboros.Network.Protocol.BlockFetch where

import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.IOSim (runSim)
import           Control.Monad.Class.MonadST (MonadST (..))
import           Control.Monad.Class.MonadSTM (MonadSTM)
import           Control.Monad.Class.MonadAsync (MonadAsync (..))

import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Proofs
import           Network.TypedProtocol.Channel

import           Ouroboros.Network.Chain (Chain, Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Channel (createPipeConnectedChannels)
import           Ouroboros.Network.Testing.ConcreteBlock (Block, BlockBody)
import qualified Ouroboros.Network.Testing.ConcreteBlock as ConcreteBlock

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Direct
import           Ouroboros.Network.Protocol.BlockFetch.Examples
import           Ouroboros.Network.Protocol.BlockFetch.Codec

import           Test.Chain (TestChainAndPoints (..))

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.BlockFetch"
  [ testProperty "direct"  prop_direct
  , testProperty "connect" prop_connect
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testProperty "pipe IO"             prop_pipe_IO
  ]

--
-- Auxilary functions
--

-- | Generate a list of @ChainRange@s from a list of points on a chain.  The
-- the ranges which both ends are on the chain are disjoint.
--
pointsToRanges
  :: forall block.
     Chain.HasHeader block
  => Chain block
  -> [Point block]
  -> [ChainRange block]
pointsToRanges chain points = go (reverse points)
 where
  go (x : y : ys) =
    if x `Chain.pointOnChain` chain -- otherwise `Chain.successorBlock` will error
      then case Chain.successorBlock x chain of
        Nothing -> ChainRange x y : go (y : ys)
        Just x' -> ChainRange (Chain.blockPoint x') y : go (y : ys)
      else ChainRange x y : go (y : ys)
  go [x] = [ChainRange Chain.genesisPoint x]
  go []  = []

-- | Compute list of received block bodies from a chain and points.
-- This is the reference function against which we compare block-fetch
-- protocol.  The @'ponitsToRanges'@ function is used to compute the ranges,
-- and then the results are then read from the chain directly.  Thus this is
-- the prototypical function for the block-fetch protocol.
--
receivedBlockBodies
  :: Chain Block
  -> [Point Block]
  -> [[BlockBody]]
receivedBlockBodies chain points =
  map f (pointsToRanges chain points)
 where
  f (ChainRange from to) = case Chain.selectBlockRange chain from to of
    Nothing -> []
    Just bs -> map ConcreteBlock.blockBody bs

--
-- Properties going directly, not via Peer.
--

-- | Run a simple block-fetch client and server,w ithout goind via the 'Peer'.
--
direct_experiment
  :: MonadSTM m
  => Chain Block
  -> [Point Block]
  -> m Bool
direct_experiment chain points = do
  let client = blockFetchClientMap (pointsToRanges chain points)
      server = blockFetchServer (ConcreteBlock.blockBody <$> rangeRequestsFromChain chain)

  (bodies', _) <- direct client server
  return (reverse bodies' == concat (receivedBlockBodies chain points))

-- | Run non piplined @'direct_experiment'@ in the simulation monad.
--
prop_direct
  :: TestChainAndPoints
  -> Bool
prop_direct (TestChainAndPoints chain points) = case runSim (direct_experiment chain points) of
  Right True -> True
  _          -> False

--
-- Properties goind via Peer, but without using a channel, without pipelining.
--

-- | Run a simple block-fetch client and server, going via the @'Peer'@
-- representation, but without going via a channel
--
connect_experiment
  :: MonadSTM m
  => Chain Block
  -> [Point Block]
  -> m Bool
connect_experiment chain points = do
  let client = blockFetchClientMap (pointsToRanges chain points)
      server = blockFetchServer (ConcreteBlock.blockBody <$> rangeRequestsFromChain chain)

  (bodies', _, _) <- connect
    (blockFetchClientPeer client)
    (blockFetchServerPeer server)
  return (reverse bodies' == concat (receivedBlockBodies chain points))

-- | Run @'connect_experiment'@ in the simulation monad.
--
prop_connect
  :: TestChainAndPoints
  -> Bool
prop_connect (TestChainAndPoints chain points) = case runSim (connect_experiment chain points) of
  Right True -> True
  _          -> False

-- Experiments using a channel
--

-- | Run a simple block-fetch client and server using two complementary channels.
--
channel_experiment
  :: forall m.
     ( MonadST m
     , MonadSTM m
     , MonadAsync m
     )
  => Channel m ByteString
  -> Channel m ByteString
  -> Chain Block
  -> [Point Block]
  -> m Property
channel_experiment clientChannel serverChannel chain points = do
  let client = blockFetchClientMap (pointsToRanges chain points)
      server = blockFetchServer (ConcreteBlock.blockBody <$> rangeRequestsFromChain chain)

  withAsync (runPeer codecBlockFetch serverChannel (blockFetchServerPeer server))
    $ \serverAsync -> withAsync (runPeer codecBlockFetch clientChannel (blockFetchClientPeer client))
      $ \clientAsync -> do

      bodies' <- wait clientAsync
      cancel serverAsync

      return ((reverse <$> bodies') === Right (concat (receivedBlockBodies chain points)))

-- |
-- Run @'channel_experiment'@ in the simulation monad.
--
prop_channel_ST
  :: TestChainAndPoints
  -> Property
prop_channel_ST (TestChainAndPoints chain points) =
  let r = runSim $ do
        (ca, cb) <- createConnectedChannels
        channel_experiment ca cb chain points
  in case r of
    Right p -> p
    _       -> property False


-- |
-- Run @'channel_experiment'@ in the IO monad.
--
prop_channel_IO
  :: TestChainAndPoints
  -> Property
prop_channel_IO (TestChainAndPoints chain points) = ioProperty $ do
  (ca, cb) <- createConnectedChannels
  channel_experiment ca cb chain points

-- |
-- Run @'channel_experiment'@ in the IO monad using local pipes.
--
prop_pipe_IO
  :: TestChainAndPoints
  -> Property
prop_pipe_IO (TestChainAndPoints chain points) = ioProperty $ do
  (ca, cb) <- createPipeConnectedChannels
  channel_experiment ca cb chain points
