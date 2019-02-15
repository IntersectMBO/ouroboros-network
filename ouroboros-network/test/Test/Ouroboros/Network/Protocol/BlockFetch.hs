{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.Protocol.BlockFetch where

import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Monad.Fail
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM (MonadSTM)
import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadThrow (MonadCatch)

import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Proofs
import           Ouroboros.Network.Channel

import           Ouroboros.Network.Chain (Chain, Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Testing.ConcreteBlock (Block, BlockBody)
import qualified Ouroboros.Network.Testing.ConcreteBlock as ConcreteBlock

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Direct
import           Ouroboros.Network.Protocol.BlockFetch.Examples
import           Ouroboros.Network.Protocol.BlockFetch.Codec

import           Test.ChainGenerators (TestChainAndPoints (..))

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.BlockFetch"
  [ testProperty "direct"            prop_direct
  , testProperty "direct_pipelined"  prop_directPipelined
  , testProperty "connect"           prop_connect
  , testProperty "channel ST"        prop_channel_ST
  , testProperty "channel IO"        prop_channel_IO
  , testProperty "pipe IO"           prop_pipe_IO
  ]


--
-- Block fetch client and server used in many subsequent tests.
--

testClient :: MonadSTM m
           => Chain Block
           -> [Point Block]
           -> BlockFetchClient Block BlockBody m [BlockBody]
testClient chain points = blockFetchClientMap (pointsToRanges chain points)

testClientPipelined :: MonadSTM m
                    => Chain Block
                    -> [Point Block]
                    -> BlockFetchClientPipelined Block BlockBody m
                         [Either (ChainRange Block) [BlockBody]]
testClientPipelined chain points =
    blockFetchClientPipelinedMax (pointsToRanges chain points)

testServer :: MonadSTM m
           => Chain Block
           -> BlockFetchServer Block BlockBody m ()
testServer chain = blockFetchServer (ConcreteBlock.blockBody <$>
                                       rangeRequestsFromChain chain)


--
-- Properties going directly, not via Peer.
--

-- | Run a simple block-fetch client and server, without goind via the 'Peer'.
--
prop_direct :: TestChainAndPoints -> Bool
prop_direct (TestChainAndPoints chain points) =
    runSimOrThrow (direct (testClient chain points)
                          (testServer chain))
 ==
    (reverse . concat $ receivedBlockBodies chain points, ())


-- | Run a pipelined block-fetch client with a server, without goind via 'Peer'.
--
prop_directPipelined :: TestChainAndPoints -> Bool
prop_directPipelined (TestChainAndPoints chain points) =
   case runSimOrThrow (directPipelined (testClientPipelined chain points)
                                       (testServer chain)) of
     (res, ()) ->
         reverse (map (either Left (Right . reverse)) res)
      ==
         map Left  (pointsToRanges      chain points)
      ++ map Right (receivedBlockBodies chain points)


--
-- Properties goind via Peer, but without using a channel
--

-- | Run a simple block-fetch client and server, going via the @'Peer'@
-- representation, but without going via a channel.
--
prop_connect :: TestChainAndPoints -> Bool
prop_connect (TestChainAndPoints chain points) =
    case runSimOrThrow
           (connect
             (blockFetchClientPeer (testClient chain points))
             (blockFetchServerPeer (testServer chain))) of
      (bodies, (), TerminalStates TokDone TokDone) ->
        reverse bodies == concat (receivedBlockBodies chain points)


--
-- Properties using a channel
--

-- | Run a simple block-fetch client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m, MonadFail m)
             => m (Channel m ByteString, Channel m ByteString)
             -> Chain Block -> [Point Block] -> m Property
prop_channel createChannels chain points = do
    Right (bodies, ()) <-
      runConnectedPeers
        createChannels codecBlockFetch
        (blockFetchClientPeer (testClient chain points))
        (blockFetchServerPeer (testServer chain))
    return $ reverse bodies === concat (receivedBlockBodies chain points)


-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: TestChainAndPoints -> Property
prop_channel_ST (TestChainAndPoints chain points) =
    runSimOrThrow (prop_channel createConnectedChannels chain points)


-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: TestChainAndPoints -> Property
prop_channel_IO (TestChainAndPoints chain points) =
    ioProperty (prop_channel createConnectedChannels chain points)

-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: TestChainAndPoints -> Property
prop_pipe_IO (TestChainAndPoints chain points) =
    ioProperty (prop_channel createPipeConnectedChannels chain points)


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
pointsToRanges chain points =
    go (reverse points)
  where
    go (x : y : ys) =
      if x `Chain.pointOnChain` chain
         -- otherwise `Chain.successorBlock` will error
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
    f (ChainRange from to) =
      case Chain.selectBlockRange chain from to of
        Nothing -> []
        Just bs -> map ConcreteBlock.blockBody bs
