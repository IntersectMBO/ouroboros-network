{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.Protocol.BlockFetch where

import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.IOSim (SimM, runSim)
import           Control.Monad.Class.MonadST (MonadST (..))
import           Control.Monad.Class.MonadSTM (MonadSTM)
import           Control.Monad.Class.MonadAsync (MonadAsync (..))

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
  [ testProperty "direct"  prop_direct
  , testProperty "connect" prop_connect
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testProperty "pipe IO"             prop_pipe_IO
  ]


--
-- Properties going directly, not via Peer.
--

-- | Run a simple block-fetch client and server, without goind via the 'Peer'.
--
prop_direct :: TestChainAndPoints -> Bool
prop_direct (TestChainAndPoints chain points) =
    case runSim (direct client server) of
      Left _             -> False
      Right (bodies, ()) ->
        reverse bodies == concat (receivedBlockBodies chain points)
  where
    client :: BlockFetchClient Block BlockBody (SimM s) [BlockBody]
    client = blockFetchClientMap (pointsToRanges chain points)

    server :: BlockFetchServer Block BlockBody (SimM s) ()
    server = blockFetchServer (ConcreteBlock.blockBody <$>
                                 rangeRequestsFromChain chain)


--
-- Properties goind via Peer, but without using a channel
--

-- | Run a simple block-fetch client and server, going via the @'Peer'@
-- representation, but without going via a channel.
--
prop_connect :: TestChainAndPoints -> Bool
prop_connect (TestChainAndPoints chain points) =
    case runSim (connect (blockFetchClientPeer client)
                         (blockFetchServerPeer server)) of
      Left _                                             -> False
      Right (bodies, (), TerminalStates TokDone TokDone) ->
        reverse bodies == concat (receivedBlockBodies chain points)
  where
    client :: BlockFetchClient Block BlockBody (SimM s) [BlockBody]
    client = blockFetchClientMap (pointsToRanges chain points)

    server :: BlockFetchServer Block BlockBody (SimM s) ()
    server = blockFetchServer (ConcreteBlock.blockBody <$>
                                 rangeRequestsFromChain chain)


--
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
