{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Ouroboros.Network.Protocol.BlockFetch where


import           Control.Monad.IOSim (runSim)

import           Control.Monad.Class.MonadSTM (MonadSTM)

import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Chain (Chain, Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Channel (createPipeConnectedChannels)
import           Ouroboros.Network.Testing.ConcreteBlock (Block, BlockBody)
import qualified Ouroboros.Network.Testing.ConcreteBlock as ConcreteBlock

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Direct (direct)
import           Ouroboros.Network.Protocol.BlockFetch.Examples

import           Test.Chain (TestChainAndPoints (..))

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.BlockFetch"
  [ testProperty "direct"  prop_direct
  , testProperty "connect" prop_connect
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
