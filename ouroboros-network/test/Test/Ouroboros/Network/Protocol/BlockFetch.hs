{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.Protocol.BlockFetch where

import           Control.Monad.IOSim (SimM, runSim)

import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Chain (Chain, Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Testing.ConcreteBlock (Block, BlockBody)
import qualified Ouroboros.Network.Testing.ConcreteBlock as ConcreteBlock

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Direct (direct)
import           Ouroboros.Network.Protocol.BlockFetch.Examples

import           Test.ChainGenerators (TestChainAndPoints (..))

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.BlockFetch"
  [ testProperty "direct"  prop_direct
  , testProperty "connect" prop_connect
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

