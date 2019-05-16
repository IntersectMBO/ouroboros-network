{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE FlexibleInstances   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.BlockFetch.Test (tests) where

import           Control.Monad.ST (runST)
import           Data.ByteString.Lazy (ByteString)
import qualified Codec.Serialise as S

import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM (MonadSTM)
import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadThrow (MonadCatch)
import           Control.Tracer (nullTracer)

import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Proofs
import           Ouroboros.Network.Channel

import           Ouroboros.Network.Block (StandardHash)
import           Ouroboros.Network.Chain (Chain, Point)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Testing.ConcreteBlock
                   (Block, BlockHeader, BlockBody)
import qualified Ouroboros.Network.Testing.ConcreteBlock as ConcreteBlock

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Direct
import           Ouroboros.Network.Protocol.BlockFetch.Examples
import           Ouroboros.Network.Protocol.BlockFetch.Codec

import           Test.ChainGenerators ( TestChainAndPoints (..)
                                      , ArbitraryChainRange (..)
                                      , ArbitraryBlockBody (..))
import           Test.Ouroboros.Network.Testing.Utils (splits2, splits3)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.BlockFetch"
  [ testProperty "direct"              prop_direct
  , testProperty "directPipelined 1"   prop_directPipelined1
  , testProperty "directPipelined 2"   prop_directPipelined2
  , testProperty "connect"             prop_connect
  , testProperty "connect_pipelined 1" prop_connect_pipelined1
  , testProperty "connect_pipelined 2" prop_connect_pipelined2
  , testProperty "connect_pipelined 3" prop_connect_pipelined3
  , testProperty "connect_pipelined 4" prop_connect_pipelined4
  , testProperty "connect_pipelined 5" prop_connect_pipelined5
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testProperty "pipe IO"             prop_pipe_IO
  , testProperty "codec"               prop_codec_BlockFetch
  , testProperty "codec 2-splits"      prop_codec_splits2_BlockFetch
  , testProperty "codec 3-splits"    $ withMaxSuccess 30
                                       prop_codec_splits3_BlockFetch
  ]


--
-- Block fetch client and server used in many subsequent tests.
--

type TestClient m = BlockFetchClient Block BlockBody m [BlockBody]
type TestServer m = BlockFetchServer Block BlockBody m ()
type TestClientPipelined m =
       BlockFetchClientPipelined Block BlockBody m
                                 [Either (ChainRange Block) [BlockBody]]

testClient :: MonadSTM m => Chain Block -> [Point Block] -> TestClient m
testClient chain points = blockFetchClientMap (pointsToRanges chain points)

testServer :: MonadSTM m => Chain Block -> TestServer m
testServer chain = blockFetchServer (ConcreteBlock.blockBody <$>
                                       rangeRequestsFromChain chain)

testClientPipelinedMax,
  testClientPipelinedMin
  :: MonadSTM m
  => Chain Block
  -> [Point Block]
  -> TestClientPipelined m

testClientPipelinedLimited
  :: MonadSTM m
  => Int
  -> Chain Block
  -> [Point Block]
  -> TestClientPipelined m

testClientPipelinedMax chain points =
    blockFetchClientPipelinedMax (pointsToRanges chain points)

testClientPipelinedMin chain points =
    blockFetchClientPipelinedMin (pointsToRanges chain points)

testClientPipelinedLimited omax chain points =
    blockFetchClientPipelinedLimited omax (pointsToRanges chain points)


--
-- Properties going directly, not via Peer.
--

-- | Run a simple block-fetch client and server, without going via the 'Peer'.
--
prop_direct :: TestChainAndPoints -> Bool
prop_direct (TestChainAndPoints chain points) =
    runSimOrThrow (direct (testClient chain points)
                          (testServer chain))
 ==
    (reverse . concat $ receivedBlockBodies chain points, ())


-- | Run a pipelined block-fetch client with a server, without going via 'Peer'.
--
--
--
prop_directPipelined1 :: TestChainAndPoints -> Bool
prop_directPipelined1 (TestChainAndPoints chain points) =
   case runSimOrThrow (directPipelined (testClientPipelinedMax chain points)
                                       (testServer chain)) of
     (res, ()) ->
         reverse (map (fmap reverse) res)
      ==
         map Left  (pointsToRanges      chain points)
      ++ map Right (receivedBlockBodies chain points)

prop_directPipelined2 :: TestChainAndPoints -> Bool
prop_directPipelined2 (TestChainAndPoints chain points) =
   case runSimOrThrow (directPipelined (testClientPipelinedMin chain points)
                                       (testServer chain)) of
     (res, ()) ->
         reverse (map (fmap reverse) res)
      ==
         concat [ [Left l, Right r]
                | l <- pointsToRanges      chain points
                | r <- receivedBlockBodies chain points ]


--
-- Properties going via Peer, but without using a channel
--

-- | Run a simple block-fetch client and server, going via the 'Peer'
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


-- | Run a pipelined block-fetch client against a server, going via the 'Peer'
-- representation, but without going via a channel.
--
connect_pipelined :: MonadSTM m
                  => TestClientPipelined m
                  -> Chain Block
                  -> [Bool]
                  -> m [Either (ChainRange Block) [BlockBody]]
connect_pipelined client chain cs = do
    (res, _, TerminalStates TokDone TokDone)
      <- connectPipelined cs
           (blockFetchClientPeerPipelined client)
           (blockFetchServerPeer (testServer chain))
    return $ reverse $ map (fmap reverse) res


-- | With a client with maximum pipelining we get all requests followed by
-- all responses.
--
prop_connect_pipelined1 :: TestChainAndPoints -> [Bool] -> Bool
prop_connect_pipelined1 (TestChainAndPoints chain points) choices =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedMax chain points) chain choices)
 ==
    map Left  (pointsToRanges      chain points)
 ++ map Right (receivedBlockBodies chain points)


-- | With a client that collects eagerly and the driver chooses maximum
-- pipelining then we get all requests followed by all responses.
--
prop_connect_pipelined2 :: TestChainAndPoints -> Bool
prop_connect_pipelined2 (TestChainAndPoints chain points) =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedMin chain points) chain choices)
 ==
    map Left  (pointsToRanges      chain points)
 ++ map Right (receivedBlockBodies chain points)
  where
    choices = repeat True


-- | With a client that collects eagerly and the driver chooses minimum
-- pipelining then we get the interleaving of requests with responses.
--
prop_connect_pipelined3 :: TestChainAndPoints -> Bool
prop_connect_pipelined3 (TestChainAndPoints chain points) =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedMin chain points) chain choices)
 ==
    concat [ [Left l, Right r]
           | l <- pointsToRanges      chain points
           | r <- receivedBlockBodies chain points ]
  where
    choices = repeat False


-- | With a client that collects eagerly and the driver chooses arbitrary
-- pipelining then we get complex interleavings given by the reference
-- specification 'pipelineInterleaving'.
--
prop_connect_pipelined4 :: TestChainAndPoints -> [Bool] -> Bool
prop_connect_pipelined4 (TestChainAndPoints chain points) choices =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedMin chain points) chain choices)
 ==
    pipelineInterleaving maxBound choices
                         (pointsToRanges      chain points)
                         (receivedBlockBodies chain points)


-- | With a client that collects eagerly and is willing to send new messages
-- up to a fixed limit of outstanding messages, and the driver chooses
-- arbitrary pipelining then we get complex interleavings given by the
-- reference specification 'pipelineInterleaving', for that limit of
-- outstanding messages.
--
prop_connect_pipelined5 :: TestChainAndPoints -> NonNegative Int
                        -> [Bool] -> Bool
prop_connect_pipelined5 (TestChainAndPoints chain points)
                        (NonNegative omax) choices =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedLimited omax chain points)
                         chain choices)
 ==
    pipelineInterleaving omax choices
                         (pointsToRanges      chain points)
                         (receivedBlockBodies chain points)


--
-- Properties using a channel
--

-- | Run a simple block-fetch client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m)
             => m (Channel m ByteString, Channel m ByteString)
             -> Chain Block -> [Point Block] -> m Property
prop_channel createChannels chain points = do
    (bodies, ()) <-
      runConnectedPeers
        createChannels nullTracer (codecBlockFetch S.encode S.encode S.decode S.decode)
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
-- Codec properties
--

instance Arbitrary (AnyMessageAndAgency (BlockFetch BlockHeader BlockBody)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) <$>
        MsgRequestRange <$> (getArbitraryChainRange <$> arbitrary)
    , return $ AnyMessageAndAgency (ServerAgency TokBusy) MsgStartBatch
    , return $ AnyMessageAndAgency (ServerAgency TokBusy) MsgNoBlocks
    , AnyMessageAndAgency (ServerAgency TokStreaming) <$>
        MsgBlock <$> getArbitraryBlockBody <$> arbitrary
    , return $ AnyMessageAndAgency (ServerAgency TokStreaming) MsgBatchDone
    , return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgClientDone
    ]

instance Show (AnyMessageAndAgency (BlockFetch BlockHeader BlockBody)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance (StandardHash header, Eq body) =>
         Eq (AnyMessage (BlockFetch header body)) where
  AnyMessage (MsgRequestRange r1) == AnyMessage (MsgRequestRange r2) = r1 == r2
  AnyMessage MsgStartBatch        == AnyMessage MsgStartBatch        = True
  AnyMessage MsgNoBlocks          == AnyMessage MsgNoBlocks          = True
  AnyMessage (MsgBlock b1)        == AnyMessage (MsgBlock b2)        = b1 == b2
  AnyMessage MsgBatchDone         == AnyMessage MsgBatchDone         = True
  AnyMessage MsgClientDone        == AnyMessage MsgClientDone        = True
  _                               ==                  _              = False

prop_codec_BlockFetch
  :: AnyMessageAndAgency (BlockFetch BlockHeader BlockBody)
  -> Bool
prop_codec_BlockFetch msg =
  runST (prop_codecM (codecBlockFetch S.encode S.encode S.decode S.decode) msg)

prop_codec_splits2_BlockFetch
  :: AnyMessageAndAgency (BlockFetch BlockHeader BlockBody)
  -> Bool
prop_codec_splits2_BlockFetch msg =
  runST (prop_codec_splitsM splits2 (codecBlockFetch S.encode S.encode S.decode S.decode) msg)

prop_codec_splits3_BlockFetch
  :: AnyMessageAndAgency (BlockFetch BlockHeader BlockBody)
  -> Bool
prop_codec_splits3_BlockFetch msg =
  runST (prop_codec_splitsM splits3 (codecBlockFetch S.encode S.encode S.decode S.decode) msg)


--
-- Auxilary functions
--

-- | Generate a list of @ChainRange@s from a list of points on a chain.  The
-- the ranges which both ends are on the chain are disjoint.
--
pointsToRanges
  :: Chain.HasHeader block
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
