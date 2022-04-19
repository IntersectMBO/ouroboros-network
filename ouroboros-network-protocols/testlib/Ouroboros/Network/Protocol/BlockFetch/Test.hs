{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.BlockFetch.Test (tests) where

import qualified Codec.Serialise as S
import           Control.Monad.ST (runST)
import           Data.ByteString.Lazy (ByteString)

import           Control.Applicative (Alternative)
import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM (MonadLabelledSTM, MonadSTM, STM)
import           Control.Monad.Class.MonadThrow (MonadCatch, MonadMask,
                     MonadThrow)
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (nullTracer)

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple (runConnectedPeers)

import           Ouroboros.Network.Block (Serialised (..), genesisPoint,
                     unwrapCBORinCBOR, wrapCBORinCBOR)

import           Ouroboros.Network.Mock.Chain (Chain, Point)
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ConcreteBlock (Block)

import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.BlockFetch.Examples
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Type

import           Test.ChainGenerators (TestChainAndPoints (..))
import           Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
                     prop_codec_valid_cbor_encoding, splits2, splits3)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol"
    [ testGroup "BlockFetch"
        [ testProperty "connect"             prop_connect
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
        , testProperty "codec cbor"          prop_codec_cbor_BlockFetch
        , testProperty "codec valid cbor"    prop_codec_valid_cbor_BlockFetch

        , testProperty "codecSerialised"                   prop_codec_BlockFetchSerialised
        , testProperty "codecSerialised 2-splits"          prop_codec_splits2_BlockFetchSerialised
        , testProperty "codecSerialised 3-splits"        $ withMaxSuccess 30
                                                           prop_codec_splits3_BlockFetchSerialised
        , testProperty "codecSerialised cbor"              prop_codec_cbor_BlockFetchSerialised
        , testProperty "codec/codecSerialised bin compat"  prop_codec_binary_compat_BlockFetch_BlockFetchSerialised
        , testProperty "codecSerialised/codec bin compat"  prop_codec_binary_compat_BlockFetchSerialised_BlockFetch
        ]
    ]


--
-- Block fetch client and server used in many subsequent tests.
--

type TestClient m = BlockFetchClient Block (Point Block) m [Block]
type TestServer m = BlockFetchServer Block (Point Block) m ()
type TestClientPipelined m =
       BlockFetchClientPipelined Block (Point Block) m
                                 [Either (ChainRange (Point Block)) Block]

testClient :: MonadSTM m => Chain Block -> [Point Block] -> TestClient m
testClient chain points = blockFetchClientMap (pointsToRanges chain points)

testServer :: MonadSTM m => Chain Block -> TestServer m
testServer chain = blockFetchServer (rangeRequestsFromChain chain)

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

--
-- Properties going via Peer, but without using a channel
--

-- | Run a simple block-fetch client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
prop_connect :: TestChainAndPoints -> Bool
prop_connect (TestChainAndPoints chain points) =
    case runSimOrThrow
           (connect [] []
             (blockFetchClientPeer (testClient chain points))
             (blockFetchServerPeer (testServer chain))) of
      (bodies, (), TerminalStates SingBFDone SingBFDone) ->
        reverse bodies == concat (receivedBlockBodies chain points)


-- | Run a pipelined block-fetch client against a server, going via the 'Peer'
-- representation, but without going via a channel.
--
connect_pipelined :: ( MonadSTM  m
                     , MonadFail m
                     )
                  => TestClientPipelined m
                  -> Chain Block
                  -> [Bool]
                  -> m [Either (ChainRange (Point Block)) Block]
connect_pipelined client chain cs = do
    (res, _, TerminalStates SingBFDone SingBFDone)
      <- connect cs []
           (blockFetchClientPeerPipelined client)
           (blockFetchServerPeer (testServer chain))
    return $ reverse res


-- | With a client with maximum pipelining we get all requests followed by
-- all responses.
--
prop_connect_pipelined1 :: TestChainAndPoints -> [Bool] -> Property
prop_connect_pipelined1 (TestChainAndPoints chain points) choices =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedMax chain points) chain choices)
 ===
    map Left  (pointsToRanges      chain points)
 ++ concatMap (map Right)
              (receivedBlockBodies chain points)


-- | With a client that collects eagerly and the driver chooses maximum
-- pipelining then we get all requests followed by all responses.
--
prop_connect_pipelined2 :: TestChainAndPoints -> Bool
prop_connect_pipelined2 (TestChainAndPoints chain points) =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedMin chain points) chain choices)
 ==
    map Left  (pointsToRanges      chain points)
 ++ concatMap (map Right)
              (receivedBlockBodies chain points)
  where
    choices = repeat True


-- | With a client that collects eagerly and the driver chooses minimum
-- pipelining then we get the interleaving of requests with responses.
--
prop_connect_pipelined3 :: TestChainAndPoints -> Property
prop_connect_pipelined3 (TestChainAndPoints chain points) =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedMin chain points) chain choices)
 ===
      concat [ Left l : map Right r
             | l <- pointsToRanges      chain points
             | r <- receivedBlockBodies chain points ]
  where
    choices = repeat False


-- | With a client that collects eagerly and the driver chooses arbitrary
-- pipelining then we get complex interleavings given by the reference
-- specification 'pipelineInterleaving'.
--
prop_connect_pipelined4 :: TestChainAndPoints -> [Bool] -> Property
prop_connect_pipelined4 (TestChainAndPoints chain points) choices =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedMin chain points) chain choices)
 ===
    blockFetchPipelineInterleaving
      maxBound choices
      (pointsToRanges      chain points)
      (receivedBlockBodies chain points)


-- | With a client that collects eagerly and is willing to send new messages
-- up to a fixed limit of outstanding messages, and the driver chooses
-- arbitrary pipelining then we get complex interleavings given by the
-- reference specification 'pipelineInterleaving', for that limit of
-- outstanding messages.
--
prop_connect_pipelined5 :: TestChainAndPoints -> NonNegative Int
                        -> [Bool] -> Property
prop_connect_pipelined5 (TestChainAndPoints chain points)
                        (NonNegative omax) choices =
    runSimOrThrow
      (connect_pipelined (testClientPipelinedLimited omax chain points)
                         chain choices)
 ===
    blockFetchPipelineInterleaving
      omax choices
      (pointsToRanges      chain points)
      (receivedBlockBodies chain points)

--
-- Properties using a channel
--

-- | Run a simple block-fetch client and server using connected channels.
--
prop_channel :: ( Alternative (STM m), MonadAsync m, MonadCatch m
                , MonadLabelledSTM m, MonadMask m, MonadST m, MonadThrow m
                , MonadThrow (STM m) )
             => m (Channel m ByteString, Channel m ByteString)
             -> Chain Block -> [Point Block] -> m Property
prop_channel createChannels chain points = do
    (bodies, ()) <-
      runConnectedPeers
        createChannels nullTracer
        codec
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


-- TODO: issue #347: BlockFetch pipelined tests using channels & pipes

--
-- Codec properties
--

codec :: MonadST m
      => Codec (BlockFetch Block (Point Block))
               S.DeserialiseFailure
               m ByteString
codec = codecBlockFetch S.encode S.decode
                        S.encode S.decode

codecWrapped :: MonadST m
             => Codec (BlockFetch Block (Point Block))
                      S.DeserialiseFailure
                      m ByteString
codecWrapped =
    codecBlockFetch
      (wrapCBORinCBOR S.encode) (unwrapCBORinCBOR (const <$> S.decode))
      S.encode                  S.decode

codecSerialised :: MonadST m
                => Codec (BlockFetch (Serialised Block) (Point Block))
                         S.DeserialiseFailure
                         m ByteString
codecSerialised = codecBlockFetch S.encode S.decode S.encode S.decode


instance Arbitrary point => Arbitrary (ChainRange point) where
  arbitrary = ChainRange <$> arbitrary <*> arbitrary
  shrink (ChainRange a b) =
    [ ChainRange a' b
    | a' <- shrink a
    ]
    ++
    [ ChainRange a b'
    | b' <- shrink b
    ]

instance (Arbitrary block, Arbitrary point)
      => Arbitrary (AnyMessage (BlockFetch block point)) where
  arbitrary = oneof
    [ AnyMessage . MsgRequestRange <$> arbitrary
    , return $ AnyMessage MsgStartBatch
    , return $ AnyMessage MsgNoBlocks
    , AnyMessage . MsgBlock <$> arbitrary
    , return $ AnyMessage MsgBatchDone
    , return $ AnyMessage MsgClientDone
    ]

  shrink (AnyMessage (MsgRequestRange range)) =
    [ AnyMessage (MsgRequestRange range')
    | range' <- shrink range
    ]
  shrink (AnyMessage MsgStartBatch) = []
  shrink (AnyMessage MsgNoBlocks) = []
  shrink (AnyMessage (MsgBlock block)) =
    [ AnyMessage (MsgBlock block')
    | block' <- shrink block
    ]
  shrink (AnyMessage MsgBatchDone) = []
  shrink (AnyMessage MsgClientDone) = []


instance (Eq block, Eq point) =>
         Eq (AnyMessage (BlockFetch block point)) where
  AnyMessage (MsgRequestRange r1) == AnyMessage (MsgRequestRange r2) = r1 == r2
  AnyMessage MsgStartBatch        == AnyMessage MsgStartBatch        = True
  AnyMessage MsgNoBlocks          == AnyMessage MsgNoBlocks          = True
  AnyMessage (MsgBlock b1)        == AnyMessage (MsgBlock b2)        = b1 == b2
  AnyMessage MsgBatchDone         == AnyMessage MsgBatchDone         = True
  AnyMessage MsgClientDone        == AnyMessage MsgClientDone        = True
  _                               ==                  _              = False

instance Arbitrary (Serialised Block) where
  arbitrary = Serialised . S.serialise @Block <$> arbitrary

  shrink (Serialised block) =
    Serialised . S.serialise @Block <$> shrink (S.deserialise block)

prop_codec_BlockFetch
  :: AnyMessage (BlockFetch Block (Point Block))
  -> Bool
prop_codec_BlockFetch msg =
  runST (prop_codecM codec msg)

prop_codec_splits2_BlockFetch
  :: AnyMessage (BlockFetch Block (Point Block))
  -> Bool
prop_codec_splits2_BlockFetch msg =
  runST (prop_codec_splitsM splits2 codec msg)

prop_codec_splits3_BlockFetch
  :: AnyMessage (BlockFetch Block (Point Block))
  -> Bool
prop_codec_splits3_BlockFetch msg =
  runST (prop_codec_splitsM splits3 codec msg)

prop_codec_cbor_BlockFetch
  :: AnyMessage (BlockFetch Block (Point Block))
  -> Bool
prop_codec_cbor_BlockFetch msg =
  runST (prop_codec_cborM codec msg)

prop_codec_valid_cbor_BlockFetch
  :: AnyMessage (BlockFetch Block (Point Block))
  -> Property
prop_codec_valid_cbor_BlockFetch = prop_codec_valid_cbor_encoding codec

prop_codec_BlockFetchSerialised
  :: AnyMessage (BlockFetch (Serialised Block) (Point Block))
  -> Bool
prop_codec_BlockFetchSerialised msg =
  runST (prop_codecM codecSerialised msg)

prop_codec_splits2_BlockFetchSerialised
  :: AnyMessage (BlockFetch (Serialised Block) (Point Block))
  -> Bool
prop_codec_splits2_BlockFetchSerialised msg =
  runST (prop_codec_splitsM splits2 codecSerialised msg)

prop_codec_splits3_BlockFetchSerialised
  :: AnyMessage (BlockFetch (Serialised Block) (Point Block))
  -> Bool
prop_codec_splits3_BlockFetchSerialised msg =
  runST (prop_codec_splitsM splits3 codecSerialised msg)

prop_codec_cbor_BlockFetchSerialised
  :: AnyMessage (BlockFetch (Serialised Block) (Point Block))
  -> Bool
prop_codec_cbor_BlockFetchSerialised msg =
  runST (prop_codec_cborM codecSerialised msg)


prop_codec_binary_compat_BlockFetch_BlockFetchSerialised
  :: AnyMessage (BlockFetch Block (Point Block))
  -> Bool
prop_codec_binary_compat_BlockFetch_BlockFetchSerialised msg =
    runST (prop_codec_binary_compatM codecWrapped codecSerialised stokEq msg)
  where
    stokEq
      :: forall (stA :: BlockFetch Block (Point Block)).
         ActiveState stA
      => StateToken stA
      -> SomeState (BlockFetch (Serialised Block) (Point Block))
    stokEq SingBFIdle      = SomeState SingBFIdle
    stokEq SingBFBusy      = SomeState SingBFBusy
    stokEq SingBFStreaming = SomeState SingBFStreaming
    stokEq a@SingBFDone    = notActiveState a

prop_codec_binary_compat_BlockFetchSerialised_BlockFetch
  :: AnyMessage (BlockFetch (Serialised Block) (Point Block))
  -> Bool
prop_codec_binary_compat_BlockFetchSerialised_BlockFetch msg =
    runST (prop_codec_binary_compatM codecSerialised codecWrapped stokEq msg)
  where
    stokEq
      :: forall (stA :: BlockFetch (Serialised Block) (Point Block)).
         ActiveState stA
      => StateToken stA
      -> SomeState (BlockFetch Block (Point Block))
    stokEq SingBFIdle      = SomeState SingBFIdle
    stokEq SingBFBusy      = SomeState SingBFBusy
    stokEq SingBFStreaming = SomeState SingBFStreaming
    stokEq a@SingBFDone    = notActiveState a

--
-- Auxiliary functions
--

-- | Generate a list of @ChainRange@s from a list of points on a chain.  The
-- the ranges which both ends are on the chain are disjoint.
--
pointsToRanges
  :: Chain.HasHeader block
  => Chain block
  -> [Point block]
  -> [ChainRange (Point block)]
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
    go [x] = [ChainRange genesisPoint x]
    go []  = []

-- | Compute list of received block bodies from a chain and points.
-- This is the reference function against which we compare block-fetch
-- protocol.  The @'pointsToRanges'@ function is used to compute the ranges,
-- and then the results are then read from the chain directly.  Thus this is
-- the prototypical function for the block-fetch protocol.
--
receivedBlockBodies
  :: Chain Block
  -> [Point Block]
  -> [[Block]]
receivedBlockBodies chain points =
      map f $ pointsToRanges chain points
 where
    f (ChainRange from to) =
      case Chain.selectBlockRange chain from to of
        Nothing -> []
        Just bs -> bs
