{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.TxSubmission.Test
  ( tests
  ) where

import           Control.Monad.ST (runST)
import           Codec.Serialise (Serialise)
import           Data.List (sortBy, foldl')
import           Numeric.Natural (Natural)
import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM (MonadSTM)
import           Control.Monad.Class.MonadThrow (MonadCatch)

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Proofs
import           Ouroboros.Network.Channel

import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Direct
import           Ouroboros.Network.Protocol.TxSubmission.Examples
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Type

import           Test.Ouroboros.Network.Testing.Utils (splits2, splits3)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

--
-- Test cases
--

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.TxSubmission"
  [ testProperty "direct"              prop_direct
  , testProperty "directPipelined 1"   prop_directPipelined1
  , testProperty "directPipelined 2"   prop_directPipelined2
  , testProperty "connect"             prop_connect
  , testProperty "connect_pipelined 1" prop_connect_pipelined1
  , testProperty "connect_pipelined 2" prop_connect_pipelined2
  , testProperty "connect_pipelined 3" prop_connect_pipelined3
  , testProperty "connect_pipelined 4" prop_connect_pipelined4
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testProperty "pipe IO"             prop_pipe_IO
  , testProperty "codec"               prop_codec_TxSubmission
  , testProperty "codec 2-splits"      prop_codec_splits2_TxSubmission
  , testProperty "codec 3-splits"    $ withMaxSuccess 30
                                       prop_codec_splits3_TxSubmission
  ]


--
-- Common types & clients and servers used in various tests in this module.
--

newtype Tx   = Tx { getHash :: Int }
  deriving (Eq, Show, Arbitrary, Serialise)

type    Hash = Int

type TestClient m = TxSubmissionClientPipelined Hash Tx m [ReqOrResp Hash Tx] 
type TestServer m = TxSubmissionServer Hash Tx m [Tx]

testClient :: MonadSTM m => [Natural] -> TestClient m
testClient ns = txSubmissionClient ns

testServer :: MonadSTM m => [Tx] -> TestServer m
testServer txs = txSubmissionServerFixed txs getHash

-- |
-- A reference implementation of @'txSubmissionClient'@.  It returns the
-- sequence of requests and responses of a @'txSubmissionClient'@.
--
txSubmissionClientReference :: [Natural] -> [Tx] -> [ReqOrResp Hash Tx]
txSubmissionClientReference = go []
    where
      go :: [ReqOrResp Hash Tx] -> [Natural] -> [Tx] -> [ReqOrResp Hash Tx]
      go acc []     _   = reverse acc
      go acc (n:ns) txs =
        let (requested, txs') = splitAt (fromIntegral n) txs
            acc' = foldl' (\xs tx -> RespTx tx : ReqTx (getHash tx) : xs) [] requested
                ++ RespHashes (map getHash requested)
                 : ReqHashes n
                 : acc
        in go acc' ns txs'

-- |
-- A reference implementation of @'txSubmissionCientMax'@.
--
txSubmissionClientMaxReference :: [Natural] -> [Tx] -> [ReqOrResp Hash Tx]
txSubmissionClientMaxReference = go []
    where
      go :: [ReqOrResp Hash Tx] -> [Natural] -> [Tx] -> [ReqOrResp Hash Tx]
      go acc []     _   = reverse acc
      go acc (n:ns) txs =
        let (requested, txs') = splitAt (fromIntegral n) txs
            acc' = foldl' (\xs tx -> RespTx tx : xs) [] requested
                ++ foldl' (\xs tx -> ReqTx (getHash tx) : xs) [] requested
                ++ RespHashes (map getHash requested)
                 : ReqHashes n
                 : acc
        in go acc' ns txs'

-- |
-- Reoders transaction requests and transaction responds as if maximum
-- pipelining was in place: e.g. place 'ReqHashes' and 'RespHashes' in place but
-- move 'ReqTx' in front of 'RespTx'.
--
cannonicalOrder :: [ReqOrResp Hash Tx]
                -> [ReqOrResp Hash Tx]
cannonicalOrder = go
    where
      go (ReqHashes n : RespHashes hs : xs) =
        let (xs', xs'') = splitAt (2 * fromIntegral n) xs
        in ReqHashes n : RespHashes hs : sortBy fn xs' ++ go xs''
      go xs = xs

      fn RespTx{}        ReqTx{}      = GT
      fn ReqTx{}         RespTx{}     = LT
      fn _               _            = EQ


-- |
-- A reference impolementation of @'txSubmissionServer'@.
--
txSubmissionServerReference :: [Natural] -> [Tx] -> [Tx]
txSubmissionServerReference ns txs = take (fromIntegral $ sum ns) txs

--
-- Properties goind directly, not via Peer.
--

prop_direct ::  [NonNegative Integer] -> [Tx] -> Bool
prop_direct ns txs =
  let ns' = map (fromIntegral . getNonNegative) ns
  in
    runSimOrThrow (direct (testClient ns')
                          (testServer txs))
  ==
    ( txSubmissionClientReference ns' txs
    , txSubmissionServerReference ns' txs
    )

prop_directPipelined1 :: [NonNegative Integer] -> [Tx] -> Bool
prop_directPipelined1 ns txs =
  let ns' = map (fromIntegral . getNonNegative) ns
  in 
    runSimOrThrow (direct (txSubmissionClientPipelinedMax ns')
                          (testServer txs))
  ==
    ( txSubmissionClientMaxReference ns' txs
    , txSubmissionServerReference ns' txs
    )

prop_directPipelined2 :: [NonNegative Integer] -> [Tx] -> Bool
prop_directPipelined2 ns txs =
  let ns' = map (fromIntegral . getNonNegative) ns
  in 
    runSimOrThrow (direct (txSubmissionClientPipelinedMin ns')
                          (testServer txs))
  ==
    -- it is the same as @'txSubmissionClientMaxReference'@ since
    -- @'Ouroboros.Network.Protocol.TxSubmission.Direct'@ will pipeline as many
    -- requests as possible.
    ( txSubmissionClientMaxReference ns' txs
    , txSubmissionServerReference ns' txs
    )

--
-- Properties going via Peer, but without using a channel
--

-- |
-- Run a simple tx-submission client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
prop_connect :: [NonNegative Integer]
             -> [Tx]
             -> Bool
prop_connect ns txs =
    let ns' = map (fromIntegral . getNonNegative) ns
    in case runSimOrThrow
              (connect
                (forgetPipelined $ txSubmissionClientPeerPipelined (testClient ns'))
                (txSubmissionServerPeer (testServer txs))) of
      (rs, _, TerminalStates TokDone TokDone) ->
        rs == txSubmissionClientReference ns' txs

-- |
-- Run a pipelined tx-submission client against a server, going via the
-- 'Peer' representation, but without going via a channel.
--
connect_pipelined :: MonadSTM m
                  => TestClient m
                  -> [Tx]
                  -> [Bool]
                  -> m [ReqOrResp Hash Tx]
connect_pipelined client txs cs = do
    (res, _, TerminalStates TokDone TokDone)
      <- connectPipelined cs
           (txSubmissionClientPeerPipelined client)
           (txSubmissionServerPeer (testServer txs))
    return res


-- |
-- With a client with maximum pipelining we get all requests followed by
-- all responses.
--
prop_connect_pipelined1 :: [NonNegative Integer] -> [Tx] -> [Bool] -> Bool
prop_connect_pipelined1 ns txs choices =
    let ns' = map (fromIntegral . getNonNegative) ns
    in
      runSimOrThrow
        (connect_pipelined (txSubmissionClientPipelinedMax ns') txs choices)
    ==
      txSubmissionClientMaxReference ns' txs


-- | With a client that collects eagerly and the driver chooses maximum
-- pipelining then we get all requests followed by all responses.
--
prop_connect_pipelined2 :: [NonNegative Integer] -> [Tx] -> Bool
prop_connect_pipelined2 ns txs =
    let ns' = map (fromIntegral . getNonNegative) ns
        choices = repeat True
    in
      runSimOrThrow
        (connect_pipelined (txSubmissionClientPipelinedMin ns') txs choices)
    ==
      txSubmissionClientMaxReference ns' txs


-- |
-- With a client that collects eagerly and the driver chooses minimum
-- pipelining then we get the interleaving of requests with responses.
--
prop_connect_pipelined3 :: [NonNegative Integer] -> [Tx] -> Bool
prop_connect_pipelined3 ns txs =
  let ns' = map (fromIntegral . getNonNegative) ns
      choices = repeat False
  in 
      runSimOrThrow
        (connect_pipelined (txSubmissionClientPipelinedMin ns') txs choices)
    ==
      txSubmissionClientReference ns' txs


-- |
-- With a client that collects eagerly and the driver chooses arbitrary
-- pipelining then we get complex interleavings given by the reference
-- specification 'pipelineInterleaving'.
--
prop_connect_pipelined4 :: [NonNegative Integer] -> [Tx] -> [Bool] -> Bool
prop_connect_pipelined4 ns txs choices =
  let ns' = map (fromIntegral . getNonNegative) ns
  in
      cannonicalOrder
        (runSimOrThrow
            (connect_pipelined (txSubmissionClientPipelinedMin ns') txs choices))
    ==
      txSubmissionClientMaxReference ns' txs

--
-- Properties using a channel
--

-- |
-- Run a simple tx-submission client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m)
             => m (Channel m ByteString, Channel m ByteString)
             -> [NonNegative Integer] -> [Tx] -> m Property
prop_channel createChannels ns txs = do
    let ns' = map (fromIntegral . getNonNegative) ns
    (res, _) <-
      runConnectedPeers
        createChannels codecTxSubmission
        (forgetPipelined $ txSubmissionClientPeerPipelined (testClient ns'))
        (txSubmissionServerPeer (testServer txs))
    return $ res === txSubmissionClientReference ns' txs

-- |
-- Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: [NonNegative Integer] -> [Tx] -> Property
prop_channel_ST ns txs =
    runSimOrThrow (prop_channel createConnectedChannels ns txs)


-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: [NonNegative Integer] -> [Tx] -> Property
prop_channel_IO ns txs =
    ioProperty (prop_channel createConnectedChannels ns txs)


-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: [NonNegative Integer] -> [Tx] -> Property
prop_pipe_IO ns txs =
    ioProperty (prop_channel createPipeConnectedChannels ns txs)

--
-- Codec properties
--

instance Arbitrary (AnyMessageAndAgency (TxSubmission Int Tx)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) . MsgGetHashes . fromIntegral @Integer . getNonNegative <$> arbitrary
    , AnyMessageAndAgency (ServerAgency TokSendHashes) . MsgSendHashes <$> arbitrary
    , AnyMessageAndAgency (ClientAgency TokIdle) . MsgGetTx <$> arbitrary
    , AnyMessageAndAgency (ServerAgency TokSendTx) . MsgTx <$> arbitrary
    , return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance Show (AnyMessageAndAgency (TxSubmission Int Tx)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance (Eq hash, Eq tx) =>
         Eq (AnyMessage (TxSubmission hash tx)) where
  AnyMessage (MsgGetHashes n0)   == AnyMessage (MsgGetHashes n1)   =  n0 == n1
  AnyMessage (MsgSendHashes hs0) == AnyMessage (MsgSendHashes hs1) = hs0 == hs1
  AnyMessage (MsgGetTx h0)       == AnyMessage (MsgGetTx h1)       =  h0 == h1
  AnyMessage (MsgTx tx0)         == AnyMessage (MsgTx tx1)         = tx0 == tx1
  AnyMessage MsgDone             == AnyMessage MsgDone             = True
  _                              == _                              = False

prop_codec_TxSubmission
  :: AnyMessageAndAgency (TxSubmission Int Tx)
  -> Bool
prop_codec_TxSubmission msg =
  runST (prop_codecM codecTxSubmission msg)

prop_codec_splits2_TxSubmission
  :: AnyMessageAndAgency (TxSubmission Int Tx)
  -> Bool
prop_codec_splits2_TxSubmission msg =
  runST (prop_codec_splitsM splits2 codecTxSubmission msg)

prop_codec_splits3_TxSubmission
  :: AnyMessageAndAgency (TxSubmission Int Tx)
  -> Bool
prop_codec_splits3_TxSubmission msg =
  runST (prop_codec_splitsM splits3 codecTxSubmission msg)
