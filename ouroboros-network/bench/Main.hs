{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Bench.TxSubmissionV2Server qualified as DirectV2

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import System.Mem (performMajorGC)
import Test.Tasty.Bench

import Test.Ouroboros.Network.PeerSelection.PeerMetric
           (microbenchmark1GenerateInput, microbenchmark1ProcessInput)
import Test.Ouroboros.Network.TxSubmission.TxLogic qualified as TX

benchLoops :: Int
benchLoops = 1_000

-- | Label suffix derived from 'benchLoops' so the bench names can't drift
-- from the actual iteration count.
benchLoopsLabel :: String
benchLoopsLabel = "x" ++ show benchLoops

main :: IO ()
main =
    defaultMain
      [ bgroup "ouroboros-network:sim-benchmarks"
        [ bgroup "PeerMetrics"
          [ env (microbenchmark1GenerateInput False 1_000) $ \i ->
                bench "1k" $ nfAppIO microbenchmark1ProcessInput i
          , env (microbenchmark1GenerateInput False 10_000) $ \i ->
                bench "10k" $ nfAppIO microbenchmark1ProcessInput i
          , env (microbenchmark1GenerateInput False 100_000) $ \i ->
                bench "100k" $ nfAppIO microbenchmark1ProcessInput i
          ]
        , bgroup "TxSubmissionV2"
          [ env (prepareEnv (TX.mkReceiveDuplicateFixture 100 3)) $ \fixture ->
                bench ("handleReceivedTxIds/duplicate-active/100existing/3txids/" ++ benchLoopsLabel) $
                  nfAppIO (TX.runReceiveDuplicateLoop benchLoops) fixture
          , env (prepareEnv (TX.mkResolvedAckFixture 100 10)) $ \fixture ->
                bench ("nextPeerAction/ack-resolved-retained/100advertisers/10txids/" ++ benchLoopsLabel) $
                  nfAppIO (TX.runPeerActionLoop benchLoops) fixture
          , env (prepareEnv (TX.mkFanoutFixture 100 3)) $ \fixture ->
                bench ("scenario/fanout-retained/100peers/3txids/" ++ benchLoopsLabel) $
                  nfAppIO (TX.runFanoutLoop benchLoops) fixture
          , env
              (prepareEnv (DirectV2.mkDirectServerFixture 1_000))
              $ \fixture ->
                bench "server/direct-interpreter/single-peer/1000batches" $
                  nfAppIO DirectV2.runDirectServerBenchmark fixture
          , env
              (prepareEnv (DirectV2.mkMultiPeerFixture 10 1_000))
              $ \fixture ->
                bench "server/direct-interpreter/multi-peer/10peers/1000batches" $
                  nfAppIO DirectV2.runDirectServerBenchmark fixture
          , env
              (prepareEnv (DirectV2.mkMultiPeerFixture 100 100))
              $ \fixture ->
                bench "server/direct-interpreter/multi-peer/100peers/100batches" $
                  nfAppIO DirectV2.runDirectServerBenchmark fixture
          , env
              (prepareEnv (DirectV2.mkMultiPeerFixture 100 1_000))
              $ \fixture ->
                bench "server/direct-interpreter/multi-peer/100peers/1000batches" $
                  nfAppIO DirectV2.runDirectServerBenchmark fixture
          ]
        ]
      ]

prepareEnv :: NFData a => a -> IO a
prepareEnv a = do
  _ <- evaluate (rnf a)
  performMajorGC
  pure a
