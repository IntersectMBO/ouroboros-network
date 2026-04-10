{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import System.Mem (performMajorGC)
import Test.Tasty.Bench

import Test.Ouroboros.Network.PeerSelection.PeerMetric
           (microbenchmark1GenerateInput, microbenchmark1ProcessInput)
import Test.Ouroboros.Network.TxSubmission.TxLogic qualified as TX

benchLoops :: Int
benchLoops = 1_0000

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
                bench "handleReceivedTxIds/duplicate-active/100existing/3txids/x1000" $
                  nfAppIO (TX.runReceiveDuplicateLoop benchLoops) fixture
          , env (prepareEnv (TX.mkForeignRejectedFixture 100 10)) $ \fixture ->
                bench "nextPeerAction/ack-foreign-rejected/100advertisers/10txids/x1000" $
                  nfAppIO (TX.runPeerActionLoop benchLoops) fixture
          , env (prepareEnv (TX.mkFanoutFixture 100 3)) $ \fixture ->
                bench "scenario/fanout/100peers/3txids/x1000" $
                  nfAppIO (TX.runFanoutLoop benchLoops) fixture
          ]
        ]
      ]

prepareEnv :: NFData a => a -> IO a
prepareEnv a = do
  _ <- evaluate (rnf a)
  performMajorGC
  pure a
