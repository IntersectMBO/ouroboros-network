{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Debug.Trace (traceMarkerIO)
import System.Random.SplitMix qualified as SM
import Test.Tasty.Bench

import Ouroboros.Network.TxSubmission.Inbound.V2.Decision qualified as Tx
import Test.Ouroboros.Network.TxSubmission.TxLogic qualified as TX
           (mkDecisionContext)

import Test.Ouroboros.Network.PeerSelection.PeerMetric
           (microbenchmark1GenerateInput, microbenchmark1ProcessInput)

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
        , bgroup "TxLogic"
          [ env (do let a = TX.mkDecisionContext (SM.mkSMGen 131) 10
                    evaluate (rnf a)
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                (\a ->
                     bench "makeDecisions: 10"
                   $ nf (\(t, p, st) -> Tx.makeDecisions t p st) a
                )
          , env (do let a = TX.mkDecisionContext (SM.mkSMGen 131) 100
                    evaluate (rnf a)
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                (\a ->
                     bench "makeDecisions: 100"
                   $ nf (\(t, p, st) -> Tx.makeDecisions t p st) a
                )
          , env (do let a = TX.mkDecisionContext (SM.mkSMGen 361) 1_000
                    evaluate (rnf a)
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                (\a ->
                     bench "makeDecisions: 1000"
                   $ nf (\(t, p, st) -> Tx.makeDecisions t p st) a
                )
{-
          , env (do
                    smGen <- SM.initSMGen
                    print smGen
                    let a = TX.mkDecisionContext smGen 1000
                    evaluate (rnf a)
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                (\a ->
                     bench "makeDecisions: random"
                   $ nf (uncurry Tx.makeDecisions) a
                )
-}
          ]
        ]
      ]
