{-# LANGUAGE CPP                #-}
{-# LANGUAGE NumericUnderscores #-}

-- pPrint
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Debug.Trace (traceMarkerIO)
import System.Mem (performMajorGC)
import Test.Tasty.Bench
import Text.Pretty.Simple (pPrint)

import Ouroboros.Network.TxSubmission.Inbound.V2.Decision qualified as Tx
import Ouroboros.Network.TxSubmission.Inbound.V2.State (SharedTxState (..))
import Test.Ouroboros.Network.TxSubmission.TxLogic qualified as TX
           (mkDecisionContexts, printTxLogicBenchmarkContexts)

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
          [ env (do let a = TX.mkDecisionContexts 131 100 10
                    evaluate (rnf a)
#ifdef TXLOGIC_PRINT
                    TX.printTxLogicBenchmarkContexts a
#endif
                    -- pPrint a
                    performMajorGC
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                (\as ->
                     bench "makeDecisions: 100 x 10"
                   $ let run (now, policy, state) =
                           Tx.makeDecisions now policy state (peerTxStates state)
                     in nf (map run) as
                )
          , env (do let a = TX.mkDecisionContexts 131 100 100
                    evaluate (rnf a)
#ifdef TXLOGIC_PRINT
                    TX.printTxLogicBenchmarkContexts a
#endif
                    -- pPrint a
                    performMajorGC
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                (\as ->
                     bench "makeDecisions: 100 x 100"
                   $ let run (now, policy, state) =
                           Tx.makeDecisions now policy state (peerTxStates state)
                     in nf (map run) as
                )
          , env (do let a = TX.mkDecisionContexts 361 100 1_000
                    evaluate (rnf a)
#ifdef TXLOGIC_PRINT
                    TX.printTxLogicBenchmarkContexts a
#endif
                    -- pPrint a
                    performMajorGC
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                (\as ->
                     bench "makeDecisions: 100 x 1000"
                   $ let run (now, policy, state) =
                           Tx.makeDecisions now policy state (peerTxStates state)
                     in nf (map run) as
                )
{-
          , env (do
                    let a = TX.mkDecisionContexts 42 100 1000
                    evaluate (rnf a)
                    traceMarkerIO "evaluated decision context"
                    return a
                )
                (\a ->
                     bench "makeDecisions: random"
                   $ let run (now, policy, state) =
                           Tx.makeDecisions now policy state (peerTxStates state)
                     in nf (map run) a
                )
-}
          ]
        ]
      ]
