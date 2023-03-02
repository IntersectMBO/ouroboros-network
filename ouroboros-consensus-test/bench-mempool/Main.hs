{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main (main) where

import           Control.Arrow (first)
import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import           Data.Maybe (fromMaybe)
import           Data.Set ()
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import           System.Exit (die, exitFailure)
import           Test.Tasty.Bench (CsvPath (CsvPath), bench, benchIngredients,
                     bgroup, env, nfIO)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.Options (changeOption)
import           Test.Tasty.Runners (parseOptions, tryIngredients)

import           Data.Aeson

import qualified Control.Tracer as Tracer

import           Bench.Consensus.Mempool
import           Bench.Consensus.Mempool.TestBlock (TestBlock)
import qualified Bench.Consensus.Mempool.TestBlock as TestBlock
import           Bench.Consensus.MempoolWithMockedLedgerItf
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool

main :: IO ()
main = do
    let csvFilePath = "mempool-benchmarks.csv"
    runBenchmarks csvFilePath
    rawValues <- parseBenchmarkResults csvFilePath
    convertCsvRowsToJsonObjects rawValues "mempool-benchmarks.json"
  where
    runBenchmarks csvFilePath = do
        opts <- parseOptions benchIngredients benchmarkJustAddingTransactions
        let opts' = changeOption (Just . fromMaybe (CsvPath csvFilePath)) opts
        case tryIngredients benchIngredients opts' benchmarkJustAddingTransactions of
          Nothing               -> exitFailure
          Just    runIngredient -> do
            success <- runIngredient
            unless success exitFailure
      where
        benchmarkJustAddingTransactions =
            bgroup "Just adding" $
                fmap benchAddNTxs [10_000, 1_000_000]
          where
            benchAddNTxs n =
                env (let txs = mkNTryAddTxs n in fmap (, txs) (openMempoolWithCapacityFor txs))
                -- The irrefutable pattern was added to avoid the
                --
                -- > Unhandled resource. Probably a bug in the runner you're using.
                --
                -- error reported here https://hackage.haskell.org/package/tasty-bench-0.3.3/docs/Test-Tasty-Bench.html#v:env
                (\ ~(mempool, txs) -> bgroup (show n <> " transactions") [
                    bench    "benchmark"     $ nfIO $ run        mempool txs
                  , testCase "test"          $        testAddTxs mempool txs
                  , testCase "txs length"    $ length txs @?= n
                  ]
                )
              where
                testAddTxs mempool txs = do
                    run mempool txs
                    mempoolTxs <- getTxs mempool
                    mempoolTxs @?= txsAddedInCmds txs

    parseBenchmarkResults csvFilePath = do
        csvData <- BL.readFile csvFilePath
        case Csv.decode Csv.HasHeader csvData of
          Left err   -> die err
          Right rows -> pure rows

    -- Output the mempool benchmark results as a JSON file, which conforms to
    -- the input expected by
    -- https://github.com/benchmark-action/github-action-benchmark
    convertCsvRowsToJsonObjects rows outFilePath =
      encodeFile outFilePath $ fmap convertRowToJsonObject rows
      where
        convertRowToJsonObject (name:mean:_) =
          object [ "name"  .= adjustName name
                 , "value" .= adjustedMean
                 , "unit"  .= unit
                 ]
          where
            adjustName = Text.replace "."          " "
                       . Text.replace ".benchmark" ""

            adjustedMean :: Integer
            (adjustedMean, unit) = first round
                                 $ convertPicosecondsWithUnit
                                 $ fromInteger
                                 $ textToInt mean
              where
                textToInt = either error fst . Text.Read.decimal

            -- Convert a number of picoseconds to the largest time unit that
            -- makes the conversion greater or equal than one.
            convertPicosecondsWithUnit :: Double -> (Double, String)
            convertPicosecondsWithUnit n
                |                        numberOfDigits  <= 4  = (n       , "picoseconds" )
                | 4 <= numberOfDigits  && numberOfDigits <  7  = (n / 1e3 , "nanoseconds" )
                | 7 <= numberOfDigits  && numberOfDigits <  10 = (n / 1e6 , "microseconds")
                | 10 <= numberOfDigits && numberOfDigits <  13 = (n / 1e9 , "milliseconds")
                | 13 <= numberOfDigits                         = (n / 1e12, "seconds"     )
              where
                numberOfDigits :: Int
                numberOfDigits = floor (logBase 10 n) + 1
            convertPicosecondsWithUnit _ = error "All the cases should be covered by the conditions above"

        convertRowToJsonObject _             = error "Wrong format"

{-------------------------------------------------------------------------------
  Adding TestBlock transactions to a mempool
-------------------------------------------------------------------------------}

openMempoolWithCapacityFor :: [MempoolCmd TestBlock] ->  IO (MempoolWithMockedLedgerItf IO TestBlock)
openMempoolWithCapacityFor cmds =
    openMempoolWithMockedLedgerItf capacityRequiredByCmds
                                   Tracer.nullTracer
                                   TestBlock.txSize
                                   TestBlock.sampleMempoolAndModelParams
  where
    capacityRequiredByCmds = Mempool.mkCapacityBytesOverride  totalTxsSize
      where totalTxsSize = sum $ fmap TestBlock.txSize $ txsAddedInCmds cmds

mkNTryAddTxs :: Int -> [MempoolCmd TestBlock.TestBlock]
mkNTryAddTxs 0 = []
mkNTryAddTxs n =        [TryAdd [TestBlock.mkTx [] [TestBlock.Token 0]]]
                <> fmap (TryAdd . (:[]) . mkSimpleTx) (zip [0 .. n - 2] [1 .. n - 1])
  where
    mkSimpleTx (x, y) = TestBlock.mkTx [TestBlock.Token (fromIntegral x)]
                                       [TestBlock.Token (fromIntegral y)]
