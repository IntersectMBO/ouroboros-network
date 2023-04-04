{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main (main) where

import           Bench.Consensus.Mempool
import           Bench.Consensus.Mempool.Params
import           Bench.Consensus.Mempool.TestBlock (TestBlock)
import qualified Bench.Consensus.Mempool.TestBlock as TestBlock
import           Bench.Consensus.MempoolWithMockedLedgerItf
import           Control.Arrow (first)
import           Control.Monad (unless)
import qualified Control.Tracer as Tracer
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import           Data.Maybe (fromMaybe)
import           Data.Set ()
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool
import           System.Exit (die, exitFailure)
import           Test.Tasty.Bench (CsvPath (CsvPath), bench, benchIngredients,
                     bgroup, env, nfIO)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.Options (changeOption)
import           Test.Tasty.Runners (parseOptions, tryIngredients)

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
            -- TODO: run with LMDB
            let ns = [10_000, 20_000, 30_000] in
            bgroup "Just adding" [
                bgroup "scenario1" [
                    benchAddNTxs params txs n
                  | n <- ns
                  , let (params, txs) = scenario1 n
                  ]
              , bgroup "scenario2 2160" [
                    benchAddNTxs params txs n
                  | n <- ns
                  , let (params, txs) = scenario2 n 2160
                  ]
              , bgroup "scenario3" [
                    benchAddNTxs params txs n
                  | n <- ns
                  , let (params, txs) = scenario3 n
                  ]
              , bgroup "scenario4" [
                    benchAddNTxs params txs n
                  | n <- ns
                  , let (params, txs) = scenario4 n
                  ]
              , bgroup "scenario5" [
                    benchAddNTxs params txs n
                  | n <- ns
                  , let (params, txs) = scenario5 n
                  ]
              , bgroup "scenario6" [
                    benchAddNTxs params txs n
                  | n <- ns
                  , let (params, txs) = scenario6 n
                  ]
              ]
          where
            benchAddNTxs params txs0 n =
                env ((,txs0) <$> openMempoolWithCapacityFor params txs0 )
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

openMempoolWithCapacityFor ::
     InitialMempoolAndModelParams IO TestBlock
  -> [MempoolCmd TestBlock]
  -> IO (MempoolWithMockedLedgerItf IO TestBlock)
openMempoolWithCapacityFor params cmds =
    openMempoolWithMockedLedgerItf capacityRequiredByCmds
                                   Tracer.nullTracer
                                   TestBlock.txSize
                                   params
  where
    capacityRequiredByCmds = Mempool.mkCapacityBytesOverride  totalTxsSize
      where totalTxsSize = sum $ fmap TestBlock.txSize $ txsAddedInCmds cmds

{-------------------------------------------------------------------------------
  Scenarios
-------------------------------------------------------------------------------}

-- | Scenario 1: The backing store is empty. The changelog is empty. There are
-- @n@ linked mempool transactions. i.e., each transaction produces a token
-- that the /next/ transaction consumes.
scenario1 ::
     Int
  -> ( InitialMempoolAndModelParams m TestBlock
     , [MempoolCmd TestBlock.TestBlock]
     )
scenario1 n =
  let
    lst   = ledgerStateFromTokens []
    blks  = []
    cmds  = let gtxs = mkSimpleGenesisGenTx 0
                     : [ mkSimpleGenTx x y
                       | (x, y) <- zip [0 .. n - 2] [1 .. n - 1]
                       ]
            in  fmap mkSimpleTryAdd gtxs
  in
    (mkParams lst blks defaultLedgerDbCfg defaultInMemoryBSS, cmds)

-- | Scenario 2: The backing store is empty. The changelog has @m@ linked (see
-- 'scenario1') entries. There are @n@ linked mempool transactions. The last
-- entry in the changelog and the first mempool transaction are also linked.
scenario2 ::
     Int
  -> Int
  -> ( InitialMempoolAndModelParams m TestBlock
     , [MempoolCmd TestBlock.TestBlock]
     )
scenario2 n m =
  let
    lst   = ledgerStateFromTokens []
    blks  = let txs  = mkSimpleGenesisTx (-m)
                     : [ mkSimpleTx x y
                       | (x, y) <- zip [-m .. -2] [-m+1 .. -1]
                       ]
            in  testBlocksFromTxs txs
    cmds  = let gtxs = mkSimpleGenTx (-1) 0
                     : [ mkSimpleGenTx x y
                       | (x, y) <- zip [0 .. n - 2] [1 .. n - 1]
                       ]
            in  fmap mkSimpleTryAdd gtxs
  in
    (mkParams lst blks defaultLedgerDbCfg defaultInMemoryBSS, cmds)

-- | Scenario 3: The backing store is empty. The changelog is empty. There
-- are @n@ mempool transactions that each produce a new token.
scenario3 ::
     Int
  -> ( InitialMempoolAndModelParams m TestBlock
     , [MempoolCmd TestBlock.TestBlock]
     )
scenario3 n =
  let
    rbs  = [0..n-1]
    lst  = ledgerStateFromTokens []
    blks = []
    cmds = [TryAdd [mkSimpleGenesisGenTx x] | x <- rbs]
  in
    (mkParams lst blks defaultLedgerDbCfg defaultInMemoryBSS, cmds)

-- | Scenario 4: The backing store has @n@ tokens. The changelog is empty. There
-- are @n@ mempool transactions that each consume one of the @n@ tokens.
scenario4 ::
     Int
  -> ( InitialMempoolAndModelParams m TestBlock
     , [MempoolCmd TestBlock.TestBlock]
     )
scenario4 n =
  let
    rbs  = [0..n-1]
    lst  = ledgerStateFromTokens [TestBlock.Token x | x <- rbs]
    blks = []
    cmds = [TryAdd [mkSimpleGenTx x (n + x)] | x <- rbs]
  in
    (mkParams lst blks defaultLedgerDbCfg defaultInMemoryBSS, cmds)

-- | Scenario 5: The backing store is empty. There are @n@ entries in the
-- changelog that produce a token. There are @n@ mempool transactions that each
-- consume one of the @n@ tokens.
scenario5 ::
     Int
  -> ( InitialMempoolAndModelParams m TestBlock
     , [MempoolCmd TestBlock.TestBlock]
     )
scenario5 n =
  let
    range = [0..n-1]
    lst  = ledgerStateFromTokens []
    blks = testBlocksFromTxs [mkSimpleGenesisTx x | x <- range]
    cmds = [TryAdd [mkSimpleGenTx x (n + x)] | x <- range]
  in
    (mkParams lst blks defaultLedgerDbCfg defaultInMemoryBSS, cmds)

-- | Scenario 6: The backing store has @n@ tokens. There are @n@ entries in the
-- changelog that each consume one of the @n@ tokens, and each of the @n@
-- entries in the changelog also produces a new token. There are @n@ mempool
-- transactions that each consume one of the @n@ new tokens.
scenario6 ::
     Int
  -> ( InitialMempoolAndModelParams m TestBlock
     , [MempoolCmd TestBlock.TestBlock]
     )
scenario6 n =
  let
    range = [0..n-1]
    lst = ledgerStateFromTokens [TestBlock.Token x | x <- range]
    blks = testBlocksFromTxs [mkSimpleTx x (n + x) | x <- range]
    cmds = [TryAdd [mkSimpleGenTx (n + x) (2*n + x)] | x <- range]
  in
    (mkParams lst blks defaultLedgerDbCfg defaultInMemoryBSS, cmds)
