{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE TupleSections      #-}

module Main (main) where

import           Data.Set ()
import           Test.Tasty.HUnit (testCase, (@?=))

import qualified Control.Tracer as Tracer
import           Test.Tasty.Bench (bench, bgroup, defaultMain, env, nfIO)

import           Bench.Consensus.Mempool
import           Bench.Consensus.Mempool.TestBlock (TestBlock)
import qualified Bench.Consensus.Mempool.TestBlock as TestBlock
import           Bench.Consensus.MempoolWithMockedLedgerItf
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool

main :: IO ()
main = defaultMain [ benchmarkJustAddingTransactions ]
  where
    benchmarkJustAddingTransactions =
        bgroup "Just adding transactions" $
            fmap benchAddNTxs [1_000, 10_000, 100_000, 1_000_000]
      where
        benchAddNTxs n =
          env (let txs = mkNTryAddTxs n in fmap (, txs) (openMempoolWithCapacipyFor txs))
              -- The irrefutable pattern was added to avoid the
              --
              -- > Unhandled resource. Probably a bug in the runner you're using.
              --
              -- error reported here https://hackage.haskell.org/package/tasty-bench-0.3.3/docs/Test-Tasty-Bench.html#v:env
              (\ ~(mempool, txs) -> bgroup (show n) [
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

{-------------------------------------------------------------------------------
  Adding TestBlock transactions to a mempool
-------------------------------------------------------------------------------}

openMempoolWithCapacipyFor :: [MempoolCmd TestBlock] ->  IO (MempoolWithMockedLedgerItf IO TestBlock)
openMempoolWithCapacipyFor cmds =
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
