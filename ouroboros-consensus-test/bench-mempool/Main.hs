{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists    #-}


module Main (main) where

import           Control.Monad (void)
import           Data.Set ()
import           Test.Tasty.HUnit (testCase, (@?=))

import qualified Control.Tracer as Tracer
import           Test.Tasty.Bench (bench, bgroup, defaultMain, nfIO)

import           Bench.Consensus.Mempool
import           Bench.Consensus.Mempool.TestBlock (TestBlock)
import qualified Bench.Consensus.Mempool.TestBlock as TestBlock
import           Bench.Consensus.MempoolWithMockedLedgerItf
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool

main :: IO ()
main = defaultMain [ benchmarkJustAddingTransactions ]
  where
    benchmarkJustAddingTransactions =
        bgroup "Just adding transactions"
          $  fmap benchAddNTxs [1_000, 10_000, 100_000, 1_000_000]
          <>
             [ testCase "Transactions are added" $ do
                 let
                   n   = 1000
                   txs = mkNTryAddTxs 1000
                 mempool    <- addNTxs txs
                 mempoolTxs <- getTxs mempool
                 mempoolTxs @?= txsAddedInCmds txs
                 length mempoolTxs @?= n
             ]
      where
        benchAddNTxs n = bench (show n) $ nfIO $ addNTxs' $ mkNTryAddTxs n
        -- FIXME: check that we need forcing. Forcing the list does not seem to make a difference

{-------------------------------------------------------------------------------
  Adding TestBlock transactions to a mempool
-------------------------------------------------------------------------------}

addNTxs' :: [MempoolCmd TestBlock] -> IO ()
addNTxs' = void . addNTxs

addNTxs :: [MempoolCmd TestBlock] -> IO (MempoolWithMockedLedgerItf IO TestBlock)
addNTxs cmds = do
  let capacityRequiredByCmds = Mempool.mkCapacityBytesOverride  totalTxsSize   -- TODO: add a convenience function in the Mempool API.
        where totalTxsSize = sum $ fmap TestBlock.txSize $ txsAddedInCmds cmds
  mempool <- openMempoolWithMockedLedgerItf
                 capacityRequiredByCmds
                 Tracer.nullTracer
                 TestBlock.txSize
                 TestBlock.sampleMempoolAndModelParams
  run mempool cmds
  pure mempool

mkNTryAddTxs :: Int -> [MempoolCmd TestBlock.TestBlock]
mkNTryAddTxs 0 = []
mkNTryAddTxs n =        [TryAdd [TestBlock.mkTx [] [TestBlock.Token 0]]]
                <> fmap (TryAdd . (:[]) . mkSimpleTx) (zip [0 .. n - 2] [1 .. n - 1])
  where
    mkSimpleTx (x, y) = TestBlock.mkTx [TestBlock.Token (fromIntegral x)]
                                       [TestBlock.Token (fromIntegral y)]
