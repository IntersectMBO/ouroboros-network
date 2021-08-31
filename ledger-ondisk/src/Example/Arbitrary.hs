{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
-- |

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Example.Arbitrary where

import Control.Monad.State.Strict
import Example.Types
import System.Random
import Data.Map (Map)
import Data.Monoid
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import Data.Vector ((!))
import Data.Int
import qualified Data.Vector as Vector

data GenState = GenState
  { gen :: !StdGen
  , txsPerBlockRange :: !(Int, Int)
  , txSizeRange :: !(Int, Int)
  , lotteryPayoutRange :: !(Int64, Int64)
  , gsUtxo :: !(Map TxIn TxOut)
  , gsAddrs :: !(Vector Addr)
  , targetNumUtxo :: !Int
  }

smallGenState :: IO GenState
smallGenState = do
  gen <- newStdGen
  let
    txsPerBlockRange = (1,5)
    txSizeRange = (2,2)
    lotteryPayoutRange = (10,20)
    gsAddrs = Vector.fromList [ Addr x | x <- [1 .. 5 ]]
    gsUtxo = Map.empty
    targetNumUtxo = 5;
  pure GenState{..}

myRandomR :: (MonadState GenState m, Random a) => (a, a) -> m a
myRandomR range = do
  GenState{gen} <- get
  let (x, g) = randomR range gen
  modify' $ \s -> s { gen = g }
  pure x

storeTx :: (MonadState GenState m) => Tx -> m ()
storeTx tx = case tx of
  LollyScramble outs -> modify' $ add_outs outs
  Tx _ outs -> modify' $ add_outs outs
  where
    add_outs outs s@GenState{gsUtxo} = s { gsUtxo = foldr add_out gsUtxo (zip [0..] outs)}
    add_out !(i, o) m = Map.insert (TxIn txid (TxIx i)) o m
    txid = txId tx

randomTxIn :: (MonadState GenState m) => m (Endo [TxIn], Coin)
randomTxIn = do
  GenState{gsUtxo} <- get
  i <- myRandomR (0, length gsUtxo - 1)
  let (k, TxOut _ c) = Map.elemAt i gsUtxo
  modify' $ \s@GenState{gsUtxo} -> s { gsUtxo = Map.delete k gsUtxo }
  pure (Endo $ (:) k, c)

randomTxOut :: (MonadState GenState m) => Coin -> m (Endo [TxOut], Coin)
randomTxOut (Coin x)
  | x == 0 = pure mempty
  | otherwise = do
      GenState{..} <- get
      v <- myRandomR (1, x)
      a <- myRandomR (0, length gsAddrs - 1)
      let addr = gsAddrs ! a
      pure (Endo $ (:) (TxOut addr (Coin v)), Coin (x - v))


randomTxOuts :: (MonadState GenState m) => Coin -> m (Endo [TxOut])
randomTxOuts c0 = do
  -- assert x > 0
  (r, c) <- randomTxOut c0
  if c == Coin 0
    then pure r
    else (r <>) <$> randomTxOuts c


randomTx :: MonadState GenState m => m Tx
randomTx = do
  GenState{targetNumUtxo, txSizeRange, gsUtxo, lotteryPayoutRange} <- get
  tx_size <- myRandomR txSizeRange
  do_lottery <- if tx_size > length gsUtxo
    then pure True
    else do
       p <- myRandomR (0.0, 1.0)
       let
         denominator :: Double
         denominator = fromIntegral targetNumUtxo
         numerator = fromIntegral (length gsUtxo)
       pure $ p > (numerator / denominator)

  tx <- if do_lottery
    then do
      payout <- myRandomR lotteryPayoutRange
      Endo outs <- randomTxOuts (Coin payout)
      pure . LollyScramble $ outs []
    else do
      (Endo (($ []) -> ins), coin) <- foldlM (\r _ -> (r <>) <$> randomTxIn) mempty [1..tx_size]
      Endo (($ []) -> outs) <- randomTxOuts coin
      pure $ Tx ins outs

  storeTx tx
  pure tx

randomBlock :: MonadState GenState m => m Block
randomBlock = do
  GenState{txsPerBlockRange} <- get
  num_txs <- myRandomR txsPerBlockRange
  let one_tx r _i = do
        tx <- randomTx
        pure $ r <> Endo ((:) tx)
  (Endo txs) <- foldlM one_tx mempty [1..num_txs]
  pure $ txs []
