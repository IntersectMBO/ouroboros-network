{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Shelley.Ledger.Stub
  ( stubComputation
  , stubComputationArg
  , calibrateStubComputationArgForTime
  )
where

import Control.DeepSeq
import Data.Bits
import Data.List (foldl')
import Data.Int
import System.CPUTime
import Text.Printf

import qualified Data.IORef       as IO
import qualified System.IO.Unsafe as IO

stubComputation :: Int -> Integer
stubComputation = fibWli

stubComputationArg :: Int
stubComputationArg = IO.unsafePerformIO (IO.readIORef stubComputationArgIORef)

calibrateStubComputationArgForTime :: Double -> Double -> IO ()
calibrateStubComputationArgForTime dt precision = do
  putStrLn $ mconcat
    [ "stub computation: calibrating fib(N) for dt=", show dt
    , ", precision=", show precision ]
  searchIntDoubleM
    (measuringPureIO stubComputation)
    stubComputationBase dt precision
    >>= setStubComputationArg
 where
   stubComputationBase = 100

-- An evolution of the original "Fastest Fib on the West", by William Lee Irwin III,
-- (also known as Nadya Yvette Chambers):
--   http://www.haskell.org/pipermail/haskell-cafe/2005-January/008839.html
-- This was picked because the minimum integer step difference is small enough,
-- to provide small errors relative to desired execution time.
fibWli :: Int -> Integer
fibWli n =
  snd . foldl_ fib_ (1, 0) . dropWhile not $
    [testBit n k | k <- let s = finiteBitSize n in [s-1,s-2..0]]
 where
   fib_ (f, g) p
     | p         = (f*(f+2*g), ss)
     | otherwise = (ss, g*(2*f-g))
    where ss = f*f+g*g
   foldl_ = foldl' -- '

-- Given a monotonic, monadic 'f', an initial guess x, the desired y and precision,
-- find x', such that f x' is within precision from y.
searchIntDoubleM
  :: forall m
  .  (Monad m, m ~ IO)
  => (Int -> m Double)
  -> Int
  -> Double
  -> Double
  -> m Int
searchIntDoubleM f x0 yTarget precision =
  f x0 >>= contain True Nothing x0 >>= uncurry shrink
 where
   -- Establish upper/lower boundaries.
   contain :: Bool -> Maybe (Int, Double) -> Int -> Double -> m ((Int, Double), (Int, Double))
   contain _ Nothing x' y' =
     f x'' >>= contain growing (Just (x', y')) x''
    where
      (,) growing x'' =
        if yTarget > y'
        then (True,  x' * 2)
        else (False, x' `div` 2)
   contain growing (Just (x, y)) x' y' = do
     printf "contain %s %d/%f -> %d/%f\n" (show growing) x y x' y'
     if needMoreRange
     then contain growing (Just (x', y')) x'' =<< f x''
     else pure answer
    where
      (,,) needMoreRange x'' answer =
        if growing
        then (yTarget > y', x'     * 2, ((x, y), (x', y')))
        else (yTarget < y', x' `div` 2, ((x', y'), (x, y)))

   -- Shrink boundaries up to precision.
   shrink :: (Int, Double) -> (Int, Double) -> m Int
   shrink l@(x1, y1) u@(x2, y2) = do
     printf "shrink %d/%f .. %d/%f prec %f\n" x1 y1 x2 y2 (abs (y1 - y2))
     if abs (y1 - y2) < precision || x2 - x1 == 1
     then pure (if lowerBetter then x1 else x2)
     else do
       yMid <- f xMid
       if yMid < yTarget
         then shrink   (xMid, yMid) u
         else shrink l (xMid, yMid)
    where
      lowerBetter = abs (yTarget - y1) < abs (yTarget - y2)
      xMid = (x2 + x1) `div` 2
{-# NOINLINE searchIntDoubleM #-}

stubComputationArgIORef :: IO.IORef Int
stubComputationArgIORef =
  IO.unsafePerformIO (IO.newIORef 0)
{-# NOINLINE stubComputationArgIORef #-}

setStubComputationArg :: Int -> IO ()
setStubComputationArg x = do
  putStrLn $ mconcat
    [ "stub computation: setting N to ", show x ]
  IO.writeIORef stubComputationArgIORef x

measuringPureIO :: NFData b => (a -> b) -> a -> IO Double
measuringPureIO f x =
  minimum <$> mapM doTimes (take 3 $ repeat 1)
 where
   doTimes times = do
     start <- getCPUTime
     _ <- nf' rnf f x times
     end   <- getCPUTime
     pure $ fromIntegral (end - start) / (10^(12 :: Int))
{-# NOINLINE measuringPureIO #-}

-- | Generate a function which applies an argument to a function a
-- given number of times, reducing the result to normal form.
-- NOTE: from criterion-measurement
nf' :: (b -> ()) -> (a -> b) -> a -> (Int64 -> IO ())
nf' reduce f x = go
  where
    go n | n <= 0    = return ()
         | otherwise = let !y = f x
                       in reduce y `seq` go (n-1)
{-# NOINLINE nf' #-}
