module Main (
    main
  ) where

import           Control.Monad
import           System.IO
import           Text.Printf (printf)

import           Data.Number.LogFloat (LogFloat)
import qualified Data.Number.LogFloat as LF
import           Statistics.Distribution
import           Statistics.Distribution.Binomial

-- | Compute the probability of seeing more than @k@ blocks in @n@ slots
moreThanK ::
     Double  -- ^ Active slot coefficient
  -> Int     -- ^ Security parameter (@k@)
  -> Int     -- ^ Number of slots
  -> LogFloat
moreThanK f k n =
    LF.sum [LF.logToLogFloat $ logProbability d i | i <- [k + 1 .. n]]
  where
    d :: BinomialDistribution
    d = binomial n f

defaultS ::
     Double  -- ^ Active slot coefficient
  -> Int     -- ^ Security parameter (@k@)
  -> Int
defaultS f k = floor (fromIntegral k / f) `div` 4

main :: IO ()
main = do
    forM_ [s .. 4 * s] $ \n -> do
      putStrLn $ show n ++ "\t" ++ showLogFloat (moreThanK f k n)
      hFlush stdout
  where
    f = 0.05
    k = 2160
    s = defaultS f k

{-------------------------------------------------------------------------------
  LogFloat util
-------------------------------------------------------------------------------}

showLogFloat :: LogFloat -> String
showLogFloat lf = printf "%6.4f * 10 ^ %d" m e
  where
    (m, e) = logFloatToScientific lf

logFloatToScientific :: LogFloat -> (Double, Int)
logFloatToScientific lf = (m, e)
  where
    l :: Double
    l = LF.logFromLogFloat lf

    e :: Int
    e = floor $ l / log10

    m :: Double
    m = exp $ l - log10 * fromIntegral e

    log10 :: Double
    log10 = log 10
