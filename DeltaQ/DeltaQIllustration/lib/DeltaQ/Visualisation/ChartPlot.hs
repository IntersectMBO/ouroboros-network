{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module DeltaQ.Visualisation.ChartPlot
where

import           Control.Lens
import           Control.Monad.Reader
import           Data.List
import           Data.Maybe
import           Graphics.Rendering.Chart.Easy
import           System.Random.MWC

import           DeltaQ.RationalProbabilityDoubleDelay
import           DeltaQ.Numeric.CDF

data DelayExtension delay
  = NoDelayExtension
  | AbsoluteDelay delay
  | AbsoluteDelayExtension delay
  | RelativeDelayExtension delay
  deriving (Show)

instance Default (DelayExtension delay) where
  def = NoDelayExtension


data PlotInfo delay
  = PI { _noSamples :: Int
       , _maxDelay  :: DelayExtension delay
       }
makeLenses ''PlotInfo

instance Default (PlotInfo d) where
  def = PI 1000 def

-- | Plot an `EmpiricalCDF`. 

{- 
plotCDF :: (MonadReader PlotInfo m, MonadIO m)
        => String
        -> EmpiricalCDF
        -> m (EC l (PlotLines Double Rational))
plotCDF l'name d' = do
  n  <- view noSamples
  de <- view maxDelay
  xs <- fmap (applyDelayExtension de)
      . fmap (normaliseCDF p'mass)
      . fmap (asCDF n . take n)
      . liftIO $ runSampleDQ d
  return $ line l'name [xs]
  where
    p'mass :: ProbabilityMass
    p'mass = fromRational $ tangibleProbMass d'
    d = fromJust $ tangibleRandomness d'
-}

-- | Generate a CDF plot by Monte-Carlo evaluation of the given
--   `DeltaQ`. The number of samples for the plot is the number of
--   samples taken from the supporting improper random variable. Uses
--   the syntatic structure to evaluate the tangible mass in the
--   distribution.
generateCDFPlot :: (MonadReader (PlotInfo Double) m, MonadIO m)
                => GenIO
                -> String
                -> DeltaQ
                -> m (EC l (PlotLines Double Double))
generateCDFPlot gen l'name dq = do
  n  <- view noSamples
  de <- view maxDelay
  let f :: [Maybe Double] -> [(Double, Double)]
      f = (applyDelayExtension de)
        . map (\(a,b) -> (a, fromRational b))
        . (asCDF n) 
  vs <- fmap f . replicateM n . liftIO $ sampleDeltaQ gen dq
  return $ line l'name [vs]
  where
    t'max  = mmmvMax . maybe no'tangible'mass id . mmmv $ dq
    asCDF n xs
      = (0,0) : zip ((sort $ catMaybes xs) ++ [t'max]) cs
      where
        cs = map ( / fromIntegral n) $ iterate (+1) 1
    no'tangible'mass
      = error "generateCDFPlot: no tangible mass"

-- | Optionall construct an additional horizontal line for the right
--   hand side of the delay Numericall CDF
applyDelayExtension :: (Num delay, Ord delay)
                    => DelayExtension delay
                    -> [(delay, t)]
                    -> [(delay, t)]
applyDelayExtension NoDelayExtension zs
  = zs
applyDelayExtension (AbsoluteDelay x) zs
  | null zs
    = zs
  | l'x > x
    = zs
  | otherwise
    = zs ++ [(x, l'y)]
  where
    (l'x, l'y) = last zs
applyDelayExtension (AbsoluteDelayExtension x) zs
  | null zs
    = zs
  | otherwise
    = zs ++ [(l'x + x , l'y)]
  where
    (l'x, l'y) = last zs
applyDelayExtension (RelativeDelayExtension x) zs
  | null zs
    = zs
  | otherwise
    = zs ++ [(l'x * (1 + x) , l'y)]
  where
    (l'x, l'y) = last zs
