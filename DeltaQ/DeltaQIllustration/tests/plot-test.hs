{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main)
where

import DeltaQ.Visualisation.ChartPlot
import DeltaQ.QTA.Support

import Control.Lens
import Control.Monad.Reader
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Gtk
import System.Random.MWC

dev'mode :: Bool
dev'mode = False

type Delay = Double

main :: IO ()
main = (flip runReaderT) test'plot'info $ do
  gen <- liftIO createSystemRandom
  r <- generateCDFPlot gen "QTA" (fromQTA qta)
  asPlot "ExampleQTA" $ do
    layout_title .= "QTA"
    plot r
 where
    asPlot fn m
      = l'plot fn $ do
         layout_x_axis . laxis_title .= "Delay (s)"
         layout_y_axis . laxis_title .= "Probability of completion"
         layout_y_axis . laxis_generate .= scaledAxis def (0,1)
         setColors $ [opaque blue] ++ repeat  (opaque black)
         m
      where
        l'plot f'
          = liftIO . if dev'mode
                     then error "no gtk support" -- toWindow 600 600
                     else toFile fo (f' ++ ".png")
        fo = FileOptions (900,900) PNG
    qta = [(50/100, 0.5), (90/100, 0.7), (95/100, 1), (99/100,2)]

test'plot'info :: PlotInfo Delay
test'plot'info
 = set noSamples 10000
 . set maxDelay (RelativeDelayExtension 0.2)
 $ def 
   
