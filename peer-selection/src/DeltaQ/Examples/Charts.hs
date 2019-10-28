module DeltaQ.Examples.Charts where

import Graphics.Rendering.Chart.Plot.Candle (Candle (..))

import DeltaQ.Statistics

-- | Make a candle by using the 0th, 25th, 50th, 75th, and 100th percentiles of
-- a data set.
percentile_candlestick :: label -> [Double] -> Candle label Double
percentile_candlestick xlabel ds = Candle
  { candle_x     = xlabel
  , candle_low   = percentile 0   ds
  , candle_open  = percentile 25  ds
  , candle_mid   = percentile 50  ds
  , candle_close = percentile 75  ds
  , candle_high  = percentile 100 ds
  }

-- TODO figure out how to get a non-integral x axis...

{-
-- TODO move out or eliminate this orphan
instance PlotValue AWS.NetNode where
  -- toValue/fromValue go by way of the enum instance for NetNode.
  toValue   = fromIntegral . fromEnum
  fromValue = toEnum . round
  -- autoAxis :: [NetNode] -> AxisData NetNode
  -- Use the autoAxis :: [Int] -> AxisData Int but convert it by way of
  -- the Enum NetNode instance.
  autoAxis points = AxisData
    { _axis_visibility = _axis_visibility intAxisData
    , _axis_viewport   = \range x -> _axis_viewport intAxisData range (fromEnum x)
    , _axis_tropweiv   = \range d -> toEnum (_axis_tropweiv intAxisData range d)
    , _axis_ticks      = fmap (first toEnum) (_axis_ticks intAxisData)
    , _axis_labels     = (fmap . fmap) (first toEnum) (_axis_labels intAxisData)
    , _axis_grid       = [minBound..maxBound] -- fmap toEnum (_axis_grid intAxisData)
    }
    where
    intAxisData :: AxisData Int
    intAxisData = autoAxis (fmap fromEnum points)
-}
