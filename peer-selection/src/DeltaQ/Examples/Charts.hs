module DeltaQ.Examples.Charts where

import Data.Colour (opaque)
import Data.Colour.Names (white)
import Data.Default.Class (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graphics.Rendering.Chart.Axis.Types (AxisData (..), PlotValue (..))
import Graphics.Rendering.Chart.Backend.Types (FillStyle (..))
import Graphics.Rendering.Chart.Easy (AlphaColour, Layout (..), solidFillStyle, withOpacity)
import Graphics.Rendering.Chart.Plot.Candle (PlotCandle (..), Candle (..))
import Graphics.Rendering.Chart.Plot.Types (toPlot)

import qualified DeltaQ.Examples.AWS as AWS
import DeltaQ.Statistics

-- | Make a candle by using the 0th, 25th, 50th, 75th, and 100th percentiles
-- of the time-to-reach data for a given node.
local_candle :: node -> [Double] -> Candle node Double
local_candle xlabel ds = Candle
  { candle_x     = xlabel
  , candle_low   = percentile 0   ds
  , candle_open  = percentile 25  ds
  , candle_mid   = percentile 50  ds
  , candle_close = percentile 75  ds
  , candle_high  = percentile 100 ds
  }

candle_plot :: AlphaColour Double -> Map node [Double] -> PlotCandle node Double
candle_plot colour dats = def
  { _plot_candle_values = fmap (uncurry local_candle) (Map.toList dats)
  , _plot_candle_fill   = True
  , _plot_candle_rise_fill_style = FillStyleSolid colour
  , _plot_candle_fall_fill_style = FillStyleSolid colour
  }

-- to_plot . candle_plot :: Map node [Double] -> Plot node Double
--
-- withOpacity can be used to get an alpha colour.

-- | Superimpose multiple candle plots each with a given colour for fill style.
-- TBD will it be readable? May need to configure the line style as well.
--
-- Type should be more along these lines
--   candle_plots :: ( PlotValue node ) => [(AlphaColour Double, Map node [Double])] -> Layout node Double
-- but since axis data in charts is a real mystery / a lot of work to set up,
-- we'll just convert the map to Int keys using the enum instance, then the
-- X axis will be ints.
candle_plots :: (Enum node) => [(AlphaColour Double, Map node [Double])] -> Layout Int Double
candle_plots plots = Layout
  { _layout_plots = fmap (toPlot . uncurry candle_plot) plots'

  , _layout_x_axis = def
  , _layout_y_axis = def

  , _layout_background = solidFillStyle $ opaque white
  , _layout_plot_background = Nothing
  , _layout_title = ""
  , _layout_title_style = def
  , _layout_top_axis_visibility = def
  , _layout_bottom_axis_visibility = def
  , _layout_left_axis_visibility = def
  , _layout_right_axis_visibility = def
  , _layout_margin = 10
  , _layout_legend = Just def
  , _layout_grid_last = False
  }
  where
  convert_map = Map.fromList . fmap (first fromEnum) . Map.toList
  plots' = fmap (second convert_map) plots

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

first :: (a -> b) -> (a, x) -> (b, x)
first f (x, y) = (f x, y)

second :: (a -> b) -> (x, a) -> (x, b)
second f (x, y) = (x, f y)

-- let color_1 = withOpacity red  0.5
--     color_2 = withOpacity blue 0.5
--     data_1 = all_pairs_time_to_send (2 ^ 21) (awsMinCycle' (2 ^ 21) [minBound..maxBound])
--     data_2 = all_pairs_time_to_send (2 ^ 21) (awsMaxCycle' (2 ^ 21) [minBound..maxBound])
--     ps = candle_plots [(color_1, fmap Map.elems data_1), (color_2, fmap Map.elems data_2)]
-- renderableToFile def "file.png" (toRenderable ps)
