{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# OPTIONS_GHC -O2 #-}

module Main (module Main) where

import           Data.Foldable (forM_)
import qualified Data.Default

import           Data.Colour.RGBSpace.HSL (hsl)
import           Data.Colour.SRGB (RGB (..), sRGB)
import           Data.Colour (Colour)

import           Graphics.Rendering.Chart.Easy hiding (x0)
import           Graphics.Rendering.Chart.Backend.Cairo

import           Numeric.RootFinding (RiddersParam (..), Root (..), ridders)

minDelta :: Int
minDelta = 0

maxDelta :: Int 
maxDelta = 20

allDeltas :: [Int]
allDeltas = [minDelta .. maxDelta]

-----

mkCoords :: [t] -> (t -> Double) -> (t -> Double) -> [(Double, Double)]
mkCoords ts x y = map f ts
  where
    f t = (xt, yt)
      where
        !xt = x t
        !yt = y t

main :: IO ()
main = do
  let plotFile :: (Default p, ToRenderable p) => String -> EC p () -> IO ()
      plotFile = toFile def{_fo_size = (600, 600)}
  plotFile "plotG1_FSG1.png" plotG1_FSG1
  plotFile "plotG3_FSG1.png" plotG3_FSG1
  plotFile "plotG4_FS1.png" plotG4_FS1
  plotFile "plotFG1_S1.png" plotFG1_S1

-----

plotLine color s xys = do
    setColors [opaque color]
    plot $ line s [xys]

plotPoints color s xys = do
    setColors [opaque color]
    setShapes [PointShapeCross]
    plot $ points s xys

plotLinePoints color s xys = do
    plotLine color s xys
    plotPoints color "" xys

rainbowLerp :: Double -> Colour Double
rainbowLerp h =
    sRGB channelRed channelGreen channelBlue
  where
    RGB{..} = hsl (60 + 300 * h) 0.75 0.5

showMilli :: Double -> String
showMilli x
    | x > 1 = error $ show x <> " > 1"
    | x < 0 = error $ show x <> " < 0"
    | otherwise = "0." <> show (round (1000 * x) :: Int)

-----

newtype A = A{valpha :: Double}
newtype B = B{vbeta :: Double}
newtype D = D{vdelta :: Int}
newtype F = F{vf :: Double}
newtype K = K{vk :: Int}
data AD = AD{valpha :: Double, vdelta :: Int}
data AF = AF{valpha :: Double, vf :: Double}
data BF = BF{vbeta :: Double, vf :: Double}
data DF = DF{vdelta :: Int, vf :: Double}
data ABK = ABK{valpha :: Double, vbeta :: Double, vk :: Int}
data ADF = ADF{valpha :: Double, vdelta :: Int, vf :: Double}

instance Show A where show A{..} = "α=" <> showMilli valpha
instance Show B where show B{..} = "β=" <> show vbeta
instance Show D where show D{..} = "Δ=" <> show vdelta
instance Show F where show F{..} = "f=" <> showMilli vf
instance Show K where show K{..} = "k=" <> show vk
instance Show AD where show AD{..} = unwords [show A{..}, show D{..}]
instance Show AF where show AF{..} = unwords [show A{..}, show F{..}]
instance Show DF where show DF{..} = unwords [show D{..}, show F{..}]
instance Show ABK where show ABK{..} = unwords [show A{..}, show B{..}, show K{..}]
instance Show ADF where show ADF{..} = unwords [show A{..}, show D{..}, show F{..}]

commonABK :: ABK
commonABK = ABK{valpha = 1, vbeta = 1, vk = 2160}

-----

eq4 :: ADF -> Double
eq4 ADF{..} = valpha * ((1 - vf) ^ (vdelta+1))

plotG1_FSG1 = do
    let ABK{..} = commonABK

    layout_title .= "c = (1-f) ^ (Δ+1)"
    (layout_x_axis . laxis_title) .= "f"
--    (layout_y_axis . laxis_title) .= "c"
    (layout_x_axis . laxis_generate) .= scaledAxis def (0, 1)
    (layout_y_axis . laxis_generate) .= scaledAxis def (0, 1)

    let u100 = map (/100) [0 .. 100] :: [Double]
    let coords = mkCoords u100 id

    forM_ allDeltas $ \vdelta -> do
      let color = rainbowLerp $ fromIntegral vdelta / fromIntegral maxDelta
      let s = if vdelta > 2 && vdelta /= maxDelta then "" else show D{..}
      plotLine color s $ coords (\vf -> eq4 ADF{..})

    let clim = 1/(2*valpha)
        flim = 16/(40*vbeta)
    plotPoints black "G1: c ≥ 1/(2α)" $ coords (\vf -> if vf < flim then 0/0 else clim )
    plotPoints black "FSG1: f ≥ 16/(40β)" $ mkCoords u100 (\c -> if c < clim then 0/0 else flim) id

inv_eq4_half :: AD -> Double
inv_eq4_half AD{..} =
    1 - ((1/(2*valpha)) ** recip (fromIntegral (vdelta+1)))

thm1_tau :: BF -> Double
thm1_tau BF{..} = vbeta * vf / 16

plotG3_FSG1 = do
    let ABK{..} = commonABK

    layout_title .= "f upper bound induced by c ≥ 1/(2α)"
    (layout_x_axis . laxis_title) .= "Δ / (20 slots)"
    (layout_x_axis . laxis_generate) .= scaledAxis def (0, 1)
    (layout_y_axis . laxis_generate) .= scaledAxis def (0, 0.5)

    let coords = mkCoords allDeltas ((/fromIntegral maxDelta) . fromIntegral)

    plotLinePoints red "G3: f ≤ ·" $ coords $ \vdelta -> inv_eq4_half AD{..}

    plotPoints black "FSG1: f ≥ 16/(40β)" $ coords (\_ -> 16/(40*vbeta))

-----

plotG4_FS1 = do
    let ABK{..} = commonABK

    layout_title .= "τ upper bound induced by c ≥ 1/(2α)"
    (layout_x_axis . laxis_title) .= "Δ / (20 slots)"
    (layout_x_axis . laxis_generate) .= scaledAxis def (0, 1)
    let maxy = 0.5 / (16 / vbeta)
    (layout_y_axis . laxis_generate) .= scaledAxis def (0, maxy)

    let coords = mkCoords allDeltas ((/fromIntegral maxDelta) . fromIntegral)
            

    plotLinePoints red "G4: τ ≤ ·" $ coords $ \vdelta ->
      let vf = inv_eq4_half AD{..} in thm1_tau BF{..}

    plotPoints black "FS1: τ ≥ 1/40" $ coords (\_ -> 1/40)

-----

plotFG1_S1 = do
    let ABK{..} = commonABK

    layout_title .= "Smallest possible window"
    (layout_x_axis . laxis_title) .= "Δ (slots)"
    (layout_y_axis . laxis_title) .= ("days, assuming " <> show K{..})

    let coords = mkCoords [minDelta..100] fromIntegral

    plotLine red "FG1: s ≥ ·" $ coords $ \vdelta ->
      let vf = inv_eq4_half AD{..}
          tau = thm1_tau BF{..}
          lim_slots = fromIntegral vk / tau
          slotsPerDay = 1 * 60 * 60 * 24
      in lim_slots / slotsPerDay

    plotPoints black "S1: s = 1 day" $ coords (\_ -> 1)
