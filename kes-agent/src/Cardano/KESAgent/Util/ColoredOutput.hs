module Cardano.KESAgent.Util.ColoredOutput
where

import System.IO

data Color3
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | DefaultColor
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data Brightness
  = Dull
  | Bright
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data FontWeight
  = Regular
  | Bold
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data Color
  = Color
  { colorBrightness :: Brightness
  , colorWeight :: FontWeight
  , colorBase :: Color3
  }
  deriving (Show, Read, Eq, Ord)

defaultColor :: Color
defaultColor = Color Dull Regular DefaultColor

black :: Color
black = Color Dull Regular Black

red :: Color
red = Color Dull Regular Red

green :: Color
green = Color Dull Regular Green

yellow :: Color
yellow = Color Dull Regular Yellow

blue :: Color
blue = Color Dull Regular Blue

magenta :: Color
magenta = Color Dull Regular Magenta

cyan :: Color
cyan = Color Dull Regular Cyan

white :: Color
white = Color Dull Regular White

bright :: Color -> Color
bright c = c {colorBrightness = Bright}

bold :: Color -> Color
bold c = c {colorWeight = Bold}

colorNumber :: Color -> Int
colorNumber c = colorNumber3 (colorBase c) + fromEnum (colorBrightness c) * 60

colorNumber3 :: Color3 -> Int
colorNumber3 c = fromEnum c + 30

colorEscape :: Color -> String
colorEscape c =
  colorEscapeBase c ++ weightEscape (colorWeight c)

colorEscapeBase :: Color -> String
colorEscapeBase Color {colorBase = DefaultColor} = resetEscape
colorEscapeBase c = "\ESC[" ++ show (colorNumber c) ++ "m"

weightEscape :: FontWeight -> String
weightEscape Regular = ""
weightEscape Bold = "\ESC[0m"

colorName :: Color -> String
colorName c = show (colorBrightness c) ++ " " ++ show (colorBase c)

resetEscape :: String
resetEscape = "\ESC[0m"

colorize :: Color -> String -> String
colorize c str = colorEscape c ++ str ++ resetEscape

data ColorMode
  = ColorsNever
  | ColorsAuto
  | ColorsAlways
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

colorsActive :: ColorMode -> Handle -> IO Bool
colorsActive ColorsNever _ = return False
colorsActive ColorsAlways _ = return True
colorsActive ColorsAuto h = hIsTerminalDevice h

applyColors :: ColorMode -> Handle -> Color -> String -> IO String
applyColors mode h c str = do
  active <- colorsActive mode h
  if active
    then
      return (colorize c str)
    else
      return str

hcPutStr :: ColorMode -> Handle -> Color -> String -> IO ()
hcPutStr mode h c str = hPutStr h =<< applyColors mode h c str

hcPutStrLn :: ColorMode -> Handle -> Color -> String -> IO ()
hcPutStrLn mode h c str = hPutStrLn h =<< applyColors mode h c str

colorTest :: IO ()
colorTest =
  mapM_
    (\c -> hcPutStrLn ColorsAlways stdout c (colorName c))
    (Color <$> [minBound .. maxBound] <*> [minBound .. maxBound] <*> [minBound .. maxBound])
