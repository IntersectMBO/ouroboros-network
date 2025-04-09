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
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data Brightness
  = Dull
  | Bright
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data Color
  = Color
  { colorBrightness :: Brightness
  , colorBase :: Color3
  }
  deriving (Show, Read, Eq, Ord)

colorNumber :: Color -> Int
colorNumber c = colorNumber3 (colorBase c) + fromEnum (colorBrightness c) * 60

colorNumber3 :: Color3 -> Int
colorNumber3 c = fromEnum c + 30

colorEscape :: Color -> String
colorEscape c = "\ESC[" ++ show (colorNumber c) ++ "m"

colorName :: Color -> String
colorName c = show (colorBrightness c) ++ " " ++ show (colorBase c)

resetEscape :: String
resetEscape = "\ESC[0m"

colorize :: Color -> String -> String
colorize c str = colorEscape c ++ str ++ resetEscape

hcPutStr :: Handle -> Color -> String -> IO ()
hcPutStr h c str = do
  isTTY <- hIsTerminalDevice h
  if isTTY
    then
      hPutStr h (colorize c str)
    else
      hPutStr h str

hcPutStrLn :: Handle -> Color -> String -> IO ()
hcPutStrLn h c str = do
  isTTY <- hIsTerminalDevice h
  if isTTY
    then
      hPutStrLn h (colorize c str)
    else
      hPutStrLn h str

colorTest :: IO ()
colorTest =
  mapM_
    (\c -> hcPutStrLn stdout c (colorName c))
    (Color <$> [minBound .. maxBound] <*> [minBound .. maxBound])
