
module Network.Mux.Time
  ( -- * DiffTime
    DiffTime
  , diffTimeToMicroseconds
  , microsecondsToDiffTime
  ) where

import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)

diffTimeToMicroseconds :: DiffTime -> Integer
diffTimeToMicroseconds = (`div` 1000000) . diffTimeToPicoseconds

microsecondsToDiffTime :: Integer -> DiffTime
microsecondsToDiffTime = picosecondsToDiffTime . (* 1000000)
