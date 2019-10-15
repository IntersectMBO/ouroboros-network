
module Network.Mux.Time (
    -- * DiffTime
    DiffTime,
    diffTimeToMicroseconds,
    microsecondsToDiffTime,

    -- * Compact timestamp
    timestampMicrosecondsLow32Bits,
  ) where

import Data.Word (Word32)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import Control.Monad.Class.MonadTime (Time(..))

diffTimeToMicroseconds :: DiffTime -> Integer
diffTimeToMicroseconds = (`div` 1000000) . diffTimeToPicoseconds

microsecondsToDiffTime :: Integer -> DiffTime
microsecondsToDiffTime = picosecondsToDiffTime . (* 1000000)

-- | This is a slightly pecluliar operation: it returns the number of
-- microseconds since an arbitrary epoch, modulo 2^32. This number of
-- microseconds wraps every ~35 minutes.
--
-- The purpose is to give a compact timestamp (compact to send over the wire)
-- for measuring time differences on the order of seconds or less.
--
timestampMicrosecondsLow32Bits :: Time -> Word32
timestampMicrosecondsLow32Bits (Time ts) =
    fromIntegral (diffTimeToMicroseconds ts)

