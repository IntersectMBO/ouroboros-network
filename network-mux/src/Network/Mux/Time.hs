
module Network.Mux.Time
  ( -- * DiffTime
    DiffTime
  , diffTimeToMicroseconds
  , microsecondsToDiffTime
    -- * Compact timestamp
  , timestampMicrosecondsLow32Bits
  ) where

import           Control.Monad.Class.MonadTime (Time (..))
import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds,
                     picosecondsToDiffTime)
import           Data.Word (Word32)

diffTimeToMicroseconds :: DiffTime -> Integer
diffTimeToMicroseconds = (`div` 1000000) . diffTimeToPicoseconds

microsecondsToDiffTime :: Integer -> DiffTime
microsecondsToDiffTime = picosecondsToDiffTime . (* 1000000)

-- | This is a slightly peculiar operation: it returns the number of
-- microseconds since an arbitrary epoch, modulo 2^32. This number of
-- microseconds wraps every ~35 minutes.
--
-- The purpose is to give a compact timestamp (compact to send over the wire)
-- for measuring time differences on the order of seconds or less.
--
timestampMicrosecondsLow32Bits :: Time -> Word32
timestampMicrosecondsLow32Bits (Time ts) =
    fromIntegral (diffTimeToMicroseconds ts)

