module Test.Util.Time (
  ioSimSecondsToDiffTime,
  ) where

import           Data.Time (DiffTime)

-- | Map a number of seconds to a 'DiffTime' planned for use in an @io-sim@
-- simulation context
--
-- The @io-sim@ layer can skip ahead through time, so this function tends to
-- have arguments with extreme magnitude.
--
ioSimSecondsToDiffTime :: Integer -> DiffTime
ioSimSecondsToDiffTime = fromInteger
