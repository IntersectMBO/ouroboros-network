module Test.Util.Time (
    dawnOfTime
  ) where

import           Data.Time (UTCTime (..), fromGregorian)

-- | Dawn of time
--
-- Everybody knows nothing happened before 2000-01-01 00:00:00
dawnOfTime :: UTCTime
dawnOfTime = UTCTime (fromGregorian 2000 01 01) 0
