{-# LANGUAGE BangPatterns #-}
module Test.Util.Time (dawnOfTime) where

import           Data.Time (UTCTime (..), fromGregorian)

-- | Dawn of time
--
-- Everybody knows nothing happened before 2000-01-01 00:00:00
dawnOfTime :: UTCTime
dawnOfTime = UTCTime day 0
  where
    -- Force it to avoid a thunk in 'UTCTime', which doesn't have bangs on its
    -- arguments. The thunk itself would be harmless, as it would be forced the
    -- first time it's accessed, but it causes the 'NoThunks' check to fail.
    !day = fromGregorian 2000 01 01
