module Test.Data.PipeliningDepth (PipeliningDepth (..)) where

import Test.QuickCheck

newtype PipeliningDepth = PipeliningDepth Int
  deriving Show

instance Arbitrary  PipeliningDepth where
    -- chain-sync is pipelining at most 300 messages,
    -- block-fetch is pipelining at most 100 messages (see
    -- 'defaultMiniProtocolParameters').
    arbitrary = PipeliningDepth <$> choose (0,300)
    shrink (PipeliningDepth a) = PipeliningDepth <$> filter (>= 0) (shrink a)
