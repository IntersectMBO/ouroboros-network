-- | Utility functions for defining @QuickCheck@'s 'Test.QuickCheck.shrink'
--
module Test.Util.Shrink (
    andId
  , dropId
  ) where

{-------------------------------------------------------------------------------
  Shrinking Helpers
-------------------------------------------------------------------------------}

-- | Drop the last element
--
-- If every source list in a comprehension uses 'andId', then the last element
-- will be a copy of the initial input.
--
dropId :: [a] -> [a]
dropId = init

-- | Add the argument as the last element of the output
--
andId :: (a -> [a]) -> a -> [a]
andId f x = f x ++ [x]
