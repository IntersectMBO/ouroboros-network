{-# LANGUAGE ScopedTypeVariables #-}

-- | Classification of lists of symbols
--
-- Intended for qualified import.
--
-- > import qualified Ouroboros.Consensus.Util.Classify as C
module Ouroboros.Consensus.Util.Classify (
    Predicate(..)
  , predicate
  , classify
    -- * Example
  , Tag
  , example
  ) where

import           Data.Either (partitionEithers)
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set

-- | Predicate over a list of @a@s, using classification @b@
data Predicate a b = Predicate {
    -- | Given an @a@, either successfully classify as @b@ or continue looking
    predApply  :: a -> Either b (Predicate a b)

    -- | End of the string
    --
    -- The predicate is given a final chance to return a value.
  , predFinish :: Maybe b
  }

-- | Construct simply predicate that returns 'Nothing' on termination
predicate :: (a -> Either b (Predicate a b)) -> Predicate a b
predicate f = Predicate f Nothing

-- | Do a linear scan over the list, returning all successful classifications
classify :: forall a b. [Predicate a b] -> [a] -> [b]
classify = go []
  where
    go :: [b] -> [Predicate a b] -> [a] -> [b]
    go acc ps [] = acc ++ bs
      where
        bs = catMaybes $ map predFinish ps
    go acc ps (a:as) = go (acc ++ bs) ps' as
      where
        (bs, ps') = partitionEithers $ map (`predApply` a) ps

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

data Tag =
    ContainsEven
  | ContainsTwoEqual
  | LengthAtLeast Int
  deriving (Show)

example :: [Int] -> [Tag]
example = classify [
      containsEven
    , containsTwoEqual Set.empty
    , lengthAtLeast 0
    ]
  where
    containsEven :: Predicate Int Tag
    containsEven = predicate $ \n ->
        if even n
          then Left ContainsEven
          else Right containsEven

    containsTwoEqual :: Set Int -> Predicate Int Tag
    containsTwoEqual acc = predicate $ \n ->
        if n `elem` acc
          then Left ContainsTwoEqual
          else Right (containsTwoEqual (Set.insert n acc))

    lengthAtLeast :: Int -> Predicate Int Tag
    lengthAtLeast n = Predicate {
          predApply   = \_ -> Right $ lengthAtLeast (n + 1)
        , predFinish = if n `div` 10 > 0
                         then Just $ LengthAtLeast (n `div` 10 * 10)
                         else Nothing
        }
