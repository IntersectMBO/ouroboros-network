{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Miscellaneous utilities
module Ouroboros.Consensus.Util (
    -- * Type-level utility
    Dict(..)
  , Empty
  , Some(..)
  , SomePair(..)
  , mustBeRight
    -- * Folding variations
  , foldlM'
  , repeatedly
  , repeatedlyM
  , nTimes
  , nTimesM
    -- * Lists
  , chunks
  , pickOne
  , markLast
  , takeLast
  , dropLast
  , firstJust
    -- * Safe variants of existing base functions
  , lastMaybe
  , safeMaximum
  , safeMaximumBy
  , safeMaximumOn
    -- * Bytestrings
  , byteStringChunks
  , lazyByteStringChunks
    -- * Monadic utilities
  , whenJust
    -- * Test code
  , checkThat
    -- * Sets
  , allDisjoint
    -- * Composition
  , (.:)
  , (..:)
  , (...:)
    -- * Miscellaneous
  , fib
  ) where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Foldable (asum, toList)
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Kind
import           Data.List (foldl', maximumBy)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           Data.Word (Word64)
import           GHC.Stack

{-------------------------------------------------------------------------------
  Type-level utility
-------------------------------------------------------------------------------}

data Dict :: Constraint -> * where
  Dict :: a => Dict a

class Empty a
instance Empty a

data Some (f :: k -> *) where
    Some :: f a -> Some f

-- | Pair of functors instantiated to the /same/ existential
data SomePair (f :: k -> *) (g :: k -> *) where
    SomePair :: f a -> g a -> SomePair f g

mustBeRight :: Either Void a -> a
mustBeRight (Left  v) = absurd v
mustBeRight (Right a) = a

{-------------------------------------------------------------------------------
  Folding variations
-------------------------------------------------------------------------------}

foldlM' :: forall m a b. Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM' f = go
  where
    go :: b -> [a] -> m b
    go !acc []     = return acc
    go !acc (x:xs) = f acc x >>= \acc' -> go acc' xs

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . foldl' . flip

repeatedlyM :: Monad m => (a -> b -> m b) -> ([a] -> b -> m b)
repeatedlyM = flip . foldlM' . flip

-- | Apply a function n times. The value of each application is forced.
nTimes :: forall a. (a -> a) -> Word64 -> (a -> a)
nTimes f n = runIdentity . nTimesM (Identity . f) n

-- | Apply a function n times through a monadic bind. The value of each
-- application is forced.
nTimesM :: forall m a. Monad m => (a -> m a) -> Word64 -> (a -> m a)
nTimesM f = go
  where
    go :: Word64 -> (a -> m a)
    go 0 !x = return x
    go n !x = go (n - 1) =<< f x

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (chunk, xs') = splitAt n xs
              in chunk : chunks n xs'

-- | All possible ways to pick on element from a list, preserving order
--
-- > pickOne [1,2,3] = [ ([], 1, [2, 3])
-- >                   , ([1], 2, [3])
-- >                   , ([1,2], 3, [])
-- >                   ]
pickOne :: [a] -> [([a], a, [a])]
pickOne []     = []
pickOne (x:xs) = ([], x, xs)
               : map (\(as, b, cs) -> (x:as, b, cs)) (pickOne xs)

-- | Mark the last element of the list as 'Right'
markLast :: [a] -> [Either a a]
markLast = go
  where
    go []     = []
    go [x]    = [Right x]
    go (x:xs) = Left x : go xs

-- | Take the last @n@ elements
takeLast :: Word64 -> [a] -> [a]
takeLast n = reverse . take (fromIntegral n) . reverse

-- | Drop the last @n@ elements
dropLast :: Word64 -> [a] -> [a]
dropLast n = reverse . drop (fromIntegral n) . reverse

firstJust :: forall a b f. Foldable f => (a -> Maybe b) -> f a -> Maybe b
firstJust f = asum . fmap f . toList

{-------------------------------------------------------------------------------
  Safe variants of existing base functions
-------------------------------------------------------------------------------}

lastMaybe :: [a] -> Maybe a
lastMaybe []     = Nothing
lastMaybe [x]    = Just x
lastMaybe (_:xs) = lastMaybe xs

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum = safeMaximumBy compare

safeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMaximumBy _cmp [] = Nothing
safeMaximumBy cmp ls  = Just $ maximumBy cmp ls

safeMaximumOn :: Ord b => (a -> b) -> [a] -> Maybe a
safeMaximumOn f = safeMaximumBy (compare `on` f)

{-------------------------------------------------------------------------------
  Bytestrings
-------------------------------------------------------------------------------}

byteStringChunks :: Int -> Strict.ByteString -> [Strict.ByteString]
byteStringChunks n = map Strict.pack . chunks n . Strict.unpack

lazyByteStringChunks :: Int -> Lazy.ByteString -> [Lazy.ByteString]
lazyByteStringChunks n bs
  | Lazy.null bs = []
  | otherwise    = let (chunk, bs') = Lazy.splitAt (fromIntegral n) bs
                   in chunk : lazyByteStringChunks n bs'

{-------------------------------------------------------------------------------
  Monadic utilities
-------------------------------------------------------------------------------}

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pure ()

{-------------------------------------------------------------------------------
  Test code
-------------------------------------------------------------------------------}

-- | Assertion
--
-- Variation on 'assert' for use in testing code.
checkThat :: (Show a, Monad m)
          => String
          -> (a -> Bool)
          -> a
          -> m ()
checkThat label prd a
  | prd a     = return ()
  | otherwise = error $ label ++ " failed on " ++ show a ++ "\n"
                     ++ prettyCallStack callStack

{-------------------------------------------------------------------------------
  Sets
-------------------------------------------------------------------------------}

-- | Check that a bunch of sets are all mutually disjoint
allDisjoint :: forall a. Ord a => [Set a] -> Bool
allDisjoint = go Set.empty
  where
    go :: Set a -> [Set a] -> Bool
    go _   []       = True
    go acc (xs:xss) = Set.disjoint acc xs && go (Set.union acc xs) xss

{-------------------------------------------------------------------------------
  Composition
-------------------------------------------------------------------------------}

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) a b = f (g a b)

(..:) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(f ..: g) a b c = f (g a b c)

(...:) :: (e -> f) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f)
(f ...: g) a b c d = f (g a b c d)

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Fast Fibonacci computation, using Binet's formula
fib :: Word64 -> Word64
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5, phi :: Double
    sq5 = sqrt 5
    phi = (1 + sq5) / 2
