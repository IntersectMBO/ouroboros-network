{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Miscellaneous utilities
module Ouroboros.Consensus.Util (
    -- * Type-level utility
    Dict (..)
  , Empty
  , ShowProxy (..)
  , Some (..)
  , SomePair (..)
  , SomeSecond (..)
  , mustBeRight
    -- * Folding variations
  , foldlM'
  , nTimes
  , nTimesM
  , repeatedly
  , repeatedlyM
    -- * Lists
  , allEqual
  , chunks
  , dropLast
  , firstJust
  , groupOn
  , groupSplit
  , markLast
  , pickOne
  , splits
  , takeLast
  , takeUntil
    -- * Safe variants of existing base functions
  , lastMaybe
  , safeMaximum
  , safeMaximumBy
  , safeMaximumOn
    -- * Hashes
  , hashFromBytesE
  , hashFromBytesShortE
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
  , (.....:)
  , (....:)
  , (...:)
  , (..:)
  , (.:)
    -- * Product
  , pairFst
  , pairSnd
    -- * Miscellaneous
  , eitherToMaybe
  , fib
  ) where

import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytes,
                     hashFromBytesShort)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Foldable (asum, toList)
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Kind (Constraint, Type)
import           Data.List (foldl', maximumBy)
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           Data.Word (Word64)
import           GHC.Stack

import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

{-------------------------------------------------------------------------------
  Type-level utility
-------------------------------------------------------------------------------}

data Dict :: Constraint -> Type where
  Dict :: a => Dict a

class Empty a
instance Empty a

data Some (f :: k -> Type) where
    Some :: f a -> Some f

-- | Pair of functors instantiated to the /same/ existential
data SomePair (f :: k -> Type) (g :: k -> Type) where
    SomePair :: f a -> g a -> SomePair f g

-- | Hide the second type argument of some functor
--
-- @SomeSecond f a@ is isomorphic to @Some (f a)@, but is more convenient in
-- partial applications.
data SomeSecond (f :: Type -> Type -> Type) a where
  SomeSecond :: !(f a b) -> SomeSecond f a

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

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:zs) = x == y && allEqual (y:zs)

-- | Take items until the condition is true. If the condition is true for an
-- item, include that item as the last item in the returned list. If the
-- condition was never true, the original list is returned.
--
-- > takeUntil (== 3) [1,2,3,4]
-- [1,2,3]
-- > takeUntil (== 2) [0,1,0]
-- [0,1,0]
-- > takeUntil (== 2) [2,2,3]
-- [2]
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = \case
    []
      -> []
    x:xs
      | p x
      -> [x]
      | otherwise
      -> x:takeUntil p xs

-- | Variation on 'groupBy' that records the matched element
--
-- >    groupOn signum [-3..3]
-- > == [ (-1, [-3, -2,-1])
-- >    , ( 0, [0])
-- >    , ( 1, [1, 2, 3])
-- >    ]
groupOn :: forall a b. Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn f = groupSplit (\a -> (f a, a))

-- | Generalization of 'groupOn' where we specify both what to compare
-- and what to collect
groupSplit :: forall a b c. Eq b => (a -> (b, c)) -> [a] -> [(b, [c])]
groupSplit f = \case
    []   -> []
    a:as -> let (b, c) = f a in go b [c] as
  where
    go :: b -> [c] -> [a] -> [(b, [c])]
    go b acc []     = [(b, reverse acc)]
    go b acc (a:as)
        | b' == b   = go b (c:acc) as
        | otherwise = (b, reverse acc) : go b' [c] as
      where
        (b', c) = f a

-- | Focus on one element in the list
--
-- E.g.
--
-- >    splits [1..3]
-- > == [ ([]    , 1 , [2,3])
-- >    , ([1]   , 2 , [3]  )
-- >    , ([1,2] , 3 , []   )
-- >    ]
splits :: [a] -> [([a], a, [a])]
splits []     = []
splits (a:as) = ([], a, as) : map (\(xs, y, zs) -> (a:xs, y, zs)) (splits as)

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
  Hashes
-------------------------------------------------------------------------------}

-- | Calls 'hashFromBytes' and throws an error if the input is of the wrong
-- length.
hashFromBytesE
  :: forall h a. (HashAlgorithm h, HasCallStack)
  => Strict.ByteString
  -> Hash h a
hashFromBytesE bs = fromMaybe (error msg) $ hashFromBytes bs
  where
    msg =
      "hashFromBytes called with ByteString of the wrong length: " <>
      show bs

-- | Calls 'hashFromBytesShort' and throws an error if the input is of the
-- wrong length.
hashFromBytesShortE
  :: forall h a. (HashAlgorithm h, HasCallStack)
  => ShortByteString
  -> Hash h a
hashFromBytesShortE bs = fromMaybe (error msg) $ hashFromBytesShort bs
  where
    msg =
      "hashFromBytesShort called with ShortByteString of the wrong length: " <>
      show bs
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

(.:) :: (y -> z) -> (x0 -> x1 -> y) -> (x0 -> x1 -> z)
(f .: g) x0 x1 = f (g x0 x1)

(..:) :: (y -> z) -> (x0 -> x1 -> x2 -> y) -> (x0 -> x1 -> x2 -> z)
(f ..: g) x0 x1 x2 = f (g x0 x1 x2)

(...:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> y) -> (x0 -> x1 -> x2 -> x3 -> z)
(f ...: g) x0 x1 x2 x3 = f (g x0 x1 x2 x3)

(....:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> x4 -> y) -> (x0 -> x1 -> x2 -> x3 -> x4 -> z)
(f ....: g) x0 x1 x2 x3 x4 = f (g x0 x1 x2 x3 x4)

(.....:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> x4 -> x5 -> y) -> (x0 -> x1 -> x2 -> x3 -> x4 -> x5 -> z)
(f .....: g) x0 x1 x2 x3 x4 x5 = f (g x0 x1 x2 x3 x4 x5)

{-------------------------------------------------------------------------------
  Product
-------------------------------------------------------------------------------}

pairFst :: Product f g a -> f a
pairFst (Pair a _) = a

pairSnd :: Product f g a -> g a
pairSnd (Pair _ b) = b

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

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x
