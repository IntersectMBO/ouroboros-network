{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Strict variants of 'Seq' operations.
--
-- TODO: Move this to @cardano-prelude@ as a new module, @Data.Sequence.Strict@.
module Data.Sequence.Strict
  ( StrictSeq (Empty, (:<|), (:|>))

    -- * Construction
  , empty
  , singleton
  , (<|)
  , (|>)
  , (><)
  , fromList
  , toStrict

    -- * Deconstruction
    -- | Additional functions for deconstructing sequences are available
    -- via the 'Foldable' instance of 'Seq'.

    -- ** Queries
  , null
  , length

    -- * Scans
  , scanl

    -- * Sublists

    -- ** Sequential searches
  , dropWhileL
  , dropWhileR
  , spanl
  , spanr

    -- * Indexing
  , take
  , takeLast
  , drop
  , dropLast
  , splitAt
  , splitAtEnd
  ) where

import           Cardano.Prelude (NoUnexpectedThunks (..), forceElemsToWHNF,
                     noUnexpectedThunksInValues)
import           Prelude hiding (drop, length, null, scanl, splitAt, take)

import           Codec.Serialise (Serialise)
import           Data.Foldable (foldl', toList)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

infixr 5 ><
infixr 5 <|
infixl 5 |>

infixr 5 :<|
infixl 5 :|>

{-# COMPLETE Empty, (:<|) #-}
{-# COMPLETE Empty, (:|>) #-}

-- | A @newtype@ wrapper around a 'Seq', representing a general-purpose finite
-- sequence that is strict in its values.
--
-- This strictness is not enforced at the type level, but rather by the
-- construction functions in this module. These functions essentially just
-- wrap the original "Data.Sequence" functions while forcing the provided
-- value to WHNF.
newtype StrictSeq a = StrictSeq (Seq a)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Foldable, Semigroup, Serialise)

-- | Instance for 'StrictSeq' checks elements only
--
-- The internal fingertree in 'Seq' might have thunks, which is essential for
-- its asymptotic complexity.
instance NoUnexpectedThunks a => NoUnexpectedThunks (StrictSeq a) where
  showTypeOf _ = "StrictSeq"
  whnfNoUnexpectedThunks ctxt = noUnexpectedThunksInValues ctxt . toList

-- | A helper function for the ':<|' pattern.
--
viewFront :: StrictSeq a -> Maybe (a, StrictSeq a)
viewFront (StrictSeq xs) = case Seq.viewl xs of
  Seq.EmptyL   -> Nothing
  x Seq.:< xs' -> Just (x, StrictSeq xs')

-- | A helper function for the ':|>' pattern.
--
viewBack :: StrictSeq a -> Maybe (StrictSeq a, a)
viewBack (StrictSeq xs) = case Seq.viewr xs of
  Seq.EmptyR   -> Nothing
  xs' Seq.:> x -> Just (StrictSeq xs', x)

-- | A bidirectional pattern synonym matching an empty sequence.
pattern Empty :: StrictSeq a
pattern Empty = StrictSeq Seq.Empty

-- | A bidirectional pattern synonym viewing the front of a non-empty
-- sequence.
pattern (:<|) :: a -> StrictSeq a -> StrictSeq a
pattern x :<| xs <- (viewFront -> Just (x, xs))
  where
    x :<| xs = x <| xs

-- | A bidirectional pattern synonym viewing the rear of a non-empty
-- sequence.
pattern (:|>) :: StrictSeq a -> a -> StrictSeq a
pattern xs :|> x <- (viewBack -> Just (xs, x))
  where
    xs :|> x = xs |> x

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | \( O(1) \). The empty sequence.
empty :: StrictSeq a
empty = Empty

-- | \( O(1) \). A singleton sequence.
singleton :: a -> StrictSeq a
singleton !x =  StrictSeq (Seq.singleton x)

-- | \( O(1) \). Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: a -> StrictSeq a -> StrictSeq a
(!x) <| StrictSeq s = StrictSeq (x Seq.<| s)

-- | \( O(1) \). Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) :: StrictSeq a -> a -> StrictSeq a
StrictSeq s |> (!x) = StrictSeq (s Seq.|> x)

-- | \( O(\log(\min(n_1,n_2))) \). Concatenate two sequences.
(><) :: StrictSeq a -> StrictSeq a -> StrictSeq a
StrictSeq xs >< StrictSeq ys = StrictSeq (xs Seq.>< ys)

fromList :: [a] -> StrictSeq a
fromList !xs = foldl' (|>) empty xs

-- | Convert a 'Seq' into a 'StrictSeq'.
toStrict :: Seq a -> StrictSeq a
toStrict xs = StrictSeq (forceElemsToWHNF xs)

{-------------------------------------------------------------------------------
  Deconstruction
-------------------------------------------------------------------------------}

-- | \( O(1) \). Is this the empty sequence?
null :: StrictSeq a -> Bool
null (StrictSeq xs) = Seq.null xs

-- | \( O(1) \). The number of elements in the sequence.
length :: StrictSeq a -> Int
length (StrictSeq xs) = Seq.length xs

{-------------------------------------------------------------------------------
  Scans
-------------------------------------------------------------------------------}

-- | 'scanl' is similar to 'foldl', but returns a sequence of reduced
-- values from the left:
--
-- > scanl f z (fromList [x1, x2, ...]) = fromList [z, z `f` x1, (z `f` x1) `f` x2, ...]
scanl :: (a -> b -> a) -> a -> StrictSeq b -> StrictSeq a
scanl f !z0 (StrictSeq xs) = StrictSeq $ forceElemsToWHNF (Seq.scanl f z0 xs)

{-------------------------------------------------------------------------------
  Sublists
-------------------------------------------------------------------------------}

-- | \( O(i) \) where \( i \) is the prefix length.  @'dropWhileL' p xs@ returns
-- the suffix remaining after @'takeWhileL' p xs@.
dropWhileL :: (a -> Bool) -> StrictSeq a -> StrictSeq a
dropWhileL p (StrictSeq xs) = StrictSeq (Seq.dropWhileL p xs)

-- | \( O(i) \) where \( i \) is the suffix length.  @'dropWhileR' p xs@ returns
-- the prefix remaining after @'takeWhileR' p xs@.
--
-- @'dropWhileR' p xs@ is equivalent to @'reverse' ('dropWhileL' p ('reverse' xs))@.
dropWhileR :: (a -> Bool) -> StrictSeq a -> StrictSeq a
dropWhileR p (StrictSeq xs) = StrictSeq (Seq.dropWhileR p xs)

-- | \( O(i) \) where \( i \) is the prefix length.  'spanl', applied to
-- a predicate @p@ and a sequence @xs@, returns a pair whose first
-- element is the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and the second element is the remainder of the sequence.
spanl :: (a -> Bool) -> StrictSeq a -> (StrictSeq a, StrictSeq a)
spanl p (StrictSeq xs) = toStrictSeqTuple (Seq.spanl p xs)

-- | \( O(i) \) where \( i \) is the suffix length.  'spanr', applied to a
-- predicate @p@ and a sequence @xs@, returns a pair whose /first/ element
-- is the longest /suffix/ (possibly empty) of @xs@ of elements that
-- satisfy @p@ and the second element is the remainder of the sequence.
spanr :: (a -> Bool) -> StrictSeq a -> (StrictSeq a, StrictSeq a)
spanr p (StrictSeq xs) = toStrictSeqTuple (Seq.spanr p xs)

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

-- | \( O(\log(\min(i,n-i))) \). The first @i@ elements of a sequence.
-- If @i@ is negative, @'take' i s@ yields the empty sequence.
-- If the sequence contains fewer than @i@ elements, the whole sequence
-- is returned.
take :: Int -> StrictSeq a -> StrictSeq a
take i (StrictSeq xs) = StrictSeq (Seq.take i xs)

-- | Take the last @n@ elements
--
-- Returns the entire sequence if it has fewer than @n@ elements.
--
-- Inherits asymptotic complexity from @drop@.
takeLast :: Int -> StrictSeq a -> StrictSeq a
takeLast i xs
  | length xs >= i = drop (length xs - i) xs
  | otherwise      = xs

-- | \( O(\log(\min(i,n-i))) \). Elements of a sequence after the first @i@.
-- If @i@ is negative, @'drop' i s@ yields the whole sequence.
-- If the sequence contains fewer than @i@ elements, the empty sequence
-- is returned.
drop :: Int -> StrictSeq a -> StrictSeq a
drop i (StrictSeq xs) = StrictSeq (Seq.drop i xs)

-- | Drop the last @n@ elements
--
-- Returns the @Empty@ sequence if it has fewer than @n@ elements.
--
-- Inherits asymptotic complexity from @take@.
dropLast :: Int -> StrictSeq a -> StrictSeq a
dropLast i xs
  | length xs >= i = take (length xs - i) xs
  | otherwise      = Empty

-- | \( O(\log(\min(i,n-i))) \). Split a sequence at a given position.
-- @'splitAt' i s = ('take' i s, 'drop' i s)@.
splitAt :: Int -> StrictSeq a -> (StrictSeq a, StrictSeq a)
splitAt i (StrictSeq xs) = toStrictSeqTuple (Seq.splitAt i xs)

-- | Split at the given position counting from the end of the sequence.
--
-- Inherits asymptotic complexity from 'splitAt'.
splitAtEnd :: Int -> StrictSeq a -> (StrictSeq a, StrictSeq a)
splitAtEnd i xs
  | length xs >= i = splitAt (length xs - i) xs
  | otherwise      = (Empty, xs)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

toStrictSeqTuple :: (Seq a, Seq a) -> (StrictSeq a, StrictSeq a)
toStrictSeqTuple (a, b) = (StrictSeq a, StrictSeq b)
