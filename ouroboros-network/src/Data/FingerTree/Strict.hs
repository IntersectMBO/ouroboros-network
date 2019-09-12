{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Strict variants of 'FingerTree' operations.
--
-- TODO: Move this to @cardano-prelude@ as a new module, @Data.FingerTree.Strict@.
module Data.FingerTree.Strict
  ( StrictFingerTree

    -- * Construction
  , empty
  , singleton
  , (<|)
  , (|>)
  , (><)
  , fromList

    -- * Deconstruction
  , null

    -- ** Examining the ends
  , viewl
  , viewr

    -- ** Search
  , SearchResult(..)
  , search

    -- ** Splitting
    -- | These functions are special cases of 'search'.
  , split
  , takeUntil
  , dropUntil

    -- * Transformation
  , reverse

    -- ** Maps
  , fmap'

  -- Re-export from "Data.FingerTree"
  , Measured(..)
  , ViewL (..)
  , ViewR (..)
  ) where

import           Prelude hiding (null, reverse)

import           Data.FingerTree (Measured (..), ViewL (..), ViewR (..))
import qualified Data.FingerTree as FT
import           Data.Foldable (foldl')
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
import           GHC.Generics (Generic)

infixr 5 ><
infixr 5 <|
infixl 5 |>

-- | A @newtype@ wrapper around a 'FT.FingerTree', representing a finger tree
-- that is strict in its values.
--
-- This strictness is not enforced at the type level, but rather by the
-- construction functions in this module. These functions essentially just
-- wrap the original "Data.FingerTree" functions while forcing the provided
-- value to WHNF.
newtype StrictFingerTree v a = SFT (FT.FingerTree v a)
  deriving (Eq, Ord, Show)

deriving instance Foldable (StrictFingerTree v)
deriving instance (Measured v a) => Measured v (StrictFingerTree v a)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | /O(1)/. The empty sequence.
empty :: Measured v a => StrictFingerTree v a
empty = SFT (FT.empty)

-- | /O(1)/. A singleton sequence.
singleton :: Measured v a => a -> StrictFingerTree v a
singleton !a = SFT (FT.singleton a)

-- | /O(n)/. Create a sequence from a finite list of elements.
-- The opposite operation 'toList' is supplied by the 'Foldable' instance.
fromList :: (Measured v a) => [a] -> StrictFingerTree v a
fromList !xs = foldl' (|>) (SFT FT.empty) xs

-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: (Measured v a) => a -> StrictFingerTree v a -> StrictFingerTree v a
(!a) <| SFT ft = SFT (a FT.<| ft)

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) :: (Measured v a) => StrictFingerTree v a -> a -> StrictFingerTree v a
SFT ft |> (!a) = SFT (ft FT.|> a)

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><) :: (Measured v a)
     => StrictFingerTree v a -> StrictFingerTree v a -> StrictFingerTree v a
SFT ft >< SFT ft' = SFT (ft FT.>< ft')

{-------------------------------------------------------------------------------
  Deconstruction
-------------------------------------------------------------------------------}

-- | /O(1)/. Is this the empty sequence?
null :: StrictFingerTree v a -> Bool
null (SFT ft) = FT.null ft

{-------------------------------------------------------------------------------
  Examining the ends
-------------------------------------------------------------------------------}

-- | /O(1)/. Analyse the left end of a sequence.
viewl :: (Measured v a)
      => StrictFingerTree v a -> ViewL (StrictFingerTree v) a
viewl (SFT ft) = case FT.viewl ft of
  EmptyL   -> EmptyL
  a :< ft' -> a :< SFT ft'

-- | /O(1)/. Analyse the right end of a sequence.
viewr :: (Measured v a)
      => StrictFingerTree v a -> ViewR (StrictFingerTree v) a
viewr (SFT ft) = case FT.viewr ft of
  EmptyR   -> EmptyR
  ft' :> a -> SFT ft' :> a

{-------------------------------------------------------------------------------
  Search
-------------------------------------------------------------------------------}

-- | A result of 'search', attempting to find a point where a predicate
-- on splits of the sequence changes from 'False' to 'True'.
--
-- @since 0.1.2.0
data SearchResult v a
    = Position !(StrictFingerTree v a) !a !(StrictFingerTree v a)
        -- ^ A tree opened at a particular element: the prefix to the
        -- left, the element, and the suffix to the right.
    | OnLeft
        -- ^ A position to the left of the sequence, indicating that the
        -- predicate is 'True' at both ends.
    | OnRight
        -- ^ A position to the right of the sequence, indicating that the
        -- predicate is 'False' at both ends.
    | Nowhere
        -- ^ No position in the tree, returned if the predicate is 'True'
        -- at the left end and 'False' at the right end.  This will not
        -- occur if the predicate in monotonic on the tree.
    deriving (Eq, Ord, Show, Generic)

-- | Convert from an 'FT.SearchResult' (the data type from "Data.FingerTree")
-- to a 'SearchResult' (the data type from this module).
fromOriginalSearchResult :: FT.SearchResult v a -> SearchResult v a
fromOriginalSearchResult (FT.Position left a right) =
  Position (SFT left) a (SFT right)
fromOriginalSearchResult FT.OnLeft = OnLeft
fromOriginalSearchResult FT.OnRight = OnRight
fromOriginalSearchResult FT.Nowhere = Nowhere

-- | /O(log(min(i,n-i)))/. Search a sequence for a point where a predicate
-- on splits of the sequence changes from 'False' to 'True'.
--
-- The argument @p@ is a relation between the measures of the two
-- sequences that could be appended together to form the sequence @t@.
-- If the relation is 'False' at the leftmost split and 'True' at the
-- rightmost split, i.e.
--
-- @not (p 'mempty' ('measure' t)) && p ('measure' t) 'mempty'@
--
-- then there must exist an element @x@ in the sequence such that @p@
-- is 'False' for the split immediately before @x@ and 'True' for the
-- split just after it:
--
-- <<images/search.svg>>
--
-- In this situation, @'search' p t@ returns such an element @x@ and the
-- pieces @l@ and @r@ of the sequence to its left and right respectively.
-- That is, it returns @'Position' l x r@ such that
--
-- * @l >< (x <| r) = t@
--
-- * @not (p (measure l) (measure (x <| r))@
--
-- * @p (measure (l |> x)) (measure r)@
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/ on @t@.
--
-- @since 0.1.2.0
search :: (Measured v a)
       => (v -> v -> Bool) -> StrictFingerTree v a -> SearchResult v a
search p (SFT ft) = fromOriginalSearchResult (FT.search p ft)

{-------------------------------------------------------------------------------
  Splitting
-------------------------------------------------------------------------------}

-- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- on the accumulated measure of the prefix changes from 'False' to 'True'.
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/.
split :: (Measured v a)
      => (v -> Bool)
      -> StrictFingerTree v a
      -> (StrictFingerTree v a, StrictFingerTree v a)
split p (SFT ft) = (SFT left, SFT right)
  where
    (left, right) = FT.split p ft

-- | /O(log(min(i,n-i)))/.
-- Given a monotonic predicate @p@, @'takeUntil' p t@ is the largest
-- prefix of @t@ whose measure does not satisfy @p@.
--
-- *  @'takeUntil' p t = 'fst' ('split' p t)@
takeUntil :: (Measured v a)
          => (v -> Bool) -> StrictFingerTree v a -> StrictFingerTree v a
takeUntil p (SFT ft) = SFT (FT.takeUntil p ft)

-- | /O(log(min(i,n-i)))/.
-- Given a monotonic predicate @p@, @'dropUntil' p t@ is the rest of @t@
-- after removing the largest prefix whose measure does not satisfy @p@.
--
-- * @'dropUntil' p t = 'snd' ('split' p t)@
dropUntil :: (Measured v a)
          => (v -> Bool) -> StrictFingerTree v a -> StrictFingerTree v a
dropUntil p (SFT ft) = SFT (FT.dropUntil p ft)

{-------------------------------------------------------------------------------
  Transformation
-------------------------------------------------------------------------------}

-- | /O(n)/. The reverse of a sequence.
reverse :: (Measured v a) => StrictFingerTree v a -> StrictFingerTree v a
reverse (SFT ft) = SFT (FT.reverse ft)

{-------------------------------------------------------------------------------
  Maps
-------------------------------------------------------------------------------}

-- | Like 'fmap', but with constraints on the element types.
fmap' :: (Measured v1 a1, Measured v2 a2)
      => (a1 -> a2) -> StrictFingerTree v1 a1 -> StrictFingerTree v2 a2
fmap' f (SFT ft) = SFT $ toStrict (FT.fmap' f ft)
  where
    -- In order to ensure that all of the values in the 'FT.FingerTree' are
    -- strict, we can simply 'foldMap' over it and 'seq' each value with unit.
    -- However, '()''s 'mappend' implementation is actually completely lazy:
    -- @_ <> _ = ()@
    -- So, in order to work around this, we instead utilize this newly
    -- introduced 'StrictUnit' whose 'mappend' implementation is specifically
    -- strict.
    toStrict :: FT.FingerTree v a -> FT.FingerTree v a
    toStrict !ft' = foldMap (`seq` StrictUnit) ft' `seq` ft'

data StrictUnit = StrictUnit

instance Semigroup StrictUnit where
  StrictUnit <> StrictUnit = StrictUnit

instance Monoid StrictUnit where
  mempty = StrictUnit
