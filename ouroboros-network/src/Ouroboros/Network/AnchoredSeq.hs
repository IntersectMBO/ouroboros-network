{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
module Ouroboros.Network.AnchoredSeq
  ( -- * 'AnchoredSeq' type
    AnchoredSeq (Empty, (:>), (:<))
    -- * 'Anchorable'
  , Anchorable (..)
    -- * Basic operations
  , anchor
  , head
  , headAnchor
  , last
  , toNewestFirst
  , toOldestFirst
  , fromNewestFirst
  , fromOldestFirst
  , splitAt
  , dropNewest
  , takeOldest
  , dropWhileNewest
  , takeWhileOldest
  , length
  , null
  , contains
  , withinBounds
  , map
  , bimap
  , mapPreservingMeasure
  , bimapPreservingMeasure
    -- * Special operations
  , rollback
  , isPrefixOf
  , isPrefixOfByMeasure
  , lookupByMeasure
  , splitAfterMeasure
  , splitBeforeMeasure
  , join
  , anchorNewest
  , selectOffsets
  , filter
  , filterWithStop
    -- * Helper functions
  , prettyPrint
    -- * Reference implementations for testing
  , filterWithStopSpec
  ) where

import           Prelude hiding (filter, head, last, length, map, null, splitAt)

import           Data.Coerce (coerce)
import           Data.FingerTree.Strict (Measured (measure), StrictFingerTree)
import qualified Data.FingerTree.Strict as FT
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

{-------------------------------------------------------------------------------
  AnchoredSeq
-------------------------------------------------------------------------------}

-- | Generalisation of a @Sequence@ with elements of type @b@ with a custom
-- measure @v@ and an anchor @a@.
--
-- This type is strict in the elements, but not strict in the spine.
--
-- For example, an 'AnchoredSeq' can represent a fragment of a chain containing
-- blocks that is anchored at a certain point. It can also represent a history
-- of ledger states with the anchor being the \"immutable\" ledger state.
--
-- NOTE: there might be multiple elements with the same measure, e.g., multiple
-- blocks with the same @WithOrigin SlotNo@. That is why functions operating on
-- an 'AnchoredSeq' often take a predicate in addition to a measure. At most one
-- element should satisfy that predicate, e.g., the block must have a certain
-- hash. The behaviour is undefined when multiple elements satisfy the
-- predicate.
data AnchoredSeq v a b = AnchoredSeq {
      anchor      :: !a
    , unanchorSeq :: !(StrictFingerTree (Measure v) (MeasuredWith v a b))
    }
  deriving (Show, Eq, Generic, NoThunks)

-- | Constaint needed to use an @AnchoredSeq@.
class (Ord v, Bounded v) => Anchorable v a b | a -> v where
  -- | @b@ as anchor
  asAnchor :: b -> a

  -- | Return the measure of an anchor
  --
  -- The advantage of this method over a @'Measured' k a@ super-class constraint
  -- is that it doesn't inherit the @'Monoid' k@ constraint, which is unused and
  -- often undesired.
  getAnchorMeasure :: Proxy b -> a -> v

-- | Helper for getting the measure of a @v@.
getElementMeasure :: forall v a b. Anchorable v a b => MeasuredWith v a b -> v
getElementMeasure (MeasuredWith x) =
    getAnchorMeasure @v @a (Proxy @b) $ asAnchor x

{-------------------------------------------------------------------------------
  Internals
-------------------------------------------------------------------------------}

-- | Internal: measure used by the fingertree in 'AnchoredSeq'.
--
-- @v@ is a custom measure. E.g., for blocks this could be the slot number. This
-- custom measure is augmented by a measure based on the size, used to
-- efficiently look up things by their position.
data Measure v = Measure {
       measureMin  :: !v
     , measureMax  :: !v
     , measureSize :: !Int
     }
  deriving (Show)

-- | Internal: newtype so that we don't impose a 'Measured' constraint on @b@,
-- which would require each instantiation to provide a 'Measured' instance.
-- Also, this fixes the functional dependency of 'Measured' internally, it
-- doesn't leak to all @b@s.
newtype MeasuredWith v a b = MeasuredWith {
      unMeasuredWith :: b
    }
  deriving (Show, Eq, Generic, NoThunks)

instance Anchorable v a b => Measured (Measure v) (MeasuredWith v a b) where
  measure x = Measure {
        measureMin  = m
      , measureMax  = m
      , measureSize = 1
      }
    where
      m :: v
      m = getElementMeasure x

instance Ord v => Semigroup (Measure v) where
  v1 <> v2 = Measure {
      measureMin  = measureMin  v1 `min` measureMin  v2
    , measureMax  = measureMax  v1 `max` measureMax  v2
    , measureSize = measureSize v1   +   measureSize v2
    }

instance (Ord v, Bounded v) => Monoid (Measure v) where
  mempty  = Measure maxBound minBound 0
  mappend = (<>)

{-------------------------------------------------------------------------------
  Pattern synonyms
-------------------------------------------------------------------------------}

-- | \( O(1) \). Pattern for matching on or creating an empty 'AnchoredSeq'. An
-- empty sequence has/needs an anchor.
pattern Empty :: Anchorable v a b => a -> AnchoredSeq v a b
pattern Empty a <- (viewRight -> EmptyR a)
  where
    Empty a = AnchoredSeq a FT.empty

-- | Auxiliary data type to define the pattern synonym
data ViewRight v a b
    = EmptyR a
    | ConsR  (AnchoredSeq v a b) b

viewRight :: Anchorable v a b => AnchoredSeq v a b -> ViewRight v a b
viewRight (AnchoredSeq a ft) = case FT.viewr ft of
    FT.EmptyR                -> EmptyR a
    ft' FT.:> MeasuredWith b -> ConsR (AnchoredSeq a ft') b

-- | \( O(1) \). Add an element to the right of the anchored sequence.
pattern (:>) :: Anchorable v a b => AnchoredSeq v a b -> b -> AnchoredSeq v a b
pattern s' :> b <- (viewRight -> ConsR s' b)
  where
    AnchoredSeq a ft :> b = AnchoredSeq a (ft FT.|> MeasuredWith b)

-- | Auxiliary data type to define the pattern synonym
data ViewLeft v a b
    = EmptyL a
    | ConsL b (AnchoredSeq v a b)

viewLeft ::
     forall v a b. Anchorable v a b
  => AnchoredSeq v a b
  -> ViewLeft v a b
viewLeft (AnchoredSeq a ft) = case FT.viewl ft of
    FT.EmptyL ->
      EmptyL a
    MeasuredWith b FT.:< ft' ->
      ConsL b (AnchoredSeq (asAnchor b) ft')

-- | \( O(1) \). View the first, leftmost block of the anchored sequence.
--
-- Note that the anchor shifts, i.e., the anchor of the second argument will
-- correspond to the first argument.
--
-- This is only a view, not a constructor, as adding a block to the left would
-- change the anchor of the sequence, but we have no information about the
-- predecessor of the block we'd be prepending.
pattern (:<) :: Anchorable v a b => b -> AnchoredSeq v a b -> AnchoredSeq v a b
pattern b :< s' <- (viewLeft -> ConsL b s')

infixl 5 :>, :<

{-# COMPLETE Empty, (:>) #-}
{-# COMPLETE Empty, (:<) #-}

{-------------------------------------------------------------------------------
  Operations
-------------------------------------------------------------------------------}

prettyPrint ::
     String
  -> (a -> String)
  -> (b -> String)
  -> AnchoredSeq v a b
  -> String
prettyPrint nl ppA ppB (AnchoredSeq a ft) =
    Foldable.foldl'
      (\s (MeasuredWith b) -> s ++ nl ++ "    " ++ ppB b)
      ("AnchoredSeq (" <> ppA a <> "):")
      ft

-- | \( O(1) \). Return the measure of the anchor.
anchorMeasure :: forall v a b. Anchorable v a b => AnchoredSeq v a b -> v
anchorMeasure = getAnchorMeasure (Proxy @b) . anchor

-- | \( O(1) \). When the sequence is empty, return the anchor,
-- otherwise the most recently added element.
head :: Anchorable v a b => AnchoredSeq v a b -> Either a b
head (_ :>  b) = Right b
head (Empty a) = Left a

-- | \( O(1) \). The anchor corresponding to the most recently added element
-- (i.e., the anchor that would be needed for a sequence starting /after/ this).
-- When the anchored sequence is empty, the anchor is returned.
headAnchor :: forall v a b. Anchorable v a b => AnchoredSeq v a b -> a
headAnchor = either id asAnchor . head

-- | \( O(1) \). When the sequence is empty, return the anchor, otherwise the
-- leftmost element.
last :: Anchorable v a b => AnchoredSeq v a b -> Either a b
last (b :< _)  = Right b
last (Empty a) = Left a

-- | \( O(n) \). Return the elements in the 'AnchoredSeq' in newest-to-oldest
-- order.
toNewestFirst :: AnchoredSeq v a b -> [b]
toNewestFirst = coerce . Foldable.foldl' (flip (:)) [] . unanchorSeq

-- | \( O(n) \). Return the elements in the 'AnchoredSeq' in oldest-to-newest
-- order.
toOldestFirst :: AnchoredSeq v a b -> [b]
toOldestFirst = coerce . Foldable.toList . unanchorSeq

-- | \( O(n) \). Make an 'AnchoredSeq' from a list of elements in
-- newest-to-oldest order. The last element in the list will be the one after
-- the given anchor.
fromNewestFirst :: Anchorable v a b => a -> [b] -> AnchoredSeq v a b
fromNewestFirst a = foldr (flip (:>)) (Empty a)

-- | \( O(n) \). Make an 'AnchoredSeq' from a list of elements in
-- oldest-to-newest order. The first element in the list will be the one after
-- the given anchor.
fromOldestFirst :: Anchorable v a b => a -> [b] -> AnchoredSeq v a b
fromOldestFirst a = AnchoredSeq a . FT.fromList . coerce

-- | \( O(\log(\min(i,n-i)) \). Split the 'AnchoredSeq' at a given
--  position.
--
-- POSTCONDITION: @(before, after) = splitAt i s@, then:
-- * @anchor      before == anchor s@
-- * @headAnchor  before == anchor after@
-- * @headAnchor  after  == headAnchor s@
-- * @join before after  == Just s@
splitAt ::
      Anchorable v a b
   => Int
   -> AnchoredSeq v a b
   -> (AnchoredSeq v a b, AnchoredSeq v a b)
splitAt i (AnchoredSeq a ft) = case FT.split (\v -> measureSize v > i) ft of
   (before, after) ->
     let before' = AnchoredSeq a before
     in (before', AnchoredSeq (headAnchor before') after)

-- | \( O(\log(\min(i,n-i)) \). Drop the newest @n@ elements from the
-- 'AnchoredSeq'. The anchor does not change.
dropNewest :: Anchorable v a b => Int -> AnchoredSeq v a b -> AnchoredSeq v a b
dropNewest n s@(AnchoredSeq a ft)
    | n == 0
    = s
    | otherwise
    = AnchoredSeq a $ FT.takeUntil (\v -> measureSize v > remainingLength) ft
  where
    remainingLength = length s - n

-- | \( O(\log(\min(i,n-i)) \). Take the oldest @n@ elements from the
-- 'AnchoredSeq'. The anchor does not change.
takeOldest :: Anchorable v a b => Int -> AnchoredSeq v a b -> AnchoredSeq v a b
takeOldest n s@(AnchoredSeq a ft)
    | n == length s
    = s
    | otherwise
    = AnchoredSeq a $ FT.takeUntil (\v -> measureSize v > n) ft

-- | \( O(n) \). Drop the newest elements that satisfy the predicate, keeping
-- the remainder. The anchor does not change.
dropWhileNewest ::
     Anchorable v a b
  => (b -> Bool)
  -> AnchoredSeq v a b
  -> AnchoredSeq v a b
dropWhileNewest _ (Empty a) = Empty a
dropWhileNewest p s@(s' :> b)
    | p b       = dropWhileNewest p s'
    | otherwise = s

-- | \( O(n) \). Take the oldest elements that satisfy the predicate. The anchor
-- does not change.
takeWhileOldest ::
     Anchorable v a b
  => (b -> Bool)
  -> AnchoredSeq v a b
  -> AnchoredSeq v a b
takeWhileOldest p = \(AnchoredSeq a ft) -> AnchoredSeq a (go ft)
  where
    go ft = case FT.viewl ft of
        FT.EmptyL
          -> FT.empty
        MeasuredWith b FT.:< ft'
          | p b
          -> MeasuredWith b FT.<| go ft'
          | otherwise
          -> FT.empty

-- | \( O(1) \). Return the number of elements. The anchor is not counted.
length :: Anchorable v a b => AnchoredSeq v a b -> Int
length = measureSize . FT.measure . unanchorSeq

-- | \( O(1) \). The anchor is not counted.
null :: AnchoredSeq v a b -> Bool
null = FT.null . unanchorSeq

-- | \( O(\log(\min(i,n-i)) \). Roll back the anchored sequence such that its
-- new head has the same measure as the given one and satisfies the predicate.
-- When there is no such element or anchor, return 'Nothing'.
rollback ::
     Anchorable v a b
  => v
  -> (Either a b -> Bool)
  -> AnchoredSeq v a b
  -> Maybe (AnchoredSeq v a b)
rollback k p s
    | anchorMeasure s == k, p (Left (anchor s))
    = Just (Empty (anchor s))
    | otherwise
    = fst <$> splitAfterMeasure k p s

-- | \( O(\log(\min(i,n-i)) \). Internal variant of 'lookupByMeasure' that
-- returns a 'FT.SearchResult'.
lookupByMeasureFT ::
     Anchorable v a b
  => v
  -> AnchoredSeq v a b
  -> FT.SearchResult (Measure v) (MeasuredWith v a b)
lookupByMeasureFT k (AnchoredSeq _ ft) =
    FT.search (\ml mr -> measureMax ml >= k && measureMin mr > k) ft

-- | \( O(\log(\min(i,n-i) + s) \) where /s/ is the number of elements with the
-- same measure. Return all elements in the anchored sequence with a measure
-- (@k@) equal to the given one. The elements will be ordered from oldest to
-- newest. Does not look at the anchor.
lookupByMeasure ::
     Anchorable v a b
  => v
  -> AnchoredSeq v a b
  -> [b]
lookupByMeasure k s = case lookupByMeasureFT k s of
    FT.Position before b _after
      | getElementMeasure b == k
        -- We have found the rightmost element with the given measure, we still
        -- have to look at the elemens before it with the same measure.
      -> elementsBefore before [unMeasuredWith b]
    _ -> []
  where
    -- Look to the left of the element we found for more elements with the same
    -- measure.
    elementsBefore before acc = case FT.viewr before of
      before' FT.:> b
        | getElementMeasure b == k
        -> elementsBefore before' (unMeasuredWith b:acc)
           -- Note that we're prepending an older element each time, so the
           -- final list of elements will be ordered from oldest to newest. No
           -- need to reverse the accumulator.
      _ -> acc

-- | \( O(\log(\min(i,n-i)) \). Does the anchored sequence contain an element
-- with the given measure that satisfies the predicate? The anchor is ignored.
contains :: Anchorable v a b => v -> (b -> Bool) -> AnchoredSeq v a b -> Bool
contains k p s = any p $ lookupByMeasure k s

-- | \( O(\log(\min(i,n-i)) \). Does the anchored sequence contain an element
-- with the given measure that satisfies the predicate? The anchor is /not/
-- ignored.
withinBounds ::
     Anchorable v a b
  => v
  -> (Either a b -> Bool)
  -> AnchoredSeq v a b
  -> Bool
withinBounds k p s =
       (k == anchorMeasure s && p (Left (anchor s)))
    || contains k (p . Right) s

-- | \( O(n) \). Maps over the elements and the elements.
map ::
     Anchorable v2 a b2
  => (b1 -> b2)
  -> AnchoredSeq v1 a b1
  -> AnchoredSeq v2 a b2
map = bimap id

-- | \( O(n) \). Maps over the elements.
bimap ::
     Anchorable v2 a2 b2
  => (a1 -> a2)
  -> (b1 -> b2)
  -> AnchoredSeq v1 a1 b1
  -> AnchoredSeq v2 a2 b2
bimap f g s =
    -- 'FT.fmap'' has an unnecessary @Measured v1 a1@ constraint. We don't want
    -- an @Anchorable v1 a1 b1@ constraint here, so we go through an
    -- intermediary list instead. This has the same time complexity and the lazy
    -- traversal and mapping should give the same space complexity.
    fromOldestFirst (f (anchor s)) (g <$> toOldestFirst s)

-- | \( O(n) \). Maps over the elements.
--
-- NOTE: the functions must preserve the measure.
--
-- More efficient than 'map'
mapPreservingMeasure ::
     (b1 -> b2)
  -> AnchoredSeq v a b1
  -> AnchoredSeq v a b2
mapPreservingMeasure = bimapPreservingMeasure id

-- | \( O(n) \). Maps over the anchor and the elements.
--
-- NOTE: the functions must preserve the measure.
--
-- More efficient than 'bimap'
bimapPreservingMeasure ::
     (a1 -> a2)
  -> (b1 -> b2)
  -> AnchoredSeq v a1 b1
  -> AnchoredSeq v a2 b2
bimapPreservingMeasure f g (AnchoredSeq a ft) =
    AnchoredSeq (f a) (FT.unsafeFmap (coerce g) ft)

-- | Take the @n@ newest elements from the anchored sequence.
--
-- WARNING: this may change the anchor
--
-- When the anchored sequence contains fewer than @n@ elements, the anchored
-- sequence will be returned unmodified.
anchorNewest ::
     forall v a b. Anchorable v a b
  => Word64  -- ^ @n@
  -> AnchoredSeq v a b
  -> AnchoredSeq v a b
anchorNewest n c
    | toDrop <= 0
    = c
    | toDrop < 5
      -- Hybrid approach: microbenchmarks have shown that a linear drop is
      -- faster when the number of elements is small. For a larger number of
      -- elements, the asymptotic complexity of 'splitAt' wins.
    = linearDrop toDrop c
    | otherwise
    = snd $ splitAt toDrop c
  where
    len, toDrop :: Int
    len    = length c
    toDrop = len - fromIntegral n

    linearDrop :: Int -> AnchoredSeq v a b -> AnchoredSeq v a b
    linearDrop !_ (Empty a) = Empty a
    linearDrop !0 c'        = c'
    linearDrop !m (_ :< c') = linearDrop (m - 1) c'

-- | \( O(\max(n_1, n_2)) \). Check whether the first anchored sequence is a
-- prefix of the second. Comparisons are done based on the 'Eq' instances.
--
-- The two 'AnchoredSeq's must have the same anchor, otherwise the first cannot
-- be a prefix of the second.
isPrefixOf ::
     forall v a b. (Eq a, Eq b)
  => AnchoredSeq v a b
  -> AnchoredSeq v a b
  -> Bool
s1 `isPrefixOf` s2 =
       anchor s1 == anchor s2
    && toElements s1 `L.isPrefixOf` toElements s2
  where
    toElements :: AnchoredSeq v a b -> [b]
    toElements = L.map unMeasuredWith . Foldable.toList . unanchorSeq

-- | \( O(\max(n_1, n_2)) \). Check whether the first anchored sequence is a
-- prefix of the second. Comparisons are done based on the measure.
--
-- The two 'AnchoredSeq's must have the same anchor, otherwise the first cannot
-- be a prefix of the second.
isPrefixOfByMeasure ::
     forall v a b. Anchorable v a b
  => AnchoredSeq v a b
  -> AnchoredSeq v a b
  -> Bool
s1 `isPrefixOfByMeasure` s2 =
       anchorMeasure s1 == anchorMeasure s2
    && toMeasures s1 `L.isPrefixOf` toMeasures s2
  where
    toMeasures :: AnchoredSeq v a b -> [v]
    toMeasures = L.map getElementMeasure . Foldable.toList . unanchorSeq

-- | \( O(\log(\min(i,n-i)) \). Split the 'AnchoredSeq' after an element or
-- anchor with the given measure that satisfies the predicate. Return 'Nothing'
-- if there is no element or anchor with the given measure that satisfies the
-- predicate.
--
-- If the given measure corresponds to the anchor and it satisfies the
-- predicate, an empty sequence with the given anchor, and the original sequence
-- are returned.
--
-- PRECONDITION: there can be multiple elements with the same measure, but there
-- should be at most one element (or anchor) with the given measure satisfying
-- the predicate.
--
-- POSTCONDITION: when @Just (before, after) = splitAfterMeasure k f s@, then:
-- * @anchor before       == anchor s@
-- * @headMeasure before  == pt@
-- * @anchorMeasure after == pt@
-- * @headAnchor after    == headAnchor s@
-- * @join before after   == Just s@
splitAfterMeasure ::
     Anchorable v a b
  => v
  -> (Either a b -> Bool)
  -> AnchoredSeq v a b
  -> Maybe (AnchoredSeq v a b, AnchoredSeq v a b)
splitAfterMeasure k p s@(AnchoredSeq a ft)
    | anchorMeasure s == k, p (Left a)
    = Just (Empty a, s)
    | (l, r) <- FT.split (\v -> measureMax v > k) ft
      -- @l@ contains elements with a measure <= the given mesaure. There could
      -- be multiple with the given measure, so try them one by one.
    = go l r
    | otherwise
    = Nothing
  where
    go l r = case FT.viewr l of
      l' FT.:> m@(MeasuredWith b)
        | getElementMeasure m == k, p (Right b)
        , let al = AnchoredSeq a l
        -> Just (al, AnchoredSeq (headAnchor al) r)
        | getElementMeasure m == k
        -> go l' (m FT.<| r)
      -- Empty tree or the measure doesn't match anymore
      _ -> Nothing

-- | \( O(\log(\min(i,n-i)) \). Split the 'AnchoredSeq' before an element with
-- the given measure that satisfies the predicate. Return 'Nothing' if the
-- anchored sequence does not contain an element with the given measure that
-- satisfies the predicate.
--
-- Unlike 'splitAfterMeasure' we can't split before the anchor.
--
-- PRECONDITION: there can be multiple elements with the same measure, but there
-- should be at most one element (or anchor) with the given measure satisfying
-- the predicate.
--
-- POSTCONDITION: joining ('join') the two anchored sequences gives back the
-- original anchored sequence.
--
-- POSTCONDITION: the last element (oldest) in the second sequence has the given
-- measure and satisfies the predicate.
splitBeforeMeasure ::
      Anchorable v a b
   => v
   -> (b -> Bool)
   -> AnchoredSeq v a b
   -> Maybe (AnchoredSeq v a b, AnchoredSeq v a b)
splitBeforeMeasure k p (AnchoredSeq a ft)
    | (l, r) <- FT.split (\v -> measureMax v >= k) ft
      -- @r@ contains elements with a measure >= the given mesaure. There could
      -- be multiple with the given measure, so try them one by one.
    = go l r
    | otherwise
    = Nothing
  where
    go l r = case FT.viewl r of
      m@(MeasuredWith b) FT.:< r'
        | getElementMeasure m == k, p b
        , let al = AnchoredSeq a l
        -> Just (al, AnchoredSeq (headAnchor al) r)
        | getElementMeasure m == k
        -> go (l FT.|> m) r'
      -- Empty tree or the measure doesn't match anymore
      _ -> Nothing

-- | \( O(\log(\min(n_1, n_2))) \). Join two anchored sequences if the given
-- function returns 'True' for the head (newest element or anchor when empty) of
-- the first sequence and the anchor of the second sequence, e.g., when they
-- match.
--
-- The returned sequence will have the same anchor as the first sequence.
join ::
     forall v a b. Anchorable v a b
  => (Either a b -> a -> Bool)
  -> AnchoredSeq v a b
  -> AnchoredSeq v a b
  -> Maybe (AnchoredSeq v a b)
join f s1@(AnchoredSeq a1 ft1) (AnchoredSeq a2 ft2)
    | f (head s1) a2
    = Just $ AnchoredSeq a1 (ft1 FT.>< ft2)
    | otherwise
    = Nothing

-- | \( O(o \log(\min(i,n-i))) \). Select the elements and optionally the anchor
-- based on the given offsets, starting from the head of the 'AnchoredSeq'.
--
-- The list of offsets must be increasing monotonically (/strictly increasing is
-- not required).
--
-- __Note__: offset @n@, where @n@ equals the length of the 'AnchoredSeq',
-- corresponds to the anchor. When the sequence is empty, offset 0 will thus
-- correspond to the anchor.
selectOffsets ::
     forall v a b. Anchorable v a b
  => [Int]
  -> AnchoredSeq v a b
  -> [Either a b]
selectOffsets offsets = go relativeOffsets
  where
    relativeOffsets = zipWith (-) offsets (0:offsets)

    go :: [Int] -> AnchoredSeq v a b -> [Either a b]
    go [] _
      = []
    go (off:offs) s
      | let i = length s - off
      , i >= 0
      , (s', _) <- splitAt i s
      = head s' : go offs s'
      | otherwise
      = []

-- | \( O\(n\) \). Variation on 'filterWithStop' without a stop condition.
filter ::
     forall v a b. Anchorable v a b
  => (b -> Bool)  -- ^ Filtering predicate
  -> AnchoredSeq v a b
  -> [AnchoredSeq v a b]
filter p = filterWithStop p (const False)

-- | \( O(n + r * \log(\min(i,n-i)) \) where /r/ is the number of consecutive
-- ranges of elements to be included in the result.
--
-- Filter out elements that don't match the predicate.
--
-- As filtering removes elements the result is a sequence of disconnected
-- sequences. The sequences are in the original order and are of maximum size.
--
-- As soon as the stop condition is true, the filtering stops and the remaining
-- sequence (starting with the first element for which the stop condition is
-- true) is the final sequence in the returned list.
--
-- The stop condition wins from the filtering predicate: if the stop condition
-- is true for an element, but the filter predicate not, then the element still
-- ends up in final sequence.
--
-- For example, given the sequence containing @[0: 1, 2, 3, 4, 5, 6]@ where the
-- anchor is separated from the elements by @:@:
--
-- > filter         odd        -> [[0: 1], [2: 3], [4: 5]]
-- > filterWithStop odd (>= 4) -> [[0: 1], [2: 3], [3: 4, 5, 6]]
filterWithStop ::
     forall v a b. Anchorable v a b
  => (b -> Bool)  -- ^ Filtering predicate
  -> (b -> Bool)  -- ^ Stop condition
  -> AnchoredSeq v a b
  -> [AnchoredSeq v a b]
filterWithStop p stop c =
    applyFilterRange c <$> startRange (zip [0..] (toOldestFirst c))
  where
    startRange :: [(Int, b)] -> [FilterRange]
    startRange [] = []
    startRange ((i, b):bs)
        | stop b
         -- We can stop filtering, the last range is from @b@ to the end of the
         -- sequence.
        = [FilterRange i (length c - 1)]

        | p b
          -- We can use @b@ to start a range, try extending it with the next
          -- element
        = extendRange i i bs

        | otherwise
          -- Not part of a range, try the next element
        = startRange bs

    extendRange :: Int -> Int -> [(Int, b)] -> [FilterRange]
    extendRange !start !end [] = [FilterRange start end]
    extendRange !start !end ((i, b):bs)
        | stop b
          -- We can stop filtering, the last range is from @start@ to the end of the
          -- sequence.
        = [FilterRange start (length c - 1)]

        | p b
          -- Extend the open range with @b@
        = extendRange start i bs

        | otherwise
          -- End the open range and try starting another one
        = FilterRange start end : startRange bs

-- | Range with /inclusive/ bounds, i.e., indices, that should be included in
-- the result of a filtering operation.
--
-- INVARIANT: the first lower bound <= the upper bound
--
-- When used in combination with an anchored sequence, both indices should be in
-- the [0, size of sequence) range.
data FilterRange = FilterRange !Int !Int
  deriving (Show)

-- | \( O(\log(\min(i,n-i)) \). Apply a 'FilterRange' to an anchored sequence,
-- returning the sequence matching the range.
--
-- For example, @FilterRange 0 0@ correspond to the first element of the
-- sequence. @FilterRange 0 1@ corresponds to the first two elements of the
-- sequence.
--
-- Since both bounds are inclusive, the sequence is never empty.
--
-- PRECONDITION: both indices are in the @[0, size of sequence)@ range.
applyFilterRange ::
     forall v a b. Anchorable v a b
  => AnchoredSeq v a b
  -> FilterRange
  -> AnchoredSeq v a b
applyFilterRange c (FilterRange start stop) = inRange
  where
    (_before, fromStart) = splitAt start c
    (inRange, _after)    = splitAt (stop - start + 1) fromStart

-- | \( O\(n\) \). Naive reference implementation of 'filterWithStop'.
--
-- While the asymptotic complexity of this function is better than that of
-- 'filterWithStop', the allocation cost is high. This function deconstructs and
-- reconstructs the anchored sequence (until the stop condition is reached),
-- even when no elements are removed.
filterWithStopSpec ::
     forall v a b. Anchorable v a b
  => (b -> Bool)  -- ^ Filtering predicate
  -> (b -> Bool)  -- ^ Stop condition
  -> AnchoredSeq v a b
  -> [AnchoredSeq v a b]
filterWithStopSpec p stop = goNext []
  where
    goNext :: [AnchoredSeq v a b]  -- Previously constructed sequences
           -> AnchoredSeq v a b    -- Sequences still to process
           -> [AnchoredSeq v a b]
    goNext cs af = go cs (Empty (anchor af)) af

    go :: [AnchoredSeq v a b]  -- Previously constructed sequences
       -> AnchoredSeq v a b    -- Currently accumulating sequence
       -> AnchoredSeq v a b    -- Sequences still to process
       -> [AnchoredSeq v a b]
    go cs c' af@(b :< c) | stop b = reverse (addToAcc (join' c' af) cs)
                         | p    b = go cs (c' :> b) c
    go cs c' (_ :< c)             = goNext (addToAcc c' cs) c
    go cs c' (Empty _)            = reverse (addToAcc c' cs)

    addToAcc :: AnchoredSeq v a b
             -> [AnchoredSeq v a b]
             -> [AnchoredSeq v a b]
    addToAcc (Empty _) acc =    acc
    addToAcc c'        acc = c':acc

    -- This is called with @c'@ and @(b : < c)@. @c'@ is the sequence containing
    -- the elements before @b@, so they must be joinable.
    join' :: AnchoredSeq v a b
          -> AnchoredSeq v a b
          -> AnchoredSeq v a b
    join' a b =
        fromMaybe (error "could not join sequences") $
        join (\_ _ -> True) a b
