{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Map.Diff.Strict.Ver4 (
    Act (..)
  , Diff (..)
  , PDiff
  , PSeq (..)
  , Partial (..)
    -- * Construction
  , diff
  , fromList
  , fromPartial
  , fromPartialUnsafe
  , fromSeq
    -- * Special subtraction
  , leftSubtr
  ) where

import           Control.Monad
import           Data.Group
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroupoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

{------------------------------------------------------------------------------
  General-purposes diffs for key-value stores
------------------------------------------------------------------------------}

type PDiff k v = Partial (Diff k v)

-- | Redefine 'Maybe'
data Partial a = Defined !a | Undefined
  deriving (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | A diff for key-value stores.
newtype Diff k v = Diff (Map k (PSeq (Act v)))
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

data PSeq a = UnsafePSeq a !(Seq a)
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | An action to apply to a value in a key-value store.
--
-- Note: This datatype froms a 'Groupoid' of insertions and deletions.
data Act v = Del !v | Ins !v | DelIns !v !v | InsDel
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

{------------------------------------------------------------------------------
  Construction
------------------------------------------------------------------------------}

toPartial :: Maybe a -> Partial a
toPartial = maybe Undefined Defined

fromPartial :: Partial a -> Maybe a
fromPartial = \case
  Undefined -> Nothing
  Defined x -> Just x

fromPartialUnsafe :: Partial a -> a
fromPartialUnsafe Undefined   = error "bad fromPartialUnsafe"
fromPartialUnsafe (Defined x) = x

fromList :: Ord k => [(k, Act v)] -> Diff k v
fromList = Diff . Map.fromList . fmap (fmap singleton)

diff :: (Ord k, Eq v) => Map k v -> Map k v -> Diff k v
diff m1 m2 = Diff $
  Merge.merge
    (Merge.mapMissing $ \_k v -> singleton (Del v))
    (Merge.mapMissing $ \_k v -> singleton (Ins v))
    (Merge.zipWithMaybeMatched $ \ _k v1 v2 ->
      if v1 == v2 then
        Nothing
      else
        Just $ singleton (DelIns v1 v2)
    )
    m1
    m2

-- * construction for @'PSeq'@s

singleton :: a -> PSeq a
singleton !x = UnsafePSeq x (Seq.singleton x)

fromSeq :: Groupoid a => a -> Seq a -> Maybe (PSeq a)
fromSeq x = foldM snoc (singleton x)

snoc :: Groupoid a => PSeq a -> a -> Maybe (PSeq a)
snoc (UnsafePSeq c sq) !x = case sq of
    Seq.Empty      -> error "impossible!"
    _ Seq.:|> prev -> do
        _ <- prev <>? x
        -- safe for the same reason at poperator below
        pure $! UnsafePSeq (c `pappendUnsafe` x) (sq Seq.:|> x)

{------------------------------------------------------------------------------
  Class instances for @'Act' v@
------------------------------------------------------------------------------}

instance Eq v => Semigroupoid (Act v) where
    l <>? r = case l of
        Del x -> case r of
            Del{}    -> Nothing   -- disallow double delete
            Ins y    -> Just $ DelIns x y

            DelIns{} -> Nothing   -- disallow double delete

            InsDel   -> Just $ Del x

        Ins x -> case r of
            Del y ->
                if x /= y then Nothing   -- disallow inaccurate delete
                else Just InsDel
            Ins{} -> Nothing   -- disallow overwrite

            DelIns y z ->
                if x /= y then Nothing   -- disallow inaccurate delete
                else Just $ Ins z

            InsDel{} -> Nothing   -- disallow overwrite

        DelIns x y -> case r of
            Del z ->
                if y /= z then Nothing   -- disallow inaccurate delete
                else Just $ Del x
            Ins{} -> Nothing   -- disallow overwrite

            DelIns z aa ->
                if y /= z then Nothing   -- disallow inaccurate delete
                else Just $ DelIns x aa

            InsDel{} -> Nothing   -- disallow overwrite

        InsDel -> case r of
            Del{}    -> Nothing   -- disallow double delete
            Ins x    -> Just $ Ins x

            DelIns{} -> Nothing   -- disallow double delete

            InsDel   -> Just InsDel

instance Eq v => Groupoid (Act v) where
  pinv = \case
      Del v      -> Ins v
      Ins v      -> Del v

      DelIns x y -> DelIns y x

      InsDel     -> InsDel

{------------------------------------------------------------------------------
  Class instances for @'PSeq' a@
------------------------------------------------------------------------------}

instance Groupoid a => Semigroupoid (PSeq a) where
  (<>?) = \(UnsafePSeq lc lsq) (UnsafePSeq rc rsq) -> do
      sq <- pappend' lsq rsq
      -- Since we've checked definedness of each adjacent pair, the total
      -- concatentation exists by /Associativity/.
      pure $! UnsafePSeq (lc `pappendUnsafe` rc) sq
    where
      pappend' :: Semigroupoid x => Seq x -> Seq x -> Maybe (Seq x)
      pappend' lsq rsq = case (lsq, rsq) of
          (Seq.Empty  , _          ) -> Just rsq
          (_          , Seq.Empty  ) -> Just lsq
          (_ Seq.:|> l, r Seq.:<| _) -> do
              _ <- l <>? r
              -- Inductive Hypothesis: the invariant on 'UnsafeSeqPG' ensures
              -- the adjacent pairs in @lsq@ and @rsq@ are already checked.
              pure $! lsq Seq.>< rsq

instance Groupoid a => Groupoid (PSeq a) where
    pinv (UnsafePSeq c sq) = UnsafePSeq (pinv c) (pinv <$> Seq.reverse sq)

leftSubtr :: (Groupoid a, Eq a, Show a) => PSeq a -> PSeq a -> Maybe (Maybe (PSeq a))
leftSubtr (UnsafePSeq lc lsq) (UnsafePSeq rc rsq) = do
    sq' <- lsq `leftSubtr'` rsq
    pure $! UnsafePSeq (pinv lc `pappendUnsafe` rc) <$> sq'
  where
    leftSubtr' :: (Eq a, Show a) => Seq a -> Seq a -> Maybe (Maybe (Seq a))
    leftSubtr' sq1 sq2 = case (sq1, sq2) of
      (_ Seq.:<| _, _ Seq.:<| _) -> leftSubtr'' sq1 sq2
      (_,  _)                    -> Nothing
    leftSubtr'' :: (Eq a, Show a) => Seq a -> Seq a -> Maybe (Maybe (Seq a))
    leftSubtr'' sq1 sq2 = case (sq1, sq2) of
      (x Seq.:<| xs, y Seq.:<| ys)
        | x == y                    -> leftSubtr'' xs ys
        | otherwise                 -> Just Nothing
      (Seq.Empty, Seq.Empty)        -> Just Nothing
      (Seq.Empty, ys)               -> Just (Just ys)
      (_, _)                        -> Nothing

{------------------------------------------------------------------------------
  Class instances for @'Diff' k v@
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => Semigroupoid (Diff k v) where
  (<>?) (Diff l) (Diff r) = Diff <$> sequenceA (
    Merge.merge
      (Merge.mapMissing (const Just))
      (Merge.mapMissing (const Just))
      (Merge.zipWithMatched f
      )
      l
      r)
    where
      f _ v1 v2 = v1 <>? v2

instance (Ord k, Eq v) => Groupoid (Diff k v) where
  pinv (Diff m) = Diff $ fmap pinv m

{------------------------------------------------------------------------------
  Class instances for @'PDiff' k v@ (partial diffs)
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => Semigroup (PDiff k v) where
  Defined x <> Defined y = toPartial (x <>? y)
  _         <> _         = Undefined

instance (Ord k, Eq v) => Monoid (PDiff k v) where
  mempty = Defined . Diff $ Map.empty

instance (Ord k, Eq v) => Group (PDiff k v) where
  invert Undefined   = Undefined
  invert (Defined x) = Defined $ pinv x
