{-# LANGUAGE LambdaCase #-}

-- | This module defines wrappers with arbitrary 'Ord' instances around types
-- for which @Cabal@ does not derive 'Ord'; we only use these to put these
-- types in maps, sets, etc.

module Cabal.Lint.Ord (
  MyVersionRange (..),
  MyLanguage (..),
  ) where

import qualified Distribution.Types.VersionRange.Internal                  as C
import qualified Language.Haskell.Extension                                as C

-----

newtype MyVersionRange = MyVersionRange {unMyVersionRange :: C.VersionRange}
  deriving (Eq)

instance Ord MyVersionRange where
  compare = (\f (MyVersionRange x) (MyVersionRange y) -> f (x, y)) $ \case
      (C.ThisVersion l, C.ThisVersion r) -> compare l r
      (C.ThisVersion{}, _              ) -> LT
      (_, C.ThisVersion{}              ) -> GT

      (C.LaterVersion l, C.LaterVersion r) -> compare l r
      (C.LaterVersion{}, _               ) -> LT
      (_, C.LaterVersion{}               ) -> GT

      (C.OrLaterVersion l, C.OrLaterVersion r) -> compare l r
      (C.OrLaterVersion{}, _                 ) -> LT
      (_, C.OrLaterVersion{}                 ) -> GT

      (C.EarlierVersion l, C.EarlierVersion r) -> compare l r
      (C.EarlierVersion{}, _                 ) -> LT
      (_, C.EarlierVersion{}                 ) -> GT

      (C.OrEarlierVersion l, C.OrEarlierVersion r) -> compare l r
      (C.OrEarlierVersion{}, _                   ) -> LT
      (_, C.OrEarlierVersion{}                   ) -> GT

      (C.MajorBoundVersion l, C.MajorBoundVersion r) -> compare l r
      (C.MajorBoundVersion{}, _                    ) -> LT
      (_, C.MajorBoundVersion{}                    ) -> GT

      (C.UnionVersionRanges l1 l2, C.UnionVersionRanges r1 r2) -> cmp l1 r1 <> cmp l2 r2
      (C.UnionVersionRanges{}, _                             ) -> LT
      (_, C.UnionVersionRanges{}                             ) -> GT

      (C.IntersectVersionRanges l1 l2, C.IntersectVersionRanges r1 r2) -> cmp l1 r1 <> cmp l2 r2
    where
      cmp x y = compare (MyVersionRange x) (MyVersionRange y)

-----

newtype MyLanguage = MyLanguage C.Language
  deriving (Eq)

instance Ord MyLanguage where
  compare = (\f (MyLanguage x) (MyLanguage y) -> f (x, y)) $ \case
    (C.Haskell98, C.Haskell98) -> EQ
    (C.Haskell98, _          ) -> LT
    (_          , C.Haskell98) -> GT

    (C.Haskell2010, C.Haskell2010) -> EQ
    (C.Haskell2010, _            ) -> LT
    (_            , C.Haskell2010) -> GT

    (C.GHC2021, C.GHC2021) -> EQ
    (C.GHC2021, _        ) -> LT
    (_        , C.GHC2021) -> GT

    (C.UnknownLanguage l, C.UnknownLanguage r) -> compare l r
