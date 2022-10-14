{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict (tests) where

import           Data.Foldable (foldl')
import           Data.Maybe
import           Data.Proxy (Proxy (Proxy))
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import           Data.Map.Diff.Strict

import           Data.Semigroupoid.Auto
import           Data.Semigroupoid.Laws

tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict" [
      testGroupWithProxy (Proxy @(DiffEntry (Smaller Int))) [
      ]
    , testGroupWithProxy (Proxy @(DiffHistory (Smaller Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (DiffHistory (Smaller Int)))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    , testGroupWithProxy (Proxy @(Diff (Smaller Int) (Smaller Int))) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (Diff (Smaller Int) (Smaller Int)))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    , testGroupWithProxy (Proxy @(Act (Smaller Int))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    ]

{------------------------------------------------------------------------------
  Preconditions
------------------------------------------------------------------------------}

-- | Check if a diff history is in normal form, where no succesive elements are
-- inverses of each other.
--
-- If two succesive diff entries are inverses, they can be cancelled out. In
-- other words, we can normalise the diff history further by cancelling out the
-- diff entries. If so, we can conclude that the input diff history is not in
-- normal form.
isNormal :: (Foldable t, Eq v) => UnsafeDiffHistory t v -> Bool
isNormal (UnsafeDiffHistory vs) =
    snd $ foldl' f (Nothing, True) vs
  where
    f (prevMay, b) cur = case prevMay of
      Nothing   -> (Just cur, b)
      Just prev -> (Just cur, b && not (areInverses prev cur))

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

newtype Smaller a = Smaller a
  deriving newtype (Show, Eq, Ord)

instance Integral a => Arbitrary (Smaller a) where
  arbitrary = Smaller . fromIntegral <$> chooseInt (-5, 5)
  shrink (Smaller x) = Smaller . fromIntegral <$> shrink @Int (fromIntegral x)

deriving newtype instance (Ord k, Eq v, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)

instance (Arbitrary v, Eq v) => Arbitrary (NEDiffHistory v) where
  arbitrary = (NEDiffHistory <$> ((:<||) <$> arbitrary <*> arbitrary))
    `suchThat` (\(MkNEDiffHistory h) -> isNormal h)
  shrink (NEDiffHistory h) =
    fmap NEDiffHistory $ mapMaybe NESeq.nonEmptySeq $ shrink (NESeq.toSeq h)

instance (Arbitrary v, Eq v) => Arbitrary (DiffHistory v) where
  arbitrary = (DiffHistory <$> arbitrary)
    `suchThat` (\(MkDiffHistory h) -> isNormal h)
  shrink (DiffHistory s) = DiffHistory <$> shrink s

instance Arbitrary v => Arbitrary (DiffEntry v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , Delete <$> arbitrary
    , UnsafeAntiInsert <$> arbitrary
    , UnsafeAntiDelete <$> arbitrary
    ]
  shrink = \case
    Insert x           -> Insert <$> shrink x
    Delete x           -> Delete <$> shrink x
    UnsafeAntiInsert x -> UnsafeAntiInsert <$> shrink x
    UnsafeAntiDelete x -> UnsafeAntiDelete <$> shrink x

instance Arbitrary v => Arbitrary (Act v) where
  arbitrary = oneof [
      Ins <$> arbitrary
    , Del <$> arbitrary
    , pure InsDel
    , DelIns <$> arbitrary <*> arbitrary
    ]
  shrink = \case
    Ins x      -> Ins <$> shrink x
    Del x      -> Del <$> shrink x
    InsDel     -> []
    DelIns x y -> DelIns <$> shrink x <*> shrink y
