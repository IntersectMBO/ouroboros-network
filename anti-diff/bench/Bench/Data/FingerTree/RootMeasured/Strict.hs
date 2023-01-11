{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Bench.Data.FingerTree.RootMeasured.Strict (benchmarks) where

import           Control.Arrow
import           Control.DeepSeq (NFData (..))
import           Data.Bifunctor
import           Data.Foldable
import           Data.Group
import           Data.Monoid
import           Text.Printf

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Bench
import           Test.Tasty.QuickCheck

import qualified Data.FingerTree as FT
import           Data.FingerTree.RootMeasured.Strict
import qualified Data.FingerTree.Strict as SFT

{-------------------------------------------------------------------------------
  Main benchmark tree
-------------------------------------------------------------------------------}

benchmarks :: Benchmark
benchmarks = bgroup "Strict" [
      benchSplits n 0
    , benchSplits n (n `quot` 2)
    , benchSplits n n
    , tests
    ]
  where
    n = 10_000_000

-- | Benchmark helper
benchSplits :: Int -> Int -> Benchmark
benchSplits n m = env (pure $ fromList [1..n]) $ \sft ->
  bgroup (printf "Split finger tree of size %d at position %d" n m) [
      bgroup "nf full fingertree" [
          bench "splitlAt" $
            nf (splitlAt m) sft
        , bench "splitrAt" $
            nf (splitrAt m) sft
        , bench "splitSizedAt" $
            nf (splitSizedAt m) sft
        ]
    , bgroup "nf root measures" [
          bench "splitlAt" $
            nf (getRootMeasures . splitlAt m) sft
        , bench "splitrAt" $
            nf (getRootMeasures . splitrAt m) sft
        , bench "splitSizedAt" $
            nf (getRootMeasures . splitSizedAt m) sft
        ]
    , bgroup "nf internal measures" [
          bench "splitlAt" $
            nf (getInternalMeasures . splitlAt m) sft
        , bench "splitrAt" $
            nf (getInternalMeasures . splitrAt m) sft
        , bench "splitSizedAt" $
            nf (getInternalMeasures . splitSizedAt m) sft
        ]
    , bgroup "nf all measures" [
          bench "splitlAt" $
            nf ((getRootMeasures &&& getInternalMeasures) . splitlAt m) sft
        , bench "splitrAt" $
            nf ((getRootMeasures &&& getInternalMeasures) . splitrAt m) sft
        , bench "splitSizedAt" $
            nf ((getRootMeasures &&& getInternalMeasures) . splitSizedAt m) sft
        ]
    , testGroup "Sanity checks" [
          testProperty "once prop_matchSplitAt_LeftRight" $
            once $ prop_matchSplitAt_LeftRight m sft
        , testProperty "once prop_matchSplitAt_RightSized" $
            once $ prop_matchSplitAt_RightSized m sft
        , testProperty "once prop_matchSplitAt_SizedLeft" $
            once $ prop_matchSplitAt_SizedLeft m sft
        ]
    ]

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type T = StrictFingerTree (Sum Int) Length Int

newtype Length = Length {unLength :: Int}
  deriving stock (Show, Eq, Ord)
  deriving newtype NFData

deriving via Sum Int instance Semigroup Length
deriving via Sum Int instance Monoid Length
deriving via Sum Int instance Group Length

instance Measured Length Int where
  measure _ = Length 1

instance RootMeasured (Sum Int) Int where
  measureRoot = Sum

instance Sized Length where
  size = unLength

{-------------------------------------------------------------------------------
  Functions to benchmark
-------------------------------------------------------------------------------}

splitlAt :: Int -> T -> (T, T)
splitlAt n = splitl (Length n<)

splitrAt :: Int -> T -> (T, T)
splitrAt n = splitr (Length n<)

splitSizedAt :: Int -> T -> (T, T)
splitSizedAt n = splitSized (Length n<)

{-------------------------------------------------------------------------------
  Function results to evaluate
-------------------------------------------------------------------------------}

getRootMeasures :: (T, T) -> (Sum Int, Sum Int)
getRootMeasures = bimap measureRoot measureRoot

getInternalMeasures :: (T, T) -> (Length, Length)
getInternalMeasures = bimap measure measure

{-------------------------------------------------------------------------------
  Orphan instances: @'NFData'@
-------------------------------------------------------------------------------}

deriving anyclass instance (NFData vr, NFData vi, NFData a, Measured vi a)
                        => NFData (StrictFingerTree vr vi a)

instance (NFData vi, NFData a, Measured vi a)
      => NFData (SFT.StrictFingerTree vi a) where
  rnf ft = rnf (FT.measure ft) `seq` rnf (toList ft)

{-------------------------------------------------------------------------------
  Property tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Tests" [
    testProperty "prop_matchSplitAt_LeftRight"
      prop_matchSplitAt_LeftRight
  , testProperty "prop_matchSplitAt_RightSized"
      prop_matchSplitAt_RightSized
  , testProperty "prop_matchSplitAt_SizedLeft"
      prop_matchSplitAt_SizedLeft
  ]

-- | Results of @'splitlAt'@ should match results of @'splitrAt'@.
prop_matchSplitAt_LeftRight :: Int -> T -> Property
prop_matchSplitAt_LeftRight n sft = splitlAt n sft === splitrAt n sft

-- | Results of @'splitrAt'@ should match results of @'splitSizedAt'@.
prop_matchSplitAt_RightSized :: Int -> T -> Property
prop_matchSplitAt_RightSized n sft = splitrAt n sft === splitSizedAt n sft

-- | Results of @'splitSizedAt'@ should match results of @'splitlAt'@.
prop_matchSplitAt_SizedLeft :: Int -> T -> Property
prop_matchSplitAt_SizedLeft n sft = splitSizedAt n sft === splitlAt n sft

instance (Arbitrary a, SuperMeasured vr vi a)
      => Arbitrary (StrictFingerTree vr vi a) where
  arbitrary = fromList <$> arbitrary
  shrink    = fmap fromList . shrink . toList
