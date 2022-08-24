{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict.Ver2a (tests) where

import           Data.Proxy (Proxy (Proxy))

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)

import           Data.Map.Diff.Strict.Ver2a

import           Test.Util.Auto
import           Test.Util.Laws (testGroupLaws, testGroupoidLaws,
                     testMonoidLaws, testSemigroupLaws, testSemigroupoidLaws)
import           Test.Util.Tasty
import           Test.Util.X

tests = testGroup "Data.Map.Diff.Strict.Ver2a" []

{-
tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict.Ver2a" [
      testGroupWithProxy (Proxy @(DiffEntry X)) [
      ]
    , testGroupWithProxy (Proxy @(DiffHistory X)) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (DiffHistory X))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    , testGroupWithProxy (Proxy @(Diff X X)) [
          testSemigroupLaws
        , testMonoidLaws
        , testGroupLaws
        ]
    , testGroupWithProxy (Proxy @(Auto (Diff X X))) [
          testSemigroupoidLaws
        , testGroupoidLaws
        ]
    ]

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)
deriving newtype instance (Arbitrary v) => Arbitrary (DiffHistory v)
instance (Arbitrary v) => Arbitrary (DiffEntry v) where
  arbitrary = oneof [Insert <$> arbitrary, Delete <$> arbitrary]
-}
