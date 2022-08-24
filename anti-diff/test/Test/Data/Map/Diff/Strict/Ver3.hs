{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Map.Diff.Strict.Ver3 (tests) where

import           Data.Typeable

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)

import           Data.Map.Diff.Strict.Ver3

import           Test.Util.Auto
import           Test.Util.Laws (testGroupLaws, testGroupoidLaws,
                     testMonoidLaws, testSemigroupLaws, testSemigroupoidLaws)
import           Test.Util.Tasty
import           Test.Util.X



-- | Testing type class laws for diff datatypes.
tests :: TestTree
tests = testGroup "Data.Map.Diff.Strict.Ver3" [
    testGroupWithProxy (Proxy @(Act X)) [
        testSemigroupoidLaws
      , testGroupoidLaws
      ]
  , testGroupWithProxy (Proxy @(Diff X X)) [
        testSemigroupoidLaws
      , testGroupoidLaws
      ]
  , testGroupWithProxy (Proxy @(PDiff X X)) [
        testSemigroupLaws
      , testMonoidLaws
      , testGroupLaws
      ]
  , testGroupWithProxy (Proxy @(Auto (PDiff X X))) [
        testSemigroupoidLaws
      , testGroupoidLaws
      ]
  ]

instance Arbitrary v => Arbitrary (Act v) where
  arbitrary = oneof [
        Del <$> arbitrary
      , Ins <$> arbitrary
      , DelIns <$> arbitrary <*> arbitrary
      , pure InsDel
      ]
deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)
instance Arbitrary a => Arbitrary (Partial a) where
  arbitrary = oneof [
      Defined <$> arbitrary
    , pure Undefined
    ]
