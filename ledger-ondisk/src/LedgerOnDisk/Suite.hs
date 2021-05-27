{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# OPTIONS -fno-warn-unused-imports #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module LedgerOnDisk.Suite where

import qualified LedgerOnDisk.QSM.Suite(tests)
import Test.Tasty
import qualified Test.Tasty.QuickCheck.Laws as Laws

import qualified LedgerOnDisk.Class
import qualified LedgerOnDisk.WWB
import Data.Proxy
import Control.Monad.Reader
import GHC.Base (noinline)
import System.IO.Unsafe
import Control.Applicative
import LedgerOnDisk.WWB
import Test.QuickCheck
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Test.Tasty.QuickCheck
import LedgerOnDisk.Diff
import Data.Monoid

prop_applyD :: (Eq v, Show v, Arbitrary v) => proxy v -> Maybe v -> D v -> D v -> Property
prop_applyD _ mb_i d1 d2 = applyD (applyD mb_i d1) d2 === applyD mb_i (d1 <> d2)

prop_applyDForK :: (Eq v, Show v, Arbitrary v) => proxy v -> Int -> D v -> D v -> HashMap Int v -> Property
prop_applyDForK _ k d1 d2 m = applyDforK k d2 (applyDforK k d1 m) === applyDforK k (d1 <> d2) m

prop_applyDtoHashMap :: (Eq v, Show v, Arbitrary v) => proxy v -> HashMap Int (D v) -> HashMap Int (D v) -> HashMap Int v -> Property
prop_applyDtoHashMap _ d1 d2 m = applyDtoHashMap d2 (applyDtoHashMap d1 m) === applyDtoHashMap (HashMap.unionWith (<>) d1 d2) m

prop_applyDtoHashMaybeMap :: (Eq v, Show v, Arbitrary v) => proxy v -> HashMap Int (D v) -> HashMap Int (D v) -> HashMap Int (Maybe v) -> Property
prop_applyDtoHashMaybeMap _ d1 d2 m = r1 === r2 .&&. r1 `check_same_size` m
  where
    check_same_size x y = length x == length y
    r1 = applyDtoHashMaybeMap d2 (applyDtoHashMaybeMap d1 m)
    r2 = applyDtoHashMaybeMap (HashMap.unionWith (<>) d1 d2) m

testManyLaws :: forall a proxy. proxy a -> TestName -> [Proxy a -> TestTree] -> TestTree
testManyLaws _ name = testGroup name . fmap ($ Proxy @ a)

testLaws :: TestTree
testLaws = testGroup "Laws"
    [ testManyLaws (Proxy @ (LedgerOnDisk.Class.D (Sum Int))) "D"
      [Laws.testEqLaws, Laws.testSemigroupLaws, Laws.testMonoidLaws ]
    , testManyLaws (Proxy @ (LedgerOnDisk.WWB.WriteBufferMeasure Int Int)) "WriteBufferMeasure"
      [Laws.testSemigroupLaws, Laws.testMonoidLaws]
    -- , testGroup "WWBT" $
    --     [ Laws.testReaderMonadLaws
    --         (Proxy @ (LedgerOnDisk.WWB.WWBT Int Int (ReaderT Int IO)))
    --         (Proxy @ Int)
    --         (Proxy @ Int)
    --         (Proxy @ Bool)
    --         (Proxy @ [Bool])
    --         (\i l r -> let
    --             go = liftA2 (==) l r
    --             in noinline . snd $ unsafePerformIO (runReaderT (LedgerOnDisk.WWB.runWWBT go True LedgerOnDisk.WWB.FPNever mempty) i))
    --         ask
    --         local
    --     ]
    ]

tests :: TestTree
tests = testGroup "LedgerOnDisk"
  [ LedgerOnDisk.QSM.Suite.tests
  , testLaws
  , testGroup "D"
    [ testProperty "prop_applyD" $ prop_applyD (Proxy @ (Sum Int))
    , testProperty "prop_applyDForK" $ prop_applyDForK (Proxy @ (Sum Int))
    , testProperty "prop_applyDtoHashMap" $ prop_applyDtoHashMap (Proxy @ (Sum Int))
    , testProperty "prop_applyDtoHashMaybeMap" $ prop_applyDtoHashMaybeMap (Proxy @ (Sum Int))
    ]
  ]
