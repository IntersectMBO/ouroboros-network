{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# OPTIONS -fno-warn-unused-imports #-}

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

testManyLaws :: forall a proxy. proxy a -> TestName -> [Proxy a -> TestTree] -> TestTree
testManyLaws _ name = testGroup name . fmap ($ Proxy @ a)

testLaws :: TestTree
testLaws = testGroup "Laws"
    [ testManyLaws (Proxy @ (LedgerOnDisk.Class.D Int)) "D"
      [Laws.testEqLaws, Laws.testSemigroupLaws, Laws.testMonoidLaws ]
    , testManyLaws (Proxy @ (LedgerOnDisk.WWB.InMemoryEntryMeasure Int Int)) "InMemoryEntryMeasure"
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
  ]
