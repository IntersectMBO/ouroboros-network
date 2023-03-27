{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.Counting (
  tests,
  ) where

import           Data.Proxy (Proxy (Proxy))
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import qualified Test.QuickCheck as QC
import qualified Test.Tasty as TT
import qualified Test.Tasty.QuickCheck as TT

-----

tests :: [TT.TestTree]
tests = [
    TT.testProperty "prop_withWindow" prop_withWindow
  ]

-----

prop_withWindow :: QC.NonNegative Int -> Int -> Int -> QC.Property
prop_withWindow (QC.NonNegative n) i m =
    case C.withWindow (C.Count n) (C.Lbl :: C.Lbl "test") (C.Count i) (C.Count m) of
        C.SomeWindow Proxy (C.Contains (C.Count i') (C.Count m')) ->
            QC.counterexample (show (i', m'))
          $ QC.conjoin [
                if i < 0
                then QC.counterexample "neg i" $ i' QC.=== 0
                else QC.counterexample "nonneg i" $ i' QC.=== i
              , if m < 0
                then QC.counterexample "neg m" $ m' QC.=== 0
                else QC.counterexample "nonneg m" $ min (n - 1) i' + m' <= n
              ] 
