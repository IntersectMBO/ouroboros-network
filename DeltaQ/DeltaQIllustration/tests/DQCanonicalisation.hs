{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DQCanonicalisation (tests,t,ndq) where

import Prelude hiding (catch)

import           Control.Exception (SomeException, catch)
import           Control.Monad (liftM, liftM2, liftM3)
import           Data.Maybe
import           Data.Typeable (Typeable(..))
import           Debug.Trace
import           System.Random
import           Test.QuickCheck (arbitrary)
import qualified Distribution.TestSuite as Cabal
import qualified Test.QuickCheck as QC

import DeltaQ.Algebra
import DeltaQ.Algebra.DelayModel.SimpleUniform
import DeltaQ.Algebra.Simplification

type RationalDeltaQ = DeltaQ Probability SimpleUniform NonNeg

data QCTest = forall prop. QC.Testable prop => QCTest String prop

test :: QC.Testable prop => String -> prop -> Cabal.Test
test n p = Cabal.impure $ QCTest n p

-- to run a single property - during development
t :: QC.Testable prop =>  prop -> IO Cabal.Result
t p = Cabal.runM (test "<interactive>" p) opts
      where
        opts = Cabal.Options [
            ("verbose", show True)
          , ("max-success", show (1000 :: Int))
          , ("max-discard", show (25 :: Int))
          , ("std-gen", "")
          , ("size", show (2 :: Int))
          ]

-- another interactive helper
ndq :: RationalDeltaQ -> RationalDeltaQ
ndq = canonicaliseDeltaQ

instance QC.Arbitrary RationalDeltaQ where
  arbitrary = QC.oneof
    [ return Bottom
    , return Unit
    , liftM (Delay . DiracDelta) arbitrary
    , liftM (Delay . UniformD) arbitrary
    , liftM2 Convolve arbitrary arbitrary
    , liftM3 ProbChoice arbitrary arbitrary arbitrary
    ]

newtype Probability = Probability Rational
                      deriving (Eq,Ord,Show, Num, Fractional, Real)
instance Bounded Probability where
  minBound = Probability 0
  maxBound = Probability 1


newtype NonNeg = NonNeg Rational
                      deriving (Eq,Ord,Show, Num, Fractional, Real)
instance Bounded NonNeg where
  minBound = NonNeg 0
  maxBound = NonNeg 1000 -- just an arbitrary choice

instance QC.Arbitrary Probability where
  arbitrary = QC.choose (0,1)  >>= return . Probability . fromRational . (toRational :: Double -> Rational)

instance QC.Arbitrary NonNeg where
    arbitrary = QC.choose (0,1000)  >>= return . NonNeg . fromRational . (toRational :: Double -> Rational)

{-
instance Cabal.Test QCTest where
    name (QCTest n _) = n
    options _ =
      [ ("std-gen", typeOf (undefined :: String))
      , ("max-success", typeOf (undefined :: Int))
      , ("max-discard", typeOf (undefined :: Int))
      , ("size", typeOf (undefined :: Int))
      , ("verbose", typeOf (undefined :: Bool))
      ]

    defaultOptions _ = do
      rng <- newStdGen
      return $ Cabal.Options $
             [ ("std-gen", show rng)
             , ("max-success", show $ QC.maxSuccess QC.stdArgs)
             , ("max-discard", show $ QC.maxDiscard QC.stdArgs)
             , ("size", show $ QC.maxSize QC.stdArgs)
             , ("verbose", show $ QC.chatty QC.stdArgs)
             ]

    check _ (Cabal.Options opts) = catMaybes
         [ maybeNothing "max-success" ([] :: [(Int, String)])
         , maybeNothing "max-discard" ([] :: [(Int, String)])
         , maybeNothing "size" ([] :: [(Int, String)])
         , maybeNothing "verbose" ([] :: [(Bool, String)])
         ]
         -- There is no need to check the parsability of "std-gen"
         -- because the Read instance for StdQC.Gen always succeeds.
         where
             maybeNothing n x =
                 maybe Nothing (\str ->
                     if reads str == x then Just n else Nothing)
                     $ lookup n opts

instance Cabal.ImpureTestable QCTest where
  runM (QCTest _ prop) o =
    catch go (return . Cabal.Error . (show :: SomeException -> String))
      where
        go = do
          result <- QC.quickCheckWithResult args prop
          return $ case result of
            QC.Success {} -> Cabal.Pass
            QC.GaveUp {}->
              Cabal.Fail $ "gave up after "
                            ++ show (QC.numTests result)
                            ++ " tests"
            QC.Failure {} -> Cabal.Fail $ QC.reason result
            QC.NoExpectedFailure {} ->
              Cabal.Fail "passed (expected failure)"
        args = QC.Args
                 { QC.replay = Just
                     ( Cabal.lookupOption "std-gen" o
                     , Cabal.lookupOption "size" o
                     )
                 , QC.maxSuccess = Cabal.lookupOption "max-success" o
                 , QC.maxDiscard = Cabal.lookupOption "max-discard" o
                 , QC.maxSize = Cabal.lookupOption "size" o
                 , QC.chatty = Cabal.lookupOption "verbose" o
                 }
-}

tests :: [Cabal.Test]
tests =
 [ -- "Prob Choice Rules"
   test "a [0⇋1] b ⇒ b" prop_p_rule_zero_choice
 , test "a [1⇋0] b ⇒ a" prop_p_rule_one_choice
 , test "⊥ [p⇋q] ⊥ ⇒ ⊥, ∀p,q" prop_p_rule_bottom
 , test "∅ [p⇋q] ∅ ⇒ ∅, ∀p,q" prop_p_rule_unit
 , test "x [p⇋q] ⊥ ⇒ ⊥ [q⇋p] x" prop_p_canon_bottom
 , test "∅ [p⇋q] x ⇒ x [q⇋p] ∅" prop_p_canon_unit
--testGroup "Convolution Identities"
 , test "⊥ ⊕ x ⇒ ⊥, ∀x" prop_c_bottom_a
 , test "x ⊕ ⊥ ⇒ ⊥, ∀x" prop_c_bottom_b
 , test "∅ ⊕ x ⇒ x, ∀x" prop_c_unit_a
 , test "x ⊕ ∅ ⇒ x, ∀x" prop_c_unit_b

-- testGroup "⊥ promotion"
-- test "(⊥ [p⇋q] x) ⊕ y ⇒ ⊥ [p⇋q] (x ⊕ y)" prop_bottom_promotion_a
-- , test "x ⊕ (⊥ [p⇋q] y) ⇒ ⊥ [p⇋q] (x ⊕ y)" prop_bottom_promotion_b

-- testGroup "∅ elimination"
-- test "(x [p⇋q] ∅) ⊕ y ⇒ (x ⊕ y) [p⇋q] y" prop_unit_elimination_a
-- , test "x ⊕ (y [p⇋q] ∅) ⇒ (x ⊕ y) [p⇋q] x" prop_unit_elimination_b

-- testGroup "⊥ concatentation"
-- test "⊥ [p⇋q] (⊥ [r⇋s] x) ⇒ ⊥ [r*(p+q)+s*p⇋(p+q)*(r+s)] x"
--               prop_bottom_concatenation

-- testGroup "∅ demotion"
-- test "(x [p⇋q] ∅) [r⇋s] y ⇒ x [p*r⇋

 ]

prop_p_rule_zero_choice :: RationalDeltaQ -> QC.Gen QC.Prop
prop_p_rule_zero_choice _
  = QC.forAll tc $ \x ->
        case x of
        (ProbChoice _ _ b) -> (canonicaliseDeltaQ  x == canonicaliseDeltaQ b)
        _ -> error $ "prop_p_rule_zero_choice generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM2 (ProbChoice 0) arbitrary arbitrary

prop_p_rule_one_choice :: RationalDeltaQ -> QC.Gen QC.Prop
prop_p_rule_one_choice _
  = QC.forAll tc $ \x ->
        case x of
        (ProbChoice _ a _) -> (canonicaliseDeltaQ  x == canonicaliseDeltaQ a)
        _ -> error $ "prop_p_rule_one_choice generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM2 (ProbChoice 1) arbitrary arbitrary

prop_p_rule_bottom :: RationalDeltaQ -> QC.Gen QC.Prop
prop_p_rule_bottom _
  = QC.forAll tc $ \x ->
        case x of
        (ProbChoice _ Bottom Bottom) -> (canonicaliseDeltaQ  x == Bottom)
        _ -> error $ "prop_p_rule_bottom generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM (\y -> ProbChoice y Bottom Bottom) arbitrary

prop_p_rule_unit :: RationalDeltaQ -> QC.Gen QC.Prop
prop_p_rule_unit _
  = QC.forAll tc $ \x ->
        case x of
        (ProbChoice _ Unit Unit) -> (canonicaliseDeltaQ  x == Unit)
        _ -> error $ "prop_p_rule_unit generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM (\y -> ProbChoice y Unit Unit) arbitrary

prop_p_norm_bottom:: RationalDeltaQ -> QC.Gen QC.Prop
prop_p_norm_bottom _
  = QC.forAll tc $ \z ->
        case z of
        (ProbChoice _p _x Bottom) -> case canonicaliseDeltaQ z of
                                    (ProbChoice _ Bottom _y) -> True
                                    Bottom -> canonicaliseDeltaQ _x == Bottom
                                    _ -> False
        _ -> error $ "prop_p_norm_bottom generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM2 (\p x -> ProbChoice p x Bottom) not0or1 arbitrary
       not0or1 = arbitrary `QC.suchThat` (\p-> p > 0 && p < 1)

prop_p_norm_unit:: RationalDeltaQ -> QC.Gen QC.Prop
prop_p_norm_unit _
  = QC.forAll tc $ \z ->
        case z of
        (ProbChoice _p Unit _y)
          -> case canonicaliseDeltaQ z of
               Unit                -> canonicaliseDeltaQ _y == Unit
               (ProbChoice _ _ y ) -> endsInUnit $ canonicaliseDeltaQ y
               _z                  -> traceShow ("prob_p_norm_unit", z, _z) False
        _ -> error $ "prop_p_norm_unit generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM2 (\p y -> ProbChoice p Unit y) not0or1 arbitrary
       not0or1 = arbitrary `QC.suchThat` (\p-> p > 0 && p < 1)

       endsInUnit Unit = True
       endsInUnit (ProbChoice _ _ y) = endsInUnit y
       endsInUnit _    = False

prop_c_bottom_a:: RationalDeltaQ -> QC.Gen QC.Prop
prop_c_bottom_a _
  = QC.forAll tc $ \z ->
        case z of
        (Convolve Bottom x)
          -> case canonicaliseDeltaQ z of
              Bottom -> True
              _z     -> traceShow (name, z, _z) False
        _ -> error $ name ++ " generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM (\x -> Convolve Bottom x ) arbitrary
       name = "prop_c_bottom_a"

prop_c_bottom_b:: RationalDeltaQ -> QC.Gen QC.Prop
prop_c_bottom_b _
  = QC.forAll tc $ \z ->
        case z of
        (Convolve x Bottom)
          -> case canonicaliseDeltaQ z of
              Bottom -> True
              _z     -> traceShow (name, z, _z) False
        _ -> error $ name ++ " generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM (\x -> Convolve x Bottom ) arbitrary
       name = "prop_c_bottom_b"

prop_c_unit_a:: RationalDeltaQ -> QC.Gen QC.Prop
prop_c_unit_a _
  = QC.forAll tc $ \z ->
        case z of
        (Convolve Unit x)
          -> case canonicaliseDeltaQ z == canonicaliseDeltaQ x of
               True  -> True
               False -> traceShow (name, z, canonicaliseDeltaQ z) False
        _ -> error $ name ++ " generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM (\x -> Convolve Unit x ) arbitrary
       name = "prop_c_unit_a"

prop_c_unit_b:: RationalDeltaQ -> QC.Gen QC.Prop
prop_c_unit_b _
  = QC.forAll tc $ \z ->
        case z of
        (Convolve x Unit)
          -> case canonicaliseDeltaQ z == canonicaliseDeltaQ x of
               True  -> True
               False -> traceShow (name, z, canonicaliseDeltaQ z) False
        _ -> error $ name ++ " generator error"
    where
       tc :: QC.Gen RationalDeltaQ
       tc = liftM (\x -> Convolve x Unit) arbitrary
       name = "prop_c_unit_b"
