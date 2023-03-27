{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.Honest (
    -- * Re-use
    TestHonest (TestHonest, testAsc, testRecipe, testRecipe')
  , genKSD
  , sized1
  , unlines'
  , unsafeMapSuchThatJust
    -- * Tests
  , tests
  ) where

import qualified Control.Exception as IO (evaluate)
import qualified Control.Monad.Except as Exn
import           Data.Complex (imagPart, magnitude, realPart)
import           Data.Functor ((<&>))
import           Data.Functor.Identity (runIdentity)
import           Data.Proxy (Proxy (Proxy))
import           Data.Word (Word8)
import qualified Numeric.LinearAlgebra as Mat
import qualified System.Random as R
import qualified System.Timeout as IO (timeout)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Honest as H
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc,
                     Delta (Delta), Kcp (Kcp), Len (Len), Scg (Scg),
                     ascFromBits, ascVal)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random (QCGen)
import qualified Test.Tasty as TT
import qualified Test.Tasty.QuickCheck as TT

-----

tests :: [TT.TestTree]
tests = [
    TT.testProperty "prop_honestChain" prop_honestChain
  ,
    TT.testProperty "prop_honestChainMutation" prop_honestChainMutation
  ,
    TT.testProperty "prop_transitionMatrix" prop_transitionMatrix
  ]

-----

sized1 :: (Int -> QC.Gen a) -> QC.Gen a
sized1 f = QC.sized (f . succ)

-- | A generator that checks its own satisfaction
--
-- WARNING: 'QC.suchThat' et al often causes a /very/ confusing
-- non-termination when its argument is impossible/extremely unlikely
unsafeMapSuchThatJust :: QC.Gen (Maybe a) -> QC.Gen a
unsafeMapSuchThatJust m = QC.suchThatMap m id

-----

data TestHonest = TestHonest {
    testAsc     :: !Asc
  ,
    testRecipe  :: !H.HonestRecipe
  ,
    testRecipe' :: !H.SomeCheckedHonestRecipe
  }
  deriving (Read, Show)

genKSD :: QC.Gen (Kcp, Scg, Delta)
genKSD = sized1 $ \sz -> do
    d <- QC.choose (0, div sz 4)
    k <- QC.choose (0, 2 * sz)
    s <- (\x -> x + d + k) <$> QC.choose (1, 3 * sz)   -- ensures @k + 1 / s - d <= 1@
    pure (Kcp k, Scg s, Delta d)

instance QC.Arbitrary TestHonest where
    arbitrary = sized1 $ \sz -> do
        testAsc <- ascFromBits <$> QC.choose (1 :: Word8, maxBound - 1)

        testRecipe <- do
            (kcp, Scg s, delta) <- genKSD

            -- s <= l, most of the time
            l <- QC.frequency [(9, (+ s) <$> QC.choose (0, 5 * sz)), (1, QC.choose (1, s))]

            pure $ H.HonestRecipe kcp (Scg s) delta (Len l)

        testRecipe' <- case Exn.runExcept $ H.checkHonestRecipe testRecipe of
            Left e  -> error $ "impossible! " <> show (testRecipe, e)
            Right x -> pure x

        pure TestHonest {
            testAsc
          ,
            testRecipe
          ,
            testRecipe'
          }

-- | No seed exists such that each 'H.checkHonestChain' rejects the result of 'H.uniformTheHonestChain'
prop_honestChain :: TestHonest -> QCGen -> QC.Property
prop_honestChain testHonest testSeed = runIdentity $ do
    H.SomeCheckedHonestRecipe Proxy Proxy recipe' <- pure testRecipe'

    let sched = H.uniformTheHonestChain (Just testAsc) recipe' testSeed

    QC.counterexample (unlines' $ H.prettyChainSchedule sched "H") <$> do
        pure $ case Exn.runExcept $ H.checkHonestChain testRecipe sched of
            Right () -> QC.property ()
            Left e   -> case e of
                H.BadCount{}      -> QC.counterexample (show e) False
                H.BadLength{}     -> QC.counterexample (show e) False
                H.BadEhcgWindow v ->
                    let str = case v of
                            H.EhcgViolation {
                                H.ehcgvWindow = win
                              } -> H.prettyWindow win "EHCGV"
                    in
                        id
                      $ QC.counterexample str
                      $ QC.counterexample (show e)
                      $ False
  where
    TestHonest {
        testAsc
      ,
        testRecipe
      ,
        testRecipe'
      } = testHonest

unlines' :: [String] -> String
unlines' []     = ""
unlines' [x]    = x
unlines' (x:xs) = x <> "\n" <> unlines' xs

-----

-- | A mutation that minimally increases the threshold density of an 'H.HonestRecipe''s EHCG constraint
data HonestMutation =
    -- | Increasing 'Delta' by one decreases the EHCG denominator
    HonestMutateDelta
  |
    -- | Increasing 'Kcp' by one increases the EHCG numerator
    HonestMutateKcp
  |
    -- | Decreasing 'Scg' by one decreases the EHCG denominator
    HonestMutateScg
  deriving (Eq, Read, Show)

data TestHonestMutation =
    TestHonestMutation
        !H.HonestRecipe
        !H.SomeCheckedHonestRecipe
        !HonestMutation
  deriving (Read, Show)

mutateHonest :: H.HonestRecipe -> HonestMutation -> H.HonestRecipe
mutateHonest recipe mut =
    H.HonestRecipe (Kcp k') (Scg s') (Delta d') len
  where
    H.HonestRecipe (Kcp k) (Scg s) (Delta d) len = recipe

    (k', s', d') = case mut of
        HonestMutateDelta -> (k,     s,     d + 1)
        HonestMutateKcp   -> (k + 1, s,     d    )
        HonestMutateScg   -> (k,     s - 1, d    )

instance QC.Arbitrary TestHonestMutation where
    arbitrary = sized1 $ \sz -> unsafeMapSuchThatJust $ do
        (kcp, Scg s, delta) <- genKSD
        l <- (+ s) <$> QC.choose (0, 5 * sz)

        let testRecipe = H.HonestRecipe kcp (Scg s) delta (Len l)

        testRecipe' <- case Exn.runExcept $ H.checkHonestRecipe testRecipe of
            Left e  -> error $ "impossible! " <> show (testRecipe, e)
            Right x -> pure x

        mut <- QC.elements [HonestMutateDelta, HonestMutateKcp, HonestMutateScg]

        pure $ case Exn.runExcept $ H.checkHonestRecipe $ mutateHonest testRecipe mut of
            Left{}  -> Nothing
            Right{} -> Just $ TestHonestMutation testRecipe testRecipe' mut

-- | There exists a seed such that each 'TestHonestMutation' causes
-- 'H.checkHonestChain' to reject the result of 'H.uniformTheHonestChain'
prop_honestChainMutation :: TestHonestMutation -> QCGen -> QC.Property
prop_honestChainMutation testHonestMut testSeedsSeed0 = QC.ioProperty $ do
    H.SomeCheckedHonestRecipe Proxy Proxy recipe' <- pure someRecipe'

    -- we're willing to wait up to 500ms to find a failure for each 'TestHonestMutation'
    IO.timeout
        (5 * 10^(5::Int))
        (IO.evaluate $ go recipe' testSeedsSeed0) <&> \case
            Nothing   -> False   -- did not find a failure caused by the mutation
            Just bool -> bool
  where
    TestHonestMutation recipe someRecipe' mut = testHonestMut

    mutatedRecipe = mutateHonest recipe mut

    go recipe' testSeedsSeed =
        let -- TODO is this a low quality random stream? Why is there no @'R.Random' 'QCGen'@ instance?
            (testSeed, testSeedsSeed') = R.split testSeedsSeed

            sched = H.uniformTheHonestChain Nothing recipe' (testSeed :: QCGen)
            m     = H.checkHonestChain mutatedRecipe sched
        in
        case Exn.runExcept m of
            Right () -> go recipe' testSeedsSeed'
            Left e   -> case e of
                H.BadCount{}      -> error $ "impossible! " <> show e
                H.BadEhcgWindow{} -> True
                H.BadLength{}     -> error $ "impossible! " <> show e

-----

data TestSetupTransitionMatrix = TestSetupTransitionMatrix !Asc !Int !Int
  deriving (Show)

instance QC.Arbitrary TestSetupTransitionMatrix where
    arbitrary = do
        asc <- ascFromBits <$> QC.choose (1 :: Word8, maxBound - 1)

        d <- QC.choose (1, 100)

        n <- QC.choose (1, d)

        pure $ TestSetupTransitionMatrix asc n d

prop_transitionMatrix :: TestSetupTransitionMatrix -> QC.Property
prop_transitionMatrix testSetup =
    checkStochastic QC..&&. checkStationary
  where
    TestSetupTransitionMatrix asc n d = testSetup

    p = H.transitionMatrix (ascVal asc) n d

    -- a reasonable tolerance since 'Asc' is no finer than 1/256 and 1/d is no finer than 1/100
    tol = 1e-6

    checkStochastic =
        QC.conjoin [
                QC.counterexample ("row " <> show i <> " does not sum to 1: " <> show x)
              $ abs (x - 1) < tol
          | i <- [0 .. Mat.rows p - 1]
          , let x = Mat.sumElements (p Mat.! i)   -- the bang operator extracts /rows/ in @hmatrix@
          ]

    checkStationary =
        let vals = Mat.eigenvalues p
            maxi = Mat.maxIndex vals
            maxv = vals Mat.! maxi

            exists =
                QC.counterexample ("max eigenvalue is not 1: " <> show maxv)
              $ imagPart maxv QC.=== 0 QC..&&. (realPart maxv - 1) <= tol

            -- vals with the greatest value set to 0
            vals2 = vals * mask   --  this * is pointwise
            mask  = Mat.size vals Mat.|> (replicate maxi 1 <> (0 : let x = 1 : x in x))
            maxv2 = vals2 Mat.! Mat.maxIndex vals2

            unique =
                QC.counterexample
                    (   "second greatest eigenvalue is within tolerance of greatest eigenvalue: "
                     <> show (tol, maxv, maxv2)
                    )
              $ realPart maxv - abs (magnitude maxv2) > tol
        in
        exists QC..&&. unique
