{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Test.Ouroboros.Network.Utils
  ( -- * Arbitrary Delays
    Delay (..)
  , genDelayWithPrecision
  , SmallDelay (..)
    -- * Utilities
  , DistinctList (..)
  , DistinctNEList (..)
    -- * Set properties
  , isSubsetProperty
  , disjointSetsProperty
    -- * QuickCheck Utils
  , arbitrarySubset
  , shrinkVector
  , ShrinkCarefully (..)
  , prop_shrink_nonequal
  , prop_shrink_valid
    -- * Tracing Utils
  , WithName (..)
  , WithTime (..)
  , tracerWithName
  , tracerWithTime
  , tracerWithTimeName
  , swapTimeWithName
  , swapNameWithTime
  , splitWithNameTrace
    -- * Tracers
  , debugTracer
  , debugTracerG
  , sayTracer
    -- * Tasty Utils
  , nightlyTest
  , ignoreTest
    -- * Auxiliary functions
  , renderRanges
  ) where

import GHC.Real

import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTime.SI
import Control.Monad.IOSim (IOSim, traceM)
import Control.Tracer (Contravariant (contramap), Tracer (..), contramapM)

import Data.Bitraversable (bimapAccumR)
import Data.List (delete, nub)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List.Trace (Trace)
import Data.List.Trace qualified as Trace
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Text.Pretty.Simple (pPrint)
import Text.Printf

import Debug.Trace (traceShowM)
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.ExpectedFailure (ignoreTest)


newtype Delay = Delay { getDelay :: DiffTime }
  deriving Show
  deriving newtype (Eq, Ord, Num, Fractional, Real)


genDelayWithPrecision :: Integer -> Gen DiffTime
genDelayWithPrecision precision =
    sized $ \n -> do
      b <- chooseInteger (1, precision)
      a <- chooseInteger (0, toInteger n * b)
      return (fromRational (a % b))

-- | This needs to be small, as we are using real time limits in block-fetch
-- examples.
--
instance Arbitrary Delay where
    arbitrary = Delay <$> genDelayWithPrecision 10
    shrink delay | delay > 0.1 =
      takeWhile (>= 0.1) . map fromRational . shrink . toRational $ delay
    shrink _delay = []


newtype SmallDelay = SmallDelay { getSmallDelay :: DiffTime }
  deriving Show
  deriving newtype (Eq, Ord, Num, Fractional, Real)

instance Arbitrary SmallDelay where
    arbitrary = resize 5 $ SmallDelay . getDelay <$> suchThat arbitrary (\(Delay d ) -> d < 5)
    shrink delay | delay > 0.1 =
      takeWhile (>= 0.1) . map fromRational . shrink . toRational $ delay
    shrink _delay = []

-- | Pick a subset of a set, using a 50:50 chance for each set element.
--
arbitrarySubset :: Ord a => Set a -> Gen (Set a)
arbitrarySubset s = do
    picks <- vectorOf (Set.size s) (arbitrary :: Gen Bool)
    let s' = Set.fromList
           . map snd
           . filter fst
           . zip picks
           . Set.toList
           $ s
    return s'


-- | Like 'shrinkList' but only shrink the elems, don't drop elements.
--
-- Useful when you want a custom strategy for dropping elements.
--
shrinkVector :: (a -> [a]) -> [a] -> [[a]]
shrinkVector _   []     = []
shrinkVector shr (x:xs) = [ x':xs | x'  <- shr x ]
                          ++ [ x:xs' | xs' <- shrinkVector shr xs ]

newtype ShrinkCarefully a = ShrinkCarefully a
  deriving (Eq,Show)

instance (Eq a, Arbitrary a) => Arbitrary (ShrinkCarefully a) where
  arbitrary = ShrinkCarefully <$> arbitrary
  shrink (ShrinkCarefully a) = ShrinkCarefully <$> delete a (shrink a)

prop_shrink_nonequal :: (Arbitrary a, Eq a, Show a) => ShrinkCarefully a -> Property
prop_shrink_nonequal (ShrinkCarefully e) =
    whenFail (pPrint e) $
    e `notElem` shrink e

-- | Check that each shrink satisfies some invariant or validity condition.
--
prop_shrink_valid :: (Arbitrary a, Show a, Testable prop)
                  => (a -> prop) -> ShrinkCarefully a -> Property
prop_shrink_valid valid (ShrinkCarefully x) =
    conjoin [ counterexample ("shrink result invalid:\n" ++ show x') (valid x')
            | x' <- shrink x ]


-- | Use in 'tabulate' to help summarise data into buckets.
--
renderRanges :: Int -> Int -> String
renderRanges r n = "[" ++ printf "% 3d" lower ++ ", " ++ printf "% 3d" upper ++ ")"
  where
    lower = n - n `mod` r
    upper = lower + (r-1)

--
-- Set properties
--

isSubsetProperty :: (Ord a, Show a) => String -> Set a -> Set a -> Property
isSubsetProperty name a b =
    let d = a Set.\\ b
    in counterexample
        (name ++ "violates subset property: " ++ show d)
        (Set.null d)

disjointSetsProperty :: (Ord a, Show a) => String -> Set a -> Set a -> Property
disjointSetsProperty name a b =
    let d = a `Set.intersection` b
    in counterexample
        (name ++ "vaiolates disjoint set property: " ++ show d)
        (Set.null d)

--
-- Tracing tools
--

data WithName name event = WithName {
    wnName  :: name,
    wnEvent :: event
  }
  deriving Functor

instance (Show name, Show event) => Show (WithName name event) where
  show (WithName name ev) = "#" <> show name <> " % " <> show ev

data WithTime event = WithTime {
    wtTime  :: Time,
    wtEvent :: event
  }
  deriving Functor

instance Show event => Show (WithTime event) where
  show (WithTime (Time t) ev) = show t <> " @ " <> show ev

tracerWithName :: name -> Tracer m (WithName name a) -> Tracer m a
tracerWithName name = contramap (WithName name)

tracerWithTime :: MonadMonotonicTime m => Tracer m (WithTime a) -> Tracer m a
tracerWithTime = contramapM $ \a -> flip WithTime a <$> getMonotonicTime

tracerWithTimeName :: MonadMonotonicTime m
                   => name
                   -> Tracer m (WithTime (WithName name a))
                   -> Tracer m a
tracerWithTimeName name =
  contramapM $ \a -> flip WithTime (WithName name a) <$> getMonotonicTime

swapNameWithTime :: WithName name (WithTime b) -> WithTime (WithName name b)
swapNameWithTime (WithName name (WithTime t b)) = WithTime t (WithName name b)

swapTimeWithName :: WithTime (WithName name b) -> WithName name (WithTime b)
swapTimeWithName (WithTime t (WithName name b)) = WithName name (WithTime t b)

-- | Split Trace events into separate traces indexed by a given name.
--
splitWithNameTrace :: Ord name
                   => Trace r (WithName name b)
                   -> Trace r [WithName name b]
splitWithNameTrace =
    fmap fromJust
  . Trace.filter isJust
  -- there might be some connections in the state, push them onto the 'Trace'
  . (\(s, o) -> foldr (\a as -> Trace.Cons (Just a) as) o (Map.elems s))
  . bimapAccumR
      ( \ s a -> (s, a))
      ( \ s wn@(WithName name _) ->
        ( Map.alter
            ( \ case
                 Nothing  -> Just [wn]
                 Just wns -> Just (wn : wns)
            ) name s
        , Nothing
        )
      )
      Map.empty


newtype DistinctList a = DistinctList { fromDistinctList :: [a] }
  deriving Show

instance (Eq a, Arbitrary a) => Arbitrary (DistinctList a) where
  arbitrary = DistinctList . nub <$> arbitrary

  shrink (DistinctList xs) =
    [ DistinctList (nub xs') | xs' <- shrink xs ]

newtype DistinctNEList a = DistinctNEList { fromDistinctNEList :: NonEmpty a }
  deriving Show

instance (Eq a, Arbitrary a) => Arbitrary (DistinctNEList a) where
  arbitrary = DistinctNEList . NE.fromList . nub <$> listOf1 arbitrary

  shrink (DistinctNEList xs) =
    [ DistinctNEList (NE.fromList (nub xs')) | xs' <- shrink (NE.toList xs), not (null xs') ]

--
-- Debugging tools
--

debugTracer :: ( Show a, Applicative m) => Tracer m a
debugTracer = Tracer traceShowM

sayTracer :: ( Show a, MonadSay m) => Tracer m a
sayTracer = Tracer (say . show)

-- | Redefine this tracer to get valuable tracing information from various
-- components:
--
-- * connection-manager
-- * inbound governor
-- * server
--
debugTracerG :: (Show a, Typeable a) => Tracer (IOSim s) a
debugTracerG =    Tracer (\msg -> getCurrentTime >>= say . show . (,msg))
               <> Tracer traceM
            -- <> Tracer Debug.traceShowM

--
-- Nightly tests
--

nightlyTest :: TestTree -> TestTree
nightlyTest =
#ifndef NIGHTLY
  ignoreTest
#else
  id
#endif
