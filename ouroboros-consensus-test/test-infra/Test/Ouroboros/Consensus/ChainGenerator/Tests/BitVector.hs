{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.BitVector (
  tests,
  ) where

import           Data.Monoid (Endo (Endo, appEndo))
import qualified Data.Vector.Unboxed as V
import           GHC.Generics(Generic)
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (E (EmptySlotE, SlotE), POL, PreImage, S)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some
import qualified Test.QuickCheck as QC
import qualified Test.Tasty as TT
import qualified Test.Tasty.QuickCheck as TT

-----

tests :: [TT.TestTree]
tests = [
    TT.testProperty "prop_findIthZeroInV" prop_findIthZeroInV
  ]

-----

data SomeFindTestSetup = forall pol. TestPOL pol => SomeFindTestSetup (FindTestSetup pol)

instance Show SomeFindTestSetup where
    showsPrec p (SomeFindTestSetup testSetup) =
        showParen (p >= 11)
      $ showString "SomeFindTestSetup " . showsPrec 11 testSetup

data ProxyPol (pol :: S.Pol) = ProxyPol
  deriving (Eq)

instance TestPOL pol => Show (ProxyPol pol) where
    showsPrec p pol =
        Some.runShowsPrec p
      $ Some.showCtor pol ("ProxyPol :: ProxyPol " <> showPol pol)

instance TestPOL pol => Read (ProxyPol pol) where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor pol (show pol)
      where
        pol = ProxyPol :: ProxyPol pol

data TestB

instance QC.Arbitrary (ProxyPol pol) where
    arbitrary = pure ProxyPol
    shrink    = QC.shrinkNothing

data FindTestSetup (pol :: S.Pol) = FindTestSetup {
    testPol   :: ProxyPol pol
  ,
    -- | INVARIANT @'testIndex' < V.sum (V.map ('countZeros' 'testPol') 'testV')@
    testIndex :: C.Index TestB (PreImage pol EmptySlotE)
  ,
    testV     :: C.Vector TestB SlotE S
  }
  deriving (Eq, Generic, Read, Show)

instance QC.Arbitrary SomeFindTestSetup where
    arbitrary = do
        pol <- QC.arbitrary
        case pol of
            True  -> SomeFindTestSetup <$> (QC.arbitrary :: QC.Gen (FindTestSetup S.Inverted   ))
            False -> SomeFindTestSetup <$> (QC.arbitrary :: QC.Gen (FindTestSetup S.NotInverted))

    shrink (SomeFindTestSetup x) = [ SomeFindTestSetup y | y <- QC.shrink x ]

instance TestPOL pol => QC.Arbitrary (FindTestSetup pol) where
    arbitrary = do
        let testPol = ProxyPol :: ProxyPol pol

        v <- V.fromList <$> QC.arbitrary

        let tc = targetCount testPol v
        i <- if 0 == tc then QC.discard else QC.choose (0, tc - 1)

        pure FindTestSetup {
            testPol
          ,
            testIndex = C.Count i
          ,
            testV     = C.Vector v
          }

    shrink x =
        [ y
        | y <- QC.genericShrink x
        , let FindTestSetup {testIndex, testPol, testV} = y
        , C.getCount testIndex < targetCount testPol (C.getVector testV)
        ]

targetCount :: TestPOL pol => proxy pol -> V.Vector S -> Int
targetCount pol = V.length . V.filter (not . S.test pol)

-----

class POL pol => TestPOL (pol :: S.Pol) where showPol :: proxy pol -> String
instance TestPOL S.Inverted             where showPol _pol = "Inverted"
instance TestPOL S.NotInverted          where showPol _pol = "NotInverted"

prop_findIthZeroInV :: SomeFindTestSetup -> QC.Property
prop_findIthZeroInV testSetup = case testSetup of
    SomeFindTestSetup FindTestSetup {
        testIndex
      ,
        testPol
      ,
        testV
      } -> case BV.findIthEmptyInV testPol testV testIndex of
        BV.NothingFound -> QC.counterexample "NothingFound" False
        BV.JustFound i  ->
              id
            $ QC.counterexample (showPol testPol)
            $ QC.counterexample
                (let C.Vector v = testV
                 in
                 "v =" <> V.foldMap (Endo . S.showS) v `appEndo` ""
                )
            $ QC.counterexample ("i = " <> show i)
            $
               (   id
                 $ QC.counterexample "i isn't the position of a post-polarization-0 in w!"
                 $ BV.testV testPol testV i QC.=== False
               )
              QC..&&.
                  (   let targetsInPrecedingWords =
                              targetCount testPol $ V.take (C.getCount i) $ C.getVector testV
                      in
                        id
                      $ QC.counterexample "There are not testIndex-many post-polarization-0s preceding i!"
                      $ targetsInPrecedingWords QC.=== C.getCount testIndex
                  )
