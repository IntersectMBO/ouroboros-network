{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.Ledger.Mock (tests)
import qualified Test.ThreadNet.BFT (tests)
import qualified Test.ThreadNet.LeaderSchedule (tests)
import qualified Test.ThreadNet.PBFT (tests)
import qualified Test.ThreadNet.Praos (tests)

import           Scratch.Praos

import qualified Criterion
import qualified Criterion.Main as Criterion
import           Test.Util.Slots (NumSlots (..))

main :: IO ()
main = defaultMain tests `asTypeOf` _main2

_main1 :: IO ()
_main1 = print $ simulateExample (NumSlots 2000)

_main2 :: IO ()
_main2 = Criterion.defaultMain [
      Criterion.bgroup "Scratch.Praos.simulateExample" $ map each rs
    ]
  where
    rs :: [NumSlots]
    rs =
        map NumSlots $
        map (*100) [1 .. 20]

    each :: NumSlots -> Criterion.Benchmark
    each r =
        Criterion.bench (show $ unNumSlots r) $
        simulateExample `Criterion.whnf` r

_tests :: TestTree
_tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.Ledger.Mock.tests
  , Test.ThreadNet.BFT.tests
  , Test.ThreadNet.LeaderSchedule.tests
  , Test.ThreadNet.PBFT.tests
  , Test.ThreadNet.Praos.tests
  ]
