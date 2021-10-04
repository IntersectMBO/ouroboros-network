{-# LANGUAGE RecordWildCards #-}

-- | A one-to-one transcript of the maxvalid-bg rule described in the report.
module Test.Consensus.Genesis.Paper (Test.Consensus.Genesis.Paper.tests) where

import           Prelude hiding (length)
import qualified Prelude (length)

import           Data.Foldable (foldl')

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Block.Abstract (Header)
import           Ouroboros.Consensus.Config.GenesisWindowLength
import           Ouroboros.Consensus.NodeKernel.Genesis
import           Ouroboros.Network.AnchoredFragment hiding (HasHeader,
                     getHeaderFields, null)
import           Ouroboros.Network.AnchoredFragment.Completeness
import           Ouroboros.Network.Block

import           Data.Maybe (fromMaybe)
import           Test.Consensus.Genesis.Framework
import           Test.Util.TestBlock (TestBlock)

{-------------------------------------------------------------------------------
 Maxvalid_bg. See Definition 21.2 in the Consensus report.
-------------------------------------------------------------------------------}
maxvalidBg ::
     GenesisWindowLength
  -> [AnchoredFragment (Header TestBlock)]
  ->  AnchoredFragment (Header TestBlock)
maxvalidBg _ []     = error "Can't decide on empty list of chains"
maxvalidBg s (f:fs) = res
  where
    res = foldl' maxvalid_bg' f fs
    maxvalid_bg' c_max c_i =
      let
        (_, prev, c_max'2, c_i'2) =
                  fromMaybe (error "Must not happen") $ intersect c_max c_i
      in
        if   density (genesisWindowBounds (headSlot prev) s) c_i'2
           > density (genesisWindowBounds (headSlot prev) s) c_max'2
        then c_i
        else c_max

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | If we run prefix selection claiming that some of the fragments are
-- incomplete, the returned fragment must be a prefix of what prefix selection
-- returns if we claim that every fragment is complete.
prop_contained :: AnnotatedBlockTree -> Property
prop_contained AnnotatedBlockTree{..} =
  let paths = pathsThroughTree bt
      known = map (`AnnotatedAnchoredFragment` FragmentComplete) paths
      a1 = prefixSelection GenesisPoint (gwl bt) known
      maybeKnown = zipWith AnnotatedAnchoredFragment paths annotations
      a2 = prefixSelection GenesisPoint (gwl bt) maybeKnown
  in
     counterexample (show a1 <> " /= " <> show a2) $
     isDominated (gwl bt) (tree bt) && (Prelude.length paths > 1) ==>
     cover 99 (Prelude.length paths > 1) "non-trivial" $
     isPrefixOf a2 a1

-- | Maxvalid_bg and prefix selection must return the same result when we claim
-- that every fragment is complete.
prop_all_complete :: BlockTree -> Property
prop_all_complete b =
  let pathsForMaxvalid = pathsThroughTree b
      pathsForPrefixSelection =
        map (`AnnotatedAnchoredFragment` FragmentComplete)  (pathsThroughTree b)
  in
    isDominated (gwl b) (tree b) && (Prelude.length pathsForMaxvalid > 1) ==>
    cover 99 (Prelude.length pathsForMaxvalid > 1) "non-trivial" $
        maxvalidBg                  (gwl b) pathsForMaxvalid
    === prefixSelection GenesisPoint (gwl b) pathsForPrefixSelection

tests :: TestTree
tests = testGroup "Prefix Selection"
      [ testProperty "Prefix Selection vs Maxvalid_bg" prop_all_complete
      , testProperty "Unknown is always prefix of known" prop_contained
      ]

