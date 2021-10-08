{-# LANGUAGE RecordWildCards #-}

-- | A one-to-one transcript of the maxvalid-bg rule described in the report.
module Test.Consensus.Genesis.Paper where

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

import           Data.Maybe (catMaybes, fromMaybe)
import           Test.Consensus.Genesis.Framework
import           Test.Util.TestBlock (TestBlock)

import           Ouroboros.Consensus.NodeKernel.Genesis.Impl
import           Ouroboros.Consensus.NodeKernel.Genesis.Spec

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

-- | Fold version == spec?
prop_fold :: AnnotatedBlockTree Dominated -> Property
prop_fold AnnotatedBlockTree{..} =
  let paths = pathsThroughTree bt
      maybeKnown = zipWith AnnotatedAnchoredFragment paths annotations
      gb = GenesisBlockchain (gwl bt) (anchoredFragmentsToTree maybeKnown)
  in
        prefixSelection GenesisPoint (gwl bt) maybeKnown
    === unitaryTreeToAnchoredFragment (genesisFoldPrefixSelection gb)

-- | If we run prefix selection claiming that some of the fragments are
-- incomplete, the returned fragment must be a prefix of what prefix selection
-- returns if we claim that every fragment is complete.
prop_contained_dominated :: AnnotatedBlockTree Dominated -> Property
prop_contained_dominated AnnotatedBlockTree{..} =
  let paths = pathsThroughTree bt
      known = map (`AnnotatedAnchoredFragment` FragmentComplete) paths
      a1 = prefixSelection GenesisPoint (gwl bt) known
      maybeKnown = zipWith AnnotatedAnchoredFragment paths annotations
      a2 = prefixSelection GenesisPoint (gwl bt) maybeKnown
  in
     counterexample (show a1 <> " /= " <> show a2) $
     label (if Prelude.length paths > 1 then "non-trivial" else "trivial") $
     isPrefixOf a2 a1

-- | Maxvalid_bg and prefix selection must return the same result when we claim
-- that every fragment is complete.
prop_all_complete_dominated :: BlockTree Dominated -> Property
prop_all_complete_dominated b =
  let pathsForMaxvalid = pathsThroughTree b
      pathsForPrefixSelection =
        map (`AnnotatedAnchoredFragment` FragmentComplete) (pathsThroughTree b)
  in
    label (if Prelude.length pathsForMaxvalid > 1 then "non-trivial" else "trivial") $
        maxvalidBg                   (gwl b) pathsForMaxvalid
    === prefixSelection GenesisPoint (gwl b) pathsForPrefixSelection

-- | If we run prefix selection claiming that some of the fragments are
-- incomplete, the returned fragment must be a prefix of what prefix selection
-- returns if we claim that every fragment is complete.
prop_contained :: AnnotatedBlockTree NonDominated -> Property
prop_contained AnnotatedBlockTree{..} =
  let paths = pathsThroughTree bt
      known = map (`AnnotatedAnchoredFragment` FragmentComplete) paths
      a1 = prefixSelection GenesisPoint (gwl bt) known
      maybeKnown = zipWith AnnotatedAnchoredFragment paths annotations
      a2 = prefixSelection GenesisPoint (gwl bt) maybeKnown
      alongA2 = case intersect a1 a2 of
          Nothing -> error "can't happen"
          Just (_, _, _, a2') ->
              catMaybes [ (\(_,_,p',_) -> AnnotatedAnchoredFragment p' a) <$> intersect p a2' | AnnotatedAnchoredFragment p a <- maybeKnown ]
  in
     counterexample (show a1 <> " /= " <> show a2) $
     label ((if isDominated (gwl bt) (tree bt) then "Dominated" else "NonDominated") <>
            "_" <>
            (if Prelude.length paths > 1 then "non-trivial" else "trivial")) $
     (property $ isPrefixOf a2 a1) .||. case intersect a1 a2 of
                                          Nothing -> property False
                                          Just (_, x, _, _) -> property $ any (not . hasKnownDensityAt (gwTo $ (genesisWindowBounds (headSlot x) (gwl bt)))) alongA2

-- -- | Maxvalid_bg and prefix selection must return the same result when we claim
-- -- that every fragment is complete.
-- prop_all_complete :: BlockTree NonDominated -> Property
-- prop_all_complete b =
--   let pathsForMaxvalid = pathsThroughTree b
--       pathsForPrefixSelection =
--         map (`AnnotatedAnchoredFragment` FragmentComplete)  (pathsThroughTree b)
--   in
--     isDominated (gwl b) (tree b) && (Prelude.length pathsForMaxvalid > 1) ==>
--     label (if isDominated (gwl b) (tree b) then "Dominated" else "NonDominated") $
--         if isDominated (gwl b) (tree b)
--         then
--                 maxvalidBg                  (gwl b) pathsForMaxvalid
--             === prefixSelection GenesisPoint (gwl b) pathsForPrefixSelection
--         else
--             let a1 = maxvalidBg                  (gwl b) pathsForMaxvalid
--                 a2 = prefixSelection GenesisPoint (gwl b) pathsForPrefixSelection
--             in
--               case intersect a1 a2 of
--                 Nothing -> property False
--                 Just (_, x, a1', a2') -> density (genesisWindowBounds (headSlot x) (gwl b)) a1'
--                                      === density (genesisWindowBounds (headSlot x) (gwl b)) a2'

tests :: TestTree
tests = testGroup "Prefix Selection"
      [ testProperty "Fold version == spec" prop_fold
      , testGroup "Dominated"
        [ testProperty "Prefix Selection vs Maxvalid_bg" prop_all_complete_dominated
        , testProperty "Unknown is always prefix of known" prop_contained_dominated
        ]
      , testGroup "Random"
      --   [ testProperty "Prefix Selection vs Maxvalid_bg" prop_all_complete
         [testProperty "Unknown is prefix of known OR there are incompletes in the window" prop_contained]
      --   ]
      ]
