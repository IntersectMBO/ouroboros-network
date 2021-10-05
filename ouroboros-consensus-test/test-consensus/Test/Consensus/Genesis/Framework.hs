{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Consensus.Genesis.Framework (
    AnnotatedBlockTree (..)
  , BlockTree (..)
  , Dominated
  , NonDominated
  , SlotDelta (..)
  , isDominated
  , pathsThroughTree
  , tests
  ) where

import           Control.Monad (replicateM)
import           Data.Function (on, (&))
import qualified Data.List as L (groupBy, sortBy)
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), partition)
import           Data.Tree
import           Data.Word

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.GenesisWindowLength
import           Ouroboros.Network.AnchoredFragment hiding (head, length,
                     successorBlock)
import           Ouroboros.Network.AnchoredFragment.Completeness
import           Test.Util.TestBlock hiding (BlockTree, Header)

{-------------------------------------------------------------------------------
 Types
-------------------------------------------------------------------------------}

newtype SlotDelta = SlotDelta SlotNo
 deriving (Show, Num, Eq)

instance Arbitrary SlotDelta where
  arbitrary = SlotDelta . SlotNo <$> choose (1,2)
  shrink (SlotDelta (SlotNo s)) =
    [ SlotDelta $ SlotNo s' | s' <- shrink s ]

instance Arbitrary GenesisWindowLength where
  arbitrary = GenesisWindowLength <$> choose (3, 6)
  shrink (GenesisWindowLength s) =
    [ GenesisWindowLength s'
    | s' <- shrink s
    , s' > 2 ]

-- | A BlockTree captures a random tree, which is then turned into a Dominated
-- tree over the associated GenesisWindowLength.
data BlockTree d = BlockTree {
    tree     :: !(Tree SlotDelta)
  , original :: !(Tree SlotDelta)
  , gwl      :: !GenesisWindowLength
  }

instance Show (BlockTree d) where
  show BlockTree{..} =
    "Original: \n" <>  (drawTree . fmap show $ original) <>
    "Modified: \n" <>  (drawTree . fmap show $ tree) <>
    "GenesisWindow: \n" <> show gwl

-- | An AnnotatedBlockTree is a BlockTree that also has a FragmentCompleteness
-- tag for each one of the paths in the resulting tree.
data AnnotatedBlockTree d = AnnotatedBlockTree {
  bt          :: !(BlockTree d),
  annotations :: ![FragmentCompleteness]
  }

instance Show (AnnotatedBlockTree d) where
  show AnnotatedBlockTree{..} =
    "Original Tree:\n" <> drawTree (show <$> original bt) <>
    "Tree:\n" <> drawTree (show <$> tree bt) <>
    "Is tree dominated: "<> show (isDominated (gwl bt) (tree bt)) <> "\n" <>
    "Paths:\n" <> unlines (zipWith (curry show) (pathsThroughTree bt) annotations) <>
    "GenesisWindowLength:\n" <> show (gwl bt)

instance Arbitrary FragmentCompleteness where
  arbitrary = frequency [
    (1, return FragmentComplete),
    (1, return FragmentIncomplete)
    ]
  shrink _ = [] -- no shrinking

instance Arbitrary (BlockTree d) => Arbitrary (AnnotatedBlockTree d) where
  arbitrary = do
    bt <- arbitrary
    annotations <- replicateM (Prelude.length (pathsThroughTree bt)) arbitrary
    return AnnotatedBlockTree{..}
  shrink = const []
  -- Shrinking is quite difficult as we have to re-tag the branches.

data DecidedTree =
    -- | A children was elected.
    DNode SlotDelta DecidedTree [Tree SlotDelta]
  | -- | We reached the end of the genesis window so the remaining tree must yet
    -- be made dominated.
    DUndecided (Tree SlotDelta)
  | -- | We reached a node with no children, the decided tree is complete.
    DLeaf SlotDelta
  deriving Show

data Dominated
data NonDominated

{-------------------------------------------------------------------------------
 Dominated tree generation
-------------------------------------------------------------------------------}

instance Arbitrary (BlockTree Dominated) where
  arbitrary = do
    s <- arbitrary
    t <- arbitrary
    t' <- makeDominated s t
    return (BlockTree t' t s)

  shrink BlockTree{..} = [ BlockTree tree' original gwl'
                         | tree' <- shrinkTree tree
                         , gwl' <- shrink gwl
                         , isDominated gwl' tree'
                         ]

instance Arbitrary (BlockTree NonDominated) where
  arbitrary = do
    s <- arbitrary
    t <- arbitrary
    return (BlockTree t t s)

  shrink BlockTree{..} = [ BlockTree tree' original gwl'
                         | tree' <- shrinkTree tree
                         , gwl' <- shrink gwl
                         , isDominated gwl' tree'
                         ]

-- | Keeping the first root (could be changed but it is easier this way),
-- generate a list of trees by removing a block somewhere.
shrinkTree :: Tree SlotDelta -> [Tree SlotDelta]
shrinkTree (Node s ts) =
  concatMap (\x -> map (Node s . (map (ts !!) ([0..x-1] ++ [x+1 .. length ts -1]) ++)) $ shrinkTree' (ts !! x)) [0..length ts - 1]
shrinkTree' :: Tree SlotDelta -> [[Tree SlotDelta]]
shrinkTree' n@(Node s ts) =
  -- remove this block (propagate distance in slots to children)
  map (\n' -> n' { rootLabel = deltaToSlotDelta $ nodeDelta n  + nodeDelta n'}) ts :
  -- for all the results of removing a block in the children, add this block and
  -- the other unaltered children.
  map (\x -> map (Node s . (map (ts !!) ([0..x-1] ++ [x+1 .. length ts -1]) ++)) . shrinkTree' $ (ts !! x)) [0..length ts - 1]

{-------------------------------------------------------------------------------
 Helpers
-------------------------------------------------------------------------------}

nodeDelta :: Tree SlotDelta -> Word64
nodeDelta (Node (SlotDelta (SlotNo s)) _) = s

deltaToSlotDelta :: Word64 -> SlotDelta
deltaToSlotDelta = SlotDelta . SlotNo

consumeWindow :: GenesisWindowLength -> SlotDelta -> GenesisWindowLength
consumeWindow (GenesisWindowLength s) (SlotDelta (SlotNo s')) =
  GenesisWindowLength (s - s')

-- | Calculate the amount of blocks in the genesis window with the provided
-- length.
density :: GenesisWindowLength -> Tree SlotDelta -> Word64
density (GenesisWindowLength g) = density' g
  where
    density' gl (Node (SlotDelta (SlotNo s')) ts)
      | gl == s'
      = 1
      | s' > gl
      = 0
      | otherwise
      = case ts of
          []  -> 1
          [t] -> 1 + density' (gl - s') t
          _   -> 1 + maximum (map (density' (gl - s')) ts)

-- | Extract all the paths through a tree as anchored fragments.
pathsThroughTree :: BlockTree d -> [AnchoredFragment (Header TestBlock)]
pathsThroughTree =
      paths (Empty AnchorGenesis :>
             getHeader (TestBlock (TestHash (0 NE.:| [])) 0 True))
    . (0,)
    . tree
  where
    paths accFrag (idx, n) =
      let
        accFrag' = (accFrag :>) . getHeader $
          case accFrag of
            Empty _  -> error "unreachable"
            _ :> tip ->
              let
                sb = (successorBlock $ testHeader tip)
              in
                  sb { tbSlot = tbSlot sb + SlotNo (nodeDelta n) - 1 }
                & modifyFork (+ idx)
      in
        case subForest n of
          [] -> [accFrag']
          _  -> concatMap (paths accFrag') $ zip [0..] (subForest n)

{-------------------------------------------------------------------------------
 Check if a tree is dominated
-------------------------------------------------------------------------------}

-- | A tree is said to be dominated under a genesis window length if it contains
-- one chain which in every intersection with other chains, it wins the density
-- comparison on the window anchored at the intersection point.
--
-- TODO: maybe this is not doing as it should, because nodes are
-- indistinguishable when they have the same SlotDelta. Probably should 1)
-- annotate the tree with hashes 2) decide on window 3) extract winning anchored
-- fragment 4) extract all paths through densest branch 5) check that winning
-- fragment is part of the densest branch always.
isDominated :: GenesisWindowLength -> Tree SlotDelta -> Bool
isDominated _ (Node _ [])  = True
isDominated s (Node _ [t]) = isDominated s t
isDominated s (Node _ ts)  =
    case  L.groupBy ((==) `on` fst)
        $ L.sortBy  (flip compare `on` fst)
        $ map (\x -> (density s x, x)) ts of
      []           -> error "can't happen"
      [(_, one)]:_ -> isDominated s one && areAllPartOf one one s (genesisWindowLength s)
      _            -> False
  where
    areAllPartOf :: Tree SlotDelta -> Tree SlotDelta -> GenesisWindowLength -> Word64 -> Bool
    areAllPartOf _              (Node _ [])  _          _               = True
    areAllPartOf mustBelongIn n@(Node _ [t]) fullWindow remainingWindow =
      areAllPartOf mustBelongIn t fullWindow (remainingWindow - nodeDelta n)
    areAllPartOf mustBelongIn n@(Node _ trees) fullWindow remainingWindow =
         nodeDelta n > remainingWindow
      || case  L.groupBy ((==) `on` fst)
             $ L.sortBy  (flip compare `on` fst)
             $ map (\x -> (density fullWindow x, x)) trees of
           []           -> error "can't happen"
           [(_, one)]:_ ->
             isSubTree one mustBelongIn
             && areAllPartOf mustBelongIn one fullWindow (remainingWindow - nodeDelta n)
           _            -> False

    isSubTree :: Tree SlotDelta -> Tree SlotDelta -> Bool
    isSubTree candidate goal = candidate == goal || (case subForest goal of
                                                      [] -> False
                                                      sf -> any (isSubTree candidate) sf)

{-------------------------------------------------------------------------------
 Alter a tree to make it dominated
-------------------------------------------------------------------------------}

-- | Make the tree dominated under the given genesis window length. At every
-- intersection point, elect a candidate from the ones with highest density and
-- for the other candidates, find the chain that is as dense as the elected one
-- and remove a block on that one. Repeat recursively on the elected candidate.
makeDominated :: GenesisWindowLength -> Tree SlotDelta -> Gen (Tree SlotDelta)
makeDominated _  tr@(Node _ [])  = return tr
makeDominated genLen tr@(Node _ [t]) = do
  t' <- makeDominated genLen t
  return $ tr { subForest = [t'] }
makeDominated genLen tr =
    makeItWin [genLen] =<< decideByDensityOnWindow genLen tr
  where
    -- | Process a DecidedTree into a normal tree by removing blocks from the
    -- competitor branches that were not elected.
    makeItWin :: [GenesisWindowLength] -> DecidedTree -> Gen (Tree SlotDelta)
    makeItWin _         (DLeaf s)          = return $ Node s []
    makeItWin _         (DUndecided n)     = makeDominated genLen n
    makeItWin mustWinAt (DNode s d others) = do
         let
           mustWinAtWindows = genLen: map (`consumeWindow` s) mustWinAt
         d' <- makeItWin mustWinAtWindows d
         case others of
             [] -> return $ Node s [d']
             (t:ts) ->
               let densityWinningAtEachWindow = map (\x -> (x, x `density` d')) mustWinAtWindows
                   (okays, wrongs) =
                     NE.partition (\x -> all (\(y, z) -> density y x < z) densityWinningAtEachWindow) (t NE.:| ts)
               in
                 return $ Node s $ d' : okays ++ concatMap (removeUntilUnder densityWinningAtEachWindow) wrongs

    -- | Remove blocks from this branch until it is under the given target
    -- density.
    removeUntilUnder :: [(GenesisWindowLength, Word64)] -> Tree SlotDelta -> [Tree SlotDelta]
    removeUntilUnder mustLooseHereAgainst n =
      concatMap
      (\n' ->
          let
            newBranch = n' { rootLabel = deltaToSlotDelta $ nodeDelta n  + nodeDelta n'}
          in
            if any (\(gl, target) -> density gl newBranch>= target) mustLooseHereAgainst
            then removeUntilUnder mustLooseHereAgainst newBranch
            else [newBranch]
      )
      (subForest n)

    -- | Create a DecidedTree on this GenesisWindow. This will elect one branch
    -- if there are multiple competitors with highest density.
    decideByDensityOnWindow :: GenesisWindowLength -> Tree SlotDelta -> Gen DecidedTree
    decideByDensityOnWindow _                    (Node s []) = return $ DLeaf s
    decideByDensityOnWindow remainingWindowAfter n@(Node s [t]) =
      if nodeDelta t > genesisWindowLength remainingWindowAfter
      then
        return $ DUndecided n
      else
        (flip (DNode s) []) <$> (decideByDensityOnWindow (consumeWindow remainingWindowAfter (rootLabel t)) t)
    decideByDensityOnWindow remainingWindowAfter n@(Node s ts) =
      case   L.groupBy ((==) `on` fst)
           $ L.sortBy (flip compare `on` fst)
           $ map (\x -> (density remainingWindowAfter x, x))
             ts of
          [] -> error "can't happen because ts /= []"
          [(0,_):_] ->
            -- all with density 0, we are at the end of the window
            return $ DUndecided n
          [(_,o)]:r ->
            -- one with max and some others with less
            -- (r can't be empty because ts /= [t])
            let
              remainingWindow = consumeWindow remainingWindowAfter (rootLabel o)
            in
              flip (DNode s) (concatMap (map snd) r) <$> decideByDensityOnWindow remainingWindow o
          best:r -> do

            -- multiple with same density >0
            electedIdx <- choose(0, length best - 1)
            let
              electedBranch = snd $ best !! electedIdx
              others = map (snd . (best !!)) ([0..electedIdx - 1] ++ [electedIdx + 1..length best - 1])
              remainingWindow = consumeWindow remainingWindowAfter (rootLabel electedBranch)
            electedBranch' <- decideByDensityOnWindow remainingWindow electedBranch
            return $ DNode s electedBranch' (others ++ concatMap (map snd) r)

{-------------------------------------------------------------------------------
 Tests
-------------------------------------------------------------------------------}

prop_generator :: BlockTree Dominated -> Bool
prop_generator BlockTree{..} = isDominated gwl tree

tests :: TestTree
tests = testGroup "Dominated tree generator"
      [ testProperty "trees are dominated" prop_generator
      ]
