{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.NodeKernel.Genesis.Impl where

import           Prelude hiding (last, length)
import qualified Prelude (last)

import           Control.Exception (assert)
import           Data.Function (on)
import           Data.List (foldl', groupBy, partition, sortBy)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (catMaybes)
import           Data.Word

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.GenesisWindowLength
import           Ouroboros.Network.AnchoredFragment hiding (filter, null)
import           Ouroboros.Network.AnchoredFragment.Completeness

{-------------------------------------------------------------------------------
 Genesis Trees
-------------------------------------------------------------------------------}

data UnfinishedBranch = IncompleteBranch | NA deriving (Show, Eq)

-- | A GenesisTree is equivalent to a Data.Tree but captures the fact that the
-- root is an @Anchor blk@.
data GenesisTree blk =
    GRoot (Anchor blk) [GenesisTree blk]
  | GNode blk UnfinishedBranch (NE.NonEmpty (GenesisTree blk))
  | GLeaf blk FragmentCompleteness
  deriving (Show)

bk :: GenesisTree blk -> blk
bk (GRoot _ _)   = error "Can't call bk on a root"
bk (GNode b _ _) = b
bk (GLeaf b _)   = b

-- | Given two unitary trees, concatenate the second at the end of the first
concatTrees :: GenesisTree blk -> GenesisTree blk -> GenesisTree blk
concatTrees (GRoot a   [])               ts = GRoot a [ts]
concatTrees (GRoot a   [t])              ts = GRoot a [concatTrees t ts]
concatTrees (GNode b f (t NE.:| []))     ts = GNode b f                (concatTrees t ts NE.:| [])
concatTrees (GLeaf b FragmentComplete)   ts = GNode b NA               (ts NE.:| [])
concatTrees (GLeaf b FragmentIncomplete) ts = GNode b IncompleteBranch (ts NE.:| [])
concatTrees _ _ = error "Precondition violated: concatTrees on non-trivial tree"

{-------------------------------------------------------------------------------
 Genesis Paths
-------------------------------------------------------------------------------}

-- | A sequence of blocks representing a path on a tree.
data GenesisPath blk =
    GPathLeaf blk FragmentCompleteness
  | GPath     blk (GenesisPath blk)

hd :: GenesisPath blk -> blk
hd (GPathLeaf b _) = b
hd (GPath     b _) = b

pathToUnitaryTree :: GenesisPath blk -> GenesisTree blk
pathToUnitaryTree (GPathLeaf b f) = GLeaf b f
pathToUnitaryTree (GPath     b n) = GNode b NA (pathToUnitaryTree n NE.:| [])

unitaryTreeToAnchoredFragment ::
  HasHeader blk
  => GenesisTree blk
  -> AnchoredFragment blk
unitaryTreeToAnchoredFragment = unitaryTreeToAnchoredFragment' undefined
  where
    unitaryTreeToAnchoredFragment' _ (GRoot a [n])            = unitaryTreeToAnchoredFragment' (Empty a :>) n
    unitaryTreeToAnchoredFragment' f (GNode b _ (n NE.:| [])) = unitaryTreeToAnchoredFragment' (f b :>) n
    unitaryTreeToAnchoredFragment' f (GLeaf b _)              = f b
    unitaryTreeToAnchoredFragment' _ _ =
      error "Precondition violated: called unitaryTreeToAnchoredFragment with non-trivial tree"

{-------------------------------------------------------------------------------
 Convert a list of AnchoredFragments into a GenesisTree
-------------------------------------------------------------------------------}

-- | Convert a list of paths through a tree into a tree
anchoredFragmentsToTree ::
     forall blk . HasHeader blk
  => [AnnotatedAnchoredFragment blk]
  -> GenesisTree blk
anchoredFragmentsToTree [] = error "Precondition violated: pathsToTree was given an empty list"
anchoredFragmentsToTree paths@(path:_) =
    -- Initially they all must be anchored at the same point
    assert (all (\x -> anchor (fragment x) == anchor (fragment path)) paths) $
    let
      root = GRoot (anchor (fragment path)) []
      fragmentToPath af =
        case toOldestFirst (fragment af) of
          [] -> Nothing
          bs -> Just
            $ foldr (\case
                         (GPathLeaf zz _) -> \acc ->  GPath zz acc
                         _                -> error "Can't happen")
                    (GPathLeaf (Prelude.last bs) (completeness af))
                    (map (`GPathLeaf` FragmentComplete) $ init bs)
      rest = catMaybes $ map fragmentToPath paths
    in
      -- We are going to fold through the list of paths adding them to the tree
      foldl' (\case
                 (GRoot r c) -> addToTree (\c' _ -> GRoot r c') c
                 _           -> error "Can't happen")
             root
             rest
  where
    addToTree ::
         (   [GenesisTree blk]
          -> (UnfinishedBranch -> UnfinishedBranch)
          -> GenesisTree blk
         ) -- ^ Update the parent node
      ->  [GenesisTree blk]
      ->   GenesisPath blk
      ->   GenesisTree blk
    addToTree upd children p =
      case children of
        [] ->
          -- if the parent node doesn't have any children, then add this unitary
          -- path.
          upd [pathToUnitaryTree p] id
        _ ->
          if not $ blockPoint (hd p) `elem` map (blockPoint . bk) children
          then
            -- if the parent node doesn't have a children that matches this one,
            -- then add this unitary path.
            upd (pathToUnitaryTree p : children) id
          else
            case p of
              GPathLeaf _ FragmentComplete ->
                -- if the path ends here, then return id.
                upd children id
              GPathLeaf _ FragmentIncomplete ->
                -- if the path ends here *AND* is incomplete, then tag the
                -- parent as Incomplete.
                upd children (\case
                                 NA -> IncompleteBranch
                                 f  -> f)
              GPath i n ->
                 -- if the path continues, find the relevant child and continue there
                 let (found, others) = partition (\x -> blockPoint (bk x) == blockPoint i) children
                 in
                   case found of
                     [GNode b f c] ->
                       -- if we found one children that continues, keep recursing
                       let upd' c' f' = upd (GNode b (f' f) (NE.fromList c') : others) id in
                         addToTree upd' (NE.toList c) n
                     [GLeaf b f] ->
                       -- if we found one children that ends here, add this as
                       -- unitary path. If it was already unfinished, then keep
                       -- it. Otherwise, it will be finished trivially.
                       -- Note: c' will be necessarily non-empty.
                       let
                         f' = if f == FragmentIncomplete then IncompleteBranch else NA
                         upd' c' _ = upd (GNode b f' (NE.fromList c') : others) id
                       in
                         (addToTree upd' [] n)
                     _           -> error "Can't happen"

{-------------------------------------------------------------------------------
 GenesisBlockchain
-------------------------------------------------------------------------------}
-- | The representation of all the information required to process a tree with
-- maxvalid_bg or equivalent algorithms (prefixSelection).
data GenesisBlockchain blk = GenesisBlockchain {
     s :: GenesisWindowLength
   , t :: GenesisTree blk
   } deriving (Show)

{-------------------------------------------------------------------------------
 foldWithCommonPrefixStep
-------------------------------------------------------------------------------}

-- | A fold over a tree that combines results at the branching points.
foldWithCommonPrefixStep ::
     forall a blk b . HasHeader blk
  => (   ([GenesisTree blk], SlotNo)
      -> Maybe (GenesisTree blk, b)
     ) -- ^ Picker function. Chooses a branch to continue plus some metadata
     -- required for the update function or short-circuits on @Nothing@.
  -> (a -> (GenesisTree blk, b) -> Either a a)
     -- ^ Updater function. updates the state with the chosen unique
     -- prefix. Short-circuits on @Left@.
  -> (GenesisTree blk -> a)
     -- ^ Initiator
  -> GenesisTree blk
     -- ^ Tree to fold
  -> a
foldWithCommonPrefixStep picker updater initial theTree =
    let
      (initialPrefix, sl, initialChildren) = splitAtIntersection theTree
      initialState = initial initialPrefix
    in
      foldWithCommonPrefixStep' (initialChildren, sl) initialState
  where
    foldWithCommonPrefixStep' tr st =
       case picker tr of
         Nothing  -> st
         Just (n, b)   ->
           let (n', sl', ch) = splitAtIntersection n
           in either id (foldWithCommonPrefixStep' (ch, sl')) $ updater st (n', b)

-- | Find the next intersection and split the tree in
-- 1) The common prefix
-- 2) The slot number at the intersection
-- 3) The branches at the intersection
splitAtIntersection ::
     HasHeader blk
  => GenesisTree blk
  -> (GenesisTree blk, SlotNo, [GenesisTree blk])

splitAtIntersection   (GRoot rt [t]) = let (pr, sl, ch) = splitAtIntersection t in     (GRoot rt [pr], sl, ch)
splitAtIntersection g@(GRoot b [])   = let sl = fromWithOrigin 0 $ anchorToSlotNo b in (g,             sl, [])
splitAtIntersection   (GRoot b c)    = let sl = fromWithOrigin 0 $ anchorToSlotNo b in (GRoot b [],    sl, c)

splitAtIntersection (GNode b f                (t NE.:| [])) = let (pr, sl, ch) = splitAtIntersection t in (GNode b f (pr NE.:| []),    sl,          ch)
splitAtIntersection (GNode b IncompleteBranch c)            =                                             (GLeaf b FragmentIncomplete, blockSlot b, NE.toList c)
splitAtIntersection (GNode b _                c)            =                                             (GLeaf b FragmentComplete,   blockSlot b, NE.toList c)

splitAtIntersection g@(GLeaf b _) = (g, blockSlot b, [])

{-------------------------------------------------------------------------------
 Find the densest branches at a intersection
-------------------------------------------------------------------------------}

-- | Given a genesis window length, a slot at which to anchor the window and a
-- list of candidates find the densest ones. The returned list might be empty in
-- which case it means that it couldn't pick due to unknown densities.
densestInWindow ::
     forall blk.  HasHeader blk
  => GenesisWindowLength
  -> SlotNo
  -> [GenesisTree blk]
  -> [GenesisTree blk]
densestInWindow window wrt candidates =
    -- Candidates must be anchored at the same point
    assert (not $ null candidates) $
    let trimmed = trimToWindow candidates
        (areComplete, nonComplete) = partition (isCompleteInWindow . fst) trimmed
    in
      case (areComplete, nonComplete) of
        ([],[]) -> error "Precondition violated, empty window"
        ([],_) -> []
                  -- no candidates are fully complete, so we don't know which
                  -- one to choose.
        _ -> let densestComplete =
                   Prelude.head
                   $ groupBy ((==) `on` fst)
                   $ sortBy (flip compare `on` fst)
                   $ map (\(x,y) -> (getDensity x, y)) areComplete
             in case nonComplete of
                  [] ->
                    -- Every candidate is known, so we provide the best ones.
                    map snd densestComplete
                  _ ->
                    -- There are some unknown candidates, the only time when we
                    -- can return something is if one of the unknown branches is
                    -- already as dense as the densest of the known ones (either
                    -- by a sub-branch which is known or even if there is an
                    -- unknown branch that is actually already denser)
                    let densestNonComplete =
                            groupBy ((==) `on` fst)
                          $ sortBy (flip compare `on` fst)
                          $ map (\(x,y) -> (getPotentialDensity x, y)) nonComplete
                    in
                      case densestNonComplete of
                        [(d,f)]:[] ->
                          -- There is only one densest unknown candidate
                          if d >= fst (Prelude.head densestComplete)
                          then [f]
                          else []
                        d:_ ->
                          -- If the perceived density of all the incomplete
                          -- branches cannot win against the densest one that
                          -- are complete ever, then we can pick the known ones.
                          if all ((fst (Prelude.head densestComplete) >=) . fst) d
                          then map snd densestComplete
                          else []
  where
    -- | Reduce the candidates to the window that we are considering, keep the
    -- original candidate (without trimming) for future use.
    trimToWindow :: [GenesisTree blk] -> [(GenesisTree blk, GenesisTree blk)]
    trimToWindow =
        catMaybes
        . map (\case
                GRoot _ _ -> error "Precondition violated"
                g@(GLeaf b _) ->
                  if isInWindow b then Just (g, g) else Nothing
                g@(GNode b f c) ->
                  if isInWindow b
                  then case trimToWindow $ NE.toList c of
                         [] -> Just $ (GLeaf b (if f == IncompleteBranch
                                               then FragmentIncomplete
                                               else FragmentComplete), g)
                         c' -> Just $ (GNode b f $ NE.fromList (map fst c'), g)
                  else Nothing)

    isInWindow :: blk -> Bool
    isInWindow x = unSlotNo (blockSlot x - wrt) <= genesisWindowLength window

    -- | Return whether a candidate is complete in the window
    isCompleteInWindow :: GenesisTree blk -> Bool
    isCompleteInWindow GRoot{} = error "Precondition violated"
    isCompleteInWindow (GNode _ IncompleteBranch _) = False
    isCompleteInWindow (GNode _ _ c) = all isCompleteInWindow c
    isCompleteInWindow (GLeaf _ FragmentComplete) = True
    isCompleteInWindow (GLeaf _ FragmentIncomplete) = False

    -- | Get the density of a candidate in this window
    getDensity :: GenesisTree blk -> Word64
    getDensity GRoot{}       = error "PV"
    getDensity GLeaf{}       = 1
    getDensity (GNode _ _ n) = 1 + maximum (map getDensity $ NE.toList n)

    getPotentialDensity :: GenesisTree blk -> Word64
    getPotentialDensity GRoot{}       = error "PV"
    getPotentialDensity (GLeaf b FragmentIncomplete) = genesisWindowLength window - unSlotNo (blockSlot b - wrt)
    getPotentialDensity (GNode b IncompleteBranch n) = maximum ((genesisWindowLength window - unSlotNo (blockSlot b - wrt)) : (map ((1 +) . getDensity) $ NE.toList n))
    getPotentialDensity (GNode b NA n) = 1 + maximum (map getDensity $ NE.toList n)
