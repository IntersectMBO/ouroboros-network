{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An implementation of the prefix selection algorithm.
module Ouroboros.Consensus.NodeKernel.Genesis (
    processWithPrefixSelection
    -- * For testing
  , density
  , findCommonPrefix
  , findSuffixes
  , prefixSelection
  ) where

import           Prelude hiding (length)

import           Data.Function (on)
import           Data.List (foldl', maximumBy)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.GenesisWindowLength
import           Ouroboros.Network.AnchoredFragment hiding (filter)
import           Ouroboros.Network.AnchoredFragment.Completeness
import           Ouroboros.Network.AnchoredSeq hiding (filter, join, map,
                     prettyPrint)

{-------------------------------------------------------------------------------
 The prefix selection algorithm
-------------------------------------------------------------------------------}

-- | Process a map of candidates as stated in the prefixSelection chapter of the
-- report.
processWithPrefixSelection ::
     ( BlockSupportsProtocol blk
     , Ord peer
     )
  => Point (Header blk)
     -- ^ The current immutable tip as point
  -> GenesisWindowLength
     -- ^ The length of the genesis window
  -> Map peer (AnnotatedAnchoredFragment (Header blk))
     -- ^ The list of candidates
  -> Map peer (AnnotatedAnchoredFragment (Header blk))
processWithPrefixSelection immTip s m =
    Map.fromList result
  where
    theList        = Map.toList m
    selectedPrefix = prefixSelection immTip s $ map snd theList
    headAsPoint    = headPoint selectedPrefix
    result         =
      catMaybes
      [ do
          (_, reAnchored)    <- splitAfterPoint (fragment candidate) immTip
          (candidate', rest) <- splitAfterPoint reAnchored headAsPoint
          return (peer, AnnotatedAnchoredFragment candidate' (case rest of
                            Empty _ -> completeness candidate
                            _       -> FragmentIncomplete))
      | (peer, candidate) <- theList ]

{-------------------------------------------------------------------------------
 Internal implementation
-------------------------------------------------------------------------------}

-- | Run the prefix selection algorithm on a list of candidates.
prefixSelection :: forall blk.
  ( HasHeader (Header blk)
  , HasHeader blk
  )
  => Point (Header blk)
     -- ^ The current immutable tip as point
  -> GenesisWindowLength
     -- ^ The length of the genesis window
  -> [AnnotatedAnchoredFragment (Header blk)]
     -- ^ The list of candidates
  ->   AnchoredFragment (Header blk)
prefixSelection currentImmTip s originalCandidates =
    go initialPrefix
  where
    -- | Compute a usable list of candidates
    --
    -- Every candidate is anchored at the block that was the tip of the
    -- immutable database when the sync was performed. The immutable tip might
    -- have moved and candidates that fork before it can be safely discarded as
    -- the ChainDB will never switch to them. Therefore we will continue only
    -- with the candidates that contain the immutable tip and we will anchor
    -- them there.
    --
    -- Note that after this, *all candidates are anchored at the same point*,
    -- namely the tip of the immutable database.
    initialCandidates :: [AnnotatedAnchoredFragment (Header blk)]
    initialCandidates =
      catMaybes
        [     flip AnnotatedAnchoredFragment (completeness candidate)
          .   snd
          <$> splitAfterPoint (fragment candidate) currentImmTip
        | candidate <- originalCandidates ]

    -- | First common prefix
    initialPrefix :: AnchoredFragment (Header blk)
    initialPrefix = findCommonPrefix (map fragment initialCandidates)

    -- | Recursive algorithm
    --
    -- Given an anchored fragment, the algorithm will run by:
    -- - anchoring a genesis window at the end of the fragment
    -- - making a decision based on densities of the fragments in the window
    -- - find the common prefix of the set of candidates that share more than
    --   just the anchor of the window with the best candidate.
    -- - repeat if in the previous step, multiple candidates share the prefix.
    go :: AnchoredFragment (Header blk) -> AnchoredFragment (Header blk)
    go thisIterationPrefix =
        case result of
          Just result' -> result'
          Nothing      -> error "Fragments that should be consecutive could not be joined"
      where

        -- | The suffixes of the candidates anchored at the head of
        -- thisIterationPrefix
        thisIterationSuffixes ::
          [AnnotatedAnchoredFragment (Header blk)]
        thisIterationSuffixes =
          findSuffixes thisIterationPrefix initialCandidates

        -- | The boundaries of the genesis window
        thisWindow :: (WithOrigin SlotNo, SlotNo)
        thisWindow = genesisWindowBounds (headSlot thisIterationPrefix) s

        -- | The best candidate in the window
        iterationBestCandidate :: PrefixSelection (AnchoredFragment (Header blk))
        iterationBestCandidate =
          if all (hasKnownDensityAt (snd thisWindow)) thisIterationSuffixes
          then
            -- Find the densest candidate and continue
              MoreDecisions
            $ maximumBy (compare `on` density thisWindow) (map fragment thisIterationSuffixes)
          else
            case filter (not . hasKnownDensityAt (snd thisWindow)) thisIterationSuffixes of
              [one] ->
                if density thisWindow (fragment one)
                   >= maximum (map (density thisWindow) (map fragment thisIterationSuffixes))
                -- there is only one candidate with unknown density and it is
                -- already the densest in the window.
                then LastDecision $ fragment one
                else Unknown
              _ -> Unknown

        -- | The common prefix for the next iteration
        nextIterationPrefix :: PrefixSelection (AnchoredFragment (Header blk))
        nextIterationPrefix =
          case iterationBestCandidate of
            MoreDecisions bestCandidate ->
              let nextIterationFragments =
                    [ candidate | candidate <- thisIterationSuffixes
                                , case intersect bestCandidate (fragment candidate) of
                                    -- discard non-intersecting
                                    Nothing                    -> False
                                    -- include iterationBestCandidate itself
                                    Just (Empty _, Empty _, a, b) ->
                                         headPoint a == headPoint b
                                      && headPoint a == headPoint bestCandidate
                                    -- discard those that only intersect on the
                                    -- anchor
                                    Just (_, Empty _, _, _) -> False
                                    _                       -> True
                                ]
              in
                case nextIterationFragments of
                  [f] -> LastDecision $ fragment f
                  _   -> MoreDecisions $ findCommonPrefix (map fragment nextIterationFragments)
            l@(LastDecision ld) ->
              let nextIterationFragments = [ candidate | candidate <- thisIterationSuffixes
                                , case intersect ld (fragment candidate) of
                                    -- discard non-intersecting
                                    Nothing                    -> False
                                    -- include iterationBestCandidate itself
                                    Just (Empty _, Empty _, a, b) ->
                                         headPoint a == headPoint b
                                      && headPoint a == headPoint ld
                                    -- discard those that only intersect on the
                                    -- anchor
                                    Just (_, Empty _, _, _) -> False
                                    _                       -> True
                                ]
              in
                case nextIterationFragments of
                  [_] -> l
                  _   -> MoreDecisions $ findCommonPrefix (map fragment nextIterationFragments)
            _ -> iterationBestCandidate

        -- | This iteration's result
        result =
            join thisIterationPrefix
            $ case nextIterationPrefix of
                LastDecision ld  -> ld
                Unknown          -> Empty $ headAnchor thisIterationPrefix
                MoreDecisions md -> go md

-- | A result of prefix selection
data PrefixSelection a =
    -- | A decision has been made at a known-density point. The next
    -- intersection point has to be resolved.
    MoreDecisions a
  | -- | There are no more intersection points to resolve.
    LastDecision a
  | -- | There was no outcome from the decision process because densities are
    -- unknown at the intersection point.
    Unknown
 deriving (Show)

{-------------------------------------------------------------------------------
 Density
-------------------------------------------------------------------------------}

-- | Whether the provided fragment has known density within a window that ends
-- at the given slot number.
hasKnownDensityAt ::
     HasHeader b
  => SlotNo
  -> AnnotatedAnchoredFragment b
  -> Bool
hasKnownDensityAt _  (AnnotatedAnchoredFragment _ FragmentComplete) = True
hasKnownDensityAt to f                                              =
  withOrigin False (>= to) (headSlot $ fragment f)

-- | Compute the density of the fragment in the given window
density ::
     HasHeader blk
  => (WithOrigin SlotNo, SlotNo)
  -> AnchoredFragment blk
  -> Int
density (a, b) f =
    length
    -- To be completely precise, the density would sometimes require a +1 but as
    -- all fragments will go through the same function, they will all be scaled.
  . takeWhileOldest (\blk -> blockSlot blk <= b)
  . maybe f snd
    -- If the fragment is already anchored at the beginning of the window, split
    -- will return nothing. This assumes that we are going to apply this
    -- function only on fragments which actually have a block at the anchor of
    -- the genesis window.
  . splitBeforeMeasure a (const True)
  $ f

{-------------------------------------------------------------------------------
 Prefixes and suffixes operations
-------------------------------------------------------------------------------}

-- | Given a list of chains, find the prefix that is common to all of them.
--
-- PRECONDITION: all candidates must be anchored at the same anchor.
findCommonPrefix ::
     HasHeader (Header blk)
  => [AnchoredFragment (Header blk)]
  ->  AnchoredFragment          (Header blk)
findCommonPrefix []     = error "findCommonPrefix called with empty list"
findCommonPrefix (f:fs) =
  case result of
    Just frag -> frag
    Nothing   -> error "Precondition violated: fragments must be anchored at the same anchor."
  where
    result =
      foldl'
        (\acc candidate -> do
            acc' <- acc
            (acc'', _, _, _) <- intersect acc' candidate
            return acc'')
        (Just f)
        fs

-- | Move the anchor point of each fragment in the list to the head of the
-- common prefix provided.
findSuffixes ::
     forall hblk. HasHeader hblk
  => AnchoredFragment hblk
  -> [AnnotatedAnchoredFragment hblk]
  -> [AnnotatedAnchoredFragment hblk]
findSuffixes commonPrefix l =
   catMaybes
     [     flip AnnotatedAnchoredFragment (completeness candidate)
       .   snd
       <$> splitAfterPoint (fragment candidate) (headPoint commonPrefix)
     | candidate <- l ]
