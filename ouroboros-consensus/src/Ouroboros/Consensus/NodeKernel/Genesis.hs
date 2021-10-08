{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An implementation of the prefix selection algorithm.
module Ouroboros.Consensus.NodeKernel.Genesis (
    processWithPrefixSelectionImpl
  , processWithPrefixSelectionSpec
    -- * Testing
  , genesisFoldIsDominated
  , genesisFoldPrefixSelection
  ) where

import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.GenesisWindowLength
import           Ouroboros.Network.AnchoredFragment
import           Ouroboros.Network.AnchoredFragment.Completeness

import           Ouroboros.Consensus.NodeKernel.Genesis.Impl
import           Ouroboros.Consensus.NodeKernel.Genesis.Spec

{-------------------------------------------------------------------------------
 Instantiations
-------------------------------------------------------------------------------}

-- | Concrete instantiation of foldWithCommonPrefixStep for prefixSelection.
genesisFoldPrefixSelection ::
     HasHeader blk
  => GenesisBlockchain blk
  -> GenesisTree blk
genesisFoldPrefixSelection gb =
    foldWithCommonPrefixStep picker updater initiator (t gb)
  where
    initiator               = id

    picker ([],         _ ) =
      -- If there are no candidates, then finish here.
      Nothing
    picker (candidates, sl) =
      case densestInWindow (s gb) sl candidates of
        -- if no candidates were picked among the provided ones, end here.
        []  -> Nothing
        -- if some candidates were selected as best, pick the first one. Note
        -- that the specification says that it is okay to "pick at random" among
        -- the densest ones.
        d:_ -> Just (d, ())

    updater st (cp, ())     =
      -- Continue going always, concat-ing the trees.
      Right $ concatTrees st cp

-- | Concrete instantiation of foldWithCommonPrefixStep for checking if a tree
-- is dominated.
genesisFoldIsDominated ::
     HasHeader blk
  => GenesisBlockchain blk
  -> Bool
genesisFoldIsDominated gb =
    foldWithCommonPrefixStep picker updater initiator (t gb)
  where
    initiator               = const True

    picker ([],         _ ) =
      -- If there are no candidates just return.
      Nothing
    picker (candidates, sl) =
      case densestInWindow (s gb) sl candidates of
        -- if no candidates will be picked, return True as it is dominated up to
        -- the known point.
        []  -> Just (undefined, True)
        -- If there is only one candidate, then return True
        [_] -> Just (undefined, True)
        -- If there are multiple candidates, then return false
        _   -> Just (undefined, False)


    -- When a False is returned, then short-circuit.
    -- Note that this is not strict on the first component of the tuple.
    updater _ (_, single)  = if single then Right True else Left False

-- | Process a map of candidates as stated in the prefixSelection chapter of the
-- report.
processWithPrefixSelectionImpl ::
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
processWithPrefixSelectionImpl immTip s m =
    Map.fromList result
  where
    candidatesList = Map.toList m
    initialCandidates =
      catMaybes
        [     flip AnnotatedAnchoredFragment (completeness candidate)
          .   snd
          <$> splitAfterPoint (fragment candidate) immTip
        | candidate <- map snd candidatesList ]
    selectedPrefix = unitaryTreeToAnchoredFragment $
      genesisFoldPrefixSelection (GenesisBlockchain s (anchoredFragmentsToTree initialCandidates))
    headAsPoint    = headPoint selectedPrefix
    result         =
      catMaybes
      [ do
          (_, reAnchored)    <- splitAfterPoint (fragment candidate) immTip
          (candidate', rest) <- splitAfterPoint reAnchored headAsPoint
          return (peer, AnnotatedAnchoredFragment candidate' (case rest of
                            Empty _ -> completeness candidate
                            _       -> FragmentIncomplete))
      | (peer, candidate) <- candidatesList ]
