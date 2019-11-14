{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.Protocol.MockChainSel where

import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe (listToMaybe)

import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Chain selection
-------------------------------------------------------------------------------}

-- | Chain selection between our chain and list of candidates
--
-- This is only a /model/ of chain selection: in reality of course we will not
-- work with entire chains in memory. This function is intended as an
-- explanation of how chain selection should work conceptually.
--
-- The @l@ parameter here models the ledger state for each chain, and serves as
-- evidence that the chains we are selecting between have been validated. (It
-- would /not/ be  correct to run chain selection on unvalidated chains and then
-- somehow fail if the selected chain turns out to be invalid.)
--
-- Returns 'Nothing' if we stick with our current chain.
selectChain :: forall p hdr l. (OuroborosTag p, HasHeader hdr, CanSelect p hdr)
            => NodeConfig p
            -> Chain hdr           -- ^ Our chain
            -> [(Chain hdr, l)]    -- ^ Upstream chains
            -> Maybe (Chain hdr, l)
selectChain cfg ours candidates =
    listToMaybe $
      sortBy (flip (compareCandidates' `on` fst)) preferredCandidates
  where
    preferredCandidates :: [(Chain hdr, l)]
    preferredCandidates = filter (preferCandidate' . fst) candidates

    -- A non-empty chain is always preferred over an empty one

    preferCandidate' :: Chain hdr -> Bool
    preferCandidate' theirs =
        go (Chain.head ours) (Chain.head theirs)
      where
        go :: Maybe hdr -> Maybe hdr -> Bool
        go Nothing  Nothing  = False
        go Nothing  (Just _) = True
        go (Just _) Nothing  = False
        go (Just a) (Just b) = preferCandidate cfg a b

    compareCandidates' :: Chain hdr -> Chain hdr -> Ordering
    compareCandidates' = go `on` Chain.head
      where
        go :: Maybe hdr -> Maybe hdr -> Ordering
        go Nothing  Nothing  = EQ
        go Nothing  (Just _) = LT
        go (Just _) Nothing  = GT
        go (Just a) (Just b) = compareCandidates cfg a b

-- | Chain selection on unvalidated chains
selectUnvalidatedChain :: forall p hdr. (
                            OuroborosTag p
                          , HasHeader hdr
                          , CanSelect p hdr
                          )
                       => NodeConfig p
                       -> Chain hdr
                       -> [Chain hdr]
                       -> Maybe (Chain hdr)
selectUnvalidatedChain cfg ours = fmap fst . selectChain cfg ours . map (, ())
