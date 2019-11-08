{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.Protocol.MockChainSel where

import           Data.Bifunctor (first)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe (listToMaybe)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.MockChain.Chain hiding (selectChain)

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
selectChain :: forall p hdr l. (
                 OuroborosTag p
               , HasHeader hdr
               , SupportedHeader p hdr
               )
            => NodeConfig p
            -> Chain hdr           -- ^ Our chain
            -> [(Chain hdr, l)]    -- ^ Upstream chains
            -> Maybe (Chain hdr, l)
selectChain cfg ours' candidates' =
    fmap (first toChain) $ listToMaybe $
    sortBy (flip (compareCandidates cfg `on` fst)) preferred
  where
    ours       = toAnchoredFragment ours'
    candidates = map (first toAnchoredFragment) candidates'
    preferred :: [(AnchoredFragment hdr, l)]
    preferred = filter (preferCandidate cfg ours . fst) candidates
    toChain :: AnchoredFragment hdr -> Chain hdr
    toChain af
      | Just c <- fromAnchoredFragment af
      = c
      | otherwise
      = error "impossible: fragment was anchored at genesis"

-- | Chain selection on unvalidated chains
selectUnvalidatedChain :: forall p hdr. (
                            OuroborosTag p
                          , HasHeader hdr
                          , SupportedHeader p hdr
                          )
                       => NodeConfig p
                       -> Chain hdr
                       -> [Chain hdr]
                       -> Maybe (Chain hdr)
selectUnvalidatedChain cfg ours = fmap fst . selectChain cfg ours . map (, ())
