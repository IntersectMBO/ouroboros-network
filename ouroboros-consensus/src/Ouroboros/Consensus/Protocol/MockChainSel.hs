{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.Protocol.MockChainSel (
    selectChain
  , selectUnvalidatedChain
  ) where

import           Data.List (sortOn)
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Ord (Down (..))

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
selectChain :: forall proxy p hdr l. ConsensusProtocol p
            => proxy p
            -> (hdr -> SelectView p)
            -> Chain hdr           -- ^ Our chain
            -> [(Chain hdr, l)]    -- ^ Upstream chains
            -> Maybe (Chain hdr, l)
selectChain p view ours =
      listToMaybe
    . map snd
    . sortOn (Down . fst)
    . mapMaybe selectPreferredCandidate
  where
    -- | Only retain a candidate if it is preferred over the current chain. As
    -- only a non-empty chain can be preferred over the current chain, we can
    -- extract the 'SelectView' of the tip of the candidate.
    selectPreferredCandidate ::
         (Chain hdr, l)
      -> Maybe (SelectView p, (Chain hdr, l))
    selectPreferredCandidate x@(cand, _) =
        case (Chain.head ours, Chain.head cand) of
          (Nothing, Just candTip)
            -> Just (view candTip, x)
          (Just ourTip, Just candTip)
            | let candView = view candTip
            , preferCandidate p (view ourTip) candView
            -> Just (candView, x)
          _otherwise
            -> Nothing

-- | Chain selection on unvalidated chains
selectUnvalidatedChain :: ConsensusProtocol p
                       => proxy p
                       -> (hdr -> SelectView p)
                       -> Chain hdr
                       -> [Chain hdr]
                       -> Maybe (Chain hdr)
selectUnvalidatedChain p view ours =
      fmap fst
    . selectChain p view ours
    . map (, ())
