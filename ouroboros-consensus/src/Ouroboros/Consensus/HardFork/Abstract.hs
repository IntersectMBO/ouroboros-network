{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Ouroboros.Consensus.HardFork.Abstract (
    HasHardForkHistory (..)
  , neverForksHardForkSummary
  ) where

import           Data.Kind (Type)

import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Abstract

class HasHardForkHistory blk where
  -- | Type level description of the hard fork shape
  --
  -- The 'Summary' infrastructure does not care what the types in this list
  -- are, it just cares how /many/ eras there are. The hard fork combinator
  -- will instantiate 'HardForkIndices' to the types of the blocks involved
  -- in the hard fork, e.g., we might have something like
  --
  -- > '[ByronBlock, ShelleyBlock, GoguenBlock]
  type family HardForkIndices blk :: [Type]

  -- | Summary of the hard fork state
  --
  -- NOTE: 'HasHardForkHistory' is the only abstraction that the consensus
  -- layer is aware in relation to potential hard forks, and is needed only for
  -- time translations (in block production and in the chain DB). It is
  -- independent from the hard fork combinator and can be used for blocks that
  -- never fork (in which case the 'Summary' will be trivial) or indeed for
  -- blocks that do support transitions but do not use the hard fork combinator.
  --
  -- It is however useful to consider what this function means in the (typical)
  -- case that the hard fork combinator /is/ used. The HFC introduces the
  -- concept of a partial ledger config, which is essentially the ledger config
  -- minus an 'EpochInfo'. Whenever the HFC calls functions on the underlying
  -- ledger, it maintains enough state to be able to /construct/ an 'EpochInfo'
  -- on the fly and then combines that with the 'PartialLedgerConfig' to get
  -- the full 'LedgerConfig'. The config of the HFC /itself/ however does /not/
  -- require an 'EpochInfo', and so the config that we pass here will not
  -- contain that 'EpochInfo' (if it did, that would be strange: we'd be
  -- computing the 'Summary' required to construct an 'EpochInfo' while we
  -- already have one). Critically, the HFC implements 'hardForkSummary'
  -- directly and does not call 'hardForkSummary' in the underlying ledgers.
  --
  -- When running ledgers that are normally run using the HFC as standalone
  -- ledgers, then the 'LedgerConfig' here must indeed already contain timing
  -- information, and so this function becomes little more than a projection
  -- (indeed, in this case the 'LedgerState' should be irrelevant).
  hardForkSummary :: LedgerConfig blk
                  -> LedgerState blk
                  -> HardFork.Summary (HardForkIndices blk)

-- | Helper function that can be used to define 'hardForkSummary'
--
-- This is basically a proof of the claim of the documentation of
-- 'hardForkSummary' that 'hardForkSummary' becomes a mere projection of
-- a block's ledger state when there are no hard forks. It is useful to give
-- blocks such as 'ShelleyBlock' their own 'HasHardForkHistory' instance so that
-- we can run them as independent ledgers (in addition to being run with the
-- hard fork combinator).
neverForksHardForkSummary :: (LedgerConfig blk -> HardFork.EraParams)
                          -> LedgerConfig blk
                          -> LedgerState blk
                          -> HardFork.Summary '[blk]
neverForksHardForkSummary getParams cfg _st =
    HardFork.neverForksSummary eraEpochSize eraSlotLength
  where
    HardFork.EraParams{..} = getParams cfg
