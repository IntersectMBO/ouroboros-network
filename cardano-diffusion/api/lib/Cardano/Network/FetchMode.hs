{-# LANGUAGE LambdaCase #-}

module Cardano.Network.FetchMode
  ( mkReadFetchMode
  , module Ouroboros.Network.BlockFetch.ConsensusInterface
  ) where

import Data.Functor ((<&>))

import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.Types (LedgerStateJudgement (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface


-- | Construct 'readFetchMode' for 'BlockFetchConsensusInterface' by branching
-- on the 'ConsensusMode'.
mkReadFetchMode
  :: Functor m
  => ConsensusMode
  -> m LedgerStateJudgement
     -- ^ Used for 'GenesisMode'.
  -> m PraosFetchMode
     -- ^ Used for 'PraosMode' for backwards compatibility.
  -> m FetchMode
mkReadFetchMode consensusMode getLedgerStateJudgement getFetchMode =
    case consensusMode of
      GenesisMode -> getLedgerStateJudgement <&> \case
        YoungEnough -> PraosFetchMode FetchModeDeadline
        TooOld      -> FetchModeGenesis
      PraosMode   -> PraosFetchMode <$> getFetchMode

