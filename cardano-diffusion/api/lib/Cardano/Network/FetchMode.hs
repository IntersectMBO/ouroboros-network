{-# LANGUAGE LambdaCase #-}

module Cardano.Network.FetchMode
  ( mkReadFetchMode
  , ConsensusMode (..)
  , LedgerStateJudgement (..)
  , module Ouroboros.Network.BlockFetch.ConsensusInterface
  ) where

import Data.Functor ((<&>))

import Cardano.Network.ConsensusMode
import Cardano.Network.LedgerStateJudgement
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

