module Ouroboros.Network.ConsensusMode where

-- | Configures the diffusion layer 
data ConsensusMode =
    GenesisMode
  -- ^ The mode available from Conway era which implements consensus chain-skipping
  -- which leverages many big ledger peers from the ledger data when syncing up
  -- to ensure we end up on an honest chain.
  | PraosMode
  -- ^ The legacy mode which depends on official relays and/or bootstrap peers
  -- configuration
  deriving Show
