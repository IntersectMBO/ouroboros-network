module Ouroboros.Consensus.Ledger.SupportsPeerSelection (
    LedgerSupportsPeerSelection (..)
  , PoolStake (..)
  , StakePoolRelay (..)
  , stakePoolRelayAddress
    -- * Re-exports for convenience
  , DomainAddress (..)
  , IP (..)
  , PortNumber
  , RelayAddress (..)
  ) where

import           Data.List.NonEmpty (NonEmpty)

import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (DomainAddress (..), IP (..), PoolStake (..), PortNumber,
                     RelayAddress (..))

import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)

-- | A relay registered for a stake pool
data StakePoolRelay =
    -- | One of the current relays
    CurrentRelay RelayAddress

    -- | One of the future relays
  | FutureRelay  RelayAddress
  deriving (Show, Eq)

stakePoolRelayAddress :: StakePoolRelay -> RelayAddress
stakePoolRelayAddress (CurrentRelay ra) = ra
stakePoolRelayAddress (FutureRelay  ra) = ra

class LedgerSupportsPeerSelection blk where
  -- | Return peers registered in the ledger ordered by descending 'PoolStake'.
  --
  -- For example, for Shelley, the relays that have been registered in the
  -- ledger for the respective stake pools will be returned.
  --
  -- Ledgers/blocks that don't support staking can return an empty list.
  --
  -- Note: if the ledger state is old, the registered relays can also be old and
  -- may no longer be online.
  getPeers :: LedgerState blk -> [(PoolStake, NonEmpty StakePoolRelay)]
