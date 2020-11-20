module Ouroboros.Consensus.Ledger.SupportsPeerSelection (
    LedgerSupportsPeerSelection (..)
  , PoolStake
  , StakePoolRelay (..)
  , stakePoolRelayDomainAddress
    -- * Re-exports for convenience
  , DomainAddress (..)
  , Domain
  , PortNumber
  ) where

import           Data.List.NonEmpty (NonEmpty)

import           Ouroboros.Network.PeerSelection.RootPeersDNS (Domain,
                     DomainAddress (..), PortNumber)

import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)

-- | The relative stake of the stakepool. A value in the [0, 1] range.
type PoolStake = Rational

-- | A relay registered for a stake pool
data StakePoolRelay =
    -- | One of the current relays
    CurrentRelay DomainAddress

    -- | One of the future relays
  | FutureRelay  DomainAddress
  deriving (Show, Eq)

stakePoolRelayDomainAddress :: StakePoolRelay -> DomainAddress
stakePoolRelayDomainAddress (CurrentRelay da) = da
stakePoolRelayDomainAddress (FutureRelay  da) = da

class LedgerSupportsPeerSelection blk where
  -- | Return peers registered in the ledger ordered by descending 'PoolStake'.
  --
  -- For example, for Shelley, the relays that have been registered in the
  -- ledger for the respective stake pools will be returned.
  --
  -- Ledgers/blocks that don't support staking can return an empty list.
  getPeers :: LedgerState blk -> [(PoolStake, NonEmpty StakePoolRelay)]
