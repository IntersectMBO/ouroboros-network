module Ouroboros.Consensus.Ledger.SupportsPeerSelection (
    LedgerSupportsPeerSelection (..)
  , PoolStake
    -- * Re-exports for convenience
  , DomainAddress (..)
  , PortNumber
  , RelayAddress (..)
  ) where

import           Data.List.NonEmpty (NonEmpty)
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (DomainAddress (..), PoolStake, PortNumber,
                     RelayAddress (..))

import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)


class LedgerSupportsPeerSelection blk where
  -- | Return peers registered in the ledger ordered by descending 'PoolStake'.
  --
  -- For example, for Shelley, the relays that have been registered in the
  -- ledger for the respective stake pools will be returned.
  --
  -- Ledgers/blocks that don't support staking can return an empty list.
  getPeers :: LedgerState blk -> [(PoolStake, NonEmpty RelayAddress)]
