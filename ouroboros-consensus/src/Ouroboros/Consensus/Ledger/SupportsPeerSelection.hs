module Ouroboros.Consensus.Ledger.SupportsPeerSelection (
    LedgerSupportsPeerSelection (..)
  , PoolStake (..)
  , StakePoolRelay (..)
  , stakePoolRelayAccessPoint
    -- * Re-exports for convenience
  , DomainAccessPoint (..)
  , IP (..)
  , PortNumber
  , RelayAccessPoint (..)
  ) where

import           Control.DeepSeq (NFData (..))
import           Data.List.NonEmpty (NonEmpty)

import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (DomainAccessPoint (..), IP (..), PoolStake (..),
                     PortNumber, RelayAccessPoint (..))

import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)

-- | A relay registered for a stake pool
data StakePoolRelay =
    -- | One of the current relays
    CurrentRelay RelayAccessPoint

    -- | One of the future relays
  | FutureRelay  RelayAccessPoint
  deriving (Show, Eq)

instance NFData StakePoolRelay where
    rnf (CurrentRelay ra) = rnf ra
    rnf (FutureRelay  ra) = rnf ra

stakePoolRelayAccessPoint :: StakePoolRelay -> RelayAccessPoint
stakePoolRelayAccessPoint (CurrentRelay ra) = ra
stakePoolRelayAccessPoint (FutureRelay  ra) = ra

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
