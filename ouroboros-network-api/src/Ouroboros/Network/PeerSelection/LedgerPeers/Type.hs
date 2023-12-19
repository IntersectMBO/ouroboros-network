{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Various types related to ledger peers.  This module is re-exported from
-- "Ouroboros.Network.PeerSelection.LedgerPeers".
--
module Ouroboros.Network.PeerSelection.LedgerPeers.Type
  ( PoolStake (..)
  , AccPoolStake (..)
  , IsBigLedgerPeer (..)
  , LedgerStateJudgement (..)
  , LedgerPeersConsensusInterface (..)
  ) where

import           Cardano.Slotting.Slot (SlotNo)
import           Control.Concurrent.Class.MonadSTM
import           Control.DeepSeq (NFData (..))
import           Data.List.NonEmpty (NonEmpty)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
                     (RelayAccessPoint)


-- | The relative stake of a stakepool in relation to the total amount staked.
-- A value in the [0, 1] range.
--
newtype PoolStake = PoolStake { unPoolStake :: Rational }
  deriving (Eq, Fractional, Num, Ord, Show)
  deriving newtype NFData


-- | The accumulated relative stake of a stake pool, like PoolStake but it also includes the
-- relative stake of all preceding pools. A value in the range [0, 1].
--
newtype AccPoolStake = AccPoolStake { unAccPoolStake :: Rational }
    deriving (Eq, Fractional, Num, Ord, Show)


-- | A boolean like type.  Big ledger peers are the largest SPOs which control
-- 90% of staked stake.
--
-- Note that 'IsBigLedgerPeer' indicates a role that peer plays in the eclipse
-- evasion, e.g. that a peer was explicitly selected as a big ledger peer, e.g.
-- 'IsNotBigLedgerPeer' does not necessarily mean that the peer isn't a big
-- ledger peer.  This is because we select root peers from all ledger peers
-- (including big ones).
--
data IsBigLedgerPeer
   = IsBigLedgerPeer
   | IsNotBigLedgerPeer
  deriving Eq

-- | Wether the node is caught up or fell too far behind the chain
data LedgerStateJudgement = YoungEnough | TooOld
  deriving (Eq, Show)

-- | Return ledger state information and ledger peers.
--
data LedgerPeersConsensusInterface m = LedgerPeersConsensusInterface {
    lpGetLatestSlot           :: STM m SlotNo,
    lpGetLedgerStateJudgement :: STM m LedgerStateJudgement,
    lpGetLedgerPeers          :: STM m [(PoolStake, NonEmpty RelayAccessPoint)]
  }
