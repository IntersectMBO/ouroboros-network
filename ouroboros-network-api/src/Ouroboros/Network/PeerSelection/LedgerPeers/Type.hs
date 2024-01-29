{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
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
  , UseLedgerPeers (..)
  , AfterSlot (..)
  , isLedgerPeersEnabled
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))
import           Control.Concurrent.Class.MonadSTM
import           Control.DeepSeq (NFData (..))
import           Data.List.NonEmpty (NonEmpty)
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
                     (RelayAccessPoint)

-- | Only use the ledger after the given slot number.
data UseLedgerPeers = DontUseLedgerPeers
                    | UseLedgerPeers AfterSlot
                    deriving (Eq, Show, Generic, NoThunks)

-- | Only use the ledger after the given slot number.
data AfterSlot = Always
               | After SlotNo
               deriving (Eq, Show, Generic)
               deriving anyclass NoThunks

isLedgerPeersEnabled :: UseLedgerPeers -> Bool
isLedgerPeersEnabled DontUseLedgerPeers = False
isLedgerPeersEnabled UseLedgerPeers {}  = True

-- | The relative stake of a stakepool in relation to the total amount staked.
-- A value in the [0, 1] range.
--
newtype PoolStake = PoolStake { unPoolStake :: Rational }
  deriving (Eq, Ord, Show)
  deriving newtype (Fractional, Num, NFData)

-- | The accumulated relative stake of a stake pool, like PoolStake but it also includes the
-- relative stake of all preceding pools. A value in the range [0, 1].
--
newtype AccPoolStake = AccPoolStake { unAccPoolStake :: Rational }
    deriving (Eq, Ord, Show)
    deriving newtype (Fractional, Num)

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
  deriving (Eq, Show, Generic, NoThunks)

-- | Return ledger state information and ledger peers.
--
data LedgerPeersConsensusInterface m = LedgerPeersConsensusInterface {
    lpGetLatestSlot           :: STM m SlotNo,
    lpGetLedgerStateJudgement :: STM m LedgerStateJudgement,
    lpGetLedgerPeers          :: STM m [(PoolStake, NonEmpty RelayAccessPoint)]
  }
