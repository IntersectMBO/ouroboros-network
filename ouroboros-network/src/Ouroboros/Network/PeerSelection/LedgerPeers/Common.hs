-- | Types used by `Ouroboros.Network.PeerSelection.LedgerPeers` and
-- `Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers`
--
module Ouroboros.Network.PeerSelection.LedgerPeers.Common where

import Control.Monad.Class.MonadTime.SI
import Data.Word (Word16)
import Text.Printf

import Data.List.NonEmpty (NonEmpty)
import Network.DNS qualified as DNS
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.RelayAccessPoint

newtype NumberOfPeers = NumberOfPeers { getNumberOfPeers :: Word16 }
  deriving Show

-- | Identifies a peer as coming from ledger or not
data IsLedgerPeer = IsLedgerPeer
                  -- ^ a ledger peer.
                  | IsNotLedgerPeer
  deriving (Eq, Show)

-- | Which ledger peers to pick.
--
data LedgerPeersKind = AllLedgerPeers | BigLedgerPeers
  deriving Show

-- | Ledger Peer request result
--
data LedgerPeers = LedgerPeers LedgerStateJudgement -- ^ Current ledger state
                               [(PoolStake, NonEmpty RelayAccessPoint)]
                               -- ^ Ledger peers
                 | BeforeSlot -- ^ No result because the node is still
                              -- before the configured UseLedgerAfter slot
                              -- number
  deriving (Eq, Show)

-- | Trace LedgerPeers events.
data TraceLedgerPeers =
      PickedBigLedgerPeer RelayAccessPoint AccPoolStake PoolStake
      -- ^ Trace for a significant ledger peer picked with accumulated and relative stake of its pool.
    | PickedLedgerPeer RelayAccessPoint AccPoolStake PoolStake
      -- ^ Trace for a ledger peer picked with accumulated and relative stake of its pool.
    | PickedBigLedgerPeers NumberOfPeers [RelayAccessPoint]
    | PickedLedgerPeers    NumberOfPeers [RelayAccessPoint]
      -- ^ Trace for the number of peers and we wanted to pick and the list of peers picked.
    | FetchingNewLedgerState Int Int
      -- ^ Trace for fetching a new list of peers from the ledger. The first Int
      -- is the number of ledger peers returned the latter is the number of big
      -- ledger peers.
    | TraceLedgerPeersDomains [DomainAccessPoint]
    | TraceLedgerPeersResult  DNS.Domain [(IP, DNS.TTL)]
    | TraceLedgerPeersFailure DNS.Domain DNS.DNSError
    | DisabledLedgerPeers
      -- ^ Trace for when getting peers from the ledger is disabled, that is DontUseLedgerPeers.
    | TraceUseLedgerPeers UseLedgerPeers
      -- ^ Trace UseLedgerPeers value
    | WaitingOnRequest
    | RequestForPeers NumberOfPeers
    | ReusingLedgerState Int DiffTime
    | FallingBackToPublicRootPeers
    | NotEnoughBigLedgerPeers NumberOfPeers Int
    | NotEnoughLedgerPeers NumberOfPeers Int


instance Show TraceLedgerPeers where
    show (PickedBigLedgerPeer addr ackStake stake) =
        printf "PickedBigLedgerPeer %s ack stake %s ( %.04f) relative stake %s ( %.04f )"
            (show addr)
            (show $ unAccPoolStake ackStake)
            (fromRational (unAccPoolStake ackStake) :: Double)
            (show $ unPoolStake stake)
            (fromRational (unPoolStake stake) :: Double)
    show (PickedLedgerPeer addr ackStake stake) =
        printf "PickedLedgerPeer %s ack stake %s ( %.04f) relative stake %s ( %.04f )"
            (show addr)
            (show $ unAccPoolStake ackStake)
            (fromRational (unAccPoolStake ackStake) :: Double)
            (show $ unPoolStake stake)
            (fromRational (unPoolStake stake) :: Double)
    show (PickedBigLedgerPeers (NumberOfPeers n) peers) =
        printf "PickedBigLedgerPeers %d %s" n (show peers)
    show (PickedLedgerPeers (NumberOfPeers n) peers) =
        printf "PickedLedgerPeers %d %s" n (show peers)
    show (FetchingNewLedgerState cnt bigCnt) =
        printf "Fetching new ledgerstate, %d registered pools, %d registered big ledger pools"
            cnt bigCnt
    show (TraceUseLedgerPeers ulp) =
        printf "UseLedgerPeers state %s"
            (show ulp)
    show WaitingOnRequest = "WaitingOnRequest"
    show (RequestForPeers (NumberOfPeers cnt)) = printf "RequestForPeers %d" cnt
    show (ReusingLedgerState cnt age) =
        printf "ReusingLedgerState %d peers age %s"
          cnt
          (show age)
    show FallingBackToPublicRootPeers = "Falling back to public root peers"
    show DisabledLedgerPeers = "LedgerPeers is disabled"
    show (NotEnoughBigLedgerPeers (NumberOfPeers n) numOfBigLedgerPeers) =
      printf "Not enough big ledger peers to pick %d out of %d" n numOfBigLedgerPeers
    show (NotEnoughLedgerPeers (NumberOfPeers n) numOfLedgerPeers) =
      printf "Not enough ledger peers to pick %d out of %d" n numOfLedgerPeers

    show (TraceLedgerPeersDomains domains) = "Resolving " ++ show domains
    show (TraceLedgerPeersResult domain l) =
      "Resolution success " ++ show domain ++ " " ++ show l
    show (TraceLedgerPeersFailure domain err) =
      "Resolution failed " ++ show domain ++ " " ++ show err
