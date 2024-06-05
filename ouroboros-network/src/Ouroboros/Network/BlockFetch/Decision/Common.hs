{-# LANGUAGE RankNTypes #-}

-- | This module contains the part of the block fetch decisions process that is
-- common to both the bulk sync and deadline modes.
module Ouroboros.Network.BlockFetch.Decision.Common (
    FetchDecisionPolicy (..)
  , PeerInfo
  , FetchDecision
  , FetchDecline (..)
) where

import GHC.Stack (HasCallStack)
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.SizeInBytes ( SizeInBytes )
import Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..), PeerFetchStatus (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..))
import Ouroboros.Network.DeltaQ ( PeerGSV )

data FetchDecisionPolicy header = FetchDecisionPolicy {
       maxInFlightReqsPerPeer  :: Word,  -- A protocol constant.

       maxConcurrencyBulkSync  :: Word,
       maxConcurrencyDeadline  :: Word,
       decisionLoopInterval    :: DiffTime,
       peerSalt                :: Int,

       plausibleCandidateChain :: HasCallStack
                               => AnchoredFragment header
                               -> AnchoredFragment header -> Bool,

       compareCandidateChains  :: HasCallStack
                               => AnchoredFragment header
                               -> AnchoredFragment header
                               -> Ordering,

       blockFetchSize          :: header -> SizeInBytes
     }

type PeerInfo header peer extra =
       ( PeerFetchStatus header,
         PeerFetchInFlight header,
         PeerGSV,
         peer,
         extra
       )

-- | Throughout the decision making process we accumulate reasons to decline
-- to fetch any blocks. This type is used to wrap intermediate and final
-- results.
--
type FetchDecision result = Either FetchDecline result

-- | All the various reasons we can decide not to fetch blocks from a peer.
--
-- It is worth highlighting which of these reasons result from competition
-- among upstream peers.
--
-- * 'FetchDeclineInFlightOtherPeer': decline this peer because all the
--   unfetched blocks of its candidate chain have already been requested from
--   other peers. This reason reflects the least-consequential competition
--   among peers: the competition that determines merely which upstream peer to
--   burden with the request (eg the one with the best
--   'Ouroboros.Network.BlockFetch.DeltaQ.DeltaQ' metrics). The consequences
--   are relatively minor because the unfetched blocks on this peer's candidate
--   chain will be requested regardless; it's merely a question of "From who?".
--   (One exception: if an adversarial peer wins this competition such that the
--   blocks are only requested from them, then it may be possible that this
--   decision determines whether the blocks are ever /received/. But that
--   depends on details of timeouts, a longer competing chain being soon
--   received within those timeouts, and so on.)
--
-- * 'FetchDeclineChainNotPlausible': decline this peer because the node has
--   already fetched, validated, and selected a chain better than its candidate
--   chain from other peers (or from the node's own block forge). Because the
--   node's current selection is influenced by what blocks other peers have
--   recently served (or it recently minted), this reason reflects that peers
--   /indirectly/ compete by serving as long of a chain as possible and as
--   promptly as possible. When the tips of the peers' selections are all
--   within their respective forecast horizons (see
--   'Ouroboros.Consensus.Ledger.SupportsProtocol.ledgerViewForecastAt'), then
--   the length of their candidate chains will typically be the length of their
--   selections, since the ChainSync is free to race ahead (in contrast, the
--   BlockFetch pipeline depth is bounded such that it will, for a syncing
--   node, not be able to request all blocks between the selection and the end
--   of the forecast window). But if one or more of their tips is beyond the
--   horizon, then the relative length of the candidate chains is more
--   complicated, influenced by both the relative density of the chains'
--   suffixes and the relative age of the chains' intersection with the node's
--   selection (since each peer's forecast horizon is a fixed number of slots
--   after the candidate's successor of that intersection).
--
-- * 'FetchDeclineConcurrencyLimit': decline this peer while the node has
--   already fully allocated the artificially scarce 'maxConcurrentFetchPeers'
--   resource amongst its other peers. This reason reflects the
--   least-fundamental competition: it's the only way a node would decline a
--   candidate chain C that it would immediately switch to if C had somehow
--   already been fetched (and any better current candidates hadn't). It is
--   possible that this peer's candidate fragment is better than the candidate
--   fragments of other peers, but that should only happen ephemerally (eg for
--   a brief while immediately after first connecting to this peer).
--
-- * 'FetchDeclineChainIntersectionTooDeep': decline this peer because the node's
--   selection has more than @K@ blocks that are not on this peer's candidate
--   chain. Typically, this reason occurs after the node has been declined---ie
--   lost the above competitions---for a long enough duration. This decision
--   only arises if the BlockFetch decision logic wins a harmless race against
--   the ChainSync client once the node's selection gets longer, since
--   'Ouroboros.Consensus.MiniProtocol.ChainSync.Client.ForkTooDeep'
--   disconnects from such a peer.
--
data FetchDecline =
     -- | This peer's candidate chain is not longer than our chain. For more
     -- details see
     -- 'Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface.mkBlockFetchConsensusInterface'
     -- which implements 'plausibleCandidateChain'.
     --
     FetchDeclineChainNotPlausible

     -- | Switching to this peer's candidate chain would require rolling back
     -- more than @K@ blocks.
     --
   | FetchDeclineChainIntersectionTooDeep

     -- | Every block on this peer's candidate chain has already been fetched.
     --
   | FetchDeclineAlreadyFetched

     -- | This peer's candidate chain has already been requested from this
     -- peer.
     --
   | FetchDeclineInFlightThisPeer

     -- | Some blocks on this peer's candidate chain have not yet been fetched,
     -- but all of those have already been requested from other peers.
     --
   | FetchDeclineInFlightOtherPeer

     -- | This peer's BlockFetch client is shutting down, see
     -- 'PeerFetchStatusShutdown'.
     --
   | FetchDeclinePeerShutdown

     -- | Blockfetch is starting up and waiting on corresponding Chainsync.
   | FetchDeclinePeerStarting


   -- The reasons above this comment are fundamental and/or obvious. On the
   -- other hand, the reasons below are heuristic.


     -- | This peer is in a potentially-temporary state in which it has not
     -- responded to us within a certain expected time limit, see
     -- 'PeerFetchStatusAberrant'.
     --
   | FetchDeclinePeerSlow

     -- | This peer is not under the 'maxInFlightReqsPerPeer' limit.
     --
     -- The argument is the 'maxInFlightReqsPerPeer' constant.
     --
   | FetchDeclineReqsInFlightLimit  !Word

     -- | This peer is not under the 'inFlightBytesHighWatermark' bytes limit.
     --
     -- The arguments are:
     --
     -- * number of bytes currently in flight for that peer
     -- * the configured 'inFlightBytesLowWatermark' constant
     -- * the configured 'inFlightBytesHighWatermark' constant
     --
   | FetchDeclineBytesInFlightLimit !SizeInBytes !SizeInBytes !SizeInBytes

     -- | This peer is not under the 'inFlightBytesLowWatermark'.
     --
     -- The arguments are:
     --
     -- * number of bytes currently in flight for that peer
     -- * the configured 'inFlightBytesLowWatermark' constant
     -- * the configured 'inFlightBytesHighWatermark' constant
     --
   | FetchDeclinePeerBusy           !SizeInBytes !SizeInBytes !SizeInBytes

     -- | The node is not under the 'maxConcurrentFetchPeers' limit.
     --
     -- The arguments are:
     --
     -- * the current 'FetchMode'
     -- * the corresponding configured limit constant, either
     --   'maxConcurrencyBulkSync' or 'maxConcurrencyDeadline'
     --
   | FetchDeclineConcurrencyLimit   !FetchMode !Word
  deriving (Eq, Show)
