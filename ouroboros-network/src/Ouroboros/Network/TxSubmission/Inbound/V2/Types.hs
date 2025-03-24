{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Types
  ( -- * PeerTxState
    PeerTxState (..)
    -- * SharedTxState
  , SharedTxState (..)
    -- * Decisions
  , TxsToMempool (..)
  , TxDecision (..)
  , emptyTxDecision
  , SharedDecisionContext (..)
  , TraceTxLogic (..)
    -- * Types shared with V1
    -- ** Various
  , ProcessedTxCount (..)
  , TxSubmissionLogicVersion (..)
    -- ** Mempool API
  , TxSubmissionMempoolWriter (..)
    -- ** Traces
  , TraceTxSubmissionInbound (..)
    -- ** Protocol Error
  , TxSubmissionProtocolError (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad.Class.MonadTime.SI
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.Random (StdGen)

import NoThunks.Class (NoThunks (..))

import Ouroboros.Network.DeltaQ (PeerGSV (..))
import Ouroboros.Network.Protocol.TxSubmission2.Type

-- | Flag to enable/disable the usage of the new tx-submission logic.
--
data TxSubmissionLogicVersion =
      -- | the legacy `Ouroboros.Network.TxSubmission.Inbound.V1`
      TxSubmissionLogicV1
      -- | the new `Ouroboros.Network.TxSubmission.Inbound.V2`
    | TxSubmissionLogicV2
    deriving (Eq, Enum, Bounded, Show)

--
-- PeerTxState, SharedTxState
--

data PeerTxState txid tx = PeerTxState {
       -- | Those transactions (by their identifier) that the client has told
       -- us about, and which we have not yet acknowledged. This is kept in
       -- the order in which the client gave them to us. This is the same order
       -- in which we submit them to the mempool (or for this example, the final
       -- result order). It is also the order we acknowledge in.
       --
       unacknowledgedTxIds      :: !(StrictSeq txid),

       -- | Set of known transaction ids which can be requested from this peer.
       --
       availableTxIds           :: !(Map txid SizeInBytes),

       -- | The number of transaction identifiers that we have requested but
       -- which have not yet been replied to. We need to track this it keep
       -- our requests within the limit on the number of unacknowledged txids.
       --
       requestedTxIdsInflight   :: !NumTxIdsToReq,

       -- | The size in bytes of transactions that we have requested but which
       -- have not yet been replied to. We need to track this it keep our
       -- requests within the limit on the number of unacknowledged txids.
       --
       requestedTxsInflightSize :: !SizeInBytes,

       -- | The set of requested `txid`s.
       --
       requestedTxsInflight     :: !(Set txid),

       -- | A subset of `unacknowledgedTxIds` which were unknown to the peer.
       -- We need to track these `txid`s since they need to be acknowledged.
       --
       -- We track these `txid` per peer, rather than in `bufferedTxs` map,
       -- since that could potentially lead to corrupting the node, not being
       -- able to download a `tx` which is needed & available from other nodes.
       --
       unknownTxs               :: !(Set txid),

       -- | Score is a metric that tracks how usefull a peer has been.
       -- The larger the value the less usefull peer. It slowly decays towards
       -- zero.
       score                    :: !Double,

       -- | Timestamp for the last time `score` was drained.
       scoreTs                  :: !Time,

       -- | A set of TXs downloaded from the peer. They are not yet
       -- acknowledged and haven't been sent to the mempool yet.
       --
       -- Life cycle of entries:
       -- * added when a tx is downloaded (see `collectTxsImpl`)
       -- * follows `unacknowledgedTxIds` (see `acknowledgeTxIds`)
       --
       downloadedTxs            :: !(Map txid tx),

       -- | A set of TXs on their way to the mempool.
       -- Tracked here so that we can cleanup `limboTxs` if the peer dies.
       --
       -- Life cycle of entries:
       -- * added by `acknowledgeTxIds` (where decide which txs can be
       --   submitted to the mempool)
       -- * removed by `withMempoolSem`
       --
       toMempoolTxs             :: !(Map txid tx)

    }
    deriving (Eq, Show, Generic)

instance ( NoThunks txid
         , NoThunks tx
         ) => NoThunks (PeerTxState txid tx)


-- | Shared state of all `TxSubmission` clients.
--
-- New `txid` enters `unacknowledgedTxIds` it is also added to `availableTxIds`
-- and `referenceCounts` (see `acknowledgeTxIdsImpl`).
--
-- When a `txid` id is selected to be downloaded, it's added to
-- `requestedTxsInflightSize` (see
-- `Ouroboros.Network.TxSubmission.Inbound.Decision.pickTxsToDownload`).
--
-- When the request arrives, the `txid` is removed from `inflightTxs`.  It
-- might be added to `unknownTxs` if the server didn't have that `txid`, or
-- it's added to `bufferedTxs` (see `collectTxsImpl`).
--
-- Whenever we choose `txid` to acknowledge (either in `acknowledtxsIdsImpl`,
-- `collectTxsImpl` or
-- `Ouroboros.Network.TxSubmission.Inbound.Decision.pickTxsToDownload`, we also
-- recalculate `referenceCounts` and only keep live `txid`s in other maps (e.g.
-- `availableTxIds`, `bufferedTxs`, `unknownTxs`).
--
data SharedTxState peeraddr txid tx = SharedTxState {

      -- | Map of peer states.
      --
      -- /Invariant:/ for peeraddr's which are registered using `withPeer`,
      -- there's always an entry in this map even if the set of `txid`s is
      -- empty.
      --
      peerTxStates    :: !(Map peeraddr (PeerTxState txid tx)),

      -- | Set of transactions which are in-flight (have already been
      -- requested) together with multiplicities (from how many peers it is
      -- currently in-flight)
      --
      -- This set can intersect with `availableTxIds`.
      --
      inflightTxs     :: !(Map txid Int),

      -- | Overall size of all `tx`s in-flight.
      --
      inflightTxsSize :: !SizeInBytes,

      -- | Map of `tx` which:
      --
      --    * were downloaded and added to the mempool,
      --    * are already in the mempool (`Nothing` is inserted in that case),
      --
      -- We only keep live `txid`, e.g. ones which `txid` is unacknowledged by
      -- at least one peer or has a `timedTxs` entry.
      --
      -- /Note:/ `txid`s which `tx` were unknown by a peer are tracked
      -- separately in `unknownTxs`.
      --
      -- /Note:/ previous implementation also needed to explicitly tracked
      -- `txid`s which were already acknowledged, but are still unacknowledged.
      -- In this implementation, this is done due to reference counting.
      --
      -- This map is useful to acknowledge `txid`s, it's basically taking the
      -- longest prefix which contains entries in `bufferedTxs` or `unknownTxs`.
      --
      bufferedTxs     :: !(Map txid (Maybe tx)),

      -- | We track reference counts of all unacknowledged and timedTxs txids.
      -- Once the count reaches 0, a tx is removed from `bufferedTxs`.
      --
      -- The `bufferedTx` map contains a subset of `txid` which
      -- `referenceCounts` contains.
      --
      -- /Invariants:/
      --
      --    * the txid count is equal to multiplicity of txid in all
      --      `unacknowledgedTxIds` sequences;
      --    * @Map.keysSet bufferedTxs `Set.isSubsetOf` Map.keysSet referenceCounts@;
      --    * all counts are positive integers.
      --
      referenceCounts :: !(Map txid Int),

      -- | A set of timeouts for txids that have been added to bufferedTxs after being
      -- inserted into the mempool.
      -- Every txid entry has a reference count in `referenceCounts`.
      timedTxs        :: Map Time [txid],

      -- | A set of txids that have been downloaded by a peer and are on their
      -- way to the mempool. We won't issue further fetch-requests for TXs in
      -- this state.  We track these txs to not re-download them from another
      -- peer.
      limboTxs        :: !(Map txid Int),

      -- | Rng used to randomly order peers
      peerRng         :: !StdGen
    }
    deriving (Eq, Show, Generic)

instance ( NoThunks peeraddr
         , NoThunks tx
         , NoThunks txid
         , NoThunks StdGen
         ) => NoThunks (SharedTxState peeraddr txid tx)


--
-- Decisions
--

newtype TxsToMempool txid tx = TxsToMempool { listOfTxsToMempool :: [(txid, tx)] }
  deriving newtype (Eq, Show, Semigroup, Monoid)


-- | Decision made by the decision logic.  Each peer will receive a 'Decision'.
--
-- /note:/ it is rather non-standard to represent a choice between requesting
-- `txid`s and `tx`'s as a product rather than a sum type.  The client will
-- need to download `tx`s first and then send a request for more txids (and
-- acknowledge some `txid`s).   Due to pipelining each client will request
-- decision from the decision logic quite often (every two pipelined requests),
-- but with this design a decision once taken will make the peer non-active
-- (e.g. it won't be returned by `filterActivePeers`) for longer, and thus the
-- expensive `makeDecision` computation will not need to take that peer into
-- account.
--
data TxDecision txid tx = TxDecision {
    txdTxIdsToAcknowledge :: !NumTxIdsToAck,
    -- ^ txid's to acknowledge

    txdTxIdsToRequest     :: !NumTxIdsToReq,
    -- ^ number of txid's to request

    txdPipelineTxIds      :: !Bool,
    -- ^ the tx-submission protocol only allows to pipeline `txid`'s requests
    -- if we have non-acknowledged `txid`s.

    txdTxsToRequest       :: !(Set txid),
    -- ^ txid's to download.

    txdTxsToMempool       :: !(TxsToMempool txid tx)
    -- ^ list of `tx`s to submit to the mempool.
  }
  deriving (Show, Eq)

-- | A non-commutative semigroup instance.
--
-- /note:/ this instance must be consistent with `pickTxsToDownload` and how
-- `PeerTxState` is updated.  It is designed to work with `TMergeVar`s.
--
instance Ord txid => Semigroup (TxDecision txid tx) where
    TxDecision { txdTxIdsToAcknowledge,
                 txdTxIdsToRequest,
                 txdPipelineTxIds = _ignored,
                 txdTxsToRequest,
                 txdTxsToMempool }
      <>
      TxDecision { txdTxIdsToAcknowledge = txdTxIdsToAcknowledge',
                   txdTxIdsToRequest     = txdTxIdsToRequest',
                   txdPipelineTxIds      = txdPipelineTxIds',
                   txdTxsToRequest       = txdTxsToRequest',
                   txdTxsToMempool       = txdTxsToMempool' }
      =
      TxDecision { txdTxIdsToAcknowledge = txdTxIdsToAcknowledge + txdTxIdsToAcknowledge',
                   txdTxIdsToRequest     = txdTxIdsToRequest + txdTxIdsToRequest',
                   txdPipelineTxIds      = txdPipelineTxIds',
                   txdTxsToRequest       = txdTxsToRequest <> txdTxsToRequest',
                   txdTxsToMempool       = txdTxsToMempool <> txdTxsToMempool'
                 }

-- | A no-op decision.
emptyTxDecision :: TxDecision txid tx
emptyTxDecision = TxDecision {
    txdTxIdsToAcknowledge = 0,
    txdTxIdsToRequest     = 0,
    txdPipelineTxIds      = False,
    txdTxsToRequest       = Set.empty,
    txdTxsToMempool       = mempty
  }

data SharedDecisionContext peeraddr txid tx = SharedDecisionContext {
    -- TODO: check how to access it.
    sdcPeerGSV       :: !(Map peeraddr PeerGSV),

    sdcSharedTxState :: !(SharedTxState peeraddr txid tx)
  }
  deriving Show


-- | TxLogic tracer.
--
data TraceTxLogic peeraddr txid tx =
    TraceSharedTxState String (SharedTxState peeraddr txid tx)
  | TraceTxDecisions (Map peeraddr (TxDecision txid tx))
  deriving Show


data ProcessedTxCount = ProcessedTxCount {
      -- | Just accepted this many transactions.
      ptxcAccepted :: Int
      -- | Just rejected this many transactions.
    , ptxcRejected :: Int
    , ptxcScore    :: Double
    }
  deriving (Eq, Show)


-- | The consensus layer functionality that the inbound side of the tx
-- submission logic requires.
--
-- This is provided to the tx submission logic by the consensus layer.
--
data TxSubmissionMempoolWriter txid tx idx m =
     TxSubmissionMempoolWriter {

       -- | Compute the transaction id from a transaction.
       --
       -- This is used in the protocol handler to verify a full transaction
       -- matches a previously given transaction id.
       --
       txId          :: tx -> txid,

       -- | Supply a batch of transactions to the mempool. They are either
       -- accepted or rejected individually, but in the order supplied.
       --
       -- The 'txid's of all transactions that were added successfully are
       -- returned.
       mempoolAddTxs :: [tx] -> m [txid]
    }


data TraceTxSubmissionInbound txid tx =
    -- | Number of transactions just about to be inserted.
    TraceTxSubmissionCollected Int
    -- | Just processed transaction pass/fail breakdown.
  | TraceTxSubmissionProcessed ProcessedTxCount
    -- | Server received 'MsgDone'
  | TraceTxInboundCanRequestMoreTxs Int
  | TraceTxInboundCannotRequestMoreTxs Int
  | TraceTxInboundAddedToMempool [txid] DiffTime

  --
  -- messages emitted by the new implementation of the server in
  -- "Ouroboros.Network.TxSubmission.Inbound.Server"; some of them are also
  -- used in this module.
  --

  | TraceTxInboundTerminated
  | TraceTxInboundDecision (TxDecision txid tx)
  deriving (Eq, Show)


data TxSubmissionProtocolError =
       ProtocolErrorTxNotRequested
     | ProtocolErrorTxIdsNotRequested
     | forall txid. (Typeable txid, Show txid)
       => ProtocolErrorTxSizeError [(txid, SizeInBytes, SizeInBytes)]
     -- ^ a list of txid for which the received size and advertised size didn't
     -- match.

deriving instance Show TxSubmissionProtocolError

instance Exception TxSubmissionProtocolError where
  displayException ProtocolErrorTxNotRequested =
      "The peer replied with a transaction we did not ask for."
  displayException ProtocolErrorTxIdsNotRequested =
      "The peer replied with more txids than we asked for."
  displayException (ProtocolErrorTxSizeError txids) =
      "The peer received txs with wrong sizes " ++ show txids
