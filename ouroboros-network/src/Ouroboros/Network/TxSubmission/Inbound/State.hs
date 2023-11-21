{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.State
  ( -- * Core API
    SharedTxState (..)
  , PeerTxState (..)
  , SharedTxStateVar
  , newSharedTxStateVar
  , receivedTxIds
  , collectTxs
  , acknowledgeTxIds
  , hasTxIdsToAcknowledge
    -- * Internals, only exported for testing purposes:
  , RefCountDiff (..)
  , updateRefCounts
  , receivedTxIdsImpl
  , collectTxsImpl
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)

import Data.Foldable (fold,
#if MIN_VERSION_base(4,20,0)
         foldl',
#endif
         toList)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)

import NoThunks.Class (NoThunks (..))

import GHC.Stack (HasCallStack)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..))
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..))


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
       unknownTxs               :: !(Set txid)
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
      --    * were downloaded,
      --    * are already in the mempool (`Nothing` is inserted in that case),
      --
      -- We only keep live `txid`, e.g. ones which `txid` is unacknowledged by
      -- at least one peer.
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

      -- | We track reference counts of all unacknowledged txids.  Once the
      -- count reaches 0, a tx is removed from `bufferedTxs`.
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
      referenceCounts :: !(Map txid Int)
    }
    deriving (Eq, Show, Generic)

instance ( NoThunks peeraddr
         , NoThunks tx
         , NoThunks txid
         ) => NoThunks (SharedTxState peeraddr txid tx)

--
-- Pure public API
--

-- | Check if a peer can acknowledge at least one `txid`.
--
hasTxIdsToAcknowledge
    :: forall peeraddr txid tx.
       Ord txid
    => SharedTxState peeraddr txid tx
    -> PeerTxState txid tx
    -> Bool
hasTxIdsToAcknowledge
    SharedTxState { bufferedTxs }
    PeerTxState { unacknowledgedTxIds, unknownTxs }
    =
    -- We just need to look at the front of the unacknowledged `txid`s.
    case unacknowledgedTxIds of
      txid StrictSeq.:<| _ -> txid `Map.member` bufferedTxs
                           || txid `Set.member` unknownTxs
      _                    -> False


acknowledgeTxIds
    :: forall peeraddr tx txid.
       Ord txid
    => SharedTxState peeraddr txid tx
    -> PeerTxState txid tx
    -> (NumTxIdsToAck, [tx], RefCountDiff txid, PeerTxState txid tx)
    -- ^ number of txid to acknowledge, txids to acknowledge with multiplicities,
    -- updated PeerTxState.
{-# INLINE acknowledgeTxIds #-}

acknowledgeTxIds
    SharedTxState { bufferedTxs }
    ps@PeerTxState { availableTxIds,
                     unacknowledgedTxIds,
                     unknownTxs }
    =
    ( fromIntegral $ StrictSeq.length acknowledgedTxIds
    , txsToMempool
    , refCountDiff
    , ps { unacknowledgedTxIds = unacknowledgedTxIds',
           availableTxIds      = availableTxIds',
           unknownTxs          = unknownTxs' }
    )
  where
    -- Split `unacknowledgedTxIds'` into the longest prefix of `txid`s which
    -- can be acknowledged and the unacknowledged `txid`s.
    (acknowledgedTxIds, unacknowledgedTxIds') =
      StrictSeq.spanl (\txid -> txid `Map.member` bufferedTxs
                             || txid `Set.member` unknownTxs
                      )
                      unacknowledgedTxIds

    txsToMempool :: [tx]
    txsToMempool = [ tx
                   | txid <- toList acknowledgedTxIds
                   , Just tx <- maybeToList $ txid `Map.lookup` bufferedTxs
                   ]

    -- the set of live `txids`
    liveSet  = Set.fromList (toList unacknowledgedTxIds')

    availableTxIds' = availableTxIds
                      `Map.restrictKeys`
                      liveSet

    -- We remove all acknowledged `txid`s which are not in
    -- `unacknowledgedTxIds''`, but also return the unknown set before any
    -- modifications (which is used to compute `unacknowledgedTxIds''`
    -- above).
    unknownTxs' = unknownTxs `Set.intersection` liveSet

    refCountDiff = RefCountDiff
                 $ foldr (\txid -> Map.alter fn txid)
                         Map.empty acknowledgedTxIds
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing  = Just 1
        fn (Just n) = Just $! n + 1


-- | `RefCountDiff` represents a map of `txid` which can be acknowledged
-- together with their multiplicities.
--
newtype RefCountDiff txid = RefCountDiff {
    txIdsToAck :: Map txid Int
  }

updateRefCounts :: Ord txid
                => Map txid Int
                -> RefCountDiff txid
                -> Map txid Int
updateRefCounts referenceCounts (RefCountDiff diff) =
    Map.merge (Map.mapMaybeMissing \_ x -> Just x)
              (Map.mapMaybeMissing \_ _ -> Nothing)
              (Map.zipWithMaybeMatched \_ x y -> assert (x >= y)
                                                 if x > y then Just $! x - y
                                                          else Nothing)
              referenceCounts
              diff


--
-- Pure internal API
--

-- | Insert received `txid`s and return the number of txids to be acknowledged
-- and the updated `SharedTxState`.
--
receivedTxIdsImpl
    :: forall peeraddr tx txid.
       (Ord txid, Ord peeraddr, HasCallStack)
    => (txid -> Bool) -- ^ check if txid is in the mempool, ref 'mempoolHasTx'
    -> peeraddr
    -> NumTxIdsToReq
    -- ^ number of requests to subtract from
    -- `requestedTxIdsInflight`

    -> StrictSeq txid
    -- ^ sequence of received `txids`
    -> Map txid SizeInBytes
    -- ^ received `txid`s with sizes

    -> SharedTxState peeraddr txid tx
    -> SharedTxState peeraddr txid tx

receivedTxIdsImpl
    mempoolHasTx
    peeraddr reqNo txidsSeq txidsMap
    st@SharedTxState{ peerTxStates,
                      bufferedTxs,
                      referenceCounts }
    =
    -- using `alterF` so the update of `PeerTxState` is done in one lookup
    case Map.alterF (fmap Just . fn . fromJust)
                    peeraddr
                    peerTxStates of
      ( st', peerTxStates' ) ->
        st' { peerTxStates = peerTxStates' }

  where
    -- update `PeerTxState` and return number of `txid`s to acknowledged and
    -- updated `SharedTxState`.
    fn :: PeerTxState txid tx
       -> ( SharedTxState peeraddr txid tx
          , PeerTxState txid tx
          )
    fn ps@PeerTxState { availableTxIds,
                        requestedTxIdsInflight,
                        unacknowledgedTxIds } =
        (st', ps')
      where
        --
        -- Handle new `txid`s
        --

        -- Divide the new txids in two: those that are already in the mempool
        -- and those that are not. We'll request some txs from the latter.
        (ignoredTxIds, availableTxIdsMap) =
          Map.partitionWithKey
            (\txid _ -> mempoolHasTx txid)
            txidsMap

        -- Add all `txids` from `availableTxIdsMap` which are not
        -- unacknowledged or already buffered. Unacknowledged txids must have
        -- already been added to `availableTxIds` map before.
        availableTxIds' =
          Map.foldlWithKey
            (\m txid sizeInBytes -> Map.insert txid sizeInBytes m)
            availableTxIds
            (Map.filterWithKey
                (\txid _ -> txid `notElem` unacknowledgedTxIds
                         && txid `Map.notMember` bufferedTxs)
                availableTxIdsMap)

        -- Add received txids to `unacknowledgedTxIds`.
        unacknowledgedTxIds' = unacknowledgedTxIds <> txidsSeq

        -- Add ignored `txs` to buffered ones.
        -- Note: we prefer to keep the `tx` if it's already in `bufferedTxs`.
        bufferedTxs' = bufferedTxs
                    <> Map.map (const Nothing) ignoredTxIds

        referenceCounts' =
          foldl' (flip $ Map.alter (\case
                                       Nothing  -> Just $! 1
                                       Just cnt -> Just $! succ cnt))
                 referenceCounts
                 txidsSeq

        st' = st { bufferedTxs     = bufferedTxs',
                   referenceCounts = referenceCounts' }
        ps' = assert (requestedTxIdsInflight >= reqNo)
              ps { availableTxIds         = availableTxIds',
                   unacknowledgedTxIds    = unacknowledgedTxIds',
                   requestedTxIdsInflight = requestedTxIdsInflight - reqNo }


collectTxsImpl
    :: forall peeraddr tx txid.
       (Ord txid, Ord peeraddr)
    => peeraddr
    -> Set txid    -- ^ set of requested txids
    -> Map txid tx -- ^ received txs
    -> SharedTxState peeraddr txid tx
    -> SharedTxState peeraddr txid tx
    -- ^ number of `txid`s to be acknowledged, `tx`s to be added to
    -- the mempool and updated state.
collectTxsImpl peeraddr requestedTxIds receivedTxs
               st@SharedTxState { peerTxStates } =

    -- using `alterF` so the update of `PeerTxState` is done in one lookup
    case Map.alterF (fmap Just . fn . fromJust)
                    peeraddr
                    peerTxStates of
      (st', peerTxStates') ->
        st' { peerTxStates = peerTxStates' }

  where
    -- Update `PeerTxState` and partially update `SharedTxState` (except of
    -- `peerTxStates`).
    fn :: PeerTxState txid tx
       -> ( SharedTxState peeraddr txid tx
          , PeerTxState txid tx
          )
    fn ps = (st'', ps'')
      where
        notReceived = requestedTxIds Set.\\ Map.keysSet receivedTxs

        -- add received `tx`s to buffered map
        bufferedTxs' = bufferedTxs st
                    <> Map.map Just receivedTxs

        -- Add not received txs to `unknownTxs` before acknowledging txids.
        unknownTxs'  = unknownTxs ps <> notReceived

        requestedTxsInflight' =
          assert (requestedTxIds `Set.isSubsetOf` requestedTxsInflight ps) $
          requestedTxsInflight ps  Set.\\ requestedTxIds

        requestedSize = fold $ availableTxIds ps `Map.restrictKeys` requestedTxIds
        requestedTxsInflightSize' =
          -- TODO: VALIDATE size of received txs against what was announced
          -- earlier;
          assert (requestedTxsInflightSize ps >= requestedSize) $
          requestedTxsInflightSize ps - requestedSize

        st' = st { bufferedTxs = bufferedTxs' }

        -- subtract requested from in-flight
        inflightTxs'' =
          Map.merge
            (Map.mapMaybeMissing \_ x -> Just x)
            (Map.mapMaybeMissing \_ _ -> assert False Nothing)
            (Map.zipWithMaybeMatched \_ x y -> assert (x >= y)
                                               let z = x - y in
                                               if z > 0
                                               then Just z
                                               else Nothing)
            (inflightTxs st')
            (Map.fromSet (const 1) requestedTxIds)

        inflightTxsSize'' = assert (inflightTxsSize st' >= requestedSize) $
                            inflightTxsSize st' - requestedSize

        st'' = st' { inflightTxs     = inflightTxs'',
                     inflightTxsSize = inflightTxsSize''
                   }

        --
        -- Update PeerTxState
        --

        -- Remove the downloaded `txid`s from the availableTxIds map, this
        -- guarantees that we won't attempt to download the `txids` from this peer
        -- once we collect the `txid`s. Also restrict keys to `liveSet`.
        --
        -- NOTE: we could remove `notReceived` from `availableTxIds`; and
        -- possibly avoid using `unknownTxs` field at all.
        --
        availableTxIds'' = availableTxIds ps
                           `Map.withoutKeys`
                           requestedTxIds

        -- Remove all acknowledged `txid`s from unknown set, but only those
        -- which are not present in `unacknowledgedTxIds'`
        unknownTxs'' = unknownTxs'
                       `Set.intersection`
                       live
          where
            -- We cannot use `liveSet` as `unknown <> notReceived` might
            -- contain `txids` which are in `liveSet` but are not `live`.
            live = Set.fromList (toList (unacknowledgedTxIds ps))

        ps'' = ps { availableTxIds           = availableTxIds'',
                    unknownTxs               = unknownTxs'',
                    requestedTxsInflightSize = requestedTxsInflightSize',
                    requestedTxsInflight     = requestedTxsInflight' }

--
-- Monadic public API
--

type SharedTxStateVar m peeraddr txid tx = StrictTVar m (SharedTxState peeraddr txid tx)

newSharedTxStateVar :: MonadSTM m
                    => m (SharedTxStateVar m peeraddr txid tx)
newSharedTxStateVar = newTVarIO SharedTxState { peerTxStates    = Map.empty,
                                                inflightTxs     = Map.empty,
                                                inflightTxsSize = 0,
                                                bufferedTxs     = Map.empty,
                                                referenceCounts = Map.empty }


-- | Acknowledge `txid`s, return the number of `txids` to be acknowledged to the
-- remote side.
--
receivedTxIds
  :: forall m peeraddr idx tx txid.
     (MonadSTM m, Ord txid, Ord peeraddr)
  => SharedTxStateVar m peeraddr txid tx
  -> MempoolSnapshot txid tx idx
  -> peeraddr
  -> NumTxIdsToReq
  -- ^ number of requests to subtract from
  -- `requestedTxIdsInflight`
  -> StrictSeq txid
  -- ^ sequence of received `txids`
  -> Map txid SizeInBytes
  -- ^ received `txid`s with sizes
  -> m ()
receivedTxIds sharedVar MempoolSnapshot{mempoolHasTx} peeraddr reqNo txidsSeq txidsMap =
  atomically $
    modifyTVar sharedVar (receivedTxIdsImpl mempoolHasTx peeraddr reqNo txidsSeq txidsMap)


-- | Include received `tx`s in `SharedTxState`.  Return number of `txids`
-- to be acknowledged and list of `tx` to be added to the mempool.
--
collectTxs
  :: forall m peeraddr tx txid.
     (MonadSTM m, Ord txid, Ord peeraddr)
  => SharedTxStateVar m peeraddr txid tx
  -> peeraddr
  -> Set txid    -- ^ set of requested txids
  -> Map txid tx -- ^ received txs
  -> m ()
  -- ^ number of txids to be acknowledged and txs to be added to the
  -- mempool
collectTxs sharedVar peeraddr txidsRequested txsMap =
  atomically $
    modifyTVar sharedVar
               (collectTxsImpl peeraddr txidsRequested txsMap)
