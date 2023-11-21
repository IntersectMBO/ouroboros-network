{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.State
  ( -- * Shared state
    SharedTxStateVar
  , newSharedTxStateVar
  , hasTxIdsToAcknowledge
  , receivedTxIds
  , collectTxs
    -- * Peer State API
  , PeerTxState (..)
  , acknowledgeTxIds
  , PeerTxStateAPI (..)
  , withPeer
    -- * API to modify state
  , getTxsToRequest
  , getNumTxIdsToRequest
  , canRequestMoreTxs
  , setRequestInflight
    -- * Internals, only exported for testing purposes:
  , SharedTxState (..)
  , RefCountDiff (..)
  , updateRefCounts
  , receivedTxIdsImpl
  , collectTxsImpl
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadThrow

import Data.Foldable (fold, foldl', toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Map.Merge.Strict qualified as Map
import Data.Maybe (fromJust, catMaybes)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word16)
import GHC.Generics (Generic)

import NoThunks.Class (NoThunks (..))

import Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..))


data PeerTxState txid tx = PeerTxState {
       -- | Those transactions (by their identifier) that the client has told
       -- us about, and which we have not yet acknowledged. This is kept in
       -- the order in which the client gave them to us. This is the same order
       -- in which we submit them to the mempool (or for this example, the final
       -- result order). It is also the order we acknowledge in.
       --
       unacknowledgedTxIds     :: !(StrictSeq txid),

       -- | Set of known transaction ids which can be requested from this peer.
       --
       availableTxIds          :: !(Map txid SizeInBytes),

       -- | The number of transaction identifiers that we have requested but
       -- which have not yet been replied to. We need to track this it keep
       -- our requests within the limit on the number of unacknowledged txids.
       --
       requestedTxIdsInflight  :: !Word16,

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
       unknownTxs              :: !(Set txid)
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
      peerTxStates         :: !(Map peeraddr (PeerTxState txid tx)),

      -- | Set of transactions which are in flight (have already been
      -- requested) together with multiplicities (from how many peers it is
      -- currently inflight)
      --
      -- This set can intersect with `availableTxIds`.
      --
      inflightTxs          :: !(Map txid Int),

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
      bufferedTxs          :: !(Map txid (Maybe tx)),

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
      referenceCounts      :: !(Map txid Int)
    }
    deriving (Eq, Show, Generic)

instance ( NoThunks peeraddr
         , NoThunks tx
         , NoThunks txid
         ) => NoThunks (SharedTxState peeraddr txid tx)

--
-- Pure API (internal)
--

-- TODO: remove.
getTxsToRequestImpl
  :: ( Ord txid
     , Ord peeraddr
     )
  => Word16 -- ^ max txs to request
  -> peeraddr
  -> SharedTxState peeraddr txid tx
  -> (Set txid, SharedTxState peeraddr txid tx)
getTxsToRequestImpl maxTxToRequest
                    peeraddr
                    st@SharedTxState { peerTxStates,
                                       inflightTxs } =
    ( txsToRequest
    , st { peerTxStates = peerTxStates',
           inflightTxs  = inflightTxs <> Map.fromSet (const 1) txsToRequest
         }
    )
  where
    PeerTxState { unacknowledgedTxIds, availableTxIds } =
      peerTxStates Map.! peeraddr

    toSet = foldl' (flip Set.insert) Set.empty

    availableTxIds' =
      availableTxIds
      `Map.restrictKeys`
      toSet unacknowledgedTxIds

    txsToRequest = Map.keysSet $ Map.take (fromIntegral maxTxToRequest) availableTxIds

    size = fold (availableTxIds `Map.restrictKeys` txsToRequest)

    peerTxStates' = Map.adjust
                      (\pts -> pts { availableTxIds = availableTxIds',
                                     requestedTxsInflightSize = requestedTxsInflightSize pts + size
                                   })
                      peeraddr
                      peerTxStates


-- TODO: can this be removed?
canRequestMoreTxsImpl
  :: ( Ord txid
     , Ord peeraddr
     )
  => peeraddr
  -> SharedTxState peeraddr txid tx
  -> Bool
canRequestMoreTxsImpl peeraddr SharedTxState { peerTxStates } =
    not (Map.null availableTxs)
  where
    PeerTxState { unacknowledgedTxIds, availableTxIds } = peerTxStates Map.! peeraddr

    toSet = foldl' (flip Set.insert) Set.empty

    availableTxs =
      availableTxIds
      `Map.restrictKeys`
      toSet unacknowledgedTxIds


getNumTxIdsToRequestImpl
  :: Word16 -- ^ max unacknowledged `tx`s
  -> Word16 -- ^ max `txid`s to request
  -> PeerTxState txid tx
  -> (Word16, PeerTxState txid tx)
getNumTxIdsToRequestImpl maxUnacked maxTxIdsToRequest
                         ps@PeerTxState { requestedTxIdsInflight,
                                          unacknowledgedTxIds } =
    let numTxIdsToRequest =
          ( maxUnacked
          - fromIntegral (StrictSeq.length unacknowledgedTxIds)
          - requestedTxIdsInflight
          )
          `min` maxTxIdsToRequest
    in ( numTxIdsToRequest
       , ps { requestedTxIdsInflight = requestedTxIdsInflight
                                     + numTxIdsToRequest }
       )


setRequestInflightImpl :: Word16
                       -> PeerTxState txid tx
                       -> PeerTxState txid tx
setRequestInflightImpl reqno st =
    st { requestedTxIdsInflight = reqno }


-- | Check if a peer can acknowledge at least one `txid`.
--
hasTxIdsToAcknowledge :: forall peeraddr txid tx.
                         ( Ord txid
                         , Ord peeraddr
                         )
                      => peeraddr
                      -> SharedTxState peeraddr txid tx
                      -> StrictSeq txid
                      -> Bool
hasTxIdsToAcknowledge peeraddr SharedTxState { bufferedTxs, peerTxStates }
                      unacknowledgedTxIds =
    -- We just need to look at the front of the unacknowledged `txid`s.
    case unacknowledgedTxIds of
      txid StrictSeq.:<| _ -> canAck txid
      _                    -> False
  where
    unknownTxs' :: Set txid
    unknownTxs' = unknownTxs (peerTxStates Map.! peeraddr)

    canAck :: txid -> Bool
    canAck txid = txid `Map.member` bufferedTxs
               || txid `Set.member` unknownTxs'


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


acknowledgeTxIds
  :: forall peeraddr tx txid.
     Ord txid
  => SharedTxState peeraddr txid tx
  -> PeerTxState txid tx
  -> (Int, RefCountDiff txid, PeerTxState txid tx)
  -- ^ number of txid to acknowledge, txids to acknowledge with multiplicities,
  -- updated PeerTxState.
{-# INLINE acknowledgeTxIds #-}

acknowledgeTxIds
    SharedTxState { bufferedTxs }
    ps@PeerTxState { availableTxIds,
                     unacknowledgedTxIds,
                     unknownTxs }
    = 
    ( StrictSeq.length acknowledgedTxIds
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


-- | Insert received `txid`s and return the number of txids to be acknowledged
-- and the updated `SharedTxState`.
--
receivedTxIdsImpl
  :: forall peeraddr tx txid.
     (Ord txid, Ord peeraddr)
  => (txid -> Bool) -- ^ check if txid is in the mempool, ref 'mempoolHasTx'
  -> peeraddr
  -> Word16
  -- ^ number of requests to subtract from
  -- `requestedTxIdsInflight`

  -> StrictSeq txid
  -- ^ sequence of received `txids`
  -> Map txid SizeInBytes
  -- ^ received `txid`s with sizes

  -> SharedTxState peeraddr txid tx
  -> (Int, SharedTxState peeraddr txid tx)
  -- ^ number of txids to be acknowledged to `peeraddr` and
  -- updated `SharedTxState`

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
      ( (numTxIdsToAck, st'), peerTxStates' ) ->
        (numTxIdsToAck, st' { peerTxStates = peerTxStates' })

  where
    -- update `PeerTxState` and return number of `txid`s to acknowledged and
    -- updated `SharedTxState`.
    fn :: PeerTxState txid tx
       -> ( (Int, SharedTxState peeraddr txid tx)
          , PeerTxState txid tx
          )
    fn ps@PeerTxState { availableTxIds,
                        requestedTxIdsInflight,
                        unacknowledgedTxIds } =
        ((numTxIdsToAck, st''), ps'')
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

        --
        -- Acknowledge `txid`s
        --

        st' = st { bufferedTxs     = bufferedTxs',
                   referenceCounts = referenceCounts' }
        ps' = ps { availableTxIds         = availableTxIds',
                   unacknowledgedTxIds    = unacknowledgedTxIds',
                   requestedTxIdsInflight = requestedTxIdsInflight - reqNo }

        (numTxIdsToAck, refCountDiff, ps'')
          = acknowledgeTxIds st' ps'

        referenceCounts'' = referenceCounts' `updateRefCounts` refCountDiff
        bufferedTxs''     = bufferedTxs'
                            `Map.restrictKeys`
                            Map.keysSet referenceCounts''

        -- final `SharedTxState`
        st'' = st' { referenceCounts = referenceCounts'',
                     bufferedTxs     = bufferedTxs'' }


collectTxsImpl
  :: forall peeraddr tx txid.
     (Ord txid, Ord peeraddr)
  => peeraddr
  -> Set txid    -- ^ set of requested txids
  -> Map txid tx -- ^ received txs
  -> SharedTxState peeraddr txid tx
  -> ((Int, [tx]), SharedTxState peeraddr txid tx)
  -- ^ number of `txid`s to be acknowledged, `tx`s to be added to
  -- the mempool and updated state.
collectTxsImpl peeraddr requestedTxIds receivedTxs
               st@SharedTxState { peerTxStates } =

    -- using `alterF` so the update of `PeerTxState` is done in one lookup
    case Map.alterF (fmap Just . fn . fromJust)
                    peeraddr
                    peerTxStates of
      ((res, st'), peerTxStates') ->
        (res, st' { peerTxStates = peerTxStates' })

  where
    -- Update `PeerTxState` and partially update `SharedTxState` (except of
    -- `peerTxStates`).
    fn :: PeerTxState txid tx
       -> ( ( (Int, [tx])
            , SharedTxState peeraddr txid tx
            )
          , PeerTxState txid tx
          )
    fn ps = (((numTxIdsToAck, txsReady), st''), ps'')
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

        requestedTxsInflightSize' =
              -- TODO: VALIDATE size of received txs against what was announced
              -- earlier;
              assert (requestedTxsInflightSize ps >= requestedSize) $
              requestedTxsInflightSize ps - requestedSize
            where
              requestedSize =
                fold $ availableTxIds ps `Map.restrictKeys` requestedTxIds

        st' = st { bufferedTxs = bufferedTxs' }

        -- Acknowledge txids.
        --
        -- We have to update the unacknowledgedTxIds here eagerly and not
        -- delay it to serverReqTxs, otherwise we could end up blocking in
        -- serverIdle on more pipelined results rather than being able to
        -- move on.
        (numTxIdsToAck, refCountDiff@RefCountDiff { txIdsToAck }, ps') =
          acknowledgeTxIds st' ps { unknownTxs               = unknownTxs',
                                    requestedTxsInflight     = requestedTxsInflight',
                                    requestedTxsInflightSize = requestedTxsInflightSize' }

        -- we can submit the acknowledged txs to our local mempool
        txsReady :: [tx]
        txsReady = catMaybes . Map.elems
                 $ bufferedTxs'
                   `Map.restrictKeys`
                   Map.keysSet txIdsToAck

        --
        -- Update SharedTxState
        --

        referenceCounts'' =
          referenceCounts st'
          `updateRefCounts`
          refCountDiff

        liveSet = Map.keysSet referenceCounts''

        bufferedTxs'' = bufferedTxs'
                        `Map.restrictKeys`
                        liveSet

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

        st'' = st' { inflightTxs          = inflightTxs'',
                     bufferedTxs          = bufferedTxs'',
                     referenceCounts      = referenceCounts'' }

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
        availableTxIds'' = availableTxIds ps'
                           `Map.withoutKeys`
                           requestedTxIds
                           `Map.restrictKeys`
                           liveSet

        -- Remove all acknowledged `txid`s from unknown set, but only those
        -- which are not present in `unacknowledgedTxIds'`
        unknownTxs'' = unknownTxs'
                       `Set.intersection`
                       live
          where
            -- We cannot use `liveSet` as `unknown <> notReceived` might
            -- contain `txids` which are in `liveSet` but are not `live`.
            live = Set.fromList (toList (unacknowledgedTxIds ps'))

        ps'' = ps' { availableTxIds           = availableTxIds'',
                     unknownTxs               = unknownTxs'' }

--
-- Monadic API (public)
--

type SharedTxStateVar m peeraddr txid tx = StrictTVar m (SharedTxState peeraddr txid tx)


-- | API to access `PeerTxState` inside `PeerTxStateVar`.
--
data PeerTxStateAPI m peeraddr txid tx = PeerTxStateAPI {
    readPeerTxState   :: STM m (PeerTxState txid tx),
    -- ^ read peer state

    writePeerTxState  :: PeerTxState txid tx
                      -> STM m (),
    -- ^ write peer state

    modifyPeerTxState :: (PeerTxState txid tx -> PeerTxState txid tx)
                      -> STM m (),
    -- ^ modify peer state

    statePeerTxState  :: forall a. (PeerTxState txid tx -> (a, PeerTxState txid tx))
                      -> STM m a
    -- ^ modify peer state and return a computed value
  }


-- | Smart constructor for `PeerTxStateAPI`
--
mkPeerTxStateAPI :: ( MonadSTM m
                    , Ord peeraddr
                    )
                 => SharedTxStateVar m peeraddr txid tx
                 -> peeraddr
                 -> PeerTxStateAPI m peeraddr txid tx
mkPeerTxStateAPI sharedVar peeraddr =
  PeerTxStateAPI {
     readPeerTxState   = (Map.! peeraddr) . peerTxStates <$> readTVar sharedVar,

     writePeerTxState  =
       \ps -> modifyTVar sharedVar
                \st -> st { peerTxStates = Map.insert peeraddr ps (peerTxStates st) },

     modifyPeerTxState =
       \f  -> modifyTVar sharedVar
                \st -> st { peerTxStates = Map.adjust f peeraddr (peerTxStates st) },

     -- note: use of partial function `fromJust` is safe inside the `withPeer` bracket
     statePeerTxState  =
       \f  -> stateTVar sharedVar
                \st -> let (a, peerTxStates') =
                             Map.alterF (fmap Just . f . fromJust)
                                        peeraddr
                                      (peerTxStates st)
                       in (a, st { peerTxStates = peerTxStates' })
  }


newSharedTxStateVar :: MonadSTM m
                    => m (SharedTxStateVar m peeraddr txid tx)
newSharedTxStateVar = newTVarIO SharedTxState { peerTxStates    = Map.empty,
                                                inflightTxs     = Map.empty,
                                                bufferedTxs     = Map.empty,
                                                referenceCounts = Map.empty }

-- | A bracket function which registers / de-registers a new peer in
-- `SharedTxStateVar` and `PeerTxStateVar`s,  which exposes `PeerTxStateAPI`.
-- `PeerTxStateAPI` is only safe inside the  `withPeer` scope.
--
withPeer :: forall peeraddr txid tx m a.
            ( MonadMask m
            , MonadSTM  m
            , Ord txid
            , Ord peeraddr
            )
         => SharedTxStateVar m peeraddr txid tx
         -> peeraddr
         --  ^ new peer
         -> (PeerTxStateAPI m peeraddr txid tx -> m a)
         -- ^ callback which gives access to `PeerTxStateAPI`
         -> m a
withPeer sharedStateVar peeraddr io =
    bracket
      (atomically $ do
          modifyTVar sharedStateVar registerPeer
          return (mkPeerTxStateAPI sharedStateVar peeraddr)
      )
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      (\PeerTxStateAPI { readPeerTxState } -> uninterruptibleMask_ $ atomically $ do
        PeerTxState { unacknowledgedTxIds = unacked } <- readPeerTxState
        modifyTVar sharedStateVar (unregisterPeer unacked)
      )
      io
  where
    registerPeer :: SharedTxState peeraddr txid tx
                 -> SharedTxState peeraddr txid tx
    registerPeer st@SharedTxState { peerTxStates } =
      st { peerTxStates = Map.insert peeraddr
                                     PeerTxState { availableTxIds           = Map.empty,
                                                   requestedTxIdsInflight   = 0,
                                                   requestedTxsInflightSize = 0,
                                                   requestedTxsInflight     = Set.empty,
                                                   unacknowledgedTxIds      = StrictSeq.empty,
                                                   unknownTxs               = Set.empty }
                                     peerTxStates
         }

    unregisterPeer :: StrictSeq txid
                   -> SharedTxState peeraddr txid tx
                   -> SharedTxState peeraddr txid tx
    unregisterPeer unacked
                   st@SharedTxState { peerTxStates,
                                      bufferedTxs,
                                      referenceCounts } =
        st { peerTxStates    = peerTxStates',
             bufferedTxs     = bufferedTxs',
             referenceCounts = referenceCounts' }
      where
        peerTxStates' = Map.delete peeraddr peerTxStates

        referenceCounts' =
          foldl' (flip $ Map.update
                             \cnt -> if cnt > 1
                                     then Just $! pred cnt
                                     else Nothing)
          referenceCounts
          unacked

        liveSet = Map.keysSet referenceCounts'

        bufferedTxs'    = bufferedTxs
                          `Map.restrictKeys`
                          liveSet


getTxsToRequest :: ( MonadSTM m
                   , Ord txid
                   , Ord peeraddr
                   )
                => SharedTxStateVar m peeraddr txid tx
                -> Word16 -- ^ max txs to request
                -> peeraddr
                -> m (Set txid)
getTxsToRequest sharedVar maxTxsToRequest peeraddr =
  atomically $
    stateTVar sharedVar (getTxsToRequestImpl maxTxsToRequest peeraddr)


getNumTxIdsToRequest
  :: MonadSTM m
  => PeerTxStateAPI m peeraddr txid tx
  -> Word16 -- ^ max unacknowledged `tx`s
  -> Word16 -- ^ max `txid`s to request
  -> m Word16
getNumTxIdsToRequest peerStateAPI maxUnacked maxTxIdsToRequest =
   atomically $ statePeerTxState peerStateAPI (getNumTxIdsToRequestImpl maxUnacked maxTxIdsToRequest)


canRequestMoreTxs
  :: ( MonadSTM m
     , Ord txid
     , Ord peeraddr
     )
  => SharedTxStateVar m peeraddr txid tx
  -> peeraddr
  -> m Bool
canRequestMoreTxs sharedVar peeraddr = atomically $
    canRequestMoreTxsImpl peeraddr <$> readTVar sharedVar


-- | Set requests in-flight.
--
setRequestInflight :: MonadSTM m
                   => PeerTxStateAPI m peeraddr txid tx
                   -> Word16
                   -> m ()
setRequestInflight peerStateAPI reqno = atomically $
    modifyPeerTxState peerStateAPI (setRequestInflightImpl reqno)


-- | Acknowledge `txid`s, return the number of `txids` to be acknowledged to the
-- remote side.
--
-- TODO: if we expose `STM` transaction, we could do that in the same
-- transaction as getting mempool state.
receivedTxIds
  :: forall m peeraddr idx tx txid.
     (MonadSTM m, Ord txid, Ord peeraddr)
  => SharedTxStateVar m peeraddr txid tx
  -> MempoolSnapshot txid tx idx
  -> peeraddr
  -> Word16
  -- ^ number of requests to subtract from
  -- `requestedTxIdsInflight`
  -> StrictSeq txid
  -- ^ sequence of received `txids`
  -> Map txid SizeInBytes
  -- ^ received `txid`s with sizes
  -> m Int
receivedTxIds sharedVar MempoolSnapshot{mempoolHasTx} peeraddr reqNo txidsSeq txidsMap =
    atomically $
      stateTVar sharedVar (receivedTxIdsImpl mempoolHasTx peeraddr reqNo txidsSeq txidsMap)


-- | Include received `tx`s in `SharedTxState`.  Return number of `txids`
-- to be acknowledged and list of `tx` to be added to the mempool.
--
collectTxs :: forall m peeraddr tx txid.
              (MonadSTM m, Ord txid, Ord peeraddr)
           => SharedTxStateVar m peeraddr txid tx
           -> peeraddr
           -> Set txid    -- ^ set of requested txids
           -> Map txid tx -- ^ received txs
           -> m (Int, [tx])
           -- ^ number of txids to be acknowledged and txs to be added to the
           -- mempool
collectTxs sharedVar peeraddr txidsRequested txsMap =
    atomically $
      stateTVar sharedVar
                (collectTxsImpl peeraddr txidsRequested txsMap)
