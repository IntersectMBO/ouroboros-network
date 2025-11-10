{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.State
  ( -- * Core API
    SharedTxState (..)
  , PeerTxState (..)
  , SharedTxStateVar
  , newSharedTxStateVar
  , receivedTxIds
  , collectTxs
  , acknowledgeTxIds
  , splitAcknowledgedTxIds
  , tickTimedTxs
  , const_MAX_TX_SIZE_DISCREPENCY
    -- * Internals, only exported for testing purposes:
  , RefCountDiff (..)
  , updateRefCounts
  , receivedTxIdsImpl
  , collectTxsImpl
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer, traceWith)

import Data.Foldable (fold, toList)
import Data.Foldable qualified as Foldable
import Data.Functor (($>))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import System.Random (StdGen)

import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..))
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..))


--
-- Pure public API
--

acknowledgeTxIds
    :: forall peeraddr tx txid.
       Ord txid
    => HasCallStack
    => TxDecisionPolicy
    -> SharedTxState peeraddr txid tx
    -> PeerTxState txid tx
    -> ( NumTxIdsToAck
       , NumTxIdsToReq
       , TxsToMempool txid tx
       , RefCountDiff txid
       , PeerTxState txid tx
       )
    -- ^ number of txid to acknowledge, requests, txs which we can submit to the
    -- mempool, txids to acknowledge with multiplicities, updated PeerTxState.
{-# INLINE acknowledgeTxIds #-}

acknowledgeTxIds
    policy
    sharedTxState
    ps@PeerTxState { availableTxIds,
                     unknownTxs,
                     requestedTxIdsInflight,
                     downloadedTxs,
                     score,
                     toMempoolTxs
                   }
    =
    -- We can only acknowledge txids when we can request new ones, since
    -- a `MsgRequestTxIds` for 0 txids is a protocol error.
    if txIdsToRequest > 0
      then
      ( txIdsToAcknowledge
      , txIdsToRequest
      , TxsToMempool txsToMempool
      , refCountDiff
      , ps { unacknowledgedTxIds    = unacknowledgedTxIds',
             availableTxIds         = availableTxIds',
             unknownTxs             = unknownTxs',
             requestedTxIdsInflight = requestedTxIdsInflight
                                    + txIdsToRequest,
             downloadedTxs          = downloadedTxs',
             score                  = score',
             toMempoolTxs           = toMempoolTxs' }
      )
      else
      ( 0
      , 0
      , TxsToMempool txsToMempool
      , RefCountDiff Map.empty
      , ps { toMempoolTxs = toMempoolTxs' }
      )
  where
    -- Split `unacknowledgedTxIds'` into the longest prefix of `txid`s which
    -- can be acknowledged and the unacknowledged `txid`s.
    (txIdsToRequest, acknowledgedTxIds, unacknowledgedTxIds')
      = splitAcknowledgedTxIds policy sharedTxState ps

    txsToMempool = [ (txid, tx)
                   | txid <- toList toMempoolTxIds
                   , txid `Map.notMember` bufferedTxs sharedTxState
                   , tx <- maybeToList $ txid `Map.lookup` downloadedTxs
                   ]
    (toMempoolTxIds, _) =
      StrictSeq.spanl (`Map.member` downloadedTxs) acknowledgedTxIds


    txsToMempoolMap = Map.fromList txsToMempool

    toMempoolTxs' = toMempoolTxs <> txsToMempoolMap

    (downloadedTxs', ackedDownloadedTxs) = Map.partitionWithKey (\txid _ -> txid `Set.member` liveSet) downloadedTxs
    -- latexTxs: transactions which were downloaded by another peer before we
    -- downloaded them; it relies on that `txToMempool` filters out
    -- `bufferedTxs`.
    lateTxs = Map.filterWithKey (\txid _ -> txid `Map.notMember` txsToMempoolMap) ackedDownloadedTxs
    score' = score + fromIntegral (Map.size lateTxs)

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
                 $ foldr (Map.alter fn)
                         Map.empty acknowledgedTxIds
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing  = Just 1
        fn (Just n) = Just $! n + 1

    txIdsToAcknowledge :: NumTxIdsToAck
    txIdsToAcknowledge = fromIntegral $ StrictSeq.length acknowledgedTxIds


-- | Split unacknowledged txids into acknowledged and unacknowledged parts, also
-- return number of txids which can be requested.
--
splitAcknowledgedTxIds
  :: Ord txid
  => HasCallStack
  => TxDecisionPolicy
  -> SharedTxState peer txid tx
  -> PeerTxState  txid tx
  -> (NumTxIdsToReq, StrictSeq.StrictSeq txid, StrictSeq.StrictSeq txid)
  -- ^ number of txids to request, acknowledged txids, unacknowledged txids
splitAcknowledgedTxIds
    TxDecisionPolicy {
      maxUnacknowledgedTxIds,
      maxNumTxIdsToRequest
    }
    SharedTxState {
      bufferedTxs
    }
    PeerTxState {
      unacknowledgedTxIds,
      unknownTxs,
      downloadedTxs,
      requestedTxsInflight,
      requestedTxIdsInflight
    }
    =
    (txIdsToRequest, acknowledgedTxIds', unacknowledgedTxIds')
  where
    (acknowledgedTxIds', unacknowledgedTxIds')
      = StrictSeq.spanl (\txid ->
                            txid `Set.notMember` requestedTxsInflight
                         && (
                              txid `Map.member` downloadedTxs
                           || txid `Set.member` unknownTxs
                           || txid `Map.member` bufferedTxs
                         )
                        )
                        unacknowledgedTxIds

    numOfUnacked        = StrictSeq.length unacknowledgedTxIds
    numOfAcked          = StrictSeq.length acknowledgedTxIds'
    unackedAndRequested = fromIntegral numOfUnacked + requestedTxIdsInflight

    txIdsToRequest =
        assert (unackedAndRequested <= maxUnacknowledgedTxIds) $
        assert (requestedTxIdsInflight <= maxNumTxIdsToRequest) $
        (maxUnacknowledgedTxIds - unackedAndRequested + fromIntegral numOfAcked)
        `min`
        (maxNumTxIdsToRequest - requestedTxIdsInflight)


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


tickTimedTxs :: forall peeraddr tx txid.
                (Ord txid)
             => Time
             -> SharedTxState peeraddr txid tx
             -> SharedTxState peeraddr txid tx
tickTimedTxs now st@SharedTxState{ timedTxs
                                 , referenceCounts
                                 , bufferedTxs } =
    let (expiredTxs', timedTxs') =
          case Map.splitLookup now timedTxs of
            (expired, Just txids, timed) ->
              (expired, -- Map.split doesn't include the `now` entry in the map
                        Map.insert now txids timed)
            (expired, Nothing, timed) ->
              (expired, timed)
        refDiff = Map.foldl' fn Map.empty expiredTxs'
        referenceCounts' = updateRefCounts referenceCounts (RefCountDiff refDiff)
        liveSet = Map.keysSet referenceCounts'
        bufferedTxs' = bufferedTxs `Map.restrictKeys` liveSet in
    st { timedTxs        = timedTxs'
       , referenceCounts = referenceCounts'
       , bufferedTxs     = bufferedTxs'
       }
  where
    fn :: Map txid Int
       -> [txid]
       -> Map txid Int
    fn m txids = Foldable.foldl' gn m txids

    gn :: Map txid Int
       -> txid
       -> Map txid Int
    gn m txid = Map.alter af txid m

    af :: Maybe Int
       -> Maybe Int
    af Nothing  = Just 1
    af (Just n) = Just $! succ n

--
-- Pure internal API
--

-- | Insert received `txid`s and return the number of txids to be acknowledged
-- and the updated `SharedTxState`.
--
receivedTxIdsImpl
    :: forall peeraddr tx txid.
       (Ord txid, Ord peeraddr, HasCallStack)
    => (txid -> Bool)      -- ^ check if txid is in the mempool, ref
                           -- 'mempoolHasTx'
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
          Foldable.foldl'
            (flip $ Map.alter (\case
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

-- | We check advertised sizes up in a fuzzy way.  The advertised and received
-- sizes need to agree up to `const_MAX_TX_SIZE_DISCREPENCY`.
--
const_MAX_TX_SIZE_DISCREPENCY :: SizeInBytes
const_MAX_TX_SIZE_DISCREPENCY = 32

collectTxsImpl
    :: forall peeraddr tx txid.
       ( Ord peeraddr
       , Ord txid
       , Show txid
       , Typeable txid
       )
    => (tx -> SizeInBytes) -- ^ compute tx size
    -> peeraddr
    -> Map txid SizeInBytes -- ^ requested txids
    -> Map txid tx          -- ^ received txs
    -> SharedTxState peeraddr txid tx
    -> Either TxSubmissionProtocolError
              (SharedTxState peeraddr txid tx)
    -- ^ Return list of `txid` which sizes didn't match or a new state.
    -- If one of the `tx` has wrong size, we return an error.  The
    -- mini-protocol will throw, which will clean the state map from this peer.
collectTxsImpl txSize peeraddr requestedTxIdsMap receivedTxs
               st@SharedTxState { peerTxStates } =

    -- using `alterF` so the update of `PeerTxState` is done in one lookup
    case Map.alterF (fmap Just . fn . fromJust)
                    peeraddr
                    peerTxStates of
      (Right st', peerTxStates') ->
        Right st' { peerTxStates = peerTxStates' }
      (Left e, _) ->
        Left $ ProtocolErrorTxSizeError e

  where
    -- Update `PeerTxState` and partially update `SharedTxState` (except of
    -- `peerTxStates`).
    fn :: PeerTxState txid tx
       -> ( Either [(txid, SizeInBytes, SizeInBytes)]
                   (SharedTxState peeraddr txid tx)
          , PeerTxState txid tx
          )
    fn ps =
        case wrongSizedTxs of
          [] -> ( Right st'
                , ps''
                )
          _  -> ( Left wrongSizedTxs
                , ps
                )
      where
        wrongSizedTxs :: [(txid, SizeInBytes, SizeInBytes)]
        wrongSizedTxs =
            map (\(a, (b,c)) -> (a,b,c))
          . Map.toList
          $ Map.merge
              Map.dropMissing
              Map.dropMissing
              (Map.zipWithMaybeMatched \_ receivedSize advertisedSize ->
                if receivedSize `checkTxSize` advertisedSize
                  then Nothing
                  else Just (receivedSize, advertisedSize)
              )
              (txSize `Map.map` receivedTxs)
              requestedTxIdsMap

        checkTxSize :: SizeInBytes
                    -> SizeInBytes
                    -> Bool
        checkTxSize received advertised
          | received > advertised
          = received - advertised <= const_MAX_TX_SIZE_DISCREPENCY
          | otherwise
          = advertised - received <= const_MAX_TX_SIZE_DISCREPENCY

        requestedTxIds = Map.keysSet requestedTxIdsMap
        notReceived    = requestedTxIds Set.\\ Map.keysSet receivedTxs
        downloadedTxs' = downloadedTxs ps <> receivedTxs
        -- Add not received txs to `unknownTxs` before acknowledging txids.
        unknownTxs'    = unknownTxs ps <> notReceived

        requestedTxsInflight' =
          assert (requestedTxIds `Set.isSubsetOf` requestedTxsInflight ps) $
          requestedTxsInflight ps  Set.\\ requestedTxIds

        requestedSize = fold $ availableTxIds ps `Map.restrictKeys` requestedTxIds
        requestedTxsInflightSize' =
          assert (requestedTxsInflightSize ps >= requestedSize) $
          requestedTxsInflightSize ps - requestedSize

        -- subtract requested from in-flight
        inflightTxs'' =
          Map.merge
            (Map.mapMaybeMissing \_ x -> Just x)
            (Map.mapMaybeMissing \_ _ -> assert False Nothing)
            (Map.zipWithMaybeMatched \_ x y ->
              assert (inFlightCount x >= y)
              let cnt' = inFlightCount x - y in
              if cnt' >= 0 -- TODO timecheck here
                 then Just $ x { inFlightCount = cnt' }
                 else Nothing)
            (inflightTxs st)
            (Map.fromSet (const 1) requestedTxIds)

        inflightTxsSize'' = assert (inflightTxsSize st >= requestedSize) $
                            inflightTxsSize st - requestedSize

        st' = st { inflightTxs     = inflightTxs'',
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
                    requestedTxsInflight     = requestedTxsInflight',
                    downloadedTxs            = downloadedTxs' }

--
-- Monadic public API
--

type SharedTxStateVar m peeraddr txid tx = StrictTVar m (SharedTxState peeraddr txid tx)

newSharedTxStateVar :: MonadSTM m
                    => StdGen
                    -> m (SharedTxStateVar m peeraddr txid tx)
newSharedTxStateVar rng = newTVarIO SharedTxState {
    peerTxStates             = Map.empty,
    inflightTxs              = Map.empty,
    inflightTxsSize          = 0,
    bufferedTxs              = Map.empty,
    referenceCounts          = Map.empty,
    timedTxs                 = Map.empty,
    inSubmissionToMempoolTxs = Map.empty,
    peerRng                  = rng
  }


-- | Acknowledge `txid`s, return the number of `txids` to be acknowledged to the
-- remote side.
--
receivedTxIds
  :: forall m peeraddr idx tx txid.
     (MonadSTM m, Ord txid, Ord peeraddr)
  => Tracer m (TraceTxLogic peeraddr txid tx)
  -> SharedTxStateVar m peeraddr txid tx
  -> STM m (MempoolSnapshot txid tx idx)
  -> peeraddr
  -> NumTxIdsToReq
  -- ^ number of requests to subtract from
  -- `requestedTxIdsInflight`
  -> StrictSeq txid
  -- ^ sequence of received `txids`
  -> Map txid SizeInBytes
  -- ^ received `txid`s with sizes
  -> m ()
receivedTxIds tracer sharedVar getMempoolSnapshot peeraddr reqNo txidsSeq txidsMap = do
  st <- atomically $ do
    MempoolSnapshot{mempoolHasTx} <- getMempoolSnapshot
    stateTVar sharedVar ((\a -> (a,a)) . receivedTxIdsImpl mempoolHasTx peeraddr reqNo txidsSeq txidsMap)
  traceWith tracer (TraceSharedTxState "receivedTxIds" st)


-- | Include received `tx`s in `SharedTxState`.  Return number of `txids`
-- to be acknowledged and list of `tx` to be added to the mempool.
--
collectTxs
  :: forall m peeraddr tx txid.
     (MonadSTM m, Ord txid, Ord peeraddr,
      Show txid, Typeable txid)
  => Tracer m (TraceTxLogic peeraddr txid tx)
  -> (tx -> SizeInBytes)
  -> SharedTxStateVar m peeraddr txid tx
  -> peeraddr
  -> Map txid SizeInBytes -- ^ set of requested txids with their announced size
  -> Map txid tx          -- ^ received txs
  -> m (Maybe TxSubmissionProtocolError)
  -- ^ number of txids to be acknowledged and txs to be added to the
  -- mempool
collectTxs tracer txSize sharedVar peeraddr txidsRequested txsMap = do
  r <- atomically $ do
    st <- readTVar sharedVar
    case collectTxsImpl txSize peeraddr txidsRequested txsMap st of
      r@(Right st') -> writeTVar sharedVar st'
                    $> r
      r@Left {}     -> pure r
  case r of
    Right st -> traceWith tracer (TraceSharedTxState "collectTxs" st)
             $> Nothing
    Left e   -> return (Just e)
