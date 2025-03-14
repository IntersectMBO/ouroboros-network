{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.State
  ( -- * Core API
    SharedTxState (..)
  , PeerTxState (..)
  , numTxIdsToRequest
  , SharedTxStateVar
  , newSharedTxStateVar
  , receivedTxIds
  , collectTxs
  , acknowledgeTxIds
  , tickTimedTxs
    -- * Internals, only exported for testing purposes:
  , RefCountDiff (..)
  , updateRefCounts
  , receivedTxIdsImpl
  , collectTxsImpl
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Control.Exception (assert)
import Control.Tracer (Tracer, traceWith)

import Data.Foldable (fold,
#if !MIN_VERSION_base(4,20,0)
         foldl',
#endif
         toList)
import Data.Functor (($>))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random (StdGen)

import GHC.Stack (HasCallStack)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..))
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Inbound.Types
import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..))


-- | Compute number of `txids` to request respecting `TxDecisionPolicy`; update
-- `PeerTxState`.
--
numTxIdsToRequest :: TxDecisionPolicy
                  -> PeerTxState txid tx
                  -> (NumTxIdsToReq, PeerTxState txid tx)
numTxIdsToRequest
    TxDecisionPolicy { maxNumTxIdsToRequest,
                       maxUnacknowledgedTxIds }
    ps@PeerTxState { unacknowledgedTxIds,
                     requestedTxIdsInflight }
    =
    ( txIdsToRequest
    , ps { requestedTxIdsInflight = requestedTxIdsInflight
                                  + txIdsToRequest }
    )
  where
    -- we are forcing two invariants here:
    -- * there are at most `maxUnacknowledgedTxIds` (what we request is added to
    --   `unacknowledgedTxIds`)
    -- * there are at most `maxNumTxIdsToRequest` txid requests at a time per
    --   peer
    --
    -- TODO: both conditions provide an upper bound for overall requests for
    -- `txid`s to all inbound peers.
    txIdsToRequest, unacked, unackedAndRequested :: NumTxIdsToReq

    txIdsToRequest =
        assert (unackedAndRequested <= maxUnacknowledgedTxIds) $
        assert (requestedTxIdsInflight <= maxNumTxIdsToRequest) $
        (maxUnacknowledgedTxIds - unackedAndRequested)
        `min` (maxNumTxIdsToRequest - requestedTxIdsInflight)

    unackedAndRequested = unacked + requestedTxIdsInflight
    unacked = fromIntegral $ StrictSeq.length unacknowledgedTxIds

--
-- Pure public API
--

acknowledgeTxIds
    :: forall peeraddr tx txid.
       Ord txid
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
    TxDecisionPolicy { maxNumTxIdsToRequest,
                       maxUnacknowledgedTxIds }
    SharedTxState { bufferedTxs }
    ps@PeerTxState { availableTxIds,
                     unacknowledgedTxIds,
                     unknownTxs,
                     requestedTxIdsInflight,
                     downloadedTxs,
                     score,
                     toMempoolTxs,
                     requestedTxsInflight }
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
      , TxsToMempool []
      , RefCountDiff Map.empty
      , ps
      )
  where
    -- Split `unacknowledgedTxIds'` into the longest prefix of `txid`s which
    -- can be acknowledged and the unacknowledged `txid`s.
    (acknowledgedTxIds, unacknowledgedTxIds') =
      StrictSeq.spanl (\txid -> (txid `Map.member` bufferedTxs
                             || txid `Set.member` unknownTxs
                             || txid `Map.member` downloadedTxs)
                             && txid `Set.notMember` requestedTxsInflight
                      )
                      unacknowledgedTxIds

    txsToMempool = [ (txid,tx)
                   | txid <- toList toMempoolTxIds
                   , tx <- maybeToList $ txid `Map.lookup` downloadedTxs
                   ]
    (toMempoolTxIds, _) =
      StrictSeq.spanl (\txid -> txid `Map.member` downloadedTxs
                             && txid `Map.notMember` bufferedTxs)
                      acknowledgedTxIds
    txsToMempoolMap = Map.fromList txsToMempool

    toMempoolTxs' = toMempoolTxs <> txsToMempoolMap

    (downloadedTxs', ackedDownloadedTxs) = Map.partitionWithKey (\txid _ -> txid `Set.member` liveSet) downloadedTxs
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

    txIdsToRequest, unacked, unackedAndRequested :: NumTxIdsToReq

    txIdsToRequest =
        assert (unackedAndRequested <= maxUnacknowledgedTxIds) $
        assert (requestedTxIdsInflight <= maxNumTxIdsToRequest) $
        (maxUnacknowledgedTxIds - unackedAndRequested + fromIntegral txIdsToAcknowledge)
        `min`
        (maxNumTxIdsToRequest - requestedTxIdsInflight)

    unackedAndRequested = unacked + requestedTxIdsInflight
    unacked = fromIntegral $ StrictSeq.length unacknowledgedTxIds


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
    let (expiredTxs, timedTxs') = Map.split now timedTxs
        expiredTxs'             =  -- Map.split doesn't include the `now` entry in any map
                                  case Map.lookup now timedTxs of
                                       Just txids -> Map.insert now txids expiredTxs
                                       Nothing    -> expiredTxs
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
    fn m txids = foldl' gn m txids

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
       ( Ord peeraddr
       , Ord txid
       , Show txid
       )
    => (tx -> SizeInBytes) -- ^ compute tx size
    -> peeraddr
    -> Set txid    -- ^ set of requested txids
    -> Map txid tx -- ^ received txs
    -> SharedTxState peeraddr txid tx
    -> Either TxSubmissionProtocolError
              (SharedTxState peeraddr txid tx)
    -- ^ Return list of `txid` which sizes didn't match or a new state.
    -- If one of the `tx` has wrong size, we return an error.  The
    -- mini-protocol will throw, which will clean the state map from this peer.
collectTxsImpl txSize peeraddr requestedTxIds receivedTxs
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
              (Map.mapMaybeMissing \_ _ -> Nothing)
              (Map.mapMaybeMissing \_ _ -> Nothing)
              (Map.zipWithMaybeMatched \_ receivedSize advertisedSize ->
                if receivedSize == advertisedSize
                  then Nothing
                  else Just (receivedSize, advertisedSize)
              )
              (txSize `Map.map` receivedTxs)
              (availableTxIds ps)


        notReceived = requestedTxIds Set.\\ Map.keysSet receivedTxs

        downloadedTxs' = downloadedTxs ps <> receivedTxs

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
newSharedTxStateVar rng = newTVarIO SharedTxState { peerTxStates    = Map.empty,
                                                inflightTxs     = Map.empty,
                                                inflightTxsSize = 0,
                                                bufferedTxs     = Map.empty,
                                                referenceCounts = Map.empty,
                                                timedTxs        = Map.empty,
                                                limboTxs        = Map.empty,
                                                peerRng         = rng }


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
      Show txid)
  => Tracer m (TraceTxLogic peeraddr txid tx)
  -> (tx -> SizeInBytes)
  -> SharedTxStateVar m peeraddr txid tx
  -> peeraddr
  -> Set txid    -- ^ set of requested txids
  -> Map txid tx -- ^ received txs
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
