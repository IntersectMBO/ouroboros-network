{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ouroboros.Network.TxSubmission.Inbound.Decision
  ( TxDecision (..)
  , emptyTxDecision
    -- * Internal API exposed for testing
  , makeDecisions
  , filterActivePeers
  , pickTxsToDownload
  ) where

import Control.Arrow ((>>>))
import Control.Exception (assert)

import Data.Bifunctor (second)
import Data.Hashable
import Data.List (foldl', mapAccumR, sortOn)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random (random)

import Data.Sequence.Strict qualified as StrictSeq
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Inbound.State
import Ouroboros.Network.TxSubmission.Inbound.Types


-- | Make download decisions.
--
makeDecisions
    :: forall peeraddr txid tx.
       ( Ord peeraddr
       , Ord txid
       , Hashable peeraddr
       )
    => TxDecisionPolicy
    -- ^ decision policy
    -> SharedTxState peeraddr txid tx
    -- ^ decision context
    -> Map peeraddr (PeerTxState txid tx)
    -- ^ list of available peers.
    --
    -- This is a subset of `peerTxStates` of peers which either:
    -- * can be used to download a `tx`,
    -- * can acknowledge some `txid`s.
    --
    -> ( SharedTxState peeraddr txid tx
       , Map peeraddr (TxDecision txid tx)
       )
makeDecisions policy st =
    let (salt, rng') = random (peerRng st)
        st' = st { peerRng = rng' }
    in  fn
      . pickTxsToDownload policy st'
      . orderByRejections salt
  where
    fn :: forall a.
          (a, [(peeraddr, TxDecision txid tx)])
       -> (a, Map peeraddr (TxDecision txid tx))
    fn (a, as) = (a, Map.fromList as)


-- | Order peers by how useful the TXs they have provided are.
--
-- TXs delivered late will fail to apply because they where included in
-- a recently adopted block. Peers can race against each other by setting
-- `txInflightMultiplicity` to > 1. In case of a tie a hash of the peeraddr
-- is used as a tie breaker. Since every invocation use a new salt a given
-- peeraddr does not have an advantage over time.
--
orderByRejections :: Hashable peeraddr
                  => Int
                  -> Map peeraddr (PeerTxState txid tx)
                  -> [ (peeraddr, PeerTxState txid tx)]
orderByRejections salt =
        sortOn (\(peeraddr, ps) -> (score ps, hashWithSalt salt peeraddr))
      . Map.toList

-- | Internal state of `pickTxsToDownload` computation.
--
data St peeraddr txid tx =
   St { stInflightSize :: !SizeInBytes,
        -- ^ size of all `tx`s in-flight.

        stInflight     :: !(Map txid Int),
        -- ^ `txid`s in-flight.

        stAcknowledged :: !(Map txid Int),
        -- ^ acknowledged `txid` with multiplicities.  It is used to update
        -- `referenceCounts`.

        stLimboTx      :: Set txid
        -- ^ TXs on their way to the mempool. Used to prevent issueing new
        -- fetch requests for them.
      }


-- | Distribute `tx`'s to download among available peers.  Peers are considered
-- in the given order.
--
-- * pick txs from the set of available tx's (in `txid` order, note these sets
--   might be different for different peers).
-- * pick txs until the peers in-flight limit (we can go over the limit by one tx)
--   (`txsSizeInflightPerPeer` limit)
-- * pick txs until the overall in-flight limit (we can go over the limit by one tx)
--   (`maxTxsSizeInflight` limit)
-- * each tx can be downloaded simultaneously from at most
--   `txInflightMultiplicity` peers.
--
pickTxsToDownload
  :: forall peeraddr txid tx.
     ( Ord peeraddr
     , Ord txid
     )
  => TxDecisionPolicy
  -- ^ decision policy
  -> SharedTxState peeraddr txid tx
  -- ^ shared state

  -> [(peeraddr, PeerTxState txid tx)]
  -> ( SharedTxState peeraddr txid tx
     , [(peeraddr, TxDecision txid tx)]
     )

pickTxsToDownload policy@TxDecisionPolicy { txsSizeInflightPerPeer,
                                            maxTxsSizeInflight,
                                            txInflightMultiplicity }
                  sharedState@SharedTxState { peerTxStates,
                                              inflightTxs,
                                              inflightTxsSize,
                                              bufferedTxs,
                                              limboTxs,
                                              referenceCounts } =
    -- outer fold: fold `[(peeraddr, PeerTxState txid tx)]`
    mapAccumR
      accumFn
      -- initial state
      St { stInflight     = inflightTxs,
           stInflightSize = inflightTxsSize,
           stAcknowledged = Map.empty,
           stLimboTx      = Map.keysSet limboTxs }

    >>>
      gn
  where
    accumFn :: St peeraddr txid tx
            -> (peeraddr, PeerTxState txid tx)
            -> ( St peeraddr txid tx
               , ( (peeraddr, PeerTxState txid tx)
                 , TxDecision txid tx
                 )
               )
    accumFn
      st@St { stInflight,
              stInflightSize,
              stAcknowledged,
              stLimboTx }
      ( peeraddr
      , peerTxState@PeerTxState { availableTxIds,
                                  unknownTxs,
                                  requestedTxsInflight,
                                  requestedTxsInflightSize
                                }
      )
      =
      let sizeInflightAll   :: SizeInBytes
          sizeInflightOther :: SizeInBytes

          sizeInflightAll   = stInflightSize
          sizeInflightOther = sizeInflightAll - requestedTxsInflightSize

      in if sizeInflightAll >= maxTxsSizeInflight
        then let (numTxIdsToAck, numTxIdsToReq, txsToMempool, RefCountDiff { txIdsToAck }, peerTxState') =
                    acknowledgeTxIds policy sharedState peerTxState

                 stAcknowledged' = Map.unionWith (+) stAcknowledged txIdsToAck
                 stLimboTx'      = stLimboTx <> (Set.fromList $ map fst txsToMempool)
             in
             if requestedTxIdsInflight peerTxState' > 0
               then
                 -- we have txids to request
                 ( st { stAcknowledged = stAcknowledged'
                      , stLimboTx      = stLimboTx' }
                 , ( (peeraddr, peerTxState')
                     , TxDecision { txdTxIdsToAcknowledge = numTxIdsToAck,
                                    txdTxIdsToRequest     = numTxIdsToReq,
                                    txdPipelineTxIds      = not
                                                          . StrictSeq.null
                                                          . unacknowledgedTxIds
                                                          $ peerTxState',
                                    txdTxsToRequest       = Set.empty,
                                    txdTxsToMempool       = txsToMempool
                                  }
                     )
                 )
               else
                 -- there are no `txid`s to request, nor we can request `tx`s due
                 -- to in-flight size limits
                 ( st
                 , ( (peeraddr, peerTxState')
                   , emptyTxDecision
                   )
                 )
        else
          let requestedTxsInflightSize' :: SizeInBytes
              txsToRequest :: Set txid

              (requestedTxsInflightSize', txsToRequest) =
                -- inner fold: fold available `txid`s
                --
                -- Note: although `Map.foldrWithKey` could be used here, it
                -- does not allow to short circuit the fold, unlike
                -- `foldWithState`.
                foldWithState
                  (\(txid, (txSize, inflightMultiplicity)) sizeInflight ->
                    if -- note that we pick `txid`'s as long the `s` is
                       -- smaller or equal to `txsSizeInflightPerPeer`.
                       sizeInflight <= txsSizeInflightPerPeer
                       -- overall `tx`'s in-flight must be smaller than
                       -- `maxTxsSizeInflight`
                    && sizeInflight + sizeInflightOther <= maxTxsSizeInflight
                       -- the transaction must not be downloaded from more
                       -- than `txInflightMultiplicity` peers simultaneously
                    && inflightMultiplicity < txInflightMultiplicity
                    -- TODO: we must validate that `txSize` is smaller than
                    -- maximum txs size
                    then Just (sizeInflight + txSize, txid)
                    else Nothing
                  )
                  (Map.assocs $
                    -- merge `availableTxIds` with `stInflight`, so we don't
                    -- need to lookup into `stInflight` on every `txid` which
                    -- is in `availableTxIds`.
                    Map.merge (Map.mapMaybeMissing \_txid -> Just . (,0))
                               Map.dropMissing
                              (Map.zipWithMatched \_txid -> (,))

                              availableTxIds
                              stInflight
                    -- remove `tx`s which were already downloaded by some
                    -- other peer or are in-flight or unknown by this peer.
                    `Map.withoutKeys`
                    (Map.keysSet bufferedTxs <> requestedTxsInflight <> unknownTxs
                        <> stLimboTx)

                  )
                  requestedTxsInflightSize
                  -- pick from `txid`'s which are available from that given
                  -- peer.  Since we are folding a dictionary each `txid`
                  -- will be selected only once from a given peer (at least
                  -- in each round).

              peerTxState' = peerTxState {
                  requestedTxsInflightSize = requestedTxsInflightSize',
                  requestedTxsInflight     = requestedTxsInflight
                                          <> txsToRequest
                }

              (numTxIdsToAck, numTxIdsToReq, txsToMempool, RefCountDiff { txIdsToAck }, peerTxState'') =
                acknowledgeTxIds policy sharedState peerTxState'

              stAcknowledged' = Map.unionWith (+) stAcknowledged txIdsToAck

              stInflightDelta :: Map txid Int
              stInflightDelta = Map.fromSet (\_ -> 1) txsToRequest
                                -- note: this is right since every `txid`
                                -- could be picked at most once

              stInflight' :: Map txid Int
              stInflight' = Map.unionWith (+) stInflightDelta stInflight

              stLimboTx'      = stLimboTx <> Set.fromList (map fst txsToMempool)
          in
            if requestedTxIdsInflight peerTxState'' > 0
              then
                -- we can request `txid`s & `tx`s
                ( St { stInflight     = stInflight',
                       stInflightSize = sizeInflightOther + requestedTxsInflightSize',
                       stAcknowledged = stAcknowledged',
                       stLimboTx      = stLimboTx' }
                , ( (peeraddr, peerTxState'')
                  , TxDecision { txdTxIdsToAcknowledge = numTxIdsToAck,
                                 txdPipelineTxIds      = not
                                                       . StrictSeq.null
                                                       . unacknowledgedTxIds
                                                       $ peerTxState'',
                                 txdTxIdsToRequest     = numTxIdsToReq,
                                 txdTxsToRequest       = txsToRequest,
                                 txdTxsToMempool       = txsToMempool
                               }
                  )
                )
              else
                -- there are no `txid`s to request, only `tx`s.
                ( st { stInflight     = stInflight',
                       stInflightSize = sizeInflightOther + requestedTxsInflightSize',
                       stLimboTx      = stLimboTx'
                     }
                , ( (peeraddr, peerTxState'')
                  , emptyTxDecision { txdTxsToRequest = txsToRequest }
                  )
                )

    gn :: ( St peeraddr txid tx
          , [((peeraddr, PeerTxState txid tx), TxDecision txid tx)]
          )
       -> ( SharedTxState peeraddr txid tx
          , [(peeraddr, TxDecision txid tx)]
          )
    gn
      ( St { stInflight,
             stInflightSize,
             stAcknowledged }
      , as
      )
      =
      let peerTxStates' = Map.fromList ((\(a,_) -> a) <$> as)
                       <> peerTxStates

          referenceCounts' =
            Map.merge (Map.mapMaybeMissing \_ x -> Just x)
                      (Map.mapMaybeMissing \_ _ -> assert False Nothing)
                      (Map.zipWithMaybeMatched \_ x y -> if x > y then Just $! x - y
                                                                  else Nothing)
                      referenceCounts
                      stAcknowledged

          liveSet = Map.keysSet referenceCounts'

          bufferedTxs' = bufferedTxs
                         `Map.restrictKeys`
                         liveSet

          limboTxs' = foldl' updateLimboTxs limboTxs as

      in ( sharedState {
             peerTxStates    = peerTxStates',
             inflightTxs     = stInflight,
             inflightTxsSize = stInflightSize,
             bufferedTxs     = bufferedTxs',
             referenceCounts = referenceCounts',
             limboTxs        = limboTxs'}
         , -- exclude empty results
           mapMaybe (\((a, _), b) -> case b of
                      TxDecision { txdTxIdsToAcknowledge = 0,
                                   txdTxIdsToRequest     = 0,
                                   txdTxsToRequest,
                                   txdTxsToMempool }
                                 | null txdTxsToRequest
                                 , null txdTxsToMempool
                                 -> Nothing
                      _          -> Just (a, b)
                    )
                    as
         )

      where
        updateLimboTxs :: forall a.
                          Map txid Int
                       -> (a, TxDecision txid tx)
                       -> Map txid Int
        updateLimboTxs m (_,d) = foldl' fn m $ txdTxsToMempool d
          where
            fn :: Map txid Int
               -> (txid,tx)
               -> Map txid Int
            fn x (txid,_) = Map.alter (\case Nothing -> Just 1
                                             Just n  -> Just $! succ n) txid x


-- | Filter peers which can either download a `tx` or acknowledge `txid`s.
--
filterActivePeers
    :: forall peeraddr txid tx.
       Ord txid
    => TxDecisionPolicy
    -> SharedTxState peeraddr txid tx
    -> Map peeraddr (PeerTxState txid tx)
filterActivePeers
    TxDecisionPolicy { maxUnacknowledgedTxIds,
                       txsSizeInflightPerPeer,
                       maxTxsSizeInflight,
                       txInflightMultiplicity,
                       maxNumTxIdsToRequest
                     }
    SharedTxState { peerTxStates,
                    bufferedTxs,
                    inflightTxs,
                    inflightTxsSize,
                    limboTxs }
    | inflightTxsSize > maxTxsSizeInflight
    = Map.filter fn peerTxStates
    | otherwise
    = Map.filter gn peerTxStates
  where
    unrequestable = Map.keysSet (Map.filter (>= txInflightMultiplicity) inflightTxs)
                 <> Map.keysSet bufferedTxs

    fn :: PeerTxState txid tx -> Bool
    fn PeerTxState { unacknowledgedTxIds,
                     requestedTxIdsInflight,
                     unknownTxs,
                     downloadedTxs,
                     requestedTxsInflight
                   } =
           requestedTxIdsInflight == 0 -- document why it's not <= maxTxIdsInFlightPerPeer
        && requestedTxIdsInflight + numOfUnacked <= maxUnacknowledgedTxIds
        && txIdsToRequest > 0
      where
        -- Split `unacknowledgedTxIds'` into the longest prefix of `txid`s which
        -- can be acknowledged and the unacknowledged `txid`s.
        (acknowledgedTxIds, _) =
          StrictSeq.spanl (\txid -> (txid `Map.member` bufferedTxs
                                 || txid `Set.member` unknownTxs
                                 || txid `Map.member` downloadedTxs)
                                 && txid `Set.notMember` requestedTxsInflight
                          )
                          unacknowledgedTxIds
        numOfUnacked = fromIntegral (StrictSeq.length unacknowledgedTxIds)
        numOfAcked   = StrictSeq.length acknowledgedTxIds
        unackedAndRequested = numOfUnacked + requestedTxIdsInflight
        txIdsToRequest =
            assert (unackedAndRequested <= maxUnacknowledgedTxIds) $
            assert (requestedTxIdsInflight <= maxNumTxIdsToRequest) $
            (maxUnacknowledgedTxIds - unackedAndRequested + fromIntegral numOfAcked)
            `min`
            (maxNumTxIdsToRequest - requestedTxIdsInflight)

    gn :: PeerTxState txid tx -> Bool
    gn PeerTxState { unacknowledgedTxIds,
                     requestedTxIdsInflight,
                     requestedTxsInflight,
                     requestedTxsInflightSize,
                     availableTxIds,
                     downloadedTxs,
                     unknownTxs } =
          (    requestedTxIdsInflight == 0
            && requestedTxIdsInflight + numOfUnacked <= maxUnacknowledgedTxIds
            &&  txIdsToRequest > 0
          )
        || (underSizeLimit && not (Map.null downloadable))
      where
        numOfUnacked   = fromIntegral (StrictSeq.length unacknowledgedTxIds)
        underSizeLimit = requestedTxsInflightSize <= txsSizeInflightPerPeer
        downloadable   = availableTxIds
            `Map.withoutKeys` requestedTxsInflight
            `Map.withoutKeys` unknownTxs
            `Map.withoutKeys` unrequestable
            `Map.withoutKeys` Map.keysSet limboTxs

        -- Split `unacknowledgedTxIds'` into the longest prefix of `txid`s which
        -- can be acknowledged and the unacknowledged `txid`s.
        (acknowledgedTxIds, _) =
          StrictSeq.spanl (\txid -> txid `Map.member` bufferedTxs
                                 || txid `Set.member` unknownTxs
                                 || txid `Map.member` downloadedTxs
                                 && txid `Set.notMember` requestedTxsInflight
                          )
                          unacknowledgedTxIds
        numOfAcked   = StrictSeq.length acknowledgedTxIds
        unackedAndRequested = numOfUnacked + requestedTxIdsInflight
        txIdsToRequest =
            assert (unackedAndRequested <= maxUnacknowledgedTxIds) $
            assert (requestedTxIdsInflight <= maxNumTxIdsToRequest) $
            (maxUnacknowledgedTxIds - unackedAndRequested + fromIntegral numOfAcked)
            `min`
            (maxNumTxIdsToRequest - requestedTxIdsInflight)

--
-- Auxiliary functions
--

-- | A fold with state implemented as a `foldr` to take advantage of fold-build
-- fusion optimisation.
--
foldWithState
  :: forall s a b.
     Ord b
  => (a -> s -> Maybe (s, b))
  -> [a] -> s -> (s, Set b)
{-# INLINE foldWithState #-}

foldWithState f = foldr cons nil
  where
    cons :: a
         -> (s -> (s, Set b))
         -> (s -> (s, Set b))
    cons a k = \ !s ->
      case f a s of
        Nothing -> nil s
        Just (!s', !b) ->
          case Set.insert b `second` k s' of
            r@(!_s, !_bs) -> r

    nil :: s -> (s, Set b)
    nil = \ !s -> (s, Set.empty)
