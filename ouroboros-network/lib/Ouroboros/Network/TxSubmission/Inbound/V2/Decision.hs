{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Decision
  ( TxDecision (..)
  , emptyTxDecision
    -- * Internal API exposed for testing
  , makeDecisions
  , filterActivePeers
  , pickTxsToDownload
  ) where

import Control.Arrow ((>>>))
import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI (addTime, Time)

import Data.Bifunctor (second)
import Data.Hashable
import Data.List qualified as List
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import System.Random (random)

import Data.Sequence.Strict qualified as StrictSeq
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types


-- | Make download decisions.
--
makeDecisions
    :: forall peeraddr txid tx.
       ( Ord peeraddr
       , Ord txid
       , Hashable peeraddr
       )
    => Time
    -- ^ current time
    -> TxDecisionPolicy
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
makeDecisions now policy st =
    let (salt, rng') = random (peerRng st)
        st' = st { peerRng = rng' }
    in  fn
      . pickTxsToDownload now policy st'
      . orderByRejections salt
  where
    fn :: forall a.
          (a, [(peeraddr, TxDecision txid tx)])
       -> (a, Map peeraddr (TxDecision txid tx))
    fn (a, as) = (a, Map.fromList as)


-- | Order peers by how useful the TXs they have provided are.
--
-- TXs delivered late will fail to apply because they were included in
-- a recently adopted block. Peers can race against each other by setting
-- `txInflightMultiplicity` to > 1. In case of a tie a hash of the peeraddr
-- is used as a tie breaker. Since every invocation use a new salt a given
-- peeraddr does not have an advantage over time.
--
orderByRejections :: Hashable peeraddr
                  => Int
                  -> Map peeraddr (PeerTxState txid tx)
                  -> [(peeraddr, PeerTxState txid tx)]
orderByRejections salt =
        List.sortOn (\(peeraddr, ps) -> (score ps, hashWithSalt salt peeraddr))
      . Map.toList


-- | Internal state of `pickTxsToDownload` computation.
--
data St peeraddr txid tx =
   St { stInflight                 :: !(Map txid InFlightState),
        -- ^ `txid`s in-flight.

        stAcknowledged             :: !(Map txid Int),
        -- ^ acknowledged `txid` with multiplicities.  It is used to update
        -- `referenceCounts`.

        stNewInSubmissionToMempoolTxs :: !(Set txid)
        -- ^ TXs newly selected for mempool submission in this decision round.
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
  => Time
  -- ^ current time
  -> TxDecisionPolicy
  -- ^ decision policy
  -> SharedTxState peeraddr txid tx
  -- ^ shared state

  -> [(peeraddr, PeerTxState txid tx)]
  -> ( SharedTxState peeraddr txid tx
     , [(peeraddr, TxDecision txid tx)]
     )

pickTxsToDownload now policy@TxDecisionPolicy { txsSizeInflightPerPeer,
                                                txInflightMultiplicity,
                                                interTxSpace }
                  sharedState@SharedTxState { peerTxStates,
                                              inflightTxs,
                                              bufferedTxs,
                                              inSubmissionToMempoolTxs,
                                              referenceCounts } =
    -- outer fold: fold `[(peeraddr, PeerTxState txid tx)]`
    List.mapAccumR
      accumFn
      -- initial state
      St { stInflight                 = inflightTxs,
           stAcknowledged             = Map.empty,
           stNewInSubmissionToMempoolTxs = Set.empty }

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
              stAcknowledged,
              stNewInSubmissionToMempoolTxs }
      ( peeraddr
      , peerTxState@PeerTxState { availableTxIds,
                                  unknownTxs,
                                  requestedTxsInflight,
                                  requestedTxsInflightSize
                                }
      )
      =
      let requestedTxsInflightSize' :: SizeInBytes
          txsToRequestMap :: Map txid SizeInBytes

          blockedTxid :: txid -> Bool
          blockedTxid txid =
               Set.member txid requestedTxsInflight
            || Set.member txid unknownTxs
            || Map.member txid bufferedTxs
            || Map.member txid inSubmissionToMempoolTxs
            || Set.member txid stNewInSubmissionToMempoolTxs

          (requestedTxsInflightSize', txsToRequestMap) =
            let candidates :: [(txid, (SizeInBytes, InFlightState))]
                candidates =
                  Map.foldrWithKey
                    (\txid txSize acc ->
                      if blockedTxid txid
                        then acc
                        else (txid, (txSize, Map.findWithDefault mempty txid stInflight)) : acc)
                    []
                    availableTxIds
            in
            -- inner fold: fold available `txid`s
            --
            -- Note: although `Map.foldrWithKey` could be used here, it
            -- does not allow to short circuit the fold, unlike
            -- `foldWithState`.
            foldWithState
              (\(txid, (txSize, inflightSt)) sizeInflight ->
                let inflightMultiplicity = inFlightCount inflightSt
                    inflightSpace        = inFlightNextReq inflightSt
                in
                if -- note that we pick `txid`'s as long the `s` is
                   -- smaller or equal to `txsSizeInflightPerPeer`.
                   sizeInflight <= txsSizeInflightPerPeer
                   -- the transaction must not be downloaded from more
                   -- than `txInflightMultiplicity` peers simultaneously
                && inflightMultiplicity < txInflightMultiplicity
                   -- we issue new requests for the same TX with
                   -- a minimum time between them
                && inflightSpace <= now
                -- TODO: we must validate that `txSize` is smaller than
                -- maximum txs size
                then Just (sizeInflight + txSize, (txid, txSize))
                else Nothing
              )
              candidates
              requestedTxsInflightSize
              -- pick from `txid`'s which are available from that given
              -- peer.  Since we are folding a dictionary each `txid`
              -- will be selected only once from a given peer (at least
              -- in each round).

          txsToRequest = Map.keysSet txsToRequestMap
          peerTxState' = peerTxState {
              requestedTxsInflightSize = requestedTxsInflightSize',
              requestedTxsInflight     = requestedTxsInflight
                                      <> txsToRequest
            }

          ( numTxIdsToAck
            , numTxIdsToReq
            , txsToMempool@TxsToMempool { listOfTxsToMempool }
            , RefCountDiff { txIdsToAck }
            , peerTxState''
            ) = acknowledgeTxIds policy sharedState peerTxState'

          stAcknowledged' = Map.unionWith (+) stAcknowledged txIdsToAck

          stInflightDelta :: Map txid InFlightState
          stInflightDelta =
            Map.map (const $ InFlightState 1 $ addTime interTxSpace now) txsToRequestMap
                            -- note: this is right since every `txid`
                            -- could be picked at most once

          stInflight' :: Map txid InFlightState
          stInflight' = Map.unionWith (<>) stInflightDelta stInflight

          stNewInSubmissionToMempoolTxs' =
            List.foldl' (\acc (txid, _) -> Set.insert txid acc)
                        stNewInSubmissionToMempoolTxs
                        listOfTxsToMempool
      in
        if requestedTxIdsInflight peerTxState'' > 0
          then
            -- we can request `txid`s & `tx`s
            ( St { stInflight                 = stInflight',
                   stAcknowledged             = stAcknowledged',
                   stNewInSubmissionToMempoolTxs = stNewInSubmissionToMempoolTxs' }
            , ( (peeraddr, peerTxState'')
              , TxDecision { txdTxIdsToAcknowledge = numTxIdsToAck,
                             txdPipelineTxIds      = not
                                                   . StrictSeq.null
                                                   . unacknowledgedTxIds
                                                   $ peerTxState'',
                             txdTxIdsToRequest     = numTxIdsToReq,
                             txdTxsToRequest       = txsToRequestMap,
                             txdTxsToMempool       = txsToMempool
                           }
              )
            )
          else
            -- there are no `txid`s to request, only `tx`s.
            ( st { stInflight                 = stInflight',
                   stNewInSubmissionToMempoolTxs = stNewInSubmissionToMempoolTxs'
                 }
            , ( (peeraddr, peerTxState'')
              , emptyTxDecision { txdTxsToRequest = txsToRequestMap }
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

          inSubmissionToMempoolTxs' =
            List.foldl' updateInSubmissionToMempoolTxs inSubmissionToMempoolTxs as

      in ( sharedState {
             peerTxStates             = peerTxStates',
             inflightTxs              = stInflight,
             bufferedTxs              = bufferedTxs',
             referenceCounts          = referenceCounts',
             inSubmissionToMempoolTxs = inSubmissionToMempoolTxs'}
         , -- exclude empty results
           mapMaybe (\((a, _), b) -> case b of
                      TxDecision { txdTxIdsToAcknowledge = 0,
                                   txdTxIdsToRequest     = 0,
                                   txdTxsToRequest,
                                   txdTxsToMempool = TxsToMempool { listOfTxsToMempool } }
                                 | null txdTxsToRequest
                                 , null listOfTxsToMempool
                                 -> Nothing
                      _          -> Just (a, b)
                    )
                    as
         )

      where
        updateInSubmissionToMempoolTxs
          :: forall a.
             Map txid Int
          -> (a, TxDecision txid tx)
          -> Map txid Int
        updateInSubmissionToMempoolTxs m (_,TxDecision { txdTxsToMempool } ) =
            List.foldl' fn m (listOfTxsToMempool txdTxsToMempool)
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
       ( Ord txid
       , Ord peeraddr
       )
    => HasCallStack
    => Time
    -> TxDecisionPolicy
    -> SharedTxState peeraddr txid tx
    -> Map peeraddr (PeerTxState txid tx)
filterActivePeers
    now
    policy@TxDecisionPolicy {
      maxUnacknowledgedTxIds,
      txsSizeInflightPerPeer,
      txInflightMultiplicity
    }
    sharedTxState@SharedTxState {
      peerTxStates,
      bufferedTxs,
      inflightTxs,
      inSubmissionToMempoolTxs,
      pendingDecisions
    }
    = Map.filterWithKey (\peer ps -> peer `Set.notMember` pendingDecisions && gn ps)
                        peerTxStates
  where
    unrequestableFilter :: InFlightState -> Bool
    unrequestableFilter InFlightState{inFlightCount, inFlightNextReq} =
      inFlightCount >= txInflightMultiplicity || inFlightNextReq > now

    isUnrequestable :: txid -> Bool
    isUnrequestable txid =
      Map.member txid bufferedTxs
      || case Map.lookup txid inflightTxs of
           Just inflightSt -> unrequestableFilter inflightSt
           Nothing         -> False

    gn :: PeerTxState txid tx -> Bool
    gn peerTxState@PeerTxState { unacknowledgedTxIds,
                                 requestedTxIdsInflight,
                                 requestedTxsInflight,
                                 requestedTxsInflightSize,
                                 availableTxIds,
                                 unknownTxs
                               } =
          (    requestedTxIdsInflight == 0
            && requestedTxIdsInflight + numOfUnacked <= maxUnacknowledgedTxIds
            && txIdsToRequest > 0
          )
        || (underSizeLimit && downloadable)
      where
        numOfUnacked   = fromIntegral (StrictSeq.length unacknowledgedTxIds)
        underSizeLimit = requestedTxsInflightSize <= txsSizeInflightPerPeer
        downloadable =
          Map.foldrWithKey
            (\txid _ acc ->
              acc
              || (    Set.notMember txid requestedTxsInflight
                  &&  Set.notMember txid unknownTxs
                  &&  not (isUnrequestable txid)
                  &&  Map.notMember txid inSubmissionToMempoolTxs
                 ))
            False
            availableTxIds

        -- Split `unacknowledgedTxIds'` into the longest prefix of `txid`s which
        -- can be acknowledged and the unacknowledged `txid`s.
        (txIdsToRequest, _, _) = splitAcknowledgedTxIds policy sharedTxState peerTxState

--
-- Auxiliary functions
--

-- | A fold with state implemented as a `foldr` to take advantage of fold-build
-- fusion optimisation.
--
foldWithState
  :: forall s a b c.
     Ord b
  => (a -> s -> Maybe (s, (b, c)))
  -> [a] -> s -> (s, Map b c)
{-# INLINE foldWithState #-}

foldWithState f = foldr cons nil
  where
    cons :: a
         -> (s -> (s, Map b c))
         -> (s -> (s, Map b c))
    cons a k = \ !s ->
      case f a s of
        Nothing -> nil s
        Just (!s', (!b, !c)) ->
          case Map.insert b c `second` k s' of
            r@(!_s, !_bs) -> r

    nil :: s -> (s, Map b c)
    nil = \ !s -> (s, Map.empty)
