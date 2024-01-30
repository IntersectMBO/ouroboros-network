{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.TxSubmission.Inbound.Decision
  ( TxDecision (..)
  , makeDecisions
  , TxDecisionPolicy (..)
  , SharedDecisionContext (..)
  ) where

import Control.Arrow ((>>>))
import Control.Exception (assert)

import Data.List (mapAccumR, sortOn)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word16)
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.DeltaQ (PeerGSV (..), gsvRequestResponseDuration)
import Ouroboros.Network.SizeInBytes (SizeInBytes)
import Ouroboros.Network.TxSubmission.Inbound.State (PeerTxState (..),
         RefCountDiff (..), SharedTxState (..), acknowledgeTxIds)


-- | Decision made by the decision logic.  Each peer will receive a 'Decision'.
--
data TxDecision txid = TxDecision {
    txdToAcknowledge :: !Int,
    -- ^ txid's to acknowledge

    txdToRequest     :: !(Set txid)
    -- ^ txid's to download
  }
  deriving Show

-- /note:/ this instance must be consistent with `pickTxsToDownload` and how
-- `PeerTxState` is updated.  It is designed to work with `TMergeVar`s.
--
instance Ord txid => Semigroup (TxDecision txid) where
    TxDecision a b <> TxDecision a' b' = TxDecision (a + a') (b <> b')

instance Ord txid => Monoid (TxDecision txid) where
    mempty = TxDecision 0 mempty

-- | Policy for making decisions
--
data TxDecisionPolicy = TxDecisionPolicy {
      maxTxIdsInflight       :: !Word16,
      -- ^ maximal number of txids inflight.

      --
      -- Configuration of tx decision logic.
      --

      txsSizeInflightPerPeer :: !SizeInBytes,
      -- ^ a limit of tx size in-flight from a single peer.
      -- It can be exceed by max tx size.

      maxTxsSizeInflight     :: !SizeInBytes,
      -- ^ a limit of tx size in-flight from all peers.
      -- It can be exceed by max tx size.

      txInflightMultiplicity :: !Int
      -- ^ from how many peers download the `txid` simultaneously
    }
  deriving Show


data SharedDecisionContext peeraddr txid tx = SharedDecisionContext {
    -- TODO: check how to access it.
    sdcPeerGSV       :: !(Map peeraddr PeerGSV),

    sdcSharedTxState :: !(SharedTxState peeraddr txid tx)
  }
  deriving Show

--
-- Decision Logic
--

-- | Make download decisions.
--
makeDecisions
    :: ( Ord peeraddr
       , Ord txid
       )
    => TxDecisionPolicy
    -- ^ decision policy
    -> SharedDecisionContext peeraddr txid tx
    -- ^ decision context
    -> Map peeraddr (PeerTxState txid tx)
    -- ^ list of available peers
    --
    -- This is a subset of `peerTxStates` of peers which either:
    -- * can be used to download a `tx`,
    -- * can acknowledge some `txid`s.
    --
    -> ( SharedTxState peeraddr txid tx
       , [(peeraddr, TxDecision txid)]
       )
makeDecisions policy SharedDecisionContext {
                       sdcPeerGSV,
                       sdcSharedTxState
                     } =
      pickTxsToDownload policy sdcSharedTxState
    . orderByDeltaQ sdcPeerGSV
    . filterPeersWhichExceedSizeInflight policy


-- | Remove peers which have more inflight `txs` than the
-- `txsSizeInflightPerPeer`.
--
filterPeersWhichExceedSizeInflight
  :: TxDecisionPolicy
  -> Map peeraddr (PeerTxState txid tx)
  -> Map peeraddr (PeerTxState txid tx)
filterPeersWhichExceedSizeInflight
    TxDecisionPolicy { txsSizeInflightPerPeer } =
    Map.filter (\PeerTxState { requestedTxsInflightSize } -> 
                 requestedTxsInflightSize <= txsSizeInflightPerPeer)


-- | Order peers by `DeltaQ`.
--
orderByDeltaQ :: forall peeraddr txid tx.
                 Ord peeraddr
              => Map peeraddr PeerGSV
              -> Map peeraddr (PeerTxState txid tx)
              -> [(peeraddr, PeerTxState txid tx)]
orderByDeltaQ dq =
        sortOn (\(peeraddr, _) ->
                   gsvRequestResponseDuration
                     (dq Map.! peeraddr) reqSize respSize)
      . Map.toList
    where
      -- according to calculations in `txSubmissionProtocolLimits`: sizes of
      -- `MsgRequestTx` with a single `txid` and `MsgReplyTxs` with a single
      -- `tx`.
      reqSize :: SizeInBytes
      reqSize = 36 -- 32 + 4 (MsgRequestTxs overhead)

      respSize :: SizeInBytes
      respSize = 65540


-- | Internal state of `pickTxsToDownload` computation.
--
data St peeraddr txid tx =
   St { stInflightSize :: !SizeInBytes,
        -- ^ size of all `tx`s in-flight.

        stInflight     :: !(Map txid Int),
        -- ^ `txid`s in-flight.

        stAcknowledged :: !(Map txid Int)
        -- ^ acknowledged `txid` with multiplicities.  It is used to update
        -- `referenceCounts`.
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
     , [(peeraddr, TxDecision txid)]
     )

pickTxsToDownload TxDecisionPolicy { txsSizeInflightPerPeer,
                                     maxTxsSizeInflight,
                                     txInflightMultiplicity }
                  sharedState@SharedTxState { peerTxStates,
                                              inflightTxs,
                                              bufferedTxs,
                                              referenceCounts } =
    -- outer fold: fold `[(peeraddr, PeerTxState txid tx)]`
    mapAccumR
      (\st@St { stInflight,
                stInflightSize,
                stAcknowledged }
        (peeraddr, peerTxState@PeerTxState { availableTxIds,
                                             unknownTxs,
                                             requestedTxsInflight,
                                             requestedTxsInflightSize }) ->

        let sizeInflightAll   :: SizeInBytes
            sizeInflightOther :: SizeInBytes

            sizeInflightAll   = stInflightSize
            sizeInflightOther = sizeInflightAll - requestedTxsInflightSize

        in if sizeInflightAll >= maxTxsSizeInflight
          then let (numTxIdsToAck, RefCountDiff { txIdsToAck }, peerTxState') =
                      acknowledgeTxIds sharedState peerTxState

                   stAcknowledged' = Map.unionWith (+) stAcknowledged txIdsToAck
               in 
               ( st { stAcknowledged = stAcknowledged' }
               , ((peeraddr, peerTxState'), TxDecision numTxIdsToAck Set.empty)
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
                    (\sizeInflight (txid, (txSize, inflightMultiplicity)) ->
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
                    requestedTxsInflightSize
                    -- pick from `txid`'s which are available from that given
                    -- peer.  Since we are folding a dictionary each `txid`
                    -- will be selected only once from a given peer (at least
                    -- in each round).
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
                      (Map.keysSet bufferedTxs <> requestedTxsInflight <> unknownTxs)

                    )

                peerTxState' = peerTxState {
                    requestedTxsInflightSize = requestedTxsInflightSize',
                    requestedTxsInflight     = requestedTxsInflight
                                            <> txsToRequest
                  }

                (numTxIdsToAck, RefCountDiff { txIdsToAck }, peerTxState'') =
                  acknowledgeTxIds sharedState peerTxState'

                stAcknowledged' = Map.unionWith (+) stAcknowledged txIdsToAck

                stInflightDelta :: Map txid Int
                stInflightDelta = Map.fromSet (\_ -> 1) txsToRequest
                                  -- note: this is right since every `txid`
                                  -- could be picked at most once

                stInflight' :: Map txid Int
                stInflight' = Map.unionWith (+) stInflightDelta stInflight


            in ( St { stInflight     = stInflight',
                      stInflightSize = sizeInflightOther + requestedTxsInflightSize',
                      stAcknowledged = stAcknowledged' }
               , ( (peeraddr, peerTxState'')
                 , TxDecision numTxIdsToAck txsToRequest
                 )
               )
      )
      -- initial state
      St { stInflightSize = foldMap requestedTxsInflightSize peerTxStates,
           stInflight     = inflightTxs,
           stAcknowledged = Map.empty
         }

    >>>

      (\(St { stInflight,
              stAcknowledged }, as) ->
        let peerTxStates' = Map.fromList (fst <$> as)
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

        in ( sharedState {
               peerTxStates    = peerTxStates',
               inflightTxs     = stInflight,
               bufferedTxs     = bufferedTxs',
               referenceCounts = referenceCounts' }
           , -- exclude empty results
             mapMaybe (\((a, _), b) -> case b of
                      TxDecision 0 txids | null txids
                                         -> Nothing
                      _                  -> Just (a, b)) as
           )
      )


--
-- Auxiliary functions
--

-- | A natural fold with state.  One cannot do that with `foldr` (no state
-- passing) or `foldAccumR` (no way to short-circuit the fold).
--
foldWithState
  :: forall s a b.
     Ord b
  => (s -> a -> Maybe (s, b))
  -> s -> [a] -> (s, Set b)
foldWithState f = go Set.empty
  where
    go :: Set b -> s -> [a] -> (s, Set b)
    go !bs !s []       = (s, bs)
    go !bs !s (a : as) =
      case f s a of
        Nothing       -> (s, bs)
        Just (s', !b) -> go (b `Set.insert` bs) s' as
{-# INLINE foldWithState #-}
