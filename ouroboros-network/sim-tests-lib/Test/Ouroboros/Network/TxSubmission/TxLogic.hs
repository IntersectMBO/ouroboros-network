{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.TxSubmission.TxLogic where

import Prelude hiding (seq)

import Control.Exception (assert)

import Data.Foldable (fold,
#if !MIN_VERSION_base(4,20,0)
         foldl',
#endif
         toList)
import Data.List (intercalate, isPrefixOf, isSuffixOf, mapAccumR, nub,
           stripPrefix)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (Sum (..))
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set

import NoThunks.Class

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.Decision
           (SharedDecisionContext (..), TxDecision (..))
import Ouroboros.Network.TxSubmission.Inbound.Decision qualified as TXS
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Inbound.State (PeerTxState (..),
           SharedTxState (..))
import Ouroboros.Network.TxSubmission.Inbound.State qualified as TXS

import Test.Ouroboros.Network.BlockFetch (PeerGSVT (..))
import Test.Ouroboros.Network.TxSubmission.Types

import Test.QuickCheck
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Monoids (All (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Pretty.Simple


tests :: TestTree
tests = testGroup "TxLogic"
  [ testGroup "State"
    [ testGroup "Arbitrary"
      [ testGroup "ArbSharedTxState"
        [ testProperty "generator"              prop_SharedTxState_generator
        , testProperty "shrinker"             $ withMaxSuccess 10
                                                prop_SharedTxState_shrinker
        , testProperty "nothunks"               prop_SharedTxState_nothunks
        ]
      , testGroup "ArbReceivedTxIds"
        [ testProperty "generator"              prop_receivedTxIds_generator
        ]
      , testGroup "ArbCollectTxs"
        [ testProperty "generator"              prop_collectTxs_generator
        , testProperty "shrinker"             $ withMaxSuccess 10
                                                prop_collectTxs_shrinker
        ]
      ]
    , testProperty "acknowledgeTxIds"           prop_acknowledgeTxIds
    , testProperty "receivedTxIdsImpl"          prop_receivedTxIdsImpl
    , testProperty "collectTxsImpl"             prop_collectTxsImpl
    , testProperty "numTxIdsToRequest"          prop_numTxIdsToRequest
    , testGroup "NoThunks"
      [ testProperty "receivedTxIdsImpl"        prop_receivedTxIdsImpl_nothunks
      , testProperty "collectTxsImpl"           prop_collectTxsImpl_nothunks
      ]
    ]
  , testGroup "Decisions"
    [ testGroup "ArbDecisionContexts"
      [ testProperty "generator"               prop_ArbDecisionContexts_generator
      , testProperty "shrinker"              $ withMaxSuccess 33
                                               prop_ArbDecisionContexts_shrinker
      ]
    , testProperty "shared state invariant"    prop_makeDecisions_sharedstate
    , testProperty "inflight"                  prop_makeDecisions_inflight
    , testProperty "policy"                    prop_makeDecisions_policy
    , testProperty "acknowledged"              prop_makeDecisions_acknowledged
    , testProperty "exhaustive"                prop_makeDecisions_exhaustive
    ]
  , testGroup "Registry"
    [ testGroup "filterActivePeers"
      [ testProperty "not limiting decisions"  prop_filterActivePeers_not_limitting_decisions
      ]
    ]
  ]


--
-- InboundState properties
--

type PeerAddr = Int

-- | 'InboundState` invariant.
--
sharedTxStateInvariant
  :: forall peeraddr txid tx.
     ( Ord txid
     , Show txid
     )
  => SharedTxState peeraddr txid tx
  -> Property
sharedTxStateInvariant SharedTxState {
                         peerTxStates,
                         inflightTxs,
                         inflightTxsSize,
                         bufferedTxs,
                         referenceCounts
                       } =

         -- -- `inflightTxs` and `bufferedTxs` are disjoint
         -- counterexample "inflightTxs not disjoint with bufferedTxs"
         -- (null (inflightTxsSet `Set.intersection` bufferedTxsSet))

         -- the set of buffered txids is equal to sum of the sets of
         -- unacknowledged txids.
         counterexample "bufferedTxs txid not a subset of unacknoledged txids"
         (bufferedTxsSet
           `Set.isSubsetOf`
           foldr (\PeerTxState { unacknowledgedTxIds } r ->
                   r <> Set.fromList (toList unacknowledgedTxIds))
                 Set.empty txStates)

    .&&. counterexample "referenceCounts invariant violation"
         ( referenceCounts
           ===
           foldl'
             (\m PeerTxState { unacknowledgedTxIds = unacked } ->
               foldl'
                 (flip $
                   Map.alter (\case
                                Nothing  -> Just $! 1
                                Just cnt -> Just $! succ cnt)
                 )
                 m
                 unacked
             )
             Map.empty txStates
         )

    .&&. counterexample ("bufferedTxs contain tx which should be gc-ed: "
                         ++ show (Map.keysSet bufferedTxs `Set.difference` liveSet))
         (Map.keysSet bufferedTxs `Set.isSubsetOf` liveSet)

    .&&. counterexample "inflightTxs must be a sum of requestedTxInflight sets"
        (inflightTxs
          ===
          foldr (\PeerTxState { requestedTxsInflight } m ->
                 Map.unionWith (+) (Map.fromSet (\_ -> 1) requestedTxsInflight) m)
                Map.empty
                peerTxStates)

         -- PeerTxState invariants
    .&&. counterexample "PeerTxState invariant violation"
         (foldMap (\ps -> All
                        . counterexample (show ps)
                        . peerTxStateInvariant
                        $ ps
                  )
                  peerTxStates)

    .&&. counterexample "inflightTxsSize invariant violation"
         (inflightTxsSize === foldMap requestedTxsInflightSize peerTxStates)



  where
    peerTxStateInvariant :: PeerTxState txid tx -> Property
    peerTxStateInvariant PeerTxState { availableTxIds,
                                       unacknowledgedTxIds,
                                       unknownTxs,
                                       requestedTxIdsInflight,
                                       requestedTxsInflight,
                                       requestedTxsInflightSize } =


             counterexample ("unknownTxs is not a subset of unacknowledgedTxIds: "
                              ++ show (unknownTxs Set.\\ unacknowledgedTxIdsSet))
             (unknownTxs `Set.isSubsetOf` unacknowledgedTxIdsSet)

        .&&. counterexample ("availableTxs is not a subset of unacknowledgedTxIds: "
                              ++ show (availableTxIdsSet Set.\\ unacknowledgedTxIdsSet))
             (availableTxIdsSet `Set.isSubsetOf` unacknowledgedTxIdsSet)

        .&&. counterexample ("unacknowledged tx must be either available, unknown or buffered: "
                              ++ show (unacknowledgedTxIdsSet
                                        Set.\\ availableTxIdsSet
                                        Set.\\ unknownTxs
                                        Set.\\ bufferedTxsSet))
             (unacknowledgedTxIdsSet
                 Set.\\ availableTxIdsSet
                 Set.\\ unknownTxs
               `Set.isSubsetOf`
               bufferedTxsSet
             )

        .&&. counterexample "requestedTxIdsInflight invariant violation"
             (requestedTxIdsInflight >= 0)

             -- a requested tx is either available or buffered
        .&&. counterexample ("requestedTxsInflight invariant violation: "
                             ++ show (requestedTxsInflight
                                       Set.\\ availableTxIdsSet
                                       Set.\\ bufferedTxsSet))
             (requestedTxsInflight Set.\\ availableTxIdsSet `Set.isSubsetOf` bufferedTxsSet)

        .&&. counterexample "requestedTxsInfightSize"
             (requestedTxsInflightSize
               ===
              fold (availableTxIds `Map.restrictKeys` requestedTxsInflight))

      where
        availableTxIdsSet :: Set txid
        availableTxIdsSet = Map.keysSet availableTxIds

        unacknowledgedTxIdsSet :: Set txid
        unacknowledgedTxIdsSet = Set.fromList (toList unacknowledgedTxIds)

    bufferedTxsSet = Map.keysSet bufferedTxs     :: Set txid
    liveSet        = Map.keysSet referenceCounts :: Set txid
    txStates       = Map.elems   peerTxStates    :: [PeerTxState txid tx]

--
-- Generate `InboundState`
--

-- | PeerTxState generator.
--
-- `mkArbPeerTxState` is the smart constructor.
--
data ArbPeerTxState txid tx =
    ArbPeerTxState { arbPeerTxState :: PeerTxState txid tx,
                     arbInflightSet :: Set tx,
                     -- ^ in-flight txs
                     arbBufferedMap :: Map txid (Maybe tx)
                   }

data TxStatus = Available | Inflight | Unknown

instance Arbitrary TxStatus where
    arbitrary = oneof [ pure Available
                      , pure Inflight
                      , pure Unknown
                      ]

data TxMask tx = TxAvailable tx TxStatus
                 -- ^ available txid with its size, the Bool indicates if it's
                 -- in-flight or not
               | TxBuffered tx

fixupTxMask :: txid -> TxMask (Tx txid) -> TxMask (Tx txid)
fixupTxMask txid (TxAvailable tx status) = TxAvailable tx { getTxId = txid } status
fixupTxMask txid (TxBuffered tx)         = TxBuffered  tx { getTxId = txid }


instance Arbitrary tx => Arbitrary (TxMask tx) where
    arbitrary = oneof [ TxAvailable
                          <$> arbitrary
                          <*> arbitrary
                      , TxBuffered <$> arbitrary
                      ]

    -- TODO: implement shrinker; this can be done by writing an inverse of
    -- `mkArbPeerTxState` and shrinking the unacknowledged txs & mask map.


-- | Smart constructor for `ArbPeerTxState`.
--
mkArbPeerTxState :: Ord txid
                 => Fun txid Bool
                 -> Int -- ^ txids in-flight
                 -> [txid]
                 -> Map txid (TxMask (Tx txid))
                 -> ArbPeerTxState txid (Tx txid)
mkArbPeerTxState mempoolHasTxFun txIdsInflight unacked txMaskMap =
    ArbPeerTxState
      PeerTxState { unacknowledgedTxIds = StrictSeq.fromList unacked,
                    availableTxIds,
                    requestedTxIdsInflight,
                    requestedTxsInflight,
                    requestedTxsInflightSize,
                    unknownTxs,
                    rejectedTxs = 0,
                    fetchedTxs = Set.empty }
      (Set.fromList $ Map.elems inflightMap)
      bufferedMap
  where
    mempoolHasTx   = apply mempoolHasTxFun
    availableTxIds = Map.fromList
                   [ (txid, getTxSize tx) | (txid, TxAvailable tx _) <- Map.assocs txMaskMap
                   , not (mempoolHasTx txid)
                   ]
    unknownTxs     = Set.fromList
                   [ txid | (txid, TxAvailable _ Unknown) <- Map.assocs txMaskMap
                   , not (mempoolHasTx txid)
                   ]

    requestedTxIdsInflight   = fromIntegral txIdsInflight
    requestedTxsInflightSize = foldMap getTxSize inflightMap
    requestedTxsInflight     = Map.keysSet inflightMap

    -- exclude `txid`s which are already in the mempool, we never request such
    -- `txid`s
    --
    -- TODO: this should be lifted, we might have the same txid in-flight from
    -- multiple peers, one will win the race and land in the mempool first
    inflightMap = Map.fromList
                [ (txid, tx)
                | (txid, TxAvailable tx Inflight) <- Map.assocs txMaskMap
                , not (mempoolHasTx txid)
                ]

    bufferedMap = Map.fromList
                  [ (txid, Nothing)
                  | txid <- Map.keys txMaskMap
                  , mempoolHasTx txid
                  ]
                `Map.union`
                  Map.fromList
                  [ (txid, mtx)
                  | (txid, TxBuffered tx) <- Map.assocs txMaskMap
                  , let !mtx = if mempoolHasTx txid
                               then Nothing
                               else Just $! tx { getTxId = txid }
                  ]


genArbPeerTxState
  :: forall txid.
     ( Arbitrary txid
     , Ord txid
     )
  => Fun txid Bool
  -> Int -- ^ max txids inflight
  -> Gen (ArbPeerTxState txid (Tx txid))
genArbPeerTxState mempoolHasTxFun maxTxIdsInflight = do
    -- unacknowledged sequence
    unacked <- arbitrary
    -- generate `Map txid (TxMask tx)`
    txIdsInflight <- choose (0, maxTxIdsInflight)
    txMap <- Map.fromList
         <$> traverse (\txid -> (\a -> (txid, fixupTxMask txid a)) <$> arbitrary)
                      (nub unacked)
    return $ mkArbPeerTxState mempoolHasTxFun txIdsInflight unacked txMap


genSharedTxState
  :: forall txid.
     ( Arbitrary txid
     , Ord txid
     , Function txid
     , CoArbitrary txid
     )
  => Int -- ^ max txids inflight
  -> Gen ( Fun txid Bool
         , (PeerAddr, PeerTxState txid (Tx txid))
         , SharedTxState PeerAddr txid (Tx txid)
         , Map PeerAddr (ArbPeerTxState txid (Tx txid))
         )
genSharedTxState maxTxIdsInflight = do
    _mempoolHasTxFun@(Fun (_, _, x) _) <- arbitrary :: Gen (Fun Bool Bool)
    let mempoolHasTxFun = Fun (function (const False), False, x) (const False)
    pss <- listOf1 (genArbPeerTxState mempoolHasTxFun maxTxIdsInflight)

    let pss' :: [(PeerAddr, ArbPeerTxState txid (Tx txid))]
        pss' = [0..] `zip` pss

    peer <- choose (0, length pss - 1)

    let st :: SharedTxState PeerAddr txid (Tx txid)
        st = fixupSharedTxState
               (apply mempoolHasTxFun)
               SharedTxState {
                 peerTxStates    = Map.fromList
                                 [ (peeraddr, arbPeerTxState)
                                 | (peeraddr, ArbPeerTxState { arbPeerTxState })
                                   <- pss'
                                 ],
                 inflightTxs     = foldl' (Map.unionWith (+)) Map.empty
                                 [ Map.fromSet (const 1) (Set.map getTxId arbInflightSet)
                                 | ArbPeerTxState { arbInflightSet }
                                   <- pss
                                 ],
                 inflightTxsSize = 0, -- It is set by fixupSharedTxState
                 bufferedTxs     = fold
                                 [ arbBufferedMap
                                 | ArbPeerTxState { arbBufferedMap }
                                   <- pss
                                 ],
                 referenceCounts = Map.empty
               }

    return ( mempoolHasTxFun
           , (peer, peerTxStates st Map.! peer)
           , st
           , Map.fromList pss'
           )


-- |  Make sure `SharedTxState` is well formed.
--
fixupSharedTxState
  :: Ord txid
  => (txid -> Bool) -- ^ mempoolHasTx
  -> SharedTxState peeraddr txid tx
  -> SharedTxState peeraddr txid tx
fixupSharedTxState _mempoolHasTx st@SharedTxState { peerTxStates } =
    st { peerTxStates    = peerTxStates',
         inflightTxs     = inflightTxs',
         inflightTxsSize = foldMap requestedTxsInflightSize peerTxStates',
         bufferedTxs     = bufferedTxs',
         referenceCounts = referenceCounts'
       }
  where
    peerTxStates' =
      Map.map (\ps@PeerTxState { availableTxIds,
                                 requestedTxsInflight } ->

        let -- requested txs must not be buffered
            requestedTxsInflight'     = requestedTxsInflight
                                 Set.\\ Map.keysSet bufferedTxs'
            requestedTxsInflightSize' = fold $ availableTxIds
                                               `Map.restrictKeys`
                                               requestedTxsInflight'

        in ps { requestedTxsInflight     = requestedTxsInflight',
                requestedTxsInflightSize = requestedTxsInflightSize' }
      )
      peerTxStates

    inflightTxs' = foldr (\PeerTxState { requestedTxsInflight } m ->
                           Map.unionWith (+)
                             (Map.fromSet (const 1) requestedTxsInflight)
                             m
                         )
                         Map.empty
                         peerTxStates'

    bufferedTxs' =
      bufferedTxs st
      `Map.restrictKeys`
        foldr (\PeerTxState {unacknowledgedTxIds = unacked } r ->
                r <> Set.fromList (toList unacked))
              Set.empty (Map.elems peerTxStates)


    referenceCounts' =
      foldl'
        (\m PeerTxState { unacknowledgedTxIds } ->
          foldl'
            (flip $
              Map.alter (\case
                           Nothing  -> Just $! 1
                           Just cnt -> Just $! succ cnt)
            )
            m
            unacknowledgedTxIds
        )
        Map.empty
        (Map.elems peerTxStates)


shrinkSharedTxState :: ( Arbitrary txid
                       , Ord txid
                       , Function txid
                       , Ord peeraddr
                       )
                    => (txid -> Bool)
                    ->  SharedTxState peeraddr txid (Tx txid)
                    -> [SharedTxState peeraddr txid (Tx txid)]
shrinkSharedTxState mempoolHasTx st@SharedTxState { peerTxStates,
                                                    inflightTxs,
                                                    bufferedTxs } =
  [ st'
  | peerTxStates' <- Map.fromList <$> shrinkList (\_ -> []) (Map.toList peerTxStates)
  , not (Map.null peerTxStates')
  , let st' = fixupSharedTxState mempoolHasTx st { peerTxStates = peerTxStates' }
  , st' /= st
  ]
  ++
  [ fixupSharedTxState mempoolHasTx st { inflightTxs = inflightTxs' }
  | inflightTxs' <- Map.fromList <$> shrinkList (\_ -> []) (Map.toList inflightTxs)
  ]
  ++
  [ st
  | bufferedTxs' <- Map.fromList
                <$> shrinkList (\_ -> []) (Map.assocs bufferedTxs)
  , let minBuffered =
          foldMap
            (\PeerTxState {
                unacknowledgedTxIds,
                availableTxIds,
                unknownTxs
              }
              ->
              Set.fromList (toList unacknowledgedTxIds)
                Set.\\ Map.keysSet availableTxIds
                Set.\\ unknownTxs
            )
            peerTxStates
        bufferedTxs'' = bufferedTxs'
                        `Map.union`
                        (bufferedTxs `Map.restrictKeys` minBuffered)
        st' = fixupSharedTxState mempoolHasTx st { bufferedTxs = bufferedTxs'' }
  , st' /= st
  ]

--
-- Arbitrary `SharaedTxState` instance
--

data ArbSharedTxState =
    ArbSharedTxState
      (Fun TxId Bool)
      (SharedTxState PeerAddr TxId (Tx TxId))
  deriving Show

instance Arbitrary ArbSharedTxState where
  arbitrary = do
    Small maxTxIdsInflight <- arbitrary
    (mempoolHasTx, _, sharedTxState, _) <- genSharedTxState maxTxIdsInflight
    return $ ArbSharedTxState mempoolHasTx sharedTxState

  shrink (ArbSharedTxState mempoolHasTx st) =
    [ ArbSharedTxState mempoolHasTx st'
    | st' <- shrinkSharedTxState (apply mempoolHasTx) st
    ]


-- | Verify that generated `SharedTxState` has no thunks if it's evaluated to
-- WHNF.
--
prop_SharedTxState_nothunks :: ArbSharedTxState -> Property
prop_SharedTxState_nothunks (ArbSharedTxState _ !st) =
    case unsafeNoThunks st of
      Nothing  -> property True
      Just ctx -> counterexample (show ctx) False


prop_SharedTxState_generator
  :: ArbSharedTxState
  -> Property
prop_SharedTxState_generator (ArbSharedTxState _ st) = sharedTxStateInvariant st


prop_SharedTxState_shrinker
  :: Fixed ArbSharedTxState
  -> Property
prop_SharedTxState_shrinker =
    property
  . foldMap (\(ArbSharedTxState _ st) -> All $ sharedTxStateInvariant st)
  . shrink
  . getFixed


--
-- `receivedTxIdsImpl` properties
--


data ArbReceivedTxIds =
     ArbReceivedTxIds (Fun TxId Bool) -- ^ mempoolHasTx
                      [Tx TxId]       -- ^ some txs to acknowledge
                      PeerAddr        -- ^ peer address
                      (PeerTxState TxId (Tx TxId))
                                      -- ^ peer state
                      (SharedTxState PeerAddr TxId (Tx TxId))
                                      -- ^ initial state
  deriving Show

instance  Arbitrary ArbReceivedTxIds where
    arbitrary = do
        Small maxTxIdsInflight <- arbitrary
        (mempoolHasTxFun, (peeraddr, ps), st, psMap) <- genSharedTxState maxTxIdsInflight
        txsToAck <- sublistOf (Set.toList $ arbInflightSet (psMap Map.! peeraddr))
        pure $ ArbReceivedTxIds
                 mempoolHasTxFun
                 txsToAck
                 peeraddr
                 ps
                 st

    shrink (ArbReceivedTxIds mempoolHasTxFun txs peeraddr ps st) =
      [ ArbReceivedTxIds mempoolHasTxFun txs' peeraddr ps st
      | txs' <- shrink txs
      ]
      ++
      [ ArbReceivedTxIds
        mempoolHasTxFun' txs peeraddr ps
        (fixupSharedTxState (apply mempoolHasTxFun') st)
      | mempoolHasTxFun' <- shrink mempoolHasTxFun
      ]


prop_receivedTxIds_generator
  :: ArbReceivedTxIds
  -> Property
prop_receivedTxIds_generator (ArbReceivedTxIds _ someTxsToAck _peeraddr _ps st) =
    label ("numToAck " ++ labelInt 100 10 (length someTxsToAck))
  . counterexample (show st)
  $ sharedTxStateInvariant st


-- | This property verifies that `acknowledgeTxIds` acknowledges a prefix of
-- unacknowledged txs, and that the `numTxIdsToAck` as well as `RefCoundDiff`
-- are correct.
--
-- It doesn't validate the returned `PeerTxState` holds it's properties as this
-- needs to be done in the context of updated `SharedTxState`.  This is verified
-- by `prop_receivedTxIdsImpl`, `prop_collectTxsImpl` and
-- `prop_makeDecisions_acknowledged`.
--
prop_acknowledgeTxIds :: ArbDecisionContextWithReceivedTxIds
                      -> Property
prop_acknowledgeTxIds (ArbDecisionContextWithReceivedTxIds policy SharedDecisionContext { sdcSharedTxState = st } ps _ _ _) =
    case TXS.acknowledgeTxIds policy st ps of
      (numTxIdsToAck, txIdsToRequest, txs, TXS.RefCountDiff { TXS.txIdsToAck }, ps') | txIdsToRequest > 0 ->
             counterexample "number of tx ids to ack must agree with RefCountDiff"
             ( fromIntegral numTxIdsToAck
               ===
               getSum (foldMap Sum txIdsToAck)
             )

        .&&. counterexample "acknowledged txs must form a prefix"
             let unacked  = toList (unacknowledgedTxIds ps)
                 unacked' = toList (unacknowledgedTxIds ps')
             in case unacked `stripSuffix` unacked' of
               Nothing -> counterexample "acknowledged txs are not a prefix" False
               Just txIdsToAck' ->
                    txIdsToAck
                    ===
                    Map.fromListWith (+) ((,1) <$> txIdsToAck')

        .&&. counterexample "acknowledged txs" (counterexample ("numTxIdsToAck = " ++ show numTxIdsToAck)
             let acked :: [TxId]
                 acked = [ txid
                         | txid <- take (fromIntegral numTxIdsToAck) (toList $ unacknowledgedTxIds ps)
                         , Just _ <- maybeToList $ txid `Map.lookup` bufferedTxs st
                         ]
             in getTxId `map` txs === acked)
      _otherwise -> property True
  where
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix as suffix =
        reverse <$> reverse suffix `stripPrefix` reverse as


-- | Verify 'inboundStateInvariant' when acknowledging a sequence of txs.
--
prop_receivedTxIdsImpl
  :: ArbReceivedTxIds
  -> Property
prop_receivedTxIdsImpl (ArbReceivedTxIds mempoolHasTxFun txs peeraddr ps st) =
       -- InboundState invariant
       counterexample
         (  "Unacknowledged in mempool: " ++
             show (apply mempoolHasTxFun <$> toList (unacknowledgedTxIds ps)) ++ "\n"
         ++ "InboundState invariant violation:\n" ++
            show st'
         )
         (sharedTxStateInvariant st')

         -- unacknowledged txs are well formed
    .&&. counterexample "unacknowledged txids are not well formed"
         (  let unacked  = toList $ unacknowledgedTxIds ps <> txidSeq
                unacked' = toList $ unacknowledgedTxIds ps'
            in counterexample ("old & received: " ++ show unacked ++ "\n" ++
                               "new:            " ++ show unacked') $
               unacked' `isSuffixOf` unacked
         )

    .&&. -- `receivedTxIdsImpl` doesn't acknowledge any `txids`
         counterexample "acknowledged property violation"
         (  let unacked  = toList $ unacknowledgedTxIds ps
                unacked' = toList $ unacknowledgedTxIds ps'
            in unacked `isPrefixOf` unacked'
         )
  where
    st' = TXS.receivedTxIdsImpl (apply mempoolHasTxFun)
                               peeraddr 0 txidSeq txidMap st
    ps' = peerTxStates st' Map.! peeraddr

    txidSeq = StrictSeq.fromList (getTxId <$> txs)
    txidMap = Map.fromList [ (getTxId tx, getTxSize tx) | tx <- txs ]


-- | Verify that `SharedTxState` returned by `receivedTxIdsImpl` if evaluated
-- to WHNF it doesn't contain any thunks.
--
prop_receivedTxIdsImpl_nothunks
  :: ArbReceivedTxIds
  -> Property
prop_receivedTxIdsImpl_nothunks (ArbReceivedTxIds mempoolHasTxFun txs peeraddr _ st) =
    case TXS.receivedTxIdsImpl (apply mempoolHasTxFun)
                               peeraddr 0 txidSeq txidMap st of
      !st' -> case unsafeNoThunks st' of
        Nothing  -> property True
        Just ctx -> counterexample (show ctx) False
  where
    txidSeq = StrictSeq.fromList (getTxId <$> txs)
    txidMap = Map.fromList [ (getTxId tx, getTxSize tx) | tx <- txs ]


--
-- `collectTxs` properties
--


data ArbCollectTxs =
    ArbCollectTxs (Fun TxId Bool)      -- ^ mempoolHasTx
                  (Set TxId)           -- ^ requested txid's
                  (Map TxId (Tx TxId)) -- ^ received txs
                  PeerAddr             -- ^ peeraddr
                  (PeerTxState TxId (Tx TxId))
                  (SharedTxState PeerAddr TxId (Tx TxId))
                                       -- ^ 'InboundState'
    deriving Show


instance Arbitrary ArbCollectTxs where
    arbitrary = do
        Small maxTxIdsInflight <- arbitrary
        (   mempoolHasTxFun
          , (peeraddr, ps@PeerTxState { availableTxIds,
                                        requestedTxIdsInflight,
                                        requestedTxsInflight,
                                        requestedTxsInflightSize })
          , st
          , _
          )
          <- genSharedTxState maxTxIdsInflight
        requestedTxIds <- take (fromIntegral requestedTxIdsInflight)
                      <$> sublistOf (toList requestedTxsInflight)

        -- Limit the requested `txid`s to satisfy `requestedTxsInflightSize`.
        let requestedTxIds' = fmap fst
                            . takeWhile (\(_,s) -> s <= requestedTxsInflightSize)
                            $ zip requestedTxIds
                                  (scanl1 (<>) [availableTxIds Map.! txid | txid <- requestedTxIds ])

        receivedTx <- sublistOf requestedTxIds'
                  >>= traverse (\txid -> do
                                  valid <- frequency [(4, pure True), (1, pure False)]
                                  pure $ Tx { getTxId = txid,
                                              getTxSize = availableTxIds Map.! txid,
                                              getTxValid = valid })

        pure $ assert (foldMap getTxSize receivedTx <= requestedTxsInflightSize)
             $ ArbCollectTxs mempoolHasTxFun
                              (Set.fromList requestedTxIds')
                              (Map.fromList [ (getTxId tx, tx) | tx <- receivedTx ])
                              peeraddr
                              ps
                              st

    shrink (ArbCollectTxs mempoolHasTx requestedTxs receivedTxs peeraddr ps st) =
      [ ArbCollectTxs mempoolHasTx
                      requestedTxs'
                      (receivedTxs `Map.restrictKeys` requestedTxs')
                      peeraddr ps st
      | requestedTxs' <- Set.fromList <$> shrinkList (\_ -> []) (Set.toList requestedTxs)
      ]
      ++
      [ ArbCollectTxs mempoolHasTx
                      requestedTxs
                      (receivedTxs `Map.restrictKeys` receivedTxIds)
                      peeraddr ps st
      | receivedTxIds <- Set.fromList <$> shrinkList (\_ -> []) (Map.keys receivedTxs)
      ]
      ++
      [ ArbCollectTxs mempoolHasTx
                     (requestedTxs
                       `Set.intersection` unacked
                       `Set.intersection` inflightTxSet)
                     (receivedTxs
                       `Map.restrictKeys` unacked
                       `Map.restrictKeys` inflightTxSet)
                     peeraddr ps
                     st'
      | let unacked = Set.fromList
                    . toList
                    . unacknowledgedTxIds
                    $ ps
      , st'@SharedTxState { inflightTxs } <- shrinkSharedTxState (apply mempoolHasTx) st
      , let inflightTxSet = Map.keysSet inflightTxs
      , peeraddr `Map.member` peerTxStates st'
      , st' /= st
      ]


prop_collectTxs_generator
  :: ArbCollectTxs
  -> Property
prop_collectTxs_generator (ArbCollectTxs _ requestedTxIds receivedTxs peeraddr
                                            ps@PeerTxState { availableTxIds,
                                                             requestedTxsInflightSize }
                                            st) =
         counterexample "size of requested txs must not be larger than requestedTxsInflightSize"
         (requestedSize <= requestedTxsInflightSize)
    .&&. counterexample "inflightTxsSize must be greater than requestedSize"
         (inflightTxsSize st >= requestedSize)
    .&&. counterexample ("receivedTxs must be a subset of requestedTxIds "
                         ++ show (Map.keysSet receivedTxs Set.\\ requestedTxIds))
         (Map.keysSet receivedTxs `Set.isSubsetOf` requestedTxIds)
    .&&. counterexample "peerTxState"
         (Map.lookup peeraddr (peerTxStates st) === Just ps)
  where
    requestedSize = fold (availableTxIds `Map.restrictKeys` requestedTxIds)


prop_collectTxs_shrinker
  :: Fixed ArbCollectTxs
  -- ^ disabled shrinking
  -> Property
prop_collectTxs_shrinker (Fixed txs) =
    property $ foldMap (\a@(ArbCollectTxs _ _ _ _ _ st) ->
                         All . counterexample (show st) $
                               f a =/= f txs
                          .&&. sharedTxStateInvariant st
                       ) (shrink txs)
  where
    f (ArbCollectTxs _ reqSet recvMap peeraddr ps st) = (reqSet, recvMap, peeraddr, ps, st)


-- | Verify `collectTxsImpl` properties:
--
-- * verify `SharedTxState` invariant;
-- * unacknowledged txids after `collectTxsImpl` must be a suffix of the
--   original ones;
-- * progress property: we acknowledge as many `txid`s as possible
--
prop_collectTxsImpl
  :: ArbCollectTxs
  -> Property
prop_collectTxsImpl (ArbCollectTxs _mempoolHasTxFun txidsRequested txsReceived peeraddr ps st) =

      label ("number of txids inflight "  ++ labelInt 25 5 (Map.size $ inflightTxs st)) $
      label ("number of txids requested " ++ labelInt 25 5 (Set.size txidsRequested)) $
      label ("number of txids received "  ++ labelInt 10 2 (Map.size txsReceived)) $

           -- InboundState invariant
           counterexample
             (  "InboundState invariant violation:\n" ++ show st' ++ "\n"
             ++ show ps'
             )
             (sharedTxStateInvariant st')

      .&&.
           -- `collectTxsImpl` doesn't modify unacknowledged TxId's
           counterexample "acknowledged property violation"
           ( let unacked  = toList $ unacknowledgedTxIds ps
                 unacked' = toList $ unacknowledgedTxIds ps'
             in unacked === unacked'
           )
  where
    st' = TXS.collectTxsImpl peeraddr txidsRequested txsReceived st
    ps' = peerTxStates st' Map.! peeraddr


-- | Verify that `SharedTxState` returned by `collectTxsImpl` if evaluated to
-- WHNF, it doesn't contain any thunks.
--
prop_collectTxsImpl_nothunks
  :: ArbCollectTxs
  -> Property
prop_collectTxsImpl_nothunks (ArbCollectTxs _mempoolHasTxFun txidsRequested txsReceived peeraddr _ st) =
    case unsafeNoThunks $! st' of
      Nothing  -> property True
      Just ctx -> counterexample (show ctx) False
  where
    st' = TXS.collectTxsImpl peeraddr txidsRequested txsReceived st


newtype ArbTxDecisionPolicy = ArbTxDecisionPolicy TxDecisionPolicy
  deriving Show

instance Arbitrary ArbTxDecisionPolicy where
    arbitrary =
          ArbTxDecisionPolicy . fixupTxDecisionPolicy
      <$> ( TxDecisionPolicy
            <$> (getSmall . getPositive <$> arbitrary)
            <*> (getSmall . getPositive <$> arbitrary)
            <*> (SizeInBytes . getPositive <$> arbitrary)
            <*> (SizeInBytes . getPositive <$> arbitrary)
            <*> (getSmall . getPositive <$> arbitrary))

    shrink (ArbTxDecisionPolicy a@TxDecisionPolicy {
              maxNumTxIdsToRequest,
              txsSizeInflightPerPeer,
              maxTxsSizeInflight,
              txInflightMultiplicity }) =
      [ ArbTxDecisionPolicy a { maxNumTxIdsToRequest = NumTxIdsToReq x }
      | (Positive (Small x)) <- shrink (Positive (Small (getNumTxIdsToReq maxNumTxIdsToRequest)))
      ]
      ++
      [ ArbTxDecisionPolicy . fixupTxDecisionPolicy
      $ a { txsSizeInflightPerPeer = SizeInBytes s }
      | Positive s <- shrink (Positive (getSizeInBytes txsSizeInflightPerPeer))
      ]
      ++
      [ ArbTxDecisionPolicy . fixupTxDecisionPolicy
      $ a { maxTxsSizeInflight = SizeInBytes s }
      | Positive s <- shrink (Positive (getSizeInBytes maxTxsSizeInflight))
      ]
      ++
      [ ArbTxDecisionPolicy . fixupTxDecisionPolicy
      $ a { txInflightMultiplicity = x }
      | Positive (Small x) <- shrink (Positive (Small txInflightMultiplicity))
      ]


fixupTxDecisionPolicy :: TxDecisionPolicy -> TxDecisionPolicy
fixupTxDecisionPolicy a@TxDecisionPolicy { txsSizeInflightPerPeer,
                                           maxTxsSizeInflight }
 = a { txsSizeInflightPerPeer = txsSizeInflightPerPeer',
       maxTxsSizeInflight     = maxTxsSizeInflight' }
 where
   txsSizeInflightPerPeer' = min txsSizeInflightPerPeer maxTxsSizeInflight
   maxTxsSizeInflight'     = max txsSizeInflightPerPeer maxTxsSizeInflight


-- | Generate  `TxDecisionPolicy` and a valid `PeerTxState` with respect to
-- that policy.
--
data ArbPeerTxStateWithPolicy =
    ArbPeerTxStateWithPolicy {
        ptspState  :: PeerTxState TxId (Tx TxId),
        ptspPolicy :: TxDecisionPolicy
    }
    deriving Show

-- | Fix-up `PeerTxState` according to `TxDecisionPolicy`.
--
fixupPeerTxStateWithPolicy :: Ord txid
                           => TxDecisionPolicy
                           -> PeerTxState txid tx
                           -> PeerTxState txid tx
fixupPeerTxStateWithPolicy
    TxDecisionPolicy { maxUnacknowledgedTxIds,
                       maxNumTxIdsToRequest }
    ps@PeerTxState { unacknowledgedTxIds,
                     availableTxIds,
                     requestedTxsInflight,
                     requestedTxIdsInflight,
                     unknownTxs
                   }
    =
    ps { unacknowledgedTxIds    = unacknowledgedTxIds',
         availableTxIds         = availableTxIds',
         requestedTxsInflight   = requestedTxsInflight',
         requestedTxIdsInflight = requestedTxIdsInflight',
         unknownTxs             = unknownTxs'
       }
  where
    -- limit the number of unacknowledged txids, and then fix-up all the other
    -- sets.
    unacknowledgedTxIds'    = StrictSeq.take (fromIntegral maxUnacknowledgedTxIds)
                                             unacknowledgedTxIds
    unackedSet              = Set.fromList (toList unacknowledgedTxIds')
    availableTxIds'         = availableTxIds `Map.restrictKeys` unackedSet
    requestedTxsInflight'   = requestedTxsInflight `Set.intersection` unackedSet
    -- requestedTxIdsInflight must be smaller than `maxNumTxIdsToRequest, and
    -- also `requestedTxIdsInflight` and the number of `unacknowledgedTxIds'`
    -- must be smaller or equal to `maxUnacknowledgedTxIds`.
    requestedTxIdsInflight' = requestedTxIdsInflight
                              `min` maxNumTxIdsToRequest
                              `min` (maxUnacknowledgedTxIds - fromIntegral (StrictSeq.length unacknowledgedTxIds'))
    unknownTxs'             = unknownTxs `Set.intersection` unackedSet


instance Arbitrary ArbPeerTxStateWithPolicy where
    arbitrary = do
      mempoolHasTx <- arbitrary
      ArbTxDecisionPolicy policy
        <- arbitrary
      ArbPeerTxState { arbPeerTxState = ps }
        <- genArbPeerTxState
            mempoolHasTx
            (fromIntegral (maxUnacknowledgedTxIds policy))
      return ArbPeerTxStateWithPolicy { ptspState  = fixupPeerTxStateWithPolicy policy ps,
                                        ptspPolicy = policy
                                      }


prop_numTxIdsToRequest
  :: ArbPeerTxStateWithPolicy
  -> Property
prop_numTxIdsToRequest
    ArbPeerTxStateWithPolicy {
      ptspPolicy = policy@TxDecisionPolicy { maxNumTxIdsToRequest,
                                             maxUnacknowledgedTxIds },
      ptspState  = ps
    }
    =
    case TXS.numTxIdsToRequest policy ps of
      (numToReq, ps') ->
             numToReq <= maxNumTxIdsToRequest
        .&&. numToReq + requestedTxIdsInflight ps === requestedTxIdsInflight ps'
        .&&. fromIntegral (StrictSeq.length (unacknowledgedTxIds ps'))
           + requestedTxIdsInflight ps'
          <= maxUnacknowledgedTxIds


data ArbDecisionContexts txid = ArbDecisionContexts {
    arbDecisionPolicy :: TxDecisionPolicy,

    arbSharedContext  :: SharedDecisionContext PeerAddr txid (Tx txid),

    arbMempoolHasTx   :: Fun txid Bool
    -- ^ needed just for shrinking
  }

instance Show txid => Show (ArbDecisionContexts txid) where
  show ArbDecisionContexts {
      arbDecisionPolicy,
      arbSharedContext = SharedDecisionContext {
          sdcPeerGSV = gsv,
          sdcSharedTxState = st
        },
      arbMempoolHasTx
    }
    =
    intercalate "\n\t"
    [ "ArbDecisionContext"
    , show arbDecisionPolicy
    , show gsv
    , show st
    , show arbMempoolHasTx
    ]


-- | Fix-up `SharedTxState` so it satisfies `TxDecisionPolicy`.
--
fixupSharedTxStateForPolicy
  :: forall peeraddr txid tx.
     Ord txid
  => (txid -> Bool) -- ^ mempoolHasTx
  -> TxDecisionPolicy
  -> SharedTxState peeraddr txid tx
  -> SharedTxState peeraddr txid tx
fixupSharedTxStateForPolicy
    mempoolHasTx
    policy@TxDecisionPolicy {
      txsSizeInflightPerPeer,
      maxTxsSizeInflight,
      txInflightMultiplicity
    }
    st@SharedTxState { peerTxStates }
    =
    fixupSharedTxState
      mempoolHasTx
      st { peerTxStates = snd . mapAccumR fn (0, Map.empty) $ peerTxStates }
  where
    -- fixup `PeerTxState` and accumulate size of all `tx`'s in-flight across
    -- all peers.
    fn :: (SizeInBytes, Map txid Int)
       -> PeerTxState txid tx
       -> ((SizeInBytes, Map txid Int), PeerTxState txid tx)
    fn
        (sizeInflightAll, inflightMap)
        ps
        =
        ( ( sizeInflightAll + requestedTxsInflightSize'
          , inflightMap'
          )
        , ps' { requestedTxsInflight     = requestedTxsInflight',
                requestedTxsInflightSize = requestedTxsInflightSize'
              }
        )
      where
        ps' = fixupPeerTxStateWithPolicy policy ps

        (requestedTxsInflightSize', requestedTxsInflight', inflightMap') =
          Map.foldrWithKey
            (\txid txSize r@(!inflightSize, !inflightSet, !inflight) ->
              let (multiplicity, inflight') =
                    Map.alterF
                      (\case
                          Nothing -> (1, Just 1)
                          Just x  -> let x' = x + 1 in (x',  Just $! x'))
                      txid inflight
              in if inflightSize <= txsSizeInflightPerPeer
                 && sizeInflightAll + inflightSize <= maxTxsSizeInflight
                 && multiplicity <= txInflightMultiplicity
                then (txSize + inflightSize, Set.insert txid inflightSet, inflight')
                else r
            )
            (0, Set.empty, inflightMap)
            (availableTxIds ps' `Map.restrictKeys` requestedTxsInflight ps')

instance (Arbitrary txid, Ord txid, Function txid, CoArbitrary txid)
      => Arbitrary (ArbDecisionContexts txid) where

    arbitrary = do
      ArbTxDecisionPolicy policy <- arbitrary
      (mempoolHasTx, _ps, st, _) <-
        genSharedTxState (fromIntegral $ maxNumTxIdsToRequest policy)
      let pss   = Map.toList (peerTxStates st)
          peers = fst `map` pss
      -- each peer must have a GSV
      gsvs <- zip peers
          <$> infiniteListOf (unPeerGSVT <$> arbitrary)
      let st' = fixupSharedTxStateForPolicy
                  (apply mempoolHasTx) policy st

      return $ ArbDecisionContexts {
          arbDecisionPolicy    = policy,
          arbMempoolHasTx      = mempoolHasTx,
          arbSharedContext     = SharedDecisionContext {
              sdcPeerGSV       = Map.fromList gsvs,
              sdcSharedTxState = st'
            }
          }

    shrink a@ArbDecisionContexts {
               arbDecisionPolicy = policy,
               arbMempoolHasTx   = mempoolHasTx,
               arbSharedContext = b@SharedDecisionContext {
                   sdcPeerGSV = gsvs,
                   sdcSharedTxState = sharedState
                 }
               } =
        -- shrink shared state
        [ a { arbSharedContext = b { sdcSharedTxState = sharedState'' } }
        | sharedState' <- shrinkSharedTxState (apply mempoolHasTx) sharedState
        , let sharedState'' = fixupSharedTxStateForPolicy
                                (apply mempoolHasTx) policy sharedState'
        , sharedState'' /= sharedState
        ]
        ++
        -- shrink peers; note all peers are present in `sdcPeerGSV`.
        [ a { arbSharedContext = SharedDecisionContext {
                                   sdcPeerGSV       = gsvs',
                                   sdcSharedTxState = sharedState'
                                 } }
        | -- shrink the set of peers
          peers' <- Set.fromList <$> shrinkList (const []) (Map.keys gsvs)
        , let gsvs' = gsvs `Map.restrictKeys` peers'
              sharedState' =
                  fixupSharedTxStateForPolicy
                    (apply mempoolHasTx) policy
                $ sharedState { peerTxStates = peerTxStates sharedState
                                               `Map.restrictKeys`
                                               peers'
                              }
        , sharedState' /= sharedState
        ]


prop_ArbDecisionContexts_generator
  :: ArbDecisionContexts TxId
  -> Property
prop_ArbDecisionContexts_generator
  ArbDecisionContexts { arbSharedContext = SharedDecisionContext { sdcSharedTxState = st } }
  =
  -- whenFail (pPrint a) $
  sharedTxStateInvariant st


prop_ArbDecisionContexts_shrinker
  :: ArbDecisionContexts TxId
  -> All
prop_ArbDecisionContexts_shrinker
  ctx
  =
  foldMap (\a ->
            All
          . counterexample (show a)
          . sharedTxStateInvariant
          . sdcSharedTxState
          . arbSharedContext
          $ a)
        $ shrink ctx


-- | Verify that `makeDecisions` preserves the `SharedTxState` invariant.
--
prop_makeDecisions_sharedstate
  :: ArbDecisionContexts TxId
  -> Property
prop_makeDecisions_sharedstate
    ArbDecisionContexts { arbDecisionPolicy = policy,
                          arbSharedContext = sharedCtx } =
    let (sharedState, decisions) = TXS.makeDecisions policy sharedCtx (peerTxStates (sdcSharedTxState sharedCtx))
    in counterexample (show sharedState)
     $ counterexample (show decisions)
     $ sharedTxStateInvariant sharedState


-- | Verify that `makeDecisions`:
--
-- * modifies `inflightTxs` map by adding `tx`s which are inflight;
-- * updates `requestedTxsInflightSize` correctly;
-- * in-flight `tx`s set is disjoint with `bufferedTxs`;
-- * requested `tx`s are coming from `availableTxIds`.
--
prop_makeDecisions_inflight
  :: ArbDecisionContexts TxId
  -> Property
prop_makeDecisions_inflight
    ArbDecisionContexts {
      arbDecisionPolicy = policy,
      arbSharedContext  = sharedCtx@SharedDecisionContext {
                            sdcSharedTxState = sharedState
                          }
    }
    =
    let (sharedState', decisions) = TXS.makeDecisions policy sharedCtx (peerTxStates sharedState)

        inflightSet :: Set TxId
        inflightSet = foldMap txdTxsToRequest decisions

        inflightSize :: Map PeerAddr SizeInBytes
        inflightSize = Map.foldrWithKey
                        (\peer TxDecision { txdTxsToRequest } m ->
                          Map.insert peer
                            (foldMap (\txid -> fromMaybe 0 $ Map.lookup peer (peerTxStates sharedState)
                                                         >>= Map.lookup txid . availableTxIds)
                                     txdTxsToRequest)
                            m
                        ) Map.empty decisions

        bufferedSet :: Set TxId
        bufferedSet = Map.keysSet (bufferedTxs sharedState)
    in
        counterexample (show sharedState') $
        counterexample (show decisions) $

        -- 'inflightTxs' set is increased by exactly the requested txs
        counterexample (concat
                          [ show inflightSet
                          , " not a subset of "
                          , show (inflightTxs sharedState')
                          ])
                       ( inflightSet <> Map.keysSet (inflightTxs sharedState')
                         ===
                         Map.keysSet (inflightTxs sharedState')
                       )

    .&&.

        -- for each peer size in flight is equal to the original size in flight
        -- plus size of all requested txs
        property
          (fold
            (Map.merge
              (Map.mapMaybeMissing
                (\peer a ->
                  Just ( All
                       . counterexample
                           ("missing peer in requestedTxsInflightSize: " ++ show peer)
                       $ (a === 0))))
              (Map.mapMaybeMissing (\_ _ -> Nothing))
              (Map.zipWithMaybeMatched
                (\peer delta PeerTxState { requestedTxsInflightSize } ->
                  let original =
                        case Map.lookup peer (peerTxStates sharedState) of
                          Nothing                                           -> 0
                          Just PeerTxState { requestedTxsInflightSize = a } -> a
                  in Just ( All
                          . counterexample (show peer)
                          $ original + delta
                            ===
                            requestedTxsInflightSize
                          )
                ))
              inflightSize
              (peerTxStates sharedState')))

    .&&. counterexample ("requested txs must not be buffered: "
                         ++ show (inflightSet `Set.intersection` bufferedSet))
         (inflightSet `Set.disjoint` bufferedSet)

    .&&. counterexample "requested txs must be available"
         ( fold $
           Map.merge
             (Map.mapMissing (\peeraddr _ ->
                               All $
                               counterexample ("peer missing in peerTxStates " ++ show peeraddr)
                               False))
             (Map.mapMissing (\_ _ -> All True))
             (Map.zipWithMatched (\peeraddr a b -> All
                                                 . counterexample (show peeraddr)
                                                 $ a `Set.isSubsetOf` b))
             -- map of requested txs
             (Map.fromList [ (peeraddr, txids)
                           | (peeraddr, TxDecision { txdTxsToRequest = txids })
                             <- Map.assocs decisions
                           ])
             -- map of available txs
             (Map.map (Map.keysSet . availableTxIds)
                      (peerTxStates sharedState)))


-- | Verify that `makeTxDecisions` obeys `TxDecisionPolicy`.
--
prop_makeDecisions_policy
  :: ArbDecisionContexts TxId
  -> Property
prop_makeDecisions_policy
    ArbDecisionContexts {
      arbDecisionPolicy = policy@TxDecisionPolicy { maxTxsSizeInflight,
                                                    txsSizeInflightPerPeer,
                                                    txInflightMultiplicity },
      arbSharedContext  = sharedCtx@SharedDecisionContext { sdcSharedTxState = sharedState }
    } =
    let (sharedState', _decisions) = TXS.makeDecisions policy sharedCtx (peerTxStates sharedState)
        maxTxsSizeInflightEff      = maxTxsSizeInflight + maxTxSize
        txsSizeInflightPerPeerEff  = txsSizeInflightPerPeer + maxTxSize

        sizeInflight =
          foldMap (\PeerTxState { availableTxIds, requestedTxsInflight } ->
                     fold (availableTxIds `Map.restrictKeys` requestedTxsInflight))
                  (peerTxStates sharedState')

    in counterexample (show sharedState') $

         -- size of txs inflight cannot exceed `maxTxsSizeInflight` by more
         -- than maximal tx size.
         counterexample ("txs inflight exceed limit " ++ show (sizeInflight, maxTxsSizeInflightEff))
         (sizeInflight <= maxTxsSizeInflightEff)
    .&&.
         -- size in flight for each peer cannot exceed `txsSizeInflightPerPeer`
         counterexample "size in flight per peer vaiolation" (
           foldMap
             (\PeerTxState { availableTxIds, requestedTxsInflight } ->
               let inflight = fold (availableTxIds `Map.restrictKeys` requestedTxsInflight)
               in All $ counterexample (show (inflight, txsSizeInflightPerPeerEff)) $
                 inflight
                 <=
                 txsSizeInflightPerPeerEff
             )
             (peerTxStates sharedState')
         )

    .&&.
         (
         -- none of the multiplicities should go above the
         -- `txInflightMultiplicity`
         let inflight = inflightTxs sharedState'
         in
              counterexample ("multiplicities violation: " ++ show inflight)
            . foldMap (All . (<= txInflightMultiplicity))
            $ inflight
         )


-- | Verify that `makeDecisions` and `acknowledgeTxIds` are compatible.
--
prop_makeDecisions_acknowledged
  :: ArbDecisionContexts TxId
  -> Property
prop_makeDecisions_acknowledged
    ArbDecisionContexts { arbDecisionPolicy = policy,
                          arbSharedContext =
                            sharedCtx@SharedDecisionContext {
                              sdcSharedTxState = sharedTxState
                            }
                        } =
    whenFail (pPrintOpt CheckColorTty defaultOutputOptionsDarkBg { outputOptionsCompact = True } sharedTxState) $
    let (_, decisions) = TXS.makeDecisions policy sharedCtx (peerTxStates sharedTxState)

        ackFromDecisions :: Map PeerAddr NumTxIdsToAck
        ackFromDecisions = Map.fromList
                         [ (peer, txdTxIdsToAcknowledge)
                         | (peer, TxDecision { txdTxIdsToAcknowledge })
                           <- Map.assocs decisions
                         ]

        ackFromState :: Map PeerAddr NumTxIdsToAck
        ackFromState =
            Map.map (\ps -> case TXS.acknowledgeTxIds policy sharedTxState ps of
                              (a, _, _, _, _) -> a)
          . peerTxStates
          $ sharedTxState

    in counterexample (show (ackFromDecisions, ackFromState))
     . fold
     $ Map.merge
        -- it is an error if `ackFromDecisions` contains a result which is
        -- missing in `ackFromState`
        (Map.mapMissing (\addr num -> All $ counterexample ("missing " ++ show (addr, num)) False))
        -- if `ackFromState` contains an enty which is missing in
        -- `ackFromDecisions` it must be `0`; `makeDecisions` might want to
        -- download some `tx`s even if there's nothing to acknowledge
        (Map.mapMissing (\_ d -> All (d === 0)))
        -- if both entries exists they must be equal
        (Map.zipWithMatched (\_ a b -> All (a === b)))
        ackFromDecisions
        ackFromState


-- | `makeDecision` is exhaustive in the sense that it returns an empty
-- decision list on a state returned by a prior call of `makeDecision`.
--
prop_makeDecisions_exhaustive
  :: ArbDecisionContexts TxId
  -> Property
prop_makeDecisions_exhaustive
  ArbDecisionContexts {
    arbDecisionPolicy = policy,
    arbSharedContext =
      sharedCtx@SharedDecisionContext {
        sdcSharedTxState = sharedTxState
      }
  }
  =
  let (sharedTxState',  decisions')
        = TXS.makeDecisions policy
                            sharedCtx
                            (peerTxStates sharedTxState)
      (sharedTxState'', decisions'')
        = TXS.makeDecisions policy
                            sharedCtx { sdcSharedTxState = sharedTxState' }
                            (peerTxStates sharedTxState')
  in counterexample ("decisions':  " ++ show decisions')
   . counterexample ("state':      " ++ show sharedTxState')
   . counterexample ("decisions'': " ++ show decisions'')
   . counterexample ("state'':     " ++ show sharedTxState'')
   $ null decisions''


data ArbDecisionContextWithReceivedTxIds = ArbDecisionContextWithReceivedTxIds {
      adcrDecisionPolicy :: TxDecisionPolicy,
      adcrSharedContext  :: SharedDecisionContext PeerAddr TxId (Tx TxId),
      adcrPeerTxState    :: PeerTxState TxId (Tx TxId),
      adcrMempoolHasTx   :: Fun TxId Bool,
      adcrTxsToAck       :: [Tx TxId],
      -- txids to acknowledge
      adcrPeerAddr       :: PeerAddr
      -- the peer which owns the acknowledged txids
    }
    deriving Show


instance Arbitrary ArbDecisionContextWithReceivedTxIds where
    arbitrary = do
      ArbTxDecisionPolicy policy <- arbitrary
      ArbReceivedTxIds mempoolHasTx
                       txIdsToAck
                       peeraddr
                       ps
                       st
        <- arbitrary

      let st' = fixupSharedTxStateForPolicy
                  (apply mempoolHasTx)
                  policy st
          ps' = fixupPeerTxStateWithPolicy policy ps
          txIdsToAck' = take (fromIntegral (TXS.requestedTxIdsInflight $ peerTxStates st' Map.! peeraddr)) txIdsToAck
          peers = Map.keys (peerTxStates st')

      gsvs <- zip peers
          <$> infiniteListOf (unPeerGSVT <$> arbitrary)

      return ArbDecisionContextWithReceivedTxIds {
          adcrDecisionPolicy = policy,
          adcrSharedContext  = SharedDecisionContext {
              sdcPeerGSV       = Map.fromList gsvs,
              sdcSharedTxState = st'
            },
          adcrPeerTxState    = ps',
          adcrMempoolHasTx   = mempoolHasTx,
          adcrTxsToAck       = txIdsToAck',
          adcrPeerAddr       = peeraddr
        }

    shrink ArbDecisionContextWithReceivedTxIds {
        adcrDecisionPolicy = policy,
        adcrSharedContext  = ctx,
        adcrPeerTxState    = ps,
        adcrMempoolHasTx   = mempoolHasTx,
        adcrTxsToAck       = txIdsToAck,
        adcrPeerAddr       = peeraddr
      }
      =
      [ ArbDecisionContextWithReceivedTxIds {
          adcrDecisionPolicy = policy',
          adcrSharedContext  = ctx',
          adcrPeerTxState    = ps,
          adcrMempoolHasTx   = mempoolHasTx',
          adcrTxsToAck       = txIdsToAck',
          adcrPeerAddr       = peeraddr
        }
      | ArbDecisionContexts {
          arbDecisionPolicy = policy',
          arbSharedContext  = ctx'@SharedDecisionContext { sdcSharedTxState = st' },
          arbMempoolHasTx   = mempoolHasTx'
        }
          <- shrink ArbDecisionContexts {
                 arbDecisionPolicy = policy,
                 arbSharedContext  = ctx,
                 arbMempoolHasTx   = mempoolHasTx
               }
      , peeraddr `Map.member` peerTxStates st'
      , let txIdsToAck' = take ( fromIntegral
                               . TXS.requestedTxIdsInflight
                               $ peerTxStates st' Map.! peeraddr
                               )
                               txIdsToAck
      ]


-- | `filterActivePeers` should not change decisions made by `makeDecisions`
--
prop_filterActivePeers_not_limitting_decisions
    :: ArbDecisionContexts TxId
    -> Property
prop_filterActivePeers_not_limitting_decisions
    ArbDecisionContexts {
        arbDecisionPolicy = policy,
      arbSharedContext =
        sharedCtx@SharedDecisionContext { sdcSharedTxState = st }
    }
    =
    counterexample (unlines
                   ["decisions:        " ++ show decisions
                   ,"                  " ++ show decisionPeers
                   ,"active decisions: " ++ show decisionsOfActivePeers
                   ,"                  " ++ show activePeers]) $

    counterexample ("active peers does not restrict the total number of valid decisions available"
                     ++ show (decisionsOfActivePeers  Map.\\ decisions)
                   )
                   (Map.keysSet decisionsOfActivePeers  `Set.isSubsetOf` Map.keysSet decisions)
  where
    activePeersMap    = TXS.filterActivePeers policy st
    activePeers       = Map.keysSet activePeersMap
    (_, decisionsOfActivePeers)
                      = TXS.makeDecisions policy sharedCtx activePeersMap

    (_, decisions)    = TXS.makeDecisions policy sharedCtx (peerTxStates st)
    decisionPeers     = Map.keysSet decisions


-- TODO: makeDecisions property: all peers which have txid's to ack are
-- included, this would catch the other bug, and it's important for the system
-- to run well.

--
-- Auxiliary functions
--

labelInt :: (Integral a, Eq a, Ord a, Show a)
         => a -- ^ upper bound
         -> a -- ^ width
         -> a -- ^ value
         -> String
labelInt _ _ 0 = "[0, 0]"
labelInt bound _ b | b >= bound = "[" ++ show bound ++ ", inf)"
labelInt _ a b =
    let l = a * (b `div` a)
        u = l + a
    in (if l == 0 then "(" else "[")
       ++ show l ++ ", "
       ++ show u ++ ")"
