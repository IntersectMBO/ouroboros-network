{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
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

module Test.Ouroboros.Network.TxSubmission (tests) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM
import Control.Exception (SomeException (..), assert)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim hiding (SimResult)
import Control.Tracer (Tracer (..), contramap, nullTracer, showTracing,
           traceWith)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable as Foldable (find, fold, foldl', toList)
import Data.Function (on)
import Data.List (intercalate, isPrefixOf, isSuffixOf, mapAccumR, nub, nubBy,
           stripPrefix)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Monoid (Sum (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word16)
import GHC.Generics (Generic)

import Network.TypedProtocol.Codec

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Driver
import Ouroboros.Network.NodeToNode (NodeToNodeVersion (..))
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound
import Ouroboros.Network.TxSubmission.Inbound.Decision
import Ouroboros.Network.TxSubmission.Inbound.Decision qualified as TXS
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Inbound.State (PeerTxState (..),
           SharedTxState (..))
import Ouroboros.Network.TxSubmission.Inbound.State qualified as TXS
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Testing.Utils
import Test.Ouroboros.Network.BlockFetch (PeerGSVT (..))

import Test.QuickCheck
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Monoids (All (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Pretty.Simple
import Text.Printf

import GHC.Stack (HasCallStack)


tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission"
  [ testProperty "txSubmission" prop_txSubmission
  , testGroup "State"
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
    , testProperty "hasTxIdsToAcknowledge"      prop_hasTxIdsToAcknowledge
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
    , testProperty "receivedTxIds"             prop_makeDecisions_receivedTxIds
    , testProperty "collectTxs"                prop_makeDecisions_collectTxs
    ]
  , testGroup "Registry"
    [ testGroup "filterActivePeers"
      [ testProperty "not limiting decisions"  prop_filterActivePeers_not_limitting_decisions
      ]
    ]
  ]


data Tx txid = Tx {
    getTxId    :: !txid,
    getTxSize  :: !SizeInBytes,
    -- | If false this means that when this tx will be submitted to a remote
    -- mempool it will not be valid.  The outbound mempool might contain
    -- invalid tx's in this sense.
    getTxValid :: !Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks txid => NoThunks (Tx txid)
instance ShowProxy txid => ShowProxy (Tx txid) where
    showProxy _ = "Tx " ++ showProxy (Proxy :: Proxy txid)

instance Arbitrary txid => Arbitrary (Tx txid) where
    arbitrary =
      Tx <$> arbitrary
         <*> chooseEnum (0, maxTxSize)
             -- note:
             -- generating small tx sizes avoids overflow error when semigroup
             -- instance of `SizeInBytes` is used (summing up all inflight tx
             -- sizes).
         <*> frequency [ (3, pure True)
                       , (1, pure False)
                       ]


-- maximal tx size
maxTxSize :: SizeInBytes
maxTxSize = 65536

type TxId = Int

newtype Mempool m txid = Mempool (TVar m (Seq (Tx txid)))


emptyMempool :: MonadSTM m => m (Mempool m txid)
emptyMempool = Mempool <$> newTVarIO Seq.empty

newMempool :: ( MonadSTM m
              , Eq txid
              )
           => [Tx txid]
           -> m (Mempool m txid)
newMempool = fmap Mempool
           . newTVarIO
           . Seq.fromList

readMempool :: MonadSTM m => Mempool m txid -> m [Tx txid]
readMempool (Mempool mempool) = toList <$> readTVarIO mempool


getMempoolReader :: forall txid m.
                    ( MonadSTM m
                    , Eq txid
                    , Show txid
                    )
                 => Mempool m txid
                 -> TxSubmissionMempoolReader txid (Tx txid) Int m
getMempoolReader (Mempool mempool) =
    TxSubmissionMempoolReader { mempoolGetSnapshot, mempoolZeroIdx = 0 }
  where
    mempoolGetSnapshot :: STM m (MempoolSnapshot txid (Tx txid) Int)
    mempoolGetSnapshot = getSnapshot <$> readTVar mempool

    getSnapshot :: Seq (Tx txid)
                -> MempoolSnapshot txid (Tx txid) Int
    getSnapshot seq =
      MempoolSnapshot {
          mempoolTxIdsAfter =
            \idx -> zipWith f [idx + 1 ..] (toList $ Seq.drop idx seq),
          -- why do I need to use `pred`?
          mempoolLookupTx   = flip Seq.lookup seq . pred,
          mempoolHasTx      = \txid -> isJust $ find (\tx -> getTxId tx == txid) seq
       }

    f :: Int -> Tx txid -> (txid, Int, SizeInBytes)
    f idx Tx {getTxId, getTxSize} = (getTxId, idx, getTxSize)


getMempoolWriter :: forall txid m.
                    ( MonadSTM m
                    , Ord txid
                    , Eq txid
                    )
                 => Mempool m txid
                 -> TxSubmissionMempoolWriter txid (Tx txid) Int m
getMempoolWriter (Mempool mempool) =
    TxSubmissionMempoolWriter {
        txId = getTxId,

        mempoolAddTxs = \txs -> do
          atomically $ do
            mempoolTxs <- readTVar mempool
            let currentIds = Set.fromList (map getTxId (toList mempoolTxs))
                validTxs = nubBy (on (==) getTxId)
                         $ filter
                             (\Tx { getTxId, getTxValid } ->
                                  getTxValid
                               && getTxId `Set.notMember` currentIds)
                           txs
                mempoolTxs' = Foldable.foldl' (Seq.|>) mempoolTxs validTxs
            writeTVar mempool mempoolTxs'
            return (map getTxId validTxs)
      }


txSubmissionCodec2 :: MonadST m
                   => Codec (TxSubmission2 Int (Tx Int))
                            CBOR.DeserialiseFailure m ByteString
txSubmissionCodec2 =
    codecTxSubmission2 CBOR.encodeInt CBOR.decodeInt
                       encodeTx decodeTx
  where
    encodeTx Tx {getTxId, getTxSize, getTxValid} =
         CBOR.encodeListLen 3
      <> CBOR.encodeInt getTxId
      <> CBOR.encodeWord32 (getSizeInBytes getTxSize)
      <> CBOR.encodeBool getTxValid

    decodeTx = do
      _ <- CBOR.decodeListLen
      Tx <$> CBOR.decodeInt
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> CBOR.decodeBool


txSubmissionSimulation
  :: forall m txid.
     ( MonadAsync m
     , MonadDelay m
     , MonadFork  m
     , MonadMask  m
     , MonadMVar  m
     , MonadSay   m
     , MonadST    m
     , MonadSTM   m
     , MonadTimer m
     , MonadThrow m
     , MonadThrow (STM m)
     , MonadMonotonicTime m
     , Ord txid
     , Eq  txid
     , ShowProxy txid
     , NoThunks (Tx txid)

     , txid ~ Int
     )
  => Tracer m (String, TraceSendRecv (TxSubmission2 txid (Tx txid)))
  -> NumTxIdsToAck
  -> [Tx txid]
  -> ControlMessageSTM m
  -> Maybe DiffTime
  -> Maybe DiffTime
  -> m ([Tx txid], [Tx txid])
txSubmissionSimulation tracer maxUnacked outboundTxs
                       controlMessageSTM
                       inboundDelay outboundDelay = do

    inboundMempool  <- emptyMempool
    outboundMempool <- newMempool outboundTxs
    (outboundChannel, inboundChannel) <- createConnectedChannels
    outboundAsync <-
      async $ runPeerWithLimits
                (("OUTBOUND",) `contramap` tracer)
                txSubmissionCodec2
                (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                timeLimitsTxSubmission2
                (maybe id delayChannel outboundDelay outboundChannel)
                (txSubmissionClientPeer (outboundPeer outboundMempool))

    inboundAsync <-
      async $ runPipelinedPeerWithLimits
                (("INBOUND",) `contramap` verboseTracer)
                txSubmissionCodec2
                (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                timeLimitsTxSubmission2
                (maybe id delayChannel inboundDelay inboundChannel)
                (txSubmissionServerPeerPipelined (inboundPeer inboundMempool))

    _ <- waitAnyCancel [ outboundAsync, inboundAsync ]

    inmp <- readMempool inboundMempool
    outmp <- readMempool outboundMempool
    return (inmp, outmp)
  where

    outboundPeer :: Mempool m txid -> TxSubmissionClient txid (Tx txid) m ()
    outboundPeer outboundMempool =
      txSubmissionOutbound
        nullTracer
        maxUnacked
        (getMempoolReader outboundMempool)
        NodeToNodeV_7
        controlMessageSTM

    inboundPeer :: Mempool m txid -> TxSubmissionServerPipelined txid (Tx txid) m ()
    inboundPeer inboundMempool =
      txSubmissionInbound
        nullTracer
        maxUnacked
        (getMempoolReader inboundMempool)
        (getMempoolWriter inboundMempool)
        NodeToNodeV_7


newtype LargeNonEmptyList a = LargeNonEmpty { getLargeNonEmpty :: [a] }
  deriving Show

instance Arbitrary a => Arbitrary (LargeNonEmptyList a) where
    arbitrary =
      LargeNonEmpty <$> suchThat (resize 500 (listOf arbitrary)) ((>25) . length)

prop_txSubmission :: Positive Word16
                  -> NonEmptyList (Tx Int)
                  -> Maybe (Positive SmallDelay)
                  -- ^ The delay must be smaller (<) than 5s, so that overall
                  -- delay is less than 10s, otherwise 'smallDelay' in
                  -- 'timeLimitsTxSubmission2' will kick in.
                  -> Property
prop_txSubmission (Positive maxUnacked) (NonEmpty outboundTxs) delay =
    let mbDelayTime = getSmallDelay . getPositive <$> delay
        tr = (runSimTrace $ do
            controlMessageVar <- newTVarIO Continue
            _ <-
              async $ do
                threadDelay
                  (fromMaybe 1 mbDelayTime
                    * realToFrac (length outboundTxs `div` 4))
                atomically (writeTVar controlMessageVar Terminate)
            txSubmissionSimulation
              verboseTracer
              (NumTxIdsToAck maxUnacked) outboundTxs
              (readTVar controlMessageVar)
              mbDelayTime mbDelayTime
            ) in
    ioProperty $ do
        tr' <- evaluateTrace tr
        case tr' of
             SimException e trace -> do
                return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn (inmp, outmp) _trace -> do
                 -- printf "Log: %s\n" (intercalate "\n" _trace)
                 let outUniqueTxIds = nubBy (on (==) getTxId) outmp
                     outValidTxs    = filter getTxValid outmp
                 case (length outUniqueTxIds == length outmp, length outValidTxs == length outmp) of
                      (True, True) ->
                          -- If we are presented with a stream of unique txids for valid
                          -- transactions the inbound transactions should match the outbound
                          -- transactions exactly.
                          return $ inmp === take (length inmp) outValidTxs
                      (True, False) ->
                          -- If we are presented with a stream of unique txids then we should have
                          -- fetched all valid transactions.
                          return $ inmp === take (length inmp) outValidTxs
                      (False, True) ->
                          -- If we are presented with a stream of valid txids then we should have
                          -- fetched some version of those transactions.
                          return $ map getTxId inmp === take (length inmp) (map getTxId $
                              filter getTxValid outUniqueTxIds)
                      (False, False)
                           -- If we are presented with a stream of valid and invalid Txs with
                           -- duplicate txids we're content with completing the protocol
                           -- without error.
                           -> return $ property True


-- TODO: Belongs in iosim.
data SimResult a = SimReturn a [String]
                 | SimException SomeException [String]
                 | SimDeadLock [String]

-- Traverses a list of trace events and returns the result along with all log messages.
-- Incase of a pure exception, ie an assert, all tracers evaluated so far are returned.
evaluateTrace :: SimTrace a -> IO (SimResult a)
evaluateTrace = go []
  where
    go as tr = do
      r <- try (evaluate tr)
      case r of
        Right (SimTrace _ _ _ (EventSay s) tr')      -> go (s : as) tr'
        Right (SimTrace _ _ _ _ tr' )                -> go as tr'
        Right (SimPORTrace _ _ _ _ (EventSay s) tr') -> go (s : as) tr'
        Right (SimPORTrace _ _ _ _ _ tr' )           -> go as tr'
        Right (TraceMainReturn _ _ a _)              -> pure $ SimReturn a (reverse as)
        Right (TraceMainException _ _ e _)           -> pure $ SimException e (reverse as)
        Right (TraceDeadlock _ _)                    -> pure $ SimDeadLock (reverse as)
        Right TraceLoop                              -> error "IOSimPOR step time limit exceeded"
        Right (TraceInternalError e)                 -> error ("IOSim: " ++ e)
        Left  (SomeException e)                      -> pure $ SimException (SomeException e) (reverse as)

data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !Time
    , wtatWithinThread :: !String
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

verboseTracer :: forall a m.
                       ( MonadAsync m
                       , MonadDelay m
                       , MonadSay m
                       , MonadMonotonicTime m
                       , Show a
                       )
               => Tracer m a
verboseTracer = threadAndTimeTracer $ showTracing $ Tracer say

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadDelay m
                       , MonadMonotonicTime m
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getMonotonicTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now (show tid) s


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
-- Generate `InboudState`
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
                    unknownTxs }
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
prop_acknowledgeTxIds :: ArbReceivedTxIds
                      -> Property
prop_acknowledgeTxIds (ArbReceivedTxIds _mempoolHasTxFun _txs _peeraddr ps st) =
    case TXS.acknowledgeTxIds st ps of
      (numTxIdsToAck, txs, TXS.RefCountDiff { TXS.txIdsToAck }, ps') ->
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
  where
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix as suffix =
        reverse <$> reverse suffix `stripPrefix` reverse as


-- | Verify that `hasTxIdsToAcknowledge` and `acknowledgeTxIds` are compatible.
--
prop_hasTxIdsToAcknowledge
  :: ArbReceivedTxIds
  -> Property
prop_hasTxIdsToAcknowledge (ArbReceivedTxIds _mempoolHasTxFun _txs _peeraddr ps st) =
    case ( TXS.hasTxIdsToAcknowledge st ps
         , TXS.acknowledgeTxIds st ps
         ) of
      (canAck, (numTxIdsToAck, _, _, _)) ->
        canAck === (numTxIdsToAck > 0)


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
            <$> (getSmall <$> arbitrary)
            <*> (getSmall <$> arbitrary)
            <*> (SizeInBytes . getPositive <$> arbitrary)
            <*> (SizeInBytes . getPositive <$> arbitrary)
            <*> (getPositive <$> arbitrary))

    shrink (ArbTxDecisionPolicy a@TxDecisionPolicy {
              maxNumTxIdsToRequest,
              txsSizeInflightPerPeer,
              maxTxsSizeInflight,
              txInflightMultiplicity }) =
      [ ArbTxDecisionPolicy a { maxNumTxIdsToRequest = NumTxIdsToReq x }
      | x <- shrink (getNumTxIdsToReq maxNumTxIdsToRequest)
      ]
      ++
      [ ArbTxDecisionPolicy . fixupTxDecisionPolicy
      $ a { txsSizeInflightPerPeer = SizeInBytes s }
      | s <- shrink (getSizeInBytes txsSizeInflightPerPeer)
      ]
      ++
      [ ArbTxDecisionPolicy . fixupTxDecisionPolicy
      $ a { maxTxsSizeInflight = SizeInBytes s }
      | s <- shrink (getSizeInBytes maxTxsSizeInflight)
      ]
      ++
      [ ArbTxDecisionPolicy . fixupTxDecisionPolicy
      $ a { txInflightMultiplicity = x }
      | Positive x <- shrink (Positive txInflightMultiplicity)
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
            Map.map (\ps -> case TXS.acknowledgeTxIds sharedTxState ps of
                              (a, _, _, _) -> a)
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
                       _ps
                       st
        <- arbitrary

      let st' = fixupSharedTxStateForPolicy
                  (apply mempoolHasTx)
                  policy st
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
          adcrMempoolHasTx   = mempoolHasTx,
          adcrTxsToAck       = txIdsToAck',
          adcrPeerAddr       = peeraddr
        }

    shrink ArbDecisionContextWithReceivedTxIds {
        adcrDecisionPolicy = policy,
        adcrSharedContext  = ctx,
        adcrMempoolHasTx   = mempoolHasTx,
        adcrTxsToAck       = txIdsToAck,
        adcrPeerAddr       = peeraddr
      }
      =
      [ ArbDecisionContextWithReceivedTxIds {
          adcrDecisionPolicy = policy',
          adcrSharedContext  = ctx',
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


-- | `receivedTxIdsImpl` and `makeDecisions` have a non trivial commutator (e.g.
-- they don't commute in an interesting way).
--
prop_makeDecisions_receivedTxIds
  :: HasCallStack
  => ArbDecisionContextWithReceivedTxIds
  -> Property
prop_makeDecisions_receivedTxIds
    ArbDecisionContextWithReceivedTxIds {
      adcrDecisionPolicy = policy,
      adcrSharedContext  = ctx@SharedDecisionContext {
          sdcSharedTxState = st
        },
      adcrMempoolHasTx   = mempoolHasTx,
      adcrTxsToAck       = txs,
      adcrPeerAddr       = peeraddr
    }
    =
    counterexample ("st'   = " ++ show st') $
    counterexample ("st''  = " ++ show st'') $
    counterexample ("stA'  = " ++ show stA') $
    counterexample ("stA'' = " ++ show stA'') $
    counterexample ("txDecisions  = " ++ show txDecisions) $
    counterexample ("txDecisionsA = " ++ show txDecisionsA) $

    counterexample "state property failure" (
      -- States should be comparable; although not identical:
      --  1. number of txids in-flight might be smaller if we first `makeDecision`
      --  and then `receivedTxIdsImpl`.
      --  2. it could happen that we acknowledge and GC a txid which is then added
      --  by `receivedTxIdsImpl`, which leads to a missing txid in `bufferedTxs`
      --  compared to do the other way around
      --  3. `availableTxs` might be smaller if we first `makeDecision` because we
      --  might acknowledge a txid which is removed from `availableTxs` and after
      --  calling `receivedTxIdsImpl` we won't get back the `txid` entry in
      --  `availableTxs`
      --  4. `unacknowledgedTxs` might be smaller if we call `makeDecision` first,
      --  simply because some of `txids` might be removed from `bufferedTxs`.
      --
      --  For simplicity we ignore differences in `bufferedTxs` and
      --  `referenceCounts` and thus we set them to empty maps.
      st''  { bufferedTxs     = Map.empty,
              referenceCounts = Map.empty
            }
      ===
      stA'' { peerTxStates    =
                let fn :: PeerTxState TxId (Tx TxId) -> PeerTxState TxId (Tx TxId)
                    fn ps = snd . TXS.numTxIdsToRequest policy -- ad 2.
                          $ ps { unacknowledgedTxIds = unacknowledgedTxIds',
                                 availableTxIds      = (availableTxIds ps <> txidMap) -- ad 3.
                                                       `Map.restrictKeys`
                                                       Set.fromList (toList unacknowledgedTxIds')
                               }
                      where
                        unacknowledgedTxIds' = StrictSeq.dropWhileL
                                                 (\txid -> txid `Map.member` bufferedTxs st -- ad 4.
                                                        || applyFun mempoolHasTx txid)
                                             $ unacknowledgedTxIds ps
                in
                Map.adjust fn peeraddr (peerTxStates stA''),
              bufferedTxs     = Map.empty,
              referenceCounts = Map.empty
            }
    )

    .&&.

    counterexample "unacknowledgedTxIds property failure" (
      Map.findWithDefault 0 peeraddr (Map.map txdTxIdsToAcknowledge txDecisions)
      ===
      Map.findWithDefault 0 peeraddr (Map.map txdTxIdsToAcknowledge txDecisionsA)
        -- account for txids which could be acknowledged because they were
        -- buffered in `st`
        + foldr (\txid x -> if txid `Map.member` bufferedTxs st
                            then x + 1
                            else 0) 0
                (TXS.unacknowledgedTxIds (peerTxStates stA'' Map.! peeraddr))

    )

    .&&.

    counterexample "txdTxsToRequest proporety failure" (
      Map.filter (not . Set.null) (Map.map txdTxsToRequest txDecisions)
      ===
      Map.filter (not . Set.null) (Map.map txdTxsToRequest txDecisionsA)
    )

  where
    txidSeq = StrictSeq.fromList (getTxId <$> txs)
    txidMap = Map.fromList [ (getTxId tx, getTxSize tx) | tx <- txs ]

    st' = TXS.receivedTxIdsImpl
            (apply mempoolHasTx)
            peeraddr
            (fromIntegral $ StrictSeq.length txidSeq)
            txidSeq txidMap
            st

    (st'', txDecisions) = TXS.makeDecisions
                            policy ctx { sdcSharedTxState = st' }
                            (filterActivePeers policy st')


    (stA', txDecisionsA) = TXS.makeDecisions
                             policy ctx
                             (filterActivePeers policy st)

    stA'' = TXS.receivedTxIdsImpl
              (apply mempoolHasTx)
              peeraddr
              (fromIntegral $ StrictSeq.length txidSeq)
              txidSeq txidMap
              stA'


data ArbDecisionContextWithCollectTxs = ArbDecisionContextWithCollectTxs {
    adccDecisionPolicy :: TxDecisionPolicy,
    adccSharedContext  :: SharedDecisionContext PeerAddr TxId (Tx TxId),
    adccMempoolHasTx   :: Fun TxId Bool,
    adccRequestedTxIds :: Set TxId,
    adccReceivedTxs    :: Map TxId (Tx TxId),
    adccPeer           :: PeerAddr
  }
  deriving Show

instance Arbitrary ArbDecisionContextWithCollectTxs where
    arbitrary = do
      ArbTxDecisionPolicy policy <- arbitrary
      ArbCollectTxs mempoolHasTx
                    requestedTxIds
                    receivedTxs
                    peeraddr
                    _
                    st
        <- arbitrary

      let st' = fixupSharedTxStateForPolicy
                  (apply mempoolHasTx)
                  policy st
          ps' =  peerTxStates st' Map.! peeraddr
          peers = Map.keys (peerTxStates st')

          requestedTxIds' = requestedTxIds
                            `Set.intersection`
                            requestedTxsInflight ps'
          receivedTxs'    = receivedTxs
                            `Map.restrictKeys`
                            requestedTxIds'

      gsvs <- zip peers
          <$> infiniteListOf (unPeerGSVT <$> arbitrary)

      return $ ArbDecisionContextWithCollectTxs {
          adccDecisionPolicy = policy,
          adccSharedContext  = SharedDecisionContext {
              sdcPeerGSV       = Map.fromList gsvs,
              sdcSharedTxState = st'
            },
          adccMempoolHasTx   = mempoolHasTx,
          adccRequestedTxIds = requestedTxIds',
          adccReceivedTxs    = receivedTxs',
          adccPeer           = peeraddr
        }

    shrink ctx@ArbDecisionContextWithCollectTxs {
                adccDecisionPolicy = decisionPolicy,
                adccSharedContext  = sharedCtx,
                adccRequestedTxIds = requestedTxIds,
                adccReceivedTxs    = receivedTxs,
                adccMempoolHasTx   = mempoolHasTx,
                adccPeer           = peer
             } =
      [ ctx { adccDecisionPolicy = decisionPolicy
            , adccSharedContext  = sharedCtx { sdcSharedTxState = st }
            , adccRequestedTxIds = requestedTxIds'
            , adccReceivedTxs    = receivedTxs'
            }
      | ArbTxDecisionPolicy decisionPolicy' <- shrink (ArbTxDecisionPolicy decisionPolicy)
      , let st = fixupSharedTxStateForPolicy
                   (apply mempoolHasTx)
                   decisionPolicy'
                   (sdcSharedTxState sharedCtx)
            ps = peerTxStates st Map.! peer
            requestedTxIds' = requestedTxIds
                              `Set.intersection`
                              requestedTxsInflight ps
            receivedTxs'    = receivedTxs
                              `Map.restrictKeys`
                              requestedTxIds'
      , st /= sdcSharedTxState sharedCtx
      ]
      ++
      [ ctx { adccDecisionPolicy = policy
            , adccSharedContext  = sharedCtx' { sdcSharedTxState = st }
            , adccRequestedTxIds = requestedTxIds'
            , adccReceivedTxs    = receivedTxs'
            , adccMempoolHasTx   = mempoolHasTx'
            }
      | ArbDecisionContexts {
          arbDecisionPolicy = policy,
          arbSharedContext  = sharedCtx'@SharedDecisionContext { sdcSharedTxState = st },
          arbMempoolHasTx   = mempoolHasTx'
        }
          <- shrink ArbDecisionContexts {
                 arbDecisionPolicy = decisionPolicy,
                 arbSharedContext  = sharedCtx,
                 arbMempoolHasTx   = mempoolHasTx
             }
      , Just ps <- [peer `Map.lookup` peerTxStates st]
      , let requestedTxIds' = requestedTxIds
                              `Set.intersection`
                              requestedTxsInflight ps
            receivedTxs'    = receivedTxs
                              `Map.restrictKeys`
                              requestedTxIds'
      ]
      ++
      [ ctx { adccRequestedTxIds = requestedTxIds'
            , adccReceivedTxs    = receivedTxs'
            }
      | -- Don't shrink `TxId`s.
        -- NOTE: if `TxId` would be a newtype an arbitrary instance would not
        -- be provided and thus `shrink requestedTxIds` would not compile.
        requestedTxIds' <- map Set.fromList $ shrinkList (\_ -> []) (Set.toList requestedTxIds)
      , let receivedTxs' = receivedTxs
                           `Map.restrictKeys`
                           requestedTxIds'
      ]

prop_makeDecisions_collectTxs
  :: ArbDecisionContextWithCollectTxs
  -> Property
prop_makeDecisions_collectTxs
    ArbDecisionContextWithCollectTxs {
      adccDecisionPolicy = policy,
      adccSharedContext  = ctx@SharedDecisionContext { sdcSharedTxState = st },
      adccRequestedTxIds = requestedTxIds,
      adccReceivedTxs    = receivedTxs,
      adccPeer           = peeraddr
    }
    =
    counterexample ("st'   = " ++ show st') $
    counterexample ("st''  = " ++ show st'') $
    counterexample ("stA'  = " ++ show stA') $
    counterexample ("stA'' = " ++ show stA'') $
    counterexample ("txDecisions  = " ++ show txDecisions) $
    counterexample ("txDecisionsA = " ++ show txDecisionsA) $

    counterexample "acknowledged txids property failure" (
      -- remove all decisions which do not acknowledge any txids; adjust number
      -- of acknowledged txids by peeraddr.
      let m  = Map.filter (/= 0)
             . Map.map txdTxIdsToAcknowledge
             $ txDecisions
          mA = Map.filter (/= 0)
             . Map.map txdTxIdsToAcknowledge
             $ txDecisionsA
      in
         and
         -- when we first collect then make a decision, we might acknowledged
         -- more txids; e.g. if we received a `tx`, then it can be acknowledged
         -- for all peers while `collectTxIds` only acknowledges it for one
         -- peer - this is reflected by the following merge function.
       $ Map.merge
         (Map.mapMaybeMissing \_ _ -> Just True)
         (Map.mapMaybeMissing \_ _ -> Just False)
         (Map.zipWithMaybeMatched \_ x y -> Just $ x >= y)
         m mA
    )

    .&&.

    (    counterexample "txs to be added to the mempool"
       . counterexample (show txs)
       . counterexample (show txsA)
       $ txsA
         `Set.isSubsetOf`
         txs
    )

  where
    st'                        = TXS.collectTxsImpl
                                   peeraddr requestedTxIds
                                   receivedTxs st
    (st'', txDecisions)        = TXS.makeDecisions
                                   policy
                                   ctx { sdcSharedTxState = st' }
                                   (filterActivePeers policy st')
    txs                        = foldMap (Set.fromList . txdTxsToMempool) txDecisions

    (stA', txDecisionsA)       = TXS.makeDecisions
                                   policy ctx
                                   (filterActivePeers policy st)
    txsA                       = foldMap (Set.fromList . txdTxsToMempool) txDecisionsA
    stA''                      = TXS.collectTxsImpl
                                   peeraddr requestedTxIds
                                   receivedTxs stA'


-- | `filterActivePeers` should not change decisions made by `makeDecisions`
--
--
-- This test checks the following properties:
--
-- In what follows, the set of active peers is defined as the keys of the map
-- returned by `filterActivePeers`.
--
-- 1. The set of active peers is a superset of peers for which a decision was
--    made;
-- 2. The set of active peer which can acknowledge txids is a subset of peers
--    for which a decision was made;
-- 3. Decisions made from the results of `filterActivePeers` is the same as from
--    the original set.
--
-- Ad 2. a stronger property is not possible. There can be a peer for which
-- a decision was not taken but which is an active peer.
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

    counterexample ("found non-active peers for which decision can be made: "
                     ++ show (decisionPeers  Set.\\ activePeers)
                   )
                   (decisionPeers  `Set.isSubsetOf` activePeers)
    .&&.
    counterexample ("found an active peer which can acknowledge txids "
                     ++ "for which decision was not made: "
                     ++ show (activePeersAck Set.\\ decisionPeers))
                   (activePeersAck `Set.isSubsetOf` decisionPeers)
    .&&.
    counterexample "decisions from active peers are not equal to decisions from all peers"
                   (decisions === decisionsOfActivePeers)
  where
    activePeersMap    = TXS.filterActivePeers policy st
    activePeers       = Map.keysSet activePeersMap
    -- peers which are active & can acknowledge txids
    activePeersAck    = activePeers
                        `Set.intersection`
                        Map.keysSet (Map.filter (TXS.hasTxIdsToAcknowledge st) (peerTxStates st))
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
