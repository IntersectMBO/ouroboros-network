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
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, nubBy, stripPrefix)
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
import Ouroboros.Network.TxSubmission.Inbound.State (PeerTxState (..),
           SharedTxState (..))
import Ouroboros.Network.TxSubmission.Inbound.State qualified as TXS
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Testing.Utils

import Test.QuickCheck
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Monoids (All (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf


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
    , testGroup "NoThunks"
      [ testProperty "receivedTxIdsImpl"        prop_receivedTxIdsImpl_nothunks
      , testProperty "collectTxsImpl"           prop_collectTxsImpl_nothunks
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
  => NumTxIdsToAck
  -> [Tx txid]
  -> ControlMessageSTM m
  -> Maybe DiffTime
  -> Maybe DiffTime
  -> m ([Tx txid], [Tx txid])
txSubmissionSimulation maxUnacked outboundTxs
                       controlMessageSTM
                       inboundDelay outboundDelay = do

    inboundMempool  <- emptyMempool
    outboundMempool <- newMempool outboundTxs
    (outboundChannel, inboundChannel) <- createConnectedChannels
    outboundAsync <-
      async $ runPeerWithLimits
                (("OUTBOUND",) `contramap` verboseTracer)
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
