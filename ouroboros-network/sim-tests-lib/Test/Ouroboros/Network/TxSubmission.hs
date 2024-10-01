{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Ouroboros.Network.TxSubmission (tests) where

import Prelude hiding (seq)

import NoThunks.Class (NoThunks)

import Control.Concurrent.Class.MonadSTM
import Control.Exception (SomeException (..))
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
import Data.Foldable as Foldable (find, foldl', toList)
import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
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
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Testing.Utils

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf


tests :: TestTree
tests = testGroup "TxSubmission"
  [ testProperty "txSubmission" prop_txSubmission
  , testProperty "x" prop_x
  ]


data Tx txid = Tx {
    getTxId    :: txid,
    getTxSize  :: !SizeInBytes,
    -- | If false this means that when this tx will be submitted to a remote
    -- mempool it will not be valid.  The outbound mempool might contain
    -- invalid tx's in this sense.
    getTxValid :: Bool
  }
  deriving (Eq, Show, Generic)

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


newtype Mempool m txid = Mempool (TVar m (Seq (Tx txid)))


emptyMempool :: MonadSTM m => m (Mempool m txid)
emptyMempool = Mempool <$> newTVarIO Seq.empty

newMempool :: MonadSTM m
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
                         $ txs
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
     , MonadTimer m
     , MonadThrow (STM m)
     , Ord txid
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

prop_x :: Property
prop_x = prop_txSubmission
  Positive {getPositive = 3}
  NonEmpty {getNonEmpty = [Tx {getTxId = -83, getTxSize = SizeInBytes 62352, getTxValid = True},Tx {getTxId = 66, getTxSize = SizeInBytes 37084, getTxValid = True},Tx {getTxId = 55, getTxSize = SizeInBytes 54825, getTxValid = False},Tx {getTxId = -94, getTxSize = SizeInBytes 54298, getTxValid = True},Tx {getTxId = -83, getTxSize = SizeInBytes 30932, getTxValid = True},Tx {getTxId = 33, getTxSize = SizeInBytes 40377, getTxValid = True},Tx {getTxId = 87, getTxSize = SizeInBytes 42883, getTxValid = False},Tx {getTxId = -87, getTxSize = SizeInBytes 21529, getTxValid = True},Tx {getTxId = 85, getTxSize = SizeInBytes 15222, getTxValid = True},Tx {getTxId = -13, getTxSize = SizeInBytes 529, getTxValid = True},Tx {getTxId = -21, getTxSize = SizeInBytes 14755, getTxValid = True},Tx {getTxId = 37, getTxSize = SizeInBytes 3921, getTxValid = True},Tx {getTxId = -44, getTxSize = SizeInBytes 42390, getTxValid = True},Tx {getTxId = 47, getTxSize = SizeInBytes 27061, getTxValid = False},Tx {getTxId = 64, getTxSize = SizeInBytes 8540, getTxValid = True},Tx {getTxId = -85, getTxSize = SizeInBytes 15138, getTxValid = False},Tx {getTxId = -23, getTxSize = SizeInBytes 16317, getTxValid = False},Tx {getTxId = -35, getTxSize = SizeInBytes 4372, getTxValid = True},Tx {getTxId = -11, getTxSize = SizeInBytes 13524, getTxValid = True},Tx {getTxId = 98, getTxSize = SizeInBytes 62024, getTxValid = True},Tx {getTxId = -42, getTxSize = SizeInBytes 63227, getTxValid = False},Tx {getTxId = 74, getTxSize = SizeInBytes 31476, getTxValid = True},Tx {getTxId = 72, getTxSize = SizeInBytes 42959, getTxValid = True},Tx {getTxId = 72, getTxSize = SizeInBytes 53084, getTxValid = True},Tx {getTxId = 6, getTxSize = SizeInBytes 5013, getTxValid = True},Tx {getTxId = -62, getTxSize = SizeInBytes 52590, getTxValid = True},Tx {getTxId = -18, getTxSize = SizeInBytes 59325, getTxValid = False},Tx {getTxId = 70, getTxSize = SizeInBytes 40956, getTxValid = True},Tx {getTxId = -82, getTxSize = SizeInBytes 33213, getTxValid = True},Tx {getTxId = -73, getTxSize = SizeInBytes 31026, getTxValid = True},Tx {getTxId = -4, getTxSize = SizeInBytes 19421, getTxValid = True},Tx {getTxId = 68, getTxSize = SizeInBytes 37501, getTxValid = False},Tx {getTxId = 47, getTxSize = SizeInBytes 25707, getTxValid = False},Tx {getTxId = -99, getTxSize = SizeInBytes 58538, getTxValid = False},Tx {getTxId = 86, getTxSize = SizeInBytes 63432, getTxValid = False},Tx {getTxId = -73, getTxSize = SizeInBytes 32185, getTxValid = True},Tx {getTxId = 52, getTxSize = SizeInBytes 55174, getTxValid = False},Tx {getTxId = 52, getTxSize = SizeInBytes 20715, getTxValid = False},Tx {getTxId = -21, getTxSize = SizeInBytes 37063, getTxValid = False},Tx {getTxId = 15, getTxSize = SizeInBytes 63172, getTxValid = True},Tx {getTxId = -26, getTxSize = SizeInBytes 51314, getTxValid = True},Tx {getTxId = 19, getTxSize = SizeInBytes 5042, getTxValid = True},Tx {getTxId = 36, getTxSize = SizeInBytes 40532, getTxValid = True},Tx {getTxId = -30, getTxSize = SizeInBytes 18812, getTxValid = True},Tx {getTxId = 22, getTxSize = SizeInBytes 61634, getTxValid = True},Tx {getTxId = 89, getTxSize = SizeInBytes 44309, getTxValid = True},Tx {getTxId = -98, getTxSize = SizeInBytes 61700, getTxValid = True},Tx {getTxId = -17, getTxSize = SizeInBytes 46606, getTxValid = True},Tx {getTxId = -37, getTxSize = SizeInBytes 25004, getTxValid = False},Tx {getTxId = -53, getTxSize = SizeInBytes 51991, getTxValid = False},Tx {getTxId = -88, getTxSize = SizeInBytes 17941, getTxValid = True},Tx {getTxId = 24, getTxSize = SizeInBytes 19866, getTxValid = True},Tx {getTxId = -99, getTxSize = SizeInBytes 52082, getTxValid = True},Tx {getTxId = 50, getTxSize = SizeInBytes 48715, getTxValid = True},Tx {getTxId = -8, getTxSize = SizeInBytes 24522, getTxValid = True},Tx {getTxId = 92, getTxSize = SizeInBytes 53516, getTxValid = True},Tx {getTxId = 59, getTxSize = SizeInBytes 16151, getTxValid = False},Tx {getTxId = -85, getTxSize = SizeInBytes 57386, getTxValid = True},Tx {getTxId = 23, getTxSize = SizeInBytes 36444, getTxValid = False},Tx {getTxId = -59, getTxSize = SizeInBytes 63727, getTxValid = False},Tx {getTxId = -59, getTxSize = SizeInBytes 12656, getTxValid = True},Tx {getTxId = 13, getTxSize = SizeInBytes 19160, getTxValid = False},Tx {getTxId = -35, getTxSize = SizeInBytes 1681, getTxValid = True},Tx {getTxId = -13, getTxSize = SizeInBytes 46705, getTxValid = False}]}
  (Just (Positive {getPositive = SmallDelay {getSmallDelay = 4.3}}))

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
                       , MonadSay m
                       , MonadMonotonicTime m
                       , Show a
                       )
               => Tracer m a
verboseTracer = threadAndTimeTracer $ showTracing $ Tracer say

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadMonotonicTime m
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getMonotonicTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now (show tid) s
