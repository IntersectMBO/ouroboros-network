{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Ouroboros.Network.TxSubmission (tests) where

import           Prelude hiding (seq)

import           NoThunks.Class (NoThunks)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (nullTracer, contramap)

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.List (nubBy)
import           Data.Foldable (toList, find, foldl')
import           Data.Function (on)
import           Data.Maybe (isJust, fromMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Word (Word16)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Util.ShowProxy
import           Ouroboros.Network.Protocol.TxSubmission.Type
import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.TxSubmission.Mempool.Reader
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion (..))

import           Ouroboros.Network.Testing.Utils

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck


tests :: TestTree
tests = testGroup "TxSubmission"
  [ testProperty "txSubmission" prop_txSubmission
  ]


data Tx txid = Tx {
    getTxId    :: txid,
    getTxSize  :: TxSizeInBytes,
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
         <*> arbitrary
         <*> frequency [ (3, pure True)
                       , (1, pure False)
                       ]

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
           . nubBy (on (==) getTxId)

readMempool :: MonadSTM m => Mempool m txid -> m [Tx txid]
readMempool (Mempool mempool) = toList <$> atomically (readTVar mempool)


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

    f :: Int -> Tx txid -> (txid, Int, TxSizeInBytes)
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
            let currentIds = Set.fromList (map getTxId (toList txs))
                validTxs = nubBy (on (==) getTxId)
                         $ filter
                             (\Tx { getTxId, getTxValid } ->
                                  getTxValid
                               && getTxId `Set.notMember` currentIds)
                         $ txs
                mempoolTxs' = foldl' (Seq.|>) mempoolTxs validTxs
            writeTVar mempool mempoolTxs'
            return (map getTxId validTxs)
      }


txSubmissionCodec :: MonadST m
                  => Codec (TxSubmission Int (Tx Int))
                           CBOR.DeserialiseFailure m ByteString
txSubmissionCodec =
    codecTxSubmission CBOR.encodeInt CBOR.decodeInt
                      encodeTx decodeTx
  where
    encodeTx Tx {getTxId, getTxSize, getTxValid} =
         CBOR.encodeListLen 3
      <> CBOR.encodeInt getTxId
      <> CBOR.encodeWord32 getTxSize
      <> CBOR.encodeBool getTxValid

    decodeTx = do
      _ <- CBOR.decodeListLen
      Tx <$> CBOR.decodeInt
         <*> CBOR.decodeWord32
         <*> CBOR.decodeBool


txSubmissionSimulation
  :: forall m txid.
     ( MonadAsync m
     , MonadFork  m
     , MonadMask  m
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
  => Word16
  -> [Tx txid]
  -> ControlMessageSTM m
  -> Maybe DiffTime
  -> Maybe DiffTime
  -> m [Tx txid]
txSubmissionSimulation maxUnacked outboundTxs
                       controlMessageSTM
                       inboundDelay outboundDelay = do

    inboundMempool  <- emptyMempool
    outboundMempool <- newMempool outboundTxs
    (outboundChannel, inboundChannel) <- createConnectedChannels
    outboundAsync <-
      async $ runPeerWithLimits
                (("OUTBOUND",) `contramap` nullTracer)
                txSubmissionCodec
                (byteLimitsTxSubmission (fromIntegral . BSL.length))
                timeLimitsTxSubmission
                (fromMaybe id (delayChannel <$> outboundDelay) outboundChannel)
                (txSubmissionClientPeer (outboundPeer outboundMempool))

    inboundAsync <-
      async $ runPipelinedPeerWithLimits
                (("INBOUND",) `contramap` nullTracer)
                txSubmissionCodec
                (byteLimitsTxSubmission (fromIntegral . BSL.length))
                timeLimitsTxSubmission
                (fromMaybe id (delayChannel <$> inboundDelay) inboundChannel)
                (txSubmissionServerPeerPipelined (inboundPeer inboundMempool))

    _ <- waitAnyCancel [ outboundAsync, inboundAsync ]

    readMempool inboundMempool
  where

    outboundPeer :: Mempool m txid -> TxSubmissionClient txid (Tx txid) m ()
    outboundPeer outboundMempool =
      txSubmissionOutbound
        nullTracer
        maxUnacked
        (getMempoolReader outboundMempool)
        NodeToNodeV_3
        controlMessageSTM

    inboundPeer :: Mempool m txid -> TxSubmissionServerPipelined txid (Tx txid) m ()
    inboundPeer inboundMempool =
      txSubmissionInbound
        nullTracer
        maxUnacked
        (getMempoolReader inboundMempool)
        (getMempoolWriter inboundMempool)
        NodeToNodeV_3


newtype LargeNonEmptyList a = LargeNonEmpty { getLargeNonEmpty :: [a] }
  deriving Show

instance Arbitrary a => Arbitrary (LargeNonEmptyList a) where
    arbitrary =
      LargeNonEmpty <$> suchThat (resize 500 (listOf arbitrary)) ((>25) . length)



prop_txSubmission :: Positive Word16
                  -> NonEmptyList (Tx Int)
                  -> Maybe Delay
                  -> Bool
prop_txSubmission (Positive maxUnacked) (NonEmpty outboundTxs) delay =
    let mbDelayTime = getDelay <$> delay
        inboundTxs =
          runSimOrThrow $ do
            controlMessageVar <- newTVarIO Continue
            _ <-
              async $ do
                threadDelay
                  (fromMaybe 1 mbDelayTime
                    * (realToFrac (length outboundTxs `div` 4)))
                atomically (writeTVar controlMessageVar Terminate)
            txSubmissionSimulation
              maxUnacked outboundTxs
              (readTVar controlMessageVar)
              mbDelayTime mbDelayTime
              
    in inboundTxs == take (length inboundTxs) outboundTxs
