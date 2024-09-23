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

module Test.Ouroboros.Network.TxSubmission.Types where

import Prelude hiding (seq)

import NoThunks.Class

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
import Control.Tracer (Tracer (..), showTracing, traceWith)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Data.ByteString.Lazy (ByteString)
import Data.Foldable as Foldable (find, foldl', toList)
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import GHC.Generics (Generic)

import Network.TypedProtocol.Codec

import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.Util.ShowProxy

import Test.QuickCheck
import Text.Printf


data Tx txid = Tx {
    getTxId      :: !txid,
    getTxSize    :: !SizeInBytes,
    getTxAdvSize :: !SizeInBytes,
    -- | If false this means that when this tx will be submitted to a remote
    -- mempool it will not be valid.  The outbound mempool might contain
    -- invalid tx's in this sense.
    getTxValid   :: !Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks txid => NoThunks (Tx txid)
instance ShowProxy txid => ShowProxy (Tx txid) where
    showProxy _ = "Tx " ++ showProxy (Proxy :: Proxy txid)

instance Arbitrary txid => Arbitrary (Tx txid) where
    arbitrary = do
      -- note:
      -- generating small tx sizes avoids overflow error when semigroup
      -- instance of `SizeInBytes` is used (summing up all inflight tx
      -- sizes).
      (size, advSize) <- frequency [ (9, (\a -> (a,a)) <$> chooseEnum (0, maxTxSize))
                                   , (1, (,) <$> chooseEnum (0, maxTxSize) <*> chooseEnum (0, maxTxSize))
                                   ]
      Tx <$> arbitrary
         <*> pure size
         <*> pure advSize
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
    encodeTx Tx {getTxId, getTxSize, getTxAdvSize, getTxValid} =
         CBOR.encodeListLen 4
      <> CBOR.encodeInt getTxId
      <> CBOR.encodeWord32 (getSizeInBytes getTxSize)
      <> CBOR.encodeWord32 (getSizeInBytes getTxAdvSize)
      <> CBOR.encodeBool getTxValid

    decodeTx = do
      _ <- CBOR.decodeListLen
      Tx <$> CBOR.decodeInt
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> CBOR.decodeBool


newtype LargeNonEmptyList a = LargeNonEmpty { getLargeNonEmpty :: [a] }
  deriving Show

instance Arbitrary a => Arbitrary (LargeNonEmptyList a) where
    arbitrary =
      LargeNonEmpty <$> suchThat (resize 500 (listOf arbitrary)) ((>25) . length)


-- TODO: Belongs in iosim.
data SimResults a = SimReturn a [String]
                  | SimException SomeException [String]
                  | SimDeadLock [String]

-- Traverses a list of trace events and returns the result along with all log messages.
-- Incase of a pure exception, ie an assert, all tracers evaluated so far are returned.
evaluateTrace :: SimTrace a -> IO (SimResults a)
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

debugTracer :: forall a s. Show a => Tracer (IOSim s) a
debugTracer = threadAndTimeTracer $ showTracing $ Tracer (traceM . show)

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
