{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.TxSubmission.Types
  ( Tx (..)
  , TxId
  , Mempool
  , emptyMempool
  , newMempool
  , readMempool
  , getMempoolReader
  , getMempoolWriter
  , InvalidTx (..)
  , maxTxSize
  , LargeNonEmptyList (..)
  , SimResults (..)
  , WithThreadAndTime (..)
  , txSubmissionCodec2
  , evaluateTrace
  , verboseTracer
  ) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.Strict qualified as StrictSTM
import Control.DeepSeq
import Control.Exception (SomeException (..))
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim hiding (SimResult)
import Control.Tracer (Tracer (..), traceWith)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Data.ByteString.Lazy (ByteString)
import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Network.TypedProtocol.Codec

import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V1
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool)
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool
import Ouroboros.Network.Util.ShowProxy

import Test.Ouroboros.Network.Utils (sayTracer)
import Test.QuickCheck
import Text.Printf


data Tx txid = Tx {
    getTxId      :: !txid,
    getTxSize    :: !SizeInBytes,
    getTxAdvSize :: !SizeInBytes,
    -- | If false this means that when this tx will be submitted to a remote
    -- mempool it will not be valid.  The outbound mempool might contain
    -- invalid tx's in this sense.
    getTxValid   :: !Bool,
    -- | Optional parent dependency: this tx may only be accepted into a
    -- mempool once its parent is already present (either from an earlier
    -- batch or earlier in the same batch).  'Nothing' means no dependency.
    -- Used by chain-aware tests to model transaction chains.
    getTxParent  :: !(Maybe txid)
  }
  deriving (Eq, Ord, Show, Generic, NFData)

instance NoThunks txid => NoThunks (Tx txid)
instance ShowProxy txid => ShowProxy (Tx txid) where
    showProxy _ = "Tx " ++ showProxy (Proxy :: Proxy txid)

instance Arbitrary txid => Arbitrary (Tx txid) where
    arbitrary = do
      -- note:
      -- generating small tx sizes avoids overflow error when semigroup
      -- instance of `SizeInBytes` is used (summing up all inflight tx
      -- sizes).
      (size, advSize) <- frequency [ (99, (\a -> (a,a)) <$> chooseEnum (0, maxTxSize))
                                   , (1, (,) <$> chooseEnum (0, maxTxSize) <*> chooseEnum (0, maxTxSize))
                                   ]
      Tx <$> arbitrary
         <*> pure size
         <*> pure advSize
         <*> frequency [ (3, pure True)
                       , (1, pure False)
                       ]
         <*> pure Nothing
           -- ^ Generic Arbitrary produces standalone txs with no parent.
           -- Chain-aware generators construct parents explicitly.

-- maximal tx size
maxTxSize :: SizeInBytes
maxTxSize = 65536

type TxId = Int

emptyMempool :: MonadSTM m => m (Mempool m txid (Tx txid))
emptyMempool = Mempool.empty

newMempool :: (MonadSTM m, Ord txid)
           => [Tx txid] -> m (Mempool m txid (Tx txid))
newMempool = Mempool.new getTxId

readMempool :: MonadSTM m => Mempool m txid (Tx txid) -> m [Tx txid]
readMempool  = Mempool.read

getMempoolReader :: forall txid m.
                    ( MonadSTM m
                    , Ord txid
                    , Show txid
                    )
                 => Mempool m txid (Tx txid)
                 -> TxSubmissionMempoolReader txid (Tx txid) Integer m
getMempoolReader = Mempool.getReader getTxId getTxAdvSize


data InvalidTx = InvalidTx | DuplicateTx | MissingParent
  deriving (Eq, Show)

getMempoolWriter :: forall txid m.
                    ( MonadSTM m
                    , MonadTime m
                    , MonadThrow m
                    , Ord txid
                    , Eq txid
                    , Typeable txid
                    , Show txid
                    )
                 => TVar m [txid]
                 -> Mempool m txid (Tx txid)
                 -> TxSubmissionMempoolWriter txid (Tx txid) Integer m InvalidTx
getMempoolWriter duplicateVar (Mempool.Mempool mempoolVar) =
  TxSubmissionMempoolWriter {
      txId = getTxId,
      mempoolAddTxs = \txs -> do
        (acceptedTxs, rejectedTxs, duplicateValidTxIds) <- atomically $ do
          Mempool.MempoolSeq { Mempool.mempoolSet, Mempool.mempoolSeq, Mempool.nextIdx } <-
            StrictSTM.readTVar mempoolVar

          let (duplicateTxs, txsToValidate) =
                List.partition (\tx -> getTxId tx `Set.member` mempoolSet) txs
              duplicateRejectedTxs =
                [ (getTxId tx, DuplicateTx)
                | tx <- duplicateTxs
                ]
              duplicateValidTxIds =
                [ getTxId tx
                | tx <- duplicateTxs
                , getTxValid tx
                ]
              (invalidRejectedTxs, validTxs) =
                partitionEithers
                  [ if getTxValid tx
                      then Right tx
                      else Left (getTxId tx, InvalidTx)
                  | tx <- txsToValidate
                  ]

              (delta, mempoolSeq', nextIdx', acceptedTxs, duplicateValidTxIds', missingParentIds) =
                List.foldl'
                  (\(set, seq, idx, accepted, duplicates, missing) tx ->
                    let txid = getTxId tx in
                    if txid `Set.member` set
                      then ( set
                           , seq
                           , idx
                           , accepted
                           , txid : duplicates
                           , missing
                           )
                      else case getTxParent tx of
                        Just p
                          | not (p `Set.member` set)
                            && not (p `Set.member` mempoolSet) ->
                              -- Parent is not in the mempool and has not
                              -- been accepted earlier in this batch.
                              ( set
                              , seq
                              , idx
                              , accepted
                              , duplicates
                              , txid : missing
                              )
                        _ ->
                          ( Set.insert txid set
                          , seq Seq.|> Mempool.WithIndex idx tx
                          , succ idx
                          , txid : accepted
                          , duplicates
                          , missing
                          )
                  )
                  (Set.empty, mempoolSeq, nextIdx, [], [], [])
                  validTxs

          StrictSTM.writeTVar
            mempoolVar
            Mempool.MempoolSeq {
              Mempool.mempoolSet = mempoolSet `Set.union` delta,
              Mempool.mempoolSeq = mempoolSeq',
              Mempool.nextIdx = nextIdx'
            }

          pure
            ( acceptedTxs
            , invalidRejectedTxs
                ++ duplicateRejectedTxs
                ++ [ (txid, DuplicateTx)   | txid <- duplicateValidTxIds' ]
                ++ [ (txid, MissingParent) | txid <- missingParentIds ]
            , duplicateValidTxIds ++ duplicateValidTxIds'
            )

        atomically $ modifyTVar' duplicateVar (duplicateValidTxIds <>)
        pure (acceptedTxs, rejectedTxs)
    }


txSubmissionCodec2 :: MonadST m
                   => Codec (TxSubmission2 Int (Tx Int))
                            CBOR.DeserialiseFailure m ByteString
txSubmissionCodec2 =
    codecTxSubmission2 CBOR.encodeInt CBOR.decodeInt
                       encodeTx decodeTx
  where
    encodeTx Tx {getTxId, getTxSize, getTxAdvSize, getTxValid, getTxParent} =
         CBOR.encodeListLen 5
      <> CBOR.encodeInt getTxId
      <> CBOR.encodeWord32 (getSizeInBytes getTxSize)
      <> CBOR.encodeWord32 (getSizeInBytes getTxAdvSize)
      <> CBOR.encodeBool getTxValid
      <> encodeMaybeInt getTxParent

    decodeTx = do
      _ <- CBOR.decodeListLen
      Tx <$> CBOR.decodeInt
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> (SizeInBytes <$> CBOR.decodeWord32)
         <*> CBOR.decodeBool
         <*> decodeMaybeInt

    encodeMaybeInt Nothing  = CBOR.encodeListLen 0
    encodeMaybeInt (Just i) = CBOR.encodeListLen 1 <> CBOR.encodeInt i

    decodeMaybeInt = do
      n <- CBOR.decodeListLen
      case n of
        0 -> pure Nothing
        1 -> Just <$> CBOR.decodeInt
        _ -> fail "decodeMaybeInt: unexpected list length"


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
verboseTracer = threadAndTimeTracer sayTracer

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
