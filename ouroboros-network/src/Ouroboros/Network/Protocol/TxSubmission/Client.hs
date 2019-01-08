{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.TxSubmission.Client where

import Protocol.Core
import Ouroboros.Network.Protocol.TxSubmission.Type

import Data.List.NonEmpty (NonEmpty (..))
import Pipes (Producer)
import qualified Pipes

newtype TxSubmissionClient tx err m a = TxSubmissionClient {
    runTxSubmissionClient :: m (TxSubmission tx err m a)
  }

data TxSubmission tx err m a where
  TxSubmission
    :: tx
    -> TxSubmissionClient tx err m a
    -> TxSubmission tx err m a
  TxSubmissionDone
    :: TxSubmissionHandler err m a
    -> TxSubmission tx err m a

newtype TxSubmissionHandler err m a = TxSubmissionHandler {
    runTxSubmissionHandler :: Maybe err -> m a
  }

txSubmissionClientFromList
  :: Applicative m
  => NonEmpty tx
  -> TxSubmissionHandler err m a
  -> TxSubmissionClient tx err m a
txSubmissionClientFromList (tx :| []) txHandler
  = TxSubmissionClient $ pure $ TxSubmission tx (TxSubmissionClient $ pure $ TxSubmissionDone txHandler)
txSubmissionClientFromList (tx :| (tx' : txs')) txHandler
  = TxSubmissionClient $ pure $ TxSubmission tx (txSubmissionClientFromList (tx' :| txs') txHandler)

txSubmissionClientFromProducer
  :: Monad m
  => Producer tx m ()
  -> TxSubmissionHandler err m a
  -> TxSubmissionClient tx err m a
txSubmissionClientFromProducer producer txHandler = TxSubmissionClient $
  Pipes.next producer >>= \nxt -> case nxt of
    Left _                -> pure $ TxSubmissionDone txHandler
    Right (tx, producer') -> pure $ TxSubmission tx (txSubmissionClientFromProducer producer' txHandler)

txSubmissionClientStream
  :: Monad m
  => TxSubmissionClient tx err m a
  -> Peer TxSubmissionProtocol (TxSubmissionMessage tx err)
    (Yielding StIdle)
    (Finished StDone)
    m a
txSubmissionClientStream (TxSubmissionClient submit) = lift $ submit >>= \cli -> case cli of
  TxSubmission tx scli       ->
    -- recursievly send all transactions
    pure $ part (MsgTx tx) (txSubmissionClientStream scli)
  TxSubmissionDone txHandler ->
    -- client sent all transactions
    pure $ over MsgClientDone $
    -- await for server response
    await $ \(MsgServerDone merr) ->
    -- end the protocol
    lift $ done <$> runTxSubmissionHandler txHandler merr
