{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.TxSubmission.Server where

import Data.Functor (($>))

import Protocol.Core
import Ouroboros.Network.Protocol.TxSubmission.Type

newtype TxSubmissionServer tx err m a = TxSubmissionServer {
    runTxSubmissionServer :: m (TxSubmissionHandlers tx err m a)
  }

data TxSubmissionHandlers tx err m a = TxSubmissionHandlers {
    handleTx     :: tx -> m (TxSubmissionHandlers tx err m a),
    handleTxDone :: m (Either err a)
  }

txSubmissionServer
  :: forall tx err m a.
     Monad m
  => (tx -> m ())    -- valid tx handler
  -> (tx -> m Bool)  -- validate a transaction
  -> ([tx] -> m err) -- error handler
  -> a
  -> TxSubmissionServer tx err m a
txSubmissionServer handleTx validateTx handleErr result = TxSubmissionServer $ pure (go [])
 where
  go :: [tx] -- malformed transactions
     -> TxSubmissionHandlers tx err m a
  go errs = TxSubmissionHandlers {
      handleTx = \tx -> validateTx tx >>= \txValid -> if txValid
                          then handleTx tx $> go errs
                          else pure $ go (tx : errs),
      handleTxDone = case errs of
        [] -> pure (Right result)
        _  -> Left <$> handleErr errs
    }

txSubmissionServerStream
  :: forall tx err m a.
     Monad m
  => TxSubmissionServer tx err m a
  -> Peer TxSubmissionProtocol (TxSubmissionMessage tx err)
      (Awaiting StIdle)
      (Finished StDone)
      m (Maybe a)
txSubmissionServerStream (TxSubmissionServer server) = lift $ go <$> server
 where
  go :: TxSubmissionHandlers tx err m a
     -> Peer TxSubmissionProtocol (TxSubmissionMessage tx err)
          (Awaiting StIdle)
          (Finished StDone)
          m (Maybe a)
  go TxSubmissionHandlers {handleTx, handleTxDone} =
    await $ \msg -> case msg of
      MsgTx tx      -> lift $ go <$> handleTx tx
      MsgClientDone -> lift $ handleTxDone >>= \r -> case r of
        Right a  -> pure $ out (MsgServerDone Nothing) (done $ Just a)
        Left err -> pure $ out (MsgServerDone (Just err)) (done Nothing)
