{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeInType          #-}
module Ouroboros.Network.Protocol.TxSubmission.Server where

import Control.Monad.Catch (MonadThrow (..))
import Control.Exception (Exception (..))
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

data TxSubmissionProtocolError
  = TxSubmissionExidedCapacityError
  deriving (Show, Eq, Ord)

instance Exception TxSubmissionProtocolError

-- | Submission server stream.  It will throw @'TxSubmissionProtocolError'@ when
-- the server recieved more than @n@ transactions.
--
txSubmissionServerStream
  :: forall (n :: Nat) tx err m a.
     ( Monad m
     , MonadThrow m
     )
  => SNat n
  -> TxSubmissionServer tx err m a
  -> Peer (TxSubmissionProtocolN n) (TxSubmissionMessage n tx err)
      (Awaiting (StIdle Zero))
      (Finished StDone)
      m (Maybe a)
txSubmissionServerStream sn (TxSubmissionServer server) = lift $ go SZero <$> server
 where
  go :: SNat i
     -> TxSubmissionServer tx err m a
     -> Peer (TxSubmissionProtocolN n) (TxSubmissionMessage n tx err)
          (Awaiting (StIdle k))
          (Finished StDone)
          m (Maybe a)
  go si TxSubmissionHandlers {handleTx, handleTxDone} =
    await $ \msg -> case msg of
      MsgTx tx -> case SSucc si `lessEqualThan` sn of
        STrue  -> lift $ go (SSucc si) <$> handleTx tx
        SFalse -> lift $ throwM TxSubmissionExidedCapacityError
      MsgClientDone -> lift $ handleTxDone >>= \r -> case r of
        Right a  -> pure $ out (MsgServerDone Nothing) (done $ Just a)
        Left err -> pure $ out (MsgServerDone (Just err)) (done Nothing)
