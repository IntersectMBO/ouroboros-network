{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TxSubmission.Server where

import           Numeric.Natural (Natural)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.TxSubmission.Type


-- |
-- Server side of the tx-submission protocol.
--
newtype TxSubmissionServer hash tx m a = TxSubmissionServer {
    runTxSubmissionServer :: m (TxSubmissionHandlers hash tx m a)
  }

instance Functor m => Functor (TxSubmissionServer hash tx m) where
  fmap f (TxSubmissionServer msender) = TxSubmissionServer ((fmap  . fmap) f msender)

-- |
-- (Recursive) handlers of the tx-submission server
--
data TxSubmissionHandlers hash tx m a = TxSubmissionHandlers {
    getHashes :: Natural -> m ([hash], TxSubmissionHandlers hash tx m a),
    getTx     :: hash    -> m (tx,     TxSubmissionHandlers hash tx m a),
    done      :: a
  }

instance Functor m => Functor (TxSubmissionHandlers hash tx m) where
  fmap f TxSubmissionHandlers {getHashes, getTx, done} = TxSubmissionHandlers {
      getHashes = fmap (\(hs, next) -> (hs, fmap f next)) . getHashes,
      getTx     = fmap (\(tx, next) -> (tx, fmap f next)) . getTx,
      done      = f done
    }


-- |
-- A non-pipelined @'Peer'@ representing the @'TxSubmissionServer'@.
--
txSubmissionServerPeer
  :: forall hash tx m a. Monad m
  => TxSubmissionServer hash tx m a
  -> Peer (TxSubmission hash tx) AsServer StIdle m a
txSubmissionServerPeer (TxSubmissionServer mserver) = Effect $ go <$> mserver
  where
    go :: TxSubmissionHandlers hash tx m a
       -> Peer (TxSubmission hash tx) AsServer StIdle m a
    go TxSubmissionHandlers {getHashes, getTx, done} =
      Await (ClientAgency TokIdle) $ \msg -> case msg of
        MsgGetHashes n -> Effect $ do
          (hs, next) <- getHashes n
          return $ Yield (ServerAgency TokSendHashes) (MsgSendHashes hs) (go next)
        MsgGetTx hash  -> Effect $ do
          (tx, next) <- getTx hash
          return $ Yield (ServerAgency TokSendTx) (MsgTx tx) (go next)
        MsgDone        -> Done TokDone done
