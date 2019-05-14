{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TxSubmission.Client where

import           Data.Word (Word16)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.TxSubmission.Type


-- |
-- Client side of the tx-submission protocol.
--
newtype TxSubmissionClient hash tx m a = TxSubmissionClient {
    runTxSubmissionClient :: m (TxSubmissionHandlers hash tx m a)
  }

instance Functor m => Functor (TxSubmissionClient hash tx m) where
  fmap f (TxSubmissionClient msender) = TxSubmissionClient ((fmap  . fmap) f msender)

-- |
-- (Recursive) handlers of the tx-submission client
--
data TxSubmissionHandlers hash tx m a = TxSubmissionHandlers {
    getHashes :: Word16 -> m ([hash], TxSubmissionHandlers hash tx m a),
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
-- A non-pipelined @'Peer'@ representing the @'TxSubmissionClient'@.
--
txSubmissionClientPeer
  :: forall hash tx m a. Monad m
  => TxSubmissionClient hash tx m a
  -> Peer (TxSubmission hash tx) AsClient StIdle m a
txSubmissionClientPeer (TxSubmissionClient mclient) = Effect $ go <$> mclient
  where
    go :: TxSubmissionHandlers hash tx m a
       -> Peer (TxSubmission hash tx) AsClient StIdle m a
    go TxSubmissionHandlers {getHashes, getTx, done} =
      Await (ServerAgency TokIdle) $ \msg -> case msg of
        MsgGetHashes n -> Effect $ do
          (hs, next) <- getHashes n
          return $ Yield (ClientAgency TokSendHashes) (MsgSendHashes hs) (go next)
        MsgGetTx hash  -> Effect $ do
          (tx, next) <- getTx hash
          return $ Yield (ClientAgency TokSendTx) (MsgTx tx) (go next)
        MsgDone        -> Done TokDone done
