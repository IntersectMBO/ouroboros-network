{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Ouroboros.Network.Protocol.TxSubmission.Direct where

import Ouroboros.Network.Protocol.TxSubmission.Type
import Ouroboros.Network.Protocol.TxSubmission.Client
import Ouroboros.Network.Protocol.TxSubmission.Server

direct
  :: forall (n :: Nat) tx err m a b. Monad m
  => TxSubmissionServer tx err m a
  -> TxSubmissionClient n tx err m b
  -> m (Either err a, b)
direct = go
 where
  go :: TxSubmissionServer tx err m a
     -> TxSubmissionClient l tx err m b
     -> m (Either err a, b)
  go handlers@TxSubmissionServer {handleTx,handleTxDone} (TxSubmissionClient mclient) = do
    client <- mclient
    case client of
      TxSubmission tx client' -> do
        _ <- handleTx tx
        go handlers client'
      TxSubmissionDone b (TxSubmissionErrHandler handleErr) ->
        handleTxDone >>= \res -> case res of
          Left err -> (Left err,) <$> handleErr (Just err)
          Right a  -> return (Right a, b)

