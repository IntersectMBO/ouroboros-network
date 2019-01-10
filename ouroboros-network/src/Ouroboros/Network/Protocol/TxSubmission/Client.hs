{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
module Ouroboros.Network.Protocol.TxSubmission.Client where

import Protocol.Core
import Ouroboros.Network.Protocol.TxSubmission.Type

import Data.List.NonEmpty (NonEmpty (..))
import Pipes (Producer)
import qualified Pipes

newtype TxSubmissionClient (n :: Nat) tx err m a = TxSubmissionClient {
    runTxSubmissionClient :: m (TxSubmission n tx err m a)
  }

instance Functor m => Functor (TxSubmissionClient n tx err m) where
  fmap f (TxSubmissionClient cli) = TxSubmissionClient $ (fmap . fmap) f cli

data TxSubmission (n :: Nat) tx err m a where
  TxSubmission
    :: LessEqualThan (Succ Zero) n ~ True
    => tx
    -> TxSubmissionClient (Pred n) tx err m a
    -> TxSubmission n tx err m a
  TxSubmissionDone
    :: a
    -> TxSubmissionErrHandler err m a
    -> TxSubmission n tx err m a

instance Functor m => Functor (TxSubmission n tx err m) where
  fmap f (TxSubmission tx cli) = TxSubmission tx (fmap f cli)
  fmap f (TxSubmissionDone a h) = TxSubmissionDone (f a) (fmap f h)

newtype TxSubmissionErrHandler err m a = TxSubmissionErrHandler {
    runTxSubmissionErrHandler :: Maybe err -> m a
  }

instance Functor m => Functor (TxSubmissionErrHandler err m) where
  fmap f (TxSubmissionErrHandler g) = TxSubmissionErrHandler $ (fmap . fmap) f g

txSubmissionClientFromList
  :: ( Applicative m
     )
  => SNat l
  -> NonEmpty tx
  -> TxSubmissionErrHandler err m [tx]
  -> TxSubmissionClient l tx err m [tx]
txSubmissionClientFromList (SSucc _)  (tx :| []) txHandler
  = TxSubmissionClient $ pure $ TxSubmission tx (TxSubmissionClient $ pure $ TxSubmissionDone [] txHandler)
txSubmissionClientFromList (SSucc sl) (tx :| (tx' : txs')) txHandler
  = TxSubmissionClient $ pure $ TxSubmission tx (txSubmissionClientFromList sl (tx' :| txs') txHandler)
txSubmissionClientFromList SZero (tx :| txs) txHandler
  = TxSubmissionClient $ pure $ TxSubmissionDone (tx : txs) txHandler

txSubmissionClientFromProducer
  :: Monad m
  => SNat l
  -> Producer tx m ()
  -> TxSubmissionErrHandler err m (Producer tx m ())
  -> TxSubmissionClient l tx err m (Producer tx m ())
txSubmissionClientFromProducer (SSucc sl) producer txHandler = TxSubmissionClient $
  Pipes.next producer >>= \nxt -> case nxt of
    Left _                -> pure $ TxSubmissionDone (return ()) txHandler
    Right (tx, producer') -> pure $ TxSubmission tx (txSubmissionClientFromProducer sl producer' txHandler)
txSubmissionClientFromProducer SZero producer txHandler = TxSubmissionClient $ pure $ TxSubmissionDone producer txHandler

txSubmissionClientStream
  :: ( LessEqualThan l n ~ True
     , LessEqualThan (Succ Zero) l ~ True
     , Monad m
     )
  => SNat n -- protocol bound
  -> SNat l -- number of txs to send
  -> TxSubmissionClient l tx err m a
  -> Peer (TxSubmissionProtocolN n) (TxSubmissionMessage n tx err)
      (Yielding (StIdle Zero))
      (Finished StDone)
      m a
txSubmissionClientStream = f SZero
 where 
  f :: forall (k :: Nat) (n :: Nat) (l :: Nat) tx err m a.
       Monad m
    => SNat k -- progress
    -> SNat n
    -> SNat l 
    -> TxSubmissionClient l tx err m a
    -> Peer (TxSubmissionProtocolN n) (TxSubmissionMessage n tx err)
        (Yielding (StIdle k))
        (Finished StDone)
        m a
  f sk sn sl (TxSubmissionClient submit) = lift $ submit >>= \cli -> case cli of
    TxSubmission tx scli       ->
      -- recursievly send all transactions
      case lessEqualThan sk sn of
        -- pattern match on the result to satisfy `MsgTx` constraint
        STrue  -> pure $ part (MsgTx tx) (f (SSucc sk) sn (spred sl) scli)
        SFalse -> error "protocol error: the impossible happend"
    TxSubmissionDone _ txHandler ->
      -- client sent all transactions
      pure $ over MsgClientDone $
      -- await for server response
      await $ \(MsgServerDone merr) ->
      -- end the protocol
      lift $ done <$> runTxSubmissionErrHandler txHandler merr
