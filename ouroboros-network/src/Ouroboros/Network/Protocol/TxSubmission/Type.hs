{-# LANGUAGE GADTs        #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.TxSubmission.Type where

import Protocol.Core

data Nat = Zero | Succ Nat

data SNat (n :: Nat) where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

type family DoubleNat (n :: Nat) :: Nat where
  DoubleNat Zero        = Zero
  DoubleNat (Succ n)    = Succ (Succ (DoubleNat n))

doubleSNat :: SNat n -> SNat (DoubleNat n)
doubleSNat SZero     = SZero
doubleSNat (SSucc n) = SSucc (SSucc (doubleSNat n))

type N32 = DoubleNat (DoubleNat (DoubleNat (DoubleNat (DoubleNat(Succ Zero)))))

s32 :: SNat N32
s32 = doubleSNat (doubleSNat (doubleSNat (doubleSNat (doubleSNat (SSucc SZero)))))

type family LessEqualThan (n :: Nat) (m :: Nat) :: Bool where
  LessEqualThan (Succ n) (Succ m) = LessEqualThan n m
  LessEqualThan Zero     _        = True
  LessEqualThan (Succ n) Zero     = False

data SBool (b :: Bool) where
  STrue  :: SBool True
  SFalse :: SBool False

lessEqualThan :: SNat (n :: Nat) -> SNat (m :: Nat) -> SBool (LessEqualThan n m)
lessEqualThan (SSucc n) (SSucc m) = lessEqualThan n m
lessEqualThan SZero     (SSucc _) = STrue
lessEqualThan SZero     SZero     = STrue
lessEqualThan (SSucc _) SZero     = SFalse

type family Pred (n :: Nat) :: Nat where
  Pred (Succ n) = n

spred :: LessEqualThan (Succ Zero) n ~ True => SNat n -> SNat (Pred n)
spred (SSucc n) = n

data TxSubmissionState (n :: Nat) where
  StIdle :: Nat -> TxSubmissionState n -- client is streaming transactions
  StBusy :: forall (m :: Nat). TxSubmissionState m -- server received all transactions
  StDone :: forall (m :: Nat). TxSubmissionState m -- transaction submission protocol finished

-- |
-- A type which indentifies client\/server partition of states in the
-- transaction submission protocol.  @n :: Nat@ is an upper bound for the number
-- of transaction a client can send using a single instance of this protocol.
--
data TxSubmissionProtocolN (n :: Nat)

type TxSubmissionProtocol32  = TxSubmissionProtocolN N32
type TxSubmissionProtocol64  = TxSubmissionProtocolN (DoubleNat N32)
type TxSubmissionProtocol128 = TxSubmissionProtocolN (DoubleNat (DoubleNat N32))

type instance Partition (TxSubmissionProtocolN n) st client server terminal =
  TxSubmissionPartition st client server terminal

type family TxSubmissionPartition st (client :: Control) (server :: Control) (terminal :: Control) :: Control where
  TxSubmissionPartition (StIdle n) client server terminal = client -- client is in charge of streaming transactions
  TxSubmissionPartition StBusy client server terminal = server
  TxSubmissionPartition StDone client server terminal = terminal

-- |
-- @'TxSubmissionProtocolN'@ messages
data TxSubmissionMessage (n :: Nat) tx err from to where
  MsgTx         :: (LessEqualThan m n ~ True) => tx -> TxSubmissionMessage n tx err (StIdle (m :: Nat)) (StIdle (Succ m))
  MsgClientDone :: TxSubmissionMessage n tx err (StIdle m) StBusy
  MsgServerDone :: Maybe err -> TxSubmissionMessage n tx err StBusy StDone
