{-# LANGUAGE GADTs        #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Ouroboros.Network.Protocol.TxSubmission.Type where

import Protocol.Core

data TxSubmissionState where
  StIdle :: TxSubmissionState -- client is streaming transactions
  StBusy :: TxSubmissionState -- server received all transactions
  StDone :: TxSubmissionState -- transaction submission protocol finished

-- |
-- A type which indentifies client\/server partition of states in the
-- transaction submission protocol.
--
data TxSubmissionProtocol

type instance Partition TxSubmissionProtocol st client server terminal =
  TxSubmissionPartition st client server terminal

type family TxSubmissionPartition st (client :: Control) (server :: Control) (terminal :: Control) :: Control where
  TxSubmissionPartition StIdle client server terminal = client -- client is in charge of streaming transactions
  TxSubmissionPartition StBusy client server terminal = server
  TxSubmissionPartition StDone client server terminal = terminal

-- |
-- @'TxSubmissionProtocol'@ messages
data TxSubmissionMessage tx err from to where
  MsgTx         :: tx -> TxSubmissionMessage tx err StIdle StIdle
  MsgClientDone :: TxSubmissionMessage tx err StIdle StBusy
  MsgServerDone :: Maybe err -> TxSubmissionMessage tx err StBusy StDone
