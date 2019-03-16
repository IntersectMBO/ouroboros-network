{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Ouroboros.Network.Protocol.TxSubmission.Type where

import           Data.Word (Word16)

import           Network.TypedProtocol.Core

-- |
-- Trasaction-submission protocol states; states transitions are given by
-- @'Message' ('TxSubmission' hash tx)@ below.
--
data TxSubmission hash tx where
  -- |
  -- Client has agency; it can either terminate, ask for transaction hashes or
  -- ask for a transaction.
  --
  StIdle       :: TxSubmission hash tx

  -- |
  -- Server has agency; it must reply with a list of transaction hashes.
  --
  StSendHashes :: TxSubmission hash tx

  -- |
  -- Server has agency;  it must replay with a trasaction.
  StSendTx     :: TxSubmission hash tx

  -- |
  -- Nobody has agency; termination state.
  StDone       :: TxSubmission hash tx


instance Protocol (TxSubmission hash tx) where

  data Message (TxSubmission hash tx) from to where
    -- |
    -- Get a list of transaction's hashes from the server.
    --
    MsgGetHashes
      :: Word16
      -> Message (TxSubmission hash tx) StIdle StSendHashes

    -- |
    -- Reply with a list of available transaction's hashes.
    --
    MsgSendHashes
      :: [hash]
      -> Message (TxSubmission hash tx) StSendHashes StIdle

    -- |
    -- Request a transaction corresponding to the given hash.
    --
    MsgGetTx
      :: hash
      -> Message (TxSubmission hash tx) StIdle StSendTx

    -- |
    -- Send a transaction.
    --
    MsgTx
      :: tx
      -> Message (TxSubmission hash tx) StSendTx StIdle

    -- |
    -- Termination message.
    --
    MsgDone
      :: Message (TxSubmission hash tx) StIdle StDone

  data ClientHasAgency st where
    TokIdle       :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokSendHashes :: ServerHasAgency StSendHashes
    TokSendTx     :: ServerHasAgency StSendTx

  data NobodyHasAgency st where
    TokDone       :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

deriving instance (Eq hash, Eq tx) => Eq (Message (TxSubmission hash tx) from to)
deriving instance (Show hash, Show tx) => Show (Message (TxSubmission hash tx) from to)
