{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}


-- | The type of the local transaction submission protocol.
--
-- This is used by local clients (like wallets and CLI tools) to submit
-- transactions to a local node.
--
module Ouroboros.Network.Protocol.LocalTxSubmission.Type where


import           Network.TypedProtocol.Core
import           Ouroboros.Network.Util.ShowProxy


-- | The kind of the local transaction-submission protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parametrised over the type of transactions and the type of reasons
-- used when rejecting a transaction.
--
data LocalTxSubmission tx reject where

  -- | The client has agency; it can submit a transaction or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle   :: LocalTxSubmission tx reject

  -- | The server has agency; it must process the submitted transaction and
  -- either accept or reject it (with a reason).
  --
  -- There is a timeout in this state. If the mempool is full and remains so
  -- for a period then the transaction should be rejected with a suitable
  -- temporary failure reason.
  --
  StBusy   :: LocalTxSubmission tx reject

  -- | Nobody has agency. The terminal state.
  --
  StDone   :: LocalTxSubmission tx reject


instance ( ShowProxy tx
         , ShowProxy reject
         ) => ShowProxy (LocalTxSubmission tx reject) where
    showProxy _ = concat
      [ "LocalTxSubmission ("
      , showProxy (Proxy :: Proxy tx)
      , ") ("
      , showProxy (Proxy :: Proxy reject)
      , ")"
      ]


-- | Isomorphic with Maybe but with a name that better describes its purpose and
-- usage.
data SubmitResult reason
  = SubmitSuccess
  | SubmitFail reason
  deriving (Eq, Functor)

instance Protocol (LocalTxSubmission tx reject) where

  -- | The messages in the transaction submission protocol.
  --
  -- In this protocol the client always initiates and the server replies.
  -- This makes it a push based protocol where the client manages the
  -- control flow. It is acceptable for this protocol to be push based
  -- because this protocol is only for use between a node and local client.
  --
  -- The protocol is a very simple request\/response pattern: a single
  -- transaction is submitted and it is either accepted or rejected.
  -- The confirmation or rejection (with reason) is returned.
  --
  data Message (LocalTxSubmission tx reject) from to where

    -- | The client submits a single transaction and waits a reply.
    --
    MsgSubmitTx
      :: tx
      -> Message (LocalTxSubmission tx reject) StIdle StBusy

    -- | The server can reply to inform the client that it has accepted the
    -- transaction.
    --
    MsgAcceptTx
      :: Message (LocalTxSubmission tx reject) StBusy StIdle

    -- | The server can reply to inform the client that it has rejected the
    -- transaction. A reason for the rejection is included.
    --
    MsgRejectTx
      :: reject
      -> Message (LocalTxSubmission tx reject) StBusy StIdle

    -- | The client can terminate the protocol.
    --
    MsgDone
      :: Message (LocalTxSubmission tx reject) StIdle StDone


  data ClientHasAgency st where
    TokIdle  :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokBusy  :: ServerHasAgency StBusy

  data NobodyHasAgency st where
    TokDone  :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


deriving instance (Eq tx, Eq reject) =>
                   Eq (Message (LocalTxSubmission tx reject) from to)

deriving instance (Show tx, Show reject) =>
                   Show (Message (LocalTxSubmission tx reject) from to)

instance Show (ClientHasAgency (st :: LocalTxSubmission tx reject)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: LocalTxSubmission tx reject)) where
  show TokBusy = "TokBusy"
