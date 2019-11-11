{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- | The type of the local transaction monitoring protocol.
--
-- This is used by local clients (like wallets, explorers and CLI tools) to
-- monitor the transactions passing through the mempool of a local node.
--
module Ouroboros.Network.Protocol.LocalTxMonitor.Type where


import           Network.TypedProtocol.Core


-- | The kind of the local transaction monitoring protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parametrised over the type of transactions.
--
data LocalTxMonitor tx where

  -- | The client has agency; it can request a transaction or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle   :: LocalTxMonitor tx

  -- | The server has agency; it must (eventually) return the next transaction
  -- that has not yet been sent to the client.
  --
  -- There is no timeout in this state.
  --
  StBusy   :: LocalTxMonitor tx

  -- | Nobody has agency. The terminal state.
  --
  StDone   :: LocalTxMonitor tx


instance Protocol (LocalTxMonitor tx) where

  -- | The messages in the transaction monitoring protocol.
  --
  -- There is no guarantee or requirement that every transaction that enters
  -- the mempool be sent, it need only be in the mempool at the time. So this
  -- protocol can be used to monitor what is in the mempool but it cannot
  -- guarantee to return everything that ever passed though the mempool. In
  -- particular if the client is too slow then txs can be removed from the
  -- mempool before the client requests them. This is reasonable semantics
  -- since it is observationally equivalent to what can happen anyway: we can
  -- \"miss\" a transaction even before the transaction makes it into the
  -- node's mempool (since it can be removed from the mempool of a peer and
  -- then not forwarded).
  --
  data Message (LocalTxMonitor tx) from to where

    -- | The client requests a single transaction and waits a reply.
    --
    -- There is no timeout.
    --
    MsgRequestTx
      :: Message (LocalTxMonitor tx) StIdle StBusy

    -- | The server responds with a single transaction. This must be a
    -- transaction that was not previously sent to the client.
    --
    -- There is no timeout. This can take an arbitrarily long time.
    --
    MsgReplyTx
      :: tx
      -> Message (LocalTxMonitor tx) StBusy StIdle

    -- | The client can terminate the protocol.
    --
    MsgDone
      :: Message (LocalTxMonitor tx) StIdle StDone


  data ClientHasAgency st where
    TokIdle  :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokBusy  :: ServerHasAgency StBusy

  data NobodyHasAgency st where
    TokDone  :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


deriving instance Show tx => Show (Message (LocalTxMonitor tx) from to)

instance Show (ClientHasAgency (st :: LocalTxMonitor tx)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: LocalTxMonitor tx)) where
  show TokBusy = "TokBusy"
