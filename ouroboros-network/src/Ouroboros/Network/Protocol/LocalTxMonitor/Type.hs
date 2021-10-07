{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | The type of the local transaction monitoring protocol.
--
-- This is used by local clients (like wallets, explorers and CLI tools) to
-- monitor the transactions passing through the mempool of a local node.
--
-- The protocol is stateful such that the server keeps track of the transactions
-- already sent to the client.
--
--
--                     START
--                       ⇓
--                     ┌───────────────┐
--             ┌──────▶│     Idle      │⇒ DONE
--             │       └───┬───────────┘
--             │           │
--             │   Acquire │
--             │           ▼
--             │       ┌───────────────┐
--     Release │       │   Acquiring   │
--             │       └───┬───────────┘
--             │           │       ▲
--             │  Acquired │       │ ReAcquire
--             │           ▼       │
--             │       ┌───────────┴───┐
--             └───────┤   Acquired    │
--                     └───┬───────────┘
--                         │       ▲
--   HasTx/NextTx/GetSizes │       │ Reply (HasTx/NextTx/GetSizes)
--                         ▼       │
--                     ┌───────────┴───┐
--                     │      Busy     │
--                     └───────────────┘
--
module Ouroboros.Network.Protocol.LocalTxMonitor.Type where


import           Data.Word
import           GHC.Generics (Generic)

import           Network.TypedProtocol.Core
import           Ouroboros.Network.Util.ShowProxy


-- | The kind of the local transaction monitoring protocol, and the types of
-- the states in the protocol state machine.
--
-- It is parametrised over the type of transactions.
--
data LocalTxMonitor txid tx slot where

  -- | The client has agency; it can request a transaction or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle   :: LocalTxMonitor txid tx slot

  -- | The server has agency; it is capturing the latest mempool snapshot.
  --
  StAcquiring :: LocalTxMonitor txid tx slot

  -- | The client has agency; The server is locked on a particular mempool
  -- snapshot. The client can now perform various requests on that snapshot,
  -- or acquire a new one, more recent.
  --
  StAcquired  :: LocalTxMonitor txid tx slot

  -- | The server has agency; It must respond, there's no timeout.
  --
  StBusy :: StBusyKind -> LocalTxMonitor txid tx slot

  -- | Nobody has agency. The terminal state.
  --
  StDone   :: LocalTxMonitor txid tx slot


instance
    ( ShowProxy txid
    , ShowProxy tx
    , ShowProxy slot
    ) =>
    ShowProxy (LocalTxMonitor txid tx slot)
  where
    showProxy _ = unwords
      [ "LocalTxMonitor"
      , showProxy (Proxy :: Proxy txid)
      , showProxy (Proxy :: Proxy tx)
      , showProxy (Proxy :: Proxy slot)
      ]

data StBusyKind where
  -- | The server is busy fetching the next transaction from the mempool
  NextTx :: StBusyKind
  -- | The server is busy looking for the presence of a specific transaction in
  -- the mempool
  HasTx :: StBusyKind
  -- | The server is busy looking for the current size and max capacity of the
  -- mempool
  GetSizes :: StBusyKind

-- | Describes the MemPool sizes and capacity for a given snapshot.
data MempoolSizeAndCapacity = MempoolSizeAndCapacity
  { capacityInBytes :: !Word32
    -- ^ The maximum capacity of the mempool. Note that this may dynamically
    -- change when the ledger state is updated.
  , sizeInBytes     :: !Word32
    -- ^ The summed byte size of all the transactions in the mempool.
  , numberOfTxs     :: !Word32
    -- ^ The number of transactions in the mempool
  } deriving (Generic, Eq, Show)

instance Protocol (LocalTxMonitor txid tx slot) where

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
  data Message (LocalTxMonitor txid tx slot) from to where

    -- | Acquire the latest snapshot. This enables subsequent queries to be
    -- made against a consistent view of the mempool.
    --
    -- There is no timeout.
    --
    MsgAcquire
      :: Message (LocalTxMonitor txid tx slot) StIdle StAcquiring

    -- | The server is now locked to a particular snapshot. It returns the
    -- slot number of the 'virtual block' under construction.
    --
    MsgAcquired
      :: slot
      -> Message (LocalTxMonitor txid tx slot) StAcquiring StAcquired

    -- | Like 'MsgAcquire', but when one is already acquired. Allows to renew
    -- the snapshot's state.
    --
    MsgReAcquire
      :: Message (LocalTxMonitor txid tx slot) StAcquired StAcquiring

    -- | The client requests a single transaction and waits a reply.
    --
    MsgNextTx
      :: Message (LocalTxMonitor txid tx slot) StAcquired (StBusy NextTx)

    -- | The server responds with a single transaction. This must be a
    -- transaction that was not previously sent to the client for this
    -- particular snapshot.
    --
    MsgReplyNextTx
      :: Maybe tx
      -> Message (LocalTxMonitor txid tx slot) (StBusy NextTx) StAcquired

    -- | The client checks whether the server knows of a particular transaction
    -- identified by its id.
    --
    MsgHasTx
      :: txid
      -> Message (LocalTxMonitor txid tx slot) StAcquired (StBusy HasTx)

    -- | The server responds 'True' when the given tx is present in the snapshot,
    -- False otherwise.
    --
    MsgReplyHasTx
      :: Bool
      -> Message (LocalTxMonitor txid tx slot) (StBusy HasTx) StAcquired

    -- | The client asks the server about the mempool current size and max
    -- capacity.
    --
    MsgGetSizes
      :: Message (LocalTxMonitor txid tx slot) StAcquired (StBusy GetSizes)

    -- | The server responds with the mempool size and max capacity.
    --
    MsgReplyGetSizes
      :: MempoolSizeAndCapacity
      -> Message (LocalTxMonitor txid tx slot) (StBusy GetSizes) StAcquired

    -- | Release the acquired snapshot, in order to loop back to the idle state.
    --
    MsgRelease
      :: Message (LocalTxMonitor txid tx slot) StAcquired StIdle

    -- | The client can terminate the protocol.
    --
    MsgDone
      :: Message (LocalTxMonitor txid tx slot) StIdle StDone

  data ClientHasAgency st where
    TokIdle     :: ClientHasAgency StIdle
    TokAcquired :: ClientHasAgency StAcquired

  data ServerHasAgency st where
    TokAcquiring :: ServerHasAgency StAcquiring
    TokBusy      :: TokBusyKind k -> ServerHasAgency (StBusy k)

  data NobodyHasAgency st where
    TokDone  :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

data TokBusyKind (k :: StBusyKind) where
  TokNextTx   :: TokBusyKind NextTx
  TokHasTx    :: TokBusyKind HasTx
  TokGetSizes :: TokBusyKind GetSizes

deriving instance (Show txid, Show tx, Show slot)
  => Show (Message (LocalTxMonitor txid tx slot) from to)

instance Show (ClientHasAgency (st :: LocalTxMonitor txid tx slot)) where
  show = \case
    TokIdle     -> "TokIdle"
    TokAcquired -> "TokAcquired"

instance Show (ServerHasAgency (st :: LocalTxMonitor txid tx slot)) where
  show = \case
    TokAcquiring        -> "TokAcquiring"
    TokBusy TokNextTx   -> "TokBusy TokNextTx"
    TokBusy TokHasTx    -> "TokBusy TokHasTx"
    TokBusy TokGetSizes -> "TokBusy TokGetSizes"
