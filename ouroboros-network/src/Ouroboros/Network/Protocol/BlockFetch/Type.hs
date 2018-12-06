{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Ouroboros.Network.Protocol.BlockFetch.Type where

import Protocol.Core

import Ouroboros.Network.Block (StandardHash)
import Ouroboros.Network.Chain (Point)

-- | Range of headers
--
data ChainRange header = ChainRange !(Point header) !(Point header)
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Client protocol: request range submission
-------------------------------------------------------------------------------}

-- | Protocol tag.  @'BlockFetchClientProtocol'@ is a protocol in which the
-- client has the agency.  It sends requests to its server side with ranges of
-- blocks.  The server just records them.  They will be handled by the server of
-- @'BlockFetchServerProtocol'@ which will stream ranges of blocks to its client
-- side.
--
data BlockFetchClientProtocol

data ClientState where
  StClientIdle :: ClientState
  StClientDone :: ClientState

type instance Partition BlockFetchClientProtocol st client server terminal =
  BlockFetchClientPartition st client server terminal

type family BlockFetchClientPartition st (client :: Control) (server :: Control) (terminal :: Control) :: Control where
  BlockFetchClientPartition StClientIdle client server termianl = client
  BlockFetchClientPartition StClientDone client server terminal = terminal

data BlockRequestClientMessage header from to where

  MessageRequestRange
    :: ChainRange header
    -> BlockRequestClientMessage header StClientIdle StClientIdle

  MessageDone
    :: BlockRequestClientMessage header StClientIdle StClientDone

instance StandardHash header => Show (BlockRequestClientMessage header from to) where
  show (MessageRequestRange range) = "MessageRequestRange " ++ show range
  show MessageDone                 = "MessageDone"
  
{-------------------------------------------------------------------------------
  Server side protocol: stream blocks to the client as requested by the
 @'BlockFetchClientProtocol'@.
-------------------------------------------------------------------------------}

-- | Protocol tag.  @'BlockFetchServerProtocol'@ keeps agency on the server
-- side.  It is designed to stream ranges blocks to its client.  Ranges are
-- received out side of this protocol, using @'BlockFetchClientProtocol'@.
--
data BlockFetchServerProtocol

data ServerState where
  -- sever being idle
  StServerAwaiting :: ServerState
  -- server seding blocks
  StServerSending  :: ServerState
  -- server termination message
  StServerDone     :: ServerState

type instance Partition BlockFetchServerProtocol st client server terminal =
  BlockFetchServerPartition st client server terminal

type family BlockFetchServerPartition st (client :: Control) (server :: Control) (terminal :: Control) :: Control where
  BlockFetchServerPartition StServerAwaiting client server terminal = server
  BlockFetchServerPartition StServerSending  client server terminal = server
  BlockFetchServerPartition StServerDone     client server terminal = terminal

data BlockRequestServerMessage header block from to where

  -- | Block fetching messages; First block from a batch
  MessageStartBatch
    :: block
    -> BlockRequestServerMessage header block StServerAwaiting StServerSending
  -- | Block streaming
  MessageBlock
    :: block
    -> BlockRequestServerMessage header block StServerSending StServerSending
  -- | End of batch
  MessageBatchDone
    :: BlockRequestServerMessage header block StServerSending StServerAwaiting

  -- | Server errors
  MessageServerError
    :: BlockRequestServerMessage header block StServerSending StServerAwaiting
  MessageNoBlocks
    :: BlockRequestServerMessage header block StServerAwaiting StServerAwaiting

  -- | Server side of this protocol can terminate the protocol.  This will
  -- usually be send when the corresponding client in the
  -- @'BlockFetchClientProtocol'@ protocol running along will send
  -- @'MessageDone'@.
  MessageServerDone
    :: BlockRequestServerMessage header block StServerAwaiting StServerDone

instance (Show header, Show block) => Show (BlockRequestServerMessage header block from to) where
  show (MessageStartBatch block) = "MessageStartBatch " ++ show block
  show (MessageBlock block)      = "MessageBlock " ++ show block
  show MessageBatchDone          = "MessageBatchDone"
  show MessageServerError        = "MessageServerError"
  show MessageNoBlocks           = "MessageNoBlock"
  show MessageServerDone         = "MessageServerDone"
