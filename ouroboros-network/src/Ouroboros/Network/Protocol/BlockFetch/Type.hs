{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-
 Block fetching consists of two protocols: submission of block ranges 'BlockRequestProtocol', and
 streaming of blocks 'BlockFetchProtocol'.

  client server
   |       |
   | \     |
   |  \    | 'BlockRequestProtocol' (requesting a range)
   |   \   |
   |    \  |
   |     \ |
   |     / |
   |    /  |
   |\  / / | 'BlockFetchProtocol' (streaming blocks)
   | \/ /  |
   | /\/ / |
   |  /\/  |
   | / /\  |
   |  /  \ |
   | /   / |
   |    /  |
   |   / / |
   |  / /  |
   | / / / |
   |  / /  |
   | / /   |
   |  /    |

  Request submitted by @'BlockRequestProtocol'@ are recorded by the server
  and supplied to the @'BlockFetchProtocol'@.  This way we achieve
  protocol pipelining.

  Both protocol never shift the agency: the 'BlockRequestProtocol' keep
  agancy on the client side and the 'BlockFetchProtocol' keeps it on the
  server side.

  The receiver of @'BlockRequestProtocol'@ writes received ranges to
  a queue which acumulates @'RangeStream' range@ elements.  The sender of
  @'BlockFetchProtocol'@ is reading this queue and sending back blocks to
  a @'BlockFetchServerReceiver'@.

  Termination

  If the @'BlockFetchClientSender'@ (of the @'BlockFetchClientProtoco'@) will
  send @'MessageRequestDone'@ the corresponding @'BlockFetchServerReciever'@ will
  write @'End'@ to the queue which connects both protocols.  When the
  @'BlockFetchServerSender'@ will encounter @'End'@, it will send
  @'MessageServerDone'@ which will terminate the protocol.

  Note: there is no mechanism which ensures that if the
  @'BlockFetchServerSender'@ will send @'MessageServerDone'@ that the
  corresponding @'BlockRequestProtocol@ will terminate.

-}
module Ouroboros.Network.Protocol.BlockFetch.Type where

import Protocol.Core

import Ouroboros.Network.Chain (Point)

-- | Range of headers
--
data ChainRange header = ChainRange !(Point header) !(Point header)
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Client protocol: requesting range of blocks
-------------------------------------------------------------------------------}

-- | Protocol tag.  @'BlockRequestProtocol'@ is a protocol in which the
-- client has the agency.  It sends requests to its server side with ranges of
-- blocks.  The server just records them.  They will be handled by the server of
-- @'BlockFetchProtocol'@ which will stream ranges of blocks to its client
-- side.
--
data BlockRequestProtocol

data ClientState where
  StClientIdle :: ClientState
  StClientDone :: ClientState

type instance Partition BlockRequestProtocol st client server terminal =
  BlockRequestPartition st client server terminal

type family BlockRequestPartition st (client :: Control) (server :: Control) (terminal :: Control) :: Control where
  BlockRequestPartition StClientIdle client server termianl = client
  BlockRequestPartition StClientDone client server terminal = terminal

data BlockRequestMessage range from to where

  MessageRequestRange
    :: range
    -> BlockRequestMessage range StClientIdle StClientIdle

  MessageRequestDone
    :: BlockRequestMessage range StClientIdle StClientDone

instance (Show range) => Show (BlockRequestMessage range from to) where
  show (MessageRequestRange range) = "MessageRequestRange " ++ show range
  show MessageRequestDone          = "MessageRequestDone"
  
{-------------------------------------------------------------------------------
  Server side protocol: stream blocks to the client as requested by the
 @'BlockRequestProtocol'@.
-------------------------------------------------------------------------------}

-- | Protocol tag.  @'BlockFetchProtocol'@ keeps agency on the server
-- side.  It is designed to stream ranges blocks to its client.  Ranges are
-- received out side of this protocol, using @'BlockRequestProtocol'@.
--
data BlockFetchProtocol

data ServerState where
  -- sever being idle
  StServerAwaiting :: ServerState
  -- server seding blocks
  StServerSending  :: ServerState
  -- server termination message
  StServerDone     :: ServerState

type instance Partition BlockFetchProtocol st client server terminal =
  BlockFetchPartition st client server terminal

type family BlockFetchPartition st (client :: Control) (server :: Control) (terminal :: Control) :: Control where
  BlockFetchPartition StServerAwaiting client server terminal = server
  BlockFetchPartition StServerSending  client server terminal = server
  BlockFetchPartition StServerDone     client server terminal = terminal

data BlockFetchMessage block from to where

  -- | Block fetching messages; First block from a batch
  MessageStartBatch
    :: BlockFetchMessage block StServerAwaiting StServerSending
  -- | Block streaming
  MessageBlock
    :: block
    -> BlockFetchMessage block StServerSending StServerSending
  -- | End of batch
  MessageBatchDone
    :: BlockFetchMessage block StServerSending StServerAwaiting

  -- | Server errors
  MessageServerError
    :: BlockFetchMessage block StServerSending StServerAwaiting
  MessageNoBlocks
    :: BlockFetchMessage block StServerAwaiting StServerAwaiting

  -- | Server side of this protocol can terminate the protocol.  This will
  -- usually be send when the corresponding client in the
  -- @'BlockRequestProtocol'@ protocol running along will send
  -- @'MessageRequestDone'@.
  MessageServerDone
    :: BlockFetchMessage block StServerAwaiting StServerDone

instance (Show block) => Show (BlockFetchMessage block from to) where
  show MessageStartBatch    = "MessageStartBatch"
  show (MessageBlock block) = "MessageBlock " ++ show block
  show MessageBatchDone     = "MessageBatchDone"
  show MessageServerError   = "MessageServerError"
  show MessageNoBlocks      = "MessageNoBlock"
  show MessageServerDone    = "MessageServerDone"
