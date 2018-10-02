{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Protocol.Chain.Type where

import Block
import Data.List.NonEmpty
import Protocol.Core

type Header = BlockHeader
type Body = BlockBody

-- |
-- = Definition of the chain exchange protocol, and a factoring of it into
-- consumer and producer sides.

-- | States in the chain exchange system.
data StChainExchange where
  StInit :: StChainExchange
  StIdle :: StChainExchange
  StBusy :: RequestKind -> StChainExchange

-- | Transitions on 'StChainExchange'. A request goes from idle to busy, and
-- a response may go from busy to idle, or stay on busy in case of multi-part
-- responses for headers and bodies.
data TrChainExchange stFrom stTo where
  -- | The first transition gives the endpoints of the chain.
  TrInit     :: Header -> Header -> TrChainExchange 'StInit 'StIdle
  TrRequest  :: Request  req     -> TrChainExchange 'StIdle ('StBusy req)
  TrRespond  :: Response req res -> TrChainExchange ('StBusy req) res

showTrChainExchange :: TrChainExchange from to -> String
showTrChainExchange tr = case tr of
  TrInit _ _ -> "Init"
  TrRequest (ReqSetHead hhs) -> "Req set head " ++ show hhs
  TrRequest _ -> "Other request"
  TrRespond (ResSetHead hh) -> "Res set head " ++ show hh
  TrRespond _ -> "Other response"

data RequestKind where
  SetHead :: RequestKind
  Download :: RequestKind
  -- | Request for the next change in the chain.
  Next    :: RequestKind
  -- | No 'Request' uses this kind. It's an intermediate state to ensure that a
  -- body follows a header during relay.
  Relay   :: RequestKind

data Request (req :: RequestKind) where
  -- | Request to update the read pointer to the newest of these blocks.
  ReqSetHead  :: NonEmpty HeaderHash ->    Request 'SetHead
  -- | Request at most this many headers, from the current read pointer.
  ReqDownload :: Word         ->           Request 'Download
  -- | Request the next update to the producer's chain.
  -- The response could be a header followed by its body (fast relay)
  -- or a typical change response with a new tip and rollback point (fork).
  ReqNext     ::                           Request 'Next

-- | Fork and extend can be responses to any request.
-- The headers and bodies responses are multi-part: an individual data point
-- can be sent, or the response can be closed, returning the state to idle.
--
-- TODO new response to indicate "all done"? Typically this would be implicitly
-- delivered by closing the comm channel.
--
--   ResDone :: Response anything 'StDone
data Response (req :: RequestKind) (res :: StChainExchange) where
  -- | New tip and rollback point.
  ResChange       :: Header -> Header -> Response anything 'StIdle
  -- | The new read pointer, possibly not in the requested set, meaning none of
  -- them are in the chain.
  ResSetHead      :: HeaderHash -> Response 'SetHead 'StIdle
  ResDownloadOne  :: Block -> Response 'Download ('StBusy 'Download)
  ResDownloadDone :: Response 'Download 'StIdle
  -- | Relay of header. Its body (or a change of tip) will follow.
  ResRelayHeader  :: Header ->          Response 'Next ('StBusy 'Relay)
  -- | If the chain changes after the header was relayed (ResRelayHeader) but
  -- before the body comes in and is relayed (ResRelayBody), and the new tip
  -- is also a candidate for fast relay (its parent is the read pointer) then
  -- we need to relay the header again.
  ResRelayHeaderAgain :: Header -> Response 'Relay ('StBusy 'Relay)
  ResRelayBody        :: Body -> Response 'Relay 'StIdle

-- |
-- = Paritioning into client/server or consumer/producer
--
-- The above transition system 'TrChainExchange' makes no explicit mention
-- of a client and a server. In theory, the protocol could be run by one
-- process. In order to write separate client and server sides which are
-- complementary, we must partition the states into 2 sets.

-- | A type to identify our client/server parition.
data ChainExchange

type instance Partition ChainExchange st client server =
  ChainExchangePartition st client server

-- | Idle states are client, producer states are server.
-- The init state is server, which is a bit weird. The server starts by giving
-- its tip-of-chain, so that for the rest of the protocol, the consumer always
-- knows the read pointer and the tip-of-chain.
type family ChainExchangePartition st client server where
  ChainExchangePartition 'StInit       client server = server
  ChainExchangePartition 'StIdle       client server = client
  ChainExchangePartition ('StBusy res) client server = server
