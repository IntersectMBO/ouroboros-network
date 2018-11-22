{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.Chain.Type where

import Data.List.NonEmpty
import Protocol.Core

-- |
-- = Definition of the chain exchange protocol, and a factoring of it into
-- consumer and producer sides.

-- | States in the chain exchange system.
data StChainExchange where
  -- | Initial state: the server sends two points.
  StInit ::                StChainExchange
  -- | Idle state: the client makes a choice.
  StIdle ::                StChainExchange
  -- | Busy state: the server responds to client choice.
  StBusy :: RequestKind -> StChainExchange
  -- | Done state: the client or the server terminated.
  StDone ::                StChainExchange

-- | Transitions on 'StChainExchange'. A request goes from idle to busy, and
-- a response may go from busy to idle, or stay on busy in case of multi-part
-- response for headers.
data TrChainExchange point header stFrom stTo where
  -- | The first transition gives the endpoints of the chain.
  TrInit         :: (point, point)                -> TrChainExchange point header 'StInit         'StIdle
  -- | A client request goes from 'StIdle to 'StBusy.
  TrRequest      :: Request point req             -> TrChainExchange point header 'StIdle         ('StBusy req)
  -- | A server response can go from 'StBusy to 'StIdle, or remain 'StBusy
  -- in case of a multi-part header download.
  TrRespond      :: Response point header req res -> TrChainExchange point header ('StBusy req)   res
  -- | The client stops.
  TrConsumerDone ::                                  TrChainExchange point header 'StIdle         'StDone
  -- | The server stops.
  TrProducerDone ::                                  TrChainExchange point header ('StBusy 'Next) 'StDone

showTrChainExchange :: (Show point, Show header) => TrChainExchange point header from to -> String
showTrChainExchange tr = case tr of
  TrInit _ -> "Init"
  TrRequest (ReqImprove hhs) -> "ReqSetHead " ++ show hhs
  TrRequest (ReqDownload _) -> "ReqDownload"
  TrRequest ReqNext -> "ReqNext"
  TrRespond (ResImprove (hh, _)) -> "ResSetHead " ++ show hh
  TrRespond (ResForked _) -> "ResChange"
  TrRespond (ResDownloadOne _) -> "ResDownloadOne"
  TrRespond (ResDownloadDone _) -> "ResDownloadDone"
  TrRespond _ -> "Res other"
  TrConsumerDone -> "Consumer done"
  TrProducerDone -> "Producer done"

data RequestKind where
  Improve  :: RequestKind
  Download :: RequestKind
  -- | Request for the next change in the chain.
  Next     :: RequestKind

data Request point (req :: RequestKind) where
  -- | Request to update the read pointer to the newest of these blocks.
  ReqImprove  :: NonEmpty point -> Request point 'Improve
  -- | Request at most this many headers, from the current read pointer.
  ReqDownload :: Word           -> Request point 'Download
  -- | Request the next update to the producer's chain: a new tip (extension)
  -- or a fork (read pointer is no longer in the producer's chain).
  ReqNext     ::                   Request point 'Next

-- | Responses parameterised by the request kind.
data Response point header (req :: RequestKind) (res :: StChainExchange) where
  -- | There was a fork: the read pointer has changed. This can be a response
  -- to anything.
  ResForked       :: (point, point)        -> Response point header anything 'StIdle
  -- | The tip of chain has changed, but there was no fork (the new chain
  -- includes the read pointer). This is only a response to 'ReqNext. If there
  -- is an extension between request and response of other types, the new
  -- tip is included in those responses.
  ResExtend       :: point                 -> Response point header 'Next 'StIdle
  -- | The new read pointer. If it's not in the requested set, then none of
  -- them were in the chain.
  ResImprove      :: (point, Maybe point)  -> Response point header 'Improve 'StIdle
  -- | Multi-part download of headers.
  ResDownloadOne  :: (header, Maybe point) -> Response point header 'Download ('StBusy 'Download)
  -- | Multi-part download of headers finished.
  ResDownloadDone :: Maybe point           -> Response point header 'Download 'StIdle

-- |
-- = Paritioning into client/server or consumer/producer
--
-- The above transition system 'TrChainExchange' makes no explicit mention
-- of a client and a server. In theory, the protocol could be run by one
-- process. In order to write separate client and server sides which are
-- complementary, we must partition the states into 2 sets.

-- | A type to identify our client/server parition.
data ChainExchange

type instance Partition ChainExchange st client server terminal =
  ChainExchangePartition st client server terminal

-- | Idle states are client, producer states are server.
-- The init state is server, which is a bit weird. The server starts by giving
-- its tip-of-chain, so that for the rest of the protocol, the consumer always
-- knows the read pointer and the tip-of-chain.
type family ChainExchangePartition st client server terminal where
  ChainExchangePartition 'StInit       client server terminal = server
  ChainExchangePartition 'StIdle       client server terminal = client
  ChainExchangePartition ('StBusy res) client server terminal = server
  ChainExchangePartition 'StDone       client server terminal = terminal
