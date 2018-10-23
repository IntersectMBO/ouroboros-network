{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.Chain.Type where

import Data.List.NonEmpty
import Protocol.Core

-- |
-- = Definition of the chain exchange protocol, and a factoring of it into
-- consumer and producer sides.

-- | States in the chain exchange system.
data StChainExchange where
  StInit :: StChainExchange
  StIdle :: StChainExchange
  StBusy :: RequestKind -> StChainExchange
  StDone :: StChainExchange

-- | Transitions on 'StChainExchange'. A request goes from idle to busy, and
-- a response may go from busy to idle, or stay on busy in case of multi-part
-- responses for headers and bodies.
data TrChainExchange point header stFrom stTo where
  -- | The first transition gives the endpoints of the chain.
  TrInit         :: point -> header -> TrChainExchange point header 'StInit 'StIdle
  TrRequest      :: Request point req -> TrChainExchange point header 'StIdle ('StBusy req)
  TrRespond      :: Response point header req res  -> TrChainExchange point header ('StBusy req) res
  TrConsumerDone :: TrChainExchange point header 'StIdle         'StDone
  TrProducerDone :: TrChainExchange point header ('StBusy 'Next) 'StDone

showTrChainExchange :: (Show point, Show header) => TrChainExchange point header from to -> String
showTrChainExchange tr = case tr of
  TrInit _ _ -> "Init"
  TrRequest (ReqSetHead hhs) -> "ReqSetHead " ++ show hhs
  TrRequest (ReqDownload _) -> "ReqDownload"
  TrRequest ReqNext -> "ReqNext"
  TrRespond (ResSetHead hh _) -> "ResSetHead " ++ show hh
  TrRespond (ResChange _ _) -> "ResChange"
  TrRespond (ResDownloadOne _ _) -> "ResDownloadOne"
  TrRespond (ResDownloadDone _) -> "ResDownloadDone"
  TrRespond _ -> "Res other"
  TrConsumerDone -> "Consumer done"
  TrProducerDone -> "Producer done"

data RequestKind where
  SetHead  :: RequestKind
  Download :: RequestKind
  -- | Request for the next change in the chain.
  Next     :: RequestKind

data Request point (req :: RequestKind) where
  -- | Request to update the read pointer to the newest of these blocks.
  ReqSetHead  :: NonEmpty point -> Request point 'SetHead
  -- | Request at most this many headers, from the current read pointer.
  ReqDownload :: Word -> Request point 'Download
  -- | Request the next update to the producer's chain.
  -- The response could be a header followed by its body (fast relay)
  -- or a typical change response with a new tip and rollback point (fork).
  ReqNext     :: Request point 'Next

-- | Fork and extend can be responses to any request.
-- The headers and bodies responses are multi-part: an individual data point
-- can be sent, or the response can be closed, returning the state to idle.
data Response point header (req :: RequestKind) (res :: StChainExchange) where
  -- | New tip and rollback point.
  ResChange       :: point -> header -> Response point header anything 'StIdle
  -- | The new read pointer, possibly not in the requested set, meaning none of
  -- them are in the chain.
  ResSetHead      :: point -> Maybe header -> Response point header 'SetHead 'StIdle
  ResDownloadOne  :: header -> Maybe header -> Response point header 'Download ('StBusy 'Download)
  ResDownloadDone :: Maybe header -> Response point header 'Download 'StIdle
  ResExtend       :: header -> Response point header 'Next 'StIdle
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
