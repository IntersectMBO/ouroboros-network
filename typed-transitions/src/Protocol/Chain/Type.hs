{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.Chain.Type where

import Block
import Chain (Point)
import Data.List.NonEmpty
import Protocol.Core

type Header = BlockHeader
type Body = BlockBody

-- | Many responses can include a new tip of chain, in case the producer's
-- chain was extended since the last request (not forked!).
type MaybeNewTip = Maybe Header

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
  TrInit     :: Point -> Header -> TrChainExchange 'StInit 'StIdle
  TrRequest  :: Request  req     -> TrChainExchange 'StIdle ('StBusy req)
  TrRespond  :: Response req res -> TrChainExchange ('StBusy req) res

showTrChainExchange :: TrChainExchange from to -> String
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

data RequestKind where
  SetHead  :: RequestKind
  Download :: RequestKind
  -- | Request for the next change in the chain.
  Next     :: RequestKind
  -- | No 'Request' uses this kind. It's an intermediate state to ensure that a
  -- body follows a header during relay.
  Relay    :: RequestKind

data Request (req :: RequestKind) where
  -- | Request to update the read pointer to the newest of these blocks.
  ReqSetHead  :: NonEmpty Point -> Request 'SetHead
  -- | Request at most this many headers, from the current read pointer.
  ReqDownload :: Word -> Request 'Download
  -- | Request the next update to the producer's chain.
  -- The response could be a header followed by its body (fast relay)
  -- or a typical change response with a new tip and rollback point (fork).
  ReqNext     :: Request 'Next

-- | Fork and extend can be responses to any request.
-- The headers and bodies responses are multi-part: an individual data point
-- can be sent, or the response can be closed, returning the state to idle.
data Response (req :: RequestKind) (res :: StChainExchange) where
  -- | New tip and rollback point.
  ResChange       :: Point -> Header -> Response anything 'StIdle
  -- | The new read pointer, possibly not in the requested set, meaning none of
  -- them are in the chain.
  ResSetHead      :: Point -> MaybeNewTip -> Response 'SetHead 'StIdle
  ResDownloadOne  :: Block -> MaybeNewTip -> Response 'Download ('StBusy 'Download)
  ResDownloadDone :: MaybeNewTip -> Response 'Download 'StIdle
  ResExtend       :: Header -> Response 'Next 'StIdle
  -- | Relay of header. Its body (or a change of tip) will follow.
  ResExtendRelay  :: Header -> Response 'Next ('StBusy 'Relay)
  -- | If the chain changes after the header was relayed (ResExtendRelay) but
  -- before the body comes in and is relayed (ResRelayBody), and the new tip
  -- is also a candidate for fast relay (its parent is the read pointer) then
  -- we need to relay the header again.
  ResExtendNewRelay :: Header -> Response 'Relay ('StBusy 'Relay)
  -- | Header extension during a relay, aborting the relay.
  ResExtendNew      :: Header -> Response 'Relay 'StIdle
  -- | Body relay finished.
  ResRelayBody      :: Body -> Response 'Relay 'StIdle

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

-- | Proof that the protocol can always progress (every transition has at least
-- one out-edge).
progress :: Progress TrChainExchange
progress tr noNext = case tr of
  TrInit _ _                     -> noNext (TrRequest ReqNext)
  TrRequest _                    -> noNext someChange
  TrRespond (ResChange _ _)      -> noNext (TrRequest ReqNext)
  TrRespond (ResSetHead _ _)     -> noNext (TrRequest ReqNext)
  TrRespond (ResDownloadOne _ _) -> noNext (TrRespond (ResDownloadDone Nothing))
  TrRespond (ResDownloadDone _)  -> noNext (TrRequest ReqNext)
  TrRespond (ResExtend _)        -> noNext (TrRequest ReqNext)
  TrRespond (ResExtendRelay h)   -> noNext (TrRespond (ResExtendNewRelay h))
  TrRespond (ResExtendNewRelay h) -> noNext (TrRespond (ResExtendNewRelay h))
  TrRespond (ResRelayBody b)      -> noNext (TrRequest ReqNext)
  TrRespond (ResExtendNew h)      -> noNext (TrRequest ReqNext)
  where
  someChange :: forall req . TrChainExchange ('StBusy req) 'StIdle
  someChange = TrRespond (ResChange somePoint someHeader)
  -- Can't be bothered to write out the point and the header, but should do so
  -- later.
  somePoint  = undefined
  someHeader = undefined
