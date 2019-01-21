{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Network.Protocol.ReqResp.Type where

import Protocol.Core


-- | States in the get protocol.
data ReqRespState where
  StIdle :: ReqRespState
  StBusy :: ReqRespState
  StDone :: ReqRespState

-- | A type to ideintiy the client \/server partition of states in the get
-- protocol.
--
data ReqRespProtocol request response

type instance Partition (ReqRespProtocol request response) st client server terminal =
  ReqRespStatePartition st client server terminal

-- | Idle state is when the client sends a request, busy state is when the
-- server responds with data and terminates.
--
type family ReqRespStatePartition st client server terminal :: k where
 ReqRespStatePartition StIdle client server terminal = client
 ReqRespStatePartition StBusy client server terminal = server
 ReqRespStatePartition StDone client server terminal = terminal

-- | Message protocol
--
data ReqRespMessage request response from to where
  MsgRequest  :: request -> ReqRespMessage request response StIdle StBusy
  MsgResponse :: response -> ReqRespMessage request response StBusy StDone

instance
  (Show request, Show response)
  => Show (ReqRespMessage request response from to)
 where
  show (MsgRequest request)   = "MsgRequest "  ++ show request
  show (MsgResponse response) = "MsgResponse " ++ show response
