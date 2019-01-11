{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Protocol.Get.Type where

import Protocol.Core


-- | States in the get protocol.
data GetState where
  StIdle :: GetState
  StBusy :: GetState
  StDone :: GetState

-- | A type to ideintiy the client \/server partition of states in the get
-- protocol.
--
data GetProtocol request response

type instance Partition (GetProtocol request response) st client server terminal =
  GetStatePartition st client server terminal

-- | Idle state is when the client sends a request, busy state is when the
-- server responds with data and terminates.
--
type family GetStatePartition st client server terminal :: k where
 GetStatePartition StIdle client server terminal = client
 GetStatePartition StBusy client server terminal = server
 GetStatePartition StDone client server terminal = terminal

-- | Message protocol
--
data GetMessage request response from to where
  MsgRequest  :: request -> GetMessage request response StIdle StBusy
  MsgResponse :: response -> GetMessage request response StBusy StDone

instance
  (Show request, Show response)
  => Show (GetMessage request response from to)
 where
  show (MsgRequest request)   = "MsgRequest "  ++ show request
  show (MsgResponse response) = "MsgResponse " ++ show response
