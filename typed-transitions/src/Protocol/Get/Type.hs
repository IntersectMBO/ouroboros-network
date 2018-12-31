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
data GetProtocol

type instance Partition GetProtocol st client server terminal =
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
data GetMessage resource resourceId from to where
  MsgRequest  :: resourceId -> GetMessage resource resourceId StIdle StBusy
  MsgResponse :: resource -> GetMessage resource resourceId StBusy StDone
  MsgNoData   :: GetMessage resource resourceId StBusy StDone

instance
  (Show resource, Show resourceId)
  => Show (GetMessage resource resourceId from to)
 where
  show (MsgRequest resourceId) = "MsgRequest " ++ show resourceId
  show (MsgResponse resource)  = "MsgResponse " ++ show resource
  show MsgNoData               = "MsgNoData"
