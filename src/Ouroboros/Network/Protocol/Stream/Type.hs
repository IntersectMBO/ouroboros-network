{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Stream values from a server to a client
module Ouroboros.Network.Protocol.Stream.Type where

import Protocol.Core

-- |
-- States in the stream system.
--
data StreamState where
  StIdle :: StreamState -- client asking for to stream data
  StBusy :: StreamState -- server is busy streaming data
  StDone :: StreamState -- streaming finished

-- |
-- A type which indentifies client\/server partition of states in the stream
-- protocol.
--
data StreamProtocol

-- |
-- Partition from the streaming protocol, i.e. who is in charge of advancing
-- the protocol.
--
type instance Partition StreamProtocol st client server terminal =
  StreamStatePartition st client server terminal

type family StreamStatePartition st client server terminal where
  StreamStatePartition 'StIdle client server terminal = client
  StreamStatePartition 'StBusy client server terminal = server   -- server is in charge of streaming data
  StreamStatePartition 'StDone client server terminal = terminal

-- |
-- Messages in the stream message protocol.  It is a simple single request,
-- multiple responds protocol.
--
-- Note: @'MsgStreamEnd'@ is not carrying any data, this make more natural to
-- create a server from a stream (e.g. a @'Producer'@ from @'pipes'@ package).
--
data StreamMessage range a from to where
  MsgRequest      :: range -> StreamMessage range a 'StIdle 'StBusy -- request a range 
  MsgData         :: a -> StreamMessage range a 'StBusy 'StBusy     -- response with data
  MsgStreamEnd    :: StreamMessage range a 'StBusy 'StDone          -- the stream is over

instance  Show a => Show (StreamMessage range a from to) where
  show (MsgRequest _r) = "MsgRequest"
  show (MsgData a)     = "MsgData " ++ show a
  show MsgStreamEnd    = "MsgStreamEnd"
