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

import Numeric.Natural (Natural)

import Protocol.Core

-- |
-- States in the stream system.
--
data StreamState where
  StIdle :: StreamState -- client asking for to stream data
  StBusy :: StreamState -- server is busy streaming data
  StFull :: StreamState -- window is full, server will ask the client to update
                        -- its window
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
  StreamStatePartition 'StFull client server terminal = client   -- client is in charge of renewing server window size
  StreamStatePartition 'StDone client server terminal = terminal

-- |
-- Messages in the stream message protocol.
--
-- The protocol starts with client sending @'MsgRequest'@ with a given range
-- and window.  Then the server streams data by sending @'MsgData'@ until the
-- window is exhousted.   In this case the server sends @'MsgRenewWindow'@ with
-- a new window size.  When server receives @'MsgUpdteWindow'@ it resumes
-- streaming until either window is exhousted again or the stream finishes.
-- When stream finishes the server notifies the client with @'MsgStreamEnd'@.
--
data StreamMessage rng a from to where
  MsgRequest      :: rng -> Natural -> StreamMessage rng a 'StIdle 'StBusy -- request a range with a give window size
  MsgData         :: a -> StreamMessage rng a 'StBusy 'StBusy -- response with data
  MsgRenewWindow  :: StreamMessage rng a 'StBusy 'StFull      -- await (end of window, server expecting `MsgUpdateWindow`)
  MsgUpdateWindow :: StreamMessage rng a 'StFull 'StBusy      -- send window update
  MsgStreamEnd    :: StreamMessage rng a 'StBusy 'StDone      -- the stream is over


instance  Show a => Show (StreamMessage rng a from to) where
  show (MsgRequest _r _n)       = "MsgRequest"
  show (MsgData a)              = "MsgData " ++ show a
  show MsgRenewWindow           = "MsgRenewWindow"
  show MsgUpdateWindow          = "MsgUpdateWindow"
  show MsgStreamEnd             = "MsgStreamEnd"
