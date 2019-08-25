{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Network.TypedProtocol.Channel.Session where

import Network.TypedProtocol.Core

-- | Channel which traces session; this is an laternative to 'Peer'.
--
data Channel ps (pr :: PeerRole) (st :: ps) m a where

    SendChannel
      :: forall ps pr st m a.
         WeHaveAgency pr st
      -> (forall st'. Message ps st st' -> m (Channel ps pr st' m a))
      -> Channel ps pr st m a

    RecvChannel
      :: forall ps pr st m a.
         TheyHaveAgency pr st
      -> (forall st'. m (Message ps st st', Channel ps pr st' m a))
      -> Channel ps pr st m a

    ClosedChannel
      :: forall ps pr st m a.
         NobodyHasAgency st
      -> a
      -> Channel ps pr st m a


-- | Duality between 'Peer' and 'Channel'.
--
-- It's could play the same role as 'Network.TypedProtocol.DriverrunPeer', but
-- this is deeper than that.
--
-- TODO: every application that is written in the form of a 'Peer' can be also
-- written by means of 'Channel'.  I would like to make this precise.
--
peerChannelDuality :: forall ps pr st m a b.
            ( Protocol ps
            , Monad m
            )
         => Peer ps pr st m a
         -> Channel ps pr st m b
         -> m (a, b)

peerChannelDuality (Effect mnext) channel = mnext >>= \next -> peerChannelDuality next channel

peerChannelDuality (Done _ a) (ClosedChannel _ b) = pure (a, b)

peerChannelDuality (Done tokDone _) (SendChannel (ClientAgency tokClient) _) =
    case exclusionLemma_NobodyAndClientHaveAgency tokDone tokClient of {}

peerChannelDuality (Done tokDone _) (SendChannel (ServerAgency tokServer) _) =
    case exclusionLemma_NobodyAndServerHaveAgency tokDone tokServer of {}

peerChannelDuality (Done tokDone _) (RecvChannel (ClientAgency tokClient) _) =
    case exclusionLemma_NobodyAndClientHaveAgency tokDone tokClient of {}

peerChannelDuality (Done tokDone _) (RecvChannel (ServerAgency tokServer) _) =
    case exclusionLemma_NobodyAndServerHaveAgency tokDone tokServer of {}

peerChannelDuality (Yield _ msg next) (SendChannel _ send) = send msg >>= peerChannelDuality next

peerChannelDuality (Yield (ClientAgency tokClient) _ _) (RecvChannel (ServerAgency tokServer) _) =
    case exclusionLemma_ClientAndServerHaveAgency tokClient tokServer of {}

peerChannelDuality (Yield (ServerAgency tokServer) _  _) (RecvChannel (ClientAgency tokClient) _) =
    case exclusionLemma_ClientAndServerHaveAgency tokClient tokServer of {}

peerChannelDuality (Yield (ClientAgency tokClient) _ _) (ClosedChannel tokDone _) =
    case exclusionLemma_NobodyAndClientHaveAgency tokDone tokClient of {}

peerChannelDuality (Yield (ServerAgency tokServer) _ _) (ClosedChannel tokDone _) =
    case exclusionLemma_NobodyAndServerHaveAgency tokDone tokServer of {}

peerChannelDuality (Await _ k) (RecvChannel _ recv) = do
    (msg, channel) <- recv
    peerChannelDuality (k msg) channel

peerChannelDuality (Await (ClientAgency tokClient) _) (SendChannel (ServerAgency tokServer) _) =
    case exclusionLemma_ClientAndServerHaveAgency tokClient tokServer of {}

peerChannelDuality (Await (ServerAgency tokServer) _) (SendChannel (ClientAgency tokClient) _) =
    case exclusionLemma_ClientAndServerHaveAgency tokClient tokServer of {}

peerChannelDuality (Await (ClientAgency tokClient) _) (ClosedChannel tokDone _) =
    case exclusionLemma_NobodyAndClientHaveAgency tokDone tokClient of {}

peerChannelDuality (Await (ServerAgency tokServer) _) (ClosedChannel tokDone _) =
    case exclusionLemma_NobodyAndServerHaveAgency tokDone tokServer of {}

--
-- PingPong protocol
--

data PingPongSt where
    PingSt :: PingPongSt
    PongSt :: PingPongSt
    DoneSt :: PingPongSt

instance Protocol PingPongSt where

  data Message PingPongSt (from :: PingPongSt) (to :: PingPongSt) where
      MsgPing :: Message PingPongSt PingSt PongSt
      MsgPong :: Message PingPongSt PongSt PingSt
      MsgDone :: Message PingPongSt PingSt DoneSt

  data ClientHasAgency (st :: PingPongSt) where
      TokPing :: ClientHasAgency PingSt

  data ServerHasAgency (st :: PingPongSt) where
      TokPong :: ServerHasAgency PongSt

  data NobodyHasAgency (st :: PingPongSt) where
      TokDone :: NobodyHasAgency DoneSt

  exclusionLemma_ClientAndServerHaveAgency TokPing tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


clientApp :: forall m a.
             Monad m
          => Channel PingPongSt AsClient PingSt m a
          -> m a
clientApp = sender
  where
    sender   :: Channel PingPongSt AsClient PingSt m a
             -> m a
    receiver :: Channel PingPongSt AsClient PongSt m a
             -> m a

    sender (SendChannel (ClientAgency TokPing) send) = do
      channel <- send MsgPing
      receiver channel

    sender (RecvChannel (ServerAgency tok) _) = case tok of {}

    sender (ClosedChannel tok _)              = case tok of {}

    receiver (RecvChannel (ServerAgency TokPong) recv) = do
      r <- recv
      case r of
        (MsgPong, channel) -> sender channel

    receiver (SendChannel (ClientAgency tok) _) = case tok of {}

    receiver (ClosedChannel tok _)              = case tok of {}


serverApp :: forall m a.
             Monad m
          => Channel PingPongSt AsServer PingSt m a
          -> m a
serverApp = receiver
  where
    receiver :: Channel PingPongSt AsServer PingSt m a
             -> m a
    sender   :: Channel PingPongSt AsServer PongSt m a
             -> m a

    receiver (RecvChannel (ClientAgency TokPing) recv) = do
      r <- recv
      case r of
        (MsgPing, channel)                          -> sender channel

        -- client closed the channel
        (MsgDone, ClosedChannel TokDone a)          -> pure a

        (MsgDone, SendChannel (ServerAgency tok) _) -> case tok of {}

        (MsgDone, RecvChannel (ClientAgency tok) _) -> case tok of {}

    receiver (SendChannel (ServerAgency tok) _) = case tok of {}

    receiver (ClosedChannel tok _) = case tok of {}

    sender (SendChannel (ServerAgency TokPong) send) = do
      channel <- send MsgPong
      receiver channel

    sender (RecvChannel (ClientAgency tok) _) = case tok of {}

    sender (ClosedChannel tok _)              = case tok of {}
