{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.TypedProtocol.Channel.Session where

import           Control.Exception (Exception)
import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Driver
import qualified Network.TypedProtocol.Channel as Channel


-- | Product of message and channel which shares the same final state of the
-- message and the channel's state.
--
data SomeMessageAndChannel ps (pr :: PeerRole) (st :: ps) m where
    SomeMessageAndChannel
      :: forall ps (pr :: PeerRole) st st' m.
         Message ps st st'
      -> Channel ps pr st' m
      -> SomeMessageAndChannel ps pr st m


-- | Channel which traces session; this is an laternative to 'Peer'.
-- A channel can be in either of the three states:
--
-- * can send a message and return a new channel
-- * can receive a message and return the new channel
-- * is closed
--
data Channel ps (pr :: PeerRole) (st :: ps) m where

    SendChannel
      :: forall ps pr st m.
         WeHaveAgency pr st
      -> (forall st'. Message ps st st' -> m (Channel ps pr st' m))
      -> Channel ps pr st m

    RecvChannel
      :: forall ps pr st m.
         TheyHaveAgency pr st
      -> m (SomeMessageAndChannel ps pr st m)
      -> Channel ps pr st m

    ClosedChannel
      :: forall ps pr st m.
         NobodyHasAgency st
      -> Channel ps pr st m


-- | Singleton type for 'PeerRole'
--
data SRole (pr :: PeerRole) where
    SAsClient :: SRole AsClient
    SAsServer :: SRole AsServer


-- | Sum of three choices; thanks to exclusion lemmas in 'Protocol' class for
-- any @pr@ and @st@ at most one element of the three sum is occupied.
--
data SomebodyHasAgency pr st where
    NobodyHasAgency
      :: NobodyHasAgency st
      -> SomebodyHasAgency pr st

    TheyHaveAgency
     :: TheyHaveAgency pr st
     -> SomebodyHasAgency pr st

    WeHaveAgency
     :: WeHaveAgency pr st
     -> SomebodyHasAgency pr st


-- | This type class extends 'Protocol' type class with a single method.
-- 'starget' returns signleton token of the target state of a message.
--
class Protocol ps => STarget ps where
    starget :: SRole pr
            -> Message ps st st'
            -> SomebodyHasAgency pr st'

-- | Duality between 'Peer' and 'Channel'.
--
-- It's could play the same role as 'Network.TypedProtocol.DriverrunPeer', but
-- this is deeper than that.
--
-- TODO: every application that is written in the form of a 'Peer' can be also
-- written by means of 'Channel'.  I would like to make this precise.
--
-- This map allows to transform
-- @'Peer'    ps pr st m a@
-- to
-- @'Channel' ps pr st m a -> m a@.
--
peerChannelDuality :: forall ps pr st m a.
            ( Protocol ps
            , STarget  ps
            , Monad m
            )
         => Peer    ps pr st m a
         -> Channel ps pr st m
         -> m a

peerChannelDuality (Effect mnext) channel = mnext >>= \next -> peerChannelDuality next channel

peerChannelDuality (Done _ a) (ClosedChannel _) = pure a

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

peerChannelDuality (Yield (ClientAgency tokClient) _ _) (ClosedChannel tokDone) =
    case exclusionLemma_NobodyAndClientHaveAgency tokDone tokClient of {}

peerChannelDuality (Yield (ServerAgency tokServer) _ _) (ClosedChannel tokDone) =
    case exclusionLemma_NobodyAndServerHaveAgency tokDone tokServer of {}

peerChannelDuality (Await _ k) (RecvChannel _ recv) = do
    (SomeMessageAndChannel msg channel) <- recv
    peerChannelDuality (k msg) channel

peerChannelDuality (Await (ClientAgency tokClient) _) (SendChannel (ServerAgency tokServer) _) =
    case exclusionLemma_ClientAndServerHaveAgency tokClient tokServer of {}

peerChannelDuality (Await (ServerAgency tokServer) _) (SendChannel (ClientAgency tokClient) _) =
    case exclusionLemma_ClientAndServerHaveAgency tokClient tokServer of {}

peerChannelDuality (Await (ClientAgency tokClient) _) (ClosedChannel tokDone) =
    case exclusionLemma_NobodyAndClientHaveAgency tokDone tokClient of {}

peerChannelDuality (Await (ServerAgency tokServer) _) (ClosedChannel tokDone) =
    case exclusionLemma_NobodyAndServerHaveAgency tokDone tokServer of {}

-- | Session type application expressed in terms of a 'Channel'.
--
type ChannelApplication ps pr (st :: ps) m a = Channel ps pr st m -> m a

-- | Transform application expressed by means of a 'Peer' to
-- a 'ChannelApplication.
--
peerAsChannelApplication
    :: forall ps pr st m a.
       ( Protocol ps
       , STarget  ps
       , Monad m
       )
    => Peer               ps pr st m a
    -> ChannelApplication ps pr st m a
peerAsChannelApplication peer channel = peerChannelDuality peer channel


-- | Like 'runPeer', but for applications which are represented as
-- 'ChannelApplication' rather than 'Peer'.
--
runChannelApplication
    :: forall ps (st :: ps) pr peerid failure bytes m a.
       ( STarget  ps
       , MonadThrow m
       , Exception failure
       )
    => Tracer m (TraceSendRecv ps peerid failure)
    -> peerid
    -> Codec ps failure m bytes
    -> Channel.Channel m bytes
    -> SRole pr
    -> Either (TheyHaveAgency pr st) (WeHaveAgency pr st)
    -> ChannelApplication ps pr st m a
    -> m a
runChannelApplication tracer peerid Codec {encode, decode} channel srole stok0 app =
      app (go (case stok0 of
                  Left a -> TheyHaveAgency a
                  Right a -> WeHaveAgency a) Nothing)
    where
      go :: forall st'.
            SomebodyHasAgency pr st'
         -> Maybe bytes
         -> Channel ps pr st' m

      go (WeHaveAgency stok) trailing = SendChannel stok $ \msg -> do
        traceWith tracer (TraceSendMsg peerid (AnyMessage msg))
        Channel.send channel (encode stok msg)
        pure $ go (starget srole msg) trailing

      go (TheyHaveAgency stok) trailing = RecvChannel stok $ do
        decoder <- decode stok
        res <- runDecoderWithChannel channel trailing decoder
        case res of
          Right (SomeMessage msg, trailing') -> do
            traceWith tracer (TraceRecvMsg peerid (AnyMessage msg))
            pure $ SomeMessageAndChannel msg (go (starget srole msg) trailing')
          Left failure -> throwM failure

      go (NobodyHasAgency stok) _trailing = ClosedChannel stok


-- | Anything that can be constructed (proved), from 'ChannelApplication' can be
-- also constructed using a 'Peer' (proven).
--
--
channelApplicationImpliesPeer :: forall ps pr st m a x.
      ( Protocol ps
      , STarget  ps
      , Monad m
      )
   => (ChannelApplication ps pr st m a -> x)
   -> (Peer               ps pr st m a -> x)
channelApplicationImpliesPeer f peer = f k
  where
    k :: Channel ps pr st m -> m a
    k channel = peerChannelDuality peer channel

-- | The dual statement to 'channelApplicationImpliesPeer', it tells us some
-- thing about 'runPeer' types:
--
-- anything that could be derived from a 'runPeer' function can be derived from
-- 'Channel'
--
-- This can be see as 'runPeer' being a weak dual (in some sense) to 'Channel'.
--
runPeerImpliesChannel :: forall ps pr st m x.
        ( Protocol ps
        , STarget  ps
        , Monad m
        )
     => ((Peer   ps pr st m () -> m ()) -> x)
     -> (Channel ps pr st m             -> x)
runPeerImpliesChannel f  channel = f k
  where
    k peer = peerChannelDuality peer channel

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


instance STarget PingPongSt where
    starget SAsClient MsgPing = TheyHaveAgency (ServerAgency TokPong)
    starget SAsServer MsgPing = WeHaveAgency   (ServerAgency TokPong)
    starget SAsClient MsgPong = WeHaveAgency   (ClientAgency TokPing)
    starget SAsServer MsgPong = TheyHaveAgency (ClientAgency TokPing)
    starget _         MsgDone = NobodyHasAgency TokDone

clientApp :: forall m a.
             Monad m
          => ChannelApplication PingPongSt AsClient PingSt m a
clientApp = sender
  where
    sender   :: Channel PingPongSt AsClient PingSt m
             -> m a
    receiver :: Channel PingPongSt AsClient PongSt m
             -> m a

    sender (SendChannel (ClientAgency TokPing) send) = do
      channel <- send MsgPing
      receiver channel

    sender (RecvChannel (ServerAgency tok) _) = case tok of {}

    sender (ClosedChannel tok)                = case tok of {}

    receiver (RecvChannel (ServerAgency TokPong) recv) = do
      r <- recv
      case r of
        SomeMessageAndChannel MsgPong channel -> sender channel

    receiver (SendChannel (ClientAgency tok) _) = case tok of {}

    receiver (ClosedChannel tok)                = case tok of {}


serverApp :: forall m.
             Monad m
          => ChannelApplication PingPongSt AsServer PingSt m ()
serverApp = receiver
  where
    receiver :: Channel PingPongSt AsServer PingSt m
             -> m ()
    sender   :: Channel PingPongSt AsServer PongSt m
             -> m ()

    receiver (RecvChannel (ClientAgency TokPing) recv) = do
      r <- recv
      case r of
        SomeMessageAndChannel MsgPing channel ->
          sender channel

        -- client closed the channel
        SomeMessageAndChannel MsgDone (ClosedChannel TokDone) ->
          pure ()

        SomeMessageAndChannel MsgDone (SendChannel (ServerAgency tok) _) ->
          case tok of {}

        SomeMessageAndChannel MsgDone (RecvChannel (ClientAgency tok) _) ->
          case tok of {}

    receiver (SendChannel (ServerAgency tok) _) = case tok of {}

    receiver (ClosedChannel tok) = case tok of {}

    sender (SendChannel (ServerAgency TokPong) send) = do
      channel <- send MsgPong
      receiver channel

    sender (RecvChannel (ClientAgency tok) _) = case tok of {}

    sender (ClosedChannel tok)                = case tok of {}
