{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}

module Network.TypedProtocol.PingPong.Type where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec hiding (Done)
import qualified Network.TypedProtocol.Pipelined as Pipelined
import           Network.TypedProtocol.Driver


-- | States in the ping pong system.
data PingPongState where
  StIdle :: PingPongState
  StBusy :: PingPongState
  StDone :: PingPongState

instance Protocol PingPongState where

  -- | We have to explain to the framework what our states mean, in terms of
  -- who is expected to send and receive in the different states.
  --
  -- Idle states are where it is for the client to send a message,
  -- busy states are where the server is expected to send a reply.
  --
  type AgencyInState StIdle = ClientHasAgency
  type AgencyInState StBusy = ServerHasAgency
  type AgencyInState StDone = NobodyHasAgency

  -- | The actual messages in our protocol.
  --
  -- These involve transitions between different states within the 'StPingPong'
  -- states. A ping request goes from idle to busy, and a pong response go from
  -- busy to idle.
  --
  -- This example is so simple that we have all the messages directly as
  -- constructors within this type. In more complex cases it may be better to
  -- factor all (or related) requests and all responses within one case (in
  -- which case the state transitions may depend on the particular message via
  -- the usual GADT tricks).
  --
  data Message from to where
    MsgPing :: Message StIdle StBusy
    MsgPong :: Message StBusy StIdle
    MsgDone :: Message StIdle StDone

  data StateToken st where
    TokIdle :: StateToken StIdle
    TokBusy :: StateToken StBusy
    TokDone :: StateToken StDone


instance Show (Message (from :: PingPongState) (to :: PingPongState)) where
  show MsgPing = "MsgPing"
  show MsgPong = "MsgPong"
  show MsgDone = "MsgDone"

pingPongClientFlood :: Peer AsClient StIdle m a
pingPongClientFlood =
    Yield MsgPing $
    Await TokBusy $ \MsgPong -> pingPongClientFlood

pingPongServerStandard :: Peer AsServer StIdle m ()
pingPongServerStandard =
    Await TokIdle $ \msg ->
    case msg of
      MsgPing -> Yield MsgPong pingPongServerStandard
      MsgDone -> Done ()

decodePingPongMessage :: forall (st :: PingPongState).
                         StateToken st
                      -> String
                      -> Maybe (SomeMessage st)
decodePingPongMessage TokIdle "ping" = Just (SomeMessage MsgPing)
decodePingPongMessage TokIdle "done" = Just (SomeMessage MsgDone)
decodePingPongMessage TokBusy "pong" = Just (SomeMessage MsgPong)
decodePingPongMessage _       _      = Nothing

example2 :: Monad m => m (Maybe ())
example2 = runPeerWithCodec decodePingPongMessage input pingPongServerStandard
  where
    input = ["ping", "done"]

pingPongClientPipelined :: Int -> Pipelined.PeerSender AsClient StIdle IO ()
pingPongClientPipelined 0 =
    Pipelined.Yield
      MsgDone
      (Pipelined.Completed)
      (Pipelined.Done ())

pingPongClientPipelined n =
    Pipelined.Yield
      MsgPing
      (Pipelined.Await TokBusy $ \MsgPong ->
         Pipelined.Effect' $ do
           print "pong"
           return Pipelined.Completed)
      (pingPongClientPipelined (n-1))

example3 :: IO (Maybe ())
example3 =
    runPipelinedPeerWithCodec
      decodePingPongMessage
      input
      (pingPongClientPipelined 2)
  where
    input = ["pong", "pong"]
