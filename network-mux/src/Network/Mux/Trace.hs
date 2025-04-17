{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}

module Network.Mux.Trace
  ( Error (..)
  , handleIOException
  , Trace (..)
  , Tracers (..)
  , BearerState (..)
  , WithBearer (..)
  , TraceLabelPeer (..)
  ) where

import Prelude hiding (read)

import Text.Printf

import Control.Exception hiding (throwIO)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer)
import Data.Bifunctor (Bifunctor (..))
import Data.Word
import GHC.Generics (Generic (..))
import Quiet (Quiet (..))

import Network.Mux.TCPInfo
import Network.Mux.Types


--
-- Errors
--

-- | Enumeration of error conditions.
--
data Error = UnknownMiniProtocol MiniProtocolNum
           -- ^ returned by 'decodeSDUHeader', thrown by 'Bearer'.
           | BearerClosed String
           -- ^ thrown by 'Bearer' when received a null byte.
           | IngressQueueOverRun MiniProtocolNum MiniProtocolDir
           -- ^ thrown by 'demux' when violating 'maximumIngressQueue'
           -- byte limit.
           | InitiatorOnly MiniProtocolNum
           -- ^ thrown when data arrives on a responder channel when the
           -- mux was set up as an 'InitiatorApp'.
           | IOException IOException String
           -- ^ 'IOException' thrown by

           | SDUDecodeError String
           -- ^ return by 'decodeSDUHeader', thrown by 'Bearer'.
           | SDUReadTimeout
           -- ^ thrown when reading of a single SDU takes too long
           | SDUWriteTimeout
           -- ^ thrown when writing a single SDU takes too long

           | Shutdown (Maybe SomeException) Status
           -- ^ Result of runMiniProtocol's completionAction in case of
           -- an error or mux being closed while a mini-protocol was
           -- still running, this is not a clean exit.
           deriving Show

instance Exception Error where
  displayException = \case
    UnknownMiniProtocol pnum      -> printf "unknown mini-protocol %s" (show pnum)
    BearerClosed msg              -> printf "bearer closed: %s" (show msg)
    IngressQueueOverRun pnum pdir -> printf "ingress queue overrun for %s %s " (show pnum) (show pdir)
    InitiatorOnly pnum            -> printf "received data on initiator only protocol %s" (show pnum)
    IOException e msg             -> displayException e ++ ": " ++ msg
    SDUDecodeError msg            -> printf "SDU decode error: %s" msg
    SDUReadTimeout                -> "SDU read timeout expired"
    SDUWriteTimeout               -> "SDU write timeout expired"
    Shutdown Nothing st           -> printf "mux shutdown error in state %s" (show st)
    Shutdown (Just e) st          -> printf "mux shutdown error (%s) in state %s " (displayException e) (show st)

-- | Handler for 'IOException's which wraps them in 'Error'.
--
-- It is used various 'Bearer' implementations:
-- * 'socketAsBearer'
-- * 'pipeAsBearer'
--
handleIOException :: MonadThrow m => String -> IOException -> m a
handleIOException msg e = throwIO (IOException e msg)


--
-- Tracing
--

-- | A peer label for use in 'Tracer's. This annotates tracer output as being
-- associated with a given peer identifier.
--
data TraceLabelPeer peerid a = TraceLabelPeer peerid a
  deriving (Eq, Functor, Show)

instance Bifunctor TraceLabelPeer where
  bimap f g (TraceLabelPeer a b) = TraceLabelPeer (f a) (g b)

-- | Type used for tracing mux events.
--
data WithBearer peerid a = WithBearer {
      wbPeerId :: !peerid
      -- ^ A tag that should identify a specific mux bearer.
    , wbEvent  :: !a
  }
  deriving (Generic)
  deriving Show via (Quiet (WithBearer peerid a))
--TODO: probably remove this type


data BearerState = Mature
                 -- ^ `Bearer` has successfully completed the handshake.
                 | Dead
                 -- ^ `Bearer` is dead and the underlying bearer has been
                 -- closed.
                 deriving (Eq, Show)

-- todo The Trace type mixes tags which are output by
-- separate components but share the type. It would make more sense
-- to break this up into separate types. Care must be
-- excercised to ensure that a particular tracer goes
-- into the component that outputs the desired tags. For instance,
-- the low level bearer tags are not output by the tracer which
-- is passed to Mux via 'Tracers'.

-- | Enumeration of Mux events that can be traced.
--
data Trace =
    -- low level bearer trace tags (these are not traced by the tracer
    -- which is passed to Mux)
      TraceRecvHeaderStart
    | TraceRecvHeaderEnd SDUHeader
    | TraceRecvDeltaQObservation SDUHeader Time
    | TraceRecvDeltaQSample Double Int Int Double Double Double Double String
    | TraceRecvRaw Int
    | TraceRecvStart Int
    | TraceRecvEnd Int
    | TraceSendStart SDUHeader
    | TraceSendEnd
    | TraceState BearerState
    | TraceSDUReadTimeoutException
    | TraceSDUWriteTimeoutException
    | TraceTCPInfo StructTCPInfo Word16
    -- mid level channel tags traced independently by each mini protocol
    -- job in Mux, for each complete message, by the 'channelTracer'
    -- within 'Tracers'
    | TraceChannelRecvStart MiniProtocolNum
    | TraceChannelRecvEnd MiniProtocolNum Int
    | TraceChannelSendStart MiniProtocolNum Int
    | TraceChannelSendEnd MiniProtocolNum
    -- high level Mux tags traced by the main Mux/Connection handler
    -- thread forked by CM. These may be monitored by the inbound
    -- governor information channel tracer. These should be traced
    -- by muxTracer of 'Tracers' and their ordering
    -- is significant at call sites or bad things will happen.
    -- You have been warned.
    | TraceCleanExit MiniProtocolNum MiniProtocolDir
    | TraceExceptionExit MiniProtocolNum MiniProtocolDir SomeException
    | TraceStartEagerly MiniProtocolNum MiniProtocolDir
    | TraceStartOnDemand MiniProtocolNum MiniProtocolDir
    | TraceStartOnDemandAny MiniProtocolNum MiniProtocolDir
    | TraceStartedOnDemand MiniProtocolNum MiniProtocolDir
    | TraceTerminating MiniProtocolNum MiniProtocolDir
    | TraceStopping
    | TraceStopped

instance Show Trace where
    show TraceRecvHeaderStart = printf "Bearer Receive Header Start"
    show (TraceRecvHeaderEnd SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = printf "Bearer Receive Header End: ts: 0x%08x (%s) %s len %d"
        (unRemoteClockModel mhTimestamp) (show mhNum) (show mhDir) mhLength
    show (TraceRecvDeltaQObservation SDUHeader { mhTimestamp, mhLength } ts) = printf "Bearer DeltaQ observation: remote ts %d local ts %s length %d"
        (unRemoteClockModel mhTimestamp) (show ts) mhLength
    show (TraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) = printf "Bearer DeltaQ Sample: duration %.3e packets %d sumBytes %d DeltaQ_S %.3e DeltaQ_VMean %.3e DeltaQ_VVar %.3e DeltaQ_estR %.3e sizeDist %s"
         d sp so dqs dqvm dqvs estR sdud
    show (TraceRecvRaw len) = printf "Bearer Receive Raw: length %d" len
    show (TraceRecvStart len) = printf "Bearer Receive Start: length %d" len
    show (TraceRecvEnd len) = printf "Bearer Receive End: length %d" len
    show (TraceSendStart SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = printf "Bearer Send Start: ts: 0x%08x (%s) %s length %d"
        (unRemoteClockModel mhTimestamp) (show mhNum) (show mhDir) mhLength
    show TraceSendEnd = printf "Bearer Send End"
    show (TraceState new) = printf "State: %s" (show new)
    show (TraceCleanExit mid dir) = printf "Miniprotocol (%s) %s terminated cleanly" (show mid) (show dir)
    show (TraceExceptionExit mid dir e) = printf "Miniprotocol %s %s terminated with exception %s" (show mid) (show dir) (show e)
    show (TraceChannelRecvStart mid) = printf "Channel Receive Start on %s" (show mid)
    show (TraceChannelRecvEnd mid len) = printf "Channel Receive End on (%s) %d" (show mid)
        len
    show (TraceChannelSendStart mid len) = printf "Channel Send Start on (%s) %d" (show mid)
        len
    show (TraceChannelSendEnd mid) = printf "Channel Send End on %s" (show mid)
    show TraceSDUReadTimeoutException = "Timed out reading SDU"
    show TraceSDUWriteTimeoutException = "Timed out writing SDU"
    show (TraceStartEagerly mid dir) = printf "Eagerly started (%s) in %s" (show mid) (show dir)
    show (TraceStartOnDemand mid dir) = printf "Preparing to start (%s) in %s" (show mid) (show dir)
    show (TraceStartOnDemandAny mid dir) = printf "Preparing to start on any (%s) in %s" (show mid) (show dir)
    show (TraceStartedOnDemand mid dir) = printf "Started on demand (%s) in %s" (show mid) (show dir)
    show (TraceTerminating mid dir) = printf "Terminating (%s) in %s" (show mid) (show dir)
    show TraceStopping = "Mux stopping"
    show TraceStopped  = "Mux stoppped"
#ifdef linux_HOST_OS
    show (TraceTCPInfo StructTCPInfo
            { tcpi_snd_mss, tcpi_rcv_mss, tcpi_lost, tcpi_retrans
            , tcpi_rtt, tcpi_rttvar, tcpi_snd_cwnd }
            len)
                                     =
      printf "TCPInfo rtt %d rttvar %d cwnd %d smss %d rmss %d lost %d retrans %d len %d"
        (fromIntegral tcpi_rtt :: Word) (fromIntegral tcpi_rttvar :: Word)
        (fromIntegral tcpi_snd_cwnd :: Word) (fromIntegral tcpi_snd_mss :: Word)
        (fromIntegral tcpi_rcv_mss :: Word) (fromIntegral tcpi_lost :: Word)
        (fromIntegral tcpi_retrans :: Word)
        len
#else
    show (TraceTCPInfo _ len) = printf "TCPInfo len %d" len
#endif

-- | Bundle of tracers passed to mux
-- Consult the 'Trace' type to determine which
-- tags are required/expected to be served by these tracers.
-- In principle, the channelTracer can be == muxTracer
-- but performance likely degrades in typical conditions
-- unnecessarily.
--
data Tracers m = Tracers {
  channelTracer :: Tracer m Trace,
  -- ^ a low level tracer for events emitted by a bearer. It emits events as frequently
  -- as receiving individual `SDU`s from the network.
  muxTracer     :: Tracer m Trace
  -- ^ mux events which are emitted less frequently.  It emits events which allow one
  -- to observe the current state of a mini-protocol.
  }
