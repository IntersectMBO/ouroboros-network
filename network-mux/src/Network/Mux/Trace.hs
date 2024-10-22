{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}

module Network.Mux.Trace
  ( Error (..)
  , ErrorType (..)
  , handleIOException
  , Trace (..)
  , BearerState (..)
  , WithBearer (..)
  , TraceLabelPeer (..)
  ) where

import Prelude hiding (read)

import Text.Printf

import Control.Exception hiding (throwIO)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Data.Bifunctor (Bifunctor (..))
import Data.Word
import GHC.Generics (Generic (..))
import Quiet (Quiet (..))

import Network.Mux.TCPInfo
import Network.Mux.Types


--
-- Errors
--

-- | Error type used in across the mux layer.
--
data Error = Error {
      errorType :: !ErrorType
    , errorMsg  :: !String
    }
  deriving Generic
  deriving Show via Quiet Error


-- | Enumeration of error conditions.
--
data ErrorType = UnknownMiniProtocol
               -- ^ returned by 'decodeSDUHeader', thrown by 'Bearer'.
               | DecodeError
               -- ^ return by 'decodeSDUHeader', thrown by 'Bearer'.
               | BearerClosed
               -- ^ thrown by 'Bearer' when received a null byte.
               | IngressQueueOverRun
               -- ^ thrown by 'demux' when violating 'maximumIngressQueue'
               -- byte limit.
               | InitiatorOnly
               -- ^ thrown when data arrives on a responder channel when the
               -- mux was set up as an 'InitiatorApp'.
               | IOException IOException
               -- ^ 'IOException' thrown by
               | SDUReadTimeout
               -- ^ thrown when reading of a single SDU takes too long
               | SDUWriteTimeout
               -- ^ thrown when writing a single SDU takes too long
               | Shutdown !(Maybe ErrorType)
               -- ^ Result of runMiniProtocol's completionAction in case of
               -- an error or mux being closed while a mini-protocol was
               -- still running, this is not a clean exit.
               | CleanShutdown
               -- ^ Mux stopped by 'Network.Mux.stop'
               deriving (Show, Eq)

instance Exception Error where
    displayException Error{errorType, errorMsg}
      = show errorType ++ " " ++ show errorMsg

-- | Handler for 'IOException's which wraps them in 'Error'.
--
-- It is used various 'Bearer' implementations:
-- * 'socketAsBearer'
-- * 'pipeAsBearer'
--
handleIOException :: MonadThrow m => String -> IOException -> m a
handleIOException errorMsg e = throwIO Error {
    errorType  = IOException e,
    errorMsg   = '(' : errorMsg ++ ")"
  }


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

-- | Enumeration of Mux events that can be traced.
--
data Trace =
      TraceRecvHeaderStart
    | TraceRecvHeaderEnd SDUHeader
    | TraceRecvDeltaQObservation SDUHeader Time
    | TraceRecvDeltaQSample Double Int Int Double Double Double Double String
    | TraceRecvStart Int
    | TraceRecvEnd Int
    | TraceSendStart SDUHeader
    | TraceSendEnd
    | TraceState BearerState
    | TraceCleanExit MiniProtocolNum MiniProtocolDir
    | TraceExceptionExit MiniProtocolNum MiniProtocolDir SomeException
    | TraceChannelRecvStart MiniProtocolNum
    | TraceChannelRecvEnd MiniProtocolNum Int
    | TraceChannelSendStart MiniProtocolNum Int
    | TraceChannelSendEnd MiniProtocolNum
    | TraceHandshakeStart
    | TraceHandshakeClientEnd DiffTime
    | TraceHandshakeServerEnd
    | forall e. Exception e => TraceHandshakeClientError e DiffTime
    | forall e. Exception e => TraceHandshakeServerError e
    | TraceSDUReadTimeoutException
    | TraceSDUWriteTimeoutException
    | TraceStartEagerly MiniProtocolNum MiniProtocolDir
    | TraceStartOnDemand MiniProtocolNum MiniProtocolDir
    | TraceStartedOnDemand MiniProtocolNum MiniProtocolDir
    | TraceTerminating MiniProtocolNum MiniProtocolDir
    | TraceStopping
    | TraceStopped
    | TraceTCPInfo StructTCPInfo Word16

instance Show Trace where
    show TraceRecvHeaderStart = printf "Bearer Receive Header Start"
    show (TraceRecvHeaderEnd SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = printf "Bearer Receive Header End: ts: 0x%08x (%s) %s len %d"
        (unRemoteClockModel mhTimestamp) (show mhNum) (show mhDir) mhLength
    show (TraceRecvDeltaQObservation SDUHeader { mhTimestamp, mhLength } ts) = printf "Bearer DeltaQ observation: remote ts %d local ts %s length %d"
        (unRemoteClockModel mhTimestamp) (show ts) mhLength
    show (TraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) = printf "Bearer DeltaQ Sample: duration %.3e packets %d sumBytes %d DeltaQ_S %.3e DeltaQ_VMean %.3e DeltaQ_VVar %.3e DeltaQ_estR %.3e sizeDist %s"
         d sp so dqs dqvm dqvs estR sdud
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
    show TraceHandshakeStart = "Handshake start"
    show (TraceHandshakeClientEnd duration) = printf "Handshake Client end, duration %s" (show duration)
    show TraceHandshakeServerEnd = "Handshake Server end"
    show (TraceHandshakeClientError e duration) =
         -- Client Error can include an error string from the peer which could be very large.
        printf "Handshake Client Error %s duration %s" (take 256 $ show e) (show duration)
    show (TraceHandshakeServerError e) = printf "Handshake Server Error %s" (show e)
    show TraceSDUReadTimeoutException = "Timed out reading SDU"
    show TraceSDUWriteTimeoutException = "Timed out writing SDU"
    show (TraceStartEagerly mid dir) = printf "Eagerly started (%s) in %s" (show mid) (show dir)
    show (TraceStartOnDemand mid dir) = printf "Preparing to start (%s) in %s" (show mid) (show dir)
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

