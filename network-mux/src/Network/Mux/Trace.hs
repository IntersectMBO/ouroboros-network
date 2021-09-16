{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Network.Mux.Trace (
      MuxError(..)
    , MuxErrorType(..)
    , handleIOException
    , MuxTrace(..)
    , MuxBearerState(..)
    , WithMuxBearer(..)
    , TraceLabelPeer(..)
    ) where

import           Prelude hiding (read)

import           Text.Printf

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Exception hiding (throwIO)
import           GHC.Generics (Generic (..))
import           Quiet (Quiet (..))

import           Network.Mux.Types


--
-- Errors
--

-- | Error type used in accross the mux layer.
--
data MuxError = MuxError {
      errorType  :: !MuxErrorType
    , errorMsg   :: !String
    }
  deriving Generic
  deriving Show via Quiet MuxError


-- | Enumeration of error conditions.
--
data MuxErrorType = MuxUnknownMiniProtocol
                  -- ^ returned by 'decodeMuxSDUHeader', thrown by 'MuxBearer'.
                  | MuxDecodeError
                  -- ^ return by 'decodeMuxSDUHeader', thrown by 'MuxBearer'.
                  | MuxBearerClosed
                  -- ^ thrown by 'MuxBearer' when received a null byte.
                  | MuxIngressQueueOverRun
                  -- ^ thrown by 'demux' when violating 'maximumIngressQueue'
                  -- byte limit.
                  | MuxInitiatorOnly
                  -- ^ thrown when data arrives on a responder channel when the
                  -- mux was set up as an 'InitiatorApp'.
                  | MuxIOException IOException
                  -- ^ 'IOException' thrown by 
                  | MuxSDUReadTimeout
                  -- ^ thrown when reading of a single SDU takes too long
                  | MuxSDUWriteTimeout
                  -- ^ thrown when writing a single SDU takes too long
                  | MuxShutdown !(Maybe MuxErrorType)
                  -- ^ Result of runMiniProtocol's completionAction in case of
                  -- an error or mux being closed while a mini-protocol was
                  -- still running, this is not a clean exit.
                  | MuxCleanShutdown
                  -- ^ Mux stopped by 'stopMux'
                  deriving (Show, Eq)

instance Exception MuxError where
    displayException MuxError{errorType, errorMsg}
      = (show errorType) ++ " " ++ (show errorMsg)

-- | Handler for 'IOException's which wrappes them in 'MuxError'.
--
-- It is used various 'MuxBearer' implementations:
-- * 'socketAsMuxBearer'
-- * 'pipeAsMuxBearer'
--
handleIOException :: MonadThrow m => String -> IOException -> m a
handleIOException errorMsg e = throwIO MuxError {
    errorType  = MuxIOException e,
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

-- | Type used for tracing mux events.
--
data WithMuxBearer peerid a = WithMuxBearer {
      wmbPeerId :: !peerid
      -- ^ A tag that should identify a specific mux bearer.
    , wmbEvent  :: !a
  }
  deriving (Generic)
  deriving Show via (Quiet (WithMuxBearer peerid a))
--TODO: probably remove this type


data MuxBearerState = Mature
                    -- ^ MuxBearer has successufully completed the handshake.
                    | Dead
                    -- ^ MuxBearer is dead and the underlying bearer has been
                    -- closed.
                    deriving (Eq, Show)

-- | Enumeration of Mux events that can be traced.
--
data MuxTrace =
      MuxTraceRecvHeaderStart
    | MuxTraceRecvHeaderEnd !MuxSDUHeader
    | MuxTraceRecvDeltaQObservation !MuxSDUHeader Time
    | MuxTraceRecvDeltaQSample !Double !Int !Int !Double !Double !Double !Double !String
    | MuxTraceRecvStart !Int
    | MuxTraceRecvEnd !Int
    | MuxTraceSendStart !MuxSDUHeader
    | MuxTraceSendEnd
    | MuxTraceState !MuxBearerState
    | MuxTraceCleanExit !MiniProtocolNum !MiniProtocolDir
    | MuxTraceExceptionExit !MiniProtocolNum !MiniProtocolDir !SomeException
    | MuxTraceChannelRecvStart !MiniProtocolNum
    | MuxTraceChannelRecvEnd !MiniProtocolNum !Int
    | MuxTraceChannelSendStart !MiniProtocolNum !Int
    | MuxTraceChannelSendEnd !MiniProtocolNum
    | MuxTraceHandshakeStart
    | MuxTraceHandshakeClientEnd !DiffTime
    | MuxTraceHandshakeServerEnd
    | forall e. Exception e => MuxTraceHandshakeClientError !e !DiffTime
    | forall e. Exception e => MuxTraceHandshakeServerError !e
    | MuxTraceSDUReadTimeoutException
    | MuxTraceSDUWriteTimeoutException
    | MuxTraceStartEagerly !MiniProtocolNum !MiniProtocolDir
    | MuxTraceStartOnDemand !MiniProtocolNum !MiniProtocolDir
    | MuxTraceStartedOnDemand !MiniProtocolNum !MiniProtocolDir
    | MuxTraceTerminating !MiniProtocolNum !MiniProtocolDir
    | MuxTraceShutdown

instance Show MuxTrace where
    show MuxTraceRecvHeaderStart = printf "Bearer Receive Header Start"
    show (MuxTraceRecvHeaderEnd MuxSDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = printf "Bearer Receive Header End: ts: 0x%08x (%s) %s len %d"
        (unRemoteClockModel mhTimestamp) (show mhNum) (show mhDir) mhLength
    show (MuxTraceRecvDeltaQObservation MuxSDUHeader { mhTimestamp, mhLength } ts) = printf "Bearer DeltaQ observation: remote ts %d local ts %s length %d"
        (unRemoteClockModel mhTimestamp) (show ts) mhLength
    show (MuxTraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) = printf "Bearer DeltaQ Sample: duration %.3e packets %d sumBytes %d DeltaQ_S %.3e DeltaQ_VMean %.3e DeltaQ_VVar %.3e DeltaQ_estR %.3e sizeDist %s"
         d sp so dqs dqvm dqvs estR sdud
    show (MuxTraceRecvStart len) = printf "Bearer Receive Start: length %d" len
    show (MuxTraceRecvEnd len) = printf "Bearer Receive End: length %d" len
    show (MuxTraceSendStart MuxSDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = printf "Bearer Send Start: ts: 0x%08x (%s) %s length %d"
        (unRemoteClockModel mhTimestamp) (show mhNum) (show mhDir) mhLength
    show MuxTraceSendEnd = printf "Bearer Send End"
    show (MuxTraceState new) = printf "State: %s" (show new)
    show (MuxTraceCleanExit mid dir) = printf "Miniprotocol (%s) %s terminated cleanly" (show mid) (show dir)
    show (MuxTraceExceptionExit mid dir e) = printf "Miniprotocol %s %s terminated with exception %s" (show mid) (show dir) (show e)
    show (MuxTraceChannelRecvStart mid) = printf "Channel Receive Start on %s" (show mid)
    show (MuxTraceChannelRecvEnd mid len) = printf "Channel Receive End on (%s) %d" (show mid)
        len
    show (MuxTraceChannelSendStart mid len) = printf "Channel Send Start on (%s) %d" (show mid)
        len
    show (MuxTraceChannelSendEnd mid) = printf "Channel Send End on %s" (show mid)
    show MuxTraceHandshakeStart = "Handshake start"
    show (MuxTraceHandshakeClientEnd duration) = printf "Handshake Client end, duration %s" (show duration)
    show MuxTraceHandshakeServerEnd = "Handshake Server end"
    show (MuxTraceHandshakeClientError e duration) =
         -- Client Error can include an error string from the peer which could be very large.
        printf "Handshake Client Error %s duration %s" (take 256 $ show e) (show duration)
    show (MuxTraceHandshakeServerError e) = printf "Handshake Server Error %s" (show e)
    show MuxTraceSDUReadTimeoutException = "Timed out reading SDU"
    show MuxTraceSDUWriteTimeoutException = "Timed out writing SDU"
    show (MuxTraceStartEagerly mid dir) = printf "Eagerly started (%s) in %s" (show mid) (show dir)
    show (MuxTraceStartOnDemand mid dir) = printf "Preparing to start (%s) in %s" (show mid) (show dir)
    show (MuxTraceStartedOnDemand mid dir) = printf "Started on demand (%s) in %s" (show mid) (show dir)
    show (MuxTraceTerminating mid dir) = printf "Terminating (%s) in %s" (show mid) (show dir)
    show MuxTraceShutdown = "Mux shutdown"

