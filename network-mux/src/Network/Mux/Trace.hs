{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE DeriveFunctor             #-}

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

import qualified Data.ByteString.Lazy as BL
import           Text.Printf

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Exception
import           GHC.Stack

import           Network.Mux.Types


--
-- Errors
--

-- | Error type used in accross the mux layer.
--
data MuxError = MuxError {
      errorType  :: !MuxErrorType
    , errorMsg   :: !String
    , errorStack :: !CallStack
    } deriving Show


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
                  | MuxControlProtocolError
                  -- ^ thrown by 'muxControl' (mux control thread), when
                  -- received a 'Muxcontrol' message on a mature 'MuxBearer'.
                  |  MuxTooLargeMessage
                  -- ^ thrown by 'muxChannel' when violationg
                  -- 'maximumMessageSize' byte limit.
                  | MuxIOException IOException
                  -- ^ 'IOException' thrown by 
                  deriving (Show, Eq)

instance Exception MuxError where
    displayException MuxError{errorType, errorMsg, errorStack}
      = printf "%s %s at %s"
          (show errorType)
          (show errorMsg)
          (prettyCallStack errorStack)

-- | Handler for 'IOException's which wrappes them in 'MuxError'.
--
-- It is used various 'MuxBearer' implementations:
-- * 'socketAsMuxBearer'
-- * 'pipeAsMuxBearer'
--
handleIOException :: MonadThrow m => String -> IOException -> m a
handleIOException errorMsg e = throwM MuxError {
    errorType  = MuxIOException e,
    errorMsg   = '(' : errorMsg ++ ")",
    errorStack = callStack 
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
--TODO: probably remove this type

instance (Show a, Show peerid) => Show (WithMuxBearer peerid a) where
    show WithMuxBearer {wmbPeerId, wmbEvent} = printf "Mux %s %s" (show wmbPeerId) (show wmbEvent)


data MuxBearerState = Larval
                    -- ^ Newly created MuxBearer.
                    | Connected
                    -- ^ MuxBearer is connected to a peer.
                    | Mature
                    -- ^ MuxBearer has successufully completed the handshake.
                    | Dying
                    -- ^ MuxBearer is in the process of beeing torn down,
                    -- requests may fail.
                    | Dead
                    -- ^ MuxBearer is dead and the underlying bearer has been
                    -- closed.
                    deriving (Eq, Show)

-- | Enumeration of Mux events that can be traced.
--
data MuxTrace =
      MuxTraceRecvHeaderStart
    | MuxTraceRecvHeaderEnd MuxSDU
    | MuxTraceRecvPayloadStart Int
    | MuxTraceRecvPayloadEnd BL.ByteString
    | MuxTraceRecvDeltaQObservation MuxSDU Time
    | MuxTraceRecvDeltaQSample Double Int Int Double Double Double Double String
    | MuxTraceRecvStart Int
    | MuxTraceRecvEnd BL.ByteString
    | MuxTraceSendStart MuxSDU
    | MuxTraceSendEnd
    | MuxTraceState MuxBearerState
    | MuxTraceCleanExit String
    | MuxTraceExceptionExit SomeException String
    | MuxTraceChannelRecvStart MiniProtocolNum
    | MuxTraceChannelRecvEnd MiniProtocolNum BL.ByteString
    | MuxTraceChannelSendStart MiniProtocolNum BL.ByteString
    | MuxTraceChannelSendEnd MiniProtocolNum
    | MuxTraceHandshakeStart
    | MuxTraceHandshakeClientEnd DiffTime
    | MuxTraceHandshakeServerEnd
    | forall e. Exception e => MuxTraceHandshakeClientError e DiffTime
    | forall e. Exception e => MuxTraceHandshakeServerError e

instance Show MuxTrace where
    show MuxTraceRecvHeaderStart = printf "Bearer Receive Header Start"
    show (MuxTraceRecvHeaderEnd sdu) = printf "Bearer Receive Header End: ts: 0x%08x %s %s len %d"
        (unRemoteClockModel $ msTimestamp sdu)
        (show $ msNum sdu)
        (show $ msMode sdu)
        (msLength sdu)
    show (MuxTraceRecvPayloadStart len) = printf "Bearer Receive Body Start: length %d" len
    show (MuxTraceRecvPayloadEnd blob) = printf "Bearer Receive Body End: length %d" (BL.length blob)
    show (MuxTraceRecvDeltaQObservation sdu ts) = printf "Bearer DeltaQ observation: remote ts %d local ts %s length %d"
        (unRemoteClockModel $ msTimestamp sdu)
        (show ts)
        (msLength sdu)
    show (MuxTraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) = printf "Bearer DeltaQ Sample: duration %.3e packets %d sumBytes %d DeltaQ_S %.3e DeltaQ_VMean %.3e DeltaQ_VVar %.3e DeltaQ_estR %.3e sizeDist %s"
         d sp so dqs dqvm dqvs estR sdud
    show (MuxTraceRecvStart len) = printf "Bearer Receive Start: length %d" len
    show (MuxTraceRecvEnd blob) = printf "Bearer Receive End: length %d" (BL.length blob)
    show (MuxTraceSendStart sdu) = printf "Bearer Send Start: ts: 0x%08x %s %s length %d"
        (unRemoteClockModel $ msTimestamp sdu)
        (show $ msNum sdu)
        (show $ msMode sdu)
        (BL.length $ msBlob sdu)
    show MuxTraceSendEnd = printf "Bearer Send End"
    show (MuxTraceState new) = printf "State: %s" (show new)
    show (MuxTraceCleanExit mp) = printf "Miniprotocol %s triggered clean exit" mp
    show (MuxTraceExceptionExit e mp) = printf "Miniprotocol %s triggered exit with %s" mp (show e)
    show (MuxTraceChannelRecvStart mid) = printf "Channel Receive Start on %s" (show mid)
    show (MuxTraceChannelRecvEnd mid blob) = printf "Channel Receive End on %s %d" (show mid)
        (BL.length blob)
    show (MuxTraceChannelSendStart mid blob) = printf "Channel Send Start on %s %d" (show mid)
        (BL.length blob)
    show (MuxTraceChannelSendEnd mid) = printf "Channel Send End on %s" (show mid)
    show MuxTraceHandshakeStart = "Handshake start"
    show (MuxTraceHandshakeClientEnd duration) = printf "Handshake Client end, duration %s" (show duration)
    show MuxTraceHandshakeServerEnd = "Handshake Server end"
    show (MuxTraceHandshakeClientError e duration) =
         -- Client Error can include an error string from the peer which could be very large.
        printf "Handshake Client Error %s duration %s" (take 256 $ show e) (show duration)
    show (MuxTraceHandshakeServerError e) = printf "Handshake Server Error %s" (show e)

