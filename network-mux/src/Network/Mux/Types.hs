{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Network.Mux.Types (
      MiniProtocolDispatch (..)
    , MiniProtocolLimits (..)
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MiniProtocolInitiatorControlTable (..)
    , MiniProtocolResponderControlTable (..)
    , MiniProtocolInitiatorControl (..)
    , MiniProtocolResponderControl (..)
    , MuxBearer (..)
    , muxBearerAsControlChannel
    , MuxBearerState (..)
    , MuxError (..)
    , MuxErrorType (..)
    , handleIOException
    , MuxSDU (..)
    , MuxSDUHeader (..)
    , MuxTrace (..)
    , PerMuxSharedState (..)
    , RemoteClockModel (..)
    , remoteClockPrecision
    , TranslocationServiceRequest (..)
    , Wanton (..)
    , WithMuxBearer (..)
    ) where

import           Prelude hiding (read)

import           Control.Exception
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           Data.Functor (void)
import           Data.Int
import           Data.Ix (Ix (..))
import           Data.Word
import           GHC.Stack
import           Text.Printf

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime

import           Network.TypedProtocol.Channel (Channel(Channel))
import qualified Network.TypedProtocol.Channel as Channel

newtype RemoteClockModel
  = RemoteClockModel { unRemoteClockModel :: Word32 }
  deriving (Eq, Bounded)

-- | The `DiffTime` represented by a tick in the `RemoteClockModel`
remoteClockPrecision :: DiffTime
remoteClockPrecision = 1e-6

--
-- Mini-protocol numbers
--

-- | The wire format includes the protocol numbers, and it's vital that these
-- are stable. They are not necessarily dense however, as new ones are added
-- and some old ones retired. So we use a dedicated class for this rather than
-- reusing 'Enum'. This also covers unrecognised protocol numbers on the
-- decoding side.
--
-- Note: the values @0@ and @1@ are reserved for 'Muxcontrol' and 'DeltaQ'
-- messages.
--
class ProtocolEnum ptcl where

    fromProtocolEnum :: ptcl -> Word16
    toProtocolEnum   :: Word16 -> Maybe ptcl


-- | The Ids of mini-protocols that the mux manages. This is expected to be
-- used with custom bounded enumerations that list all the mini-protocols in
-- that instance of the overall muxed protocol.
--
-- It is necessary to have 'Ord', 'Enum' and 'Bounded' instances. For example:
--
-- > data NodeToClient = ChainSyncWithBlocks
-- >                   | ClientRPC
-- >   deriving (Eq, Ord, Enum, Bounded, Show)
--
data MiniProtocolId ptcl = Muxcontrol
                         -- ^ indicates 'MuxSDU's which belong to initial
                         -- handshake using the 'Hanshake' protocol.
                         | DeltaQ
                         | AppProtocolId ptcl
                    deriving (Eq, Ord, Show)

-- | Shift the inner enumeration up by two, to account for the two mux built-in
-- mini-protocols. This determines the final wire format for the protocol
-- numbers.
--
instance ProtocolEnum ptcl => ProtocolEnum (MiniProtocolId ptcl) where
  fromProtocolEnum Muxcontrol           = 0
  fromProtocolEnum DeltaQ               = 1
  fromProtocolEnum (AppProtocolId ptcl) = fromProtocolEnum ptcl

  toProtocolEnum 0 = Just Muxcontrol
  toProtocolEnum 1 = Just DeltaQ
  toProtocolEnum n = AppProtocolId <$> toProtocolEnum n

-- | Shift the inner enumeration up by two, to account for the two mux built-in
-- mini-protocols. This instance is never used for the wire format, just for
-- internal purposes such as dispatch tables.
--
instance Enum ptcl => Enum (MiniProtocolId ptcl) where
  fromEnum Muxcontrol          = 0
  fromEnum DeltaQ              = 1
  fromEnum (AppProtocolId pid) = 2 + fromEnum pid

  toEnum 0 = Muxcontrol
  toEnum 1 = DeltaQ
  toEnum n = AppProtocolId (toEnum (n-2))

-- | We will be indexing arrays by the mini-protocol id, so need @Ix@.
--
instance (Ord ptcl, Enum ptcl) => Ix (MiniProtocolId ptcl) where
  range   (from, to)       = enumFromTo from to
  inRange (from, to) x     = x >= from && x <= to
  index   (from, to) x
    | inRange (from, to) x = fromEnum x - fromEnum from
    | otherwise            = throw (IndexOutOfBounds msg)
       where msg = "not inRange " ++ show (fromEnum from, fromEnum to)
                           ++ " " ++ show (fromEnum x)

-- | Need bounds to be able to construct arrays of mini-protocols.
--
instance Bounded ptcl => Bounded (MiniProtocolId ptcl) where
  minBound = Muxcontrol
  maxBound = AppProtocolId maxBound

-- | Per Miniprotocol limits
-- maximumIngressQueue must be >= maximumMessageSize
class MiniProtocolLimits ptcl where
    -- | Limit on the maximum message that can be sent over a given miniprotocol.
    maximumMessageSize :: ptcl -> Int64
    -- | Limit on the maximum number of bytes that can be queued in the miniprotocol's ingress queue.
    maximumIngressQueue :: ptcl -> Int64

instance (MiniProtocolLimits ptcl) => MiniProtocolLimits (MiniProtocolId ptcl) where
    maximumMessageSize Muxcontrol = 0xffff
    maximumMessageSize DeltaQ     = 0xffff
    maximumMessageSize (AppProtocolId pid) = maximumMessageSize pid

    maximumIngressQueue Muxcontrol = 0xffff
    maximumIngressQueue DeltaQ     = 0xffff
    maximumIngressQueue (AppProtocolId pid) = maximumIngressQueue pid

-- | Control Interface for the Initiator side of Miniprotocols.
-- The outer STM action is used to signal that the miniprotocol should start.
-- The inner STM action provides the result of the miniprotocol.
newtype MiniProtocolInitiatorControl m a = MiniProtocolInitiatorControl (STM m (STM m a))

-- | STM action for reading the result of the responder side of a Miniprotocol.
newtype MiniProtocolResponderControl m b = MiniProtocolResponderControl (STM m b)

--
-- Mux internal types
--

newtype MiniProtocolDispatch ptcl m =
        MiniProtocolDispatch (Array (MiniProtocolId ptcl, MiniProtocolMode)
                                    (StrictTVar m BL.ByteString))

newtype MiniProtocolInitiatorControlTable ptcl m a =
        MiniProtocolInitiatorControlTable (Array (MiniProtocolId ptcl)
                                                 (StrictTMVar m (), StrictTMVar m a))
newtype MiniProtocolResponderControlTable ptcl m b =
        MiniProtocolResponderControlTable (Array (MiniProtocolId ptcl)
                                                 (StrictTMVar m b))

data MiniProtocolMode = ModeInitiator | ModeResponder
  deriving (Eq, Ord, Ix, Enum, Bounded, Show)

data MuxSDU ptcl = MuxSDU {
      msTimestamp :: !RemoteClockModel
    , msId        :: !(MiniProtocolId ptcl)
    , msMode      :: !MiniProtocolMode
    , msLength    :: !Word16
    , msBlob      :: !BL.ByteString
    }

data MuxSDUHeader = MuxSDUHeader {
      mshTimestamp :: !RemoteClockModel
    , mshIdAndMode :: !Word16
    , mshLength    :: !Word16
    }

-- | A TranslocationServiceRequest is a demand for the translocation
--  of a single mini-protocol message. This message can be of
--  arbitrary (yet bounded) size. This multiplexing layer is
--  responsible for the segmentation of concrete representation into
--  appropriate SDU's for onward transmission.
data TranslocationServiceRequest ptcl m
  = TLSRDemand (MiniProtocolId ptcl) MiniProtocolMode (Wanton m)

-- | A Wanton represent the concrete data to be translocated, note that the
--  TMVar becoming empty indicates -- that the last fragment of the data has
--  been enqueued on the -- underlying bearer.
newtype Wanton m = Wanton { want :: StrictTMVar m BL.ByteString }

-- | Each peer's multiplexer has some state that provides both
-- de-multiplexing details (for despatch of incoming mesages to mini
-- protocols) and for dispatching incoming SDUs.  This is shared
-- between the muxIngress and the bearerIngress processes.
data PerMuxSharedState ptcl m = PerMuxSS {
  -- | Ingress dispatch table, fixed and known at instantiation.
    dispatchTable :: MiniProtocolDispatch ptcl m
  -- | Egress queue, shared by all miniprotocols
  , tsrQueue      :: TBQueue m (TranslocationServiceRequest ptcl m)
  , bearer        :: MuxBearer ptcl m
  }

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


-- | Low level access to underlying socket or pipe.  There are three smart
-- constructors:
--
-- * 'Network.Socket.socketAsMuxBearer'
-- * 'Network.Pipe.pipeAsMuxBearer'
-- * @Test.Mux.queuesAsMuxBearer@
--
data MuxBearer ptcl m = MuxBearer {
    -- | Timestamp and send MuxSDU.
      write   :: MuxSDU ptcl -> m Time
    -- | Read a MuxSDU
    , read    :: m (MuxSDU ptcl, Time)
    -- | Return a suitable MuxSDU payload size.
    , sduSize :: m Word16
    , state   :: StrictTVar m MuxBearerState
    }


-- | A channel which wraps each message as an 'MuxSDU', each sdu is send as
-- 'Mx.Muxcontrol'.
--
muxBearerAsControlChannel
  :: forall ptcl.
     ProtocolEnum ptcl
  => MuxBearer ptcl IO
  -> MiniProtocolMode
  -> Channel IO BL.ByteString
muxBearerAsControlChannel bearer mode = Channel {
        Channel.send = send,
        Channel.recv = recv
      }
    where
      send blob = void $ write bearer (wrap blob)
      recv = Just . msBlob . fst <$> read bearer

      -- wrap a 'ByteString' as 'MuxSDU'
      wrap :: BL.ByteString -> MuxSDU ptcl
      wrap blob = MuxSDU {
            -- it will be filled when the 'MuxSDU' is send by the 'bearer'
            msTimestamp = RemoteClockModel 0,
            msId = Muxcontrol,
            msMode = mode,
            msLength = fromIntegral $ BL.length blob,
            msBlob = blob
          }


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
                  | MuxTooLargeMessage
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

-- Type used for tracing mux events.
--
data WithMuxBearer peerid a = WithMuxBearer {
      wmbPeerId :: !peerid
      -- ^ A tag that should identify a specific mux bearer.
    , wmbEvent  :: !a
}

instance (Show a, Show peerid) => Show (WithMuxBearer peerid a) where
    show WithMuxBearer {wmbPeerId, wmbEvent} = printf "Mux %s %s" (show wmbPeerId) (show wmbEvent)

-- | Enumeration of Mux events that can be traced.
--
data MuxTrace ptcl =
      MuxTraceRecvHeaderStart
    | MuxTraceRecvHeaderEnd (MuxSDU ptcl)
    | MuxTraceRecvPayloadStart Int
    | MuxTraceRecvPayloadEnd BL.ByteString
    | MuxTraceRecvDeltaQObservation (MuxSDU ptcl) Time
    | MuxTraceRecvDeltaQSample Double Int Int Double Double Double Double String
    | MuxTraceRecvStart Int
    | MuxTraceRecvEnd BL.ByteString
    | MuxTraceSendStart (MuxSDU ptcl)
    | MuxTraceSendEnd
    | MuxTraceStateChange MuxBearerState MuxBearerState
    | MuxTraceCleanExit String
    | MuxTraceExceptionExit SomeException String
    | MuxTraceChannelRecvStart (MiniProtocolId ptcl)
    | MuxTraceChannelRecvEnd (MiniProtocolId ptcl) BL.ByteString
    | MuxTraceChannelSendStart (MiniProtocolId ptcl) BL.ByteString
    | MuxTraceChannelSendEnd (MiniProtocolId ptcl)
    | MuxTraceHandshakeStart
    | MuxTraceHandshakeClientEnd DiffTime
    | MuxTraceHandshakeServerEnd
    | forall e. Exception e => MuxTraceHandshakeClientError e DiffTime
    | forall e. Exception e => MuxTraceHandshakeServerError e
    | MuxTraceBrooderResponderBrood (MiniProtocolId ptcl)
    | MuxTraceBrooderResponderHatch (MiniProtocolId ptcl)
    | MuxTraceBrooderResponderDone (MiniProtocolId ptcl) Int64
    | MuxTraceBrooderInitiatorBrood (MiniProtocolId ptcl)
    | MuxTraceBrooderInitiatorHatch (MiniProtocolId ptcl)
    | MuxTraceBrooderInitiatorDone (MiniProtocolId ptcl) Int64

instance Show ptcl => Show (MuxTrace ptcl) where
    show MuxTraceRecvHeaderStart = printf "Bearer Receive Header Start"
    show (MuxTraceRecvHeaderEnd sdu) = printf "Bearer Receive Header End: ts: 0x%08x %s %s len %d"
        (unRemoteClockModel $ msTimestamp sdu)
        (show $ msId sdu)
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
        (show $ msId sdu)
        (show $ msMode sdu)
        (BL.length $ msBlob sdu)
    show MuxTraceSendEnd = printf "Bearer Send End"
    show (MuxTraceStateChange old new) = printf "State Change: %s -> %s" (show old) (show new)
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
    show (MuxTraceBrooderResponderBrood mid) = printf "Responder Brood on %s" (show mid)
    show (MuxTraceBrooderResponderHatch mid) = printf "Responder Hatch on %s" (show mid)
    show (MuxTraceBrooderResponderDone mid len) = printf "Responder Done on %s %d bytes left" (show mid) len
    show (MuxTraceBrooderInitiatorBrood mid) = printf "Initiator Brood on %s" (show mid)
    show (MuxTraceBrooderInitiatorHatch mid) = printf "Initiator Hatch on %s" (show mid)
    show (MuxTraceBrooderInitiatorDone mid len) = printf "Initiator Done on %s  %d bytes left" (show mid) len

