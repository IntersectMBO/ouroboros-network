{-# LANGUAGE GeneralizedNewtypeDeriving#-}

module Network.Mux.Types (
      MiniProtocolLimits (..)
    , MiniProtocolNum (..)
    , MiniProtocolMode (..)
    , MuxBearer (..)
    , muxBearerAsControlChannel
    , MuxSDU (..)
    , RemoteClockModel (..)
    , remoteClockPrecision
    ) where

import           Prelude hiding (read)

import qualified Data.ByteString.Lazy as BL
import           Data.Functor (void)
import           Data.Int
import           Data.Ix (Ix (..))
import           Data.Word

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
newtype MiniProtocolNum = MiniProtocolNum Word16
  deriving (Eq, Ord, Enum, Ix, Show)

-- | Per Miniprotocol limits
-- maximumIngressQueue must be >= maximumMessageSize
data MiniProtocolLimits =
     MiniProtocolLimits {
       -- | Limit on the maximum size of an individual message that can be sent
       -- over a given miniprotocol.
       --
       maximumMessageSize :: !Int64,

       -- | Limit on the maximum number of bytes that can be queued in the
       -- miniprotocol's ingress queue.
       --
       maximumIngressQueue :: !Int64
     }


--
-- Mux internal types
--

data MiniProtocolMode = ModeInitiator | ModeResponder
  deriving (Eq, Ord, Ix, Enum, Bounded, Show)

data MuxSDU = MuxSDU {
      msTimestamp :: !RemoteClockModel
    , msNum       :: !MiniProtocolNum
    , msMode      :: !MiniProtocolMode
    , msLength    :: !Word16
    , msBlob      :: !BL.ByteString
    }


-- | Low level access to underlying socket or pipe.  There are three smart
-- constructors:
--
-- * 'Network.Socket.socketAsMuxBearer'
-- * 'Network.Pipe.pipeAsMuxBearer'
-- * @Test.Mux.queuesAsMuxBearer@
--
data MuxBearer m = MuxBearer {
    -- | Timestamp and send MuxSDU.
      write   :: MuxSDU -> m Time
    -- | Read a MuxSDU
    , read    :: m (MuxSDU, Time)
    -- | Return a suitable MuxSDU payload size.
    , sduSize :: Word16
    }


-- | A channel which wraps each message as an 'MuxSDU', each sdu is send as
-- 'Mx.Muxcontrol'.
--
muxBearerAsControlChannel
  :: MuxBearer IO
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
      wrap :: BL.ByteString -> MuxSDU
      wrap blob = MuxSDU {
            -- it will be filled when the 'MuxSDU' is send by the 'bearer'
            msTimestamp = RemoteClockModel 0,
            msNum  = MiniProtocolNum 0,
            msMode = mode,
            msLength = fromIntegral $ BL.length blob,
            msBlob = blob
          }

