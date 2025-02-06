{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Types used by the multiplexer.
--
module Network.Mux.Types
  ( MiniProtocolInfo (..)
  , MiniProtocolNum (..)
  , MiniProtocolDirection (..)
  , MiniProtocolLimits (..)
  , Mode (..)
  , HasInitiator
  , HasResponder
  , Status (..)
  , IngressQueue
  , MiniProtocolIx
  , MiniProtocolDir (..)
  , protocolDirEnum
  , MiniProtocolState (..)
  , MiniProtocolStatus (..)
  , Bearer (..)
  , bearerAsChannel
  , SDU (..)
  , SDUHeader (..)
  , SDUSize (..)
  , msTimestamp
  , setTimestamp
  , msNum
  , msDir
  , msLength
  , msHeaderLength
  , RemoteClockModel (..)
  , remoteClockPrecision
  , RuntimeError (..)
  , ReadBuffer (..)
  ) where

import Prelude hiding (read)

import Control.Exception (Exception, SomeException)
import Data.ByteString.Lazy qualified as BL
import Data.Functor (void)
import Data.Int
import Data.Ix (Ix (..))
import Data.Word
import Foreign.Ptr (Ptr)
import Quiet

import GHC.Generics (Generic)

import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import Control.Monad.Class.MonadTime.SI

import Network.Mux.Channel (ByteChannel, Channel (..))
import Network.Mux.Timeout (TimeoutFn)


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
newtype MiniProtocolNum = MiniProtocolNum Word16
  deriving (Eq, Ord, Enum, Ix, Show)

-- | Per Miniprotocol limits
newtype MiniProtocolLimits =
     MiniProtocolLimits {
       -- | Limit on the maximum number of bytes that can be queued in the
       -- miniprotocol's ingress queue.
       --
       maximumIngressQueue :: Int
     }

-- $interface
--
-- To run a node you will also need a bearer and a way to run a server, see
--
-- * @'Ouroboros.Network.Socket'@ module provides a socket based bearer and
--   a server that accepts connections and allows to connect to remote peers.
--
-- * @'Ouroboros.Network.Pipe'@ module provides a pipe based bearer with
--   a function that runs the mux layer on it.
--

-- | Statically configured multiplexer mode.
--
data Mode where
    -- | Only execute initiator protocols.  In this mode the multiplexer will
    -- only run its egress side.
    InitiatorMode          :: Mode
    -- | Only execute responder protocols.  It this mode the multiplexer will
    -- only run its ingress side.
    ResponderMode          :: Mode
    -- | Execute initiator and responder protocols.  In this mode the
    -- multiplexer will run both ingress and egress sides.
    InitiatorResponderMode :: Mode

type family HasInitiator (mode :: Mode) :: Bool where
    HasInitiator InitiatorMode          = True
    HasInitiator ResponderMode          = False
    HasInitiator InitiatorResponderMode = True

type family HasResponder (mode :: Mode) :: Bool where
    HasResponder InitiatorMode          = False
    HasResponder ResponderMode          = True
    HasResponder InitiatorResponderMode = True

-- | A static description of a mini-protocol.
--
data MiniProtocolInfo (mode :: Mode) =
     MiniProtocolInfo {
       miniProtocolNum        :: !MiniProtocolNum,
       -- ^ Unique mini-protocol number.
       miniProtocolDir        :: !(MiniProtocolDirection mode),
       -- ^ Mini-protocol direction.
       miniProtocolLimits     :: !MiniProtocolLimits,
       -- ^ ingress queue limits for the protocol
       miniProtocolCapability :: !(Maybe Int)
       -- ^ capability on which the mini-protocol should run
     }

data MiniProtocolDirection (mode :: Mode) where
    InitiatorDirectionOnly :: MiniProtocolDirection InitiatorMode
    ResponderDirectionOnly :: MiniProtocolDirection ResponderMode
    InitiatorDirection     :: MiniProtocolDirection InitiatorResponderMode
    ResponderDirection     :: MiniProtocolDirection InitiatorResponderMode

deriving instance Eq (MiniProtocolDirection (mode :: Mode))
deriving instance Ord (MiniProtocolDirection (mode :: Mode))

--
-- Mux status
--

data Status
    -- | Initial mux state, mux is ready to accept requests.  It does not
    -- indicate weather mux thread was started or not.
    = Ready

    -- | Mux failed with 'SomeException'
    | Failed SomeException

    -- | Mux is being stopped; mux will not accept any new mini-protocols to
    -- start.
    | Stopping

     -- | Mux stopped.
    | Stopped
    deriving Show

--
-- Mux internal types
--

type IngressQueue m = StrictTVar m BL.ByteString

-- | The index of a protocol in a MuxApplication, used for array indices
newtype MiniProtocolIx = MiniProtocolIx Int
  deriving (Eq, Ord, Num, Enum, Ix, Show)

data MiniProtocolDir = InitiatorDir | ResponderDir
  deriving (Eq, Ord, Ix, Enum, Bounded, Show)

protocolDirEnum :: MiniProtocolDirection mode -> MiniProtocolDir
protocolDirEnum InitiatorDirectionOnly = InitiatorDir
protocolDirEnum ResponderDirectionOnly = ResponderDir
protocolDirEnum InitiatorDirection     = InitiatorDir
protocolDirEnum ResponderDirection     = ResponderDir

data MiniProtocolState mode m = MiniProtocolState {
       miniProtocolInfo         :: MiniProtocolInfo mode,
       miniProtocolIngressQueue :: IngressQueue m,
       miniProtocolStatusVar    :: StrictTVar m MiniProtocolStatus
     }

data MiniProtocolStatus = StatusIdle
                        | StatusStartOnDemand
                        | StatusRunning
                        | StatusStartOnDemandAny
  deriving (Eq, Show)

data SDUHeader = SDUHeader {
      mhTimestamp :: !RemoteClockModel
    , mhNum       :: !MiniProtocolNum
    , mhDir       :: !MiniProtocolDir
    , mhLength    :: !Word16
    }


data SDU = SDU {
      msHeader :: !SDUHeader
    , msBlob   :: !BL.ByteString
    }

msTimestamp :: SDU -> RemoteClockModel
msTimestamp = mhTimestamp . msHeader

setTimestamp :: SDU -> RemoteClockModel -> SDU
setTimestamp sdu@SDU { msHeader } mhTimestamp =
    sdu { msHeader = msHeader { mhTimestamp } }

msNum :: SDU -> MiniProtocolNum
msNum = mhNum . msHeader

msDir :: SDU -> MiniProtocolDir
msDir = mhDir . msHeader

msLength :: SDU -> Word16
msLength = mhLength . msHeader

-- | Size of a MuxHeader in Bytes
msHeaderLength :: Int64
msHeaderLength = 8

-- | Low level access to underlying socket or pipe.  There are three smart
-- constructors:
--
-- * 'Network.Socket.socketAsBearer'
-- * 'Network.Pipe.pipeAsBearer'
-- * @Test.Mux.queuesAsBearer@
--
data Bearer m = Bearer {
    -- | Timestamp and send SDU.
      write     :: TimeoutFn m -> SDU -> m Time
    -- | Timestamp and send many SDUs.
    , writeMany :: TimeoutFn m -> [SDU] -> m Time
    -- | Read a SDU
    , read      :: TimeoutFn m -> m (SDU, Time)
    -- | Return a suitable SDU payload size.
    , sduSize   :: SDUSize
    -- | Return a suitable batch size
    , batchSize :: Int
    -- | Name of the bearer
    , name      :: String
    }

newtype SDUSize = SDUSize { getSDUSize :: Word16 }
  deriving Generic
  deriving Show via Quiet SDUSize
  deriving (Eq, Ord, Enum)
  deriving (Num, Real, Integral)

-- | A channel which wraps each message as an 'SDU' using giving
-- 'MiniProtocolNum' and 'MiniProtocolDir'.
--
bearerAsChannel
  :: forall m. Functor m
  => Bearer m
  -> MiniProtocolNum
  -> MiniProtocolDir
  -> ByteChannel m
bearerAsChannel bearer ptclNum ptclDir =
      Channel {
        send = \blob -> void $ write bearer noTimeout (wrap blob),
        recv = Just . msBlob . fst <$> read bearer noTimeout
      }
    where
      -- wrap a 'ByteString' as 'SDU'
      wrap :: BL.ByteString -> SDU
      wrap blob = SDU {
            -- it will be filled when the 'SDU' is send by the 'bearer'
            msHeader = SDUHeader {
                mhTimestamp = RemoteClockModel 0,
                mhNum       = ptclNum,
                mhDir       = ptclDir,
                mhLength    = fromIntegral $ BL.length blob
              },
            msBlob = blob
          }

      noTimeout :: TimeoutFn m
      noTimeout _ r = Just <$> r

--
-- Errors
--

data RuntimeError =
    ProtocolAlreadyRunning       !MiniProtocolNum !MiniProtocolDir !MiniProtocolStatus
  | UnknownProtocolInternalError !MiniProtocolNum !MiniProtocolDir
  | BlockedOnCompletionVar       !MiniProtocolNum
  deriving Show

instance Exception RuntimeError

-- | ReadBuffer for Mux Bearers
--
-- This is used to read more data than whats currently needed in one syscall.
-- Any extra data read is cached in rbVar until the next read request.
data ReadBuffer m = ReadBuffer {
  -- | Read cache
    rbVar  :: StrictTVar m BL.ByteString
  -- | Buffer, used by the kernel to write the received data into.
  , rbBuf  :: Ptr Word8
  -- | Size of `rbBuf`.
  , rbSize :: Int
  }
