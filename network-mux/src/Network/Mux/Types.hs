{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Network.Mux.Types (
      MiniProtocolBundle (..)
    , MiniProtocolInfo (..)
    , MiniProtocolNum (..)
    , MiniProtocolDirection (..)
    , MiniProtocolLimits (..)

    , MuxMode (..)
    , HasInitiator
    , HasResponder

    , IngressQueue
    , MiniProtocolIx
    , MiniProtocolDir (..)
    , protocolDirEnum
    , MiniProtocolState (..)
    , MiniProtocolStatus (..)
    , MuxBearer (..)
    , muxBearerAsChannel
    , MuxSDU (..)
    , MuxSDUHeader (..)
    , SDUSize (..)
    , msTimestamp
    , setTimestamp
    , msNum
    , msDir
    , msLength
    , RemoteClockModel (..)
    , remoteClockPrecision

    , MuxRuntimeError (..)
    ) where

import           Prelude hiding (read)

import           Control.Exception (Exception)
import           Data.Functor (void)
import           Data.Ix (Ix (..))
import           Data.Word
import qualified Data.ByteString.Lazy as BL
import           Quiet

import           GHC.Generics (Generic)

import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadSTM.Strict (StrictTVar)

import           Network.Mux.Channel (Channel(..))
import           Network.Mux.Timeout (TimeoutFn)


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
data MiniProtocolLimits =
     MiniProtocolLimits {
       -- | Limit on the maximum number of bytes that can be queued in the
       -- miniprotocol's ingress queue.
       --
       maximumIngressQueue :: !Int
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

data MuxMode where
    InitiatorMode          :: MuxMode
    ResponderMode          :: MuxMode
    InitiatorResponderMode :: MuxMode

type family HasInitiator (mode :: MuxMode) :: Bool where
    HasInitiator InitiatorMode          = True
    HasInitiator ResponderMode          = False
    HasInitiator InitiatorResponderMode = True

type family HasResponder (mode :: MuxMode) :: Bool where
    HasResponder InitiatorMode          = False
    HasResponder ResponderMode          = True
    HasResponder InitiatorResponderMode = True

-- | Application run by mux layer.
--
-- * enumeration of client application, e.g. a wallet application communicating
--   with a node using ChainSync and TxSubmission protocols; this only requires
--   to run client side of each protocol.
--
-- * enumeration of server applications: this application type is mostly useful
--   tests.
--
-- * enumeration of both client and server applications, e.g. a full node
--   serving downstream peers using server side of each protocol and getting
--   updates from upstream peers using client side of each of the protocols.
--
newtype MiniProtocolBundle (mode :: MuxMode) =
        MiniProtocolBundle [MiniProtocolInfo mode]

data MiniProtocolInfo (mode :: MuxMode) =
     MiniProtocolInfo {
       miniProtocolNum    :: !MiniProtocolNum,
       miniProtocolDir    :: !(MiniProtocolDirection mode),
       miniProtocolLimits :: !MiniProtocolLimits
     }

data MiniProtocolDirection (mode :: MuxMode) where
    InitiatorDirectionOnly :: MiniProtocolDirection InitiatorMode
    ResponderDirectionOnly :: MiniProtocolDirection ResponderMode
    InitiatorDirection     :: MiniProtocolDirection InitiatorResponderMode
    ResponderDirection     :: MiniProtocolDirection InitiatorResponderMode

deriving instance Eq (MiniProtocolDirection (mode :: MuxMode))
deriving instance Ord (MiniProtocolDirection (mode :: MuxMode))

--
-- Mux internal types
--

type IngressQueue m = StrictTVar m BL.ByteString

-- | The index of a protocol in a MuxApplication, used for array indicies
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

data MiniProtocolStatus = StatusIdle | StatusStartOnDemand | StatusRunning
  deriving (Eq, Show)

data MuxSDUHeader = MuxSDUHeader {
      mhTimestamp :: !RemoteClockModel
    , mhNum       :: !MiniProtocolNum
    , mhDir       :: !MiniProtocolDir
    , mhLength    :: !Word16
    }


data MuxSDU = MuxSDU {
      msHeader :: !MuxSDUHeader
    , msBlob   :: !BL.ByteString
    }

msTimestamp :: MuxSDU -> RemoteClockModel
msTimestamp = mhTimestamp . msHeader

setTimestamp :: MuxSDU -> RemoteClockModel -> MuxSDU
setTimestamp sdu@MuxSDU { msHeader } mhTimestamp =
    sdu { msHeader = msHeader { mhTimestamp } } 

msNum :: MuxSDU -> MiniProtocolNum
msNum = mhNum . msHeader

msDir :: MuxSDU -> MiniProtocolDir
msDir = mhDir . msHeader

msLength :: MuxSDU -> Word16
msLength = mhLength . msHeader


-- | Low level access to underlying socket or pipe.  There are three smart
-- constructors:
--
-- * 'Network.Socket.socketAsMuxBearer'
-- * 'Network.Pipe.pipeAsMuxBearer'
-- * @Test.Mux.queuesAsMuxBearer@
--
data MuxBearer m = MuxBearer {
    -- | Timestamp and send MuxSDU.
      write   :: TimeoutFn m -> MuxSDU -> m Time
    -- | Read a MuxSDU
    , read    :: TimeoutFn m -> m (MuxSDU, Time)
    -- | Return a suitable MuxSDU payload size.
    , sduSize :: SDUSize
    }

newtype SDUSize = SDUSize { getSDUSize :: Word16 }
  deriving Generic
  deriving Show via Quiet SDUSize

-- | A channel which wraps each message as an 'MuxSDU' using giving
-- 'MiniProtocolNum' and 'MiniProtocolDir'.
--
muxBearerAsChannel
  :: forall m. Functor m
  => MuxBearer m
  -> MiniProtocolNum
  -> MiniProtocolDir
  -> Channel m
muxBearerAsChannel bearer ptclNum ptclDir =
      Channel {
        send = \blob -> void $ write bearer noTimeout (wrap blob),
        recv = Just . msBlob . fst <$> read bearer noTimeout
      }
    where
      -- wrap a 'ByteString' as 'MuxSDU'
      wrap :: BL.ByteString -> MuxSDU
      wrap blob = MuxSDU {
            -- it will be filled when the 'MuxSDU' is send by the 'bearer'
            msHeader = MuxSDUHeader {
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

data MuxRuntimeError =
    ProtocolAlreadyRunning       !MiniProtocolNum !MiniProtocolDir !MiniProtocolStatus
  | UnknownProtocolInternalError !MiniProtocolNum !MiniProtocolDir
  | MuxBlockedOnCompletionVar    !MiniProtocolNum
  deriving Show

instance Exception MuxRuntimeError
