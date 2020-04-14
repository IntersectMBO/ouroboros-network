{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}

module Network.Mux.Types (
      MiniProtocolLimits (..)
    , MiniProtocolNum (..)
    , MiniProtocolMode (..)

    , AppType (..)
    , HasInitiator
    , HasResponder
    , MuxApplication (..)
    , MuxMiniProtocol (..)
    , RunMiniProtocol (..)

    , MiniProtocolIx
    , MuxBearer (..)
    , muxBearerAsChannel
    , MuxSDU (..)
    , MuxSDUHeader (..)
    , msTimestamp
    , setTimestamp
    , msNum
    , msMode
    , msLength
    , RemoteClockModel (..)
    , remoteClockPrecision
    ) where

import           Prelude hiding (read)

import           Data.Void (Void)
import           Data.Functor (void)
import           Data.Int
import           Data.Ix (Ix (..))
import           Data.Word
import qualified Data.ByteString.Lazy as BL

import           Control.Monad.Class.MonadTime

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
       maximumIngressQueue :: !Int64
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

data AppType where
    InitiatorApp             :: AppType
    ResponderApp             :: AppType
    InitiatorAndResponderApp :: AppType

type family HasInitiator (appType :: AppType) :: Bool where
    HasInitiator InitiatorApp             = True
    HasInitiator ResponderApp             = False
    HasInitiator InitiatorAndResponderApp = True

type family HasResponder (appType :: AppType) :: Bool where
    HasResponder InitiatorApp             = False
    HasResponder ResponderApp             = True
    HasResponder InitiatorAndResponderApp = True

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
newtype MuxApplication (appType :: AppType) m a b =
        MuxApplication [MuxMiniProtocol appType m a b]

data MuxMiniProtocol (appType :: AppType) m a b =
     MuxMiniProtocol {
       miniProtocolNum    :: !MiniProtocolNum,
       miniProtocolLimits :: !MiniProtocolLimits,
       miniProtocolRun    :: !(RunMiniProtocol appType m a b)
     }

data RunMiniProtocol (appType :: AppType) m a b where
  InitiatorProtocolOnly
    -- Initiator application; most simple application will be @'runPeer'@ or
    -- @'runPipelinedPeer'@ supplied with a codec and a @'Peer'@ for each
    -- @ptcl@.  But it allows to handle resources if just application of
    -- @'runPeer'@ is not enough.  It will be run as @'ModeInitiator'@.
    :: (Channel m -> m a)
    -> RunMiniProtocol InitiatorApp m a Void

  ResponderProtocolOnly
    -- Responder application; similarly to the @'MuxInitiatorApplication'@ but it
    -- will be run using @'ModeResponder'@.
    :: (Channel m -> m b)
    -> RunMiniProtocol ResponderApp m Void b

  InitiatorAndResponderProtocol
    -- Initiator and server applications.
    :: (Channel m -> m a)
    -> (Channel m -> m b)
    -> RunMiniProtocol InitiatorAndResponderApp m a b

--
-- Mux internal types
--

-- | The index of a protocol in a MuxApplication, used for array indicies
newtype MiniProtocolIx = MiniProtocolIx Int
  deriving (Eq, Ord, Num, Enum, Ix, Show)

data MiniProtocolMode = ModeInitiator | ModeResponder
  deriving (Eq, Ord, Ix, Enum, Bounded, Show)


data MuxSDUHeader = MuxSDUHeader {
      mhTimestamp :: !RemoteClockModel
    , mhNum       :: !MiniProtocolNum
    , mhMode      :: !MiniProtocolMode
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

msMode :: MuxSDU -> MiniProtocolMode
msMode = mhMode . msHeader

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
    , sduSize :: Word16
    }


-- | A channel which wraps each message as an 'MuxSDU' using giving
-- 'MiniProtocolNum' and 'MiniProtocolMode'.
--
muxBearerAsChannel
  :: MuxBearer IO
  -> MiniProtocolNum
  -> MiniProtocolMode
  -> Channel IO
muxBearerAsChannel bearer protocolNum mode =
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
                mhNum       = protocolNum,
                mhMode      = mode,
                mhLength    = fromIntegral $ BL.length blob
              },
            msBlob = blob
          }

      noTimeout :: TimeoutFn IO
      noTimeout _ r = Just <$> r
