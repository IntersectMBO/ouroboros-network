{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Mux.Types (
      MiniProtocolDescription (..)
    , MiniProtocolDescriptions
    , MiniProtocolDispatch (..)
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxBearer (..)
    , MuxBearerState (..)
    , MuxError (..)
    , MuxErrorType (..)
    , MuxStyle (..)
    , MuxSDU (..)
    , MuxSDUHeader (..)
    , NetworkMagic (..)
    , PerMuxSharedState (..)
    , RemoteClockModel (..)
    , TranslocationServiceRequest (..)
    , Version (..)
    , versionToVersionNumber
    , versionMagic
    , VersionNumber (..)
    , Wanton (..)
    ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise.Class
import           Control.Exception
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           Data.Ix (Ix (..))
import           Data.Word
import           GHC.Stack
import           Text.Printf

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Ouroboros.Network.Channel

newtype RemoteClockModel = RemoteClockModel { unRemoteClockModel :: Word32 } deriving Eq

data VersionNumber = VersionNumber0
                   | VersionNumber1
                   deriving (Eq, Ord, Show, Enum, Ix, Bounded)

newtype NetworkMagic = NetworkMagic { unNetworkMagic :: Word32 } deriving (Eq, Show, Ord)

instance Serialise NetworkMagic where
    encode (NetworkMagic nm) = encode nm
    decode = NetworkMagic <$> decode

data Version = Version0 !NetworkMagic -- Plain NodeToNode
             | Version1 !NetworkMagic -- NodeToClient
             deriving (Eq, Ord)

versionMagic :: Version -> NetworkMagic
versionMagic (Version0 nm) = nm
versionMagic (Version1 nm) = nm

instance Show Version where
    show (Version0 m) = printf "Version 0 NetworkMagic %d" $ unNetworkMagic m
    show (Version1 m) = printf "Version 1 NetworkMagic %d" $ unNetworkMagic m

instance Serialise Version where
    encode (Version0 m) = CBOR.encodeWord 0 <> encode m
    encode (Version1 m) = CBOR.encodeWord 1 <> encode m

    decode = do
        vn <- CBOR.decodeWord
        case vn of
             0 -> Version0 . NetworkMagic <$> decode
             1 -> Version1 . NetworkMagic <$> decode
             n -> throw $ MuxError MuxControlUnknownVersion ("unknown version " ++ show n) callStack

versionToVersionNumber :: Version -> VersionNumber
versionToVersionNumber (Version0 _) = VersionNumber0
versionToVersionNumber (Version1 _) = VersionNumber1

--
-- Mini-protocol numbers
--

-- | The wire format includes the protocol numbers, and it's vital that these
-- are stable. They are not necessarily dense however, as new ones are added
-- and some old ones retired. So we use a dedicated class for this rather than
-- reusing 'Enum'. This also covers unrecognised protocol numbers on the
-- decoding side.
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


--
-- Mini-protocol descriptions
--

{- | The 'MiniProtocolDescription' is used to provide
 two functions which will consume and produce messages
 for either the initiator (client) or responder (server)
 side of the given miniprotocol.
 The functions will execute in their own threads and should
 any of them exit the underlying Mux Bearer will be torn down
 along with all other miniprotocols.
 -}
data MiniProtocolDescription ptcl m = MiniProtocolDescription {
    -- | Initiator function, consumes and produces messages related to the initiator side.
      mpdInitiator :: Channel m BL.ByteString -> m ()
    -- | Responder function, consumes and produces messages related to the responder side.
    , mpdResponder :: Channel m BL.ByteString -> m ()
    }

type MiniProtocolDescriptions ptcl m = ptcl -> MiniProtocolDescription ptcl m

--
-- Mux internal types
--

newtype MiniProtocolDispatch ptcl m =
        MiniProtocolDispatch (Array (MiniProtocolId ptcl, MiniProtocolMode)
                                    (TBQueue m BL.ByteString))

data MiniProtocolMode = ModeInitiator | ModeResponder
  deriving (Eq, Ord, Ix, Enum, Bounded, Show)

data MuxStyle = StyleClient | StyleServer
  deriving (Eq, Show)

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
newtype Wanton m = Wanton { want :: TMVar m BL.ByteString }

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
                    | Connected
                    | Mature
                    | Dying
                    | Dead
                    deriving (Eq, Show)

data MuxBearer ptcl m = MuxBearer {
    -- | Timestamp and send MuxSDU
      write   :: MuxSDU ptcl -> m (Time m)
    -- | Read a MuxSDU
    , read    :: m (MuxSDU ptcl, Time m)
    -- | Return a suitable MuxSDU payload size
    , sduSize :: m Word16
    , close   :: m ()
    , state   :: TVar m MuxBearerState
    }

data MuxError = MuxError {
      errorType  :: !MuxErrorType
    , errorMsg   :: !String
    , errorStack :: !CallStack
    } deriving Show

data MuxErrorType = MuxUnknownMiniProtocol
                  | MuxDecodeError
                  | MuxBearerClosed
                  | MuxControlUnknownMessage
                  | MuxControlUnknownVersion
                  | MuxControlNoMatchingVersion
                  deriving (Show, Eq)

instance Exception MuxError where
    displayException MuxError{..} = printf "%s %s at %s"
        (show errorType)
        (show errorMsg)
        (prettyCallStack errorStack)

