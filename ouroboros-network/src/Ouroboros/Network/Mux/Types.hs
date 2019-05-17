{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Mux.Types (
      MiniProtocolDescriptions
    , MiniProtocolDispatch (..)
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxBearer (..)
    , MuxBearerState (..)
    , MuxError (..)
    , MuxErrorType (..)
    , MuxSDU (..)
    , MuxSDUHeader (..)
    , PerMuxSharedState (..)
    , RemoteClockModel (..)
    , TranslocationServiceRequest (..)
    , Wanton (..)
    ) where

import           Control.Exception
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           Data.Ix (Ix (..))
import           Data.Word
import           GHC.Stack
import           Text.Printf

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime

import           Network.TypedProtocol.Channel


newtype RemoteClockModel = RemoteClockModel { unRemoteClockModel :: Word32 } deriving Eq


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


type MiniProtocolDescriptions ptcl m = ptcl -> Channel m BL.ByteString -> m ()

--
-- Mux internal types
--

newtype MiniProtocolDispatch ptcl m =
        MiniProtocolDispatch (Array (MiniProtocolId ptcl)
                                    (TBQueue m BL.ByteString))

data MiniProtocolMode = ModeInitiator | ModeResponder
  deriving (Eq, Ord, Ix, Enum, Bounded, Show)

data MuxSDU ptcl = MuxSDU {
      msTimestamp :: !RemoteClockModel
    , msId        :: !(MiniProtocolId ptcl)
    , msLength    :: !Word16
    , msBlob      :: !BL.ByteString
    }

data MuxSDUHeader = MuxSDUHeader {
      mshTimestamp :: !RemoteClockModel
    , mshId        :: !Word16
    , mshLength    :: !Word16
    }

-- | A TranslocationServiceRequest is a demand for the translocation
--  of a single mini-protocol message. This message can be of
--  arbitrary (yet bounded) size. This multiplexing layer is
--  responsible for the segmentation of concrete representation into
--  appropriate SDU's for onward transmission.
data TranslocationServiceRequest ptcl m
  = TLSRDemand (MiniProtocolId ptcl) (Wanton m)

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

data MuxBearerState = Larval    -- Newly created MuxBearer.
                    | Connected -- MuxBearer is connected to a peer.
                    | Mature    -- MuxBearer has successufully completed verison negotiation.
                    | Dying     -- MuxBearer is in the process of beeing torn down, requests may fail.
                    | Dead      -- MuxBearer is dead and the underlying bearer has been closed.
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
                  | MuxControlProtocolError
                  deriving (Show, Eq)

instance Exception MuxError where
    displayException MuxError{..} = printf "%s %s at %s"
        (show errorType)
        (show errorMsg)
        (prettyCallStack errorStack)

