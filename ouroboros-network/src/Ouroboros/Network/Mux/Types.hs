{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Mux.Types (
      MiniProtocolDescription (..)
    , MiniProtocolDescriptions (..)
    , MiniProtocolDispatch (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxSDU (..)
    , PerMuxSharedState (..)
    , RemoteClockModel (..)
    , TranslocationServiceRequest (..)
    , Wanton (..)
    ) where

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.Word

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Protocol.Channel

newtype RemoteClockModel = RemoteClockModel { unRemoteClockModel :: Word32 }

data MiniProtocolId = Muxcontrol
                    | DeltaQ
                    | ChainSync
                    | BlockFetch
                    | TxSubmission
                    deriving (Eq, Ord, Show)

{- | The 'MiniProtocolDescription' is used to provide
 two functions which will consume and produce messages
 for either the initiator (client) or responder (server)
 side of the given miniprotocol.
 The functions will execute in their own threads and should
 any of them exit the underlying Mux Bearer will be torn down
 along with all other miniprotocols.
 -}
data MiniProtocolDescription m = MiniProtocolDescription {
    -- | The 'MiniProtocolId' described.
      mpdId        :: MiniProtocolId
    -- | Initiator function, consumes and produces messages related to the initiator side.
    , mpdInitiator :: Duplex m m CBOR.Encoding BS.ByteString -> m ()
    -- | Responder function, consumes and produces messages related to the responder side.
    , mpdResponder :: Duplex m m CBOR.Encoding BS.ByteString -> m ()
    }

newtype MiniProtocolDescriptions m = MiniProtocolDescriptions (M.Map MiniProtocolId (MiniProtocolDescription m))

newtype MiniProtocolDispatch m = MiniProtocolDispatch (M.Map (MiniProtocolId, MiniProtocolMode)
                                                             (TBQueue m BL.ByteString))

data MiniProtocolMode = ModeInitiator | ModeResponder deriving (Eq, Ord, Show)

data MuxSDU = MuxSDU {
      msTimestamp :: !RemoteClockModel
    , msId        :: !MiniProtocolId
    , msMode      :: !MiniProtocolMode
    , msLength    :: !Word16
    , msBlob      :: !BL.ByteString
    }

-- | A TranslocationServiceRequest is a demand for the translocation
--  of a single mini-protocol message. This message can be of
--  arbitrary (yet bounded) size. This multiplexing layer is
--  responsible for the segmentation of concrete representation into
--  appropriate SDU's for onward transmission.
data TranslocationServiceRequest m
  = TLSRDemand MiniProtocolId MiniProtocolMode (Wanton m)

-- | A Wanton represent the concrete data to be translocated, note that the
--  TMVar becoming empty indicates -- that the last fragment of the data has
--  been enqueued on the -- underlying bearer.
newtype Wanton m = Wanton { want :: TMVar m BL.ByteString }

-- | Each peer's multiplexer has some state that provides both
-- de-multiplexing details (for despatch of incoming mesages to mini
-- protocols) and for dispatching incoming SDUs.  This is shared
-- between the muxIngress and the bearerIngress processes.
data PerMuxSharedState m = PerMuxSS {
  -- | Ingress dispatch table, fixed and known at instantiation.
    dispatchTable :: MiniProtocolDispatch m
  -- | Egress queue, shared by all miniprotocols
  , tsrQueue      :: TBQueue m (TranslocationServiceRequest m)
  -- | Timestamp and send MuxSDU
  , write         :: MuxSDU -> m (Time m)
  -- | Read a MuxSDU
  , read          :: m (MuxSDU, Time m)
  -- | Return a suitable MuxSDU payload size
  , sduSize       :: m Word16
  }

