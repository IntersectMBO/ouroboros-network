{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Mux (
      MiniProtocolId (..)
    , MuxBearer (..)
    , MuxSDU (..)
    , RemoteClockModel (..)
    , encodeMuxSDU
    , decodeMuxSDUHeader
    ) where

import qualified Data.Binary.Put as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as BL
import           Data.Word

data RemoteClockModel = RemoteClockModel {
    unRemoteClockModel :: !Word32
  }

data MiniProtocolId = Muxcontrol
                    | DeltaQ
                    | ChainSync
                    | Blockdownload
                    | DelegationCertificates
                    | TxSubmission
                    deriving (Show, Eq)

data MuxSDU = MuxSDU {
      msTimestamp :: !RemoteClockModel
    , msId        :: !MiniProtocolId
    , msLength    :: !Word16
    , msBlob      :: !BL.ByteString
    }

encodeMuxSDU :: MuxSDU -> BL.ByteString
encodeMuxSDU sdu =
  let hdr = Bin.runPut enc in
  BL.append hdr $ msBlob sdu
  where
    enc = do
        Bin.putWord32be $ unRemoteClockModel $ msTimestamp sdu
        putId $ msId sdu
        Bin.putWord16be $ fromIntegral $ BL.length $ msBlob sdu

    putId Muxcontrol             = Bin.putWord16be 0
    putId DeltaQ                 = Bin.putWord16be 1
    putId ChainSync              = Bin.putWord16be 2
    putId Blockdownload          = Bin.putWord16be 3
    putId DelegationCertificates = Bin.putWord16be 4
    putId TxSubmission           = Bin.putWord16be 5

decodeMuxSDUHeader :: BL.ByteString -> Maybe MuxSDU
decodeMuxSDUHeader buf =
    case Bin.runGetOrFail dec buf of
         Left  (_, _, _)  -> Nothing
         Right (_, _, ph) -> Just ph

  where
    dec = do
        ts <- Bin.getWord32be
        id_ <- Bin.getWord16be
        len <- Bin.getWord16be
        return $ MuxSDU (RemoteClockModel ts) (getId id_) len BL.empty

    getId 0 = Muxcontrol
    getId 1 = DeltaQ
    getId 2 = ChainSync
    getId 3 = Blockdownload
    getId 4 = DelegationCertificates
    getId 5 = TxSubmission
    getId a = error $ "unknow miniprotocol " ++ show a -- XXX

--remoteClockTimestampFromLocalClock :: (MonadTime m) => m RemoteClockModel
--remoteClockTimestampFromLocalClock = undefined -- use getMonotonicTime

class MuxBearer m where
  type LocalClockModel m :: *
  type AssociationDetails m :: *
  type MuxBearerHandle m :: *
  open :: AssociationDetails m -> m (MuxBearerHandle m)
  server :: AssociationDetails m -> (MuxBearerHandle m -> m ()) -> m (MuxBearerHandle m)
  sduSize :: MuxBearerHandle m-> m Int
  write :: MuxBearerHandle m -> (RemoteClockModel -> MuxSDU) -> m (LocalClockModel m)
  read :: MuxBearerHandle m -> m (MuxSDU, LocalClockModel m)
  close :: MuxBearerHandle m -> m ()
  abandon :: MuxBearerHandle m -> m ()


