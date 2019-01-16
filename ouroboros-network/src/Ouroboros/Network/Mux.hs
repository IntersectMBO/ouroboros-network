{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Mux (
      MiniProtocolId (..)
    , MuxBearer (..)
    , MuxSDU (..)
    , RemoteClockModel (..)
    , encodeMuxSDU
    , decodeMuxSDUHeader
    , start
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadSTM
import qualified Data.Binary.Put as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as BL
import           Data.Word

import           Protocol.Channel

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Write as CBOR (toLazyByteString)


data RemoteClockModel = RemoteClockModel {
    unRemoteClockModel :: !Word32
  }

data MiniProtocolId = Muxcontrol
                    | DeltaQ
                    | ChainSync
                    | Blockdownload
                    | DelegationCertificates
                    | TxSubmission
                    deriving (Eq, Ord, Show)

data MiniProtocolTable m = MiniProtocolTable {
      mpcbMuxControl :: TBQueue m BL.ByteString
    , mpcbDeltaQ     :: TBQueue m BL.ByteString
    , mpcbChainSync  :: TBQueue m BL.ByteString
    , mpcbBlockdownload :: TBQueue m BL.ByteString
    , mpcbDelegationCertificates :: TBQueue m BL.ByteString
    , mpcbTxSubmission :: TBQueue m BL.ByteString
    }

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
  sduSize :: MuxBearerHandle m-> m Word16
  write :: MuxBearerHandle m -> (RemoteClockModel -> MuxSDU) -> m (LocalClockModel m)
  read :: MuxBearerHandle m -> m (MuxSDU, LocalClockModel m)
  close :: MuxBearerHandle m -> m ()
  abandon :: MuxBearerHandle m -> m ()

demux :: forall m. (MuxBearer m, MonadSTM m) => PerMuxSharedState m -> m ()
demux pmss = forever $ do
    (sdu, _) <- Ouroboros.Network.Mux.read (bearerHandle pmss)
    atomically $ writeTBQueue (ingressQueue (dispatchTable pmss) (msId sdu)) (msBlob sdu)

ingressQueue :: (MuxBearer m) => MiniProtocolTable m -> MiniProtocolId -> TBQueue m BL.ByteString
ingressQueue idmap Muxcontrol = mpcbMuxControl idmap
ingressQueue idmap DeltaQ = mpcbDeltaQ idmap
ingressQueue idmap ChainSync = mpcbChainSync idmap
ingressQueue idmap Blockdownload = mpcbBlockdownload idmap
ingressQueue idmap DelegationCertificates = mpcbDelegationCertificates idmap
ingressQueue idmap TxSubmission = mpcbTxSubmission idmap

start :: (MuxBearer m, MonadSTM m, MonadFork m) => AssociationDetails m -> m ()
start addr = do
    tbl <- setupTbl
    handle <- open addr
    tq <- atomically $ newTBQueue 100

    let pmss = PerMuxSS tbl handle tq

    fork $ demux pmss
    fork $ mux pmss
  where
    setupTbl = do
        mc <- atomically $ newTBQueue 2
        dq <- atomically $ newTBQueue 2
        cs <- atomically $ newTBQueue 2
        bd <- atomically $ newTBQueue 2
        dc <- atomically $ newTBQueue 2
        td <- atomically $ newTBQueue 2
        return $ MiniProtocolTable mc dq cs bd dc td

muxDuplex :: (MuxBearer m, MonadSTM m) =>
    PerMuxSharedState m ->
    MiniProtocolId ->
    Duplex m m CBOR.Encoding BL.ByteString
muxDuplex pmss mid = uniformDuplex send recv
  where
    send = undefined
    recv = do
        blob <- atomically $ readTBQueue (ingressQueue (dispatchTable pmss) mid)
        if BL.null blob
           then pure Nothing
           else return $ Just BL.empty

-- | Desired servicing semantics
--   ===========================
--
--   Constructing fairness
--   ---------------------
--   In this context we are defining fairness as:
--    - no starvation
--    - when presented with equal demand (from a selection of mini
--      protocols) deliver "equal" service.
--
--   Equality here might be in terms of equal service rate of
--   requests (or segmented requests) and/or in terms of effective
--   (SDU) data rates.
--
--
--  Notes:
--
--   1) It is assumed that (for a given peer) that bulk delivery of
--      blocks (i.e. in recovery mode) and normal, interactive,
--      operation (e.g. chain following) are mutually exclusive. As
--      such there is no requirement to create a notion of
--      prioritisation between such traffic.
--
--   2) We are assuming that the underlying TCP/IP bearer is managed
--      so that indivual Mux-layer PDUs are paced. a) this is necessary
--      to mitigate head-of-line blocking effects (i.e. arbitrary
--      amounts of data accruing in the O/S kernel); b) ensuring that
--      any host egress data rate limits can be respected / enforced.
--
--  Current Caveats
--
--  1) Not considering how mini-protocol associations are constructed
--     (depending on deployment model this might be resolved within
--     the instantiation of the peer relationship)
--
--  2) Not yet considered notion of orderly termination - this not
--     likely to be used in an operational context, but may be needed
--     for test harness use.
--
--  Principle of operation
--  ======================
--
--  Egress direction (mini protocol instance to remote peer)
--  --------------------------------------------------------
--
--  The request for service (the demand) from a mini protocol is
--  encapsulatedin a `Wanton`, such `Wanton`s are placed in a (finite)
--  queue (e.g TBMQ) of `TranslocationServiceRequest`s.
--
--
--  A `TranslocationServiceRequest` is a demand for the translocation
--  of a single mini-protocol message. This message can be of
--  arbitrary (yet bounded) size. This multiplexing layer is
--  responsible for the segmentation of concrete representation into
--  appropriate SDU's for onward transmission.

data TranslocationServiceRequest m
  = TLSRDemand  { protocolIndex :: MiniProtocolId
                , demand        :: Wanton m
                }
  | TLSRControl { protocolIndex :: MiniProtocolId
                , action        :: TLSRAction
                }

data TLSRAction = Abort | Done

-- The concrete data to be translocated, note that the TMVar becoming empty indicates
-- that the last fragment of the data has been enqueued on the
-- underlying bearer.
data Wanton m = Wanton { want :: TMVar m BL.ByteString }

-- Each peer's multiplexer has some state that provides both
-- de-multiplexing details (for despatch of incoming mesages to mini
-- protocols) and for dispatching incoming SDUs.  This is shared
-- between the muxIngress and the bearerIngress processes.
data PerMuxSharedState m = PerMuxSS {
      dispatchTable  :: MiniProtocolTable m -- fixed, known at instantiation
  ,   bearerHandle  :: MuxBearerHandle m
  ,   tsrQueue      :: TBQueue m (TranslocationServiceRequest m)
   -- handles to senders or pipes or whatever
   -- additional performance info (perhaps)
  }

-- Process the messages from the mini protocols - there is a single
-- shared FIFO that contains the items of work. This is processed so
-- that each active demand gets a `maxSDU`s work of data processed
-- each time it gets to the front of the queue
mux :: (MonadSTM m, MuxBearer m)
     => PerMuxSharedState m
     -> m ()
mux pmss = do
    w <- atomically $ readTBQueue $ tsrQueue pmss
    case w of
         TLSRDemand mid d
             -> processSingleWanton pmss mid d >> mux pmss
         TLSRControl _ _
             -> undefined

-- Pull a `maxSDU`s worth of data out out the `Wanton` - if there is
-- data remaining requeue the `TranslocationServiceRequest` (this
-- ensures that any other items on the queue will get some service
-- first.
processSingleWanton :: (MonadSTM m, MuxBearer m)
                    => PerMuxSharedState m
                    -> MiniProtocolId
                    -> Wanton m
                    -> m ()
processSingleWanton pmss mpi wanton = do
    maxSDU <- sduSize (bearerHandle pmss)
    blob <- atomically $ do
      -- extract next SDU
      d <- takeTMVar (want wanton)
      let (frag, rest) = BL.splitAt (fromIntegral maxSDU) d
      -- if more to process then enqueue remaining work
      unless (BL.null rest) $
        do putTMVar (want wanton) rest
           writeTBQueue (tsrQueue pmss) (TLSRDemand mpi wanton)
      -- return data to send
      pure frag
    let sdu = MuxSDU (RemoteClockModel 0) mpi (fromIntegral $ BL.length blob) blob
    tNow <- write (bearerHandle pmss) (cb sdu)
    --paceTransmission tNow
    return ()

  where
    cb sdu ts = sdu {msTimestamp = ts}

{-paceTransmission :: (MuxBearer m) => LocalClockModel m -> m ()
paceTransmission = return () -- -}


