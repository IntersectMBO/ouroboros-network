{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Mux (
      MiniProtocolDescription (..)
    , MiniProtocolDescriptions (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
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
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.Word

import           Protocol.Channel

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Write as CBOR (toLazyByteString)
import           Text.Printf


data RemoteClockModel = RemoteClockModel {
    unRemoteClockModel :: !Word32
  }

data MiniProtocolId = Muxcontrol
                    | DeltaQ
                    | ChainSync
                    | Blockdownload
                    | TxSubmission
                    deriving (Eq, Ord, Show)

data MiniProtocolDescription m = MiniProtocolDescription {
      mpdId :: MiniProtocolId
    , mpdInitiator :: Duplex m m CBOR.Encoding BL.ByteString -> m ()
    , mpdResponder :: Duplex m m CBOR.Encoding BL.ByteString -> m ()
    }

data MiniProtocolDescriptions m = MiniProtocolDescriptions (M.Map MiniProtocolId (MiniProtocolDescription m))

data MiniProtocolDispatch m = MiniProtocolDispatch (M.Map (MiniProtocolId, MiniProtocolMode)
                                                   (TBQueue m BL.ByteString))

data MiniProtocolMode = ModeInitiator | ModeResponder deriving (Eq, Ord, Show)

negMiniProtocolMode :: MiniProtocolMode -> MiniProtocolMode
negMiniProtocolMode ModeInitiator = ModeResponder
negMiniProtocolMode ModeResponder = ModeInitiator

data MuxSDU = MuxSDU {
      msTimestamp :: !RemoteClockModel
    , msId        :: !MiniProtocolId
    , msMode      :: !MiniProtocolMode
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
        putId (msId sdu) (putMode $ msMode sdu)
        Bin.putWord16be $ fromIntegral $ BL.length $ msBlob sdu

    putId Muxcontrol mode        = Bin.putWord16be $ 0 .|. mode
    putId DeltaQ mode            = Bin.putWord16be $ 1 .|. mode
    putId ChainSync mode         = Bin.putWord16be $ 2 .|. mode
    putId Blockdownload mode     = Bin.putWord16be $ 3 .|. mode
    putId TxSubmission mode      = Bin.putWord16be $ 4 .|. mode

    putMode :: MiniProtocolMode -> Word16
    putMode ModeInitiator = 0
    putMode ModeResponder = 0x8000

decodeMuxSDUHeader :: BL.ByteString -> Maybe MuxSDU
decodeMuxSDUHeader buf =
    case Bin.runGetOrFail dec buf of
         Left  (_, _, _)  -> Nothing
         Right (_, _, ph) -> Just ph

  where
    dec = do
        ts <- Bin.getWord32be
        mid <- Bin.getWord16be
        len <- Bin.getWord16be
        return $ MuxSDU (RemoteClockModel ts) (getId (mid .&. 0x7fff)) (getMode (mid .&. 0x8000))
                        len BL.empty

    getMode 0      = ModeInitiator
    getMode 0x8000 = ModeResponder

    getId 0 = Muxcontrol
    getId 1 = DeltaQ
    getId 2 = ChainSync
    getId 3 = Blockdownload
    getId 4 = TxSubmission
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
    atomically $ writeTBQueue (ingressQueue (dispatchTable pmss) (msId sdu) (msMode sdu)) (msBlob sdu)

ingressQueue :: (MuxBearer m) => MiniProtocolDispatch m -> MiniProtocolId -> MiniProtocolMode -> TBQueue m BL.ByteString
ingressQueue (MiniProtocolDispatch tbl) dis mode =
  -- Notice the mode reversal, ModeResponder is delivered to ModeInitiator and vice versa.
  case M.lookup (dis, negMiniProtocolMode mode) tbl of
       Nothing -> error $ printf "Missing MiniProtocol %s mode %s in dispatch table"
                                 (show dis) (show mode) -- XXX
       Just q  -> q

start :: (MuxBearer m, MonadSTM m, MonadFork m) =>
    MiniProtocolDescriptions m ->
    AssociationDetails m -> m ()
start (MiniProtocolDescriptions udesc) addr = do
    tbl <- setupTbl
    handle <- open addr
    tq <- atomically $ newTBQueue 100

    let pmss = PerMuxSS tbl handle tq

    fork $ demux pmss
    fork $ mux pmss

    mapM_ (spawnHandler pmss) $ M.elems udesc
    return ()

  where

    spawnHandler pmss mpd = do
        w_i <- atomically newEmptyTMVar
        fork $ (mpdInitiator mpd) $ muxDuplex pmss (mpdId mpd) ModeInitiator w_i

        w_r <- atomically newEmptyTMVar
        fork $ (mpdResponder mpd) $ muxDuplex pmss (mpdId mpd) ModeResponder w_r
        return ()

    setupTbl = do
        let ps = [Muxcontrol, DeltaQ] ++ (M.keys udesc)
        tbl <- foldM addMp M.empty ps
        return $ MiniProtocolDispatch tbl

    addMp t p = do
        a <- atomically $ newTBQueue 2
        b <- atomically $ newTBQueue 2
        return $ M.insert (p, ModeInitiator) a $ M.insert (p, ModeResponder) b t

muxDuplex :: (MuxBearer m, MonadSTM m) =>
    PerMuxSharedState m ->
    MiniProtocolId ->
    MiniProtocolMode ->
    TMVar m BL.ByteString ->
    Duplex m m CBOR.Encoding BL.ByteString
muxDuplex pmss mid md w = do
  uniformDuplex snd_ rcv
  where
    snd_ = \encoding -> do
        atomically $ putTMVar w (CBOR.toLazyByteString encoding)
        atomically $ writeTBQueue (tsrQueue pmss) (TLSRDemand mid md (Wanton w))
    rcv = do
        blob <- atomically $ readTBQueue (ingressQueue (dispatchTable pmss) mid md)
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
  = TLSRDemand MiniProtocolId MiniProtocolMode (Wanton m)
  | TLSRControl MiniProtocolId TLSRAction

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
      dispatchTable  :: MiniProtocolDispatch m -- fixed, known at instantiation
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
         TLSRDemand mid md d
             -> processSingleWanton pmss mid md d >> mux pmss
         TLSRControl _ _
             -> undefined

-- Pull a `maxSDU`s worth of data out out the `Wanton` - if there is
-- data remaining requeue the `TranslocationServiceRequest` (this
-- ensures that any other items on the queue will get some service
-- first.
processSingleWanton :: (MonadSTM m, MuxBearer m)
                    => PerMuxSharedState m
                    -> MiniProtocolId
                    -> MiniProtocolMode
                    -> Wanton m
                    -> m ()
processSingleWanton pmss mpi md wanton = do
    maxSDU <- sduSize (bearerHandle pmss)
    blob <- atomically $ do
      -- extract next SDU
      d <- takeTMVar (want wanton)
      let (frag, rest) = BL.splitAt (fromIntegral maxSDU) d
      -- if more to process then enqueue remaining work
      unless (BL.null rest) $
        do putTMVar (want wanton) rest
           writeTBQueue (tsrQueue pmss) (TLSRDemand mpi md wanton)
      -- return data to send
      pure frag
    let sdu = MuxSDU (RemoteClockModel 0) mpi md (fromIntegral $ BL.length blob) blob
    tNow <- write (bearerHandle pmss) (cb sdu)
    --paceTransmission tNow
    return ()

  where
    cb sdu ts = sdu {msTimestamp = ts}

{-paceTransmission :: (MuxBearer m) => LocalClockModel m -> m ()
paceTransmission = return () -- -}


