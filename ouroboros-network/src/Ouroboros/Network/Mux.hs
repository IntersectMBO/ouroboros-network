{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
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
    , muxJobs

    -- $ingress
    -- $egress
    -- $servicingsSemantics
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.Word

import           Protocol.Channel

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Write as CBOR (toLazyByteString)
import           Text.Printf


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
 any of them exit the underlying 'MuxBearer' will be torn down
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


-- | Encode a 'MuxSDU' as a 'ByteString'.
--
-- > Binary format used by 'encodeMuxSDU' and 'decodeMuxSDUHeader'
-- >  0                   1                   2                   3
-- >  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
-- > +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- > |              transmission time                                |
-- > +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- > |M|    conversation id          |              length           |
-- > +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--
-- All fields are in big endian byteorder.
encodeMuxSDU :: MuxSDU -> BL.ByteString
encodeMuxSDU sdu =
  let hdr = Bin.runPut enc in
  BL.append hdr $ msBlob sdu
  where
    enc = do
        Bin.putWord32be $ unRemoteClockModel $ msTimestamp sdu
        putId (msId sdu) (putMode $ msMode sdu)
        Bin.putWord16be $ fromIntegral $ BL.length $ msBlob sdu

    putId Muxcontrol mode    = Bin.putWord16be $ 0 .|. mode
    putId DeltaQ mode        = Bin.putWord16be $ 1 .|. mode
    putId ChainSync mode     = Bin.putWord16be $ 2 .|. mode
    putId BlockFetch mode    = Bin.putWord16be $ 3 .|. mode
    putId TxSubmission mode  = Bin.putWord16be $ 4 .|. mode

    putMode :: MiniProtocolMode -> Word16
    putMode ModeInitiator = 0
    putMode ModeResponder = 0x8000

{- | Decode a MuSDU header -}
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
    getMode _      = error "impossible use of bitmask" -- XXX

    getId 0 = Muxcontrol
    getId 1 = DeltaQ
    getId 2 = ChainSync
    getId 3 = BlockFetch
    getId 4 = TxSubmission
    getId a = error $ "unknow miniprotocol " ++ show a -- XXX

class MuxBearer m where
  type LocalClockModel m :: *
  type AssociationDetails m :: *
  type MuxBearerHandle m :: *
  type ResponderHandle m :: *
  initiator :: MiniProtocolDescriptions m -> AssociationDetails m -> AssociationDetails m -> m ()
  responder :: MiniProtocolDescriptions m -> AssociationDetails m -> m (ResponderHandle m)
  killResponder :: ResponderHandle m -> m ()
  sduSize :: MuxBearerHandle m-> m Word16
  write :: MuxBearerHandle m -> (RemoteClockModel -> MuxSDU) -> m (LocalClockModel m)
  read :: MuxBearerHandle m -> m (MuxSDU, LocalClockModel m)
  close :: MuxBearerHandle m -> m ()
  abandon :: MuxBearerHandle m -> m ()

-- $ingress
-- = Ingress Path
--
-- >                  o
-- >                  |
-- >                  | ByteStrings
-- >                  |
-- >                  V
-- >          +-------+-------+
-- >          | Bearer.read() | MuxBearer implementation (Socket, Pipes, etc.)
-- >          +---------------+
-- >                  |
-- >                  | MuxSDUs
-- >                  |
-- >                  V
-- >          +-------+-------+
-- >          |     demux     | For a given MuxBearer there is a single demux thread reading from
-- >          +-------+-------+ the underlying berarer.
-- >                  |
-- >            +---+-+--+--+
-- >           /    |    |   \
-- >           v    v    v    v
-- >         |  | |  | |  | |  | There is a bounded queue for each mode (responder/initiator) per
-- >         |ci| |  | |bi| |br| miniprotocol.
-- >         |ci| |cr| |bi| |br|
-- >         +--+ +--+ +--+ +--+
-- >          |              |
-- >          | CBOR data    |
-- >          V              |
-- >      +---+-------+ Every ingress queue has a dedicated thread which will read
-- >      | muxDuplex | CBOR encoded data from its queue.
-- >      | Initiator |      |
-- >      | ChainSync |      |
-- >      +-----------+      |
-- >                         V
-- >                    +----+------+
-- >                    | muxDuplex |
-- >                    | Responder |
-- >                    | BlockFetch|
-- >                    +-----------+

-- | demux runs as a single separate thread and reads complete 'MuxSDU's from the underlying
-- 'MuxBearer' and forwards it to the matching ingress queueu.
demux :: forall m. (MuxBearer m, MonadSTM m, MonadSay m) => PerMuxSharedState m -> m ()
demux pmss = forever $ do
    (sdu, _) <- Ouroboros.Network.Mux.read (bearerHandle pmss)
    --say $ printf "demuxing sdu on mid %s mode %s" (show $ msId sdu) (show $ msMode sdu)
    -- Notice the mode reversal, ModeResponder is delivered to ModeInitiator and vice versa.
    atomically $ writeTBQueue (ingressQueue (dispatchTable pmss) (msId sdu) (negMiniProtocolMode $ msMode sdu)) (msBlob sdu)

-- | Return the ingress queueu for a given 'MiniProtocolId' and 'MiniProtocolMode'.
ingressQueue :: (MuxBearer m) => MiniProtocolDispatch m -> MiniProtocolId -> MiniProtocolMode -> TBQueue m BL.ByteString
ingressQueue (MiniProtocolDispatch tbl) dis mode =
    case M.lookup (dis, mode) tbl of
         Nothing -> error $ printf "Missing MiniProtocol %s mode %s in dispatch table"
                                   (show dis) (show mode) -- XXX
         Just q  -> q

-- | muxJobs constructs a list of jobs which needs to be started in separate threads by
-- the specific 'MuxBearer' instance.
-- TODO: replace MonadSay with iohk-monitoring-framework.
muxJobs :: (MuxBearer m, MonadSTM m, MonadSay m) =>
    MiniProtocolDescriptions m ->
    MuxBearerHandle m ->
    m [m ()]
muxJobs (MiniProtocolDescriptions udesc) bearer = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    let pmss = PerMuxSS tbl bearer tq
        jobs = [ demux pmss
               , mux pmss
               , muxControl pmss ModeResponder
               , muxControl pmss ModeInitiator
               ]
    mjobs <- mapM (mpsJob pmss) $ M.elems udesc
    return $ jobs ++ concat mjobs

  where
    setupTbl = do
        let ps = [Muxcontrol, DeltaQ] ++ M.keys udesc
        tbl <- foldM addMp M.empty ps
        return $ MiniProtocolDispatch tbl

    addMp t p = do
        a <- atomically $ newTBQueue 2
        b <- atomically $ newTBQueue 2
        return $ M.insert (p, ModeInitiator) a $ M.insert (p, ModeResponder) b t

    mpsJob pmss mpd = do
        w_i <- atomically newEmptyTMVar
        w_r <- atomically newEmptyTMVar

        return [ mpdInitiator mpd $ muxDuplex pmss (mpdId mpd) ModeInitiator w_i
               , mpdResponder mpd $ muxDuplex pmss (mpdId mpd) ModeResponder w_r]

muxControl :: (MuxBearer m, MonadSTM m, MonadSay m) =>
    PerMuxSharedState m ->
    MiniProtocolMode ->
    m ()
muxControl pmss md = do
    w <- atomically newEmptyTMVar
    forever $ do
        -- XXX actual protocol is missing
        blob <- atomically $ readTBQueue (ingressQueue (dispatchTable pmss) Muxcontrol md)
        --say $ printf "muxcontrol mode %s blob len %d" (show md) (BL.length blob)
        atomically $ putTMVar w blob
        atomically $ writeTBQueue (tsrQueue pmss) (TLSRDemand Muxcontrol md (Wanton w))

-- | muxDuplex creates a duplex channel for a specific 'MiniProtocolId' and 'MiniProtocolMode'.
muxDuplex :: (MuxBearer m, MonadSTM m, MonadSay m) =>
    PerMuxSharedState m ->
    MiniProtocolId ->
    MiniProtocolMode ->
    TMVar m BL.ByteString ->
    Duplex m m CBOR.Encoding BS.ByteString
muxDuplex pmss mid md w = uniformDuplex snd_ rcv
  where
    snd_ encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.
        --say $ printf "send mid %s mode %s" (show mid) (show md)
        atomically $ putTMVar w (CBOR.toLazyByteString encoding)
        atomically $ writeTBQueue (tsrQueue pmss) (TLSRDemand mid md (Wanton w))
    rcv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queueu. This is same queue the 'demux' thread writes to.
        blob <- atomically $ readTBQueue (ingressQueue (dispatchTable pmss) mid md)
        --say $ printf "recv mid %s mode %s blob len %d" (show mid) (show md) (BL.length blob)
        if BL.null blob
           then pure Nothing
           else return $ Just $ BL.toStrict blob


-- $servicingsSemantics
-- = Desired Servicing Semantics
--
--  == /Constructing Fairness/
--
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
--  == /Current Caveats/
--
--  1) Not considering how mini-protocol associations are constructed
--     (depending on deployment model this might be resolved within
--     the instantiation of the peer relationship)
--
--  2) Not yet considered notion of orderly termination - this not
--     likely to be used in an operational context, but may be needed
--     for test harness use.
--
--  == /Principle of Operation/
--
--
--  Egress direction (mini protocol instance to remote peer)
--
--  The request for service (the demand) from a mini protocol is
--  encapsulated in a `Wanton`, such `Wanton`s are placed in a (finite)
--  queue (e.g TBMQ) of `TranslocationServiceRequest`s.
--

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
  -- | Handle for underlying bearer
  ,   bearerHandle  :: MuxBearerHandle m
  -- | Egress queue, shared by all miniprotocols
  ,   tsrQueue      :: TBQueue m (TranslocationServiceRequest m)
   -- additional performance info (perhaps)
  }

-- $egress
-- = Egress Path
--
-- > +-----+-----+ +-----+-----+ +-----+-----+ +-----+-----+ Every mode per miniprotocol has a
-- > | muxDuplex | | muxDuplex | | muxDuplex | | muxDuplex | dedicated thread which will
-- > | Initiator | | Responder | | Initiator | | Responder | send ByteStrings of CBOR encoded
-- > | ChainSync | | ChainSync | | BlockFetch| | BlockFetch| data.
-- > +-----+-----+ +-----+-----+ +-----+-----+ +-----+-----+
-- >       |             |             |             |
-- >        \            \            /             /
-- >         -------------------+-------------------
-- >                            | CBOR data
-- >                            V
-- >                          |  | For a given MuxBearer there is a single egress queue shared
-- >                          |ci| among all miniprotocols. To ensure fairness each miniprotocol
-- >                          |cr| can at most have one message in the queue, see
-- >                          +--+ Desired Servicing Semantics.
-- >                           |
-- >                           V
-- >                        +--+--+ The egress queue is served by a dedicated thread which
-- >                        | mux | chops up the CBOR data into MuxSDUs with at most sduSize
-- >                        +-----+ bytes of data in them.
-- >                           |
-- >                           | MuxSDUs
-- >                           |
-- >                           V
-- >                   +-------+--------+
-- >                   | Bearer.write() | MuxBearer implementation specific write
-- >                   +----------------+
-- >                           |
-- >                           | ByteStrings
-- >                           V
-- >                           o

-- | Process the messages from the mini protocols - there is a single
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

-- | Pull a `maxSDU`s worth of data out out the `Wanton` - if there is
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
        -- Note that to preserve bytestream ordering withing a given
        -- miniprotocol the takeTMVar and putTMVar operations
        -- must be inside the same STM transaction.
        do putTMVar (want wanton) rest
           writeTBQueue (tsrQueue pmss) (TLSRDemand mpi md wanton)
      -- return data to send
      pure frag
    let sdu = MuxSDU (RemoteClockModel 0) mpi md (fromIntegral $ BL.length blob) blob
    void $ write (bearerHandle pmss) (cb sdu)
    --paceTransmission tNow
    return ()

  where
    cb sdu ts = sdu {msTimestamp = ts}

{-paceTransmission :: (MuxBearer m) => LocalClockModel m -> m ()
paceTransmission = return () -- -}
