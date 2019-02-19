{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Mux.Egress (
      encodeMuxSDU
    , mux
    -- $egress
    -- $servicingsSemantics
    ) where

import           Control.Monad
import qualified Data.Binary.Put as Bin
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Word

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Network.Mux.Types

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

    putId Muxcontrol mode   = Bin.putWord16be $ 0 .|. mode
    putId DeltaQ mode       = Bin.putWord16be $ 1 .|. mode
    putId ChainSync mode    = Bin.putWord16be $ 2 .|. mode
    putId BlockFetch mode   = Bin.putWord16be $ 3 .|. mode
    putId TxSubmission mode = Bin.putWord16be $ 4 .|. mode

    putMode :: MiniProtocolMode -> Word16
    putMode ModeInitiator = 0
    putMode ModeResponder = 0x8000

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
-- >                          |  | For a given Mux Bearer there is a single egress queue shared
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
-- >                   | Bearer.write() | Mux Bearer implementation specific write
-- >                   +----------------+
-- >                           |
-- >                           | ByteStrings
-- >                           V
-- >                           o

-- | Process the messages from the mini protocols - there is a single
-- shared FIFO that contains the items of work. This is processed so
-- that each active demand gets a `maxSDU`s work of data processed
-- each time it gets to the front of the queue
mux :: (MonadSTM m)
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
processSingleWanton :: MonadSTM m
                    => PerMuxSharedState m
                    -> MiniProtocolId
                    -> MiniProtocolMode
                    -> Wanton m
                    -> m ()
processSingleWanton pmss mpi md wanton = do
    maxSDU <- sduSize pmss
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
    void $ write pmss sdu
    --paceTransmission tNow



