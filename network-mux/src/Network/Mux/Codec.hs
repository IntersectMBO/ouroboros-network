{-# LANGUAGE NamedFieldPuns #-}

module Network.Mux.Codec where

import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin
import Data.Bits
import Data.ByteString.Lazy qualified as BL
import Data.Word

import Network.Mux.Trace
import Network.Mux.Types


-- | Encode a 'SDU' as a 'ByteString'.
--
-- > Binary format used by 'encodeSDU' and 'decodeSDUHeader'
-- >  0                   1                   2                   3
-- >  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
-- > +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- > |         send cookie           |         echo cookie           |
-- > +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- > |d|    mini-protocol number     |             length            |
-- > +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--
-- All fields are in big endian byte order.
--
-- * send cookie: an opaque per-SDU 16-bit identifier drawn from a
--   PRNG by the local muxer at SDU construction.
-- * echo cookie: the freshest 'send cookie' the local muxer observed
--   from its peer; the peer uses this on receipt to look up the local
--   send time and compute an RTT sample.
-- * @d@: mini-protocol direction (`MiniProtocolDir`):
--
--     * 1 - initiator direction
--     * 0 - responder direction
--
-- * mini-protocol number (`MiniProtocolNum`)
-- * length: length of the payload
--
encodeSDU :: SDU -> BL.ByteString
encodeSDU sdu =
  let hdr = Bin.runPut enc in
  BL.append hdr $ msBlob sdu
  where
    enc = do
        Bin.putWord16be $ unCookie $ msSendCookie sdu
        Bin.putWord16be $ unCookie $ msEchoCookie sdu
        Bin.putWord16be $ putNumAndMode (msNum sdu) (msDir sdu)
        Bin.putWord16be $ fromIntegral $ BL.length $ msBlob sdu

    putNumAndMode :: MiniProtocolNum -> MiniProtocolDir -> Word16
    putNumAndMode (MiniProtocolNum n) InitiatorDir = n
    putNumAndMode (MiniProtocolNum n) ResponderDir = n .|. 0x8000


-- | Decode a 'MuSDU' header.  A left inverse of 'encodeSDU'.
--
decodeSDU :: BL.ByteString -> Either Error SDU
decodeSDU buf =
    case Bin.runGetOrFail dec buf of
         Left  (_, _, e)  -> Left $ SDUDecodeError e
         Right (_, _, h) ->
           if mhLength h > 0
             then
               Right $ SDU {
                     msHeader = h
                   , msBlob   = BL.empty
                   }
             else Left $ SDUDecodeError "short SDU"
  where
    dec = do
        mhSendCookie <- Cookie <$> Bin.getWord16be
        mhEchoCookie <- Cookie <$> Bin.getWord16be
        a <- Bin.getWord16be
        mhLength <- Bin.getWord16be
        let mhDir = getDir a
            mhNum = MiniProtocolNum (a .&. 0x7fff)
        return $ SDUHeader {
            mhSendCookie,
            mhEchoCookie,
            mhNum,
            mhDir,
            mhLength
          }

    getDir mid =
        if mid .&. 0x8000 == 0 then InitiatorDir
                               else ResponderDir
