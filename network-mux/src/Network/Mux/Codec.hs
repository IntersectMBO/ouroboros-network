{-# LANGUAGE NamedFieldPuns #-}

module Network.Mux.Codec where

import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Bld
import Data.ByteString.Builder.Extra qualified as Bld
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
-- > |                        transmission time                      |
-- > +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-- > |d|    mini-protocol number     |             length            |
-- > +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--
-- All fields are in big endian byte order.
--
-- * transmission time: time when the SDU was sent
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
    Bld.toLazyByteStringWith
      (Bld.untrimmedStrategy hdrLength hdrLength)
      (msBlob sdu) hdr
  where
    hdrLength = fromIntegral msHeaderLength

    hdr = Bld.word32BE (unRemoteClockModel (msTimestamp sdu))
       <> Bld.word16BE (putNumAndMode (msNum sdu) (msDir sdu))
       <> Bld.word16BE (fromIntegral (BL.length (msBlob sdu)))

    putNumAndMode :: MiniProtocolNum -> MiniProtocolDir -> Word16
    putNumAndMode (MiniProtocolNum n) InitiatorDir = n
    putNumAndMode (MiniProtocolNum n) ResponderDir = n .|. 0x8000


-- | Decode a 'MuSDU' header.  A left inverse of 'encodeSDU'.
--
decodeSDU :: BL.ByteString -> Either Error SDU
decodeSDU buf
    | BL.length buf < msHeaderLength
    = Left $ SDUDecodeError "not enough bytes"
    | mhLength > 0
    = Right $ SDU {
          msHeader = SDUHeader {
              mhTimestamp,
              mhNum,
              mhDir,
              mhLength
            }
        , msBlob   = BL.empty
        }
    | otherwise
    = Left $ SDUDecodeError "short SDU"
  where
    hdr = BL.toStrict $ BL.take msHeaderLength buf

    byte :: Int -> Word32
    byte i = fromIntegral $ BS.index hdr i

    mhTimestamp = RemoteClockModel $
                      byte 0 `shiftL` 24
                  .|. byte 1 `shiftL` 16
                  .|. byte 2 `shiftL`  8
                  .|. byte 3
    a           = fromIntegral $ byte 4 `shiftL` 8 .|. byte 5 :: Word16
    mhLength    = fromIntegral $ byte 6 `shiftL` 8 .|. byte 7
    mhNum       = MiniProtocolNum $ a .&. 0x7fff
    mhDir       = if a .&. 0x8000 == 0 then InitiatorDir
                                       else ResponderDir
