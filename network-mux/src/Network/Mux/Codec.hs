{-# LANGUAGE NamedFieldPuns #-}

module Network.Mux.Codec where

import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin
import Data.Bits
import Data.ByteString.Lazy qualified as BL
import Data.Word

import Network.Mux.Trace
import Network.Mux.Types


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
-- All fields are in big endian byte order.
--
encodeMuxSDU :: MuxSDU -> BL.ByteString
encodeMuxSDU sdu =
  let hdr = Bin.runPut enc in
  BL.append hdr $ msBlob sdu
  where
    enc = do
        Bin.putWord32be $ unRemoteClockModel $ msTimestamp sdu
        Bin.putWord16be $ putNumAndMode (msNum sdu) (msDir sdu)
        Bin.putWord16be $ fromIntegral $ BL.length $ msBlob sdu

    putNumAndMode :: MiniProtocolNum -> MiniProtocolDir -> Word16
    putNumAndMode (MiniProtocolNum n) InitiatorDir = n
    putNumAndMode (MiniProtocolNum n) ResponderDir = n .|. 0x8000


-- | Decode a 'MuSDU' header.  A left inverse of 'encodeMuxSDU'.
--
decodeMuxSDU :: BL.ByteString -> Either MuxError MuxSDU
decodeMuxSDU buf =
    case Bin.runGetOrFail dec buf of
         Left  (_, _, e)  -> Left $ MuxError MuxDecodeError e
         Right (_, _, h) ->
           if mhLength h > 0
             then
               Right $ MuxSDU {
                     msHeader = h
                   , msBlob   = BL.empty
                   }
             else Left $ MuxError MuxDecodeError "short SDU"
  where
    dec = do
        mhTimestamp <- RemoteClockModel <$> Bin.getWord32be
        a <- Bin.getWord16be
        mhLength <- Bin.getWord16be
        let mhDir  = getDir a
            mhNum  = MiniProtocolNum (a .&. 0x7fff)
        return $ MuxSDUHeader {
            mhTimestamp,
            mhNum,
            mhDir,
            mhLength
          }

    getDir mid =
        if mid .&. 0x8000 == 0 then InitiatorDir
                               else ResponderDir
