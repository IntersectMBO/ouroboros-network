{-# LANGUAGE NamedFieldPuns #-}

module Network.Mux.Codec where

import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           GHC.Stack

import           Network.Mux.Types
import           Network.Mux.Trace


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
--
encodeMuxSDU :: MuxSDU -> BL.ByteString
encodeMuxSDU sdu =
  let hdr = Bin.runPut enc in
  BL.append hdr $ msBlob sdu
  where
    enc = do
        Bin.putWord32be $ unRemoteClockModel $ msTimestamp sdu
        Bin.putWord16be $ putNumAndMode (msNum sdu) (msMode sdu)
        Bin.putWord16be $ fromIntegral $ BL.length $ msBlob sdu

    putNumAndMode :: MiniProtocolNum -> MiniProtocolMode -> Word16
    putNumAndMode (MiniProtocolNum n) ModeInitiator = n
    putNumAndMode (MiniProtocolNum n) ModeResponder = n .|. 0x8000


-- | Decode a 'MuSDU' header.  A left inverse of 'encodeMuxSDU'.
--
decodeMuxSDU :: HasCallStack
             => BL.ByteString -> Either MuxError MuxSDU
decodeMuxSDU buf =
    case Bin.runGetOrFail dec buf of
         Left  (_, _, e)  -> Left $ MuxError MuxDecodeError e callStack
         Right (_, _, h) ->
             Right $ MuxSDU {
                   msHeader = h
                 , msBlob   = BL.empty
                 }
  where
    dec = do
        mhTimestamp <- RemoteClockModel <$> Bin.getWord32be
        a <- Bin.getWord16be
        mhLength <- Bin.getWord16be
        let mhMode = getMode a
            mhNum  = MiniProtocolNum (a .&. 0x7fff)
        return $ MuxSDUHeader {
            mhTimestamp,
            mhNum,
            mhMode,
            mhLength
          }

    getMode mid =
        if mid .&. 0x8000 == 0 then ModeInitiator
                               else ModeResponder
