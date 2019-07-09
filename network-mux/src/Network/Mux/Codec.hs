module Network.Mux.Codec where

import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           GHC.Stack

import           Network.Mux.Types


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
encodeMuxSDU :: ProtocolEnum ptcl => MuxSDU ptcl -> BL.ByteString
encodeMuxSDU sdu =
  let hdr = Bin.runPut enc in
  BL.append hdr $ msBlob sdu
  where
    enc = do
        Bin.putWord32be $ unRemoteClockModel $ msTimestamp sdu
        putId (msId sdu) (putMode $ msMode sdu)
        Bin.putWord16be $ fromIntegral $ BL.length $ msBlob sdu

    putId ptcl mode = Bin.putWord16be $ fromProtocolEnum ptcl .|. mode

    putMode :: MiniProtocolMode -> Word16
    putMode ModeInitiator = 0
    putMode ModeResponder = 0x8000


-- | Decode a 'MuSDU' header
--
decodeMuxSDUHeader :: (HasCallStack , ProtocolEnum ptcl)
                   => BL.ByteString -> Either MuxError (MuxSDU ptcl)
decodeMuxSDUHeader buf =
    case Bin.runGetOrFail dec buf of
         Left  (_, _, e)  -> Left $ MuxError MuxDecodeError e callStack
         Right (_, _, ph) ->
             let mode  = getMode $ mshIdAndMode ph
                 mid_m = getId $ mshIdAndMode ph .&. 0x7fff in
             case mid_m of
                  Left  e   -> Left  $ MuxError MuxUnknownMiniProtocol ("id = " ++ show e) callStack
                  Right mid -> Right $ MuxSDU {
                        msTimestamp = mshTimestamp ph
                      , msId = mid
                      , msMode = mode
                      , msLength = mshLength ph
                      , msBlob = BL.empty
                      }
  where
    dec = do
        ts <- Bin.getWord32be
        mid <- Bin.getWord16be
        len <- Bin.getWord16be
        return $ MuxSDUHeader (RemoteClockModel ts) mid len

    getMode mid =
        if mid .&. 0x8000 == 0 then ModeInitiator
                               else ModeResponder

    getId :: ProtocolEnum ptcl => Word16 -> Either Word16 (MiniProtocolId ptcl)
    getId n | Just ptcl <- toProtocolEnum n
            = Right ptcl
    getId a = Left a
