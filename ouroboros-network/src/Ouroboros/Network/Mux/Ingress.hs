{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Mux.Ingress (
      decodeMuxSDUHeader
    , demux
    , ingressQueue
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import qualified Data.Binary.Get as Bin
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Text.Printf

import           Ouroboros.Network.Mux.Types

negMiniProtocolMode :: MiniProtocolMode -> MiniProtocolMode
negMiniProtocolMode ModeInitiator = ModeResponder
negMiniProtocolMode ModeResponder = ModeInitiator

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

-- | demux runs as a single separate thread and reads complete 'MuxSDU's from the underlying
-- 'MuxBearer' and forwards it to the matching ingress queueu.
demux :: forall m. (MuxBearer m, MonadSTM m, MonadSay m) => PerMuxSharedState m -> m ()
demux pmss = forever $ do
    (sdu, _) <- Ouroboros.Network.Mux.Types.read (bearerHandle pmss)
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



