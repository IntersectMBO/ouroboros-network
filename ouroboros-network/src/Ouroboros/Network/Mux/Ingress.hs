{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
import           Data.Word
import qualified Data.ByteString.Lazy as BL
import           Data.Array

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
-- >          | Bearer.read() | Mux Bearer implementation (Socket, Pipes, etc.)
-- >          +---------------+
-- >                  |
-- >                  | MuxSDUs
-- >                  |
-- >                  V
-- >          +-------+-------+
-- >          |     demux     | For a given Mux Bearer there is a single demux thread reading from
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
decodeMuxSDUHeader :: ProtocolEnum ptcl
                   => BL.ByteString -> Maybe (MuxSDU ptcl)
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

    getId :: ProtocolEnum ptcl => Word16 -> MiniProtocolId ptcl
    getId n | Just ptcl <- toProtocolEnum n
            = ptcl
    getId a = error $ "unknow miniprotocol " ++ show a -- XXX

-- | demux runs as a single separate thread and reads complete 'MuxSDU's from the underlying
-- Mux Bearer and forwards it to the matching ingress queueu.
demux :: (MonadSTM m, MonadSay m, Ord ptcl, Enum ptcl)
      => PerMuxSharedState ptcl m -> m ()
demux pmss = forever $ do
    (sdu, _) <- Ouroboros.Network.Mux.Types.read pmss
    --say $ printf "demuxing sdu on mid %s mode %s" (show $ msId sdu) (show $ msMode sdu)
    -- Notice the mode reversal, ModeResponder is delivered to ModeInitiator and vice versa.
    atomically $ writeTBQueue (ingressQueue (dispatchTable pmss) (msId sdu) (negMiniProtocolMode $ msMode sdu)) (msBlob sdu)

-- | Return the ingress queueu for a given 'MiniProtocolId' and 'MiniProtocolMode'.
ingressQueue :: (MonadSTM m, Ord ptcl, Enum ptcl)
             => MiniProtocolDispatch ptcl m
             -> MiniProtocolId ptcl
             -> MiniProtocolMode
             -> TBQueue m BL.ByteString
ingressQueue (MiniProtocolDispatch tbl) dis mode =
    tbl ! (dis, mode)
    -- We can use array indexing here, because we constructed the dispatch
    -- table to cover the full range of the type given by 'MiniProtocolId ptcl'



