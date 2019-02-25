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
import qualified Data.Binary.Get as Bin
import           Data.Bits
import           Data.Word
import qualified Data.ByteString.Lazy as BL
import           Data.Array
import           GHC.Stack

import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM

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



