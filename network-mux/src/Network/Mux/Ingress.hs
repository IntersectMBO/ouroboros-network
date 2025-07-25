{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Mux.Ingress
  ( -- $ingress
    demuxer
  ) where

import Data.Array
import Data.ByteString.Builder.Internal (lazyByteStringInsert,
           lazyByteStringThreshold)
import Data.ByteString.Lazy qualified as BL
import Data.List (nub)
import Data.Strict.Tuple (pattern (:!:))

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Tracer (Tracer)

import Network.Mux.Timeout
import Network.Mux.Trace
import Network.Mux.Types as Mx


flipMiniProtocolDir :: MiniProtocolDir -> MiniProtocolDir
flipMiniProtocolDir InitiatorDir = ResponderDir
flipMiniProtocolDir ResponderDir = InitiatorDir

-- $ingress
-- = Ingress Path
--
-- >                  ●
-- >                  │
-- >                  │ ByteStrings
-- >                  │
-- >         ░░░░░░░░░▼░░░░░░░░░
-- >         ░┌───────────────┐░
-- >         ░│ Bearer.read() │░ Mux Bearer implementation (Socket, Pipes, etc.)
-- >         ░└───────────────┘░
-- >         ░░░░░░░░░│░░░░░░░░░
-- >                 ░│░         SDUs
-- >         ░░░░░░░░░▼░░░░░░░░░
-- >         ░┌───────────────┐░
-- >         ░│     demux     │░ For a given Mux Bearer there is a single demux
-- >         ░└───────┬───────┘░ thread reading from the underlying bearer.
-- >         ░░░░░░░░░│░░░░░░░░░
-- >                 ░│░
-- >        ░░░░░░░░░░▼░░░░░░░░░░
-- >        ░ ╭────┬────┬─────╮ ░ There is a limited queue (in bytes) for each mode
-- >        ░ │    │    │     │ ░ (responder/initiator) per miniprotocol. Overflowing
-- >        ░ ▼    ▼    ▼     ▼ ░ a queue is a protocol violation and a
-- >        ░│  │ │  │ │  │ │  │░ IngressQueueOverRun exception is thrown
-- >        ░│ci│ │  │ │bi│ │br│░ and the bearer torn down.
-- >        ░│ci│ │cr│ │bi│ │br│░
-- >        ░└──┘ └──┘ └──┘ └──┘░ Every ingress queue has a dedicated thread which will
-- >        ░░│░░░░│░░░░│░░░░│░░░ read application encoded data from its queue.
-- >          │    │    │    │
-- >           application data
-- >          │    │    │    │
-- >          ▼    │    │    ▼
-- > ┌───────────┐ │    │  ┌───────────┐
-- > │ muxDuplex │ │    │  │ muxDuplex │
-- > │ Initiator │ │    │  │ Responder │
-- > │ ChainSync │ │    │  │ BlockFetch│
-- > └───────────┘ │    │  └───────────┘
-- >               ▼    ▼
-- >    ┌───────────┐  ┌───────────┐
-- >    │ muxDuplex │  │ muxDuplex │
-- >    │ Responder │  │ Initiator │
-- >    │ ChainSync │  │ BlockFetch│
-- >    └───────────┘  └───────────┘

-- | Each peer's multiplexer has some state that provides both
-- de-multiplexing details (for despatch of incoming messages to mini
-- protocols) and for dispatching incoming SDUs.  This is shared
-- between the muxIngress and the bearerIngress processes.
--
data MiniProtocolDispatch m =
     MiniProtocolDispatch
       !(Array MiniProtocolNum (Maybe MiniProtocolIx))
       !(Array (MiniProtocolIx, MiniProtocolDir)
               (MiniProtocolDispatchInfo m))

data MiniProtocolDispatchInfo m =
     MiniProtocolDispatchInfo
       !(IngressQueue m)
       !Int
   | MiniProtocolDirUnused


-- | demux runs as a single separate thread and reads complete 'SDU's from
-- the underlying Bearer and forwards it to the matching ingress queue.
demuxer :: (MonadAsync m, MonadFork m, MonadMask m, MonadThrow (STM m),
            MonadTimer m)
      => [MiniProtocolState mode m]
      -> Tracer m BearerTrace
      -> Bearer m
      -> m void
demuxer ptcls tracer bearer =
  let !dispatchTable = setupDispatchTable ptcls in
  withTimeoutSerial $ \timeout ->
  forever $ do
    (sdu, _) <- Mx.read bearer tracer timeout
    -- say $ printf "demuxing sdu on mid %s mode %s lenght %d " (show $ msId sdu) (show $ msDir sdu)
    --             (BL.length $ msBlob sdu)
    case lookupMiniProtocol dispatchTable (msNum sdu)
                            -- Notice the mode reversal, ResponderDir is
                            -- delivered to InitiatorDir and vice versa:
                            (flipMiniProtocolDir $ msDir sdu) of
      Nothing   -> throwIO (UnknownMiniProtocol (msNum sdu))
      Just MiniProtocolDirUnused ->
                   throwIO (InitiatorOnly (msNum sdu))
      Just (MiniProtocolDispatchInfo q qMax) ->
        atomically $ do
          len :!: buf <- readTVar q
          let !len' = len + BL.length (msBlob sdu)
          if len' <= fromIntegral qMax
              then do
                let !buf' = if len == 0
                               then -- Don't copy the payload if the queue was empty
                                 lazyByteStringInsert $ msBlob sdu
                               else -- Copy payloads smaller than 128 bytes
                                 buf <> lazyByteStringThreshold 128 (msBlob sdu)
                writeTVar q $ len' :!: buf'
              else throwSTM $ IngressQueueOverRun (msNum sdu) (msDir sdu)

lookupMiniProtocol :: MiniProtocolDispatch m
                   -> MiniProtocolNum
                   -> MiniProtocolDir
                   -> Maybe (MiniProtocolDispatchInfo m)
lookupMiniProtocol (MiniProtocolDispatch pnumArray ptclArray) pnum pdir
  | inRange (bounds pnumArray) pnum
  , Just mpid <- pnumArray ! pnum = Just (ptclArray ! (mpid, pdir))
  | otherwise                   = Nothing

-- | Construct the table that maps 'MiniProtocolNum' and 'MiniProtocolDir' to
-- 'MiniProtocolDispatchInfo'. Use 'lookupMiniProtocol' to index it.
--
setupDispatchTable :: forall mode m.
                      [MiniProtocolState mode m] -> MiniProtocolDispatch m
setupDispatchTable ptcls =
    MiniProtocolDispatch pnumArray ptclArray
  where
    -- The 'MiniProtocolNum' space is sparse but we don't want a huge single
    -- table if we use large protocol numbers. So we use a two level mapping.
    --
    -- The first array maps 'MiniProtocolNum' to a dense space of intermediate
    -- integer indexes. These indexes are meaningless outside of the context of
    -- this table. Then we use the index and the 'MiniProtocolDir' for the
    -- second table.
    --
    pnumArray :: Array MiniProtocolNum (Maybe MiniProtocolIx)
    pnumArray =
      array (minpnum, maxpnum) $
            -- Fill in Nothing first to cover any unused ones.
            [ (pnum, Nothing)    | pnum <- [minpnum..maxpnum] ]

            -- And override with the ones actually used.
         ++ [ (pnum, Just pix)   | (pnum, pix) <- zip pnums [0..] ]

    ptclArray :: Array (MiniProtocolIx, MiniProtocolDir)
                       (MiniProtocolDispatchInfo m)
    ptclArray =
      array ((minpix, InitiatorDir), (maxpix, ResponderDir)) $
            -- Fill in MiniProtocolDirUnused first to cover any unused ones.
            [ ((pix, dir), MiniProtocolDirUnused)
            | (pix, dir) <- range ((minpix, InitiatorDir),
                                   (maxpix, ResponderDir)) ]

             -- And override with the ones actually used.
         ++ [ ((pix, dir), MiniProtocolDispatchInfo q qMax)
            | MiniProtocolState {
                miniProtocolInfo =
                  MiniProtocolInfo {
                    miniProtocolNum,
                    miniProtocolDir,
                    miniProtocolLimits
                  },
                miniProtocolIngressQueue = q
              } <- ptcls
            , let pix  =
                   case pnumArray ! miniProtocolNum of
                     Just a  -> a
                     -- This error is impossible to trigger - note that
                     -- `pnumArray` is constructed to ensure that every
                     -- `miniProtocolNum` in `ptcls` indexes to a `Just` value.
                     Nothing -> error ("setupDispatchTable: impossible: missing " ++ show miniProtocolNum)
                  dir      = protocolDirEnum miniProtocolDir
                  qMax     = maximumIngressQueue miniProtocolLimits
            ]

    -- The protocol numbers actually used, in the order of the first use within
    -- the 'ptcls' list. The order does not matter provided we do it
    -- consistently between the two arrays.
    pnums   = nub $ map (miniProtocolNum . miniProtocolInfo) ptcls

    -- The dense range of indexes of used protocol numbers.
    minpix, maxpix :: MiniProtocolIx
    minpix  = 0
    maxpix  = fromIntegral (length pnums - 1)

    -- The sparse range of protocol numbers
    minpnum, maxpnum :: MiniProtocolNum
    minpnum = minimum pnums
    maxpnum = maximum pnums
