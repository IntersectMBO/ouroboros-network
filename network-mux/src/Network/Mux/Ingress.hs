{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Mux.Ingress (
    -- $ingress
      demuxer
    ) where

import           Data.Array
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           GHC.Stack
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer hiding (timeout)

import           Network.Mux.Timeout
import           Network.Mux.Trace
import           Network.Mux.Types


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
-- >                 ░│░         MuxSDUs
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
-- >        ░│  │ │  │ │  │ │  │░ MuxIngressQueueOverRun exception is thrown
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
-- de-multiplexing details (for despatch of incoming mesages to mini
-- protocols) and for dispatching incoming SDUs.  This is shared
-- between the muxIngress and the bearerIngress processes.
--
data MiniProtocolDispatch m =
     MiniProtocolDispatch
       !(Array (MiniProtocolNum, MiniProtocolDir) (Maybe MiniProtocolIx))
       !(Array (MiniProtocolIx, MiniProtocolDir)
               (MiniProtocolDispatchInfo m))

data MiniProtocolDispatchInfo m =
     MiniProtocolDispatchInfo
       !(IngressQueue m)
       !Int
   | MiniProtocolDirUnused


-- | demux runs as a single separate thread and reads complete 'MuxSDU's from
-- the underlying Mux Bearer and forwards it to the matching ingress queue.
demuxer :: (MonadAsync m, MonadFork m, MonadMask m, MonadThrow (STM m),
            MonadTimer m, MonadTime m, HasCallStack)
      => [MiniProtocolState mode m]
      -> MuxBearer m
      -> m void
demuxer ptcls bearer =
  let !dispatchTable = setupDispatchTable ptcls in
  withTimeoutSerial $ \timeout ->
  forever $ do
    (sdu, _) <- Network.Mux.Types.read bearer timeout
    -- say $ printf "demuxing sdu on mid %s mode %s lenght %d " (show $ msId sdu) (show $ msDir sdu)
    --             (BL.length $ msBlob sdu)
    case lookupMiniProtocol dispatchTable (msNum sdu)
                            -- Notice the mode reversal, ResponderDir is
                            -- delivered to InitiatorDir and vice versa:
                            (flipMiniProtocolDir $ msDir sdu) of
      Nothing   -> throwM (MuxError MuxUnknownMiniProtocol
                          ("id = " ++ show (msNum sdu)) callStack)
      Just MiniProtocolDirUnused ->
                   throwM (MuxError MuxInitiatorOnly
                          ("id = " ++ show (msNum sdu) ++ ("dir = " ++ show (msDir sdu))) callStack)
      Just (MiniProtocolDispatchInfo q qMax) ->
        atomically $ do
          buf <- readTVar q
          if BL.length buf + BL.length (msBlob sdu) <= fromIntegral qMax
              then writeTVar q $ BL.append buf (msBlob sdu)
              else throwM $ MuxError MuxIngressQueueOverRun
                                (printf "Ingress Queue overrun on %s %s"
                                 (show $ msNum sdu) (show $ msDir sdu))
                                callStack

lookupMiniProtocol :: MiniProtocolDispatch m
                   -> MiniProtocolNum
                   -> MiniProtocolDir
                   -> Maybe (MiniProtocolDispatchInfo m)
lookupMiniProtocol (MiniProtocolDispatch codeTbl ptclTbl) code mode
  | inRange (bounds codeTbl) (code, mode)
  , Just mpid <- codeTbl ! (code, mode) = Just (ptclTbl ! (mpid, mode))
  | otherwise                           = Nothing

-- Construct the array of TBQueues, one for each protocol id, and each mode
setupDispatchTable :: [MiniProtocolState mode m] -> MiniProtocolDispatch m
setupDispatchTable ptcls =
    MiniProtocolDispatch
      (array ((mincode, InitiatorDir), (maxcode, ResponderDir)) $
             [ ((pix, dir), Nothing)
             | (pix, dir) <- range ((mincode, InitiatorDir),
                                     (maxcode, ResponderDir)) ]
          ++ [ ((code, dir), Just pix)
             | (pix, ptcl) <- zip [0..] ptcls
             , let code = miniProtocolNum (miniProtocolInfo ptcl)
                   MiniProtocolState {
                     miniProtocolInfo =
                       MiniProtocolInfo {
                         miniProtocolDir
                       }
                   }    = ptcl
                   dir  = protocolDirEnum miniProtocolDir
             ])
      (array ((minpix, InitiatorDir), (maxpix, ResponderDir)) $
             [ ((pix, dir), MiniProtocolDirUnused)
             | (pix, dir) <- range ((minpix, InitiatorDir),
                                    (maxpix, ResponderDir)) ]
          ++ [ ((pix, dir), MiniProtocolDispatchInfo q qMax)
             | (pix, ptcl) <- zip [0..] ptcls
             , let MiniProtocolState {
                     miniProtocolIngressQueue = q,
                     miniProtocolInfo =
                       MiniProtocolInfo {
                         miniProtocolDir,
                         miniProtocolLimits
                       }
                   }    = ptcl
                   qMax = maximumIngressQueue miniProtocolLimits
                   dir  = protocolDirEnum miniProtocolDir
             ])
  where
    minpix = 0
    maxpix = fromIntegral (length ptcls - 1)

    codes   = map (miniProtocolNum . miniProtocolInfo) ptcls
    mincode = minimum codes
    maxcode = maximum codes

