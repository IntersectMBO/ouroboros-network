{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Ouroboros.Network.Mux (
      MiniProtocolDescription (..)
    , MiniProtocolDescriptions (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxSDU (..)
    , RemoteClockModel (..)
    , encodeMuxSDU
    , decodeMuxSDUHeader
    , muxJobs
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.Word

import           Ouroboros.Network.Channel

import           Ouroboros.Network.Mux.Egress
import           Ouroboros.Network.Mux.Ingress
import           Ouroboros.Network.Mux.Types

-- | muxJobs constructs a list of jobs which needs to be started in separate threads by
-- the specific Mux Bearer instance.
-- TODO: replace MonadSay with iohk-monitoring-framework.
muxJobs :: (MonadSTM m, MonadSay m) =>
    MiniProtocolDescriptions m ->
    (MuxSDU -> m (Time m)) ->
    m (MuxSDU, Time m) ->
    m Word16 ->
    m [m ()]
muxJobs (MiniProtocolDescriptions udesc) wfn rfn sdufn = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    let pmss = PerMuxSS tbl tq wfn rfn sdufn
        jobs = [ demux pmss
               , mux pmss
               , muxControl pmss ModeResponder
               , muxControl pmss ModeInitiator
               ]
    mjobs <- mapM (mpsJob pmss) $ M.elems udesc
    return $ jobs ++ concat mjobs

  where
    setupTbl = do
        let ps = [Muxcontrol, DeltaQ] ++ M.keys udesc
        tbl <- foldM addMp M.empty ps
        return $ MiniProtocolDispatch tbl

    addMp t p = do
        a <- atomically $ newTBQueue 2
        b <- atomically $ newTBQueue 2
        return $ M.insert (p, ModeInitiator) a $ M.insert (p, ModeResponder) b t

    mpsJob pmss mpd = do
        w_i <- atomically newEmptyTMVar
        w_r <- atomically newEmptyTMVar

        return [ mpdInitiator mpd $ muxChannel pmss (mpdId mpd) ModeInitiator w_i
               , mpdResponder mpd $ muxChannel pmss (mpdId mpd) ModeResponder w_r]

muxControl :: (MonadSTM m, MonadSay m) =>
    PerMuxSharedState m ->
    MiniProtocolMode ->
    m ()
muxControl pmss md = do
    w <- atomically newEmptyTMVar
    forever $ do
        -- XXX actual protocol is missing
        blob <- atomically $ readTBQueue (ingressQueue (dispatchTable pmss) Muxcontrol md)
        --say $ printf "muxcontrol mode %s blob len %d" (show md) (BL.length blob)
        atomically $ putTMVar w blob
        atomically $ writeTBQueue (tsrQueue pmss) (TLSRDemand Muxcontrol md (Wanton w))

-- | muxChannel creates a duplex channel for a specific 'MiniProtocolId' and 'MiniProtocolMode'.
muxChannel :: (MonadSTM m, MonadSay m) =>
    PerMuxSharedState m ->
    MiniProtocolId ->
    MiniProtocolMode ->
    TMVar m BL.ByteString ->
    Channel m BL.ByteString
muxChannel pmss mid md w =
    Channel {send, recv}
  where
    send encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.
        --say $ printf "send mid %s mode %s" (show mid) (show md)
        atomically $ putTMVar w encoding
        atomically $ writeTBQueue (tsrQueue pmss) (TLSRDemand mid md (Wanton w))
    recv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queueu. This is the same queue the 'demux' thread writes to.
        blob <- atomically $ readTBQueue (ingressQueue (dispatchTable pmss) mid md)
        --say $ printf "recv mid %s mode %s blob len %d" (show mid) (show md) (BL.length blob)
        if BL.null blob
           then pure Nothing
           else return $ Just blob

