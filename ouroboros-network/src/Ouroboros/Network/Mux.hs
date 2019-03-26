{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Ouroboros.Network.Mux (
      MiniProtocolDescription (..)
    , MiniProtocolDescriptions
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxError (..)
    , MuxErrorType (..)
    , MuxSDU (..)
    , RemoteClockModel (..)
    , encodeMuxSDU
    , decodeMuxSDUHeader
    , muxStart
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime (Time)
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           Data.Word

import           Ouroboros.Network.Channel

import           Ouroboros.Network.Mux.Egress
import           Ouroboros.Network.Mux.Ingress
import           Ouroboros.Network.Mux.Types

-- | muxStart starts a mux bearer for the specified protocols with the provided read and write
-- functions.
-- TODO: replace MonadSay with iohk-monitoring-framework.
muxStart :: (MonadAsync m, MonadFork m, MonadSay m, MonadSTM m, Ord ptcl, Enum ptcl, Bounded ptcl)
        => MiniProtocolDescriptions ptcl m
        -> (MuxSDU ptcl -> m (Time m))          -- Write function
        -> m (MuxSDU ptcl, Time m)              -- Read function
        -> m Word16                             -- SDU size function
        -> m ()                                 -- Close function
        -> Maybe (Maybe SomeException -> m ())  -- Optional callback for result
        -> m (ThreadId m)
muxStart udesc wfn rfn sdufn close  rescb_m = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0
    let pmss = PerMuxSS tbl tq wfn rfn sdufn
        jobs = [ demux pmss
               , mux cnt pmss
               , muxControl pmss ModeResponder
               , muxControl pmss ModeInitiator
               ]
    mjobs <- sequence [ mpsJob cnt pmss (udesc ptcl)
                      | ptcl <- [minBound..maxBound] ]
    aids <- mapM async $ jobs ++ concat mjobs
    fork $ watcher aids

  where
    watcher as = do
        (_,r) <- waitAnyCatchCancel as
        close
        case rescb_m of
             Nothing ->
                 case r of
                      Left  e -> say $ "Mux Bearer died due to " ++ show e
                      Right _ -> return ()
             Just rescb ->
                 case r of
                      Left  e -> rescb $ Just e
                      Right _ -> rescb Nothing


    -- Construct the array of TBQueues, one for each protocol id, and each mode
    setupTbl = MiniProtocolDispatch
            -- cover full range of type (MiniProtocolId ptcl, MiniProtocolMode)
             . array (minBound, maxBound)
           <$> sequence [ do q <- atomically (newTBQueue 2)
                             return ((ptcl, mode), q)
                        | ptcl <- [minBound..maxBound]
                        , mode <- [ModeInitiator, ModeResponder] ]

    mpsJob cnt pmss mpd = do
        w_i <- atomically newEmptyTMVar
        w_r <- atomically newEmptyTMVar

        return [ mpdInitiator mpd (muxChannel pmss (mpdId mpd) ModeInitiator w_i cnt)
                     >> mpsJobExit cnt
               , mpdResponder mpd (muxChannel pmss (mpdId mpd) ModeResponder w_r cnt)
                     >> mpsJobExit cnt
               ]

    -- cnt represent the number of SDUs that are queued but not yet sent.
    -- job threads will be prevented from exiting until all SDUs have been transmitted.
    mpsJobExit cnt = atomically $ do
        c <- readTVar cnt
        unless (c == 0) retry

muxControl :: (MonadSTM m, MonadSay m, Ord ptcl, Enum ptcl) =>
    PerMuxSharedState ptcl m ->
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
muxChannel :: (MonadSTM m, MonadSay m, Ord ptcl, Enum ptcl) =>
    PerMuxSharedState ptcl m ->
    MiniProtocolId ptcl ->
    MiniProtocolMode ->
    TMVar m BL.ByteString ->
    TVar m Int ->
    Channel m BL.ByteString
muxChannel pmss mid md w cnt =
    Channel {send, recv}
  where
    send encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.
        --say $ printf "send mid %s mode %s" (show mid) (show md)
        atomically $ modifyTVar' cnt (+ 1)
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

