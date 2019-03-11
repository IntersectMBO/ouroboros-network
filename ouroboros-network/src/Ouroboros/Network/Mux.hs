{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Ouroboros.Network.Mux (
      MiniProtocolDescription (..)
    , MiniProtocolDescriptions
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxBearerState (..)
    , MuxError (..)
    , MuxErrorType (..)
    , MuxStyle (..)
    , MuxSDU (..)
    , NetworkMagic (..)
    , Version (..)
    , VersionNumber (..)
    , RemoteClockModel (..)
    , encodeMuxSDU
    , decodeMuxSDUHeader
    , muxBearerSetState
    , muxStart
    ) where

import qualified Codec.CBOR.Read as CBOR
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           Data.List (intersect)
import           Data.Maybe (fromJust)
import           GHC.Stack

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Mux.Control
import           Ouroboros.Network.Mux.Egress
import           Ouroboros.Network.Mux.Ingress
import           Ouroboros.Network.Mux.Types


-- | muxStart starts a mux bearer for the specified protocols corresponding to
-- one of the provided Versions.
-- TODO: replace MonadSay with iohk-monitoring-framework.
muxStart :: ( MonadAsync m, MonadFork m, MonadSay m, MonadSTM m, MonadThrow m
            , Ord ptcl, Enum ptcl, Bounded ptcl)
        => [(Version, MiniProtocolDescriptions ptcl m)]
        -> MuxBearer ptcl m
        -> MuxStyle
        -> Maybe (Maybe SomeException -> m ())  -- Optional callback for result
        -> m ()
muxStart verMpds bearer style rescb_m = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0
    let vfn = if style == StyleClient then muxClient
                                      else muxServer
    (_, udesc) <- vfn verMpds bearer

    let pmss = PerMuxSS tbl tq bearer
        jobs = [ demux pmss
               , mux cnt pmss
               , muxControl pmss ModeResponder
               , muxControl pmss ModeInitiator
               ]
    mjobs <- sequence [ mpsJob cnt pmss udesc ptcl
                      | ptcl <- [minBound..maxBound] ]
    aids <- mapM async $ jobs ++ concat mjobs
    muxBearerSetState bearer Mature
    watcher aids

  where
    watcher as = do
        (_,r) <- waitAnyCatchCancel as
        close bearer
        muxBearerSetState bearer Dead
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

    mpsJob cnt pmss udesc mpdId = do
        let mpd = udesc mpdId
        w_i <- atomically newEmptyTMVar
        w_r <- atomically newEmptyTMVar

        return [ mpdInitiator mpd (muxChannel pmss (AppProtocolId mpdId) ModeInitiator w_i cnt)
                     >> mpsJobExit cnt
               , mpdResponder mpd (muxChannel pmss (AppProtocolId mpdId) ModeResponder w_r cnt)
                     >> mpsJobExit cnt
               ]

    -- cnt represent the number of SDUs that are queued but not yet sent.
    -- job threads will be prevented from exiting until all SDUs have been transmitted.
    mpsJobExit cnt = do
        muxBearerSetState bearer Dying
        atomically $ do
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

muxBearerSetState :: (MonadSTM m, Ord ptcl, Enum ptcl, Bounded ptcl)
                  => MuxBearer ptcl m
                  -> MuxBearerState
                  -> m ()
muxBearerSetState bearer newState = atomically $ writeTVar (state bearer) newState


muxClient :: (MonadAsync m, MonadFork m, MonadSay m, MonadSTM m, MonadThrow m,
            Ord ptcl, Enum ptcl, Bounded ptcl, HasCallStack)
        => [(Version, MiniProtocolDescriptions ptcl m)]
        -> MuxBearer ptcl m
        -> m (Version, MiniProtocolDescriptions ptcl m)
muxClient verMpds bearer = do
    let versions = map fst verMpds
        msg = MsgInitReq versions
        blob = toLazyByteString $ encodeCtrlMsg msg
        sdu = MuxSDU (RemoteClockModel 0) Muxcontrol ModeInitiator (fromIntegral $ BL.length blob) blob

    void $ write bearer sdu
    (rsp, _) <- Ouroboros.Network.Mux.Types.read bearer
    if msId rsp /= Muxcontrol || msMode rsp /= ModeResponder
       then throwM $ MuxError MuxUnknownMiniProtocol "invalid muxInit rsp id or mode" callStack
       else do
           let rspMsg_e = CBOR.deserialiseFromBytes decodeCtrlMsg (msBlob rsp)
           case rspMsg_e of
                Left e -> throwM e
                Right (_, MsgInitRsp version) ->
                    -- verify that rsp version matches one of our proposals
                    case lookup version verMpds of
                         Nothing   -> error "muxInit invalid version selected"
                         Just mpds -> return (version, mpds)
                Right (_, MsgInitReq _) -> error "muxInit response as request"
                Right (_, MsgInitFail e) -> throwM $ MuxError MuxControlNoMatchingVersion e callStack

muxServer :: (MonadAsync m, MonadFork m, MonadSay m, MonadSTM m, MonadThrow m,
            Ord ptcl, Enum ptcl, Bounded ptcl, HasCallStack)
        => [(Version, MiniProtocolDescriptions ptcl m)]
        -> MuxBearer ptcl m
        -> m (Version, MiniProtocolDescriptions ptcl m)
muxServer verMpds bearer = do
    let localVersions = map fst verMpds
    (req, _) <- Ouroboros.Network.Mux.Types.read bearer
    if msId req /= Muxcontrol || msMode req /= ModeInitiator
       then throwM $ MuxError MuxUnknownMiniProtocol "invalid muxInit req id or mode" callStack
       else do
           let reqMsg_e = CBOR.deserialiseFromBytes decodeCtrlMsg (msBlob req)
           case reqMsg_e of
                Left e -> throwM e
                Right (_, MsgInitReq remoteVersions) -> do
                    let matchingVersions = localVersions `intersect` remoteVersions
                    if null matchingVersions
                       then do
                           sndSdu $ MsgInitFail $ "No matching version, try " ++
                                show (last localVersions)

                           throwM $ MuxError MuxControlNoMatchingVersion "no matching version" callStack
                       else do
                           let version = maximum matchingVersions
                               msg  = MsgInitRsp version

                           sndSdu msg
                           return (version, fromJust $ lookup version verMpds)
                Right (_, MsgInitRsp _) -> error "muxInit request as response"
                Right (_, MsgInitFail _) -> error "muxInit fail"

  where
    sndSdu msg = do
        let blob = toLazyByteString $ encodeCtrlMsg msg
            sdu = MuxSDU (RemoteClockModel 0) Muxcontrol ModeResponder
                         (fromIntegral $ BL.length blob) blob
        void $ write bearer sdu

