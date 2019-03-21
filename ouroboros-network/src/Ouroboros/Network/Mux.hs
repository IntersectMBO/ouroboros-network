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
    , MuxVersion
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
         => [SomeVersion]
         -> (SomeVersion -> Maybe (MiniProtocolDescriptions ptcl m))
         -> MuxBearer ptcl m
         -> MuxStyle
         -> Maybe (Maybe SomeException -> m ())  -- Optional callback for result
         -> m ()
muxStart versions mpds bearer style rescb_m = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0
    udesc <- case style of
                       StyleClient -> Just <$> muxClient versions mpds bearer
                       StyleServer -> muxServer versions mpds bearer

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
    -- Job threads will be prevented from exiting until all SDUs have been transmitted unless
    -- an exception/error is encounter. In that case all jobs will be cancelled directly.
    mpsJobExit cnt = do
        muxBearerSetState bearer Dying
        atomically $ do
            c <- readTVar cnt
            unless (c == 0) retry

muxControl :: (HasCallStack, MonadSTM m, MonadSay m, MonadThrow m, Ord ptcl, Enum ptcl)
           => PerMuxSharedState ptcl m
           -> MiniProtocolMode
           -> m ()
muxControl pmss md = do
    _ <- atomically $ readTBQueue (ingressQueue (dispatchTable pmss) Muxcontrol md)
    throwM $ MuxError MuxControlProtocolError "MuxControl message on mature MuxBearer" callStack

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

-- | Initiate version negotiation with the peer the MuxBearer is connected to
muxClient :: (MonadAsync m, MonadFork m, MonadSay m, MonadSTM m, MonadThrow m,
            Ord ptcl, Enum ptcl, Bounded ptcl, HasCallStack)
        => [SomeVersion]
        -> (SomeVersion -> Maybe (MiniProtocolDescriptions ptcl m))
        -> MuxBearer ptcl m
        -> m (MiniProtocolDescriptions ptcl m)
muxClient versions mpds_fn bearer = do
    let msg = MsgInitReq versions
        blob = toLazyByteString $ encodeControlMsg msg
        sdu = MuxSDU (RemoteClockModel 0) Muxcontrol ModeInitiator (fromIntegral $ BL.length blob) blob

    void $ write bearer sdu
    (rsp, _) <- Ouroboros.Network.Mux.Types.read bearer
    if msId rsp /= Muxcontrol || msMode rsp /= ModeResponder
       then throwM $ MuxError MuxUnknownMiniProtocol "invalid muxInit rsp id or mode" callStack
       else do
           let rspMsg_e = CBOR.deserialiseFromBytes (decodeControlMsg versions) (msBlob rsp)
           case rspMsg_e of
                Left e -> throwM e
                Right (_, MsgInitRsp version) ->
                    -- verify that rsp version matches one of our proposals
                    case mpds_fn version of
                         Nothing   -> throwM $ MuxError MuxControlProtocolError
                                               "muxInit invalid version selected" callStack
                         Just mpds -> return mpds
                Right (_, MsgInitReq _) -> throwM $ MuxError MuxControlProtocolError
                                                    "muxInit response as request" callStack
                Right (_, MsgInitFail e) -> throwM $ MuxError MuxControlNoMatchingVersion e callStack

-- | Wait for the connected peer to initiate version negotiation.
muxServer :: (MonadAsync m, MonadFork m, MonadSay m, MonadSTM m, MonadThrow m,
             Ord ptcl, Enum ptcl, Bounded ptcl, HasCallStack)
          => [SomeVersion]
          -> (SomeVersion -> Maybe (MiniProtocolDescriptions ptcl m))
          -> MuxBearer ptcl m
          -> m (Maybe (MiniProtocolDescriptions ptcl m))
muxServer localVersions mpds_fn bearer = do
    (req, _) <- Ouroboros.Network.Mux.Types.read bearer
    if msId req /= Muxcontrol || msMode req /= ModeInitiator
       then throwM $ MuxError MuxUnknownMiniProtocol "invalid muxInit req id or mode" callStack
       else do
           let reqMsg_e = CBOR.deserialiseFromBytes (decodeControlMsg localVersions) (msBlob req)
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
                           return (mpds_fn version)

                Right (_, MsgInitRsp _) -> throwM $ MuxError MuxControlProtocolError
                                                    "muxInit request as response" callStack
                Right (_, MsgInitFail _) -> throwM $ MuxError MuxControlProtocolError
                                                     "muxInit fail in request" callStack

  where
    sndSdu msg = do
        let blob = toLazyByteString $ encodeControlMsg msg
            sdu = MuxSDU (RemoteClockModel 0) Muxcontrol ModeResponder
                         (fromIntegral $ BL.length blob) blob
        void $ write bearer sdu

