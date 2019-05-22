{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Mux (
      MiniProtocolLimits (..)
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxBearerState (..)
    , MuxError (..)
    , MuxErrorType (..)
    , MuxSDU (..)
    , RemoteClockModel (..)
    , encodeMuxSDU
    , decodeMuxSDUHeader
    , muxBearerSetState
    , muxStart
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           GHC.Stack
import           Text.Printf

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Mux.Interface
import           Ouroboros.Network.Mux.Egress
import           Ouroboros.Network.Mux.Ingress
import           Ouroboros.Network.Mux.Types


-- | muxStart starts a mux bearer for the specified protocols corresponding to
-- one of the provided Versions.
-- TODO: replace MonadSay with iohk-monitoring-framework.
muxStart :: forall m appType ptcl.
            ( MonadAsync m, MonadSay m, MonadSTM m, MonadThrow m, MonadThrow (STM m)
            , MonadMask m , Ord ptcl, Enum ptcl, Bounded ptcl, Show ptcl, MiniProtocolLimits ptcl)
         => MuxApplication appType ptcl m
         -> MuxBearer ptcl m
         -> m ()
muxStart app bearer = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0

    let pmss = PerMuxSS tbl tq bearer
        jobs = [ demux pmss
               , mux cnt pmss
               , muxControl pmss ModeResponder
               , muxControl pmss ModeInitiator
               ]
    mjobs <- sequence [ mpsJob cnt pmss ptcl
                      | ptcl <- [minBound..maxBound] ]

    mask $ \unmask -> do
      as <- traverse (async . unmask) (jobs ++ concat mjobs)
      muxBearerSetState bearer Mature
      unmask (void $ waitAnyCancel as)
      muxBearerSetState bearer Dead

  where
    -- Construct the array of TBQueues, one for each protocol id, and each mode
    setupTbl :: m (MiniProtocolDispatch ptcl m)
    setupTbl = MiniProtocolDispatch
            -- cover full range of type (MiniProtocolId ptcl, MiniProtocolMode)
             . array (minBound, maxBound)
           <$> sequence [ do q <- atomically (newTVar BL.empty)
                             return ((ptcl, mode), q)
                        | ptcl <- [minBound..maxBound]
                        , mode <- [ModeInitiator, ModeResponder] ]


    mpsJob
      :: TVar m Int
      -> PerMuxSharedState ptcl m
      -> ptcl
      -> m [m ()]
    mpsJob cnt pmss mpdId = do

        w_i <- atomically newEmptyTMVar
        w_r <- atomically newEmptyTMVar

        let initiatorChannel :: Channel m BL.ByteString
            initiatorChannel = muxChannel pmss
                                (AppProtocolId mpdId)
                                ModeInitiator
                                w_i cnt

            responderChannel :: Channel m BL.ByteString
            responderChannel = muxChannel pmss
                                (AppProtocolId mpdId)
                                ModeResponder
                                w_r cnt

        return $ case app of
          MuxClientApplication initiator -> [ initiator mpdId initiatorChannel >> mpsJobExit cnt ]
          MuxServerApplication responder -> [ responder mpdId responderChannel >> mpsJobExit cnt ]
          MuxClientAndServerApplication initiator responder
                                          -> [ initiator mpdId initiatorChannel >> mpsJobExit cnt
                                             , responder mpdId responderChannel >> mpsJobExit cnt
                                             ]

    -- cnt represent the number of SDUs that are queued but not yet sent.  Job
    -- threads will be prevented from exiting until all SDUs have been
    -- transmitted unless an exception/error is encounter. In that case all
    -- jobs will be cancelled directly.
    mpsJobExit :: TVar m Int -> m ()
    mpsJobExit cnt = do
        muxBearerSetState bearer Dying
        atomically $ do
            c <- readTVar cnt
            unless (c == 0) retry

muxControl :: (HasCallStack, MonadSTM m, MonadSay m, MonadThrow m, Ord ptcl, Enum ptcl
              , MiniProtocolLimits ptcl)
           => PerMuxSharedState ptcl m
           -> MiniProtocolMode
           -> m ()
muxControl pmss md = do
    _ <- atomically $ do
        buf <- readTVar (ingressQueue (dispatchTable pmss) Muxcontrol md)
        when (buf == BL.empty)
            retry
    throwM $ MuxError MuxControlProtocolError "MuxControl message on mature MuxBearer" callStack

-- | muxChannel creates a duplex channel for a specific 'MiniProtocolId' and 'MiniProtocolMode'.
muxChannel :: (MonadSTM m, MonadSay m, MonadThrow m, Ord ptcl, Enum ptcl, Show ptcl
              , MiniProtocolLimits ptcl , HasCallStack) =>
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
        -- This check is dependant on the good will of the sender and a receiver can't
        -- assume that it will never receive messages larger than maximumMessageSize.
        --say $ printf "send mid %s mode %s" (show mid) (show md)
        when (BL.length encoding > maximumMessageSize mid) $
            throwM $ MuxError MuxTooLargeMessage
                (printf "Attempting to send a message of size %d on %s %s" (BL.length encoding)
                        (show mid) (show $ md))
                callStack
        atomically $ modifyTVar' cnt (+ 1)
        atomically $ putTMVar w encoding
        atomically $ writeTBQueue (tsrQueue pmss) (TLSRDemand mid md (Wanton w))
    recv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queueu. This is the same queue the 'demux' thread writes to.
        blob <- atomically $ do
            let q = ingressQueue (dispatchTable pmss) mid md
            blob <- readTVar q
            if blob == BL.empty
                then retry
                else writeTVar q BL.empty >> return blob
        -- say $ printf "recv mid %s mode %s blob len %d" (show mid) (show md) (BL.length blob)
        return $ Just blob

muxBearerSetState :: (MonadSTM m, Ord ptcl, Enum ptcl, Bounded ptcl)
                  => MuxBearer ptcl m
                  -> MuxBearerState
                  -> m ()
muxBearerSetState bearer newState = atomically $ writeTVar (state bearer) newState
