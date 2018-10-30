{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.Stream.Server
    ( Server
    , newServer
    , streamServer
    ) where

import Numeric.Natural (Natural)
import Streaming (Stream, Of (..), streamFold)
import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically)

import Protocol.Core
import Protocol.Stream.Type

-- |
-- Streaming server.
data Server m rng a = Server
    { serverStream       :: rng -> Stream (Of a) m ()
    , serverReadWindow   :: m Natural
    , serverUpdateWindow :: Natural -> m ()
    }

-- |
-- Create a new streaming server from a stream.
--
newServer
    :: (rng -> Stream (Of a) IO ())
    -> IO (Server IO rng a)
newServer serverStream = do
    var <- atomically $ newTVar 0
    return $ Server { serverStream
                    , serverReadWindow    = atomically (readTVar var)
                    , serverUpdateWindow  = atomically . writeTVar var
                    }
    
-- |
-- Run @'Server'@ until its stream has finished.
--
streamServer
    :: forall m rng a. Monad m
    => Server m rng a
    -> Peer StreamProtocol (StreamMessage rng a) ('Awaiting 'StIdle) ('Finished 'StDone) m ()
streamServer Server{..} =
    await $ \req ->
        case req of
            MsgRequest range window ->
                streamFold done_ hole construct (serverStream range)
  where
    -- finish the stream with @'MsgStreamEnd'@
    done_ :: () 
         -> Peer StreamProtocol (StreamMessage rng a) ('Yielding 'StBusy) ('Finished 'StDone) m ()
    done_ x = out MsgStreamEnd (done x)

    -- consume a single value from the stream
    construct
        :: Of a (Peer StreamProtocol (StreamMessage rng a) ('Yielding 'StBusy) next m ())
        -> Peer StreamProtocol (StreamMessage rng a) ('Yielding 'StBusy) next m ()
    construct (a :> next) = hole $ do
        window <- serverReadWindow
        if window <= 0
            -- release control flow to the consumer and ask to
            -- update the window size; after a response from the client update
            -- the window with the new size and resume streaming.
            then pure $ over MsgRenewWindow $ await $ \req' ->
                case req' of
                    MsgUpdateWindow -> hole $ do
                        serverUpdateWindow (pred window)
                        pure $ part (MsgData a) next
            -- update the window size, send next @'MsgData'@, and loop over the
            -- stream
            else do
                serverUpdateWindow (pred window)
                pure $ part (MsgData a) next
