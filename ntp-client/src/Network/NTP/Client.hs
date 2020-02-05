{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE LambdaCase          #-}
module Network.NTP.Client (
    NtpClient(..)
  , withNtpClient
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM (STM, atomically, check)
import           Control.Concurrent.STM.TVar
import           Control.Monad (when)
import           System.IO.Error (catchIOError)
import           Control.Tracer
import           Data.Void (Void)

import           Network.NTP.Query
import           Network.NTP.Trace


-- | 'NtpClient' which recieves updates of the wall clcok drift every
-- 'ntpPollDelay'.  It also allows to force engaging in ntp protocol.
--
data NtpClient = NtpClient
    { -- | Query the current NTP status.
      ntpGetStatus     :: STM NtpStatus
      -- | Force to update the ntp state, unless an ntp query is already
      -- running.  This is a blocking operation.
    , ntpQueryBlocking :: IO NtpStatus
      -- | Ntp client thread
    , ntpThread        :: Async Void
    }


-- | Setup a NtpClient and run an application that uses provided 'NtpClient'.
-- The 'NtpClient' is terminated when the callback returns.  The application
-- can 'waitCatch' on 'ntpThread'.
--
withNtpClient :: Tracer IO NtpTrace -> NtpSettings -> (NtpClient -> IO a) -> IO a
withNtpClient tracer ntpSettings action = do
    traceWith tracer NtpTraceStartNtpClient
    ntpStatus <- newTVarIO NtpSyncPending
    withAsync (ntpClientThread tracer ntpSettings ntpStatus) $ \tid -> do
        let client = NtpClient
              { ntpGetStatus = readTVar ntpStatus
              , ntpQueryBlocking = do
                  traceWith tracer NtpTraceTriggerUpdate
                  -- trigger an update, unless an ntp query is not already
                  -- running
                  atomically $ do
                    status <- readTVar ntpStatus
                    when (status /= NtpSyncPending)
                      $ writeTVar ntpStatus NtpSyncPending
                  -- block until the state changes
                  atomically $ do
                      status <- readTVar ntpStatus
                      check $ status /= NtpSyncPending
                      return status
              , ntpThread = tid
              }
        action client

awaitPendingWithTimeout :: TVar NtpStatus -> Int -> IO ()
awaitPendingWithTimeout tvar t
    = race_
       ( threadDelay t )
       ( atomically $ do
           s <- readTVar tvar
           check $ s == NtpSyncPending
       )

-- | ntp client thread which wakes up every 'ntpPollDelay' to make ntp queries.
-- It can be woken up earlier by setting 'NptStatus' to 'NtpSyncPending'.
--
-- TODO: Reset the delay time if ntpQuery did one successful query.
ntpClientThread ::
       Tracer IO NtpTrace
    -> NtpSettings
    -> TVar NtpStatus
    -> IO Void
ntpClientThread tracer ntpSettings ntpStatus = go 0
  where
    -- outer loop of the ntp client.  If inner loop errors we restart after the
    -- 'delay' seconds
    go :: Int -> IO Void
    go delay | delay <= 0 = do
      queryLoop
        `catchIOError` (traceWith tracer . NtpTraceIOError)
      atomically $ writeTVar ntpStatus NtpSyncUnavailable
      go 5
    go delay = do
      traceWith tracer $ NtpTraceRestartDelay delay
      awaitPendingWithTimeout ntpStatus $ delay * 1_000_000
      traceWith tracer NtpTraceRestartingClient
      queryLoop
        `catchIOError` (traceWith tracer . NtpTraceIOError)
      atomically $ writeTVar ntpStatus NtpSyncUnavailable
      go (2 * delay `min` 600) 

    -- inner loop of the ntp client.  Note that 'nptQuery' will return either
    -- 'NptDrift' or 'NptSyncUnavailable'.
    queryLoop :: IO ()
    queryLoop = ntpQuery tracer ntpSettings >>= \case
      status@NtpDrift{} -> do
        atomically $ writeTVar ntpStatus status
        traceWith tracer NtpTraceClientSleeping
        awaitPendingWithTimeout ntpStatus $ fromIntegral $ ntpPollDelay ntpSettings
        queryLoop
      status@NtpSyncUnavailable ->
        -- we need to update the status even if the result is
        -- 'NptSyncUnavailable', so that the thread blocked on it will be
        -- waken up.
        atomically $ writeTVar ntpStatus status
      NtpSyncPending -> error "ntpClientThread: impossible happend"
