{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE LambdaCase          #-}
module Network.NTP.Client (
-- * The API for starting an ntp client-thread.
    withNtpClient
  , NtpSettings(..)
  , NtpClient(..)
  , NtpStatus(..)
  -- ** Low level interface
  -- *** Running an @ntp@ query
  , ntpQuery

  -- * Logging interface
  , NtpTrace(..)
  , IPVersion(..)
  , ResultOrFailure(..)
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM (STM, atomically, check)
import           Control.Concurrent.STM.TVar
import           Control.Monad (when)
import           System.IO.Error (tryIOError)
import           Control.Tracer
import           Data.Void (Void)

import           System.IOManager

import           Network.NTP.Client.Query

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
withNtpClient :: IOManager
              -> Tracer IO NtpTrace
              -> NtpSettings
              -> (NtpClient -> IO a)
              -> IO a
withNtpClient ioManager tracer ntpSettings action = do
    traceWith tracer NtpTraceStartNtpClient
    ntpStatus <- newTVarIO NtpSyncPending
    withAsync (ntpClientThread ioManager tracer ntpSettings ntpStatus) $ \tid -> do
        let client = NtpClient
              { ntpGetStatus = readTVar ntpStatus
              , ntpQueryBlocking = do
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
ntpClientThread
    :: IOManager
    -> Tracer IO NtpTrace
    -> NtpSettings
    -> TVar NtpStatus
    -> IO Void
ntpClientThread ioManager tracer ntpSettings ntpStatus = queryLoop initialErrorDelay
  where
    queryLoop :: Int -> IO Void
    queryLoop errorDelay = tryIOError (ntpQuery ioManager tracer ntpSettings) >>= \case
        Right status@NtpDrift{} -> do
            atomically $ writeTVar ntpStatus status
            -- After a successful query the client sleeps
            -- for the time interval set in `ntpPollDelay`.
            awaitPendingWithTimeout ntpStatus $ fromIntegral $ ntpPollDelay ntpSettings
            queryLoop initialErrorDelay -- Use the initialErrorDelay.
        Right NtpSyncUnavailable -> fastRetry errorDelay
        Right NtpSyncPending -> error "ntpClientThread: impossible happened"
        Left err -> do
            traceWith tracer $ NtpTraceIOError err
            fastRetry errorDelay

    -- When a query was not successful client does a fast retry.
    -- It sleeps for the time defined by `errorDelay`.
    fastRetry errorDelay = do
        atomically $ writeTVar ntpStatus NtpSyncUnavailable
        traceWith tracer $ NtpTraceRestartDelay errorDelay
        awaitPendingWithTimeout ntpStatus $ errorDelay * 1_000_000
        traceWith tracer NtpTraceRestartingClient
        -- Double the error delay but, do not wait more than 600s.
        queryLoop (2 * errorDelay `min` 600)

    initialErrorDelay = 5
