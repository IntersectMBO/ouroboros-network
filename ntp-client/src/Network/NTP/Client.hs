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
import           System.IO.Error (catchIOError)
import           Control.Monad
import           Control.Tracer

import           Network.NTP.Query
import           Network.NTP.Trace

data NtpClient = NtpClient
    { -- | Query the current NTP status.
      ntpGetStatus        :: STM NtpStatus
      -- | Bypass all internal threadDelays and trigger a new NTP query (non-blocking).
    , ntpTriggerUpdate    :: IO ()
      -- | Perform a query, update and return the NtpStatus (blocking).
    , ntpQueryBlocking    :: IO NtpStatus
    , ntpThread           :: Async ()
    }

-- | Setup a NtpClient and run a application that uses that client.
-- The NtpClient is terminated when the application returns.
-- The application should use waitCatch on ntpThread.
withNtpClient :: Tracer IO NtpTrace -> NtpSettings -> (NtpClient -> IO a) -> IO a
withNtpClient tracer ntpSettings action = do
    traceWith tracer NtpTraceStartNtpClient
    ntpStatus <- newTVarIO NtpSyncPending
    withAsync (ntpClientThread tracer ntpSettings ntpStatus) $ \tid -> do
        let client = NtpClient
              { ntpGetStatus = readTVar ntpStatus
              , ntpTriggerUpdate = do
                  traceWith tracer NtpTraceTriggerUpdate
                  atomically $ writeTVar ntpStatus NtpSyncPending
              , ntpQueryBlocking = do
                  traceWith tracer NtpTraceTriggerUpdate
                  atomically $ writeTVar ntpStatus NtpSyncPending
                  status <- atomically $ do
                      s <- readTVar ntpStatus
                      check $ s /= NtpSyncPending
                      return s
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

-- TODO: maybe reset the delaytime if ntpQuery did one sucessful query
ntpClientThread ::
       Tracer IO NtpTrace
    -> NtpSettings
    -> TVar NtpStatus
    -> IO ()
ntpClientThread tracer ntpSettings ntpStatus = forM_ restartDelay $ \t -> do
    traceWith tracer $ NtpTraceRestartDelay t
    awaitPendingWithTimeout ntpStatus $ t * 1_000_000
    traceWith tracer NtpTraceRestartingClient
    catchIOError queryLoop (\err -> traceWith tracer $ NtpTraceIOError err)
    atomically $ writeTVar ntpStatus NtpSyncUnavailable
    where
        restartDelay :: [Int]
        restartDelay = [0, 5, 10, 20, 60, 180, 600] ++ repeat 600

        queryLoop = ntpQuery tracer ntpSettings >>= \case
            status@(NtpDrift _ ) -> do
                atomically $ writeTVar ntpStatus status
                traceWith tracer NtpTraceClientSleeping
                awaitPendingWithTimeout ntpStatus $ fromIntegral $ ntpPollDelay ntpSettings
                queryLoop
            _ -> return ()
