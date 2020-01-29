{-# LANGUAGE NumericUnderscores  #-}
module Network.NTP.Client
where

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
      -- | Bypass all internal threadDelays and trigger a new NTP query.
    , ntpTriggerUpdate    :: IO ()
    , ntpThread           :: Async ()
    }

-- | Setup a NtpClient and run a application that uses that client.
-- The NtpClient is terminated when the application returns.
-- And also the application is terminated when the NtpClient crashes.
withNtpClient :: Tracer IO NtpTrace -> NtpSettings -> (NtpClient -> IO a) -> IO a
withNtpClient tracer ntpSettings action = do
    traceWith tracer NtpTraceStartNtpClient
    ntpStatus <- newTVarIO NtpSyncPending
    withAsync (ntpClientThread tracer ntpSettings ntpStatus) $ \tid -> do
        let client = NtpClient
              { ntpGetStatus = readTVar ntpStatus
              , ntpTriggerUpdate = do
                   traceWith tracer NtpTraceClientActNow
                   atomically $ writeTVar ntpStatus NtpSyncPending
              , ntpThread = tid
              }
        link tid         -- an error in the ntp-client kills the appliction !
        action client

threadDelayInterruptible :: TVar NtpStatus -> Int -> IO ()
threadDelayInterruptible tvar t
    = race_
       ( threadDelay t )
       ( atomically $ do
           s <- readTVar tvar
           check $ s == NtpSyncPending
       )

-- TODO: maybe reset the delaytime if the oneshotClient did one sucessful query
ntpClientThread ::
       Tracer IO NtpTrace
    -> NtpSettings
    -> TVar NtpStatus
    -> IO ()
ntpClientThread tracer ntpSettings ntpStatus = forM_ restartDelay $ \t -> do
    traceWith tracer $ NtpTraceRestartDelay t
    threadDelayInterruptible ntpStatus $ t * 1_000_000
    traceWith tracer NtpTraceRestartingClient
    catchIOError
        (forever $ do
            status <- ntpQuery tracer ntpSettings
            atomically $ writeTVar ntpStatus status
            traceWith tracer NtpTraceClientSleeping
            threadDelayInterruptible ntpStatus $ fromIntegral $ ntpPollDelay ntpSettings
        )
        (\err -> traceWith tracer $ NtpTraceIOError err)
    atomically $ writeTVar ntpStatus NtpSyncUnavailable
    where
        restartDelay :: [Int]
        restartDelay = [0, 5, 10, 20, 60, 180, 600] ++ repeat 600
