{-# LANGUAGE NumericUnderscores  #-}
module Network.NTP.Test
where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.Async
import           Control.Monad (forever)
import           Control.Tracer

import           Network.NTP.Client
import           Network.NTP.Query

testClient :: IO ()
testClient = withNtpClient (contramapM (return . show) stdoutTracer) testSettings runApplication
  where
    runApplication ntpClient = race_ getLine $ forever $ do
        status <- atomically $ ntpGetStatus ntpClient
        traceWith stdoutTracer $ show ("main"::String, status)
        threadDelay 10_000_000
        ntpTriggerUpdate ntpClient

testNtpQuery :: IO ()
testNtpQuery = forever $ do
    status <- ntpQuery tracer testSettings
    traceWith stdoutTracer $ show ("main"::String, status)
    threadDelay 10_000_000
    where
        tracer = contramapM (return . show) stdoutTracer

testSettings :: NtpSettings
testSettings = NtpSettings
    { ntpServers = ["0.de.pool.ntp.org", "0.europe.pool.ntp.org", "0.pool.ntp.org"
                   , "1.pool.ntp.org", "2.pool.ntp.org", "3.pool.ntp.org"]
    , ntpResponseTimeout = fromInteger 1_000_000
    , ntpPollDelay       = fromInteger 300_000_000
    , ntpReportPolicy    = minimumOfThree
    }
