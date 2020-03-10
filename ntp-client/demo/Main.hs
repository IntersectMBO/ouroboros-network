{-# LANGUAGE NumericUnderscores  #-}
module Main
where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad (forever)
import           Control.Tracer

import           Network.NTP.Client (withNtpClient
                                    , NtpSettings(..)
                                    , NtpClient(..))

main :: IO ()
main = withNtpClient (showTracing stdoutTracer) testSettings runApplication
  where
    runApplication ntpClient = do
        link $ ntpThread ntpClient  -- propergate any errors in the NTP thread.
        race_ getLine $ forever $ do
            -- loose patience and perform an instant query.
            status <- ntpQueryBlocking ntpClient
            traceWith (showTracing stdoutTracer) ("main", status)
            -- sleep 10 seconds
            threadDelay 10_000_000

testSettings :: NtpSettings
testSettings = NtpSettings
    { ntpServers = ["0.de.pool.ntp.org", "0.europe.pool.ntp.org", "0.pool.ntp.org"
                   , "1.pool.ntp.org", "2.pool.ntp.org", "3.pool.ntp.org"]
    , ntpResponseTimeout = fromInteger 1_000_000
    , ntpPollDelay       = fromInteger 300_000_000
    }
