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

import           System.IOManager


main :: IO ()
main =
    withIOManager $ \ioManager ->
      withNtpClient ioManager (showTracing stdoutTracer) testSettings runApplication
  where
    runApplication ntpClient = do
        link $ ntpThread ntpClient  -- propergate any errors in the NTP thread.
        forever (threadDelay maxBound)

testSettings :: NtpSettings
testSettings = NtpSettings
    { ntpServers = [ "0.de.pool.ntp.org"
                   , "0.europe.pool.ntp.org"
                   , "0.pool.ntp.org"
                   , "1.pool.ntp.org"
                   , "2.pool.ntp.org"
                   , "3.pool.ntp.org"
                   ]
    , ntpRequiredNumberOfResults = 5
    , ntpResponseTimeout = fromInteger 1_000_000
    , ntpPollDelay       = fromInteger 10_000_000
    }
