module Main
where
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Control.Tracer

import Network.NTP.Client

settings :: NtpClientSettings
settings = NtpClientSettings
    { ntpServers = ["0.de.pool.ntp.org","0.europe.pool.ntp.org","0.pool.ntp.org"]
    , ntpResponseTimeout = fromInteger 1000000
    , ntpPollDelay       = fromInteger 3000000
    , ntpSelection       = minimum
    }

main :: IO ()
main = do
    ntpClient <- forkNtpClient (contramapM (return . show) stdoutTracer) settings
    race_ getLine $ forever $ do
        status <- ntpGetStatus ntpClient
        traceWith stdoutTracer $ show ("main",status)
        threadDelay 3000000
    ntpAbort ntpClient
    return ()
