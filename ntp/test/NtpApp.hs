module Main
where
import Control.Tracer

import Ntp.Client

settings :: NtpClientSettings
settings = NtpClientSettings
    { ntpServers = ["0.de.pool.ntp.org","0.europe.pool.ntp.org","0.pool.ntp.org"]
    , ntpResponseTimeout = fromInteger 1000000
    , ntpPollDelay       = fromInteger 3000000
    , ntpSelection       = minimum
    }

main :: IO ()
main = do
    _status <- withNtpClient (contramapM (return . show) stdoutTracer) settings
    _<- getLine
    return ()
