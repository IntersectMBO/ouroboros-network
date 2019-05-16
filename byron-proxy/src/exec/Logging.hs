{-# LANGUAGE OverloadedStrings #-}

module Logging where

import Control.Concurrent (myThreadId)
import Control.Tracer (Tracer (..))
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX (getCurrentTime)

import qualified Cardano.BM.Configuration.Model as Monitoring (setupFromRepresentation)
import qualified Cardano.BM.Data.BackendKind as Monitoring
import qualified Cardano.BM.Data.Configuration as Monitoring (Representation (..), parseRepresentation)
import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Output as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring
import qualified Cardano.BM.Setup as Monitoring (withTrace)
import qualified Cardano.BM.Trace as Monitoring (Trace)

-- | Set up logging using an optional configuration file.
withLogging
  :: Maybe FilePath -- ^ Config file
  -> Text           -- ^ Logger name
  -> (Monitoring.Trace IO Text -> IO t)
  -> IO t
withLogging mLoggerConfig name k = do
  -- Set up logging. If there's a config file we read it, otherwise use a
  -- default.
  loggerConfig <- case mLoggerConfig of
    Nothing -> pure defaultLoggerConfig
    Just fp -> Monitoring.parseRepresentation fp
  -- iohk-monitoring uses some MVar for configuration, which corresponds to
  -- the "Representation" which we call config.
  loggerConfig' <- Monitoring.setupFromRepresentation loggerConfig
  Monitoring.withTrace loggerConfig' name k

-- | `withTrace` from the monitoring framework gives us a trace that
-- works on `LogObjects`. `convertTrace` will make it into a `Trace IO Text`
-- which fills in the `LogObject` details.
--
-- It's not a contramap, because it requires grabbing the thread id and
-- the current time.
convertTrace
  :: Monitoring.Trace IO Text
  -> Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text)
convertTrace trace = case trace of
  Tracer f -> Tracer $ \(name, sev, text) -> do
    tid <- pack . show <$> myThreadId
    now <- getCurrentTime
    let logMeta    = Monitoring.LOMeta
                       { Monitoring.tstamp = now
                       , Monitoring.tid = tid
                       , Monitoring.severity = sev
                       , Monitoring.privacy = Monitoring.Public
                       }
        logContent = Monitoring.LogMessage text
        logObject  = Monitoring.LogObject name logMeta logContent
    f logObject

-- | It's called `Representation` but is closely related to the `Configuration`
-- from iohk-monitoring. The latter has to do with `MVar`s. It's all very
-- weird.
defaultLoggerConfig :: Monitoring.Representation
defaultLoggerConfig = Monitoring.Representation
  { Monitoring.minSeverity     = Monitoring.Debug
  , Monitoring.rotation        = Nothing
  , Monitoring.setupScribes    = [stdoutScribe]
  , Monitoring.defaultScribes  = [(Monitoring.StdoutSK, "stdout")]
  , Monitoring.setupBackends   = [Monitoring.KatipBK]
  , Monitoring.defaultBackends = [Monitoring.KatipBK]
  , Monitoring.hasEKG          = Nothing
  , Monitoring.hasPrometheus   = Nothing
  , Monitoring.hasGUI          = Nothing
  , Monitoring.options         = mempty
  }
  where
  stdoutScribe = Monitoring.ScribeDefinition
    { Monitoring.scKind     = Monitoring.StdoutSK
    , Monitoring.scFormat   = Monitoring.ScText
    , Monitoring.scName     = "stdout"
    , Monitoring.scPrivacy  = Monitoring.ScPublic
    , Monitoring.scRotation = Nothing
    }
