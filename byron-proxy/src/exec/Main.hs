{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Monad (forM_)
import Control.Tracer (Tracer (..), contramap, traceWith)
import qualified Data.ByteString.Lazy as Lazy (fromStrict)
import Data.Functor.Contravariant (Op (..))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getCurrentTime)
import qualified Options.Applicative as Opt
import System.Random (StdGen, getStdGen, randomR)

import qualified Cardano.BM.Configuration.Model as Monitoring (setupFromRepresentation)
import qualified Cardano.BM.Data.BackendKind as Monitoring
import qualified Cardano.BM.Data.Configuration as Monitoring (Representation (..), parseRepresentation)
import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Output as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring
import qualified Cardano.BM.Setup as Monitoring (withTrace)
import qualified Cardano.BM.Trace as Monitoring (Trace)

import qualified Pos.Binary.Class as CSL (decode)
import qualified Pos.Chain.Block as CSL (Block, BlockHeader (..), genesisBlock0, headerHash)
import qualified Pos.Chain.Lrc as CSL (genesisLeaders)
import qualified Pos.Chain.Block as CSL (recoveryHeadersMessage, streamWindow)
import qualified Pos.Chain.Update as CSL (lastKnownBlockVersion, updateConfiguration)
import qualified Pos.Configuration as CSL (networkConnectionTimeout)
import qualified Pos.Chain.Genesis as CSL.Genesis (Config)
import qualified Pos.Chain.Genesis as CSL (configEpochSlots, configGenesisHash,
                                           configProtocolConstants, configProtocolMagic,
                                           configBlockVersionData)
import qualified Pos.Diffusion.Full as CSL (FullDiffusionConfiguration (..))
import qualified Pos.Infra.Diffusion.Types as CSL

import qualified Pos.Infra.Network.CLI as CSL (NetworkConfigOpts (..), intNetworkConfigOpts, networkConfigOption)
import qualified Pos.Launcher.Configuration as CSL (ConfigurationOptions (..), HasConfigurations, withConfigurations)
import qualified Pos.Client.CLI.Options as CSL (configurationOptionsParser)

import Pos.Util.Trace (Trace)
import qualified Pos.Util.Trace.Named as Trace (LogNamed (..), appendName, named)
import qualified Pos.Util.Trace
import qualified Pos.Util.Wlog as Wlog

import qualified Network.Socket as Network

import qualified Ouroboros.Byron.Proxy.DB as DB
import Ouroboros.Byron.Proxy.Main

import DB (DBConfig (..), seedWithGenesis, withDB)
import IPC (runVersionedClient, runVersionedServer)

-- | The main action using the proxy, index, and immutable database: download
-- the best known chain from the proxy and put it into the database, over and
-- over again.
--
-- No exception handling is done.
byronProxyMain
  :: Tracer IO String
  -> DB.DB IO
  -> ByronProxy
  -> IO x
byronProxyMain tracer db bp = getStdGen >>= mainLoop Nothing

  where

  waitForNext
    :: Maybe (BestTip CSL.BlockHeader)
    -> STM (Either (BestTip CSL.BlockHeader) Atom)
  waitForNext mBt = do
    mBt' <- bestTip bp
    if mBt == mBt'
    -- If recvAtom retires then the whole STM will retry and we'll check again
    -- for the best tip to have changed.
    then fmap Right (recvAtom bp)
    else case mBt' of
        Nothing -> retry
        Just bt -> pure (Left bt)

  mainLoop :: Maybe (BestTip CSL.BlockHeader) -> StdGen -> IO x
  mainLoop mBt rndGen = do
    -- Wait until the best tip has changed from the last one we saw. That can
    -- mean the header changed and/or the list of peers who announced it
    -- changed.
    next <- atomically $ waitForNext mBt
    case next of
      -- TODO we don't get to know from where it was received. Problem? Maybe
      -- not.
      Right atom -> do
        traceWith tracer $ mconcat
          [ "Got atom: "
          , show atom
          ]
        mainLoop mBt rndGen
      Left bt -> do
        -- Get the tip from the database.
        -- It must not be empty; the DB must be seeded with the genesis block.
        -- FIXME throw exception, don't use error.
        tip <- DB.readTip db
        (isEBB, tipSlot, tipHash) <- case tip of
          DB.TipGenesis -> error "database is empty"
          DB.TipEBB   slot hash _ -> pure (True, slot, hash)
          DB.TipBlock slot bytes -> case DB.decodeFull CSL.decode (Lazy.fromStrict bytes) of
            Left cborError -> error "failed to decode block"
            Right (blk :: CSL.Block) -> pure (False, slot, CSL.headerHash blk)
        -- Pick a peer from the list of announcers at random and download
        -- the chain.
        let (peer, rndGen') = pickRandom rndGen (btPeers bt)
        traceWith tracer $ mconcat
          [ "Using tip with hash "
          , show tipHash
          , " at slot "
          , show tipSlot
          , if isEBB then " (EBB)" else ""
          ]
        traceWith tracer $ mconcat
          [ "Downloading the chain with tip hash "
          , show tipHash
          , " from "
          , show peer
          ]
        _ <- downloadChain
              bp
              peer
              (CSL.headerHash (btTip bt))
              [tipHash]
              streamer
        mainLoop (Just bt) rndGen'

  -- If it ends at an EBB, the EBB will _not_ be written. The tip will be the
  -- parent of the EBB.
  -- This should be OK.
  streamer :: CSL.StreamBlocks CSL.Block IO ()
  streamer = CSL.StreamBlocks
    { CSL.streamBlocksMore = \blocks -> DB.appendBlocks db $ \dbwrite -> do
        -- List comes in newest-to-oldest order.
        let orderedBlocks = NE.toList (NE.reverse blocks)
        forM_ orderedBlocks (DB.appendBlock dbwrite)
        pure streamer
    , CSL.streamBlocksDone = pure ()
    }

  pickRandom :: StdGen -> NonEmpty t -> (t, StdGen)
  pickRandom rndGen ne =
    let (idx, rndGen') = randomR (0, NE.length ne - 1) rndGen
    in  (ne NE.!! idx, rndGen')

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
    tid <- Text.pack . show <$> myThreadId
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

-- | `Tracer` comes from the `contra-tracer` package, but cardano-sl still
-- works with the cardano-sl-util definition of the same thing.
mkCSLTrace
  :: Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text)
  -> Trace IO (Trace.LogNamed (Wlog.Severity, Text))
mkCSLTrace tracer = case tracer of
  Tracer f -> Pos.Util.Trace.Trace $ Op $ \namedI ->
    let logName    = Text.intercalate (Text.pack ".") (Trace.lnName namedI)
        (sev, txt) = Trace.lnItem namedI
    in  f (logName, convertSeverity sev, txt)

  where

  -- | Diffusion layer uses log-waper severity, iohk-monitoring uses its own.
  convertSeverity :: Wlog.Severity -> Monitoring.Severity
  convertSeverity sev = case sev of
    Wlog.Debug   -> Monitoring.Debug
    Wlog.Info    -> Monitoring.Info
    Wlog.Notice  -> Monitoring.Notice
    Wlog.Warning -> Monitoring.Warning
    Wlog.Error   -> Monitoring.Error

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
  , Monitoring.hasGUI          = Nothing
  , Monitoring.options         = mempty
  }
  where
  stdoutScribe = Monitoring.ScribeDefinition
    { Monitoring.scKind     = Monitoring.StdoutSK
    , Monitoring.scName     = "stdout"
    , Monitoring.scPrivacy  = Monitoring.ScPublic
    , Monitoring.scRotation = Nothing
    }

-- Problem: optparse-applicative doesn't seem to allow for choices of options
-- (due to its applicative nature). Ideally we'd have core options, then a
-- set of required options for Byron source, and required options for Shelley
-- source, which must be exclusive.
-- Can't `Alternative` do this?

data ByronProxyOptions = ByronProxyOptions
  { bpoDatabasePath                :: !FilePath
  , bpoIndexPath                   :: !FilePath
  , bpoLoggerConfigPath            :: !(Maybe FilePath)
    -- ^ Optional file path; will use default configuration if none given.
  , bpoCardanoConfigurationOptions :: !CSL.ConfigurationOptions
  , bpoServerOptions               :: !ServerOptions
  , bpoClientOptions               :: !(Maybe ClientOptions)
  }

data ServerOptions = ServerOptions
  { soHostName    :: !Network.HostName
  , soServiceName :: !Network.ServiceName
  }

type ClientOptions = Either ShelleyClientOptions ByronClientOptions

data ByronClientOptions = ByronClientOptions
  { bcoCardanoNetworkOptions       :: !CSL.NetworkConfigOpts
    -- ^ To use with `intNetworkConfigOpts` to get a `NetworkConfig` from
    -- cardano-sl, required in order to run a diffusion layer.
  }

data ShelleyClientOptions = ShelleyClientOptions
  { scoHostName          :: !Network.HostName
    -- ^ Of remote peer
  , scoServiceName       :: !Network.ServiceName
    -- ^ Of remote peer
  }

-- | Parser for command line options.
cliParser :: Opt.Parser ByronProxyOptions
cliParser = ByronProxyOptions
  <$> cliDatabasePath
  <*> cliIndexPath
  <*> cliLoggerConfigPath
  <*> cliCardanoConfigurationOptions
  <*> cliServerOptions
  <*> cliClientOptions

  where

  cliDatabasePath = Opt.strOption $
    Opt.long "database-path"   <>
    Opt.metavar "FILEPATH"     <>
    Opt.value "db-byron-proxy" <>
    Opt.help "Path to folder of the database. Will be created if it does not exist."

  cliIndexPath = Opt.strOption $
    Opt.long "index-path"         <>
    Opt.metavar "FILEPATH"        <>
    Opt.value "index-byron-proxy" <>
    Opt.help "Path to folder of the SQLite database index."

  cliLoggerConfigPath = Opt.optional $ Opt.strOption $
    Opt.long "logger-config" <>
    Opt.metavar "FILEPATH"   <>
    Opt.help "Path to the logger config file."

  cliCardanoConfigurationOptions = CSL.configurationOptionsParser

  cliServerOptions = ServerOptions
    <$> cliHostName ["server"]
    <*> cliServiceName ["server"]

  cliHostName prefix = Opt.strOption $
    Opt.long (dashconcat (prefix ++ ["host"])) <>
    Opt.help "Host"

  cliServiceName prefix = Opt.strOption $
    Opt.long (dashconcat (prefix ++ ["port"])) <>
    Opt.help "Port"

  -- FIXME
  -- TODO it's impossible for this to ever fail, so it's impossible to ever
  -- _not_ run a client.
  cliClientOptions :: Opt.Parser (Maybe ClientOptions)
  cliClientOptions = Opt.optional $
    (Left <$> cliShelleyClient) Opt.<|> (Right <$> cliByronClient)

  cliShelleyClient = ShelleyClientOptions
    <$> cliHostName ["remote"]
    <*> cliServiceName ["remote"]

  cliByronClient = ByronClientOptions <$> CSL.networkConfigOption

  dashconcat :: [String] -> String
  dashconcat = intercalate "-"

-- | Parser "info" for command line options (optparse-applicative).
cliParserInfo :: Opt.ParserInfo ByronProxyOptions
cliParserInfo = Opt.info cliParser infoMod
  where
  infoMod :: Opt.InfoMod ByronProxyOptions
  infoMod =
       Opt.header "Byron proxy"
    <> Opt.progDesc "Store and forward blocks from a Byron or Shelley server"
    <> Opt.fullDesc

runServer
  :: Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text)
  -> ServerOptions
  -> DB.DB IO
  -> IO ()
runServer tracer serverOptions db = do
  addrInfos <- Network.getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  case addrInfos of
    [] -> error "no getAddrInfo"
    (addrInfo : _) -> runVersionedServer addrInfo stringTracer mainTx usPoll db

  where
  host = soHostName serverOptions
  port = soServiceName serverOptions
  addrInfoHints = Network.defaultHints

  -- TODO configure this
  -- microsecond polling time of the DB. Needed until there is a proper
  -- storage layer.
  usPoll = 1000000
  mainTx :: STM ()
  mainTx = retry

  stringTracer :: Tracer IO String
  stringTracer = contramap
    (\str -> (Text.pack "main.server", Monitoring.Info, Text.pack str))
    tracer

-- | The reflections constraints are needed for the cardano-sl configuration
-- stuff, because the client may need to hit a Byron server using the logic
-- and diffusion layer. This is OK: run it under a `withConfigurations`.
runClient
  :: ( CSL.HasConfigurations )
  => Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text)
  -> Maybe ClientOptions
  -> CSL.Genesis.Config
  -> DB.DB IO
  -> IO ()
runClient tracer clientOptions genesisConfig db = case clientOptions of

  Nothing -> pure ()

  Just (Left shelleyClientOptions) -> do
    addrInfos <- Network.getAddrInfo (Just addrInfoHints) (Just host) (Just port)
    case addrInfos of
      [] -> error "no getAddrInfo"
      (addrInfo : _) -> runVersionedClient addrInfo stringTracer
    where

    host = scoHostName shelleyClientOptions
    port = scoServiceName shelleyClientOptions
    addrInfoHints = Network.defaultHints

  Just (Right byronClientOptions) -> do
    let cslTrace = mkCSLTrace tracer
    -- Get the `NetworkConfig` from the options
    networkConfig <- CSL.intNetworkConfigOpts
      (Trace.named cslTrace)
      (bcoCardanoNetworkOptions byronClientOptions)
    let bpc :: ByronProxyConfig
        bpc = ByronProxyConfig
          { bpcAdoptedBVData = CSL.configBlockVersionData genesisConfig
            -- ^ Hopefully that never needs to change.
          , bpcNetworkConfig = networkConfig
          , bpcDiffusionConfig = CSL.FullDiffusionConfiguration
              { CSL.fdcProtocolMagic = CSL.configProtocolMagic genesisConfig
              , CSL.fdcProtocolConstants = CSL.configProtocolConstants genesisConfig
              , CSL.fdcRecoveryHeadersMessage = CSL.recoveryHeadersMessage
              , CSL.fdcLastKnownBlockVersion = CSL.lastKnownBlockVersion CSL.updateConfiguration
              , CSL.fdcConvEstablishTimeout = CSL.networkConnectionTimeout
              -- Diffusion layer logs will have "diffusion" in their names.
              , CSL.fdcTrace = Trace.appendName "diffusion" cslTrace
              , CSL.fdcStreamWindow = CSL.streamWindow
              , CSL.fdcBatchSize    = 64
              }
            -- 40 seconds.
            -- TODO configurable for these 3.
          , bpcPoolRoundInterval = 40000000
          , bpcSendQueueSize     = 1
          , bpcRecvQueueSize     = 1
          }
    withByronProxy bpc db $ \bp -> do
      byronProxyMain stringTracer db bp

  where

  stringTracer :: Tracer IO String
  stringTracer = contramap
    (\str -> (Text.pack "main.client", Monitoring.Info, Text.pack str))
    tracer

main :: IO ()
main = do
  bpo <- Opt.execParser cliParserInfo
  -- Set up logging. If there's a config file we read it, otherwise use a
  -- default.
  loggerConfig <- case bpoLoggerConfigPath bpo of
    Nothing -> pure defaultLoggerConfig
    Just fp -> Monitoring.parseRepresentation fp
  -- iohk-monitoring uses some MVar for configuration, which corresponds to
  -- the "Representation" which we call config.
  loggerConfig' <- Monitoring.setupFromRepresentation loggerConfig
  Monitoring.withTrace loggerConfig' "byron-proxy" $ \trace -> do
    -- We always need the cardano-sl configuration, even if we're not
    -- connecting to a Byron peer, because that's where the blockchain
    -- configuration comes from: slots-per-epoch in particular.
    -- We'll use the tracer that was just set up to give debug output. That
    -- requires converting the iohk-monitoring trace to the one used in CSL.
    let cslTrace = mkCSLTrace (convertTrace trace)
        infoTrace = contramap ((,) Wlog.Info) (Trace.named cslTrace)
        confOpts = bpoCardanoConfigurationOptions bpo
    CSL.withConfigurations infoTrace Nothing Nothing False confOpts $ \genesisConfig _ _ _ -> do
      let genesisBlock = CSL.genesisBlock0 (CSL.configProtocolMagic genesisConfig)
                                           (CSL.configGenesisHash genesisConfig)
                                           (CSL.genesisLeaders genesisConfig)
          epochSlots = CSL.configEpochSlots genesisConfig
          -- Next, set up the database, taking care to seed with the genesis
          -- block if it's empty.
          dbc :: DBConfig
          dbc = DBConfig
            { dbFilePath    = bpoDatabasePath bpo
            , indexFilePath = bpoIndexPath bpo
            , slotsPerEpoch = epochSlots
            }
      withDB dbc $ \db -> do
        seedWithGenesis genesisBlock db
        let server = runServer (convertTrace trace) (bpoServerOptions bpo) db
            client = runClient (convertTrace trace) (bpoClientOptions bpo) genesisConfig db
        _ <- concurrently server client
        pure ()
