{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}

import Control.Concurrent.Async (concurrently)
import Control.Monad (void)
import Control.Tracer (Tracer (..), contramap)
import Data.Functor.Contravariant (Op (..))
import Data.List (intercalate)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Options.Applicative as Opt

import qualified Cardano.BM.Data.Aggregated as Monitoring
import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring

import qualified Cardano.Chain.Slotting as Cardano

import qualified Pos.Chain.Block as CSL (genesisBlock0)
import qualified Pos.Chain.Lrc as CSL (genesisLeaders)
import qualified Pos.Chain.Block as CSL (recoveryHeadersMessage, streamWindow)
import qualified Pos.Chain.Update as CSL (lastKnownBlockVersion, updateConfiguration)
import qualified Pos.Configuration as CSL (networkConnectionTimeout)
import qualified Pos.Chain.Genesis as CSL.Genesis (Config)
import qualified Pos.Chain.Genesis as CSL (configEpochSlots, configGenesisHash,
                                           configProtocolConstants, configProtocolMagic,
                                           configBlockVersionData)
import qualified Pos.Diffusion.Full as CSL (FullDiffusionConfiguration (..))

import qualified Pos.Infra.Network.CLI as CSL (NetworkConfigOpts (..),
                                               externalNetworkAddressOption,
                                               intNetworkConfigOpts,
                                               listenNetworkAddressOption)
import Pos.Infra.Network.Types (NetworkConfig (..))
import qualified Pos.Infra.Network.Policy as Policy
import qualified Pos.Launcher.Configuration as CSL (ConfigurationOptions (..),
                                                    HasConfigurations,
                                                    withConfigurations)
import qualified Pos.Client.CLI.Options as CSL (configurationOptionsParser)

import Pos.Util.Trace (Trace)
import qualified Pos.Util.Trace.Named as Trace (LogNamed (..), appendName, named)
import qualified Pos.Util.Trace
import qualified Pos.Util.Wlog as Wlog

import Ouroboros.Network.Block (SlotNo (..))

import qualified Ouroboros.Byron.Proxy.DB as DB
import Ouroboros.Byron.Proxy.Main

import qualified Byron
import DB (DBConfig (..), withDB)
import qualified Logging as Logging
import qualified Shelley.Client
import qualified Shelley.Server

data ByronProxyOptions = ByronProxyOptions
  { bpoDatabasePath                :: !FilePath
  , bpoIndexPath                   :: !FilePath
  , bpoLoggerConfigPath            :: !(Maybe FilePath)
    -- ^ Optional file path; will use default configuration if none given.
  , bpoCardanoConfigurationOptions :: !CSL.ConfigurationOptions
  , bpoServerOptions               :: !Shelley.Server.Options
  , bpoClientOptions               :: !ClientOptions
  }

data ByronClientOptions = ByronClientOptions
  { bcoCardanoNetworkOptions       :: !CSL.NetworkConfigOpts
    -- ^ To use with `intNetworkConfigOpts` to get a `NetworkConfig` from
    -- cardano-sl, required in order to run a diffusion layer.
  }

-- | `Tracer` comes from the `contra-tracer` package, but cardano-sl still
-- works with the cardano-sl-util definition of the same thing.
mkCSLTrace
  :: Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text.Builder)
  -> Trace IO (Trace.LogNamed (Wlog.Severity, Text))
mkCSLTrace tracer = case tracer of
  Tracer f -> Pos.Util.Trace.Trace $ Op $ \namedI ->
    let logName    = Text.intercalate (Text.pack ".") (Trace.lnName namedI)
        (sev, txt) = Trace.lnItem namedI
    in  f (logName, convertSeverity sev, Text.fromText txt)

  where

  -- | Diffusion layer uses log-waper severity, iohk-monitoring uses its own.
  convertSeverity :: Wlog.Severity -> Monitoring.Severity
  convertSeverity sev = case sev of
    Wlog.Debug   -> Monitoring.Debug
    Wlog.Info    -> Monitoring.Info
    Wlog.Notice  -> Monitoring.Notice
    Wlog.Warning -> Monitoring.Warning
    Wlog.Error   -> Monitoring.Error

-- | Until we have a storage layer which can deal with multiple sources, a
-- Shelley source for blocks will supercede a Byron source. You can still run
-- both a Byron server and a Shelley server (must run a Shelley server) but if
-- a Shelley client is given, blocks will not be downloaded from Byron.
--
-- NB: it's confusing/bad nomenclature: in order to get a Byron _server_ you
-- need to give Byron client options. That's because the diffusion layer for
-- Byron bundles both of these.
data ClientOptions = ClientOptions
  { coShelley :: !(Maybe Shelley.Client.Options)
  , coByron   :: !(Maybe ByronClientOptions)
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

  cliServerOptions = Shelley.Server.Options
    <$> cliHostName ["server"]
    <*> cliServiceName ["server"]

  cliHostName prefix = Opt.strOption $
    Opt.long (dashconcat (prefix ++ ["host"])) <>
    Opt.metavar "HOST" <>
    Opt.help "Host"

  cliServiceName prefix = Opt.strOption $
    Opt.long (dashconcat (prefix ++ ["port"])) <>
    Opt.metavar "PORT" <>
    Opt.help "Port"

  cliClientOptions :: Opt.Parser ClientOptions
  cliClientOptions = ClientOptions <$> cliShelleyClientOptions <*> cliByronClientOptions

  cliByronClientOptions :: Opt.Parser (Maybe ByronClientOptions)
  cliByronClientOptions = Opt.optional $
    ByronClientOptions <$> cliNetworkConfig

  cliShelleyClientOptions :: Opt.Parser (Maybe Shelley.Client.Options)
  cliShelleyClientOptions = Opt.optional $ Shelley.Client.Options
    <$> cliHostName ["remote"]
    <*> cliServiceName ["remote"]

  -- We can't use `Pos.Infra.Network.CLI.networkConfigOption` from
  -- cardano-sl-infra because all of its fields are optional, but we need at
  -- least one non-optional, so that this parser can fail, and therefore
  -- `cliClientOptions` can give `Nothing.
  -- Very similar to `Pos.Infra.Network.CLI.networkConfigOption`, but the
  -- topology file is _not_ optional.
  cliNetworkConfig :: Opt.Parser CSL.NetworkConfigOpts
  cliNetworkConfig = do
      ncoTopology <-
          fmap Just $
          Opt.strOption $
          mconcat
              [ Opt.long "topology"
              , Opt.metavar "FILEPATH"
              , Opt.help "Path to a YAML file containing the network topology"
              ]
      ncoKademlia <-
          Opt.optional $ Opt.strOption $
          mconcat
              [ Opt.long "kademlia"
              , Opt.metavar "FILEPATH"
              , Opt.help
                    "Path to a YAML file containing the kademlia configuration"
              ]
      ncoSelf <-
          Opt.optional $ Opt.option (fromString <$> Opt.str) $
          mconcat
              [ Opt.long "node-id"
              , Opt.metavar "NODE_ID"
              , Opt.help "Identifier for this node within the network"
              ]
      ncoPort <-
          Opt.option Opt.auto $
          mconcat
              [ Opt.long "default-port"
              , Opt.metavar "PORT"
              , Opt.help "Port number for IP address to node ID translation"
              , Opt.value 3000
              ]
      ncoPolicies <-
          Opt.optional $ Opt.strOption $
          mconcat
              [ Opt.long "policies"
              , Opt.metavar "FILEPATH"
              , Opt.help "Path to a YAML file containing the network policies"
              ]
      ncoExternalAddress <- Opt.optional $ CSL.externalNetworkAddressOption Nothing
      ncoBindAddress <- Opt.optional $ CSL.listenNetworkAddressOption Nothing
      pure $ CSL.NetworkConfigOpts
        { CSL.ncoTopology = ncoTopology
        , CSL.ncoKademlia = ncoKademlia
        , CSL.ncoSelf = ncoSelf
        , CSL.ncoPort = ncoPort
        , CSL.ncoPolicies = ncoPolicies
        , CSL.ncoExternalAddress = ncoExternalAddress
        , CSL.ncoBindAddress = ncoBindAddress
        }

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

-- | The reflections constraints are needed for the cardano-sl configuration
-- stuff, because the client may need to hit a Byron server using the logic
-- and diffusion layer. This is OK: run it under a `withConfigurations`.
runClient
  :: ( CSL.HasConfigurations )
  => Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text.Builder)
  -> ClientOptions
  -> CSL.Genesis.Config
  -> Cardano.EpochSlots
  -> DB.DB IO
  -> IO ()
runClient tracer clientOptions genesisConfig epochSlots db =

  case coByron clientOptions of

    -- If there's no Byron client, then there's also no Byron server, so we
    -- just run the Shelley client, which may also be nothing (`pure ()`).
    Nothing -> shelleyClient

    -- If there is a Byron client, then there's also a Byron server, so we
    -- run that, and then within the continuation decide whether to download
    -- from Byron or Shelley depending on whether a Shelley client is defined.
    Just byronClientOptions -> do
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
                { ncEnqueuePolicy = Policy.defaultEnqueuePolicyRelay
                , ncDequeuePolicy = Policy.defaultDequeuePolicyRelay
                }
              -- ^ These default relay policies should do what we want.
              -- If not, could give a --policy option and use yaml files as in
              -- cardano-sl
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
          genesisBlock = CSL.genesisBlock0 (CSL.configProtocolMagic genesisConfig)
                                           (CSL.configGenesisHash genesisConfig)
                                           (CSL.genesisLeaders genesisConfig)
      withByronProxy (contramap (\(a, b) -> ("", a, b)) tracer) bpc db $ \bp -> void $
        concurrently (byronClient genesisBlock bp) shelleyClient

  where

  -- Download from Byron only if there is no Shelley client.
  -- Always announce the header to Byron peers.
  byronClient genesisBlock bp = case coShelley clientOptions of
    Nothing -> void $ concurrently
      (Byron.download textTracer genesisBlock epochSlots db bp)
      (Byron.announce textTracer Nothing                 db bp)
    Just _  -> Byron.announce textTracer Nothing db bp

  shelleyClient = case coShelley clientOptions of
    Nothing -> pure ()
    Just options -> Shelley.Client.runClient options textTracer epochSlots db

  textTracer :: Tracer IO Text.Builder
  textTracer = contramap
    (\tbuilder -> (Text.pack "main.client", Monitoring.Info, tbuilder))
    tracer

main :: IO ()
main = do
  bpo <- Opt.execParser cliParserInfo
  Logging.withLogging (bpoLoggerConfigPath bpo) "byron-proxy" $ \trace -> do
    -- We always need the cardano-sl configuration, even if we're not
    -- connecting to a Byron peer, because that's where the blockchain
    -- configuration comes from: slots-per-epoch in particular.
    -- We'll use the tracer that was just set up to give debug output. That
    -- requires converting the iohk-monitoring trace to the one used in CSL.
    let cslTrace = mkCSLTrace (Logging.convertTrace' trace)
        infoTrace = contramap ((,) Wlog.Info) (Trace.named cslTrace)
        confOpts = bpoCardanoConfigurationOptions bpo
    CSL.withConfigurations infoTrace Nothing Nothing False confOpts $ \genesisConfig _ _ _ -> do
      let epochSlots = Cardano.EpochSlots (fromIntegral (CSL.configEpochSlots genesisConfig))
          -- Next, set up the database, taking care to seed with the genesis
          -- block if it's empty.
          dbc :: DBConfig
          dbc = DBConfig
            { dbFilePath    = bpoDatabasePath bpo
            , indexFilePath = bpoIndexPath bpo
            , slotsPerEpoch = epochSlots
            }
          -- Trace DB writes in such a way that they appear in EKG.
          dbTracer = flip contramap (Logging.convertTrace trace) $ \(DB.DBWrite (SlotNo count)) ->
            ("db", Monitoring.Info, Monitoring.LogValue "block count" (Monitoring.PureI (fromIntegral count)))
      withDB dbc dbTracer $ \db -> do
        let server = Shelley.Server.runServer (bpoServerOptions bpo) epochSlots db
            client = runClient (Logging.convertTrace' trace) (bpoClientOptions bpo) genesisConfig epochSlots db
        _ <- concurrently server client
        pure ()
