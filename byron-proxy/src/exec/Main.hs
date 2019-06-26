{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Lens ((^.))
import Control.Monad (forM_, unless, void)
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.Functor.Contravariant (Op (..))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Options.Applicative as Opt
import System.Random (StdGen, getStdGen, randomR)

import qualified Cardano.BM.Data.Aggregated as Monitoring
import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano

import qualified Pos.Binary.Class as CSL (decodeFull)
import qualified Pos.Chain.Block as CSL (Block, BlockHeader (..), GenesisBlock,
                                         HeaderHash, MainBlock, gbHeader,
                                         genesisBlock0, headerHash)
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

import DB (DBConfig (..), withDB)
import qualified Logging as Logging
import qualified Shelley.Client
import qualified Shelley.Server

-- | Download the best available chain from Byron peers and write to the
-- database, over and over again.
--
-- No exception handling is done.
byronProxyDownload
  :: Tracer IO Text.Builder
  -> CSL.GenesisBlock -- ^ For use as checkpoint when DB is empty. Also will
                      -- be put into an empty DB.
                      -- Sadly, old Byron net API doesn't give any meaning to an
                      -- empty checkpoint set; it'll just fall over.
  -> Cardano.EpochSlots
  -> DB.DB IO
  -> ByronProxy
  -> IO x
byronProxyDownload tracer genesisBlock epochSlots db bp = getStdGen >>= mainLoop Nothing

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
          , Text.fromString (show atom)
          ]
        mainLoop mBt rndGen
      Left bt -> do
        -- Get the tip from the database.
        -- It must not be empty; the DB must be seeded with the genesis block.
        -- FIXME throw exception, don't use error.
        tip <- DB.readTip db
        (isEBB, tipSlot, tipHash) <- case tip of
          -- If the DB is empty, we use the genesis hash as our tip, but also
          -- we need to put the genesis block into the database, because the
          -- Byron peer _will not serve it to us_!
          DB.TipGenesis -> do
            DB.appendBlocks db $ \dbwrite ->
              DB.appendBlock dbwrite (DB.LegacyBlockToWrite (Left genesisBlock))
            pure (True, 0, CSL.headerHash genesisBlock)
          DB.TipEBB   slot hash _ -> pure (True, slot, DB.coerceHashToLegacy hash)
          DB.TipBlock slot bytes -> case Binary.decodeFullAnnotatedBytes "Block or boundary" (Cardano.fromCBORABlockOrBoundary epochSlots) bytes of
            Left decoderError -> error $ "failed to decode block: " ++ show decoderError
            Right (Cardano.ABOBBoundary _) -> error $ "Corrput DB: got EBB where block expected"
            -- We have a cardano-ledger `HeaderHash` but we need a cardano-sl
            -- `HeaderHash`.
            -- FIXME should not come from DB module
            Right (Cardano.ABOBBlock blk) ->
              pure (False, slot, DB.coerceHashToLegacy (Cardano.blockHashAnnotated blk))
        -- Pick a peer from the list of announcers at random and download
        -- the chain.
        let (peer, rndGen') = pickRandom rndGen (btPeers bt)
        traceWith tracer $ mconcat
          [ "Using tip with hash "
          , Text.fromString (show tipHash)
          , " at slot "
          , Text.fromString (show tipSlot)
          , if isEBB then " (EBB)" else ""
          ]
        traceWith tracer $ mconcat
          [ "Downloading the chain with tip hash "
          , Text.fromString (show tipHash)
          , " from "
          , Text.fromString (show peer)
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
        forM_ orderedBlocks (DB.appendBlock dbwrite . DB.LegacyBlockToWrite)
        pure streamer
    , CSL.streamBlocksDone = pure ()
    }

  pickRandom :: StdGen -> NonEmpty t -> (t, StdGen)
  pickRandom rndGen ne =
    let (idx, rndGen') = randomR (0, NE.length ne - 1) rndGen
    in  (ne NE.!! idx, rndGen')

-- | Repeatedly check the database for the latest block, and announce it
-- whenever it changes.
-- NB: only proper main blocks can be announced, not EBBs.
-- The poll interval is hard-coded to 10 seconds.
-- FIXME polling won't be needed, once we have a DB layer that can notify on
-- tip change.
-- No exception handling is done.
byronProxyAnnounce
  :: Tracer IO Text.Builder
  -> Maybe CSL.HeaderHash -- ^ Of block most recently announced.
  -> DB.DB IO
  -> ByronProxy
  -> IO x
byronProxyAnnounce tracer mHashOfLatest db bp = do
  tip <- DB.readTip db
  mHashOfLatest' <- case tip of
    -- Genesis means empty database. Announce nothing.
    DB.TipGenesis         -> pure mHashOfLatest
    -- EBBs are not announced.
    DB.TipEBB   _ _ _     -> pure mHashOfLatest
    -- Main blocks must be decoded to CSL blocks.
    DB.TipBlock _   bytes -> case CSL.decodeFull bytes of
      Left txt                               -> error "byronProxyAnnounce: could not decode block"
      Right (Left (ebb :: CSL.GenesisBlock)) -> error "byronProxyAnnounce: ebb where block expected"
      Right (Right (blk :: CSL.MainBlock))   -> do
        let header = blk ^. CSL.gbHeader
            hash   = Just (CSL.headerHash header)
        unless (hash == mHashOfLatest) (announceChain bp header)
        pure hash
  threadDelay 10000000
  byronProxyAnnounce tracer mHashOfLatest' db bp

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
      (byronProxyDownload textTracer genesisBlock epochSlots db bp)
      (byronProxyAnnounce textTracer Nothing                 db bp)
    Just _  -> byronProxyAnnounce textTracer Nothing db bp

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

