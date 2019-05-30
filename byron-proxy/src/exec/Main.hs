{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}

import Codec.SerialiseTerm (CodecCBORTerm (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, concurrently)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.Functor.Contravariant (Op (..))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Options.Applicative as Opt
import System.Random (StdGen, getStdGen, randomR)

import qualified Cardano.BM.Data.LogItem as Monitoring
import qualified Cardano.BM.Data.Severity as Monitoring

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano

import qualified Pos.Chain.Block as CSL (Block, BlockHeader (..), GenesisBlock,
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
import qualified Pos.Launcher.Configuration as CSL (ConfigurationOptions (..),
                                                    HasConfigurations,
                                                    withConfigurations)
import qualified Pos.Client.CLI.Options as CSL (configurationOptionsParser)

import Pos.Util.Trace (Trace)
import qualified Pos.Util.Trace.Named as Trace (LogNamed (..), appendName, named)
import qualified Pos.Util.Trace
import qualified Pos.Util.Wlog as Wlog

import qualified Network.Socket as Network

import Ouroboros.Network.Socket
import Ouroboros.Network.Protocol.Handshake.Type (Accept (..))

import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import qualified Ouroboros.Byron.Proxy.ChainSync.Server as Server
import qualified Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync
import qualified Ouroboros.Byron.Proxy.DB as DB
import Ouroboros.Byron.Proxy.Main
import Ouroboros.Byron.Proxy.Network.Protocol

-- For orphan instances
import qualified Control.Monad.Class.MonadThrow as NonStandard
import qualified Control.Monad.Catch as Standard

import DB (DBConfig (..), withDB)
import qualified Logging as Logging

-- | The main action using the proxy, index, and immutable database: download
-- the best known chain from the proxy and put it into the database, over and
-- over again.
--
-- No exception handling is done.
byronProxyMain
  :: Tracer IO String
  -> CSL.GenesisBlock -- ^ For use as checkpoint when DB is empty. Also will
                      -- be put into an empty DB.
                      -- Sadly, old Byron net API doesn't give any meaning to an
                      -- empty checkpoint set; it'll just fall over.
  -> Cardano.EpochSlots
  -> DB.DB IO
  -> ByronProxy
  -> IO x
byronProxyMain tracer genesisBlock epochSlots db bp = getStdGen >>= mainLoop Nothing

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
        forM_ orderedBlocks (DB.appendBlock dbwrite . DB.LegacyBlockToWrite)
        pure streamer
    , CSL.streamBlocksDone = pure ()
    }

  pickRandom :: StdGen -> NonEmpty t -> (t, StdGen)
  pickRandom rndGen ne =
    let (idx, rndGen') = randomR (0, NE.length ne - 1) rndGen
    in  (ne NE.!! idx, rndGen')

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
    Opt.metavar "HOST" <>
    Opt.help "Host"

  cliServiceName prefix = Opt.strOption $
    Opt.long (dashconcat (prefix ++ ["port"])) <>
    Opt.metavar "PORT" <>
    Opt.help "Port"

  cliClientOptions :: Opt.Parser (Maybe ClientOptions)
  cliClientOptions = Opt.optional $
    (Left <$> cliShelleyClient) Opt.<|> (Right . ByronClientOptions <$> cliNetworkConfig)

  cliShelleyClient = ShelleyClientOptions
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

runServer
  :: Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text)
  -> ServerOptions
  -> Cardano.EpochSlots
  -> DB.DB IO
  -> IO ()
runServer tracer serverOptions epochSlots db = do
  addrInfos <- Network.getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  case addrInfos of
    [] -> error "no getAddrInfo"
    (addrInfo : _) -> withServerNode
      addrInfo
      encodeTerm
      decodeTerm
      (\_ _ _ -> Accept)
      (fmap AnyMuxResponderApp (responderVersions epochSlots chainSyncServer))
      wait

  where
  host = soHostName serverOptions
  port = soServiceName serverOptions
  addrInfoHints = Network.defaultHints

  chainSyncServer = Server.chainSyncServer epochSlots err poll db
  err =  throwIO
  poll :: Server.PollT IO
  poll p m = do
    s <- m
    mbT <- p s
    case mbT of
      Nothing -> lift (threadDelay usPoll) >> poll p m
      Just t  -> pure t

  -- TODO configure this
  -- microsecond polling time of the DB. Needed until there is a proper
  -- storage layer.
  usPoll = 1000000

  stringTracer :: Tracer IO Text
  stringTracer = contramap
    (\txt -> (Text.pack "main.server", Monitoring.Info, txt))
    tracer

-- | The reflections constraints are needed for the cardano-sl configuration
-- stuff, because the client may need to hit a Byron server using the logic
-- and diffusion layer. This is OK: run it under a `withConfigurations`.
runClient
  :: ( CSL.HasConfigurations )
  => Tracer IO (Monitoring.LoggerName, Monitoring.Severity, Text)
  -> Maybe ClientOptions
  -> CSL.Genesis.Config
  -> Cardano.EpochSlots
  -> DB.DB IO
  -> IO ()
runClient tracer clientOptions genesisConfig epochSlots db = case clientOptions of

  Nothing -> pure ()

  Just (Left shelleyClientOptions) -> do
    addrInfosLocal  <- Network.getAddrInfo (Just addrInfoHints) (Just "127.0.0.1") (Just "0")
    addrInfosRemote <- Network.getAddrInfo (Just addrInfoHints) (Just host) (Just port)
    case (addrInfosLocal, addrInfosRemote) of
      (addrInfoLocal : _, addrInfoRemote : _) -> connectTo
        encodeTerm
        decodeTerm
        (initiatorVersions epochSlots chainSyncClient)
        addrInfoLocal
        addrInfoRemote
      _ -> error "no getAddrInfo"
    where

    host = scoHostName shelleyClientOptions
    port = scoServiceName shelleyClientOptions
    addrInfoHints = Network.defaultHints
    -- | This chain sync client will first try to improve the read pointer to
    -- the tip of the database, and then will roll forward forever, stopping
    -- if there is a roll-back.
    -- It makes sense given that we only have an immutable database and one
    -- source for blocks: one read pointer improve is always enough.
    chainSyncClient = Client.chainSyncClient fold
      where
      fold :: Client.Fold (ResourceT IO) ()
      fold = Client.Fold $ do
        tip <- lift $ DB.readTip db
        mPoint <- case tip of
          -- DB is empty. Can go without improving read pointer.
          DB.TipGenesis -> pure Nothing
          -- EBB is nice because we already have the header hash.
          DB.TipEBB   slotNo hhash _     -> pure $ Just $ ChainSync.Point
            { ChainSync.pointSlot = slotNo
            , ChainSync.pointHash = hhash
            }
          DB.TipBlock slotNo       bytes -> pure $ Just $ ChainSync.Point
            { ChainSync.pointSlot = slotNo
            , ChainSync.pointHash = hhash
            }
            where
            hhash = case Binary.decodeFullAnnotatedBytes "Block or boundary" (Cardano.fromCBORABlockOrBoundary epochSlots) bytes of
              Left cborError -> error "failed to decode block"
              Right blk -> case blk of
                Cardano.ABOBBoundary _ -> error "Corrupt DB: expected block but got EBB"
                Cardano.ABOBBlock blk  -> Cardano.blockHashAnnotated blk
        case mPoint of
          Nothing -> Client.runFold roll
          -- We don't need to do anything with the result; the point is that
          -- the server now knows the proper read pointer.
          Just point -> pure $ Client.Improve [point] $ \_ _ -> roll
      roll :: Client.Fold (ResourceT IO) ()
      roll = Client.Fold $ pure $ Client.Continue forward backward
      forward :: ChainSync.Block -> ChainSync.Point -> Client.Fold (ResourceT IO) ()
      forward blk point = Client.Fold $ do
        lift $ traceWith (contramap chainSyncShow stringTracer) (Right blk, point)
        -- FIXME
        -- Write one block at a time. CPS doesn't mix well with the typed
        -- protocol style.
        -- This will give terrible performance for the SQLite index as it is
        -- currently defined. As a workaround, the SQLite index is set to use
        -- non-synchronous writes (per connection).
        -- Possible solution: do the batching automatically, within the index
        -- itself?
        lift $ DB.appendBlocks db $ \dbAppend ->
          DB.appendBlock dbAppend (DB.CardanoBlockToWrite blk)
        Client.runFold roll
      backward :: ChainSync.Point -> ChainSync.Point -> Client.Fold (ResourceT IO) ()
      backward point1 point2 = Client.Fold $ do
        lift $ traceWith (contramap chainSyncShow stringTracer) (Left point1, point2)
        pure $ Client.Stop ()

    chainSyncShow
      :: (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
      -> String
    chainSyncShow = \(roll, _tip) -> case roll of
      Left  back    -> mconcat
        [ "Roll back to "
        , show back
        ]
      Right forward -> mconcat
        [ "Roll forward to "
        , case Binary.unAnnotated forward of
            Cardano.ABOBBoundary ebb -> show ebb
            -- TODO Cardano.renderBlock
            Cardano.ABOBBlock    blk -> show blk
        ]

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

        genesisBlock = CSL.genesisBlock0 (CSL.configProtocolMagic genesisConfig)
                                         (CSL.configGenesisHash genesisConfig)
                                         (CSL.genesisLeaders genesisConfig)
    withByronProxy (contramap (\(a, b) -> ("", a, b)) tracer) bpc db $ \bp -> do
      byronProxyMain stringTracer genesisBlock epochSlots db bp

  where

  stringTracer :: Tracer IO String
  stringTracer = contramap
    (\str -> (Text.pack "main.client", Monitoring.Info, Text.pack str))
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
    let cslTrace = mkCSLTrace (Logging.convertTrace trace)
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
      withDB dbc $ \db -> do
        let server = runServer (Logging.convertTrace trace) (bpoServerOptions bpo) epochSlots db
            client = runClient (Logging.convertTrace trace) (bpoClientOptions bpo) genesisConfig epochSlots db
        _ <- concurrently server client
        pure ()

-- Orphan, forced upon me because of the IO sim stuff.
-- Required because we use ResourceT in the chain sync server, and `runPeer`
-- demands this non-standard `MonadThrow`. That could be fixed by returning
-- the failure reason rather than throwing it...

instance NonStandard.MonadThrow (ResourceT IO) where
  throwM = Standard.throwM
  -- There's a default definition fo this which requires
  -- NonStandard.MonadCatch (ResourceT IO). To avoid having to give those,
  -- we'll just use the standard definition.
  -- NB: it's weird huh? This implementation uses the _standard_ MonadMask
  -- constraint, but the non-standard one is not defined.
  bracket = Standard.bracket
