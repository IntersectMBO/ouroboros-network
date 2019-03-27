{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Lazy as Lazy (fromStrict)
import Data.Functor.Contravariant (Op (..), contramap)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getCurrentTime)
import Options.Applicative (execParser, fullDesc, help, info, long, metavar,
                            progDesc, strOption, value)
import qualified System.Directory
import System.Random (StdGen, getStdGen, randomR)

import Cardano.BM.Configuration as BM (setup)
import Cardano.BM.Data.LogItem hiding (LogNamed)
import qualified Cardano.BM.Data.Severity as BM
import Cardano.BM.Setup (withTrace)
import qualified Cardano.BM.Trace as BM (Trace)
import Control.Tracer (Tracer (..))

import qualified Pos.Binary.Class as CSL (decode, encode)
import Pos.Chain.Block (Block, BlockHeader (..), HeaderHash, genesisBlock0,
                        headerHash)
import Pos.Chain.Lrc (genesisLeaders)
import Pos.Core (SlotCount (..))
import Pos.Chain.Block (recoveryHeadersMessage, streamWindow)
import Pos.Chain.Update (lastKnownBlockVersion, updateConfiguration)
import Pos.Configuration (networkConnectionTimeout)
import Pos.Client.CLI (SimpleNodeArgs (..), CommonNodeArgs (..), CommonArgs (logConfig),
                       commonNodeArgsParser, configurationOptions, nodeArgsParser)
import Pos.Client.CLI.Params (getNodeParams)
import Pos.Chain.Genesis (Config (configGeneratedSecrets),
                          configEpochSlots, configGenesisHash,
                          configProtocolConstants, configProtocolMagic,
                          configBlockVersionData)
import Pos.Diffusion.Full (FullDiffusionConfiguration (..))
import Pos.Infra.Diffusion.Types
import Pos.Launcher (NodeParams (..), withConfigurations)
import Pos.Util.CompileInfo (withCompileInfo)
import Pos.Util.Trace (Trace, traceWith)
import Pos.Util.Trace.Named as Trace (LogNamed (..), appendName, named)
import qualified Pos.Util.Trace
import qualified Pos.Util.Wlog as Wlog

import qualified Network.TypedProtocol.Proofs as Protocol (connect)
import Network.TypedProtocols.Driver (runPeer)

import Ouroboros.Network.Channel (socketAsChannel)
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import qualified Ouroboros.Byron.Proxy.ChainSync.Server as Server
import qualified Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync

import qualified Ouroboros.Byron.Proxy.DB as DB
import qualified Ouroboros.Byron.Proxy.Index.Sqlite as Index
import Ouroboros.Byron.Proxy.Types
import qualified Ouroboros.Storage.Common as Immutable
import qualified Ouroboros.Storage.ImmutableDB.API as Immutable
import qualified Ouroboros.Storage.ImmutableDB.Impl as Immutable
import Ouroboros.Storage.FS.API.Types
import Ouroboros.Storage.FS.API
import Ouroboros.Storage.FS.IO
import Ouroboros.Storage.Util.ErrorHandling (exceptions)

-- | The main action using the proxy, index, and immutable database: download
-- the best known chain from the proxy and put it into the database, over and
-- over again.
--
-- No exception handling is done.
byronProxyMain
  :: DB.DB IO
  -> ByronProxy
  -> IO x
byronProxyMain db bp = getStdGen >>= mainLoop Nothing

  where

  waitForNext
    :: Maybe (BestTip BlockHeader)
    -> STM (Either (BestTip BlockHeader) Atom)
  waitForNext mBt = do
    mBt' <- bestTip bp
    if mBt == mBt'
    -- If recvAtom retires then the whole STM will retry and we'll check again
    -- for the best tip to have changed.
    then fmap Right (recvAtom bp)
    else case mBt' of
        Nothing -> retry
        Just bt -> pure (Left bt)

  mainLoop :: Maybe (BestTip BlockHeader) -> StdGen -> IO x
  mainLoop mBt rndGen = do
    -- Wait until the best tip has changed from the last one we saw. That can
    -- mean the header changed and/or the list of peers who announced it
    -- changed.
    next <- atomically $ waitForNext mBt
    case next of
      -- TODO we don't get to know from where it was received. Problem? Maybe
      -- not.
      Right atom -> do
        putStrLn $ mconcat
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
            Right (blk :: Block) -> pure (False, slot, headerHash blk)
        -- Pick a peer from the list of announcers at random and download
        -- the chain.
        let (peer, rndGen') = pickRandom rndGen (btPeers bt)
        putStrLn $ mconcat
          [ "Using tip with hash "
          , show tipHash
          , " at slot "
          , show tipSlot
          , if isEBB then " (EBB)" else ""
          ]
        putStrLn $ mconcat
          [ "Downloading the chain with tip hash "
          , show tipHash
          , " from "
          , show peer
          ]
        _ <- downloadChain
              bp
              peer
              (headerHash (btTip bt))
              [tipHash]
              streamer
        mainLoop (Just bt) rndGen'

  -- If it ends at an EBB, the EBB will _not_ be written. The tip will be the
  -- parent of the EBB.
  -- This should be OK.
  streamer :: StreamBlocks Block IO ()
  streamer = StreamBlocks
    { streamBlocksMore = \blocks -> DB.appendBlocks db $ \dbwrite -> do
        -- List comes in newest-to-oldest order.
        let orderedBlocks = NE.toList (NE.reverse blocks)
        forM_ orderedBlocks (DB.appendBlock dbwrite)
        pure streamer
    , streamBlocksDone = pure ()
    }

  pickRandom :: StdGen -> NonEmpty t -> (t, StdGen)
  pickRandom rndGen ne =
    let (idx, rndGen') = randomR (0, NE.length ne - 1) rndGen
    in  (ne NE.!! idx, rndGen')

-- | Echos rolls (forward or backward) using a trace.
chainSyncClient
  :: forall m .
     ( Monad m )
  => Trace m (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
  -> ChainSync.ChainSyncClient ChainSync.Block ChainSync.Point m ()
chainSyncClient trace = Client.chainSyncClient fold
  where
  fold :: Client.Fold m ()
  fold = Client.Fold $ pure $ Client.Continue forward backward
  forward :: ChainSync.Block -> ChainSync.Point -> Client.Fold m ()
  forward blk point = Client.Fold $ do
    traceWith trace (Right blk, point)
    Client.runFold fold
  backward :: ChainSync.Point -> ChainSync.Point -> Client.Fold m ()
  backward point1 point2 = Client.Fold $ do
    traceWith trace (Left point1, point2)
    Client.runFold fold

-- | Use a DB and a given number of microseconds to poll for changes, to get
-- a chain sync server that serves whole blocks.
-- The `ResourceT` is needed because we deal with DB iterators.
chainSyncServer
  :: Int
  -> DB.DB IO
  -> ChainSync.ChainSyncServer ChainSync.Block ChainSync.Point (ResourceT IO) ()
chainSyncServer usPoll = Server.chainSyncServer err poll
  where
  err = throwIO
  poll :: Server.PollT IO
  poll p m = do
    s <- m
    mbT <- p s
    case mbT of
      Nothing -> lift (threadDelay usPoll) >> poll p m
      Just t  -> pure t

{-
-- | Run a chain sync server over a UNIX domain socket.
-- Accepts at most one connection at a time, for simplicity.
runChainSyncServer :: Int -> DB.DB IO -> IO x
runChainSyncServer usPoll db = bracket mkSocket Socket.close $ \socket -> do
  let channel = socketAsChannel sock
  runPeer codec channel (chainSyncServer usPoll db)
  where
  codec = ChainSync.codec
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
-}

-- | Diffusion layer uses log-waper severity, iohk-monitoring uses its own.
convertSeverity :: Wlog.Severity -> BM.Severity
convertSeverity sev = case sev of
  Wlog.Debug   -> BM.Debug
  Wlog.Info    -> BM.Info
  Wlog.Notice  -> BM.Notice
  Wlog.Warning -> BM.Warning
  Wlog.Error   -> BM.Error

convertTrace :: BM.Trace IO Text -> Trace IO (LogNamed (Wlog.Severity, Text))
convertTrace trace = case trace of
  Tracer f -> Pos.Util.Trace.Trace $ Op $ \namedI -> do
    tid <- pack . show <$> myThreadId
    now <- getCurrentTime
    let logName    = Text.intercalate (Text.pack ".") (Trace.lnName namedI)
        (sev, txt) = Trace.lnItem namedI
        logMeta    = LOMeta { tstamp = now
                            , tid = tid
                            , severity = convertSeverity sev
                            , privacy = Public }
        logContent = LogMessage txt
        logObject  = LogObject logName logMeta logContent
    f logObject

data ByronProxyOptions = ByronProxyOptions
  { bpoIndexPath    :: !String
    -- ^ For the index database. We re-use dbPath inside SimpleNodeArgs for
    -- the immutable DB.
  , bpoNodeArgs     :: !SimpleNodeArgs
  }

getCommandLineOptions :: IO ByronProxyOptions
getCommandLineOptions = execParser programInfo
  where
  programInfo = info
    (ByronProxyOptions <$> indexPathParser <*> simpleNodeArgsParser)
    (fullDesc <> progDesc "Byron proxy")
  -- Defined in cardano-sl but not exported.
  simpleNodeArgsParser = SimpleNodeArgs <$> commonNodeArgsParser <*> nodeArgsParser

  indexPathParser = strOption $
    long "index-path"         <>
    metavar "FILEPATH"        <>
    value "index-byron-proxy" <>
    help "Index database file path (sqlite)"

-- | Use cardano-sl library stuff to get all of the necessary configuration
-- options. There's a bunch of stuff that we don't actually need but that's OK.
main :: IO ()
main = withCompileInfo $ do
  -- withCompileInfo forced upon us by 'getSimpleNodeOptions'. It uses it to
  -- print the git revision...
  -- Take all of the arguments from a cardano-sl node. Hijack db-path and
  -- reinterpret it as the immutable DB path.
  options <- getCommandLineOptions
  let SimpleNodeArgs cArgs nArgs = bpoNodeArgs options
      confOpts = configurationOptions (commonArgs cArgs)
      -- Take the log config file from the common args, and default it.
      loggerConfigFile = fromMaybe "logging.yaml" (logConfig (commonArgs cArgs))
  -- No need to setup logging from cardano-sl-util! Because we don't need a
  -- instance `WithLogger`! Wonderful. iohk-monitoring-framework is used
  -- directly.
  traceConfig <- BM.setup loggerConfigFile
  withTrace traceConfig "byron-proxy" $ \trace -> do
    let -- Convert from the BM trace to the cardano-sl-util trace.
        logTrace = convertTrace trace
        -- Give it no names, and log at info.
        infoTrace = contramap ((,) Wlog.Info) (named logTrace)
    -- `trace` is a `BaseTrace` from iohk-monitoring, but `withConfigurations`
    -- needs `Trace IO Text`. Not so easy to get that since `BaseTrace` takes
    -- a bunch of other stuff. See below when we use it to get diffusion-layer
    -- trace. So we use `noTrace` for this part.
    withConfigurations infoTrace Nothing Nothing False confOpts $ \genesisConfig _ _ _ -> do
      (nodeParams, Just sscParams) <- getNodeParams
        (named logTrace)
        "byron-adapter"
        cArgs
        nArgs
        (configGeneratedSecrets genesisConfig)
      let genesisBlock = genesisBlock0 (configProtocolMagic genesisConfig)
                                       (configGenesisHash genesisConfig)
                                       (genesisLeaders genesisConfig)
          -- Here's all of the config needed for the Byron proxy server itself.
          bpc :: ByronProxyConfig
          bpc = ByronProxyConfig
            { bpcAdoptedBVData = configBlockVersionData genesisConfig
              -- ^ Hopefully that never needs to change.
            , bpcNetworkConfig = npNetworkConfig nodeParams
            , bpcDiffusionConfig = FullDiffusionConfiguration
                { fdcProtocolMagic = configProtocolMagic genesisConfig
                , fdcProtocolConstants = configProtocolConstants genesisConfig
                , fdcRecoveryHeadersMessage = recoveryHeadersMessage
                , fdcLastKnownBlockVersion = lastKnownBlockVersion updateConfiguration
                , fdcConvEstablishTimeout = networkConnectionTimeout
                -- Diffusion layer logs will have "diffusion" in their names.
                , fdcTrace = appendName "diffusion" logTrace
                , fdcStreamWindow = streamWindow
                , fdcBatchSize    = 64
                }
              -- 40 seconds.
            , bpcPoolRoundInterval = 40000000
            , bpcSendQueueSize     = 1
            , bpcRecvQueueSize     = 1
            }
          -- The number of slots in an epoch. We use the one in the genesis
          -- configuration, and we assume it will never change.
          epochSlots :: SlotCount
          epochSlots = configEpochSlots genesisConfig
          getEpochSize :: Immutable.EpochNo -> IO Immutable.EpochSize
          -- FIXME the 'fromIntegral' casts from 'Word64' to 'Word'.
          -- For sufficiently high k, on certain machines, there could be an
          -- overflow.
          getEpochSize epoch = return $ Immutable.EpochSize $
            fromIntegral (getSlotCount epochSlots)
          -- Default database path is ./db-byron-proxy
          dbFilePath :: FilePath
          dbFilePath = fromMaybe "db-byron-proxy" (dbPath cArgs)
      -- Must ensure that the mount point given to ioHasFS exists or else
      -- opening the ImmutableDB will fail.
      -- NB: System.Directory, not the HasFS API.
      System.Directory.createDirectoryIfMissing True dbFilePath
      let fsMountPoint :: MountPoint
          fsMountPoint = MountPoint dbFilePath
          fs :: HasFS IO HandleIO
          fs = ioHasFS fsMountPoint
          -- These 2 go by way of the cardano-sl `Bi` class, which is not the
          -- same as the `Serialise` class. Typeclasses are way over used and,
          -- more often than not, very annoying.
          decodeHeaderHash :: forall s . CBOR.Decoder s HeaderHash
          decodeHeaderHash = CSL.decode
          encodeHeaderHash :: HeaderHash -> CBOR.Encoding
          encodeHeaderHash = CSL.encode
          openDB = Immutable.openDB
            decodeHeaderHash
            encodeHeaderHash
            fs
            exceptions
            getEpochSize
            Immutable.ValidateMostRecentEpoch
            (DB.epochFileParser epochSlots fs)
          -- The supporting header hash and tip index.
          indexFilePath :: FilePath
          indexFilePath = bpoIndexPath options
      Index.withDB_ indexFilePath $ \idx -> do
        putStrLn "Index is open. Press any key to create immutable DB"
        getChar
        Immutable.withDB openDB $ \idb -> do
          putStrLn "Immutable DB is open. Press any key to start downloading."
          getChar
          let db = DB.mkDB throwIO epochSlots idx idb
          -- Check the tip, and if it says the DB is empty, seed it with
          -- the genesis block (`Left genesisBlock :: Block`).
          tip <- DB.readTip db
          case tip of
            DB.TipGenesis -> do
              putStrLn "DB is empty. Seeding with genesis."
              DB.appendBlocks db $ \dbAppend ->
                DB.appendBlock dbAppend (Left genesisBlock)
            _ -> pure ()
          withByronProxy bpc db $ \bp -> do
            let userInterrupt = getChar
                usPoll = 1000000 -- microseconds
                -- Client must go in `ResourceT`, since the server is in
                -- `ResourceT`.
                clientEcho :: Trace (ResourceT IO) (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
                clientEcho = Pos.Util.Trace.Trace $ Op $ \(roll, _tip) ->
                  let msg = case roll of
                        Left  back    -> mconcat
                          [ "Roll back to "
                          , show back
                          ]
                        Right forward -> mconcat
                          [ "Roll forward to "
                          , case ChainSync.getBlock forward of
                              Left ebb  -> show $ DB.ebbEpoch ebb
                              Right blk -> show $ DB.blockEpochAndRelativeSlot blk
                          ]
                  in  lift (putStrLn msg)
                client = chainSyncClient clientEcho
                server = chainSyncServer usPoll db
                -- NB: 'connect' actually makes a distinction between client
                -- and server, much to my dismay. The "client" (whatever that
                -- means) must be the first parameter.
                chainSyncThread = runResourceT $ Protocol.connect
                  (ChainSync.chainSyncClientPeer client)
                  (ChainSync.chainSyncServerPeer server)
                {- chainSyncThread = runChainSyncServer readFifo writeFifo usPoll db -}
                byronThread = byronProxyMain db bp
                mainThread = fst <$> concurrently byronThread chainSyncThread
            outcome <- race userInterrupt mainThread
            case outcome of
              Left _ -> pure ()
              Right impossible -> impossible
