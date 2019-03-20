{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Data.Functor.Contravariant (Op (..), contramap)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getCurrentTime)
import Options.Applicative (execParser, fullDesc, help, info, long, metavar,
                            progDesc, strOption, value)
import qualified System.Directory
import System.Random (StdGen, getStdGen, randomR)

import Cardano.BM.BaseTrace hiding (noTrace)
import Cardano.BM.Configuration as BM (setup)
import Cardano.BM.Data.LogItem hiding (LogNamed)
import qualified Cardano.BM.Data.LogItem as BM
import qualified Cardano.BM.Data.Severity as BM
import Cardano.BM.Setup (withTrace)
import qualified Cardano.BM.Trace as BM (Trace)

import Pos.Chain.Block (Block, BlockHeader (..), genesisBlock0, headerHash)
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
import Pos.Util.Trace (Trace)
import Pos.Util.Trace.Named as Trace (LogNamed (..), appendName, named)
import qualified Pos.Util.Trace
import qualified Pos.Util.Wlog as Wlog

import qualified Ouroboros.Byron.Proxy.DB as DB
import qualified Ouroboros.Byron.Proxy.Index as Index
import Ouroboros.Byron.Proxy.Types
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
        -- Find our tip of chain from the index.
        tip <- DB.readTip db
        let tipHash = headerHash tip
            -- Pick a peer from the list of announcers at random and download
            -- the chain.
            (peer, rndGen') = pickRandom rndGen (btPeers bt)
        putStrLn $ mconcat
          [ "Downloading chain with tip hash "
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


-- | Diffusion layer uses log-waper severity, iohk-monitoring uses its own.
convertSeverity :: Wlog.Severity -> BM.Severity
convertSeverity sev = case sev of
  Wlog.Debug   -> BM.Debug
  Wlog.Info    -> BM.Info
  Wlog.Notice  -> BM.Notice
  Wlog.Warning -> BM.Warning
  Wlog.Error   -> BM.Error

-- |
-- The iohk-monitoring-framework took the `Trace` type and renamed
-- it `Cardano.BM.BaseTrace.BaseTrace`. Then `Trace` is defined
-- to be `(TraceContext, BaseTrace m NamedLogItem)` where
-- `TraceContext` contains configuration stuff and a mysterious
-- `IO ()`.
--
-- The challenge: how to make the original `Trace` from this
-- thing, to give to the diffusion layer, in such a way that it
-- behaves as expected? What is the `TraceContext` for? Is it
-- needed? It seems all wrong: configuration for the `Trace`
-- should be kept _in its closure_ if need be rather than shown
-- up-front, and with a mention of `IO` to boot. Very poor
-- abstraction.
convertTrace :: BM.Trace IO -> Trace IO (LogNamed (Wlog.Severity, Text))
convertTrace trace = case trace of
  (traceContext, BaseTrace (Op f)) -> Pos.Util.Trace.Trace $ Op $ \namedI -> do
    tid <- myThreadId
    now <- getCurrentTime
    let name       = Text.intercalate (Text.pack ".") (Trace.lnName namedI)
        (sev, txt) = Trace.lnItem namedI
        logMeta    = LOMeta { tstamp = now, tid = tid }
        logContent = LogMessage logItem
        logItem    = LogItem { liSelection = Both
                             , liSeverity = convertSeverity sev
                             , liPayload = txt
                             }
        logObject  = LogObject logMeta logContent
        logNamed   = BM.LogNamed { BM.lnName = name, BM.lnItem = logObject }
    f logNamed

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
          openDB = Immutable.openDB
            fs
            exceptions
            getEpochSize
            Immutable.ValidateMostRecentEpoch
            (DB.epochFileParser epochSlots fs)
          -- The supporting header hash and tip index.
          indexFilePath :: FilePath
          indexFilePath = bpoIndexPath options
      Index.withDB_ indexFilePath $ \idx -> do
        Immutable.withDB openDB $ \idb _ -> do
          DB.withDB throwIO genesisBlock epochSlots idx idb $ \db ->
            withByronProxy bpc db $ \bp -> do
              let userInterrupt = getChar
              outcome <- race userInterrupt (byronProxyMain db bp)
              case outcome of
                Left _ -> pure ()
                Right impossible -> impossible
