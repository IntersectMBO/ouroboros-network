{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (catch, throwIO)
import Control.Lens ((^.))
import qualified Control.Lens as Lens (to)
import Control.Monad (forM_, void, when)
import Data.Functor.Contravariant (Op (..), contramap)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getCurrentTime)
import Options.Applicative (execParser, fullDesc, help, info, long, metavar,
                            progDesc, strOption, value)
import System.Random (StdGen, getStdGen, randomR)

import Cardano.BM.BaseTrace hiding (noTrace)
import Cardano.BM.Configuration as BM (setup)
import Cardano.BM.Data.LogItem hiding (LogNamed)
import qualified Cardano.BM.Data.LogItem as BM
import qualified Cardano.BM.Data.Severity as BM
import Cardano.BM.Setup (withTrace)
import qualified Cardano.BM.Trace as BM (Trace)

import Pos.Chain.Block (Block, BlockHeader (..), GenesisBlock, gbhConsensus, gcdEpoch,
                        genesisBlock0, getBlockHeader, headerHash, mcdSlot)
import Pos.Chain.Lrc (genesisLeaders)
import Pos.Core (SlotCount (..), getEpochIndex, getSlotIndex, siEpoch, siSlot)
import Pos.Binary.Class (serialize', serializeBuilder)
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
import Pos.DB.Class (Serialized (..), SerializedBlock)
import Pos.Diffusion.Full (FullDiffusionConfiguration (..))
import Pos.Infra.Diffusion.Types
import Pos.Launcher (NodeParams (..), withConfigurations)
import Pos.Util.CompileInfo (withCompileInfo)
import Pos.Util.Trace (Trace)
import Pos.Util.Trace.Named as Trace (LogNamed (..), appendName, named)
import qualified Pos.Util.Trace
import qualified Pos.Util.Wlog as Wlog

import qualified Ouroboros.Byron.Proxy.Index as Index
import Ouroboros.Byron.Proxy.Types
import Ouroboros.Storage.ImmutableDB.API
import qualified Ouroboros.Storage.ImmutableDB.Impl as DB
import Ouroboros.Storage.FS.API.Types
import Ouroboros.Storage.FS.API
import Ouroboros.Storage.FS.IO
import Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

-- | The main action using the proxy, index, and immutable database: download
-- the best known chain from the proxy and put it into the database, over and
-- over again.
--
-- No exception handling is done.
byronProxyMain
  :: GenesisBlock -- ^ Needed as checkpoint when DB is empty.
  -> SlotCount    -- ^ Needed in order to know when to start new epoch in DB.
  -> Index.Index IO
  -> ImmutableDB IO
  -> ByronProxy
  -> IO x
byronProxyMain genesisBlock epochSlots indexDB db bp = getStdGen >>= mainLoop Nothing

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
        mTip <- Index.indexRead indexDB Index.Tip
        let tipHash = case mTip of
              Nothing      -> headerHash genesisBlock
              Just (hh, _) -> Index.getOwnHash hh
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

  -- This is what we do with the blocks that come in from streaming.
  streamer :: StreamBlocks Block IO ()
  streamer = StreamBlocks
    { streamBlocksMore = \blocks -> Index.indexWrite indexDB $ \iwrite -> do
        -- List comes in newest-to-oldest order.
        forM_ (NE.toList (NE.reverse blocks)) $ \blk -> do
          let hh = headerHash blk
              epochSlot = blockHeaderIndex epochSlots (Pos.Chain.Block.getBlockHeader blk)
              rslot = _relativeSlot epochSlot
          Index.updateTip iwrite hh epochSlot
          appendBinaryBlob db rslot (serializeBuilder blk)
          -- Sometimes we need to start a new epoch...
          when (fromIntegral (getRelativeSlot rslot) == getSlotCount epochSlots)
               -- Add 1 because of EBBs
               (void (startNewEpoch db (fromIntegral (getSlotCount epochSlots) + 1)))
        pure streamer
    , streamBlocksDone = pure ()
    }

  pickRandom :: StdGen -> NonEmpty t -> (t, StdGen)
  pickRandom rndGen ne =
    let (idx, rndGen') = randomR (0, NE.length ne - 1) rndGen
    in  (ne NE.!! idx, rndGen')

-- | Extract the DB-relevant indices from a block.
-- For epoch boundary blocks (EBBs, also sometimes called genesis blocks) the
-- epoch is the one _before_ it, and the index is 1 plus the maximum local
-- slot index. We store the first proper main block of an epoch at index 0.
blockHeaderIndex :: SlotCount -> BlockHeader -> EpochSlot
blockHeaderIndex epochSlots blkHeader = EpochSlot epoch relativeSlot
  where
  -- FIXME
  -- EpochIndex is Word64. Epoch is Word. Whether Word is big enough depends
  -- upon the machine/implementation.
  epoch :: Epoch
  epoch = case blkHeader of
    -- Forced to use lenses :(
    BlockHeaderGenesis gBlkHeader -> gBlkHeader ^.
        gbhConsensus
      . gcdEpoch
      . Lens.to getEpochIndex
      . Lens.to fromIntegral
      . Lens.to (\x -> x - 1)
    BlockHeaderMain mBlkHeader -> mBlkHeader ^.
        gbhConsensus
      . mcdSlot 
      . Lens.to siEpoch
      . Lens.to getEpochIndex
      . Lens.to fromIntegral
  relativeSlot :: RelativeSlot
  relativeSlot = RelativeSlot $ case blkHeader of
    BlockHeaderGenesis _ -> fromIntegral . getSlotCount $ epochSlots
    BlockHeaderMain mBlkHeader -> mBlkHeader ^.
        gbhConsensus
      . mcdSlot
      . Lens.to siSlot
      . Lens.to getSlotIndex
      . Lens.to fromIntegral

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
  --
  -- TODO want to add another CLI for index db path.
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
          -- The database configuration follows.
          -- We need to open it up before starting the Byron proxy server, since
          -- it needs some block source that we derive from the database.
          --
          -- The number of slots in an epoch. We use the one in the genesis
          -- configuration, and we assume it will never change.
          epochSlots :: SlotCount
          epochSlots = configEpochSlots genesisConfig
          -- Default database path is ./db-byron-proxy
          dbFilePath :: FsPath
          dbFilePath = maybe ["db-byron-proxy"] pure (dbPath cArgs)
          fsMountPoint :: MountPoint
          fsMountPoint = MountPoint ""
          fs :: HasFS IO
          fs = ioHasFS fsMountPoint
          errorHandling :: ErrorHandling ImmutableDBError IO
          errorHandling = ErrorHandling
            { throwError = throwIO
            , catchError = catch
            }
          epochSizes :: Map Epoch EpochSize
          -- FIXME the 'fromIntegral' casts from 'Word64' to 'Word'.
          -- For sufficiently high k, on certain machines, there could be an
          -- overflow.
          -- FIXME need to be able to give a function `const epochSlots`.
          -- 0 to 200 should be fine for now... I think.
          --
          -- Add 1 to the slot count because we put EBBs at the end of every
          -- epoch.
          epochSizes = Map.fromList (fmap (\i -> (i, fromIntegral (getSlotCount epochSlots) + 1)) [0..200])
          openDB dbEpoch = DB.openDB
            fs
            errorHandling
            dbFilePath
            dbEpoch
            epochSizes
            NoValidation
          -- The supporting header hash and tip index.
          indexFilePath :: FilePath
          indexFilePath = bpoIndexPath options
      Index.withDB_ indexFilePath $ \indexDB -> do
        -- Now we can use the index to get the tip.
        -- BUT if it's empty, so there is no tip, what do we do? Use epoch 0
        -- I guess.
        mTip <- Index.indexRead indexDB Index.Tip
        let dbEpoch = case mTip of
              Nothing -> 0
              Just (_, EpochSlot tipEpoch _) -> tipEpoch
        withDB (openDB dbEpoch) $ \db _ -> do
          -- Create the block source to support the logic layer.
          let bbs :: ByronBlockSource IO
              bbs = Index.byronBlockSource
                      errInconsistent
                      genesisSerialized
                      indexDB
                      db
              errInconsistent :: forall x . Index.IndexInconsistent -> IO x
              errInconsistent = throwIO
              genesisSerialized :: SerializedBlock
              genesisSerialized = Serialized (serialize' genesisBlock)
          withByronProxy bpc bbs $ \bp -> do
            let userInterrupt = getChar
            outcome <- race userInterrupt (byronProxyMain genesisBlock epochSlots indexDB db bp)
            putStrLn "Bye"
            case outcome of
              Left _ -> pure ()
              Right impossible -> impossible
