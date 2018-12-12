{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Exception (bracket, catch, throwIO)
import Control.Lens ((^.))
import qualified Control.Lens as Lens (to)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad (forM_, void, when)
import qualified Data.ByteString as BS
import Data.Functor.Contravariant (Op (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..), tagWith)

-- Logging stuff...
import Control.Concurrent (myThreadId)
import Data.Time.Clock.POSIX (getCurrentTime)
import Cardano.BM.Data.LogItem
import Cardano.BM.Data.Trace
import Cardano.BM.BaseTrace
import Cardano.BM.Setup (shutdownTrace)
import Pos.Util.Klog.Compatibility (getTrace, setupLogging', parseLoggerConfig)
import Pos.Util.Wlog.Compatibility (Severity (..))

import Pos.Chain.Block (Block, BlockHeader (..), GenesisBlock, HasHeaderHash,
                        HeaderHash, blockHeader, gbhConsensus, gcdEpoch,
                        genesisBlock0, getBlockHeader, headerHash, mcdSlot)
import Pos.Chain.Lrc (genesisLeaders)
import Pos.Chain.Security (SecurityParams)
import Pos.Chain.Ssc (MCCommitment (..), MCOpening (..), MCShares (..),
                      MCVssCertificate (..), getCertId)
import Pos.Chain.Txp (TxAux (..), TxMsgContents (..))
import Pos.Chain.Update (BlockVersionData, UpdateProposal (..), UpdateVote (..))
import Pos.Communication (NodeId)
import Pos.Core (StakeholderId, SlotCount (..), addressHash, getEpochIndex,
                 getSlotIndex, siEpoch, siSlot)
import Pos.Core.Chrono (NewestFirst (..))
import Pos.Crypto (hash, toPublic)
import Pos.DB.Block (GetHeadersFromManyToError (..), GetHashesRangeError (..))
import Pos.Diffusion.Full (FullDiffusionConfiguration (..), diffusionLayerFull)
import Pos.Infra.Diffusion.Types
-- An ancient relic. Needed for the network configuration type.
import Pos.Infra.DHT.Real.Param (KademliaParams)
import Pos.Infra.Network.Types (NetworkConfig (..))
import Pos.Logic.Types hiding (streamBlocks)
import qualified Pos.Logic.Types as Logic
import qualified Pos.Util.Trace

-- Stuff for bringing up a node (arguments etc.)
import Pos.Behavior (bcSecurityParams)
import Pos.Binary.Class (serializeBuilder)
import Pos.Chain.Block (Block, BlockHeader, HeaderHash, recoveryHeadersMessage, streamWindow)
import Pos.Chain.Update (lastKnownBlockVersion, updateConfiguration)
import Pos.Configuration (networkConnectionTimeout)
import Pos.Client.CLI (SimpleNodeArgs (..), CommonNodeArgs (..), CommonArgs (logConfig),
                       configurationOptions, getSimpleNodeOptions)
import Pos.Client.CLI.Params (getNodeParams)
import Pos.Chain.Genesis (Config (configGeneratedSecrets), GenesisHash (..),
                          configEpochSlots, configGenesisHash,
                          configProtocolConstants, configProtocolMagic,
                          configBlockVersionData)
import Pos.Communication (NodeId)
import Pos.Launcher (LoggingParams (lpDefaultName), NodeParams (..), cfoKey, withConfigurations)
import Pos.Util.CompileInfo (withCompileInfo)

import qualified Ouroboros.Byron.Proxy.Index as Index
import Ouroboros.Byron.Proxy.Types


import Ouroboros.Storage.ImmutableDB.API
import qualified Ouroboros.Storage.ImmutableDB.Impl as DB
import Ouroboros.Storage.FS.API.Types
import Ouroboros.Storage.FS.API
import Ouroboros.Storage.FS.IO
import Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

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


-- | Use cardano-sl library stuff to get all of the necessary configuration
-- options. There's a bunch of stuff that we don't actually need but that's OK.
main :: IO ()
main = withCompileInfo $ do
  -- withCompileInfo forced upon us by 'getSimpleNodeOptions'. It uses it to
  -- print the git revision...
  -- Take all of the arguments from a cardano-sl node. Hijack db-path and
  -- reinterpret it as the immutable DB path.
  SimpleNodeArgs cArgs nArgs <- getSimpleNodeOptions
  let confOpts = configurationOptions (commonArgs cArgs)
      -- Take the log config file from the common args, and default it.
      -- FIXME this is sloppy and just awful.
      loggerConfigFile = fromMaybe "logging.yaml" (logConfig (commonArgs cArgs))
  loggerConfig <- parseLoggerConfig loggerConfigFile
  lh <- setupLogging' "byron-adapter" loggerConfig
  -- Should be able to get a trace that we can give to the diffusion layer.
  trace <- getTrace lh
  withConfigurations Nothing Nothing False confOpts $ \genesisConfig _ _ _ -> do
    (nodeParams, Just sscParams) <- getNodeParams
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
              -- The iohk-monitoring-framework took the `Trace` type and renamed
              -- it `Cardano.BM.BaseTrace.BaseTrace`. Then `Trace` is defined
              -- to be `(TraceContext, BaseTrace m NamedLogItem)` where
              -- `TraceContext` contains configuration stuff and a mysterious
              -- `IO ()`.
              --
              -- The challenge: how to make the original `Trace` from this
              -- thing, to give to the diffusion layer, in such a way that it
              -- ehaves as expected? What is the `TraceContext` for? Is it
              -- needed? It seems all wrong: configuration for the `Trace`
              -- should be kept _in its closure_ if need be rather than shown
              -- up-front, and with a mention of `IO` to boot. Very poor
              -- abstraction.
              , fdcTrace = case trace of
                  (traceContext, BaseTrace (Op f)) -> Pos.Util.Trace.Trace $ Op $ \(severity, text) -> do
                    tid <- myThreadId
                    now <- getCurrentTime
                    let logMeta    = LOMeta { tstamp = now, tid = tid }
                        logContent = LogMessage logItem
                        logItem    = LogItem { liSelection = Both, liSeverity = severity, liPayload = text }
                        logObject  = LogObject logMeta logContent
                        logNamed   = LogNamed { lnName = "diffusion", lnItem = logObject }
                    f logNamed
              , fdcStreamWindow = streamWindow
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
        -- Default database path is ./db-byron-adapter
        dbFilePath :: FsPath
        dbFilePath = maybe ["db-byron-adapter"] pure (dbPath cArgs)
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
        openDB dbEpoch = DB.openDB fs errorHandling dbFilePath dbEpoch epochSizes
        -- The supporting header hash and tip index.
        indexFilePath :: FilePath
        indexFilePath = "index-byron-adapter"
    Index.withDB_ indexFilePath $ \indexDB -> do
      -- Now we can use the index to get the tip.
      -- BUT if it's empty, so there is no tip, what do we do? Use epoch 0
      -- I guess.
      mTip <- Index.indexAt indexDB Index.Tip
      let dbEpoch = case mTip of
            Nothing -> 0
            Just (_, EpochSlot tipEpoch _) -> tipEpoch
      withDB (openDB dbEpoch) $ \db -> do
        -- TODO Thursday: derive this from the ImmutableDB and Index taken
        -- together. We assume also that the Index is append-only, so should
        -- be OK for an oldest-to-newest iterator.
        let bbs = ByronBlockSource
              { bbsStream = const (pure ())
              , bbsTip    = error "bbsTip: not implemented"
              }
        withByronProxy bpc bbs $ \bp -> do
          putStrLn "Ready"
          let echoQueue = do
                next <- atomically (recvAtom bp)
                print next
                echoQueue
              -- Every time the header changes, echo it ...
              echoHeaders mBt = do
                mBt' <- atomically $ do
                  mBt' <- bestTip bp
                  if mBt == mBt'
                  then retry
                  else pure mBt'
                print ((fmap . fmap) headerHash mBt')
                echoHeaders mBt'
              -- ... and download the chain.
              download mBt = do
                mBt' <- atomically $ do
                  mBt' <- bestTip bp
                  if mBt == mBt'
                  then retry
                  else pure mBt'
                case mBt' of
                  Nothing -> download mBt'
                  Just bt -> do
                    -- Get our tip from the database, using genesis if
                    -- Nothing.
                    -- Use that as sole checkpoint in download.
                    -- For each step in the stream, write the tip to the index.
                    mTip <- Index.indexAt indexDB Index.Tip
                    let tipHash = case mTip of
                          Nothing      -> headerHash genesisBlock
                          Just (hh, _) -> Index.getOwnHash hh
                        peer = NE.head (btPeers bt)
                        streamer :: StreamBlocks Block IO ()
                        streamer = StreamBlocks
                          { streamBlocksMore = \blocks -> do
                              -- List comes in newest-to-oldest order.
                              forM_ (NE.toList (NE.reverse blocks)) $ \blk -> do
                                let hh = headerHash blk
                                    epochSlot = blockHeaderIndex epochSlots (Pos.Chain.Block.getBlockHeader blk)
                                    rslot = _relativeSlot epochSlot
                                Index.updateTip indexDB hh epochSlot
                                appendBinaryBlob db rslot (serializeBuilder blk)
                                -- Sometimes we need to start a new epoch...
                                when (fromIntegral (getRelativeSlot rslot) == getSlotCount epochSlots)
                                     -- Add 1 because of EBBs
                                     (void (startNewEpoch db (fromIntegral (getSlotCount epochSlots) + 1)))
                              pure streamer
                          , streamBlocksDone = pure ()
                          }
                    _ <- downloadChain
                      bp
                      peer
                      (headerHash (btTip bt))
                      [tipHash]
                      streamer
                    download (Just bt)
          race getChar (concurrently (echoHeaders Nothing) (download Nothing))
          putStrLn "Shutting down"
        shutdownTrace trace
        putStrLn "Goodbye"
