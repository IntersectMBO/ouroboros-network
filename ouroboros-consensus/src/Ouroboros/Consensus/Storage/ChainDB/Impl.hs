{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl (
    -- * Initialization
    ChainDbArgs (..)
  , SerialiseDiskConstraints
  , defaultArgs
  , openDB
  , withDB
    -- * Trace types
  , LgrDB.TraceReplayEvent
  , NewTipInfo (..)
  , TraceAddBlockEvent (..)
  , TraceCopyToImmutableDBEvent (..)
  , TraceEvent (..)
  , TraceFollowerEvent (..)
  , TraceGCEvent (..)
  , TraceInitChainSelEvent (..)
  , TraceIteratorEvent (..)
  , TraceOpenEvent (..)
  , TraceValidationEvent (..)
    -- * Re-exported for convenience
  , Args.RelativeMountPoint (..)
  , ImmutableDB.ImmutableDbSerialiseConstraints
  , LgrDB.LgrDbSerialiseConstraints
  , VolatileDB.VolatileDbSerialiseConstraints
    -- * Internals for testing purposes
  , Internal (..)
  , openDBInternal
  ) where

import           Control.Monad (when)
import           Control.Monad.Trans.Class (lift)
import           Control.Tracer
import           Data.Functor ((<&>))
import           Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import           GHC.Stack (HasCallStack)

import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import           Ouroboros.Consensus.Util.ResourceRegistry (WithTempRegistry,
                     allocate, runInnerWithTempRegistry, runWithTempRegistry)

import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (ChainDbArgs,
                     defaultArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as Args
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Background as Background
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel as ChainSel
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Follower as Follower
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

withDB
  :: forall m blk a.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , ConvertRawHash blk
     , SerialiseDiskConstraints blk
     )
  => ChainDbArgs Identity m blk
  -> (ChainDB m blk -> m a)
  -> m a
withDB args = bracket (fst <$> openDBInternal args True) API.closeDB

openDB
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , ConvertRawHash blk
     , SerialiseDiskConstraints blk
     )
  => ChainDbArgs Identity m blk
  -> m (ChainDB m blk)
openDB args = fst <$> openDBInternal args True

openDBInternal
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , ConvertRawHash blk
     , SerialiseDiskConstraints blk
     )
  => ChainDbArgs Identity m blk
  -> Bool -- ^ 'True' = Launch background tasks
  -> m (ChainDB m blk, Internal m blk)
openDBInternal args launchBgTasks = runWithTempRegistry $ do
    lift $ traceWith tracer $ TraceOpenEvent StartedOpeningDB
    lift $ traceWith tracer $ TraceOpenEvent StartedOpeningImmutableDB
    immutableDB <- ImmutableDB.openDB argsImmutableDb $ innerOpenCont ImmutableDB.closeDB
    immutableDbTipPoint <- lift $ atomically $ ImmutableDB.getTipPoint immutableDB
    let immutableDbTipChunk =
          chunkIndexOfPoint (Args.cdbChunkInfo args) immutableDbTipPoint
    lift $ traceWith tracer $
      TraceOpenEvent $
        OpenedImmutableDB immutableDbTipPoint immutableDbTipChunk

    lift $ traceWith tracer $ TraceOpenEvent StartedOpeningVolatileDB
    volatileDB <- VolatileDB.openDB argsVolatileDb $ innerOpenCont VolatileDB.closeDB
    (chainDB, testing, env) <- lift $ do
      traceWith tracer $ TraceOpenEvent OpenedVolatileDB
      let lgrReplayTracer =
            LgrDB.decorateReplayTracerWithGoal
              immutableDbTipPoint
              (contramap TraceLedgerReplayEvent tracer)
      traceWith tracer $ TraceOpenEvent StartedOpeningLgrDB
      (lgrDB, replayed) <- LgrDB.openDB argsLgrDb
                            lgrReplayTracer
                            immutableDB
                            (Query.getAnyKnownBlock immutableDB volatileDB)
      traceWith tracer $ TraceOpenEvent OpenedLgrDB

      varInvalid      <- newTVarIO (WithFingerprint Map.empty (Fingerprint 0))
      varFutureBlocks <- newTVarIO Map.empty

      let initChainSelTracer = contramap TraceInitChainSelEvent tracer

      traceWith initChainSelTracer StartedInitChainSelection
      chainAndLedger <- ChainSel.initialChainSelection
                          immutableDB
                          volatileDB
                          lgrDB
                          initChainSelTracer
                          (Args.cdbTopLevelConfig args)
                          varInvalid
                          varFutureBlocks
                          (Args.cdbCheckInFuture args)
      traceWith initChainSelTracer InitalChainSelected

      let chain  = VF.validatedFragment chainAndLedger
          ledger = VF.validatedLedger   chainAndLedger
          cfg    = Args.cdbTopLevelConfig args

      atomically $ LgrDB.setCurrent lgrDB ledger
      varChain           <- newTVarIO chain
      varIterators       <- newTVarIO Map.empty
      varFollowers       <- newTVarIO Map.empty
      varNextIteratorKey <- newTVarIO (IteratorKey 0)
      varNextFollowerKey <- newTVarIO (FollowerKey   0)
      varCopyLock        <- newMVar  ()
      varKillBgThreads   <- newTVarIO $ return ()
      blocksToAdd        <- newBlocksToAdd (Args.cdbBlocksToAddSize args)

      let env = CDB { cdbImmutableDB     = immutableDB
                    , cdbVolatileDB      = volatileDB
                    , cdbLgrDB           = lgrDB
                    , cdbChain           = varChain
                    , cdbIterators       = varIterators
                    , cdbFollowers       = varFollowers
                    , cdbTopLevelConfig  = cfg
                    , cdbInvalid         = varInvalid
                    , cdbNextIteratorKey = varNextIteratorKey
                    , cdbNextFollowerKey = varNextFollowerKey
                    , cdbCopyLock        = varCopyLock
                    , cdbTracer          = tracer
                    , cdbTraceLedger     = Args.cdbTraceLedger args
                    , cdbRegistry        = Args.cdbRegistry args
                    , cdbGcDelay         = Args.cdbGcDelay args
                    , cdbGcInterval      = Args.cdbGcInterval args
                    , cdbKillBgThreads   = varKillBgThreads
                    , cdbChunkInfo       = Args.cdbChunkInfo args
                    , cdbCheckIntegrity  = Args.cdbCheckIntegrity args
                    , cdbCheckInFuture   = Args.cdbCheckInFuture args
                    , cdbBlocksToAdd     = blocksToAdd
                    , cdbFutureBlocks    = varFutureBlocks
                    }
      h <- fmap CDBHandle $ newTVarIO $ ChainDbOpen env
      let chainDB = API.ChainDB
            { addBlockAsync         = getEnv1    h ChainSel.addBlockAsync
            , getCurrentChain       = getEnvSTM  h Query.getCurrentChain
            , getLedgerDB           = getEnvSTM  h Query.getLedgerDB
            , getTipBlock           = getEnv     h Query.getTipBlock
            , getTipHeader          = getEnv     h Query.getTipHeader
            , getTipPoint           = getEnvSTM  h Query.getTipPoint
            , getBlockComponent     = getEnv2    h Query.getBlockComponent
            , getIsFetched          = getEnvSTM  h Query.getIsFetched
            , getIsValid            = getEnvSTM  h Query.getIsValid
            , getMaxSlotNo          = getEnvSTM  h Query.getMaxSlotNo
            , stream                = Iterator.stream  h
            , newFollower           = Follower.newFollower h
            , getIsInvalidBlock     = getEnvSTM  h Query.getIsInvalidBlock
            , closeDB               = closeDB h
            , isOpen                = isOpen  h
            }
          testing = Internal
            { intCopyToImmutableDB       = getEnv  h Background.copyToImmutableDB
            , intGarbageCollect          = getEnv1 h Background.garbageCollect
            , intUpdateLedgerSnapshots   = getEnv  h Background.updateLedgerSnapshots
            , intAddBlockRunner          = getEnv  h Background.addBlockRunner
            , intKillBgThreads           = varKillBgThreads
            }

      traceWith tracer $ TraceOpenEvent $ OpenedDB
        (castPoint $ AF.anchorPoint chain)
        (castPoint $ AF.headPoint   chain)

      when launchBgTasks $ Background.launchBgTasks env replayed

      return (chainDB, testing, env)

    _ <- lift $ allocate (Args.cdbRegistry args) (\_ -> return $ chainDB) API.closeDB

    return ((chainDB, testing), env)
  where
    tracer = Args.cdbTracer args
    (argsImmutableDb, argsVolatileDb, argsLgrDb, _) = Args.fromChainDbArgs args

-- | We use 'runInnerWithTempRegistry' for the component databases.
innerOpenCont ::
     IOLike m
  => (innerDB -> m ())
  -> WithTempRegistry st m (innerDB, st)
  -> WithTempRegistry (ChainDbEnv m blk) m innerDB
innerOpenCont closer m =
  runInnerWithTempRegistry
    (fmap (\(innerDB, st) -> (innerDB, st, innerDB)) m)
    ((True <$) . closer)
    (\_env _innerDB -> True)
      -- This check is degenerate because handles in @_env@ and the
      -- @_innerDB@ handle do not support an equality check; all of the
      -- identifying data is only in the handle's closure, not
      -- accessible because of our intentional encapsulation choices.

isOpen :: IOLike m => ChainDbHandle m blk -> STM m Bool
isOpen (CDBHandle varState) = readTVar varState <&> \case
    ChainDbClosed    -> False
    ChainDbOpen _env -> True

closeDB
  :: forall m blk.
     ( IOLike m
     , HasHeader (Header blk)
     , HasCallStack
     )
  => ChainDbHandle m blk -> m ()
closeDB (CDBHandle varState) = do
    mbOpenEnv <- atomically $ readTVar varState >>= \case
      -- Idempotent
      ChainDbClosed   -> return Nothing
      ChainDbOpen env -> do
        writeTVar varState ChainDbClosed
        return $ Just env

    -- Only when the ChainDB was open
    whenJust mbOpenEnv $ \cdb@CDB{..} -> do

      Follower.closeAllFollowers cdb
      Iterator.closeAllIterators cdb

      killBgThreads <- atomically $ readTVar cdbKillBgThreads
      killBgThreads

      ImmutableDB.closeDB cdbImmutableDB
      VolatileDB.closeDB cdbVolatileDB

      chain <- atomically $ readTVar cdbChain

      traceWith cdbTracer $ TraceOpenEvent $ ClosedDB
        (castPoint $ AF.anchorPoint chain)
        (castPoint $ AF.headPoint chain)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Lift 'chunkIndexOfSlot' to 'Point'
--
-- Returns 'firstChunkNo' in case of 'GenesisPoint'.
chunkIndexOfPoint :: ImmutableDB.ChunkInfo -> Point blk -> ImmutableDB.ChunkNo
chunkIndexOfPoint chunkInfo = \case
    GenesisPoint      -> ImmutableDB.firstChunkNo
    BlockPoint slot _ -> ImmutableDB.chunkIndexOfSlot chunkInfo slot
