{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl (
    -- * Initialization
    ChainDbArgs(..)
  , defaultArgs
  , withDB
  , openDB
    -- * Trace types
  , TraceEvent (..)
  , TraceAddBlockEvent (..)
  , TraceReaderEvent (..)
  , TraceCopyToImmDBEvent (..)
  , TraceGCEvent (..)
  , TraceValidationEvent (..)
  , TraceInitChainSelEvent (..)
  , TraceOpenEvent (..)
  , TraceIteratorEvent (..)
  , LgrDB.TraceLedgerReplayEvent
    -- * Internals for testing purposes
  , openDBInternal
  , Internal (..)
  ) where

import           Control.Monad (when)
import           Control.Tracer
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           GHC.Stack (HasCallStack)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (pattern BlockPoint,
                     pattern GenesisPoint, HasHeader, Point, castPoint)

import           Ouroboros.Consensus.Block (Header, toIsEBB)
import           Ouroboros.Consensus.BlockchainTime (getCurrentSlot)
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API

import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (ChainDbArgs,
                     defaultArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as Args
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Background as Background
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel as ChainSel
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LedgerCursor as LedgerCursor
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Reader as Reader
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB as VolDB

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

withDB
  :: forall m blk a. (IOLike m, LedgerSupportsProtocol blk)
  => ChainDbArgs m blk
  -> (ChainDB m blk -> m a)
  -> m a
withDB args = bracket (fst <$> openDBInternal args True) API.closeDB

openDB
  :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
  => ChainDbArgs m blk
  -> m (ChainDB m blk)
openDB args = fst <$> openDBInternal args True

openDBInternal
  :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
  => ChainDbArgs m blk
  -> Bool -- ^ 'True' = Launch background tasks
  -> m (ChainDB m blk, Internal m blk)
openDBInternal args launchBgTasks = do
    immDB <- ImmDB.openDB argsImmDb
    immDbTipPoint <- ImmDB.getPointAtTip immDB
    let immDbTipChunk = chunkIndexOfPoint (Args.cdbChunkInfo args) immDbTipPoint
    traceWith tracer $ TraceOpenEvent $ OpenedImmDB immDbTipPoint immDbTipChunk

    volDB   <- VolDB.openDB argsVolDb
    traceWith tracer $ TraceOpenEvent OpenedVolDB
    let lgrReplayTracer =
          LgrDB.decorateReplayTracer
            immDbTipPoint
            (contramap TraceLedgerReplayEvent tracer)
    (lgrDB, replayed) <- LgrDB.openDB argsLgrDb
                            lgrReplayTracer
                            immDB
                            (Query.getAnyKnownBlock immDB volDB)
    traceWith tracer $ TraceOpenEvent OpenedLgrDB

    varInvalid <- newTVarM (WithFingerprint Map.empty (Fingerprint 0))

    curSlot        <- atomically $ getCurrentSlot (Args.cdbBlockchainTime args)
    chainAndLedger <- ChainSel.initialChainSelection
      immDB
      volDB
      lgrDB
      tracer
      (Args.cdbTopLevelConfig args)
      varInvalid
      curSlot

    let chain  = VF.validatedFragment chainAndLedger
        ledger = VF.validatedLedger   chainAndLedger
        cfg    = Args.cdbTopLevelConfig args

    atomically $ LgrDB.setCurrent lgrDB ledger
    varChain           <- newTVarM chain
    varIterators       <- newTVarM Map.empty
    varReaders         <- newTVarM Map.empty
    varNextIteratorKey <- newTVarM (IteratorKey 0)
    varNextReaderKey   <- newTVarM (ReaderKey   0)
    varCopyLock        <- newMVar  ()
    varKillBgThreads   <- newTVarM $ return ()
    varFutureBlocks    <- newTVarM Map.empty
    blocksToAdd        <- newBlocksToAdd (Args.cdbBlocksToAddSize args)

    let env = CDB { cdbImmDB           = immDB
                  , cdbVolDB           = volDB
                  , cdbLgrDB           = lgrDB
                  , cdbChain           = varChain
                  , cdbIterators       = varIterators
                  , cdbReaders         = varReaders
                  , cdbTopLevelConfig  = cfg
                  , cdbInvalid         = varInvalid
                  , cdbNextIteratorKey = varNextIteratorKey
                  , cdbNextReaderKey   = varNextReaderKey
                  , cdbCopyLock        = varCopyLock
                  , cdbTracer          = tracer
                  , cdbTraceLedger     = Args.cdbTraceLedger args
                  , cdbRegistry        = Args.cdbRegistry args
                  , cdbGcDelay         = Args.cdbGcDelay args
                  , cdbGcInterval      = Args.cdbGcInterval args
                  , cdbKillBgThreads   = varKillBgThreads
                  , cdbChunkInfo       = Args.cdbChunkInfo args
                  , cdbIsEBB           = toIsEBB . isJust . Args.cdbIsEBB args
                  , cdbCheckIntegrity  = Args.cdbCheckIntegrity args
                  , cdbBlockchainTime  = Args.cdbBlockchainTime args
                  , cdbBlocksToAdd     = blocksToAdd
                  , cdbFutureBlocks    = varFutureBlocks
                  }
    h <- fmap CDBHandle $ newTVarM $ ChainDbOpen env
    let chainDB = API.ChainDB
          { addBlockAsync      = getEnv1    h ChainSel.addBlockAsync
          , getCurrentChain    = getEnvSTM  h Query.getCurrentChain
          , getCurrentLedger   = getEnvSTM  h Query.getCurrentLedger
          , getTipBlock        = getEnv     h Query.getTipBlock
          , getTipHeader       = getEnv     h Query.getTipHeader
          , getTipPoint        = getEnvSTM  h Query.getTipPoint
          , getBlockComponent  = getEnv2    h Query.getBlockComponent
          , getIsFetched       = getEnvSTM  h Query.getIsFetched
          , getMaxSlotNo       = getEnvSTM  h Query.getMaxSlotNo
          , stream             = Iterator.stream  h
          , newReader          = Reader.newReader h (Args.cdbEncodeHeader args)
          , newLedgerCursor    = getEnv     h $ \env' ->
              LedgerCursor.newLedgerCursor
                (cdbLgrDB env')
                (castPoint . AF.anchorPoint <$> Query.getCurrentChain env')
          , getIsInvalidBlock  = getEnvSTM  h Query.getIsInvalidBlock
          , closeDB            = closeDB h
          , isOpen             = isOpen  h
          }
        testing = Internal
          { intCopyToImmDB             = getEnv  h Background.copyToImmDB
          , intGarbageCollect          = getEnv1 h Background.garbageCollect
          , intUpdateLedgerSnapshots   = getEnv  h Background.updateLedgerSnapshots
          , intScheduledChainSelection = getEnv1 h Background.scheduledChainSelection
          , intAddBlockRunner          = getEnv  h Background.addBlockRunner
          , intKillBgThreads           = varKillBgThreads
          }

    traceWith tracer $ TraceOpenEvent $ OpenedDB
      (castPoint $ AF.anchorPoint chain)
      (castPoint $ AF.headPoint   chain)

    when launchBgTasks $ Background.launchBgTasks env replayed

    return (chainDB, testing)
  where
    tracer = Args.cdbTracer args
    (argsImmDb, argsVolDb, argsLgrDb, _) = Args.fromChainDbArgs args


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

      Reader.closeAllReaders     cdb
      Iterator.closeAllIterators cdb

      killBgThreads <- atomically $ readTVar cdbKillBgThreads
      killBgThreads

      ImmDB.closeDB cdbImmDB
      VolDB.closeDB cdbVolDB

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
chunkIndexOfPoint :: ImmDB.ChunkInfo -> Point blk -> ImmDB.ChunkNo
chunkIndexOfPoint chunkInfo = \case
    GenesisPoint      -> ImmDB.firstChunkNo
    BlockPoint slot _ -> ImmDB.chunkIndexOfSlot chunkInfo slot
