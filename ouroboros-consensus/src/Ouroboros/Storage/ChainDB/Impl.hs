{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module Ouroboros.Storage.ChainDB.Impl (
    -- * Initialization
    ChainDbArgs(..)
  , defaultArgs
  , withDB
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
  , intReopen
  ) where

import           Control.Monad (when)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)

import           Control.Tracer

import           Control.Monad.Class.MonadThrow (bracket)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader (..), castPoint,
                     genesisBlockNo, genesisPoint)

import           Ouroboros.Consensus.Block (headerPoint)
import           Ouroboros.Consensus.BlockchainTime (getCurrentSlot)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))

import           Ouroboros.Storage.Common (EpochNo)
import           Ouroboros.Storage.EpochInfo (epochInfoEpoch)

import           Ouroboros.Storage.ChainDB.API

import           Ouroboros.Storage.ChainDB.Impl.Args (ChainDbArgs, defaultArgs)
import qualified Ouroboros.Storage.ChainDB.Impl.Args as Args
import qualified Ouroboros.Storage.ChainDB.Impl.Background as Background
import qualified Ouroboros.Storage.ChainDB.Impl.ChainSel as ChainSel
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Storage.ChainDB.Impl.Query as Query
import qualified Ouroboros.Storage.ChainDB.Impl.Reader as Reader
import qualified Ouroboros.Storage.ChainDB.Impl.Reopen as Reopen
import           Ouroboros.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

withDB
  :: forall m blk a. (IOLike m, ProtocolLedgerView blk)
  => ChainDbArgs m blk
  -> (ChainDB m blk -> m a)
  -> m a
withDB args = bracket (fst <$> openDBInternal args True) closeDB

openDBInternal
  :: forall m blk. (IOLike m, ProtocolLedgerView blk)
  => ChainDbArgs m blk
  -> Bool -- ^ 'True' = Launch background tasks
  -> m (ChainDB m blk, Internal m blk)
openDBInternal args launchBgTasks = do
    immDB <- ImmDB.openDB argsImmDb
    -- In order to figure out the 'BlockNo' and 'Point' at the tip of the
    -- ImmutableDB, we need to read the header at the tip of the ImmutableDB.
    immDbTipHeader <- sequence =<< ImmDB.getBlockComponentAtTip immDB GetHeader
    -- Note that 'immDbTipBlockNo' might not end up being the \"immutable\"
    -- block(no), because the current chain computed from the VolatileDB could
    -- be longer than @k@.
    let immDbTipBlockNo = maybe genesisBlockNo blockNo     immDbTipHeader
        immDbTipPoint   = maybe genesisPoint   headerPoint immDbTipHeader
    immDbTipEpoch      <- maybe (return 0)     blockEpoch  immDbTipHeader
    traceWith tracer $ TraceOpenEvent $ OpenedImmDB
      { _immDbTip      = immDbTipPoint
      , _immDbTipEpoch = immDbTipEpoch
      }

    volDB   <- VolDB.openDB argsVolDb
    traceWith tracer $ TraceOpenEvent OpenedVolDB
    lgrReplayTracer <- LgrDB.decorateReplayTracer
      (Args.cdbEpochInfo args)
      immDbTipPoint
      (contramap TraceLedgerReplayEvent tracer)
    lgrDB   <- LgrDB.openDB argsLgrDb
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
      (Args.cdbNodeConfig args)
      varInvalid
      curSlot

    let chain      = ChainSel.clChain  chainAndLedger
        ledger     = ChainSel.clLedger chainAndLedger
        cfg        = Args.cdbNodeConfig args
        secParam   = protocolSecurityParam cfg
        immBlockNo = ChainSel.getImmBlockNo secParam chain immDbTipBlockNo

    atomically $ LgrDB.setCurrent lgrDB ledger
    varChain          <- newTVarM chain
    varImmBlockNo     <- newTVarM immBlockNo
    varIterators      <- newTVarM Map.empty
    varReaders        <- newTVarM Map.empty
    varNextIteratorId <- newTVarM (IteratorId 0)
    varNextReaderId   <- newTVarM 0
    varCopyLock       <- newMVar  ()
    varKillBgThreads  <- newTVarM $ return ()
    varFutureBlocks   <- newTVarM Map.empty

    let env = CDB { cdbImmDB          = immDB
                  , cdbVolDB          = volDB
                  , cdbLgrDB          = lgrDB
                  , cdbChain          = varChain
                  , cdbImmBlockNo     = varImmBlockNo
                  , cdbIterators      = varIterators
                  , cdbReaders        = varReaders
                  , cdbNodeConfig     = cfg
                  , cdbInvalid        = varInvalid
                  , cdbNextIteratorId = varNextIteratorId
                  , cdbNextReaderId   = varNextReaderId
                  , cdbCopyLock       = varCopyLock
                  , cdbTracer         = tracer
                  , cdbTraceLedger    = Args.cdbTraceLedger args
                  , cdbRegistry       = Args.cdbRegistry args
                  , cdbGcDelay        = Args.cdbGcDelay args
                  , cdbKillBgThreads  = varKillBgThreads
                  , cdbEpochInfo      = Args.cdbEpochInfo args
                  , cdbIsEBB          = isJust . Args.cdbIsEBB args
                  , cdbBlockSize      = Args.cdbBlockSize args
                  , cdbBlockchainTime = Args.cdbBlockchainTime args
                  , cdbFutureBlocks   = varFutureBlocks
                  }
    h <- fmap CDBHandle $ newTVarM $ ChainDbOpen env
    let chainDB = ChainDB
          { addBlock           = getEnv1    h ChainSel.addBlock
          , getCurrentChain    = getEnvSTM  h Query.getCurrentChain
          , getCurrentLedger   = getEnvSTM  h Query.getCurrentLedger
          , getPastLedger      = getEnv1    h Query.getPastLedger
          , getTipBlock        = getEnv     h Query.getTipBlock
          , getTipHeader       = getEnv     h Query.getTipHeader
          , getTipPoint        = getEnvSTM  h Query.getTipPoint
          , getTipBlockNo      = getEnvSTM  h Query.getTipBlockNo
          , getBlockComponent  = getEnv2    h Query.getBlockComponent
          , getIsFetched       = getEnvSTM  h Query.getIsFetched
          , getMaxSlotNo       = getEnvSTM  h Query.getMaxSlotNo
          , stream             = Iterator.stream  h
          , newReader          = Reader.newReader h (Args.cdbEncodeHeader args)
          , getIsInvalidBlock  = getEnvSTM  h Query.getIsInvalidBlock
          , closeDB            = Reopen.closeDB h
          , isOpen             = Reopen.isOpen  h
          }
        testing = Internal
          { intReopen_                 = Reopen.reopen  h
          , intCopyToImmDB             = getEnv  h Background.copyToImmDB
          , intGarbageCollect          = getEnv1 h Background.garbageCollect
          , intUpdateLedgerSnapshots   = getEnv  h Background.updateLedgerSnapshots
          , intScheduledChainSelection = getEnv1 h Background.scheduledChainSelection
          , intKillBgThreads           = varKillBgThreads
          }

    traceWith tracer $ TraceOpenEvent $ OpenedDB
      { _immTip   = castPoint $ AF.anchorPoint chain
      , _chainTip = castPoint $ AF.headPoint   chain
      }

    when launchBgTasks $ Background.launchBgTasks env

    return (chainDB, testing)
  where
    tracer = Args.cdbTracer args
    (argsImmDb, argsVolDb, argsLgrDb, _) = Args.fromChainDbArgs args

    blockEpoch :: forall b. HasHeader b => b -> m EpochNo
    blockEpoch = epochInfoEpoch (Args.cdbEpochInfo args) . blockSlot
