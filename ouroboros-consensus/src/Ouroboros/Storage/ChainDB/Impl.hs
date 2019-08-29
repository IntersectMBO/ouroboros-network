{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module Ouroboros.Storage.ChainDB.Impl (
    -- * Initialization
    ChainDbArgs(..)
  , defaultArgs
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
import qualified Data.Map.Strict as Map

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm

import           Control.Tracer

import           Cardano.Prelude (NoUnexpectedThunks (..))

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockNo, blockPoint, blockSlot,
                     castPoint, genesisBlockNo, genesisPoint)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (unsafeNoThunks)
import           Ouroboros.Consensus.Util.STM (Fingerprint (..))

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

openDB
  :: forall m blk.
     ( MonadAsync m
     , MonadFork  m
     , MonadST    m
     , MonadMask  m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow (STM m)
     , ProtocolLedgerView blk
     , NoUnexpectedThunks (m ())
     , NoUnexpectedThunks (StrictTVar m (ReaderState m blk))
     )
  => ChainDbArgs m blk
  -> m (ChainDB m blk)
openDB args = fst <$> openDBInternal args True

openDBInternal
  :: forall m blk.
     ( MonadAsync m
     , MonadFork  m
     , MonadST    m
     , MonadMask  m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow (STM m)
     , ProtocolLedgerView blk
     , NoUnexpectedThunks (m ())
     , NoUnexpectedThunks (StrictTVar m (ReaderState m blk))
     )
  => ChainDbArgs m blk
  -> Bool -- ^ 'True' = Launch background tasks
  -> m (ChainDB m blk, Internal m blk)
openDBInternal args launchBgTasks = do
    immDB <- ImmDB.openDB argsImmDb
    -- In order to figure out the 'BlockNo' and 'Point' at the tip of the
    -- ImmutableDB, we need to read the block at the tip of the ImmutableDB.
    immDbTipBlock <- ImmDB.getBlockAtTip immDB
    -- Note that 'immDbTipBlockNo' might not end up being the \"immutable\"
    -- block(no), because the current chain computed from the VolatileDB could
    -- be longer than @k@.
    let immDbTipBlockNo = maybe genesisBlockNo blockNo    immDbTipBlock
        immDbTipPoint   = maybe genesisPoint   blockPoint immDbTipBlock
    immDbTipEpoch      <- maybe (return 0)     blockEpoch immDbTipBlock
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

    varInvalid <- uncheckedNewTVarM (Map.empty, Fingerprint 0)

    chainAndLedger <- ChainSel.initialChainSelection
      immDB
      volDB
      lgrDB
      tracer
      (Args.cdbNodeConfig args)
      varInvalid

    let chain      = ChainSel.clChain  chainAndLedger
        ledger     = ChainSel.clLedger chainAndLedger
        cfg        = Args.cdbNodeConfig args
        secParam   = protocolSecurityParam cfg
        immBlockNo = ChainSel.getImmBlockNo secParam chain immDbTipBlockNo

    atomically $ LgrDB.setCurrent lgrDB ledger
    varChain          <- newTVarM  chain
    varImmBlockNo     <- newTVarM  immBlockNo
    varIterators      <- newTVarM  Map.empty
    varReaders        <- newTVarM  Map.empty
    varNextIteratorId <- newTVarM  (IteratorId 0)
    varNextReaderId   <- newTVarM  0
    varCopyLock       <- newTMVarM ()
    varBgThreads      <- newTVarM  []

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
                  , cdbRegistry       = Args.cdbRegistry args
                  , cdbGcDelay        = Args.cdbGcDelay args
                  , cdbBgThreads      = varBgThreads
                  , cdbEpochInfo      = Args.cdbEpochInfo args
                  }
    h <- fmap CDBHandle $ uncheckedNewTVarM $ ChainDbOpen env
    let chainDB = ChainDB
          { addBlock           = getEnv1    h ChainSel.addBlock
          , getCurrentChain    = getEnvSTM  h Query.getCurrentChain
          , getCurrentLedger   = getEnvSTM  h Query.getCurrentLedger
          , getTipBlock        = getEnv     h Query.getTipBlock
          , getTipHeader       = getEnv     h Query.getTipHeader
          , getTipPoint        = getEnvSTM  h Query.getTipPoint
          , getTipBlockNo      = getEnvSTM  h Query.getTipBlockNo
          , getBlock           = getEnv1    h Query.getBlock
          , getIsFetched       = getEnvSTM  h Query.getIsFetched
          , streamBlocks       = Iterator.streamBlocks  h
          , newHeaderReader    = Reader.newHeaderReader h
          , newBlockReader     = Reader.newBlockReader  h
          , getIsInvalidBlock  = getEnvSTM  h Query.getIsInvalidBlock
          , closeDB            = Reopen.closeDB h
          , isOpen             = Reopen.isOpen  h
          }
        testing = Internal
          { intReopen                = Reopen.reopen  h
          , intCopyToImmDB           = getEnv  h Background.copyToImmDB
          , intGarbageCollect        = getEnv1 h Background.garbageCollect
          , intUpdateLedgerSnapshots = getEnv  h Background.updateLedgerSnapshots
          , intBgThreads             = varBgThreads
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

    blockEpoch :: blk -> m EpochNo
    blockEpoch = epochInfoEpoch (Args.cdbEpochInfo args) . blockSlot
