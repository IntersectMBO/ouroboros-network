{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Types used throughout the implementation: handle, state, environment,
-- types, trace types, etc.
module Ouroboros.Storage.ChainDB.Impl.Types (
    ChainDbHandle (..)
  , getEnv
  , getEnv1
  , getEnv2
  , getEnvSTM
  , ChainDbState (..)
  , ChainDbEnv (..)
    -- * Exposed internals for testing purposes
  , Internal (..)
    -- * Reader-related
  , ReaderState (..)
  , ReaderRollState (..)
  , readerRollStatePoint
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
  ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict (Map)
import           Data.Time.Clock (DiffTime)
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)

import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (BlockNo, HasHeader, HeaderHash, Point,
                     SlotNo)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block (BlockProtocol, Header, IsEBB (..))
import           Ouroboros.Consensus.Ledger.Abstract (ProtocolLedgerView)
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError)
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (WithFingerprint)

import           Ouroboros.Storage.Common (EpochNo)
import           Ouroboros.Storage.EpochInfo (EpochInfo)

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..), IteratorId,
                     ReaderId, StreamFrom, StreamTo, UnknownRange)

import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Storage.ChainDB.Impl.LgrDB (LgrDB)
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.Impl.VolDB (VolDB)

-- | A handle to the internal ChainDB state
newtype ChainDbHandle m blk = CDBHandle (StrictTVar m (ChainDbState m blk))

-- | Check if the ChainDB is open, if so, executing the given function on the
-- 'ChainDbEnv', otherwise, throw a 'CloseDBError'.
getEnv :: forall m blk r. IOLike m
       => ChainDbHandle m blk
       -> (ChainDbEnv m blk -> m r)
       -> m r
getEnv (CDBHandle varState) f = atomically (readTVar varState) >>= \case
    ChainDbOpen    env -> f env
    ChainDbClosed _env -> throwM ClosedDBError
    -- See the docstring of 'ChainDbReopening'
    ChainDbReopening   -> error "ChainDB used while reopening"

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 :: IOLike m
        => ChainDbHandle m blk
        -> (ChainDbEnv m blk -> a -> m r)
        -> a -> m r
getEnv1 h f a = getEnv h (\env -> f env a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 :: IOLike m
        => ChainDbHandle m blk
        -> (ChainDbEnv m blk -> a -> b -> m r)
        -> a -> b -> m r
getEnv2 h f a b = getEnv h (\env -> f env a b)


-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM :: forall m blk r. IOLike m
          => ChainDbHandle m blk
          -> (ChainDbEnv m blk -> STM m r)
          -> STM m r
getEnvSTM (CDBHandle varState) f = readTVar varState >>= \case
    ChainDbOpen    env -> f env
    ChainDbClosed _env -> throwM ClosedDBError
    -- See the docstring of 'ChainDbReopening'
    ChainDbReopening   -> error "ChainDB used while reopening"

data ChainDbState m blk
  = ChainDbOpen   !(ChainDbEnv m blk)
  | ChainDbClosed !(ChainDbEnv m blk)
    -- ^ Note: this 'ChainDbEnv' will only be used to reopen the ChainDB.
  | ChainDbReopening
    -- ^ The ChainDB is being reopened, this should not be performed
    -- concurrently with any other operations, including reopening itself.
    --
    -- This state can only be reached by the 'intReopen' function, which is an
    -- internal function only exposed for testing. During normal use of the
    -- 'ChainDB', it should /never/ be used.
  deriving (Generic, NoUnexpectedThunks)

data ChainDbEnv m blk = CDB
  { cdbImmDB          :: !(ImmDB m blk)
  , cdbVolDB          :: !(VolDB m blk)
  , cdbLgrDB          :: !(LgrDB m blk)
  , cdbChain          :: !(StrictTVar m (AnchoredFragment (Header blk)))
    -- ^ Contains the current chain fragment.
    --
    -- INVARIANT: the anchor point of this fragment is the tip of the
    -- ImmutableDB.
    --
    -- Note that this fragment might be shorter than @k@ headers when the
    -- whole chain is shorter than @k@ or in case of corruption of the
    -- VolatileDB.
    --
    -- Note that this fragment might also be /longer/ than @k@ headers,
    -- because the oldest blocks from the fragment might not yet have been
    -- copied from the VolatileDB to the ImmutableDB.
  , cdbImmBlockNo     :: !(StrictTVar m BlockNo)
    -- ^ The block number corresponding to the block @k@ blocks back. This is
    -- the most recent \"immutable\" block according to the protocol, i.e., a
    -- block that cannot be rolled back.
    --
    -- INVARIANT: the anchor point of 'getCurrentChain' refers to the same
    -- block as this 'BlockNo'.
    --
    -- Note that the \"immutable\" block isn't necessarily at the tip of the
    -- ImmutableDB, but could temporarily still be on the in-memory chain
    -- fragment. When the background thread that copies blocks to the
    -- ImmutableDB has caught up, the \"immutable\" block will be at the tip
    -- of the ImmutableDB again.
    --
    -- Note that the \"immutable\" block might be less than @k@ blocks from
    -- our tip in case the whole chain is shorter than @k@ or in case of
    -- corruption of the VolatileDB.
    --
    -- Note that the \"immutable\" block will /never/ be /more/ than @k@
    -- blocks back, as opposed to the anchor point of 'cdbChain'.
  , cdbIterators      :: !(StrictTVar m (Map IteratorId (m ())))
    -- ^ The iterators.
    --
    -- This maps the 'IteratorId's of each open 'Iterator' to a function that,
    -- when called, closes the iterator. This is used when closing the
    -- ChainDB: the open file handles used by iterators can be closed, and the
    -- iterators themselves are closed so that it is impossible to use an
    -- iterator after closing the ChainDB itself.
  , cdbReaders        :: !(StrictTVar m (Map ReaderId (StrictTVar m (ReaderState m blk))))
    -- ^ The readers.
    --
    -- INVARIANT: the 'readerPoint' of each reader is 'withinFragmentBounds'
    -- of the current chain fragment (retrieved 'cdbGetCurrentChain', not by
    -- reading 'cdbChain' directly).
  , cdbNodeConfig     :: !(NodeConfig (BlockProtocol blk))
  , cdbInvalid        :: !(StrictTVar m (WithFingerprint (Map (HeaderHash blk) SlotNo)))
    -- ^ Hashes corresponding to invalid blocks. This is used to ignore these
    -- blocks during chain selection.
    --
    -- The slot number of the block is stored too, so that whenever a garbage
    -- collection is performed on the VolatileDB for some slot @s@, the hashes
    -- older or equal to @s@ can be removed from this map.
    --
    -- The 'Fingerprint' changes every time a hash is added to the map, but
    -- not when hashes are garbage-collected from the map.
  , cdbNextIteratorId :: !(StrictTVar m IteratorId)
  , cdbNextReaderId   :: !(StrictTVar m ReaderId)
  , cdbCopyLock       :: !(StrictMVar m ())
    -- ^ Lock used to ensure that 'copyToImmDB' is not executed more than
    -- once concurrently.
    --
    -- Note that 'copyToImmDB' can still be executed concurrently with all
    -- others functions, just not with itself.
  , cdbTracer         :: !(Tracer m (TraceEvent blk))
  , cdbTraceLedger    :: !(Tracer m (LgrDB.LedgerDB blk))
  , cdbRegistry       :: !(ResourceRegistry m)
    -- ^ Resource registry that will be used to (re)start the background
    -- threads, see 'cdbBgThreads'.
  , cdbGcDelay        :: !DiffTime
    -- ^ How long to wait between copying a block from the VolatileDB to
    -- ImmutableDB and garbage collecting it from the VolatileDB
  , cdbBgThreads      :: !(StrictTVar m [Thread m ()])
    -- ^ The background threads.
  , cdbEpochInfo      :: !(EpochInfo m)
  , cdbIsEBB          :: !(blk -> Bool)
  } deriving (Generic)

-- | We include @blk@ in 'showTypeOf' because it helps resolving type families
-- (but avoid including @m@ because we cannot impose @Typeable m@ as a
-- constraint and still have it work with the simulator)
instance (IOLike m, ProtocolLedgerView blk)
      => NoUnexpectedThunks (ChainDbEnv m blk) where
    showTypeOf _ = "ChainDbEnv m " ++ show (typeRep (Proxy @blk))

{-------------------------------------------------------------------------------
  Exposed internals for testing purposes
-------------------------------------------------------------------------------}

data Internal m blk = Internal
  { intReopen                :: Bool -> m ()
    -- ^ Reopen a closed ChainDB.
    --
    -- A no-op if the ChainDB is still open.
    --
    -- NOTE: not thread-safe, no other operation should be called on the
    -- ChainDB at the same time.
    --
    -- The 'Bool' arguments indicates whether the background tasks should be
    -- relaunched after reopening the ChainDB.
  , intCopyToImmDB           :: m (WithOrigin SlotNo)
    -- ^ Copy the blocks older than @k@ from to the VolatileDB to the
    -- ImmutableDB and update the in-memory chain fragment correspondingly.
    --
    -- The 'SlotNo' of the tip of the ImmutableDB after copying the blocks is
    -- returned. This can be used for a garbage collection on the VolatileDB.
  , intGarbageCollect        :: SlotNo -> m ()
    -- ^ Perform garbage collection for blocks <= the given 'SlotNo'.
  , intUpdateLedgerSnapshots :: m ()
    -- ^ Write a new LedgerDB snapshot to disk and remove the oldest one(s).
  , intBgThreads             :: StrictTVar m [Thread m ()]
      -- ^ The background threads.
  }

{-------------------------------------------------------------------------------
  Reader-related
-------------------------------------------------------------------------------}

-- Note: these things are not in the Reader module, because 'TraceEvent'
-- depends on them, 'ChainDbEnv.cdbTracer' depends on 'TraceEvent', and most
-- modules depend on 'ChainDbEnv'. Also, 'ChainDbEnv.cdbReaders' depends on
-- 'ReaderState'.

data ReaderState m blk
  = ReaderInImmDB !(ReaderRollState blk) !(ImmDB.Iterator (HeaderHash blk) m blk)
    -- ^ The 'Reader' is reading from the ImmutableDB
  | ReaderInMem   !(ReaderRollState blk)
    -- ^ The 'Reader' is reading from the in-memory current chain fragment.
  deriving (Generic, NoUnexpectedThunks)

-- | Similar to 'Ouroboros.Network.MockChain.ProducerState.ReaderState'.
data ReaderRollState blk
  = RollBackTo      !(Point blk)
    -- ^ The reader should roll back to this point.
  | RollForwardFrom !(Point blk)
    -- ^ The reader should roll forward from this point.
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

-- | Get the point the 'ReaderRollState' should roll back to or roll forward
-- from.
readerRollStatePoint :: ReaderRollState blk -> Point blk
readerRollStatePoint (RollBackTo      pt) = pt
readerRollStatePoint (RollForwardFrom pt) = pt

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

-- | Trace type for the various events of the ChainDB.
data TraceEvent blk
  = TraceAddBlockEvent     (TraceAddBlockEvent     blk)
  | TraceReaderEvent       (TraceReaderEvent       blk)
  | TraceCopyToImmDBEvent  (TraceCopyToImmDBEvent  blk)
  | TraceGCEvent           (TraceGCEvent           blk)
  | TraceInitChainSelEvent (TraceInitChainSelEvent blk)
  | TraceOpenEvent         (TraceOpenEvent         blk)
  | TraceIteratorEvent     (TraceIteratorEvent     blk)
  | TraceLedgerEvent       (LgrDB.TraceEvent (Point blk))
  | TraceLedgerReplayEvent (LgrDB.TraceLedgerReplayEvent blk)
  | TraceImmDBEvent        (ImmDB.TraceEvent       blk)
  deriving (Generic)

deriving instance
  ( HasHeader blk
  , Eq (Header blk)
  , ProtocolLedgerView blk
  ) => Eq (TraceEvent blk)
deriving instance
  ( HasHeader blk
  , Show (Header blk)
  , ProtocolLedgerView blk
  ) => Show (TraceEvent blk)

data TraceOpenEvent blk
  = OpenedDB
    { _immTip   :: Point blk
    , _chainTip :: Point blk
    }
    -- ^ The ChainDB was opened.
  | ClosedDB
    { _immTip   :: Point blk
    , _chainTip :: Point blk
    }
    -- ^ The ChainDB was closed.
  | ReopenedDB
    { _immTip   :: Point blk
    , _chainTip :: Point blk
    }
    -- ^ The ChainDB was successfully reopened.
  | OpenedImmDB
    { _immDbTip      :: Point blk
    , _immDbTipEpoch :: EpochNo
    }
    -- ^ The ImmutableDB was opened.
  | OpenedVolDB
    -- ^ The VolatileDB was opened.
  | OpenedLgrDB
    -- ^ The LedgerDB was opened.
  deriving (Generic, Eq, Show)

-- | Trace type for the various events that occur when adding a block.
data TraceAddBlockEvent blk
  = AddedBlockToVolDB    !(Point blk) !BlockNo !IsEBB
    -- ^ A block was added to the Volatile DB

  | TryAddToCurrentChain (Point blk)
    -- ^ The block fits onto the current chain, we'll try to use it to extend
    -- our chain.

  | TrySwitchToAFork     (Point blk) (NonEmpty (HeaderHash blk))
    -- ^ The block fits onto some fork, we'll try to switch to that fork (if
    -- it is preferable to our chain).

  | StoreButDontChange   (Point blk)
    -- ^ The block doesn't fit onto any other block, so we store it and ignore
    -- it.

  | SwitchedToChain
    { _prevChain :: AnchoredFragment (Header blk)
    , _newChain  :: AnchoredFragment (Header blk)
    }
    -- ^ We successfully installed a new chain.

  | ChainChangedInBg
    { _prevChain :: AnchoredFragment (Header blk)
    , _newChain  :: AnchoredFragment (Header blk)
    }
    -- ^ We have found a new chain, but the current chain has changed in the
    -- background such that our new chain is no longer preferable to the
    -- current chain.

  | AddBlockValidation (TraceValidationEvent blk)
    -- ^ An event traced during validating performed while adding a block.
  deriving (Generic)

deriving instance
  ( HasHeader                 blk
  , Eq (Header                blk)
  , ProtocolLedgerView        blk
  ) => Eq (TraceAddBlockEvent blk)
deriving instance
  ( HasHeader                   blk
  , Show (Header                blk)
  , ProtocolLedgerView          blk
  ) => Show (TraceAddBlockEvent blk)

data TraceValidationEvent blk
  = InvalidBlock
    { _validationErr :: ExtValidationError blk
    , _invalidPoint  :: Point blk
    }
    -- ^ A point was found to be invalid.

  | InvalidCandidate
    { _candidate     :: AnchoredFragment (Header blk)
    }
    -- ^ A candidate chain was invalid.

  | ValidCandidate (AnchoredFragment (Header blk))
    -- ^ A candidate chain was valid.

  | CandidateExceedsRollback
    { _supportedRollback :: Word64
    , _candidateRollback :: Word64
    , _candidate         :: AnchoredFragment (Header blk)
    }
    -- ^ Candidate required rollback past what LedgerDB supported
    --
    -- This should only happen in exceptional circumstances (like after
    -- disk corruption).
  deriving (Generic)

deriving instance
  ( HasHeader                 blk
  , Eq (Header                blk)
  , ProtocolLedgerView        blk
  ) => Eq (TraceValidationEvent blk)
deriving instance
  ( Show (Header                blk)
  , ProtocolLedgerView          blk
  ) => Show (TraceValidationEvent blk)

data TraceInitChainSelEvent blk
  = InitChainSelValidation (TraceValidationEvent blk)
    -- ^ An event traced during validation performed while performing initial
    -- chain selection.
  deriving (Generic)

deriving instance
  ( HasHeader                     blk
  , Eq (Header                    blk)
  , ProtocolLedgerView            blk
  ) => Eq (TraceInitChainSelEvent blk)
deriving instance
  ( Show (Header                    blk)
  , ProtocolLedgerView              blk
  ) => Show (TraceInitChainSelEvent blk)


data TraceReaderEvent blk
  = NewReader ReaderId
    -- ^ A new reader was created.

  | ReaderNoLongerInMem (ReaderRollState blk)
    -- ^ The reader was in the 'ReaderInMem' state but its point is no longer
    -- on the in-memory chain fragment, so it has to switch to the
    -- 'ReaderInImmDB' state.

  | ReaderSwitchToMem
    { _readerPoint      :: Point blk
    , _slotNoAtImmDBTip :: WithOrigin SlotNo
    }
    -- ^ The reader was in the 'ReaderInImmDB' state and is switched to the
    -- 'ReaderInMem' state.

  | ReaderNewImmIterator
    { _readerPoint      :: Point blk
    , _slotNoAtImmDBTip :: WithOrigin SlotNo
    }
    -- ^ The reader is in the 'ReaderInImmDB' state but the iterator is
    -- exhausted while the ImmutableDB has grown, so we open a new iterator to
    -- stream these blocks too.
  deriving (Generic, Eq, Show)


data TraceCopyToImmDBEvent blk
  = CopiedBlockToImmDB (Point blk)
    -- ^ A block was successfully copied to the ImmutableDB.
  | NoBlocksToCopyToImmDB
    -- ^ There are no block to copy to the ImmutableDB.
  deriving (Generic, Eq, Show)

data TraceGCEvent blk
  = ScheduledGC SlotNo DiffTime
    -- ^ A garbage collection for the given 'SlotNo' was scheduled to happen
    -- after the given delay.
  | PerformedGC SlotNo
    -- ^ A garbage collection for the given 'SlotNo' was performed.
  deriving (Generic, Eq, Show)

data TraceIteratorEvent blk
  = UnknownRangeRequested (UnknownRange blk)
    -- ^ An unknown range was requested, see 'UnknownRange'.
  | StreamFromVolDB
    { _streamFrom :: StreamFrom blk
    , _streamTo   :: StreamTo   blk
    , _hashes     :: [HeaderHash blk]
    }
    -- ^ Stream only from the VolatileDB.
  | StreamFromImmDB
    { _streamFrom :: StreamFrom blk
    , _streamTo   :: StreamTo   blk
    }
    -- ^ Stream only from the ImmutableDB.
  | StreamFromBoth
    { _streamFrom :: StreamFrom blk
    , _streamTo   :: StreamTo   blk
    , _hashes     :: [HeaderHash blk]
    }
    -- ^ Stream from both the VolatileDB and the ImmutableDB.
  | BlockMissingFromVolDB (HeaderHash blk)
    -- ^ A block is no longer in the VolatileDB because it has been garbage
    -- collected. It might now be in the ImmutableDB if it was part of the
    -- current chain.
  | BlockWasCopiedToImmDB (HeaderHash blk)
    -- ^ A block that has been garbage collected from the VolatileDB is now
    -- found and streamed from the ImmutableDB.
  | BlockGCedFromVolDB    (HeaderHash blk)
    -- ^ A block is no longer in the VolatileDB and isn't in the ImmutableDB
    -- either; it wasn't part of the current chain.
  | SwitchBackToVolDB
    -- ^ We have stream one or more blocks from the ImmutableDB that were part
    -- of the VolatileDB when initialising the iterator. Now, we have to look
    -- back in the VolatileDB again because the ImmutableDB doesn't have the
    -- next block we're looking for.
  deriving (Generic, Eq, Show)
