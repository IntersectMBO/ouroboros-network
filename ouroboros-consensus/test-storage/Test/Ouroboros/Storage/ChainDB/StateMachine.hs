{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Ouroboros.Storage.ChainDB.StateMachine ( tests ) where

import           Prelude hiding (elem)

import           Codec.Serialise (Serialise, decode, encode)
import           Control.Monad (forM_, replicateM, unless, when)
import           Control.Monad.State (StateT, evalStateT, get, lift, modify,
                     put)
import           Control.Tracer
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Bifunctor.TH as TH
import           Data.Bitraversable
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (toList)
import           Data.Functor.Classes (Eq1, Show1)
import           Data.Functor.Identity (Identity (..))
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Ord (Down (..))
import           Data.Proxy
import           Data.Sequence.Strict (StrictSeq)
import           Data.TreeDiff (ToExpr (..))
import           Data.Typeable
import           Data.Word (Word16, Word32)
import           GHC.Generics (Generic)
import           GHC.Stack (callStack)

import qualified Generics.SOP as SOP

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.StateMachine
import           Test.StateMachine.Labelling
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Cardano.Crypto.DSIGN.Mock
import           Cardano.Slotting.Slot hiding (At)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo, ChainHash (..), ChainUpdate,
                     HasHeader (..), HeaderHash, MaxSlotNo, Point, SlotNo (..),
                     StandardHash)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.MockChain.ProducerState (ChainProducerState,
                     ReaderNext, ReaderState)
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import qualified Ouroboros.Network.Point as Point

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
                     (settableBlockchainTime)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util (takeLast)
import           Ouroboros.Consensus.Util.AnchoredFragment
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike hiding (fork)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))

import           Ouroboros.Consensus.Storage.ChainDB hiding
                     (TraceReaderEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB
                     (ValidationPolicy (ValidateAllChunks))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (unsafeChunkNoToEpochNo)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory
                     (LedgerDbParams (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB

import           Test.Ouroboros.Storage.ChainDB.Model (IteratorId,
                     ModelSupportsBlock, ReaderId)
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import           Test.Ouroboros.Storage.TestBlock

import           Test.Util.ChunkInfo
import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.QuickCheck ((=:=))
import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE
import           Test.Util.SOP
import           Test.Util.Tracer (recordingTracerIORef)
import           Test.Util.WithEq

{-------------------------------------------------------------------------------
  Abstract model
-------------------------------------------------------------------------------}

-- | Commands
data Cmd blk it rdr
  = AddBlock              blk
  | AddFutureBlock        blk SlotNo -- ^ The current slot number
  | GetCurrentChain
  | GetCurrentLedger
  | GetPastLedger         (Point blk)
  | GetTipBlock
  | GetTipHeader
  | GetTipPoint
  | GetBlockComponent     (RealPoint blk)
  | GetGCedBlockComponent (RealPoint blk)
    -- ^ Only for blocks that may have been garbage collected.
  | GetMaxSlotNo
  | Stream                (StreamFrom blk) (StreamTo blk)
  | IteratorNext          it
  | IteratorNextGCed      it
    -- ^ Only for blocks that may have been garbage collected.
  | IteratorClose         it
  | NewReader
  | ReaderInstruction     rdr
    -- ^ 'readerInstructionBlocking' is excluded, as it requires multiple
    -- threads. Its code path is pretty much the same as 'readerInstruction'
    -- anyway.
  | ReaderForward         rdr [Point blk]
  | ReaderClose           rdr
  | Close
  | Reopen
    -- Internal
  | RunBgTasks
  deriving (Generic, Show, Functor, Foldable, Traversable)

-- = Invalid blocks
--
-- We don't test 'getIsInvalidBlock' because the simple chain selection in the
-- model and the incremental chain selection in the real implementation differ
-- non-trivially.
--
-- In the real chain selection, if a block is invalid, no chains containing
-- that block will be validated again. So if a successor of the block is added
-- afterwards, it will never be validated, as any chain it would be part of
-- would also contain the block we know to be invalid. So if the successor is
-- also invalid, we would never discover and record it (as there is no need
-- from the point of chain selection, as we'll never use it in a candidate
-- chain anyway). In the model implementation of chain selection, all possible
-- chains are (re)validated each time, including previously invalid chains, so
-- new invalid blocks that are successors of known invalid blocks /are/ being
-- validated and recorded as invalid blocks.
--
-- Further complicating this is the fact that the recorded invalid blocks are
-- also garbage-collected. We can work around this, just like for 'getBlock'.
--
-- While it is certainly possible to overcome the issues described above,
-- e.g., we could change the model to closer match the real implementation
-- (but at the cost of more complexity), it is not worth the effort. The whole
-- point of recording invalid blocks is to avoid constructing candidates
-- containing known invalid blocks and needlessly validating them, which is
-- something we are testing in 'prop_trace', see
-- 'invalidBlockNeverValidatedAgain'.

-- = Blocks from the future
--
-- In the real implementation, blocks from the future are added to the
-- VolatileDB directly, but the chain selection triggered by that block is
-- scheduled for the slot of the block.
--
-- This means the implementation relies on the 'BlockchainTime'. In the tests,
-- we advance the slot number only when a new block is added, as that is the
-- only mutation of the ChainDB. Whenever a new block is added, the current
-- slot number is advanced to the slot of that block (unless that slot is in
-- the past). Blocks from the future include an extra slot number, which is
-- treated as the current slot number while adding the block. This slot number
-- must be >= the actual current slot number.
--
-- Whenever we advance the current slot number, we trigger the corresponding
-- scheduled chain selections in the real implementation. For the model, we
-- call 'Model.advanceCurSlot' which adds the blocks to the model stored in
-- 'Model.futureBlocks' which are no longer in the future.

deriving instance SOP.Generic         (Cmd blk it rdr)
deriving instance SOP.HasDatatypeInfo (Cmd blk it rdr)

-- | Return type for successful database operations.
data Success blk it rdr
  = Unit                ()
  | Chain               (AnchoredFragment (Header blk))
  | Ledger              (ExtLedgerState blk)
  | MbLedger            (Maybe (ExtLedgerState blk))
  | MbBlock             (Maybe blk)
  | MbAllComponents     (Maybe (AllComponents blk))
  | MbGCedAllComponents (MaybeGCedBlock (AllComponents blk))
  | MbHeader            (Maybe (Header blk))
  | Point               (Point blk)
  | BlockNo             BlockNo
  | UnknownRange        (UnknownRange blk)
  | Iter                it
  | IterResult          (IteratorResult blk (AllComponents blk))
  | IterResultGCed      (IteratorResultGCed blk)
  | Rdr                 rdr
  | MbChainUpdate       (Maybe (ChainUpdate blk (AllComponents blk)))
  | MbPoint             (Maybe (Point blk))
  | MaxSlot             MaxSlotNo
  deriving (Functor, Foldable, Traversable)

-- | Product of all 'BlockComponent's. As this is a GADT, generating random
-- values of it (and combinations!) is not so simple. Therefore, we just
-- always request all block components.
allComponents :: BlockComponent (ChainDB m blk) (AllComponentsM m blk)
allComponents = (,,,,,,,,)
    <$> GetBlock
    <*> GetHeader
    <*> GetRawBlock
    <*> GetRawHeader
    <*> GetHash
    <*> GetSlot
    <*> GetIsEBB
    <*> GetBlockSize
    <*> GetHeaderSize

-- | 'AllComponentsM' instantiated to 'Identity'.
type AllComponents blk = AllComponentsM Identity blk

-- | A list of all the 'BlockComponent' indices (@b@) we are interested in.
type AllComponentsM m blk =
  ( m blk
  , m (Header blk)
  , ByteString
  , ByteString
  , HeaderHash blk
  , SlotNo
  , IsEBB
  , Word32
  , Word16
  )

-- | Convert @'AllComponentsM m'@ to 'AllComponents'
runAllComponentsM :: IOLike m => AllComponentsM m blk -> m (AllComponents blk)
runAllComponentsM (mblk, mhdr, a, b, c, d, e, f, g) = do
    blk <- mblk
    hdr <- mhdr
    return (Identity blk, Identity hdr, a, b, c, d, e, f, g)

type TestConstraints blk =
  ( ConsensusProtocol  (BlockProtocol blk)
  , LedgerSupportsProtocol            blk
  , Eq (ConsensusState (BlockProtocol blk))
  , Eq (LedgerState                   blk)
  , Eq                                blk
  , Show                              blk
  , HasHeader                         blk
  , StandardHash                      blk
  , Serialise                         blk
  , ModelSupportsBlock                blk
  , Eq                       (Header  blk)
  , Show                     (Header  blk)
  , Serialise                (Header  blk)
  )

deriving instance (TestConstraints blk, Eq   it, Eq   rdr)
               => Eq   (Success blk it rdr)
deriving instance (TestConstraints blk, Show it, Show rdr)
               => Show (Success blk it rdr)

-- | Short-hand
type TestIterator m blk = WithEq (Iterator m blk (AllComponents blk))
-- | Short-hand
type TestReader   m blk = WithEq (Reader   m blk (AllComponents blk))

run :: forall m blk. (IOLike m, HasHeader blk)
    => ChainDB          m blk
    -> ChainDB.Internal m blk
    -> ResourceRegistry m
    -> StrictTVar m SlotNo
    -> StrictTVar m Id
    ->    Cmd     blk (TestIterator m blk) (TestReader m blk)
    -> m (Success blk (TestIterator m blk) (TestReader m blk))
run chainDB@ChainDB{..} internal registry varCurSlot varNextId = \case
    AddBlock blk             -> Point               <$> (advanceAndAdd (blockSlot blk) blk)
    AddFutureBlock blk s     -> Point               <$> (advanceAndAdd s               blk)
    GetCurrentChain          -> Chain               <$> atomically getCurrentChain
    GetCurrentLedger         -> Ledger              <$> atomically getCurrentLedger
    GetPastLedger pt         -> MbLedger            <$> getPastLedger chainDB pt
    GetTipBlock              -> MbBlock             <$> getTipBlock
    GetTipHeader             -> MbHeader            <$> getTipHeader
    GetTipPoint              -> Point               <$> atomically getTipPoint
    GetBlockComponent pt     -> mbAllComponents     =<< getBlockComponent allComponents pt
    GetGCedBlockComponent pt -> mbGCedAllComponents =<< getBlockComponent allComponents pt
    GetMaxSlotNo             -> MaxSlot             <$> atomically getMaxSlotNo
    Stream from to           -> iter                =<< stream registry allComponents from to
    IteratorNext  it         -> IterResult          <$> iteratorNext (unWithEq it)
    IteratorNextGCed  it     -> iterResultGCed      <$> iteratorNext (unWithEq it)
    IteratorClose it         -> Unit                <$> iteratorClose (unWithEq it)
    NewReader                -> reader              =<< newReader registry allComponents
    ReaderInstruction rdr    -> MbChainUpdate       <$> readerInstruction (unWithEq rdr)
    ReaderForward rdr pts    -> MbPoint             <$> readerForward (unWithEq rdr) pts
    ReaderClose rdr          -> Unit                <$> readerClose (unWithEq rdr)
    Close                    -> Unit                <$> closeDB
    Reopen                   -> Unit                <$> intReopen internal False
    RunBgTasks               -> ignore              <$> runBgTasks internal
  where
    mbAllComponents = fmap MbAllComponents . traverse runAllComponentsM
    mbGCedAllComponents = fmap (MbGCedAllComponents . MaybeGCedBlock True) . traverse runAllComponentsM
    iterResultGCed = IterResultGCed . IteratorResultGCed True
    iter = either (return . UnknownRange) (fmap Iter . giveWithEq . traverseIterator runAllComponentsM)
    reader = fmap Rdr . giveWithEq . traverseReader runAllComponentsM
    ignore _ = Unit ()

    advanceAndAdd newCurSlot blk = do
      open <- atomically isOpen
      when open $ do
        prevCurSlot <- atomically $ readTVar varCurSlot
        -- Tick each slot and execute the scheduled chain selections for that
        -- slot
        forM_ [prevCurSlot + 1..newCurSlot] $ \slot -> do
          atomically $ writeTVar varCurSlot slot
          intScheduledChainSelection internal slot
      addBlock chainDB blk

    giveWithEq :: a -> m (WithEq a)
    giveWithEq a =
      fmap (`WithEq` a) $ atomically $ updateTVar varNextId $ \i -> (succ i, i)

runBgTasks :: IOLike m => ChainDB.Internal m blk -> m ()
runBgTasks ChainDB.Internal{..} = do
    mSlotNo <- intCopyToImmDB
    case mSlotNo of
      Point.Origin    -> pure ()
      Point.At slotNo -> intGarbageCollect slotNo
    intUpdateLedgerSnapshots

-- | Result type for 'getBlock'. Note that the real implementation of
-- 'getBlock' is non-deterministic: if the requested block is older than @k@
-- and not part of the current chain, it might have been garbage collected.
--
-- The first source of non-determinism is whether or not the background thread
-- that performs garbage collection has been run yet. We disable this thread
-- in the state machine tests and instead generate the 'CopyToImmDBAndGC'
-- command that triggers the garbage collection explicitly. So this source of
-- non-determinism is not a problem in the tests.
--
-- However, there is a second source of non-determinism: if a garbage
-- collection has been performed and the block was eligible for collection, it
-- might still not have been removed because it was part of a file that
-- contained other blocks that cannot be garbage collected yet. So the block
-- is still left in the VolatileDB. We do not try to imitate this behaviour,
-- which would couple the test too tightly to the actual implementation.
-- Instead, we accept this source of non-determinism and are more lenient when
-- comparing the results of 'getBlock' when the block may have been garbage
-- collected.
--
-- Equality of two 'MaybeGCedBlock' is determined as follows:
-- * If both are produced by a model implementation: the @Maybe blk@s must be
--   equal, as the these results are deterministic.
-- * If at least one of them is produced by a real implementation: if either
--   is 'Nothing', which means the block might have been garbage-collected,
--   they are equal (even if the other is 'Just', which means it was not yet
--   garbage-collected).
-- * If both are 'Just's, then the blocks must be equal.
--
-- In practice, this equality is used when comparing the result of the real
-- implementation with the result of the model implementation.
data MaybeGCedBlock blk = MaybeGCedBlock
  { real    :: Bool
    -- ^ 'True':  result of calling 'getBlock' on the real implementation
    -- ^ 'False': result of calling 'getBlock' on the model implementation
  , mbBlock :: Maybe blk
  } deriving (Show)

instance Eq blk => Eq (MaybeGCedBlock blk) where
  MaybeGCedBlock real1 mbBlock1 == MaybeGCedBlock real2 mbBlock2 =
      case (real1, real2) of
        (False, False) -> mbBlock1 == mbBlock2
        (True,  _)     -> eqIfJust
        (_,     True)  -> eqIfJust
    where
      eqIfJust = case (mbBlock1, mbBlock2) of
        (Just b1, Just b2) -> b1 == b2
        _                  -> True

-- | Similar to 'MaybeGCedBlock', but for the block returned by
-- 'iteratorNext'. A garbage-collected block could result in
-- 'IteratorBlockGCed' instead of 'IteratorResult'.
data IteratorResultGCed blk = IteratorResultGCed
  { real       :: Bool
    -- ^ 'True':  result of calling 'getBlock' on the real implementation
    -- ^ 'False': result of calling 'getBlock' on the model implementation
  , iterResult :: IteratorResult blk (AllComponents blk)
  }

deriving instance (Show blk, Show (Header blk), StandardHash blk)
               => Show (IteratorResultGCed blk)

instance (Eq blk, Eq (Header blk), StandardHash blk)
      => Eq (IteratorResultGCed blk) where
  IteratorResultGCed real1 iterResult1 == IteratorResultGCed real2 iterResult2 =
      case (real1, real2) of
        (False, False) -> iterResult1 == iterResult2
        (True,  _)     -> eqIfNotGCed
        (_,     True)  -> eqIfNotGCed
    where
      eqIfNotGCed = case (iterResult1, iterResult2) of
        (IteratorBlockGCed {}, _)                    -> True
        (_,                    IteratorBlockGCed {}) -> True
        (IteratorResult b1,    IteratorResult b2)    -> b1 == b2
        (IteratorExhausted,    IteratorExhausted)    -> True
        _                                            -> False

{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error.
newtype Resp blk it rdr = Resp
  { getResp :: Either ChainDbError (Success blk it rdr) }
  deriving (Functor, Foldable, Traversable)

deriving instance (TestConstraints blk, Show it, Show rdr)
               => Show (Resp blk it rdr)

instance (TestConstraints blk, Eq it, Eq rdr) => Eq (Resp blk it rdr) where
  Resp (Left  e) == Resp (Left  e') = e == e'
  Resp (Right a) == Resp (Right a') = a == a'
  _              == _               = False

-- We can't reuse 'run' because the 'ChainDB' API uses 'STM'. Instead, we call
-- the model directly.
runPure :: forall blk.
           TestConstraints blk
        => TopLevelConfig  blk
        -> Cmd             blk IteratorId ReaderId
        -> DBModel         blk
        -> (Resp           blk IteratorId ReaderId, DBModel blk)
runPure cfg = \case
    AddBlock blk             -> ok  Point               $ update  (advanceAndAdd (blockSlot blk) blk)
    AddFutureBlock blk s     -> ok  Point               $ update  (advanceAndAdd s               blk)
    GetCurrentChain          -> ok  Chain               $ query   (Model.lastK k getHeader)
    GetCurrentLedger         -> ok  Ledger              $ query    Model.currentLedger
    GetPastLedger pt         -> ok  MbLedger            $ query   (Model.getPastLedger cfg pt)
    GetTipBlock              -> ok  MbBlock             $ query    Model.tipBlock
    GetTipHeader             -> ok  MbHeader            $ query   (fmap getHeader . Model.tipBlock)
    GetTipPoint              -> ok  Point               $ query    Model.tipPoint
    GetBlockComponent pt     -> err MbAllComponents     $ query   (Model.getBlockComponentByPoint @Identity allComponents pt)
    GetGCedBlockComponent pt -> err mbGCedAllComponents $ query   (Model.getBlockComponentByPoint @Identity allComponents pt)
    GetMaxSlotNo             -> ok  MaxSlot             $ query    Model.maxSlotNo
    Stream from to           -> err iter                $ updateE (Model.stream k from to)
    IteratorNext  it         -> ok  IterResult          $ update  (Model.iteratorNext @Identity it allComponents)
    IteratorNextGCed it      -> ok  iterResultGCed      $ update  (Model.iteratorNext @Identity it allComponents)
    IteratorClose it         -> ok  Unit                $ update_ (Model.iteratorClose it)
    NewReader                -> ok  Rdr                 $ update   Model.newReader
    ReaderInstruction rdr    -> err MbChainUpdate       $ updateE (Model.readerInstruction @Identity rdr allComponents)
    ReaderForward rdr pts    -> err MbPoint             $ updateE (Model.readerForward rdr pts)
    ReaderClose rdr          -> ok  Unit                $ update_ (Model.readerClose rdr)
    -- TODO can this execute while closed?
    RunBgTasks               -> ok  Unit                $ update_ (Model.garbageCollect k)
    Close                    -> openOrClosed            $ update_  Model.closeDB
    Reopen                   -> openOrClosed            $ update_  Model.reopen
  where
    k = configSecurityParam cfg

    advanceAndAdd slot blk m = (Model.tipPoint m', m')
      where
        m' = Model.addBlock cfg blk $ Model.advanceCurSlot cfg slot m

    iter = either UnknownRange Iter
    mbGCedAllComponents = MbGCedAllComponents . MaybeGCedBlock False
    iterResultGCed      = IterResultGCed . IteratorResultGCed False

    query   f m = (f m, m)

    update  f m = f m
    update_ f m = ((), f m)
    updateE f m = case f m of
      Left  e       -> (Left e, m)
      Right (a, m') -> (Right a, m')

    -- Only executed when the ChainDB is open, otherwise a 'ClosedDBError' is
    -- returned.
    ok toSuccess f = err toSuccess (first Right . f)
    err toSuccess f m
      | Model.isOpen m
      = first (Resp . fmap toSuccess) (f m)
      | otherwise
      = (Resp (Left (ClosedDBError callStack)), m)

    -- Executed whether the ChainDB is open or closed.
    openOrClosed f = first (Resp . Right . Unit) . f

runIO :: HasHeader blk
      => ChainDB          IO blk
      -> ChainDB.Internal IO blk
      -> ResourceRegistry IO
      -> StrictTVar       IO SlotNo
      -> StrictTVar       IO Id
      ->     Cmd  blk (WithEq (Iterator IO blk (AllComponents blk))) (WithEq (Reader IO blk (AllComponents blk)))
      -> IO (Resp blk (WithEq (Iterator IO blk (AllComponents blk))) (WithEq (Reader IO blk (AllComponents blk))))
runIO db internal registry varCurSlot varNextId cmd =
    Resp <$> try (run db internal registry varCurSlot varNextId cmd)

{-------------------------------------------------------------------------------
  Collect arguments
-------------------------------------------------------------------------------}

-- | Collect all iterators created.
iters :: Bitraversable t => t it rdr -> [it]
iters = bifoldMap (:[]) (const [])

-- | Collect all readers created.
rdrs :: Bitraversable t => t it rdr -> [rdr]
rdrs = bifoldMap (const []) (:[])

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

-- | Concrete or symbolic references to a real iterator
type IterRef blk m r = Reference (Opaque (TestIterator m blk)) r

-- | Mapping between iterator references and mocked iterators
type KnownIters blk m r = RefEnv (Opaque (TestIterator m blk)) IteratorId r

-- | Concrete or symbolic references to a real reader
type ReaderRef blk m r = Reference (Opaque (TestReader m blk)) r

-- | Mapping between iterator references and mocked readers
type KnownReaders blk m r = RefEnv (Opaque (TestReader m blk)) ReaderId r

type DBModel blk = Model.Model blk

-- | Execution model
data Model blk m r = Model
  { dbModel      :: DBModel                blk
  , knownIters   :: KnownIters             blk m r
  , knownReaders :: KnownReaders           blk m r
  , modelConfig  :: Opaque (TopLevelConfig blk)
  } deriving (Generic)

deriving instance (TestConstraints blk, Show1 r) => Show (Model blk m r)

-- | Initial model
initModel :: TopLevelConfig blk
          -> ExtLedgerState blk
          -> Model blk m r
initModel cfg initLedger = Model
  { dbModel      = Model.empty initLedger
  , knownIters   = RE.empty
  , knownReaders = RE.empty
  , modelConfig  = QSM.Opaque cfg
  }

-- | Key property of the model is that we can go from real to mock responses
toMock :: (Bifunctor (t blk), Eq1 r)
       => Model blk m r -> At t blk m r -> t blk IteratorId ReaderId
toMock Model {..} (At t) = bimap (knownIters RE.!) (knownReaders RE.!) t

-- | Step the mock semantics
--
-- We cannot step the whole Model here (see 'event', below)
step :: (TestConstraints blk, Eq1 r)
     => Model  blk m r
     -> At Cmd blk m r
     -> (Resp  blk IteratorId ReaderId, DBModel blk)
step model@Model { dbModel, modelConfig } cmd =
    runPure (QSM.unOpaque modelConfig) (toMock model cmd) dbModel

{-------------------------------------------------------------------------------
  Wrapping in quickcheck-state-machine references
-------------------------------------------------------------------------------}

-- | Instantiate functor @t blk@ to
-- @t blk ('IterRef' blk m r) ('ReaderRef' blk m r)@.
--
-- Needed because we need to (partially) apply @'At' t blk rdr m@ to @r@.
newtype At t blk m r = At { unAt :: t blk (IterRef blk m r) (ReaderRef blk m r) }
  deriving (Generic)


deriving instance Show (t blk (IterRef blk m r) (ReaderRef blk m r))
               => Show (At t blk m r)

deriving instance (TestConstraints blk, Eq1 r) => Eq (At Resp blk m r)

instance Bifunctor (t blk) => Rank2.Functor (At t blk m) where
  fmap = \f (At x) -> At (bimap (app f) (app f) x)
    where
      app :: (r x -> r' x) -> QSM.Reference x r -> QSM.Reference x r'
      app f (QSM.Reference x) = QSM.Reference (f x)

instance Bifoldable (t blk) => Rank2.Foldable (At t blk m) where
  foldMap = \f (At x) -> bifoldMap (app f) (app f) x
    where
      app :: (r x -> n) -> QSM.Reference x r -> n
      app f (QSM.Reference x) = f x

instance Bitraversable (t blk) => Rank2.Traversable (At t blk m) where
  traverse = \f (At x) -> At <$> bitraverse (app f) (app f) x
    where
      app :: Functor f
          => (r x -> f (r' x)) -> QSM.Reference x r -> f (QSM.Reference x r')
      app f (QSM.Reference x) = QSM.Reference <$> f x

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

-- | An event records the model before and after a command along with the
-- command itself, and the response.
type ChainDBEvent blk m = Event (Model blk m) (At Cmd blk m) (At Resp blk m)

eventMockResp :: Eq1 r => ChainDBEvent blk m r -> Resp blk IteratorId ReaderId
eventMockResp Event{..} = toMock eventAfter eventResp

-- | Construct an event
lockstep :: (TestConstraints blk, Eq1 r, Show1 r)
         => Model        blk m r
         -> At Cmd       blk m r
         -> At Resp      blk m r
         -> ChainDBEvent blk m r
lockstep model@Model {..} cmd (At resp) = Event
    { eventBefore = model
    , eventCmd    = cmd
    , eventAfter  = model'
    , eventResp   = At resp
    }
  where
    (mockResp, dbModel') = step model cmd
    newIters   = RE.fromList $ zip (iters resp) (iters mockResp)
    newReaders = RE.fromList $ zip (rdrs  resp) (rdrs  mockResp)
    model' = case unAt cmd of
      -- When closing the database, all open iterators and readers are closed
      -- too, so forget them.
      Close -> model
        { dbModel      = dbModel'
        , knownIters   = RE.empty
        , knownReaders = RE.empty
        }
      _ -> model
        { dbModel      = dbModel'
        , knownIters   = knownIters `RE.union` newIters
        , knownReaders = knownReaders `RE.union` newReaders
        }


{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

type BlockGen blk m = Model blk m Symbolic -> Gen blk

-- | Generate a 'Cmd'
generator
  :: forall blk m. TestConstraints blk
  => BlockGen     blk m
  -> Model        blk m Symbolic
  -> Gen (At Cmd  blk m Symbolic)
generator genBlock m@Model {..} = At <$> frequency
    [ (30, genAddBlock)
    , (if empty then 1 else 10, return GetCurrentChain)
    , (if empty then 1 else 10, return GetCurrentLedger)
    , (if empty then 1 else 10, return GetTipBlock)
      -- To check that we're on the right chain
    , (if empty then 1 else 10, return GetTipPoint)
    , (10, genGetBlockComponent)
    , (if empty then 1 else 10, return GetMaxSlotNo)
    , (if empty then 1 else 10, genGetPastLedger)

    -- Iterators
    , (if empty then 1 else 10, uncurry Stream <$> genBounds)
    , (if null iterators then 0 else 20, genIteratorNext)
      -- Use a lower frequency for closing, so that the chance increases that
      -- we can stream multiple blocks from an iterator.
    , (if null iterators then 0 else 2, genIteratorClose)

    -- Readers
    , (10, return NewReader)
    , (if null readers then 0 else 10, genReaderInstruction)
    , (if null readers then 0 else 10, genReaderForward)
      -- Use a lower frequency for closing, so that the chance increases that
      -- we can read multiple blocks from a reader
    , (if null readers then 0 else 2, genReaderClose)

    , (if empty then 1 else 10, return Close)
    , (if empty then 1 else 10, return Reopen)

      -- Internal
    , (if empty then 1 else 10, return RunBgTasks)
    ]
    -- TODO adjust the frequencies after labelling
  where
    cfg :: TopLevelConfig blk
    cfg = unOpaque modelConfig

    secParam :: SecurityParam
    secParam = configSecurityParam cfg

    iterators :: [Reference (Opaque (TestIterator m blk)) Symbolic]
    iterators = RE.keys knownIters

    readers :: [Reference (Opaque (TestReader m blk)) Symbolic]
    readers = RE.keys knownReaders

    genRandomPoint :: Gen (RealPoint blk)
    genRandomPoint = blockRealPoint <$> genBlock m

    pointsInDB :: [RealPoint blk]
    pointsInDB = blockRealPoint <$> Map.elems (Model.blocks dbModel)

    empty :: Bool
    empty = null pointsInDB

    genRealPoint :: Gen (RealPoint blk)
    genRealPoint = frequency
      [ (2, genRandomPoint)
      , (if empty then 0 else 7, elements pointsInDB)
      ]

    genPoint :: Gen (Point blk)
    genPoint = frequency
      [ (1, return Block.genesisPoint)
      , (9, realPointToPoint <$> genRealPoint)
      ]

    genGetBlockComponent :: Gen (Cmd blk it rdr)
    genGetBlockComponent = do
      pt <- genRealPoint
      return $ if Model.garbageCollectablePoint secParam dbModel pt
        then GetGCedBlockComponent pt
        else GetBlockComponent     pt

    genGetPastLedger :: Gen (Cmd blk it rdr)
    genGetPastLedger = do
        GetPastLedger <$> elements (takeLast (maxRollbacks secParam) onChain)
      where
        -- Non-empty list of points on our chain
        onChain :: [Point blk]
        onChain = (Block.genesisPoint :)
                . map Block.blockPoint
                . Chain.toOldestFirst
                . Model.currentChain
                $ dbModel

    genAddBlock = do
      let curSlot = Model.currentSlot dbModel
      blk <- genBlock m
      if blockSlot blk > Model.currentSlot dbModel
        -- When the slot of the block is in the future, we can either advance
        -- the current time ('AddBlock') or choose to add a block from the
        -- future ('AddFutureBlock')
        then frequency
          [ (1, return $ AddBlock blk)
          , (1, AddFutureBlock blk <$> chooseSlot curSlot (blockSlot blk - 1))
          ]
        else return $ AddBlock blk

    genBounds :: Gen (StreamFrom blk, StreamTo blk)
    genBounds = frequency
      [ (1, genRandomBounds)
      , (if empty then 0 else 3, genExistingBounds)
      ]

    genRandomBounds :: Gen (StreamFrom blk, StreamTo blk)
    genRandomBounds = (,)
      <$> (do inEx <- genFromInEx
              case inEx of
                Left  inc -> inc <$> genRealPoint
                Right exc -> exc <$> genPoint)
      <*> (genToInEx <*> genRealPoint)

    genFromInEx :: Gen (Either (RealPoint blk -> StreamFrom blk)
                               (Point     blk -> StreamFrom blk))
    genFromInEx = elements [Left StreamFromInclusive, Right StreamFromExclusive]

    genFromInEx' :: Gen (RealPoint blk -> StreamFrom blk)
    genFromInEx' = either id (. realPointToPoint) <$> genFromInEx

    genToInEx :: Gen (RealPoint blk -> StreamTo blk)
    genToInEx = elements [StreamToInclusive, StreamToExclusive]

    -- Generate bounds that correspond to existing blocks in the DB. Make sure
    -- that the start bound is older than the end bound.
    -- NOTE: this does not mean that these bounds are on the same chain.
    genExistingBounds :: Gen (StreamFrom blk, StreamTo blk)
    genExistingBounds = do
      start <- elements pointsInDB
      end   <- elements pointsInDB `suchThat` ((>= realPointSlot start) .
                                               realPointSlot)
      (,) <$> (genFromInEx' <*> return start)
          <*> (genToInEx    <*> return end)

    genIteratorClose = IteratorClose <$> elements iterators
    genIteratorNext  = do
      it <- elements iterators
      let blockCanBeGCed = Model.garbageCollectableIteratorNext
            secParam dbModel (knownIters RE.! it)
      return $ if blockCanBeGCed
        then IteratorNextGCed it
        else IteratorNext     it

    genReaderInstruction = ReaderInstruction <$> elements readers
    genReaderForward     = ReaderForward     <$> elements readers
                                             <*> genReaderForwardPoints

    genReaderForwardPoints :: Gen [Point blk]
    genReaderForwardPoints = choose (1, 3) >>= \n ->
      sortOn (Down . Block.pointSlot) <$> replicateM n genReaderForwardPoint

    genReaderForwardPoint :: Gen (Point blk)
    genReaderForwardPoint = genPoint

    genReaderClose = ReaderClose <$> elements readers

chooseSlot :: SlotNo -> SlotNo -> Gen SlotNo
chooseSlot (SlotNo start) (SlotNo end) = SlotNo <$> choose (start, end)

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Shrinker
shrinker :: Model   blk m Symbolic
         ->  At Cmd blk m Symbolic
         -> [At Cmd blk m Symbolic]
shrinker _ = const [] -- TODO

{-------------------------------------------------------------------------------
  The final state machine
-------------------------------------------------------------------------------}

-- | Mock a response
--
-- We do this by running the pure semantics and then generating mock
-- references for any new handles.
mock :: (TestConstraints blk, Typeable m)
     => Model            blk m Symbolic
     ->         At Cmd   blk m Symbolic
     -> GenSym (At Resp  blk m Symbolic)
mock model cmd = At <$> bitraverse (const genSym) (const genSym) resp
  where
    (resp, _dbm) = step model cmd

precondition :: forall m blk. TestConstraints blk
             => Model blk m Symbolic -> At Cmd blk m Symbolic -> Logic
precondition Model {..} (At cmd) =
   forall (iters cmd) (`member` RE.keys knownIters)   .&&
   forall (rdrs  cmd) (`member` RE.keys knownReaders) .&&
   case cmd of
     -- Even though we ensure this in the generator, shrinking might change
     -- it.
     GetBlockComponent     pt -> Not $ garbageCollectable pt
     GetGCedBlockComponent pt -> garbageCollectable pt
     IteratorNext     it      -> Not $ garbageCollectableIteratorNext it
     IteratorNextGCed it      -> garbageCollectableIteratorNext it

     -- TODO The real implementation allows streaming blocks from the
     -- VolatileDB that have no path to the current chain. The model
     -- implementation disallows this, as it only allows streaming from one of
     -- the possible forks, each starting at genesis. Temporarily only test
     -- with iterators that the model allows. So we only test a subset of the
     -- functionality, which does not include error paths.
     Stream from to           -> isValidIterator from to
     -- Make sure we don't close (and reopen) when there are other chain
     -- equally preferable or even more preferable (which we can't switch to
     -- because they fork back more than @k@) than the current chain in the
     -- ChainDB. We might pick another one than the current one when
     -- reopening, which would bring us out of sync with the model, for which
     -- reopening is a no-op (no chain selection). See #1533.
     Close                    -> Not equallyOrMorePreferableFork
     -- To be in the future, @blockSlot blk@ must be greater than @slot@.
     AddFutureBlock blk s     -> s .>= Model.currentSlot dbModel .&&
                                 blockSlot blk .> s
     _                        -> Top
  where
    garbageCollectable :: RealPoint blk -> Logic
    garbageCollectable =
      Boolean . Model.garbageCollectablePoint secParam dbModel

    garbageCollectableIteratorNext :: IterRef blk m Symbolic -> Logic
    garbageCollectableIteratorNext it = Boolean $
      Model.garbageCollectableIteratorNext secParam dbModel (knownIters RE.! it)

    curChain :: Chain blk
    curChain = Model.currentChain dbModel

    forks :: [Chain blk]
    (_, forks) = map fst <$>
      Model.validChains cfg (Model.initLedger dbModel) (Model.blocks dbModel)

    equallyOrMorePreferableFork :: Logic
    equallyOrMorePreferableFork = exists forks $ \fork ->
      Boolean (equallyOrMorePreferable cfg curChain fork) .&&
      Chain.head curChain ./= Chain.head fork

    cfg :: TopLevelConfig blk
    cfg = unOpaque modelConfig

    secParam :: SecurityParam
    secParam = configSecurityParam cfg

    isValidIterator :: StreamFrom blk -> StreamTo blk -> Logic
    isValidIterator from to =
      case Model.between secParam from to dbModel of
        Left  _    -> Bot
        -- All blocks must be valid
        Right blks -> forall blks $ \blk -> Boolean $
          Map.notMember (blockHash blk) $
          forgetFingerprint (Model.invalid dbModel)

equallyOrMorePreferable :: forall blk. BlockSupportsProtocol blk
                        => TopLevelConfig blk
                        -> Chain blk -> Chain blk -> Bool
equallyOrMorePreferable cfg chain1 chain2 =
    not (preferAnchoredCandidate cfg chain1' chain2')
  where
    chain1', chain2' :: AnchoredFragment (Header blk)
    chain1' = Chain.toAnchoredFragment (getHeader <$> chain1)
    chain2' = Chain.toAnchoredFragment (getHeader <$> chain2)

equallyPreferable :: forall blk. BlockSupportsProtocol blk
                  => TopLevelConfig blk
                  -> Chain blk -> Chain blk -> Bool
equallyPreferable cfg chain1 chain2 =
    not (preferAnchoredCandidate cfg chain1' chain2') &&
    not (preferAnchoredCandidate cfg chain2' chain1')
  where
    chain1', chain2' :: AnchoredFragment (Header blk)
    chain1' = Chain.toAnchoredFragment (getHeader <$> chain1)
    chain2' = Chain.toAnchoredFragment (getHeader <$> chain2)


transition :: (TestConstraints blk, Show1 r, Eq1 r)
           => Model   blk m r
           -> At Cmd  blk m r
           -> At Resp blk m r
           -> Model   blk m r
transition model cmd = eventAfter . lockstep model cmd

postcondition :: TestConstraints blk
              => Model   blk m Concrete
              -> At Cmd  blk m Concrete
              -> At Resp blk m Concrete
              -> Logic
postcondition model cmd resp =
    (toMock (eventAfter ev) resp .== eventMockResp ev)
    .// "real response didn't match model response"
  where
    ev = lockstep model cmd resp

semantics :: forall blk. TestConstraints blk
          => ChainDB IO blk
          -> ChainDB.Internal IO blk
          -> ResourceRegistry IO
          -> StrictTVar       IO SlotNo
          -> StrictTVar       IO Id
          -> At Cmd blk IO Concrete
          -> IO (At Resp blk IO Concrete)
semantics db internal registry varCurSlot varNextId (At cmd) =
    At . (bimap (QSM.reference . QSM.Opaque) (QSM.reference . QSM.Opaque)) <$>
    runIO db internal registry varCurSlot varNextId (bimap QSM.opaque QSM.opaque cmd)

-- | The state machine proper
sm :: TestConstraints blk
   => ChainDB          IO       blk
   -> ChainDB.Internal IO       blk
   -> ResourceRegistry IO
   -> StrictTVar       IO SlotNo
   -> StrictTVar       IO Id
   -> BlockGen              blk IO
   -> TopLevelConfig        blk
   -> ExtLedgerState        blk
   -> StateMachine (Model   blk IO)
                   (At Cmd  blk IO)
                                IO
                   (At Resp blk IO)
sm db internal registry varCurSlot varNextId genBlock env initLedger = StateMachine
  { initModel     = initModel env initLedger
  , transition    = transition
  , precondition  = precondition
  , postcondition = postcondition
  , generator     = Just . generator genBlock
  , shrinker      = shrinker
  , semantics     = semantics db internal registry varCurSlot varNextId
  , mock          = mock
  , invariant     = Nothing
  , cleanup       = noCleanup
  }

{-------------------------------------------------------------------------------
  Bitraversable instances
-------------------------------------------------------------------------------}

TH.deriveBifunctor     ''Cmd
TH.deriveBifoldable    ''Cmd
TH.deriveBitraversable ''Cmd

TH.deriveBifunctor     ''Success
TH.deriveBifoldable    ''Success
TH.deriveBitraversable ''Success

TH.deriveBifunctor     ''Resp
TH.deriveBifoldable    ''Resp
TH.deriveBitraversable ''Resp

{-------------------------------------------------------------------------------
  Required instances

  The 'ToExpr' constraints come from "Data.TreeDiff".
-------------------------------------------------------------------------------}

instance CommandNames (At Cmd blk m) where
  cmdName (At cmd) = constrName cmd
  cmdNames (_ :: Proxy (At Cmd blk m r)) =
    constrNames (Proxy @(Cmd blk () ()))

deriving instance Generic ReaderNext
deriving instance Generic IteratorId
deriving instance Generic (Point blk)
deriving instance Generic (Chain blk)
deriving instance Generic (ChainProducerState blk)
deriving instance Generic (ReaderState blk)

deriving instance ( ToExpr (HeaderState blk)
                  , ToExpr (LedgerState blk)
                  )
                 => ToExpr (ExtLedgerState blk)
deriving instance ToExpr Fingerprint
deriving instance ToExpr BlockNo
deriving instance ToExpr SlotNo
deriving instance ToExpr ReaderNext
deriving instance ToExpr MaxSlotNo
deriving instance ToExpr blk  => ToExpr (Point.WithOrigin blk)
deriving instance ToExpr hash => ToExpr (Point.Block SlotNo hash)
deriving instance ToExpr (HeaderHash blk) => ToExpr (ChainHash blk)
deriving instance ToExpr (HeaderHash blk) => ToExpr (Point blk)
deriving instance ToExpr (HeaderHash blk) => ToExpr (RealPoint blk)
deriving instance ToExpr (HeaderHash blk) => ToExpr (ReaderState blk)
deriving instance ToExpr blk => ToExpr (Chain blk)
deriving instance ( ToExpr blk
                  , ToExpr (HeaderHash blk)
                  )
                 => ToExpr (ChainProducerState blk)
deriving instance ToExpr a => ToExpr (WithFingerprint a)
deriving instance ( ToExpr (HeaderHash blk)
                  , ToExpr (ExtValidationError blk)
                  )
                 => ToExpr (InvalidBlockReason blk)
deriving instance ( ToExpr blk
                  , ToExpr (HeaderHash  blk)
                  , ToExpr (HeaderState blk)
                  , ToExpr (LedgerState blk)
                  , ToExpr (ExtValidationError blk)
                  )
                 => ToExpr (DBModel blk)
deriving instance ( ToExpr blk
                  , ToExpr (HeaderHash  blk)
                  , ToExpr (HeaderState blk)
                  , ToExpr (LedgerState blk)
                  , ToExpr (ExtValidationError blk)
                  )
                 => ToExpr (Model blk IO Concrete)

-- Blk specific instances

deriving instance ToExpr IsEBB
deriving instance ToExpr TestHeader
deriving instance ToExpr TestHeaderHash
deriving instance ToExpr TestBody
deriving instance ToExpr TestBodyHash
deriving instance ToExpr TestBlockError
deriving instance ToExpr Blk
deriving instance ToExpr (AnnTip Blk)
deriving instance ToExpr (LedgerState Blk)
deriving instance ToExpr (HeaderState Blk)
deriving instance ToExpr (HeaderError Blk)
deriving instance ToExpr (HeaderEnvelopeError Blk)
deriving instance ToExpr BftValidationErr
deriving instance ToExpr (ExtValidationError Blk)

instance ToExpr a => ToExpr (StrictSeq a) where
  toExpr = toExpr . toList

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

deriving instance SOP.Generic         (TraceEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceEvent blk)
deriving instance SOP.Generic         (TraceAddBlockEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceAddBlockEvent blk)
deriving instance SOP.Generic         (ChainDB.TraceReaderEvent blk)
deriving instance SOP.HasDatatypeInfo (ChainDB.TraceReaderEvent blk)
deriving instance SOP.Generic         (TraceCopyToImmDBEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceCopyToImmDBEvent blk)
deriving instance SOP.Generic         (TraceValidationEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceValidationEvent blk)
deriving instance SOP.Generic         (TraceInitChainSelEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceInitChainSelEvent blk)
deriving instance SOP.Generic         (TraceOpenEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceOpenEvent blk)
deriving instance SOP.Generic         (TraceGCEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceGCEvent blk)
deriving instance SOP.Generic         (TraceIteratorEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceIteratorEvent blk)
deriving instance SOP.Generic         (LedgerDB.TraceEvent r)
deriving instance SOP.HasDatatypeInfo (LedgerDB.TraceEvent r)
deriving instance SOP.Generic         (LedgerDB.TraceReplayEvent r replayTo)
deriving instance SOP.HasDatatypeInfo (LedgerDB.TraceReplayEvent r replayTo)
deriving instance SOP.Generic         (ImmDB.TraceEvent e hash)
deriving instance SOP.HasDatatypeInfo (ImmDB.TraceEvent e hash)
deriving instance SOP.Generic         (VolDB.TraceEvent e hash)
deriving instance SOP.HasDatatypeInfo (VolDB.TraceEvent e hash)

-- TODO labelling

{-------------------------------------------------------------------------------
  Generator for TestBlock
-------------------------------------------------------------------------------}

type Blk = TestBlock

instance ModelSupportsBlock TestBlock where
  isEBB = thIsEBB . unTestHeader

-- | Note that the 'Blk = TestBlock' is general enough to be used by both the
-- ChainDB /and/ the ImmutableDB, its generators cannot. For example, in the
-- ChainDB, blocks are added /out of order/, while in the ImmutableDB, they
-- must be added /in order/. This generator can thus not be reused for the
-- ImmutableDB.
genBlk :: ImmDB.ChunkInfo -> BlockGen Blk m
genBlk chunkInfo Model{..} = frequency
    [ (if empty then 0 else 1, genAlreadyInChain)
    , (5,                      genAppendToCurrentChain)
    , (5,                      genFitsOnSomewhere)
    , (3,                      genGap)
    ]
  where
    blocksInChainDB   = Model.blocks dbModel
    modelSupportsEBBs = ImmDB.chunkInfoSupportsEBBs chunkInfo
    canContainEBB     = const modelSupportsEBBs -- TODO: we could be more precise

    empty :: Bool
    empty = Map.null blocksInChainDB

    genBody :: Gen TestBody
    genBody = do
      isValid <- frequency
        [ (4, return True)
        , (1, return False)
        ]
      forkNo <- choose (1, 3)
      return TestBody
        { tbForkNo  = forkNo
        , tbIsValid = isValid
        }

    -- A block that already exists in the ChainDB
    genAlreadyInChain :: Gen TestBlock
    genAlreadyInChain = elements $ Map.elems blocksInChainDB

    -- A block that fits onto the current chain
    genAppendToCurrentChain :: Gen TestBlock
    genAppendToCurrentChain = case Model.tipBlock dbModel of
      Nothing -> genFirstBlock
      Just b  -> genFitsOn b

    -- A block that fits onto some block @b@ in the ChainDB. The block @b@
    -- could be at the tip of the chain and the generated block might already
    -- be present in the ChainDB.
    genFitsOnSomewhere :: Gen TestBlock
    genFitsOnSomewhere = case Model.tipBlock dbModel of
      Nothing -> genFirstBlock
      Just _  -> genAlreadyInChain >>= genFitsOn

    -- A block that doesn't fit onto a block in the ChainDB, but it creates a
    -- gap of a couple of blocks between genesis or an existing block in the
    -- ChainDB. We generate it by generating a few intermediary blocks first,
    -- which we don't add. But the chance exists that we will generate them
    -- again later on.
    genGap :: Gen TestBlock
    genGap = do
        gapSize <- choose (1, 3)
        start   <- genFitsOnSomewhere
        go gapSize start
      where
        go :: Int -> TestBlock -> Gen TestBlock
        go 0 b = return b
        go n b = genFitsOn b >>= go (n - 1)

    -- Generate a block or EBB fitting on genesis
    genFirstBlock :: Gen TestBlock
    genFirstBlock = frequency
      [ ( 1
        , firstBlock canContainEBB <$> chooseSlot 0 2 <*> genBody
        )
      , ( if modelSupportsEBBs then 1 else 0
        , firstEBB   canContainEBB <$> genBody
        )
      ]

    -- Helper that generates a block that fits onto the given block.
    genFitsOn :: TestBlock -> Gen TestBlock
    genFitsOn b = frequency
        [ (4, do
                slotNo <- if fromIsEBB (testBlockIsEBB b)
                  then chooseSlot (blockSlot b)     (blockSlot b + 2)
                  else chooseSlot (blockSlot b + 1) (blockSlot b + 3)
                body   <- genBody
                return $ mkNextBlock canContainEBB b slotNo body)
        -- An EBB is never followed directly by another EBB, otherwise they
        -- would have the same 'BlockNo', as the EBB has the same 'BlockNo' of
        -- the block before it.
        , (if fromIsEBB (testBlockIsEBB b) || not modelSupportsEBBs then 0 else 1, do
             let prevSlotNo    = blockSlot b
                 prevChunk     = ImmDB.chunkIndexOfSlot
                                   chunkInfo
                                   prevSlotNo
                 prevEpoch     = unsafeChunkNoToEpochNo prevChunk
                 nextEBB       = ImmDB.chunkSlotForBoundaryBlock
                                   chunkInfo
                                   (prevEpoch + 1)
                 nextNextEBB   = ImmDB.chunkSlotForBoundaryBlock
                                   chunkInfo
                                   (prevEpoch + 2)
             slotNo <- (ImmDB.chunkSlotToSlot chunkInfo) <$> frequency
               [ (7, return nextEBB)
               , (1, return nextNextEBB)
               ]
             body   <- genBody
             return $ mkNextEBB canContainEBB b slotNo body
          )
        ]

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

testCfg :: TopLevelConfig TestBlock
testCfg = TopLevelConfig {
      configConsensus = BftConfig
        { bftParams   = BftParams { bftSecurityParam = k
                                  , bftNumNodes      = numCoreNodes
                                  }
        , bftNodeId   = CoreId (CoreNodeId 0)
        , bftSignKey  = SignKeyMockDSIGN 0
        , bftVerKeys  = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
        }
    , configLedger = LedgerConfig
    , configBlock  = TestBlockConfig slotLengths numCoreNodes
    }
  where
    slotLengths :: SlotLengths
    slotLengths = singletonSlotLengths $ slotLengthFromSec 20

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes 1

    k = SecurityParam 2

dbUnused :: ChainDB blk m
dbUnused = error "ChainDB used during command generation"

internalUnused :: ChainDB.Internal blk m
internalUnused = error "ChainDB.Internal used during command generation"

registryUnunused :: ResourceRegistry m
registryUnunused = error "ResourceRegistry used during command generation"

varCurSlotUnused :: StrictTVar m SlotNo
varCurSlotUnused = error "StrictTVar m SlotNo used during command generation"

varNextIdUnused :: StrictTVar m Id
varNextIdUnused = error "StrictTVar m Id used during command generation"

smUnused :: ImmDB.ChunkInfo
         -> StateMachine (Model Blk IO) (At Cmd Blk IO) IO (At Resp Blk IO)
smUnused chunkInfo =
    sm dbUnused
       internalUnused
       registryUnunused
       varCurSlotUnused
       varNextIdUnused
       (genBlk chunkInfo)
       testCfg
       testInitExtLedger

prop_sequential :: SmallChunkInfo -> Property
prop_sequential (SmallChunkInfo chunkInfo) =
    forAllCommands (smUnused chunkInfo) Nothing $ \cmds -> QC.monadicIO $ do
      (hist, prop) <- QC.run $ test cmds
      -- TODO label tags
      prettyCommands (smUnused chunkInfo) hist prop
  where
    test :: QSM.Commands (At Cmd Blk IO) (At Resp Blk IO)
         -> IO
            ( QSM.History (At Cmd Blk IO) (At Resp Blk IO)
            , Property
            )
    test cmds = do
      threadRegistry     <- unsafeNewRegistry
      iteratorRegistry   <- unsafeNewRegistry
      (tracer, getTrace) <- recordingTracerIORef
      varCurSlot         <- uncheckedNewTVarM 0
      varNextId          <- uncheckedNewTVarM 0
      fsVars             <- (,,)
        <$> uncheckedNewTVarM Mock.empty
        <*> uncheckedNewTVarM Mock.empty
        <*> uncheckedNewTVarM Mock.empty
      let args = mkArgs testCfg chunkInfo testInitExtLedger tracer threadRegistry
            varCurSlot fsVars
      (db, internal)     <- openDBInternal args False
      withAsync (intAddBlockRunner internal) $ \addBlockAsync -> do
        link addBlockAsync
        let sm' = sm db
                     internal
                     iteratorRegistry
                     varCurSlot
                     varNextId
                     (genBlk chunkInfo)
                     testCfg
                     testInitExtLedger
        (hist, model, res) <- QSM.runCommands' sm' cmds
        cancel addBlockAsync
        trace <- getTrace
        open <- atomically $ isOpen db
        unless open $ intReopen internal False
        -- Copy blocks from our chain to the ImmutableDB, otherwise we might
        -- pick a longer chain that forked off more than @k@ blocks back when
        -- restarting
        runBgTasks internal

        realChain <- toChain db
        closeDB db
        -- We already generate a command to test 'reopenDB', but since that
        -- code path differs slightly from 'openDB', we test that code path
        -- here too by simply opening the DB from scratch again and comparing
        -- the resulting chain.
        (db', _) <- openDBInternal args False
        realChain' <- toChain db'
        closeDB db'

        closeRegistry threadRegistry

        -- 'closeDB' should have closed all open 'Reader's and 'Iterator's,
        -- freeing up all resources, so there should be no more clean-up
        -- actions left.
        --
        -- Note that this is only true because we're not simulating exceptions
        -- (yet), in which case there /will be/ clean-up actions left. This is
        -- exactly the reason for introducing the 'ResourceRegistry' in the
        -- first place: to clean up resources in case exceptions get thrown.
        remainingCleanups <- countResources iteratorRegistry
        closeRegistry iteratorRegistry

        -- Read the final MockFS of each database
        let (immDbFsVar, volDbFsVar, lgrDbFsVar) = fsVars
        fses <- atomically $ (,,)
          <$> readTVar immDbFsVar
          <*> readTVar volDbFsVar
          <*> readTVar lgrDbFsVar

        let modelChain = Model.currentChain $ dbModel model
            (immDbFs, volDbFs, lgrDbFs) = fses
            prop =
              counterexample ("Real  chain: " <> condense realChain)       $
              counterexample ("Model chain: " <> condense modelChain)      $
              counterexample ("TraceEvents: " <> unlines (map show trace)) $
              tabulate "Chain length" [show (Chain.length realChain)]      $
              tabulate "TraceEvents" (map traceEventName trace)            $
              res === Ok .&&.
              counterexample
                ("Real chain and model chain differ")
                (realChain =:= modelChain) .&&.
              prop_trace trace .&&.
              -- Another equally preferable fork may be selected when opening
              -- the DB.
              counterexample
               ("Real chain after reopening: " <> show realChain' <> "\n" <>
                "Chain after reopening not equally preferable to previous chain")
               (equallyPreferable (cdbTopLevelConfig args) realChain realChain') .&&.
              counterexample "ImmutableDB is leaking file handles"
                             (Mock.numOpenHandles immDbFs === 0) .&&.
              counterexample "VolatileDB is leaking file handles"
                             (Mock.numOpenHandles volDbFs === 0) .&&.
              counterexample "LedgerDB is leaking file handles"
                             (Mock.numOpenHandles lgrDbFs === 0) .&&.
              counterexample "There were registered clean-up actions"
                             (remainingCleanups === 0)
        return (hist, prop)

prop_trace :: [TraceEvent Blk] -> Property
prop_trace trace = invalidBlockNeverValidatedAgain
  where
    -- Whenever we validate a block that turns out to be invalid, check that
    -- we never again validate the same block.
    invalidBlockNeverValidatedAgain =
      whenOccurs trace  invalidBlock $ \trace' invalidPoint  ->
      whenOccurs trace' invalidBlock $ \_      invalidPoint' ->
        counterexample "An invalid block is validated twice" $
        invalidPoint =/= invalidPoint'

    invalidBlock :: TraceEvent blk -> Maybe (RealPoint blk)
    invalidBlock = \case
        TraceAddBlockEvent (AddBlockValidation ev)         -> extract ev
        TraceInitChainSelEvent (InitChainSelValidation ev) -> extract ev
        _                                                  -> Nothing
      where
        extract (ChainDB.InvalidBlock _ pt) = Just pt
        extract _                           = Nothing

-- | Given a trace of events, for each event in the trace for which the
-- predicate yields a @Just a@, call the continuation function with the
-- remaining events and @a@.
whenOccurs :: [ev] -> (ev -> Maybe a) -> ([ev] -> a -> Property) -> Property
whenOccurs evs occurs k = go evs
  where
    go [] = property True
    go (ev:evs')
      | Just a <- occurs ev
      = k evs' a .&&. go evs'
      | otherwise
      = go evs'

traceEventName :: TraceEvent blk -> String
traceEventName = \case
    TraceAddBlockEvent       ev    -> "AddBlock."     <> case ev of
      AddBlockValidation     ev' -> constrName ev'
      _                          -> constrName ev
    TraceReaderEvent         ev    -> "Reader."       <> constrName ev
    TraceCopyToImmDBEvent    ev    -> "CopyToImmDB."  <> constrName ev
    TraceInitChainSelEvent   ev    -> "InitChainSel." <> case ev of
      InitChainSelValidation ev'   -> constrName ev'
    TraceOpenEvent           ev    -> "Open."         <> constrName ev
    TraceGCEvent             ev    -> "GC."           <> constrName ev
    TraceIteratorEvent       ev    -> "Iterator."     <> constrName ev
    TraceLedgerEvent         ev    -> "Ledger."       <> constrName ev
    TraceLedgerReplayEvent   ev    -> "LedgerReplay." <> constrName ev
    TraceImmDBEvent          ev    -> "ImmDB."        <> constrName ev
    TraceVolDBEvent          ev    -> "VolDB."        <> constrName ev

mkArgs :: IOLike m
       => TopLevelConfig Blk
       -> ImmDB.ChunkInfo
       -> ExtLedgerState Blk
       -> Tracer m (TraceEvent Blk)
       -> ResourceRegistry m
       -> StrictTVar m SlotNo
       -> (StrictTVar m MockFS, StrictTVar m MockFS, StrictTVar m MockFS)
          -- ^ ImmutableDB, VolatileDB, LedgerDB
       -> ChainDbArgs m Blk
mkArgs cfg chunkInfo initLedger tracer registry varCurSlot
       (immDbFsVar, volDbFsVar, lgrDbFsVar) = ChainDbArgs
    { -- Decoders
      cdbDecodeHash           = decode
    , cdbDecodeBlock          = const <$> decode
    , cdbDecodeHeader         = const <$> decode
    , cdbDecodeLedger         = decode
    , cdbDecodeConsensusState = decode
    , cdbDecodeTipInfo        = decode

      -- Encoders
    , cdbEncodeHash           = encode
    , cdbEncodeBlock          = testBlockToBinaryInfo
    , cdbEncodeHeader         = encode
    , cdbEncodeLedger         = encode
    , cdbEncodeConsensusState = encode
    , cdbEncodeTipInfo        = encode

      -- HasFS instances
    , cdbHasFSImmDb           = simHasFS immDbFsVar
    , cdbHasFSVolDb           = simHasFS volDbFsVar
    , cdbHasFSLgrDB           = simHasFS lgrDbFsVar

      -- Policy
    , cdbImmValidation        = ValidateAllChunks
    , cdbVolValidation        = VolDB.ValidateAll
    , cdbBlocksPerFile        = VolDB.mkBlocksPerFile 4
    , cdbParamsLgrDB          = LedgerDbParams {
                                    -- Pick a small value for 'ledgerDbSnapEvery',
                                    -- so that maximum supported rollback is limited
                                    ledgerDbSnapEvery     = 2
                                  , ledgerDbSecurityParam = configSecurityParam cfg
                                  }
    , cdbDiskPolicy           = defaultDiskPolicy (configSecurityParam cfg)

      -- Integration
    , cdbTopLevelConfig       = cfg
    , cdbChunkInfo            = chunkInfo
    , cdbHashInfo             = testHashInfo
    , cdbIsEBB                = testHeaderEpochNoIfEBB chunkInfo
    , cdbCheckIntegrity       = testBlockIsValid
    , cdbGenesis              = return initLedger
    , cdbBlockchainTime       = settableBlockchainTime varCurSlot
    , cdbAddHdrEnv            = \_ _ -> id
    , cdbImmDbCacheConfig     = Index.CacheConfig 2 60

    -- Misc
    , cdbTracer               = tracer
    , cdbTraceLedger          = nullTracer
    , cdbRegistry             = registry
    , cdbGcDelay              = 0
    , cdbBlocksToAddSize      = 2
    }

tests :: TestTree
tests = testGroup "ChainDB q-s-m"
    [ testProperty "sequential" prop_sequential
    ]

{-------------------------------------------------------------------------------
  Running commands directly
-------------------------------------------------------------------------------}

-- | Debugging utility: run some commands against the real implementation.
_runCmds :: ImmDB.ChunkInfo
         ->    [Cmd  Blk IteratorId ReaderId]
         -> IO [Resp Blk IteratorId ReaderId]
_runCmds chunkInfo cmds = withRegistry $ \registry -> do
    varCurSlot <- uncheckedNewTVarM 0
    varNextId  <- uncheckedNewTVarM 0
    fsVars <- (,,)
      <$> uncheckedNewTVarM Mock.empty
      <*> uncheckedNewTVarM Mock.empty
      <*> uncheckedNewTVarM Mock.empty
    let args = mkArgs
          testCfg
          chunkInfo
          testInitExtLedger
          (showTracing stdoutTracer)
          registry
          varCurSlot
          fsVars
    (db, internal) <- openDBInternal args False
    evalStateT (mapM (go db internal registry varCurSlot varNextId) cmds) emptyRunCmdState
  where
    go :: ChainDB IO Blk -> ChainDB.Internal IO Blk
       -> ResourceRegistry IO
       -> StrictTVar IO SlotNo
       -> StrictTVar IO Id
       -> Cmd Blk IteratorId ReaderId
       -> StateT RunCmdState
                 IO
                 (Resp Blk IteratorId ReaderId)
    go db internal resourceRegistry varCurSlot varNextId cmd = do
      RunCmdState { rcsKnownIters, rcsKnownReaders} <- get
      let cmd' = At $
            bimap (revLookup rcsKnownIters) (revLookup rcsKnownReaders) cmd
      resp <- lift $ unAt <$> semantics db internal resourceRegistry varCurSlot varNextId cmd'
      newIters <- RE.fromList <$>
       mapM (\rdr -> (rdr, ) <$> newIteratorId) (iters resp)
      newReaders <- RE.fromList <$>
        mapM (\rdr -> (rdr, ) <$> newReaderId) (rdrs  resp)
      let knownIters'   = rcsKnownIters   `RE.union` newIters
          knownReaders' = rcsKnownReaders `RE.union` newReaders
          resp'      = bimap (knownIters' RE.!) (knownReaders' RE.!) resp
      modify $ \s ->
        s { rcsKnownIters = knownIters', rcsKnownReaders = knownReaders' }
      return resp'

    revLookup :: Eq a => RefEnv k a r -> a -> Reference k r
    revLookup env a = head $ RE.reverseLookup (== a) env

    newIteratorId :: forall m. Monad m => StateT RunCmdState m IteratorId
    newIteratorId = do
      s@RunCmdState { rcsNextIteratorId } <- get
      put (s { rcsNextIteratorId = succ rcsNextIteratorId })
      return rcsNextIteratorId

    newReaderId :: forall m. Monad m => StateT RunCmdState m ReaderId
    newReaderId = do
      s@RunCmdState { rcsNextReaderId } <- get
      put (s { rcsNextReaderId = succ rcsNextReaderId })
      return rcsNextReaderId

data RunCmdState = RunCmdState
  { rcsKnownIters     :: KnownIters   Blk IO Concrete
  , rcsKnownReaders   :: KnownReaders Blk IO Concrete
  , rcsNextIteratorId :: IteratorId
  , rcsNextReaderId   :: ReaderId
  }

emptyRunCmdState :: RunCmdState
emptyRunCmdState = RunCmdState
  { rcsKnownIters     = RE.empty
  , rcsKnownReaders   = RE.empty
  , rcsNextIteratorId = 0
  , rcsNextReaderId   = 0
  }
