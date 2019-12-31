{-# LANGUAGE ConstraintKinds       #-}
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

{-# OPTIONS_GHC -Wno-orphans -Wredundant-constraints #-}
module Test.Ouroboros.Storage.ChainDB.StateMachine ( tests ) where

import           Prelude hiding (elem)

import           Codec.Serialise (decode, encode)
import           Control.Monad (forM_, replicateM, unless, when)
import           Control.Monad.State (StateT, evalStateT, get, lift, modify,
                     put)
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Bifunctor.TH as TH
import           Data.Bitraversable
import           Data.Functor.Classes (Eq1, Show1)
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Ord (Down (..))
import           Data.Proxy
import           Data.TreeDiff (ToExpr)
import           Data.Typeable
import           GHC.Generics (Generic)

import qualified Generics.SOP as SOP

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.StateMachine
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import           Cardano.Crypto.DSIGN.Mock

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

import           Ouroboros.Consensus.Block (IsEBB (..), fromIsEBB, getHeader)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util.AnchoredFragment
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike hiding (fork)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))

import           Ouroboros.Storage.ChainDB
import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common (EpochSize (..))
import           Ouroboros.Storage.EpochInfo (fixedSizeEpochInfo)
import           Ouroboros.Storage.ImmutableDB
                     (ValidationPolicy (ValidateAllEpochs))
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import           Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import           Ouroboros.Storage.LedgerDB.InMemory (ledgerDbDefaultParams)
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import           Test.Ouroboros.Storage.TestBlock
import           Test.Ouroboros.Storage.Util ((=:=))

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE
import           Test.Util.SOP
import           Test.Util.Tracer (recordingTracerIORef)

{-------------------------------------------------------------------------------
  Abstract model
-------------------------------------------------------------------------------}

-- | Commands
data Cmd blk it rdr
  = AddBlock          blk
  | AddFutureBlock    blk SlotNo -- ^ The current slot number
  | GetCurrentChain
  | GetCurrentLedger
  | GetTipBlock
  | GetTipHeader
  | GetTipPoint
  | GetTipBlockNo
  | GetBlock          (Point blk)
  | GetGCedBlock      (Point blk)
    -- ^ Only for blocks that may have been garbage collected.
  | GetMaxSlotNo
  | StreamBlocks      (StreamFrom blk) (StreamTo blk)
  | IteratorNext      it
  | IteratorNextGCed  it
    -- ^ Only for blocks that may have been garbage collected.
  | IteratorClose     it
  | NewBlockReader
    -- ^ Subsumes 'newHeaderReader' as it will include the same code path.
  | ReaderInstruction rdr
    -- ^ 'readerInstructionBlocking' is excluded, as it requires multiple
    -- threads. Its code path is pretty much the same as 'readerInstruction'
    -- anyway.
  | ReaderForward     rdr [Point blk]
  | ReaderClose       rdr
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
  = Unit           ()
  | Chain          (AnchoredFragment (Header blk))
  | Ledger         (ExtLedgerState blk)
  | MbBlock        (Maybe blk)
  | MbGCedBlock    (MaybeGCedBlock blk)
  | MbHeader       (Maybe (Header blk))
  | Point          (Point blk)
  | BlockNo        BlockNo
  | UnknownRange   (UnknownRange blk)
  | Iter           it
  | IterResult     (IteratorResult blk)
  | IterResultGCed (IteratorResultGCed blk)
  | BlockReader    rdr
  | MbChainUpdate  (Maybe (ChainUpdate blk blk))
  | MbPoint        (Maybe (Point blk))
  | MaxSlot        MaxSlotNo
  deriving (Functor, Foldable, Traversable)

type TestConstraints blk =
  ( OuroborosTag   (BlockProtocol blk)
  , ProtocolLedgerView            blk
  , CanSelect (BlockProtocol blk) blk
  , Eq (ChainState (BlockProtocol blk))
  , Eq (LedgerState               blk)
  , Eq                            blk
  , Show                          blk
  , HasHeader                     blk
  , StandardHash                  blk
  , Eq                    (Header blk)
  , Show                  (Header blk)
  )

deriving instance (TestConstraints blk, Eq   it, Eq   rdr)
               => Eq   (Success blk it rdr)
deriving instance (TestConstraints blk, Show it, Show rdr)
               => Show (Success blk it rdr)

run :: forall m blk. (IOLike m, HasHeader blk)
    => ChainDB          m blk
    -> ChainDB.Internal m blk
    -> ResourceRegistry m
    -> StrictTVar m SlotNo
    ->    Cmd     blk (Iterator m blk) (Reader m blk blk)
    -> m (Success blk (Iterator m blk) (Reader m blk blk))
run ChainDB{..} internal@ChainDB.Internal{..} registry varCurSlot = \case
    AddBlock blk          -> Unit           <$> (advanceAndAdd (blockSlot blk) blk)
    AddFutureBlock blk s  -> Unit           <$> (advanceAndAdd s               blk)
    GetCurrentChain       -> Chain          <$> atomically getCurrentChain
    GetCurrentLedger      -> Ledger         <$> atomically getCurrentLedger
    GetTipBlock           -> MbBlock        <$> getTipBlock
    GetTipHeader          -> MbHeader       <$> getTipHeader
    GetTipPoint           -> Point          <$> atomically getTipPoint
    GetTipBlockNo         -> BlockNo        <$> atomically getTipBlockNo
    GetBlock pt           -> MbBlock        <$> getBlock pt
    GetGCedBlock pt       -> mbGCedBlock    <$> getBlock pt
    GetMaxSlotNo          -> MaxSlot        <$> atomically getMaxSlotNo
    StreamBlocks from to  -> iter           <$> streamBlocks registry from to
    IteratorNext  it      -> IterResult     <$> iteratorNext it
    IteratorNextGCed  it  -> iterResultGCed <$> iteratorNext it
    IteratorClose it      -> Unit           <$> iteratorClose it
    NewBlockReader        -> blockReader    <$> newBlockReader registry
    ReaderInstruction rdr -> MbChainUpdate  <$> readerInstruction rdr
    ReaderForward rdr pts -> MbPoint        <$> readerForward rdr pts
    ReaderClose rdr       -> Unit           <$> readerClose rdr
    Close                 -> Unit           <$> closeDB
    Reopen                -> Unit           <$> intReopen False
    RunBgTasks            -> ignore         <$> runBgTasks internal
  where
    mbGCedBlock = MbGCedBlock . MaybeGCedBlock True
    iterResultGCed = IterResultGCed . IteratorResultGCed True
    iter = either UnknownRange (Iter . deserialiseIterator)
    blockReader = BlockReader . deserialiseReader
    ignore _ = Unit ()

    advanceAndAdd newCurSlot blk = do
      open <- atomically isOpen
      when open $ do
        prevCurSlot <- atomically $ readTVar varCurSlot
        -- Tick each slot and execute the scheduled chain selections for that
        -- slot
        forM_ [prevCurSlot + 1..newCurSlot] $ \slot -> do
          atomically $ writeTVar varCurSlot slot
          intScheduledChainSelection slot
      addBlock blk

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
  , iterResult :: IteratorResult blk
  }

deriving instance (Show blk, Show (HeaderHash blk))
               => Show (IteratorResultGCed blk)

instance (Eq blk, Eq (HeaderHash blk)) => Eq (IteratorResultGCed blk) where
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
           TestConstraints           blk
        => NodeConfig (BlockProtocol blk)
        -> Cmd                       blk   IteratorId ReaderId
        -> DBModel                   blk
        -> (Resp                     blk   IteratorId ReaderId, DBModel blk)
runPure cfg = \case
    AddBlock blk          -> ok  Unit           $ update_ (advanceAndAdd (blockSlot blk) blk)
    AddFutureBlock blk s  -> ok  Unit           $ update_ (advanceAndAdd s               blk)
    GetCurrentChain       -> ok  Chain          $ query   (Model.lastK k getHeader)
    GetCurrentLedger      -> ok  Ledger         $ query    Model.currentLedger
    GetTipBlock           -> ok  MbBlock        $ query    Model.tipBlock
    GetTipHeader          -> ok  MbHeader       $ query   (fmap getHeader . Model.tipBlock)
    GetTipPoint           -> ok  Point          $ query    Model.tipPoint
    GetTipBlockNo         -> ok  BlockNo        $ query    Model.tipBlockNo
    GetBlock pt           -> err MbBlock        $ query   (Model.getBlockByPoint pt)
    GetGCedBlock pt       -> err mbGCedBlock    $ query   (Model.getBlockByPoint pt)
    GetMaxSlotNo          -> ok  MaxSlot        $ query    Model.maxSlotNo
    StreamBlocks from to  -> err iter           $ updateE (Model.streamBlocks k from to)
    IteratorNext  it      -> ok  IterResult     $ update  (Model.iteratorNext  it)
    IteratorNextGCed it   -> ok  iterResultGCed $ update  (Model.iteratorNext  it)
    IteratorClose it      -> ok  Unit           $ update_ (Model.iteratorClose it)
    NewBlockReader        -> ok  BlockReader    $ update   Model.readBlocks
    ReaderInstruction rdr -> err MbChainUpdate  $ updateE (Model.readerInstruction id rdr)
    ReaderForward rdr pts -> err MbPoint        $ updateE (Model.readerForward rdr pts)
    ReaderClose rdr       -> ok  Unit           $ update_ (Model.readerClose rdr)
    -- TODO can this execute while closed?
    RunBgTasks            -> ok  Unit           $ update_ (Model.garbageCollect k)
    Close                 -> openOrClosed       $ update_  Model.closeDB
    Reopen                -> openOrClosed       $ update_  Model.reopen
  where
    k = protocolSecurityParam cfg

    advanceAndAdd slot blk =
      Model.addBlock cfg blk . Model.advanceCurSlot cfg slot

    iter = either UnknownRange Iter
    mbGCedBlock = MbGCedBlock . MaybeGCedBlock False
    iterResultGCed = IterResultGCed . IteratorResultGCed False

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
      = (Resp (Left ClosedDBError), m)

    -- Executed whether the ChainDB is open or closed.
    openOrClosed f = first (Resp . Right . Unit) . f

runIO :: HasHeader blk
      => ChainDB          IO blk
      -> ChainDB.Internal IO blk
      -> ResourceRegistry IO
      -> StrictTVar       IO SlotNo
      ->     Cmd  blk (Iterator IO blk) (Reader IO blk blk)
      -> IO (Resp blk (Iterator IO blk) (Reader IO blk blk))
runIO db internal registry varCurSlot cmd =
    Resp <$> try (run db internal registry varCurSlot cmd)

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
type IterRef blk m r = Reference (Opaque (Iterator m blk)) r

-- | Mapping between iterator references and mocked iterators
type KnownIters blk m r = RefEnv (Opaque (Iterator m blk)) IteratorId r

-- | Concrete or symbolic references to a real reader
type ReaderRef blk m r = Reference (Opaque (Reader m blk blk)) r

-- | Mapping between iterator references and mocked readers
type KnownReaders blk m r = RefEnv (Opaque (Reader m blk blk)) ReaderId r

type DBModel blk = Model.Model blk

-- | Execution model
data Model blk m r = Model
  { dbModel      :: DBModel                           blk
  , knownIters   :: KnownIters                        blk m r
  , knownReaders :: KnownReaders                      blk m r
  , modelConfig  :: Opaque (NodeConfig (BlockProtocol blk))
  } deriving (Generic)

deriving instance (TestConstraints blk, Show1 r) => Show (Model blk m r)

-- | Initial model
initModel :: NodeConfig (BlockProtocol blk)
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
-- command itself, and a mocked version of the response.
data Event blk m r = Event
  { eventBefore   :: Model  blk m r
  , eventCmd      :: At Cmd blk m r
  , eventAfter    :: Model  blk m r
  , eventMockResp :: Resp   blk     IteratorId ReaderId
  }

deriving instance (TestConstraints blk, Show1 r) => Show (Event blk m r)

-- | Construct an event
lockstep :: (TestConstraints blk, Eq1 r, Show1 r)
         => Model     blk m r
         -> At Cmd    blk m r
         -> At Resp   blk m r
         -> Event     blk m r
lockstep model@Model {..} cmd (At resp) = Event
    { eventBefore   = model
    , eventCmd      = cmd
    , eventAfter    = model'
    , eventMockResp = mockResp
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
generator :: forall       blk m.
             (HasHeader   blk, OuroborosTag (BlockProtocol blk))
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
    , (if empty then 1 else 10, return GetTipBlockNo)
    , (10, genGetBlock)
    , (if empty then 1 else 10, return GetMaxSlotNo)

    -- Iterators
    , (if empty then 1 else 10, uncurry StreamBlocks <$> genBounds)
    , (if null iterators then 0 else 20, genIteratorNext)
      -- Use a lower frequency for closing, so that the chance increases that
      -- we can stream multiple blocks from an iterator.
    , (if null iterators then 0 else 2, genIteratorClose)

    -- Readers
    , (10, return NewBlockReader)
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
    cfg :: NodeConfig (BlockProtocol blk)
    cfg = unOpaque modelConfig

    secParam :: SecurityParam
    secParam = protocolSecurityParam cfg

    iterators :: [Reference (Opaque (Iterator m blk)) Symbolic]
    iterators = RE.keys knownIters

    readers :: [Reference (Opaque (Reader m blk blk)) Symbolic]
    readers = RE.keys knownReaders

    genRandomPoint :: Gen (Point blk)
    genRandomPoint = Block.blockPoint <$> genBlock m

    pointsInDB :: [Point blk]
    pointsInDB = Block.blockPoint <$> Map.elems (Model.blocks dbModel)

    empty :: Bool
    empty = null pointsInDB

    genPoint :: Gen (Point blk)
    genPoint = frequency
      [ (1, return Block.genesisPoint)
      , (2, genRandomPoint)
      , (if empty then 0 else 7, elements pointsInDB)
      ]

    genGetBlock :: Gen (Cmd blk it rdr)
    genGetBlock = do
      pt <- genPoint
      return $ if Model.garbageCollectablePoint secParam dbModel pt
        then GetGCedBlock pt
        else GetBlock     pt

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
      <$> (genFromInEx <*> genPoint)
      <*> (genToInEx   <*> genPoint)

    genFromInEx :: Gen (Point blk -> StreamFrom blk)
    genFromInEx = elements [StreamFromInclusive, StreamFromExclusive]

    genToInEx :: Gen (Point blk -> StreamTo blk)
    genToInEx = elements [StreamToInclusive, StreamToExclusive]

    -- Generate bounds that correspond to existing blocks in the DB. Make sure
    -- that the start bound is older than the end bound.
    -- NOTE: this does not mean that these bounds are on the same chain.
    genExistingBounds :: Gen (StreamFrom blk, StreamTo blk)
    genExistingBounds = do
      start <- elements pointsInDB
      end   <- elements pointsInDB `suchThat` ((>= Block.pointSlot start) .
                                               Block.pointSlot)
      (,) <$> (genFromInEx <*> return start)
          <*> (genToInEx   <*> return end)

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
   forall (iters cmd) (`elem` RE.keys knownIters)   .&&
   forall (rdrs  cmd) (`elem` RE.keys knownReaders) .&&
   case cmd of
     -- Even though we ensure this in the generator, shrinking might change
     -- it.
     GetBlock     pt      -> Not $ garbageCollectable pt
     GetGCedBlock pt      -> garbageCollectable pt
     IteratorNext     it  -> Not $ garbageCollectableIteratorNext it
     IteratorNextGCed it  -> garbageCollectableIteratorNext it

     -- TODO The real implementation allows streaming blocks from the
     -- VolatileDB that have no path to the current chain. The model
     -- implementation disallows this, as it only allows streaming from one of
     -- the possible forks, each starting at genesis. Temporarily only test
     -- with iterators that the model allows. So we only test a subset of the
     -- functionality, which does not include error paths.
     StreamBlocks from to -> isValidIterator from to
     -- Make sure we don't close (and reopen) when there are multiple equally
     -- preferable forks in the ChainDB, because we might pick another one
     -- than the current one when reopening, which would bring us out of sync
     -- with the model.
     Close                -> Not equallyPreferableFork
     -- To be in the future, @blockSlot blk@ must be greater than @slot@.
     AddFutureBlock blk s -> s .>= Model.currentSlot dbModel .&&
                             blockSlot blk .> s
     _                    -> Top
  where
    garbageCollectable :: Point blk -> Logic
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

    equallyPreferableFork :: Logic
    equallyPreferableFork = exists forks $ \fork ->
      Boolean (equallyPreferable cfg curChain fork) .&&
      Chain.head curChain ./= Chain.head fork

    cfg :: NodeConfig (BlockProtocol blk)
    cfg = unOpaque modelConfig

    secParam :: SecurityParam
    secParam = protocolSecurityParam cfg

    isValidIterator :: StreamFrom blk -> StreamTo blk -> Logic
    isValidIterator from to =
      case Model.between secParam from to dbModel of
        Left  _    -> Bot
        -- All blocks must be valid
        Right blks -> forall blks $ \blk -> Boolean $
          Map.notMember (blockHash blk) $
          forgetFingerprint (Model.invalid dbModel)

equallyPreferable :: forall blk.
                     ( OuroborosTag (BlockProtocol blk)
                     , HasHeader blk
                     , CanSelect (BlockProtocol blk) blk
                     )
                  => NodeConfig (BlockProtocol blk)
                  -> Chain blk -> Chain blk -> Bool
equallyPreferable cfg chain1 chain2 =
    not (preferAnchoredCandidate cfg chain1' chain2') &&
    not (preferAnchoredCandidate cfg chain2' chain1')
  where
    chain1', chain2' :: AnchoredFragment blk
    chain1' = Chain.toAnchoredFragment chain1
    chain2' = Chain.toAnchoredFragment chain2


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
          -> At Cmd blk IO Concrete
          -> IO (At Resp blk IO Concrete)
semantics db internal registry varCurSlot (At cmd) =
    At . (bimap (QSM.reference . QSM.Opaque) (QSM.reference . QSM.Opaque)) <$>
    runIO db internal registry varCurSlot (bimap QSM.opaque QSM.opaque cmd)

-- | The state machine proper
sm :: TestConstraints blk
   => ChainDB          IO       blk
   -> ChainDB.Internal IO       blk
   -> ResourceRegistry IO
   -> StrictTVar       IO SlotNo
   -> BlockGen                  blk IO
   -> NodeConfig (BlockProtocol blk)
   -> ExtLedgerState            blk
   -> StateMachine (Model       blk IO)
                   (At Cmd      blk IO)
                                    IO
                   (At Resp     blk IO)
sm db internal registry varCurSlot genBlock env initLedger = StateMachine
  { initModel     = initModel env initLedger
  , transition    = transition
  , precondition  = precondition
  , postcondition = postcondition
  , generator     = Just . generator genBlock
  , shrinker      = shrinker
  , semantics     = semantics db internal registry varCurSlot
  , mock          = mock
  , invariant     = Nothing
  , distribution  = Nothing
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

deriving instance ( ToExpr (ChainState (BlockProtocol blk))
                  , ToExpr (LedgerState blk)
                  )
                 => ToExpr (ExtLedgerState blk)
deriving instance ToExpr Fingerprint
deriving instance ToExpr BlockNo
deriving instance ToExpr SlotNo
deriving instance ToExpr ReaderNext
deriving instance ToExpr IteratorId
deriving instance ToExpr MaxSlotNo
deriving instance ToExpr blk  => ToExpr (Point.WithOrigin blk)
deriving instance ToExpr hash => ToExpr (Point.Block SlotNo hash)
deriving instance ToExpr (HeaderHash blk) => ToExpr (ChainHash blk)
deriving instance ToExpr (HeaderHash blk) => ToExpr (Point blk)
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
                  , ToExpr (HeaderHash blk)
                  , ToExpr (ChainState (BlockProtocol blk))
                  , ToExpr (LedgerState blk)
                  , ToExpr (ExtValidationError blk)
                  )
                 => ToExpr (DBModel blk)
deriving instance ( ToExpr blk
                  , ToExpr (HeaderHash blk)
                  , ToExpr (ChainState (BlockProtocol blk))
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
deriving instance ToExpr (LedgerState Blk)
deriving instance ToExpr BftValidationErr
deriving instance ToExpr (ExtValidationError Blk)

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

deriving instance SOP.Generic         (TraceEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceEvent blk)
deriving instance SOP.Generic         (TraceAddBlockEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceAddBlockEvent blk)
deriving instance SOP.Generic         (TraceReaderEvent blk)
deriving instance SOP.HasDatatypeInfo (TraceReaderEvent blk)
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
deriving instance SOP.Generic         (LedgerDB.TraceReplayEvent r replayTo blockInfo)
deriving instance SOP.HasDatatypeInfo (LedgerDB.TraceReplayEvent r replayTo blockInfo)
deriving instance SOP.Generic         (ImmDB.TraceEvent e hash)
deriving instance SOP.HasDatatypeInfo (ImmDB.TraceEvent e hash)

-- TODO labelling

{-------------------------------------------------------------------------------
  Generator for TestBlock
-------------------------------------------------------------------------------}

type Blk = TestBlock

-- | Note that the 'Blk = TestBlock' is general enough to be used by both the
-- ChainDB /and/ the ImmutableDB, its generators cannot. For example, in the
-- ChainDB, blocks are added /out of order/, while in the ImmutableDB, they
-- must be added /in order/. This generator can thus not be reused for the
-- ImmutableDB.
genBlk :: BlockGen Blk m
genBlk Model{..} = frequency
    [ (if empty then 0 else 1, genAlreadyInChain)
    , (5,                      genAppendToCurrentChain)
    , (5,                      genFitsOnSomewhere)
    , (3,                      genGap)
    ]
  where
    blocksInChainDB = Model.blocks dbModel

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
      [ (1, firstBlock <$> chooseSlot 0 2 <*> genBody)
      , (1, firstEBB <$> genBody)
      ]

    -- Helper that generates a block that fits onto the given block.
    genFitsOn :: TestBlock -> Gen TestBlock
    genFitsOn b = frequency
        [ (4, do
                slotNo <- if fromIsEBB (testBlockIsEBB b)
                  then chooseSlot (blockSlot b)     (blockSlot b + 2)
                  else chooseSlot (blockSlot b + 1) (blockSlot b + 3)
                body   <- genBody
                return $ mkNextBlock b slotNo body)
        -- An EBB is never followed directly by another EBB, otherwise they
        -- would have the same 'BlockNo', as the EBB has the same 'BlockNo' of
        -- the block before it.
        , (if fromIsEBB (testBlockIsEBB b) then 0 else 1, do
                let SlotNo prevSlotNo   = blockSlot b
                    EpochSize epochSize = fixedEpochSize
                    nextEBBSlotNo = SlotNo $
                      (prevSlotNo `div` epochSize + 1) * epochSize
                slotNo <- frequency
                  [ (7, return $ nextEBBSlotNo)
                    -- Skip an epoch
                  , (1, return $ nextEBBSlotNo + SlotNo (1 * epochSize))
                    -- Skip two epochs
                  ]
                body   <- genBody
                return $ mkNextEBB b slotNo body)
        ]

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

testCfg :: NodeConfig (BlockProtocol Blk)
testCfg = BftNodeConfig
    { bftParams   = BftParams { bftSecurityParam = k
                              , bftNumNodes      = 1
                              , bftSlotLengths   = singletonSlotLengths $
                                                     slotLengthFromSec 20
                              }
    , bftNodeId   = CoreId 0
    , bftSignKey  = SignKeyMockDSIGN 0
    , bftVerKeys  = Map.singleton (CoreId 0) (VerKeyMockDSIGN 0)
    }
  where
    k = SecurityParam 2

dbUnused :: ChainDB blk m
dbUnused = error "ChainDB used during command generation"

internalUnused :: ChainDB.Internal blk m
internalUnused = error "ChainDB.Internal used during command generation"

registryUnunused :: ResourceRegistry m
registryUnunused = error "ResourceRegistry used during command generation"

varCurSlotUnused :: StrictTVar m SlotNo
varCurSlotUnused = error "StrictTVar m SlotNo used during command generation"

smUnused :: StateMachine (Model Blk IO) (At Cmd Blk IO) IO (At Resp Blk IO)
smUnused =
    sm dbUnused internalUnused registryUnunused varCurSlotUnused genBlk
      testCfg testInitExtLedger

-- | The current slot can be changed by modifying the given 'StrictTVar'.
-- 'onSlotChange_' is not implemented as it is currently not used by the
-- ChainDB.
settableBlockchainTime :: IOLike m => StrictTVar m SlotNo -> BlockchainTime m
settableBlockchainTime varCurSlot = BlockchainTime {
      getCurrentSlot = readTVar varCurSlot
    , onSlotChange_  = error "unimplemented onSlotChange_"
    }

prop_sequential :: Property
prop_sequential = forAllCommands smUnused Nothing $ \cmds -> QC.monadicIO $ do
    (hist, prop) <- test cmds
    -- TODO label tags
    prettyCommands smUnused hist prop
  where
    test :: QSM.Commands (At Cmd Blk IO) (At Resp Blk IO)
         -> QC.PropertyM IO
            ( QSM.History (At Cmd Blk IO) (At Resp Blk IO)
            , Property
            )
    test cmds = do
      threadRegistry     <- QC.run unsafeNewRegistry
      iteratorRegistry   <- QC.run unsafeNewRegistry
      immRegistry        <- QC.run unsafeNewRegistry
      (tracer, getTrace) <- QC.run recordingTracerIORef
      varCurSlot         <- QC.run $ uncheckedNewTVarM 0
      fsVars             <- QC.run $ (,,)
        <$> uncheckedNewTVarM Mock.empty
        <*> uncheckedNewTVarM Mock.empty
        <*> uncheckedNewTVarM Mock.empty
      let args = mkArgs testCfg testInitExtLedger tracer threadRegistry varCurSlot fsVars
      (db, internal)     <- QC.run $ openDBInternal immRegistry args False
      let sm' = sm db internal iteratorRegistry varCurSlot genBlk testCfg testInitExtLedger
      (hist, model, res) <- runCommands sm' cmds
      (realChain, realChain', trace, fses, remainingCleanups) <- QC.run $ do
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
        (db', _) <- openDBInternal immRegistry args False
        realChain' <- toChain db'
        closeDB db'

        closeRegistry immRegistry
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

        return (realChain, realChain', trace, fses, remainingCleanups)

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
             (equallyPreferable (cdbNodeConfig args) realChain realChain') .&&.
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

    invalidBlock :: TraceEvent blk -> Maybe (Point blk)
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

fixedEpochSize :: EpochSize
fixedEpochSize = 10

mkArgs :: IOLike m
       => NodeConfig (BlockProtocol Blk)
       -> ExtLedgerState Blk
       -> Tracer m (TraceEvent Blk)
       -> ResourceRegistry m
       -> StrictTVar m SlotNo
       -> (StrictTVar m MockFS, StrictTVar m MockFS, StrictTVar m MockFS)
          -- ^ ImmutableDB, VolatileDB, LedgerDB
       -> ChainDbArgs m Blk
mkArgs cfg initLedger tracer registry varCurSlot
       (immDbFsVar, volDbFsVar, lgrDbFsVar) = ChainDbArgs
    { -- Decoders
      cdbDecodeHash       = decode
    , cdbDecodeBlock      = const <$> decode
    , cdbDecodeHeader     = const <$> decode
    , cdbDecodeLedger     = decode
    , cdbDecodeChainState = decode

      -- Encoders
    , cdbEncodeHash       = encode
    , cdbEncodeBlock      = testBlockToBinaryInfo
    , cdbEncodeHeader     = encode
    , cdbEncodeLedger     = encode
    , cdbEncodeChainState = encode

      -- Error handling
    , cdbErrImmDb         = EH.monadCatch
    , cdbErrVolDb         = EH.monadCatch
    , cdbErrVolDbSTM      = EH.throwSTM

      -- HasFS instances
    , cdbHasFSImmDb       = simHasFS EH.monadCatch immDbFsVar
    , cdbHasFSVolDb       = simHasFS EH.monadCatch volDbFsVar
    , cdbHasFSLgrDB       = simHasFS EH.monadCatch lgrDbFsVar

      -- Policy
    , cdbValidation       = ValidateAllEpochs
    , cdbBlocksPerFile    = 4
    , cdbParamsLgrDB      = ledgerDbDefaultParams (protocolSecurityParam cfg)
    , cdbDiskPolicy       = defaultDiskPolicy (protocolSecurityParam cfg)

      -- Integration
    , cdbNodeConfig       = cfg
    , cdbEpochInfo        = fixedSizeEpochInfo fixedEpochSize
    , cdbHashInfo         = testHashInfo
    , cdbIsEBB            = testBlockEpochNoIfEBB fixedEpochSize
    , cdbCheckIntegrity   = testBlockIsValid
    , cdbGenesis          = return initLedger
    , cdbBlockchainTime   = settableBlockchainTime varCurSlot
    , cdbAddHdrEnv        = const id

    -- Misc
    , cdbTracer           = tracer
    , cdbTraceLedger      = nullTracer
    , cdbRegistry         = registry
    , cdbGcDelay          = 0
    }

tests :: TestTree
tests = testGroup "ChainDB q-s-m"
    [ testProperty "sequential" prop_sequential
    ]

{-------------------------------------------------------------------------------
  Running commands directly
-------------------------------------------------------------------------------}

-- | Debugging utility: run some commands against the real implementation.
_runCmds :: [Cmd Blk IteratorId ReaderId] -> IO [Resp Blk IteratorId ReaderId]
_runCmds cmds = withRegistry $ \registry -> withRegistry $ \immRegistry -> do
    varCurSlot <- uncheckedNewTVarM 0
    fsVars <- (,,)
      <$> uncheckedNewTVarM Mock.empty
      <*> uncheckedNewTVarM Mock.empty
      <*> uncheckedNewTVarM Mock.empty
    let args = mkArgs
          testCfg
          testInitExtLedger
          (showTracing stdoutTracer)
          registry
          varCurSlot
          fsVars
    (db, internal) <- openDBInternal immRegistry args False
    evalStateT (mapM (go db internal registry varCurSlot) cmds) emptyRunCmdState
  where
    go :: ChainDB IO Blk -> ChainDB.Internal IO Blk
       -> ResourceRegistry IO
       -> StrictTVar IO SlotNo
       -> Cmd Blk IteratorId ReaderId
       -> StateT RunCmdState
                 IO
                 (Resp Blk IteratorId ReaderId)
    go db internal resourceRegistry varCurSlot cmd = do
      RunCmdState { _knownIters, _knownReaders} <- get
      let cmd' = At $
            bimap (revLookup _knownIters) (revLookup _knownReaders) cmd
      resp <- lift $ unAt <$> semantics db internal resourceRegistry varCurSlot cmd'
      newIters <- RE.fromList <$>
       mapM (\rdr -> (rdr, ) <$> newIteratorId) (iters resp)
      newReaders <- RE.fromList <$>
        mapM (\rdr -> (rdr, ) <$> newReaderId) (rdrs  resp)
      let knownIters'   = _knownIters   `RE.union` newIters
          knownReaders' = _knownReaders `RE.union` newReaders
          resp'      = bimap (knownIters' RE.!) (knownReaders' RE.!) resp
      modify $ \s ->
        s { _knownIters = knownIters', _knownReaders = knownReaders' }
      return resp'

    revLookup :: Eq a => RefEnv k a r -> a -> Reference k r
    revLookup env a = head $ RE.reverseLookup (== a) env

    newIteratorId :: forall m. Monad m => StateT RunCmdState m IteratorId
    newIteratorId = do
      s@RunCmdState { _nextIteratorId } <- get
      put (s { _nextIteratorId = succ _nextIteratorId })
      return _nextIteratorId

    newReaderId :: forall m. Monad m => StateT RunCmdState m ReaderId
    newReaderId = do
      s@RunCmdState { _nextReaderId } <- get
      put (s { _nextReaderId = succ _nextReaderId })
      return _nextReaderId

data RunCmdState = RunCmdState
  { _knownIters     :: KnownIters   Blk IO Concrete
  , _knownReaders   :: KnownReaders Blk IO Concrete
  , _nextIteratorId :: IteratorId
  , _nextReaderId   :: ReaderId
  }

emptyRunCmdState :: RunCmdState
emptyRunCmdState = RunCmdState
  { _knownIters     = RE.empty
  , _knownReaders   = RE.empty
  , _nextIteratorId = IteratorId 0
  , _nextReaderId   = 0
  }
