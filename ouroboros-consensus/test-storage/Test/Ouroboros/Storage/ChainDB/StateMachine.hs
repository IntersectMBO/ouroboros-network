{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wredundant-constraints #-}
module Test.Ouroboros.Storage.ChainDB.StateMachine
  ( tests
  , mkArgs
  ) where

import           Prelude hiding (elem)

import           Codec.Serialise (decode, encode)
import           Control.Monad (replicateM, unless)
import           Control.Monad.State (StateT, evalStateT, get, lift, modify,
                     put)
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Bifunctor.TH as TH
import           Data.Bitraversable
import           Data.Functor.Classes (Eq1, Show1)
import           Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Ord (Down (..))
import           Data.Proxy
import           Data.TreeDiff (ToExpr)
import           Data.Typeable
import           Data.Word (Word64)
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
                     HasHeader, HeaderHash, MaxSlotNo, Point, SlotNo (..),
                     StandardHash)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.MockChain.ProducerState (ChainProducerState,
                     ReaderNext, ReaderState)
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import qualified Ouroboros.Network.Point as Point

import           Ouroboros.Consensus.Block (getHeader)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike hiding (fork)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Fingerprint (..))

import           Ouroboros.Storage.ChainDB
import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.ChainDB.Model as Model
import           Ouroboros.Storage.EpochInfo (fixedSizeEpochInfo)
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (simHasFS)
import           Ouroboros.Storage.ImmutableDB
                     (ValidationPolicy (ValidateAllEpochs))
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import           Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import           Ouroboros.Storage.LedgerDB.InMemory (ledgerDbDefaultParams)
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.ChainDB.TestBlock
import           Test.Ouroboros.Storage.Util ((=:=))

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

deriving instance SOP.Generic         (Cmd blk it rdr)
deriving instance SOP.HasDatatypeInfo (Cmd blk it rdr)

-- | Return type for successful database operations.
data Success blk it rdr
  = Unit          ()
  | Chain         (AnchoredFragment (Header blk))
  | Ledger        (ExtLedgerState blk)
  | MbBlock       (Maybe blk)
  | MbGCedBlock   (MaybeGCedBlock blk)
  | MbHeader      (Maybe (Header blk))
  | Point         (Point blk)
  | BlockNo       BlockNo
  | UnknownRange  (UnknownRange blk)
  | Iter          it
  | IterResult    (IteratorResult blk)
  | BlockReader   rdr
  | MbChainUpdate (Maybe (ChainUpdate blk blk))
  | MbPoint       (Maybe (Point blk))
  | MaxSlot       MaxSlotNo
  deriving (Functor, Foldable, Traversable)

type TestConstraints blk =
  ( OuroborosTag   (BlockProtocol blk)
  , ProtocolLedgerView            blk
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

run :: forall m blk. (IOLike m)
    => ChainDB          m blk
    -> ChainDB.Internal m blk
    -> ResourceRegistry m
    ->    Cmd     blk (Iterator m blk) (Reader m blk blk)
    -> m (Success blk (Iterator m blk) (Reader m blk blk))
run ChainDB{..} ChainDB.Internal{..} registry = \case
    AddBlock blk          -> Unit          <$> addBlock blk
    GetCurrentChain       -> Chain         <$> atomically getCurrentChain
    GetCurrentLedger      -> Ledger        <$> atomically getCurrentLedger
    GetTipBlock           -> MbBlock       <$> getTipBlock
    GetTipHeader          -> MbHeader      <$> getTipHeader
    GetTipPoint           -> Point         <$> atomically getTipPoint
    GetTipBlockNo         -> BlockNo       <$> atomically getTipBlockNo
    GetBlock pt           -> MbBlock       <$> getBlock pt
    GetGCedBlock pt       -> mbGCedBlock   <$> getBlock pt
    GetMaxSlotNo          -> MaxSlot       <$> atomically getMaxSlotNo
    StreamBlocks from to  -> iter          <$> streamBlocks registry from to
    IteratorNext  it      -> IterResult    <$> iteratorNext it
    IteratorClose it      -> Unit          <$> iteratorClose it
    NewBlockReader        -> BlockReader   <$> newBlockReader registry
    ReaderInstruction rdr -> MbChainUpdate <$> readerInstruction rdr
    ReaderForward rdr pts -> MbPoint       <$> readerForward rdr pts
    ReaderClose rdr       -> Unit          <$> readerClose rdr
    Close                 -> Unit          <$> closeDB
    Reopen                -> Unit          <$> intReopen False
    RunBgTasks            -> ignore        <$> runBgTasks
  where
    mbGCedBlock = MbGCedBlock . MaybeGCedBlock True
    iter = either UnknownRange Iter
    ignore _ = Unit ()

    runBgTasks = do
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


{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error.
newtype Resp blk it rdr = Resp
  { getResp :: Either (ChainDbError blk) (Success blk it rdr) }
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
    AddBlock blk          -> ok  Unit          $ update_ (Model.addBlock cfg blk)
    GetCurrentChain       -> ok  Chain         $ query   (Model.lastK k getHeader)
    GetCurrentLedger      -> ok  Ledger        $ query    Model.currentLedger
    GetTipBlock           -> ok  MbBlock       $ query    Model.tipBlock
    GetTipHeader          -> ok  MbHeader      $ query   (fmap getHeader . Model.tipBlock)
    GetTipPoint           -> ok  Point         $ query    Model.tipPoint
    GetTipBlockNo         -> ok  BlockNo       $ query    Model.tipBlockNo
    GetBlock pt           -> err MbBlock       $ query   (Model.getBlockByPoint pt)
    GetGCedBlock pt       -> err mbGCedBlock   $ query   (Model.getBlockByPoint pt)
    GetMaxSlotNo          -> ok  MaxSlot       $ query    Model.maxSlotNo
    StreamBlocks from to  -> err iter          $ updateE (Model.streamBlocks k from to)
    IteratorNext  it      -> ok  IterResult    $ update  (Model.iteratorNext  it)
    IteratorClose it      -> ok  Unit          $ update_ (Model.iteratorClose it)
    NewBlockReader        -> ok  BlockReader   $ update   Model.readBlocks
    ReaderInstruction rdr -> err MbChainUpdate $ updateE (Model.readerInstruction rdr)
    ReaderForward rdr pts -> err MbPoint       $ updateE (Model.readerForward rdr pts)
    ReaderClose rdr       -> ok  Unit          $ update_ (Model.readerClose rdr)
    -- TODO can this execute while closed?
    RunBgTasks            -> ok  Unit          $ update_ (Model.garbageCollect k)
    Close                 -> openOrClosed      $ update_  Model.closeDB
    Reopen                -> openOrClosed      $ update_  Model.reopen
  where
    k = protocolSecurityParam cfg

    iter = either UnknownRange Iter
    mbGCedBlock = MbGCedBlock . MaybeGCedBlock False

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

runIO :: TestConstraints blk
      => ChainDB          IO blk
      -> ChainDB.Internal IO blk
      -> ResourceRegistry IO
      ->     Cmd  blk (Iterator IO blk) (Reader IO blk blk)
      -> IO (Resp blk (Iterator IO blk) (Reader IO blk blk))
runIO db internal registry cmd = Resp <$> try (run db internal registry cmd)

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

-- | Don't print the 'At' constructor.
instance Show (t blk (IterRef blk m r) (ReaderRef blk m r))
      => Show (At t blk m r) where
  show = show . unAt

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
    [ (20, AddBlock <$> genBlock m)
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

    genIteratorNext  = IteratorNext  <$> elements iterators
    genIteratorClose = IteratorClose <$> elements iterators

    genReaderInstruction = ReaderInstruction <$> elements readers
    genReaderForward     = ReaderForward     <$> elements readers
                                             <*> genReaderForwardPoints

    genReaderForwardPoints :: Gen [Point blk]
    genReaderForwardPoints = choose (1, 3) >>= \n ->
      sortOn (Down . Block.pointSlot) <$> replicateM n genReaderForwardPoint

    genReaderForwardPoint :: Gen (Point blk)
    genReaderForwardPoint = genPoint

    genReaderClose = ReaderClose <$> elements readers

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
     _                    -> Top
  where
    garbageCollectable :: Point blk -> Logic
    garbageCollectable =
      Boolean . Model.garbageCollectablePoint secParam dbModel

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
        Left  _ -> Bot
        Right _ -> Top

equallyPreferable :: (OuroborosTag (BlockProtocol blk), HasHeader blk)
                  => NodeConfig (BlockProtocol blk)
                  -> Chain blk -> Chain blk -> Bool
equallyPreferable cfg chain1 chain2 =
    compareCandidates cfg (Chain.toAnchoredFragment chain1) (Chain.toAnchoredFragment chain2) == EQ


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
          -> At Cmd blk IO Concrete
          -> IO (At Resp blk IO Concrete)
semantics db internal registry (At cmd) =
    At . (bimap (QSM.reference . QSM.Opaque) (QSM.reference . QSM.Opaque)) <$>
    runIO db internal registry (bimap QSM.opaque QSM.opaque cmd)

-- | The state machine proper
sm :: TestConstraints blk
   => ChainDB          IO       blk
   -> ChainDB.Internal IO       blk
   -> ResourceRegistry IO
   -> BlockGen                  blk IO
   -> NodeConfig (BlockProtocol blk)
   -> ExtLedgerState            blk
   -> StateMachine (Model       blk IO)
                   (At Cmd      blk IO)
                                    IO
                   (At Resp     blk IO)
sm db internal registry genBlock env initLedger = StateMachine
  { initModel     = initModel env initLedger
  , transition    = transition
  , precondition  = precondition
  , postcondition = postcondition
  , generator     = Just . generator genBlock
  , shrinker      = shrinker
  , semantics     = semantics db internal registry
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

deriving instance Generic Fingerprint
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
deriving instance ( ToExpr blk
                  , ToExpr (HeaderHash blk)
                  , ToExpr (ChainState (BlockProtocol blk))
                  , ToExpr (LedgerState blk)
                  )
                 => ToExpr (DBModel blk)
deriving instance ( ToExpr blk
                  , ToExpr (HeaderHash blk)
                  , ToExpr (ChainState (BlockProtocol blk))
                  , ToExpr (LedgerState blk)
                  )
                 => ToExpr (Model blk IO Concrete)

-- Blk specific instances

deriving instance ToExpr TestHash
deriving instance ToExpr Blk
deriving instance ToExpr (LedgerState Blk)

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
deriving instance SOP.Generic         (ImmDB.TraceEvent e)
deriving instance SOP.HasDatatypeInfo (ImmDB.TraceEvent e)

-- TODO labelling

{-------------------------------------------------------------------------------
  Generator for TestBlock
-------------------------------------------------------------------------------}

type Blk = TestBlock

genBlk :: BlockGen Blk m
genBlk Model{..} = do
    testHash <- genHash
    let slot = 2 * fromIntegral (length (unTestHash testHash))
    return $ TestBlock
      { tbHash  = testHash
      , tbSlot  = SlotNo slot
      , tbValid = isValid testHash
      }
  where
    hashesInDB :: [TestHash]
    hashesInDB = Map.keys (Model.blocks dbModel)

    empty :: Bool
    empty = null hashesInDB

    -- If the maximum fork number is @n@, it means that only @n + 1@ different
    -- forks may "fork off" after each block.
    maxForkNo = 2

    -- We say that a block with most recent fork number 2 is invalid.
    --
    -- The hash (point) of an invalid block is remembered to ignore that block
    -- in the future, this means that for each hash, the value of 'tbValid'
    -- must always be the same. In other words, we can never generate an
    -- invalid block with hash @h@ and also generate a valid block with the
    -- same hash @h@. Formally:
    --
    -- > forall b1 b2. (tbHash b1 == tbHash b2) <=> (tbValid b1 == tbValid b2)
    --
    -- To enforce this, we reserve the most recent fork number for invalid
    -- blocks, e.g., [2], [2,0], [2,0,1,0], [2,0,0,2,0] (since 2 occured
    -- before, this block is already on an invalid chain). Note that valid
    -- blocks may fit onto an invalid chain, e.g., [0,2,0].
    isValid :: TestHash -> Bool
    isValid (TestHash (2 NE.:| _)) = False
    isValid _                      = True

    genForkNo :: Gen Word64
    genForkNo = frequency
      -- Give a slight preference to "not forking off"
      [ (1, return 0)
      , (1, choose (1, maxForkNo))
      ]

    genHash :: Gen TestHash
    genHash = frequency
        [ (1, genRandomHash)
        , (5, genAppendToCurrentChain)
        , (if empty then 0 else 3, genFitsOnSomewhere)
        , (3, genGap)
        ]

    -- A random hash: random length and random fork numbers
    genRandomHash :: Gen TestHash
    genRandomHash = do
        n <- frequency
          [ (5, choose (1,  3))
          , (4, choose (4,  6))
          , (3, choose (7,  9))
          , (2, choose (10, 12))
          , (1, choose (12, 20))
          ]
        TestHash . NE.fromList <$> replicateM n genForkNo

    -- A hash that fits onto the current chain, it may or may not fork off.
    genAppendToCurrentChain :: Gen TestHash
    genAppendToCurrentChain = do
        forkNo <- genForkNo
        return $ TestHash $ case Block.pointHash $ Model.tipPoint dbModel of
          GenesisHash            ->         forkNo NE.:| []
          BlockHash (TestHash h) -> NE.cons forkNo h

    -- A hash that fits onto a block in the ChainDB. The block could be at the
    -- tip of the chain and the hash might already be present in the ChainDB.
    genFitsOnSomewhere :: Gen TestHash
    genFitsOnSomewhere = do
        hashInDB <- unTestHash <$> elements hashesInDB
        forkNo <- genForkNo
        return $ TestHash $ NE.cons forkNo hashInDB

    -- A hash that doesn't fit onto a block in the ChainDB, but it creates a
    -- gap of a couple of blocks between genesis or an existing block in the
    -- ChainDB.
    genGap :: Gen TestHash
    genGap = do
        hash <- frequency
          [ (if empty then 0 else 3,
             NE.toList . unTestHash <$> elements hashesInDB)
            -- Genesis
          , (1, return [])
          ]
        gapSize <- choose (1, 2) -- TODO relate it to @k@?
        -- ForkNos for the gap
        forkNos <- replicateM gapSize genForkNo
        return $ TestHash $ NE.fromList $ foldr (:) hash forkNos

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

testCfg :: NodeConfig (BlockProtocol Blk)
testCfg = BftNodeConfig
    { bftParams   = BftParams { bftSecurityParam = k
                              , bftNumNodes      = 1
                              }
    , bftNodeId   = CoreId 0
    , bftSignKey  = SignKeyMockDSIGN 0
    , bftVerKeys  = Map.singleton (CoreId 0) (VerKeyMockDSIGN 0)
    }
  where
    k = SecurityParam 2

testInitLedger :: ExtLedgerState Blk
testInitLedger = testInitExtLedger

dbUnused :: ChainDB blk m
dbUnused = error "ChainDB used during command generation"

internalUnused :: ChainDB.Internal blk m
internalUnused = error "ChainDB.Internal used during command generation"

registryUnunused :: ResourceRegistry m
registryUnunused = error "ResourceRegistry used during command generation"

smUnused :: StateMachine (Model Blk IO) (At Cmd Blk IO) IO (At Resp Blk IO)
smUnused = sm dbUnused internalUnused registryUnunused genBlk testCfg testInitLedger

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
      (tracer, getTrace) <- QC.run recordingTracerIORef
      fsVars             <- QC.run $ (,,)
        <$> uncheckedNewTVarM Mock.empty
        <*> uncheckedNewTVarM Mock.empty
        <*> uncheckedNewTVarM Mock.empty
      let args = mkArgs testCfg testInitLedger tracer threadRegistry fsVars
      (db, internal)     <- QC.run $ openDBInternal args False
      let sm' = sm db internal iteratorRegistry genBlk testCfg testInitLedger
      (hist, model, res) <- runCommands sm' cmds
      (realChain, realChain', trace, fses, remainingCleanups) <- QC.run $ do
        trace <- getTrace
        open <- atomically $ isOpen db
        unless open $ intReopen internal False
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

        return (realChain, realChain', trace, fses, remainingCleanups)

      let modelChain = Model.currentChain $ dbModel model
          (immDbFs, volDbFs, lgrDbFs) = fses
          prop =
            counterexample ("Real  chain: " <> condense realChain)       $
            counterexample ("Model chain: " <> condense modelChain)      $
            counterexample ("TraceEvents: " <> unlines (map show trace)) $
            tabulate "Chain length" [show (Chain.length realChain)]      $
            tabulate "TraceEvents" (map traceEventName trace)            $
            res === Ok               .&&.
            realChain =:= modelChain .&&.
            prop_trace trace         .&&.
            -- Another equally preferable fork may be selected when opening
            -- the DB.
            equallyPreferable (cdbNodeConfig args) realChain realChain' .&&.
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

mkArgs :: IOLike m
       => NodeConfig (BlockProtocol Blk)
       -> ExtLedgerState Blk
       -> Tracer m (TraceEvent Blk)
       -> ResourceRegistry m
       -> (StrictTVar m MockFS, StrictTVar m MockFS, StrictTVar m MockFS)
          -- ^ ImmutableDB, VolatileDB, LedgerDB
       -> ChainDbArgs m Blk
mkArgs cfg initLedger tracer registry
       (immDbFsVar, volDbFsVar, lgrDbFsVar) = ChainDbArgs
    { -- Decoders
      cdbDecodeHash       = decode
    , cdbDecodeBlock      = const <$> decode
    , cdbDecodeLedger     = decode
    , cdbDecodeChainState = decode

      -- Encoders
    , cdbEncodeBlock      = encode
    , cdbEncodeHash       = encode
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
    , cdbDiskPolicy       = defaultDiskPolicy (protocolSecurityParam cfg) 10000

      -- Integration
    , cdbNodeConfig       = cfg
    , cdbEpochInfo        = fixedSizeEpochInfo 10
    , cdbIsEBB            = const Nothing -- TODO
    , cdbGenesis          = return initLedger

    -- Misc
    , cdbTracer           = tracer
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

_mkBlk :: [Word64] -> TestBlock
_mkBlk h = TestBlock
    { tbHash  = testHashFromList h
    , tbSlot  = SlotNo $ fromIntegral $ 2 * length h
    , tbValid = True
    }

-- | Debugging utility: run some commands against the real implementation.
_runCmds :: [Cmd Blk IteratorId ReaderId] -> IO [Resp Blk IteratorId ReaderId]
_runCmds cmds = withRegistry $ \registry -> do
    fsVars <- (,,)
      <$> uncheckedNewTVarM Mock.empty
      <*> uncheckedNewTVarM Mock.empty
      <*> uncheckedNewTVarM Mock.empty
    let args = mkArgs
          testCfg
          testInitLedger
          (showTracing stdoutTracer)
          registry
          fsVars
    (db, internal) <- openDBInternal args False
    evalStateT (mapM (go db internal registry) cmds) emptyRunCmdState
  where
    go :: ChainDB IO Blk -> ChainDB.Internal IO Blk
       -> ResourceRegistry IO
       -> Cmd Blk IteratorId ReaderId
       -> StateT RunCmdState
                 IO
                 (Resp Blk IteratorId ReaderId)
    go db internal resourceRegistry cmd = do
      RunCmdState { _knownIters, _knownReaders} <- get
      let cmd' = At $
            bimap (revLookup _knownIters) (revLookup _knownReaders) cmd
      resp <- lift $ unAt <$> semantics db internal resourceRegistry cmd'
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
