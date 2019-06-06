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
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wredundant-constraints #-}
module Test.Ouroboros.Storage.ChainDB.StateMachine where

import           Prelude hiding (elem)

import           Codec.Serialise (decode, encode)
import           Control.Monad (void)
import           Data.Bifunctor (first)
import           Data.Foldable (toList)
import           Data.Functor.Classes (Eq1, Show1)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.TreeDiff (ToExpr)
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic, Generic1)

import qualified Generics.SOP as SOP

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.StateMachine
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (BlockNo (..), ChainHash (..),
                     HasHeader, HeaderHash, Point, SlotNo (..), StandardHash)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (Chain (..), chainToList, genesisPoint)
import           Ouroboros.Network.ChainProducerState (ChainProducerState,
                     ReaderNext, ReaderState)
import qualified Ouroboros.Network.ChainProducerState as CPS

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.ChainDB hiding (ChainUpdate (..))
import qualified Ouroboros.Storage.ChainDB.Model as Model
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (simHasFS)
import           Ouroboros.Storage.ImmutableDB
                     (ValidationPolicy (ValidateAllEpochs))
import           Ouroboros.Storage.LedgerDB.MemPolicy (defaultMemPolicy)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.ChainDB.TestBlock

import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE


{-------------------------------------------------------------------------------
  Abstract model
-------------------------------------------------------------------------------}

-- | Commands
data Cmd blk hdr it
  = AddBlock         blk
  | GetCurrentChain
  | GetCurrentLedger
  | GetTipBlock
  | GetTipHeader
  | GetTipPoint
  | GetBlock         (Point blk)
  | StreamBlocks     (StreamFrom blk) (StreamTo blk)
  | IteratorNext     it
  | IteratorClose    it
  deriving (Generic, Show, Functor, Foldable, Traversable)

-- TODO getIsFetched, readBlocks, readHeaders, knownInvalidBlocks

deriving instance SOP.Generic         (Cmd blk hdr it)
deriving instance SOP.HasDatatypeInfo (Cmd blk hdr it)

-- | Return type for successful database operations.
data Success blk hdr it
  = Unit         ()
  | Chain        (AnchoredFragment hdr)
  | Ledger       (ExtLedgerState blk)
  | MbBlock      (Maybe blk)
  | MbHeader     (Maybe hdr)
  | Point        (Point blk)
  | UnknownRange (UnknownRange blk)
  | Iter         it
  | IterResult   (IteratorResult blk)
  deriving (Functor, Foldable, Traversable)

type TestConstraints blk hdr =
  ( OuroborosTag (BlockProtocol blk)
  , UpdateLedger                blk
  , ProtocolLedgerView          blk
  , LedgerConfigView            blk
  , HasHeader                   blk
  , StandardHash                blk
  , HasHeader                       hdr
  , StandardHash                    hdr
  )

deriving instance (TestConstraints blk hdr, Eq blk, Eq hdr, Eq it)
                => Eq (Success blk hdr it)
deriving instance (TestConstraints blk hdr, Show blk, Show hdr, Show it)
                => Show (Success blk hdr it)

run :: forall m blk hdr. MonadSTM m
    => ChainDB m blk hdr
    -> Cmd blk hdr (Iterator m blk)
    -> m (Success blk hdr (Iterator m blk))
run ChainDB{..} = \case
    AddBlock blk         -> Unit       <$> addBlock blk
    GetCurrentChain      -> Chain      <$> atomically getCurrentChain
    GetCurrentLedger     -> Ledger     <$> atomically getCurrentLedger
    GetTipBlock          -> MbBlock    <$> getTipBlock
    GetTipHeader         -> MbHeader   <$> getTipHeader
    GetTipPoint          -> Point      <$> atomically getTipPoint
    GetBlock pt          -> MbBlock    <$> getBlock pt
    StreamBlocks from to -> iter       <$> streamBlocks from to
    IteratorNext  it     -> IterResult <$> iteratorNext it
    IteratorClose it     -> Unit       <$> iteratorClose it
  where
    iter = either UnknownRange Iter


{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error.
newtype Resp blk hdr it = Resp
  { getResp :: Either (ChainDbError blk) (Success blk hdr it) }
  deriving (Functor, Foldable, Traversable)

deriving instance (TestConstraints blk hdr, Show blk, Show hdr, Show it)
               => Show (Resp blk hdr it)

instance (TestConstraints blk hdr, Eq blk, Eq hdr, Eq it)
       => Eq (Resp blk hdr it) where
  Resp (Left  e) == Resp (Left  e') = e == e'
  Resp (Right a) == Resp (Right a') = a == a'
  _              == _               = False

-- | Read-only data needed to run 'Cmd's on the pure model.
data ModelEnv blk hdr = ModelEnv
  { cfg         :: NodeConfig (BlockProtocol blk)
  , blockHeader :: blk -> hdr
  }

-- We can't reuse 'run' because the 'ChainDB' API uses 'STM'. Instead, we call
-- the model directly.
runPure :: TestConstraints blk hdr
        => ModelEnv        blk hdr
        -> Cmd             blk hdr   IteratorId
        -> Model.Model     blk
        -> (Resp           blk hdr   IteratorId, Model.Model blk)
runPure ModelEnv{..} = \case
    AddBlock blk         -> ok  Unit       . update_ (Model.addBlock cfg blk)
    GetCurrentChain      -> ok  Chain      . query   (Model.lastK k blockHeader)
    GetCurrentLedger     -> ok  Ledger     . query    Model.currentLedger
    GetTipBlock          -> ok  MbBlock    . query    Model.tipBlock
    GetTipHeader         -> ok  MbHeader   . query   (fmap blockHeader . Model.tipBlock)
    GetTipPoint          -> ok  Point      . query    Model.tipPoint
    GetBlock pt          -> err MbBlock    . query   (Model.getBlockByPoint pt)
    StreamBlocks from to -> err iter       . updateE (Model.streamBlocks k from to)
    IteratorNext  it     -> ok  IterResult . update  (Model.iteratorNext  it)
    IteratorClose it     -> ok  Unit       . update_ (Model.iteratorClose it)
  where
    ok  suc = first (Resp . Right . suc)
    err suc = first (Resp . fmap suc)

    k = protocolSecurityParam cfg

    iter = either UnknownRange Iter

    query   f m = (f m, m)

    update  f m = f m
    update_ f m = ((), f m)
    updateE f m = case f m of
      Left  e       -> (Left e, m)
      Right (a, m') -> (Right a, m')


runIO :: TestConstraints blk hdr
      => ChainDB IO blk hdr
      -> Cmd blk hdr (Iterator IO blk)
      -> IO (Resp blk hdr (Iterator IO blk))
runIO db cmd = Resp <$> try (run db cmd)

{-------------------------------------------------------------------------------
  Collect arguments
-------------------------------------------------------------------------------}

-- | Collect all iterators created.
iters :: Traversable t => t it -> [it]
iters = toList

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

-- | Concrete or symbolic references to a real iterator
type IterRef blk hdr m r = Reference (Opaque (Iterator m blk)) r

-- | Mapping between iterator references and mocked iterators
type KnownIters blk hdr m r = RefEnv (Opaque (Iterator m blk)) IteratorId r

type DBModel blk = Model.Model blk

-- | Execution model
data Model blk hdr m r = Model
  { dbModel    :: DBModel          blk
  , knownIters :: KnownIters       blk hdr m r
  , modelEnv   :: Opaque (ModelEnv blk hdr)
  } deriving (Generic)

deriving instance (TestConstraints blk hdr, Show blk, Show1 r)
               => Show (Model blk hdr m r)

-- | Initial model
initModel :: ModelEnv blk hdr -> ExtLedgerState blk -> Model blk hdr m r
initModel env initLedger = Model
  { dbModel    = Model.empty initLedger
  , knownIters = RE.empty
  , modelEnv   = QSM.Opaque env
  }


-- | Key property of the model is that we can go from real to mock responses
toMock :: (Functor (t blk hdr), Eq1 r)
       => Model blk hdr m r -> At t blk hdr m r -> t blk hdr IteratorId
toMock Model {..} (At t) = fmap (knownIters RE.!) t

-- | Step the mock semantics
--
-- We cannot step the whole Model here (see 'event', below)
step :: (TestConstraints blk hdr, Eq1 r)
     => Model  blk hdr m r
     -> At Cmd blk hdr m r
     -> (Resp  blk hdr IteratorId, DBModel blk)
step model@Model { modelEnv, dbModel } cmd =
    runPure (QSM.unOpaque modelEnv) (toMock model cmd) dbModel

{-------------------------------------------------------------------------------
  Wrapping in quickcheck-state-machine references
-------------------------------------------------------------------------------}

-- TODO we could instantiate blk and hdr to the right test block and header
-- throughout this whole module, so we can get rid of these two type
-- parameters. Possibly m too.

-- | Instantiate functor @t blk hdr@ to @t blk hdr ('IterRef' blk hdr m r)@.
--
-- Needed because we need to (partially) apply @'At' t blk hdr m@ to @r@.
newtype At t blk hdr m r = At { unAt :: t blk hdr (IterRef blk hdr m r) }
  deriving (Generic)

-- | Don't print the 'At' constructor.
instance Show (t blk hdr (IterRef blk hdr m r)) => Show (At t blk hdr m r) where
  show = show . unAt

deriving instance (TestConstraints blk hdr, Eq blk, Eq hdr, Eq1 r)
                => Eq (At Resp blk hdr m r)

deriving instance Generic1          (At Cmd blk hdr m)
deriving instance Rank2.Foldable    (At Cmd blk hdr m)
deriving instance Rank2.Functor     (At Cmd blk hdr m)
deriving instance Rank2.Traversable (At Cmd blk hdr m)

deriving instance Generic1          (At Resp blk hdr m)
deriving instance Rank2.Foldable    (At Resp blk hdr m)

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event blk hdr m r = Event
  { eventBefore   :: Model  blk hdr m r
  , eventCmd      :: At Cmd blk hdr m r
  , eventAfter    :: Model  blk hdr m r
  , eventMockResp :: Resp   blk hdr   IteratorId
  }

deriving instance (TestConstraints blk hdr, Show blk, Show hdr, Show1 r)
               => Show (Event blk hdr m r)

eventMockCmd :: Eq1 r => Event blk hdr m r -> Cmd blk hdr IteratorId
eventMockCmd Event {..} = toMock eventBefore eventCmd

-- | Construct an event
lockstep :: (TestConstraints blk hdr, Eq1 r, Show1 r)
         => Model     blk hdr m r
         -> At Cmd    blk hdr m r
         -> At Resp   blk hdr m r
         -> Event     blk hdr m r
lockstep model@Model {..} cmd (At resp) = Event
    { eventBefore   = model
    , eventCmd      = cmd
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model cmd
    newIters = RE.fromList $ zip (iters resp) (iters mockResp)
    model' = model
      { dbModel    = dbModel'
      , knownIters = knownIters `RE.union` newIters
      }


{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

-- | Generate a 'Cmd'
generator :: forall      blk hdr m.
             HasHeader   blk
          => Gen         blk
          -> Model       blk hdr m Symbolic
          -> Gen (At Cmd blk hdr m Symbolic)
generator genBlock Model {..} = At <$> frequency
    [ (3, AddBlock <$> genBlock)
    , (1, return GetCurrentChain)
    , (1, return GetCurrentLedger)
    , (1, return GetTipBlock)
      -- To check that we're on the right chain
    , (3, return GetTipPoint)
    , (2, GetBlock <$> genPoint)
    , (2, uncurry StreamBlocks <$> genBounds)
    , (if null iterators then 0 else 4, genIteratorNext)
    , (if null iterators then 0 else 1, genIteratorClose)
    ]
  where
    iterators :: [Reference (Opaque (Iterator m blk)) Symbolic]
    iterators = RE.keys knownIters

    genRandomPoint :: Gen (Point blk)
    genRandomPoint = undefined

    pointsInDB :: [Point blk]
    pointsInDB = Block.blockPoint <$> Map.elems (Model.blocks dbModel)

    empty :: Bool
    empty = null pointsInDB

    genPoint :: Gen (Point blk)
    genPoint = frequency
      [ (1, return genesisPoint)
      -- , (2, genRandomPoint) TODO
      , (if empty then 0 else 7, elements pointsInDB)
      ]

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
      end   <- elements pointsInDB `suchThat` ((> Block.pointSlot start) .
                                               Block.pointSlot)
      (,) <$> (genFromInEx <*> return start)
          <*> (genToInEx   <*> return end)

    genIteratorNext  = IteratorNext  <$> elements iterators
    genIteratorClose = IteratorClose <$> elements iterators

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Shrinker
shrinker :: Model   blk hdr m Symbolic
         ->  At Cmd blk hdr m Symbolic
         -> [At Cmd blk hdr m Symbolic]
shrinker _ = const [] -- TODO

{-------------------------------------------------------------------------------
  The final state machine
-------------------------------------------------------------------------------}

-- | Mock a response
--
-- We do this by running the pure semantics and then generating mock
-- references for any new handles.
mock :: (TestConstraints blk hdr, Typeable m)
     => Model            blk hdr m Symbolic
     ->         At Cmd   blk hdr m Symbolic
     -> GenSym (At Resp  blk hdr m Symbolic)
mock model cmd = At <$> traverse (const genSym) resp
  where
    (resp, _dbm) = step model cmd

precondition :: Model blk hdr m Symbolic -> At Cmd blk hdr m Symbolic -> Logic
precondition Model {..} (At cmd) =
   forall (iters cmd) (`elem` RE.keys knownIters)

transition :: (TestConstraints blk hdr, Show1 r, Eq1 r)
           => Model   blk hdr m r
           -> At Cmd  blk hdr m r
           -> At Resp blk hdr m r
           -> Model   blk hdr m r
transition model cmd = eventAfter . lockstep model cmd

postcondition :: (TestConstraints blk hdr, Eq blk, Eq hdr, Show blk, Show hdr)
              => Model   blk hdr m Concrete
              -> At Cmd  blk hdr m Concrete
              -> At Resp blk hdr m Concrete
              -> Logic
postcondition model cmd resp =
    (toMock (eventAfter ev) resp .== eventMockResp ev)
    .// "real response didn't match model response"
  where
    ev = lockstep model cmd resp

semantics :: forall blk hdr. TestConstraints blk hdr
          => ChainDB IO blk hdr
          -> At Cmd blk hdr IO Concrete
          -> IO (At Resp blk hdr IO Concrete)
semantics db (At cmd) =
  At . (fmap (QSM.reference . QSM.Opaque)) <$> runIO db (QSM.opaque <$> cmd)

-- | The state machine proper
sm :: ( TestConstraints     blk hdr
      , Eq                  blk
      , Eq                      hdr
      , Show                blk
      , Show                    hdr
      )
   => ChainDB IO            blk hdr
   -> Gen                   blk
   -> ModelEnv              blk hdr
   -> ExtLedgerState        blk
   -> StateMachine (Model   blk hdr IO)
                   (At Cmd  blk hdr IO)
                                    IO
                   (At Resp blk hdr IO)
sm db genBlock env initLedger = StateMachine
  { initModel     = initModel env initLedger
  , transition    = transition
  , precondition  = precondition
  , postcondition = postcondition
  , generator     = Just . generator genBlock
  , shrinker      = shrinker
  , semantics     = semantics db
  , mock          = mock
  , invariant     = Nothing
  , distribution  = Nothing
  }


{-------------------------------------------------------------------------------
  Required instances

  The 'ToExpr' constraints come from "Data.TreeDiff".
-------------------------------------------------------------------------------}

instance CommandNames (At Cmd blk hdr it) where
  cmdName (At cmd) = constrName cmd
  cmdNames (_ :: Proxy (At Cmd blk hdr it r)) =
    constrNames (Proxy @(Cmd blk hdr ()))

deriving instance Generic ReaderNext
deriving instance Generic IteratorId
deriving instance Generic (Point blk)
deriving instance Generic (Chain blk)
deriving instance Generic (ChainProducerState blk)
deriving instance Generic (ReaderState blk)
deriving instance Generic (ExtLedgerState blk)

deriving instance ( ToExpr (ChainState (BlockProtocol blk))
                  , ToExpr (LedgerState blk)
                  )
                 => ToExpr (ExtLedgerState blk)
deriving instance ToExpr BlockNo
deriving instance ToExpr SlotNo
deriving instance ToExpr ReaderNext
deriving instance ToExpr IteratorId
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
                 => ToExpr (Model blk hdr IO Concrete)

-- Blk and Hdr specific instances

deriving instance ToExpr TestHash
deriving instance ToExpr Blk
deriving instance ToExpr (LedgerState Blk)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

type Blk = TestBlock
type Hdr = TestBlock

prop_sequential :: Property
prop_sequential = forAllCommands smUnused Nothing $ \cmds -> QC.monadicIO $ do
    (hist, prop) <- test cmds
    -- TODO tags
    prettyCommands smUnused hist prop
  where
    test :: QSM.Commands (At Cmd Blk Hdr IO) (At Resp Blk Hdr IO)
         -> QC.PropertyM IO
            ( QSM.History (At Cmd Blk Hdr IO) (At Resp Blk Hdr IO)
            , Property
            )
    test cmds = do
      args <- QC.run $ mkArgs cfg initLedger
      db   <- QC.run $ openDB args
      let sm' = sm db genBlock env initLedger
      (hist, model, res) <- runCommands sm' cmds
      realChain <- QC.run $ chainHashes <$> toChain db
      QC.run $ closeDB db
      let modelChain = chainHashes $ Model.currentChain $ dbModel model
          prop =
            counterexample ("Real  chain: " <> show realChain)  $
            counterexample ("Model chain: " <> show modelChain) $
            res === Ok .&&. realChain === modelChain
      return (hist, prop)

    cfg :: NodeConfig (BlockProtocol Blk)
    cfg = singleNodeTestConfig

    initLedger :: ExtLedgerState Blk
    initLedger = testInitExtLedger

    env :: ModelEnv Blk Hdr
    env = ModelEnv { cfg = cfg, blockHeader = id }

    smUnused = sm dbUnused genBlock env initLedger

    -- TODO
    genBlock :: Gen Blk
    genBlock = do
      fork <- choose (1, 4)
      no   <- choose (1, 20)
      let prevHash
            | no == 1   = GenesisHash
            | otherwise = BlockHash (TestHash (fork * 1000 + no - 1))
      return TestBlock
        { tbHash     = TestHash (fork * 1000 + no)
        , tbPrevHash = prevHash
        , tbNo       = BlockNo no
        , tbSlot     = SlotNo no
        }

    -- Compare the hashes (with '===') so we get much more readable output
    chainHashes :: Chain Blk -> [Word64]
    chainHashes = map (unTestHash . tbHash) . chainToList

prop_parallel :: Property
prop_parallel = forAllParallelCommands smUnused $ \cmds -> QC.monadicIO $
    test cmds
  where
    test :: QSM.ParallelCommands (At Cmd Blk Hdr IO) (At Resp Blk Hdr IO)
         -> QC.PropertyM IO ()
    test cmds = do
      args <- QC.run $ mkArgs cfg initLedger
      db   <- QC.run $ openDB args
      let sm' = sm db genBlock env initLedger
      -- TODO 1 is not enough of course, but otherwise we end up running the
      -- same commands on the same database.
      -- IDEA: make sure that the first command is some kind of openDB call
      -- and the last command is some kind of closeDB call.
      prettyParallelCommands cmds =<< runParallelCommandsNTimes 1 sm' cmds
      QC.run $ closeDB db
      return ()

    cfg :: NodeConfig (BlockProtocol Blk)
    cfg = singleNodeTestConfig

    initLedger :: ExtLedgerState Blk
    initLedger = testInitExtLedger

    env :: ModelEnv Blk Hdr
    env = ModelEnv { cfg = cfg, blockHeader = id }

    smUnused = sm dbUnused genBlock env initLedger

    -- TODO
    genBlock :: Gen Blk
    genBlock = do
      fork <- choose (1, 4)
      no   <- choose (1, 20)
      let prevHash
            | no == 1   = GenesisHash
            | otherwise = BlockHash (TestHash (fork * 1000 + no - 1))
      return TestBlock
        { tbHash     = TestHash (fork * 1000 + no)
        , tbPrevHash = prevHash
        , tbNo       = BlockNo no
        , tbSlot     = SlotNo no
        }

mkArgs :: (MonadSTM m, MonadCatch m, MonadCatch (STM m))
       => NodeConfig (BlockProtocol Blk) -> ExtLedgerState Blk
       -> m (ChainDbArgs m Blk Hdr)
mkArgs cfg initLedger = do
    (immDbFsVar, volDbFsVar, lgrDbFsVar) <- atomically $
      (,,) <$> newTVar Mock.empty
           <*> newTVar Mock.empty
           <*> newTVar Mock.empty
    return ChainDbArgs
      { -- Decoders
        cdbDecodeHash = decode
      , cdbDecodeBlock = decode
      , cdbDecodeLedger = decode
      , cdbDecodeChainState = decode
        -- Encoders
      , cdbEncodeBlock      = encode
      , cdbEncodeHash       = encode
        -- Error handling
      , cdbErrImmDb         = EH.monadCatch
      , cdbErrVolDb         = EH.monadCatch
      , cdbErrVolDbSTM      = EH.throwCantCatch EH.monadCatch
        -- HasFS instances
      , cdbHasFSImmDb       = simHasFS EH.monadCatch immDbFsVar
      , cdbHasFSVolDb       = simHasFS EH.monadCatch volDbFsVar
      , cdbHasFSLgrDB       = simHasFS EH.monadCatch lgrDbFsVar

        -- Policy
      , cdbValidation       = ValidateAllEpochs
      , cdbBlocksPerFile    = 4
      , cdbMemPolicy        = defaultMemPolicy $ protocolSecurityParam cfg

        -- Integration
      , cdbNodeConfig       = cfg
      , cdbEpochSize        = const (return 10)
      , cdbIsEBB            = const Nothing -- TODO
      , cdbGetHeader        = id
      , cdbGenesis          = return initLedger
      }

dbUnused :: ChainDB blk hdr m
dbUnused = error "semantics and DB used during command generation"

tests :: TestTree
tests = testGroup "ChainDB q-s-m"
    [ testProperty "sequential" prop_sequential
    , testProperty "parallel"   prop_parallel
    ]

-- | Debugging utility: run some commands against the real implementation.
_runCmd :: [Cmd Blk Hdr ()] -> IO [Resp Blk Hdr ()]
_runCmd cmds = do
    args <- mkArgs singleNodeTestConfig testInitExtLedger
    db   <- openDB args
    mapM (go db) cmds
  where
    go :: ChainDB IO Blk Hdr -> Cmd Blk Hdr () -> IO (Resp Blk Hdr ())
    go db cmd = void . unAt <$> semantics db (At (noIter <$> cmd))

    noIter :: () -> IterRef Blk Hdr IO Concrete
    noIter = error "iterators no supported"

_mkBlk :: Word64 -> TestBlock
_mkBlk n = TestBlock
    { tbHash     = TestHash n
    , tbPrevHash = case no of
                     1 -> GenesisHash
                     _ -> BlockHash (TestHash (n - 1))
    , tbNo       = BlockNo no
    , tbSlot     = SlotNo no
   }
  where
    no = n `mod` 1000

{-------------------------------------------------------------------------------
  generics-sop auxiliary
-------------------------------------------------------------------------------}

cmdConstrInfo :: Proxy (Cmd blk hdr it)
              -> SOP.NP SOP.ConstructorInfo (SOP.Code (Cmd blk hdr it))
cmdConstrInfo = SOP.constructorInfo . SOP.datatypeInfo

constrName :: forall blk hdr it. Cmd blk hdr it -> String
constrName a =
    SOP.hcollapse $ SOP.hliftA2 go (cmdConstrInfo p) (SOP.unSOP (SOP.from a))
  where
    go :: SOP.ConstructorInfo a -> SOP.NP SOP.I a -> SOP.K String a
    go nfo _ = SOP.K $ SOP.constructorName nfo

    p = Proxy @(Cmd blk hdr it)

constrNames :: Proxy (Cmd blk hdr it) -> [String]
constrNames p =
    SOP.hcollapse $ SOP.hmap go (cmdConstrInfo p)
  where
    go :: SOP.ConstructorInfo a -> SOP.K String a
    go nfo = SOP.K $ SOP.constructorName nfo
