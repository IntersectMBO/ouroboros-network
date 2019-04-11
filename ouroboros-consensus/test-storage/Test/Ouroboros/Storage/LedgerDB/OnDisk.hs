{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.OnDisk (
    tests
  , showLabelledExamples
  ) where

import           Prelude hiding (elem)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import           Control.Monad.Except (Except, runExcept, throwError)
import           Control.Monad.State (StateT (..))
import qualified Control.Monad.State as State
import           Data.Bifunctor (first)
import           Data.Foldable (toList)
import           Data.Functor.Classes
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Proxy
import           Data.TreeDiff (ToExpr (..))
import           Data.Word
import           GHC.Generics (Generic)
import           System.IO (IOMode (..))
import           System.Random (getStdRandom, randomR)

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import qualified Test.QuickCheck.Random as QC
import           Test.StateMachine
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Util
import qualified Ouroboros.Consensus.Util.Classify as C

import           Ouroboros.Storage.FS.API
import qualified Ouroboros.Storage.FS.Sim.MockFS as MockFS
import           Ouroboros.Storage.FS.Sim.STM
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.InMemory
import           Ouroboros.Storage.LedgerDB.MemPolicy
import           Ouroboros.Storage.LedgerDB.Offsets
import           Ouroboros.Storage.LedgerDB.OnDisk

-- For the Arbitrary instance of 'MemPolicy'
import           Test.Ouroboros.Storage.LedgerDB.InMemory ()

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "OnDisk" [
      testProperty "LedgerSimple" $ prop_sequential (Proxy @'LedgerSimple)
    ]

{-------------------------------------------------------------------------------
  Modelling the ledger
-------------------------------------------------------------------------------}

-- | Type level description of the ledger model
data LedgerUnderTest = LedgerSimple

class ( Show      (BlockRef  t)
      , Ord       (BlockRef  t)
      , Serialise (BlockRef  t)
      , Show      (BlockVal  t)
      , Eq        (BlockVal  t)
      , ToExpr    (BlockVal  t)
      , Eq        (LedgerSt  t)
      , Show      (LedgerSt  t)
      , Serialise (LedgerSt  t)
      , ToExpr    (LedgerSt  t)
      , Eq        (LedgerErr t)
      , Show      (LedgerErr t)
      ) => LUT (t :: LedgerUnderTest) where
  data family LedgerSt  t :: *
  type family LedgerErr t :: *
  data family BlockVal  t :: *
  type family BlockRef  t :: *

  -- | Genesis value
  ledgerGenesis :: LedgerSt t

  -- | Apply ledger rules
  ledgerApply   :: BlockVal t -> LedgerSt t -> Either (LedgerErr t) (LedgerSt t)

  -- | Compute reference to a block
  blockRef      :: BlockVal t -> BlockRef t

  -- | Produce new block, given current ledger and tip
  genBlock      :: LedgerSt t -> Gen (BlockVal t)

refValPair :: LUT t => BlockVal t -> (BlockRef t, BlockVal t)
refValPair b = (blockRef b, b)

genBlocks :: LUT t => Word64 -> LedgerSt t -> Gen [BlockVal t]
genBlocks 0 _ = return []
genBlocks n l = do b <- genBlock l
                   case ledgerApply b l of
                     Left  _  -> error invalidBlock
                     Right l' -> do
                       bs <- genBlocks (n - 1) l'
                       return (b:bs)
  where
    invalidBlock = "genBlocks: genBlock produced invalid block"

type LedgerDbConf' m t = LedgerDbConf m (LedgerSt t) (BlockRef t) (BlockVal t) (LedgerErr t)
type LedgerDB'       t = LedgerDB       (LedgerSt t) (BlockRef t)
type StreamAPI'    m t = StreamAPI    m              (BlockRef t) (BlockVal t)
type NextBlock'      t = NextBlock                   (BlockRef t) (BlockVal t)
type BlockInfo'      t = BlockInfo                   (BlockRef t) (BlockVal t)
type Tip'            t = Tip                         (BlockRef t)

{-------------------------------------------------------------------------------
  Simple instantiation of LUT
-------------------------------------------------------------------------------}

instance LUT 'LedgerSimple where
  data LedgerSt 'LedgerSimple = SimpleLedger Int
    deriving (Show, Eq, Generic, Serialise, ToExpr)

  data BlockVal 'LedgerSimple = SimpleBlock Int
    deriving (Show, Eq, Generic, Serialise, ToExpr)

  type LedgerErr 'LedgerSimple = (Int, Int)
  type BlockRef  'LedgerSimple = Int

  ledgerGenesis :: LedgerSt 'LedgerSimple
  ledgerGenesis = SimpleLedger 0

  ledgerApply :: BlockVal 'LedgerSimple
              -> LedgerSt 'LedgerSimple
              -> Either (LedgerErr 'LedgerSimple) (LedgerSt 'LedgerSimple)
  ledgerApply (SimpleBlock b) (SimpleLedger l) =
      if b > l then Right (SimpleLedger b)
               else Left (b, l)

  blockRef :: BlockVal 'LedgerSimple -> BlockRef 'LedgerSimple
  blockRef (SimpleBlock b) = b

  genBlock :: LedgerSt 'LedgerSimple -> Gen (BlockVal 'LedgerSimple)
  genBlock (SimpleLedger l) = return $ SimpleBlock (l + 1)

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

data Corruption =
    -- | Delete the snapshot entirely
    Delete

    -- | Truncate the file
    --
    -- This is just a simple way to cause a deserialisation error
  | Truncate
  deriving (Show, Eq, Generic, ToExpr)

data Cmd (t :: LedgerUnderTest) ss =
    -- | Get the current ledger state
    Current

    -- | Push a block
  | Push (BlockVal t)

    -- | Switch to a fork
  | Switch Word64 [BlockVal t]

    -- | Take a snapshot (write to disk)
  | Snap

    -- | Restore the DB from on-disk, then return it
    --
    -- We could avoid returning the ledger from 'Restore', instead relying on
    -- 'Current' to do the check, but returning the ledger immediately means
    -- we spot problems sooner and makes tagging easier.
  | Restore

    -- | Corrupt a previously taken snapshot
  | Corrupt Corruption ss

    -- | Corruption of the chain
    --
    -- Chain corruption, no matter what form, always results in truncation. We
    -- model this as the number of blocks that got truncated from the end of the
    -- chain.
    --
    -- NOTE: Since this is modelling /disk/ corruption, and it is the
    -- responsibility of the 'ChainStateDB' to /notice/ disk corruption (by
    -- catching the appropriate exceptions), the assume that the ledger state
    -- will immediately be re-initialized after a 'Truncate' (which is precisely
    -- what the 'ChainStateDB' would do, after first doing recovery on the
    -- underlying 'ChainDB'). This is important because otherwise the model
    -- would diverge from the real thing.
    --
    -- Since 'Drop' therefore implies a 'Restore', we return the new ledger.
  | Drop Word64
  deriving (Functor, Foldable, Traversable)

data Success t ss =
    Unit ()
  | MaybeErr (Either (LedgerErr t) ())
  | Ledger (LedgerSt t)
  | Snapped ss
  deriving (Functor, Foldable, Traversable)

-- | Currently we don't have any error responses
newtype Resp t ss = Resp (Success t ss)
  deriving (Functor, Foldable, Traversable)

deriving instance (LUT t, Show ss) => Show (Cmd t ss)
deriving instance (LUT t, Eq   ss) => Eq   (Cmd t ss)

deriving instance (LUT t, Show ss) => Show (Success t ss)
deriving instance (LUT t, Eq   ss) => Eq   (Success t ss)

deriving instance (LUT t, Show ss) => Show (Resp t ss)
deriving instance (LUT t, Eq   ss) => Eq   (Resp t ss)

{-------------------------------------------------------------------------------
  Pure model
-------------------------------------------------------------------------------}

-- The mock ledger records the blocks and ledger values at each point
type MockLedger t = [(BlockVal t, LedgerSt t)]

-- | We simply enumerate snapshots
--
-- We only keep track of this to be able to give more meaningful statistics
-- about generated tests. The mock implementation doesn't actually " take "
-- any snapshots (instead it stores the legder state at each point).
newtype MockSnap = MockSnap Int
  deriving (Show, Eq, Ord, Generic, ToExpr)

-- | Mock implementation
--
-- The mock implementation simply records the ledger at every point.
-- We store the chain most recent first.
data Mock t = Mock {
      mockLedger :: MockLedger t
    , mockSnaps  :: Map MockSnap SnapState
    , mockNext   :: Int
    }
  deriving (Generic)

deriving instance LUT t => Show   (Mock t)
deriving instance LUT t => ToExpr (Mock t)

data SnapState = SnapOk | SnapCorrupted Corruption
  deriving (Show, Eq, Generic, ToExpr)

mockInit :: Mock t
mockInit = Mock [] Map.empty 1

mockCurrent :: LUT t => Mock t -> LedgerSt t
mockCurrent Mock{..} =
    case mockLedger of
      []       -> ledgerGenesis
      (_, l):_ -> l

mockChainLength :: Mock t -> Word64
mockChainLength Mock{..} = fromIntegral (length mockLedger)

mockRollback :: Word64 -> Mock t -> Mock t
mockRollback n mock@Mock{..} = mock {
      mockLedger = drop (fromIntegral n) mockLedger
    }

mockUpdateLedger :: StateT (MockLedger t) (Except (LedgerErr t)) a
                 -> Mock t -> (Either (LedgerErr t) a, Mock t)
mockUpdateLedger f mock =
    case runExcept (runStateT f (mockLedger mock)) of
      Left  err          -> (Left err, mock)
      Right (a, ledger') -> (Right a, mock { mockLedger = ledger' })

mockRecentSnap :: Mock t -> Maybe SnapState
mockRecentSnap Mock{..} =
    case Map.toDescList mockSnaps of
      []        -> Nothing
      (_, st):_ -> Just st

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

runMock :: forall t. LUT t
        => Cmd t MockSnap -> Mock t -> (Resp t MockSnap, Mock t)
runMock = first Resp .: go
  where
    go :: Cmd t MockSnap -> Mock t -> (Success t MockSnap, Mock t)
    go Current        = \mock -> (Ledger (cur (mockLedger mock)), mock)
    go (Push b)       = first MaybeErr . mockUpdateLedger (push b)
    go (Switch n bs)  = first MaybeErr . mockUpdateLedger (switch n bs)
    go Restore        = go Current
    go Snap           = \mock@Mock{..} -> (
          Snapped (MockSnap mockNext)
        , mock { mockNext  = mockNext + 1
               , mockSnaps = Map.insert (MockSnap mockNext) SnapOk mockSnaps
               }
        )
    go (Corrupt c ss) = \mock -> (
          Unit ()
        , mock { mockSnaps = Map.alter corrupt ss (mockSnaps mock) }
        )
      where
        -- Deletion trumps corruption
        corrupt :: Maybe SnapState -> Maybe SnapState
        corrupt old = Just $
          case (c, old) of
            (_        , Nothing    )               -> SnapCorrupted c
            (Delete   , _          )               -> SnapCorrupted Delete
            (Truncate , Just SnapOk)               -> SnapCorrupted Truncate
            (Truncate , (Just (SnapCorrupted c'))) -> SnapCorrupted c'
    go (Drop n) = \mock@Mock{..} ->
        go Restore $ mock { mockLedger = drop (fromIntegral n) mockLedger }

    push :: BlockVal t -> StateT (MockLedger t) (Except (LedgerErr t)) ()
    push b = do
        ls <- State.get
        case ledgerApply b (cur ls) of
          Left  err -> throwError err
          Right l'  -> State.put ((b, l'):ls)

    switch :: Word64
           -> [BlockVal t]
           -> StateT (MockLedger t) (Except (LedgerErr t)) ()
    switch n bs = do
        State.modify $ drop (fromIntegral n)
        mapM_ push bs

    cur :: MockLedger t -> LedgerSt t
    cur []         = ledgerGenesis
    cur ((_, l):_) = l

{-------------------------------------------------------------------------------
  Standalone instantiation of the ledger DB
-------------------------------------------------------------------------------}

-- | Arguments required by 'StandaloneDB'
data DbEnv m = forall fh. DbEnv {
      dbHasFS     :: HasFS m fh
    , dbMemPolicy :: MemPolicy
    }

-- | Standalone ledger DB
--
-- Under normal circumstances the ledger DB is maintained by the 'ChainStateDB',
-- and supported by the 'ChainDB'. In order to test it stand-alone we need to
-- mock these components.
data StandaloneDB m t = DB {
      -- | Arguments
      dbEnv    :: DbEnv m

      -- | Block storage
      --
      -- We can think of this as mocking the volatile DB. Blocks can be
      -- added to this without updating the rest of the state.
    , dbBlocks :: TVar m (Map (BlockRef t) (BlockVal t))

      -- | Current chain and corresponding ledger state
      --
      -- We can think of this as mocking the ChainStateDB, which must keep
      -- track of a current chain and keep the ledger DB in sync with it.
      --
      -- Invariant: all references @r@ here must be present in 'dbBlocks'.
    , dbState  :: TVar m ([BlockRef t], LedgerDB (LedgerSt t) (BlockRef t))
    }

initStandaloneDB :: (MonadSTM m, LUT t) => DbEnv m -> m (StandaloneDB m t)
initStandaloneDB dbEnv@DbEnv{..} = do
    dbBlocks <- atomically $ newTVar Map.empty
    dbState  <- atomically $ newTVar (initChain, initDB)
    return DB{..}
  where
    initChain = []
    initDB    = ledgerDbFromGenesis dbMemPolicy ledgerGenesis

dbConf :: forall m t. (MonadSTM m, LUT t)
       => StandaloneDB m t -> LedgerDbConf' m t
dbConf DB{..} = LedgerDbConf {..}
  where
    ledgerDbGenesis = return ledgerGenesis
    ledgerDbApply   = const  ledgerApply
    ledgerDbResolve = \r -> atomically $ getBlock r <$> readTVar dbBlocks

    getBlock :: BlockRef t -> Map (BlockRef t) (BlockVal t) -> BlockVal t
    getBlock = Map.findWithDefault (error blockNotFound)

    blockNotFound :: String
    blockNotFound = concat [
          "dbConf: "
        , "invariant violation: "
        , "block in dbChain not in dbBlocks, "
        , "or LedgerDB not re-initialized after chain truncation"
        ]

dbStreamAPI :: forall m t. (MonadSTM m, LUT t)
            => StandaloneDB m t -> StreamAPI' m t
dbStreamAPI DB{..} = StreamAPI {..}
  where
    streamAfter :: Tip' t -> (Maybe (m (NextBlock' t)) -> m a) -> m a
    streamAfter tip k = do
        rs       <- atomically $ reverse . fst <$> readTVar dbState
        toStream <- case tip of
                      TipGen -> do atomically $ Just <$> newTVar rs
                      Tip r  -> case dropWhile (/= r) rs of
                                   []    -> return Nothing
                                   _:rs' -> atomically $ Just <$> newTVar rs'
        k (getNext <$> toStream)

    getNext :: TVar m [BlockRef t] -> m (NextBlock' t)
    getNext toStream = do
        mr <- atomically $ do
                rs <- readTVar toStream
                case rs of
                  []    -> return Nothing
                  r:rs' -> writeTVar toStream rs' >> return (Just r)
        case mr of
          Nothing -> return NoMoreBlocks
          Just r  -> do mb <- atomically $ Map.lookup r <$> readTVar dbBlocks
                        case mb of
                          Just b  -> return $ NextBlock (r, b)
                          Nothing -> error blockNotFound

    blockNotFound :: String
    blockNotFound = concat [
          "dbStreamAPI: "
        , "invariant violation: "
        , "block in dbChain not present in dbBlocks"
        ]

runDB :: forall m t.
         (MonadSTM m, MonadThrow m, MonadST m, LUT t)
      => StandaloneDB m t
      -> Cmd t DiskSnapshot -> m (Resp t DiskSnapshot)
runDB standalone@DB{..} cmd =
    case dbEnv of
      DbEnv{dbHasFS} -> Resp <$> go dbHasFS cmd
  where
    conf      = dbConf      standalone
    streamAPI = dbStreamAPI standalone

    go :: HasFS m fh -> Cmd t DiskSnapshot -> m (Success t DiskSnapshot)
    go _ Current =
        atomically $ (Ledger . ledgerDbCurrent . snd) <$> readTVar dbState
    go _ (Push b) = do
        atomically $ modifyTVar dbBlocks $
          uncurry Map.insert (refValPair b)
        upd (push b) $ ledgerDbPush conf (new b)
    go _ (Switch n bs) = do
        atomically $ modifyTVar dbBlocks $
          repeatedly (uncurry Map.insert) (map refValPair bs)
        upd (switch n bs) $ ledgerDbSwitch conf n (map new bs)
    go hasFS Snap = do
        (_, db) <- atomically $ readTVar dbState
        Snapped <$> takeSnapshot hasFS S.encode S.encode db
    go hasFS Restore = do
        db <- initLedgerDB
                hasFS
                S.decode
                S.decode
                (dbMemPolicy dbEnv)
                conf
                streamAPI
        atomically $ modifyTVar dbState (\(rs, _) -> (rs, db))
        go hasFS Current
    go hasFS (Corrupt c ss) =
        EH.catchError (hasFsErr hasFS)
          (case c of
             Delete   -> Unit <$> deleteSnapshot   hasFS ss
             Truncate -> Unit <$> truncateSnapshot hasFS ss)
          (\_ -> return $ Unit()) -- ignore any errors during corruption
    go hasFS (Drop n) = do
        -- During recovery the ChainStateDB would ask the ChainDB to recover
        -- and pick a new current chain; only once that is done would it
        -- compute a new ledger state. During this process the ChainStateDB
        -- would effectively be closed.
        atomically $ do
            (rs, _db) <- readTVar dbState
            writeTVar dbState (drop (fromIntegral n) rs, error "ledger DB not initialized")
        go hasFS Restore

    push :: BlockVal t -> [BlockRef t] -> [BlockRef t]
    push b = (blockRef b:)

    switch :: Word64 -> [BlockVal t] -> [BlockRef t] -> [BlockRef t]
    switch 0 bs = (reverse (map blockRef bs) ++)
    switch n bs = switch 0 bs . drop (fromIntegral n)

    new :: BlockVal t -> BlockInfo' t
    new = BlockVal NotPrevApplied . refValPair

    upd :: ([BlockRef t] -> [BlockRef t])
        -> (LedgerDB' t -> m (Either (LedgerErr t) (LedgerDB' t)))
        -> m (Success t DiskSnapshot)
    upd f g = do
        -- We cannot run the whole thing in a transaction, since computing the
        -- new value of the ledger DB may require reading from the chain DB
        (rs, db) <- atomically $ readTVar dbState
        mDB'     <- g db
        case mDB' of
          Left  e   -> return $ MaybeErr (Left e)
          Right db' -> do atomically $ writeTVar dbState (f rs, db')
                          return $ MaybeErr (Right ())

    truncateSnapshot :: HasFS m fh -> DiskSnapshot -> m ()
    truncateSnapshot hasFS@HasFS{..} ss =
        withFile hasFS (snapshotToPath ss) AppendMode $ \h ->
          hTruncate h 0

{-------------------------------------------------------------------------------
  References
-------------------------------------------------------------------------------}

newtype At f r = At (f (Reference DiskSnapshot r))
type    f :@ r = At f r

deriving instance Show (f (Reference DiskSnapshot r)) => Show (At f r)

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

type SnapRefs r = [(Reference DiskSnapshot r, MockSnap)]

(!) :: Eq r => [(r, a)] -> r -> a
env ! r = fromJust (lookup r env)

data Model t r = Model {
    modelMock  :: Mock t
  , modelSnaps :: SnapRefs r
  }
  deriving (Generic)

deriving instance (Show1 r, LUT t) => Show (Model t r)

initModel :: Model t r
initModel = Model mockInit []

toMock :: (Functor f, Eq1 r) => Model t r -> f :@ r -> f MockSnap
toMock m (At fr) = (modelSnaps m !) <$> fr

step :: (Eq1 r, LUT t) => Model t r -> Cmd t :@ r -> (Resp t MockSnap, Mock t)
step m cmd = runMock (toMock m cmd) (modelMock m)

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event t r = Event {
      eventBefore   :: Model t    r
    , eventCmd      :: Cmd   t :@ r
    , eventResp     :: Resp  t :@ r
    , eventAfter    :: Model t    r
    , eventMockResp :: Resp  t MockSnap
    }

deriving instance (LUT t, Show1 r) => Show (Event t r)

lockstep :: (Eq1 r, LUT t)
         => Model t    r
         -> Cmd   t :@ r
         -> Resp  t :@ r
         -> Event t    r
lockstep m@(Model _ hs) cmd (At resp) = Event {
      eventBefore   = m
    , eventCmd      = cmd
    , eventResp     = At resp
    , eventAfter    = Model mock' (hs' <> hs) -- new references override old!
    , eventMockResp = resp'
    }
  where
    (resp', mock') = step m cmd
    hs' = zip (toList resp) (toList resp')

execCmd :: LUT t
        => Model t Symbolic
        -> QSM.Command (At (Cmd t)) (At (Resp t))
        -> Event t Symbolic
execCmd model (QSM.Command cmd resp _vars) = lockstep model cmd resp

execCmds :: forall t. LUT t
         => QSM.Commands (At (Cmd t)) (At (Resp t))
         -> [Event t Symbolic]
execCmds = \(QSM.Commands cs) -> go initModel cs
  where
    go :: Model t Symbolic
       -> [QSM.Command (At (Cmd t)) (At (Resp t))]
       -> [Event t Symbolic]
    go _ []     = []
    go m (c:cs) = e : go (eventAfter e) cs
      where
        e = execCmd m c

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

generator :: forall t. LUT t
          => MemPolicy -> Model t Symbolic -> Maybe (Gen (Cmd t :@ Symbolic))
generator memPolicy (Model mock hs) = Just $ QC.oneof $ concat [
      withoutRef
    , withRef
    ]
  where
    withoutRef :: [Gen (Cmd t :@ Symbolic)]
    withoutRef = [
          fmap At $ return Current
        , fmap At $ Push <$> genBlock (mockCurrent mock)
        , fmap At $ do
            let maxRollback = minimum [
                    mockChainLength mock
                  , memPolicyMaxRollback memPolicy
                  ]
            numRollback  <- QC.choose (0, maxRollback)
            numNewBlocks <- QC.choose (numRollback, numRollback + 2)
            let afterRollback = mockRollback numRollback mock
            Switch numRollback <$> genBlocks numNewBlocks (mockCurrent afterRollback)
        , fmap At $ return Snap
        , fmap At $ return Restore
        , fmap At $ (Drop . fromIntegral) <$> QC.choose (0, mockChainLength mock)
        ]

    withRef :: [Gen (Cmd t :@ Symbolic)]
    withRef = [
          fmap At $ Corrupt <$> genCorruption <*> genSnapshot
        ]

    genCorruption :: Gen Corruption
    genCorruption = QC.elements [Delete, Truncate]

    genSnapshot :: Gen (Reference DiskSnapshot Symbolic)
    genSnapshot = QC.elements (map fst hs)

shrinker :: Model t Symbolic -> Cmd t :@ Symbolic -> [Cmd t :@ Symbolic]
shrinker _ _ = []

{-------------------------------------------------------------------------------
  Additional type class instances required by QSM
-------------------------------------------------------------------------------}

instance CommandNames (At (Cmd t)) where
  cmdName (At Current{}) = "Current"
  cmdName (At Push{})    = "Push"
  cmdName (At Switch{})  = "Switch"
  cmdName (At Snap{})    = "Snap"
  cmdName (At Restore{}) = "Restore"
  cmdName (At Corrupt{}) = "Corrupt"
  cmdName (At Drop{})    = "Drop"

  cmdNames _ = [
      "Current"
    , "Push"
    , "Switch"
    , "Snap"
    , "Restore"
    , "Corrupt"
    , "Drop"
    ]

instance Functor f => Rank2.Functor (At f) where
  fmap = \f (At x) -> At $ fmap (lift f) x
    where
      lift :: (r x -> r' x) -> QSM.Reference x r -> QSM.Reference x r'
      lift f (QSM.Reference x) = QSM.Reference (f x)

instance Foldable f => Rank2.Foldable (At f) where
  foldMap = \f (At x) -> foldMap (lift f) x
    where
      lift :: (r x -> m) -> QSM.Reference x r -> m
      lift f (QSM.Reference x) = f x

instance Traversable t => Rank2.Traversable (At t) where
  traverse = \f (At x) -> At <$> traverse (lift f) x
    where
      lift :: Functor f
           => (r x -> f (r' x)) -> QSM.Reference x r -> f (QSM.Reference x r')
      lift f (QSM.Reference x) = QSM.Reference <$> f x

instance LUT t => ToExpr (Model t Concrete)

deriving instance ToExpr DiskSnapshot

{-------------------------------------------------------------------------------
  Final state machine
-------------------------------------------------------------------------------}

semantics :: (MonadSTM m, MonadThrow m, MonadST m, LUT t)
          => StandaloneDB m t
          -> Cmd t :@ Concrete -> m (Resp t :@ Concrete)
semantics db (At cmd) = (At . fmap reference) <$> runDB db (concrete <$> cmd)

transition :: (Eq1 r, LUT t)
           => Model t    r
           -> Cmd   t :@ r
           -> Resp  t :@ r
           -> Model t    r
transition m cmd = eventAfter . lockstep m cmd

postcondition :: LUT t
              => Model t    Concrete
              -> Cmd   t :@ Concrete
              -> Resp  t :@ Concrete
              -> Logic
postcondition m cmd r = toMock (eventAfter e) r .== eventMockResp e
  where
    e = lockstep m cmd r

precondition :: Model t Symbolic -> Cmd t :@ Symbolic -> Logic
precondition (Model mock hs) (At c) =
        forall (toList c) (`elem` map fst hs)
    .&& validCmd c
  where
    validCmd :: Cmd t ss -> Logic
    validCmd (Switch n _) = n .<= mockChainLength mock
    validCmd _otherwise   = Top

symbolicResp :: LUT t
             => Model        t    Symbolic
             -> Cmd          t :@ Symbolic
             -> GenSym (Resp t :@ Symbolic)
symbolicResp m c = At <$> traverse (const genSym) resp
  where
    (resp, _mock') = step m c

sm :: (MonadSTM m, MonadThrow m, MonadST m, LUT t)
   => MemPolicy
   -> StandaloneDB m t
   -> StateMachine (Model t) (At (Cmd t)) m (At (Resp t))
sm memPolicy db = StateMachine {
      initModel     = initModel
    , transition    = transition
    , precondition  = precondition
    , postcondition = postcondition
    , invariant     = Nothing
    , generator     = generator memPolicy
    , distribution  = Nothing
    , shrinker      = shrinker
    , semantics     = semantics db
    , mock          = symbolicResp
    }

prop_sequential :: forall t. LUT t => Proxy t -> MemPolicy -> QC.Property
prop_sequential p memPolicy =
    QC.collect (tagMemPolicy memPolicy) $
      forAllCommands (sm memPolicy (dbUnused p)) Nothing $ \cmds ->
        QC.monadicIO (prop cmds)
  where
    k = memPolicyMaxRollback memPolicy

    -- Ideally we'd like to use @SimM s@ instead of IO, but unfortunately
    -- QSM requires monads that implement MonadIO.
    prop :: QSM.Commands (At (Cmd t)) (At (Resp t))
         -> QC.PropertyM IO ()
    prop cmds = do
      fs <- QC.run $ atomically $ newTVar MockFS.empty
      let dbEnv :: DbEnv IO
          dbEnv = DbEnv (simHasFS EH.exceptions fs) memPolicy
      db <- QC.run $ initStandaloneDB dbEnv
      let sm' = sm memPolicy db
      (hist, _model, res) <- runCommands sm' cmds
      prettyCommands sm' hist
        $ QC.tabulate "Tags" (map show $ tagEvents k (execCmds cmds))
        $ res QC.=== Ok

dbUnused :: Proxy t -> StandaloneDB IO t
dbUnused = error "DB unused during command generation"

{-------------------------------------------------------------------------------
  Labelling of the mem policy
-------------------------------------------------------------------------------}

data TagMemPolicy =
     -- | Otherwise we record the maximum offset
    MemPolicyMaxOffset (Range Word64)
  deriving (Show)

tagMemPolicy :: MemPolicy -> TagMemPolicy
tagMemPolicy = go . offsetsDropValues . memPolicyToOffsets
  where
    go :: [Word64] -> TagMemPolicy
    go os = MemPolicyMaxOffset $ range maxBound (maximum os)

{-------------------------------------------------------------------------------
  Event labelling

  TODO: We need at least a label for restore-after-corruption
-------------------------------------------------------------------------------}

data Tag =
    -- | Restore
    --
    -- We record the length of the chain at the time of the restore (to the
    -- closest power of two) as well as the state of the most recent snapshot,
    -- if any has been created.
    --
    -- We will look for the /maximum/ chain length in each case.
    TagRestore (Maybe SnapState) (Range Word64)

    -- | Tag rollback
    --
    -- We record the rollback length
  | TagMaxRollback (Range Word64)

    -- | Tag chain truncation
    --
    -- We record how many blocks were dropped
  | TagMaxDrop (Range Word64)
  deriving (Show, Eq)

type EventPred t = C.Predicate (Event t Symbolic) Tag

tagEvents :: forall t. Word64 -> [Event t Symbolic] -> [Tag]
tagEvents k = C.classify [
      tagMaxRollback
    , tagMaxDrop
    , tagRestore Nothing
    , tagRestore (Just SnapOk)
    , tagRestore (Just (SnapCorrupted Truncate))
    , tagRestore (Just (SnapCorrupted Delete))
    ]
  where
    tagMaxRollback :: EventPred t
    tagMaxRollback =
        fmap (TagMaxRollback . range k) $ C.maximum $ \ev ->
          case eventCmd ev of
            At (Switch n _) -> Just n
            _otherwise      -> Nothing

    tagMaxDrop :: EventPred t
    tagMaxDrop =
        fmap (TagMaxDrop . range k) $ C.maximum $ \ev ->
          case eventCmd ev of
            At (Drop n) -> Just n
            _otherwise  -> Nothing

    tagRestore :: Maybe SnapState -> EventPred t
    tagRestore mST =
        fmap (TagRestore mST . range k) $ C.maximum $ \ev ->
          let mock = modelMock (eventBefore ev) in
          case eventCmd ev of
            At Restore | mockRecentSnap mock == mST -> Just (mockChainLength mock)
            _otherwise                              -> Nothing

{-------------------------------------------------------------------------------
  Inspecting the labelling function
-------------------------------------------------------------------------------}

showLabelledExamples :: MemPolicy
                     -> Maybe Int
                     -> (Tag -> Bool) -- ^ Which tag are we interested in?
                     -> IO ()
showLabelledExamples memPolicy mReplay relevant = do
    replaySeed <- case mReplay of
                    Nothing   -> getStdRandom $ randomR (1, 999999)
                    Just seed -> return seed

    putStrLn $ "Using replaySeed " ++ show replaySeed

    let args = QC.stdArgs {
            QC.maxSuccess = 10000
          , QC.replay     = Just (QC.mkQCGen replaySeed, 0)
          }

    QC.labelledExamplesWith args $
      forAllCommands (sm memPolicy (dbUnused p)) Nothing $ \cmds ->
        repeatedly QC.collect (filter relevant . tagEvents k . execCmds $ cmds) $
          QC.property True
  where
    k = memPolicyMaxRollback memPolicy
    p = Proxy @'LedgerSimple


{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data Range n = R_Eq n | R_Btwn (n, n) | R_Gt n | R_Gt_k
  deriving (Show, Eq)

range :: (Ord n, Show n, Num n) => n -> n -> Range n
range k n
  | n > k           = R_Gt_k
  | n > limit       = R_Gt limit
  | n `L.elem` vals = R_Eq n
  | otherwise       =
      case L.find (\(lo, hi) -> lo <= n && n <= hi) rngs of
        Nothing  -> error $ "range: unable to classify " ++ show n
        Just rng -> R_Btwn rng
  where
    vals  = [0, 1, 2, 3, 4]
    rngs  = [(5, 10), (11, 20)]
    limit = 20
