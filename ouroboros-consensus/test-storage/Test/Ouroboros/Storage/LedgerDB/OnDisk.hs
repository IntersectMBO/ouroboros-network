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
{-# LANGUAGE TupleSections             #-}
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
import           Data.Typeable (Typeable)
import           Data.Word
import           GHC.Generics (Generic)
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

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Util
import qualified Ouroboros.Consensus.Util.Classify as C

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
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

import           Test.Util.Range

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
      , ToExpr    (BlockRef  t)
      , Typeable  (BlockRef  t)
      , Show      (BlockVal  t)
      , Eq        (BlockVal  t)
      , ToExpr    (BlockVal  t)
      , Eq        (LedgerSt  t)
      , Show      (LedgerSt  t)
      , Serialise (LedgerSt  t)
      , ToExpr    (LedgerSt  t)
      , Eq        (LedgerErr t)
      , Show      (LedgerErr t)
      , Typeable t
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
type BlockInfo'      t = (Apply 'False,     RefOrVal (BlockRef t) (BlockVal t))
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

    -- | Restore the DB from on-disk, then return it along with the init log
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
  | Snapped (ss, Tip' t)
  | Restored (MockInitLog t ss, LedgerSt t)
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

-- The mock ledger records the blocks and ledger values (new to old)
type MockLedger t = [(BlockVal t, LedgerSt t)]

-- | We simply enumerate snapshots
--
-- We only keep track of this to be able to give more meaningful statistics
-- about generated tests. The mock implementation doesn't actually " take "
-- any snapshots (instead it stores the legder state at each point).
newtype MockSnap = MockSnap Int
  deriving (Show, Eq, Ord, Generic, ToExpr)

-- | State of all snapshots on disk
--
-- In addition to the state of the snapshot we also record the tip of the chain
-- at the time we took the snapshot; this is important for 'mockMaxRollback'.
type MockSnaps t = Map MockSnap (Tip' t, SnapState)

-- | Mock implementation
--
-- The mock implementation simply records the ledger at every point.
-- We store the chain most recent first.
data Mock t = Mock {
      -- | Current ledger
      mockLedger  :: MockLedger t

      -- | Current state the snapshots
    , mockSnaps   :: MockSnaps t

      -- | The oldest (tail) block in the real DB at the most recent restore
      --
      -- This puts a limit on how far we can roll back.
      -- See also 'applyMockLog', 'mockMaxRollback'.
    , mockRestore :: Tip' t

      -- | Counter to assign 'MockSnap's
    , mockNext    :: Int

      -- | Memory policy
      --
      -- We need the memory policy only to compute which snapshots the real
      -- implementation would take, so that we can accurately predict how far
      -- the real implementation can roll back.
    , mockPolicy  :: MemPolicy
    }
  deriving (Generic)

deriving instance LUT t => Show   (Mock t)
deriving instance LUT t => ToExpr (Mock t)

data SnapState = SnapOk | SnapCorrupted Corruption
  deriving (Show, Eq, Generic, ToExpr)

mockInit :: MemPolicy -> Mock t
mockInit = Mock [] Map.empty TipGen 1

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
      []             -> Nothing
      (_, (_, st)):_ -> Just st

{-------------------------------------------------------------------------------
  Modelling restoration

  Although the mock implementation itself is not affected by disk failures
  (in fact, the concept makes no sense, since we don't store anything on disk),
  we /do/ need to be able to accurately predict how the real DB will be
  initialized (from which snapshot); this is important, because this dictates
  how far the real DB can roll back.
-------------------------------------------------------------------------------}

data MockInitLog t ss =
    MockFromGenesis
  | MockFromSnapshot ss (Tip' t)
  | MockReadFailure  ss          (MockInitLog t ss)
  | MockTooRecent    ss (Tip' t) (MockInitLog t ss)
  deriving (Functor, Foldable, Traversable)

deriving instance (LUT t, Show ss) => Show (MockInitLog t ss)
deriving instance (LUT t, Eq   ss) => Eq   (MockInitLog t ss)

fromInitLog :: InitLog (BlockRef t) -> MockInitLog t DiskSnapshot
fromInitLog  InitFromGenesis          = MockFromGenesis
fromInitLog (InitFromSnapshot ss tip) = MockFromSnapshot ss tip
fromInitLog (InitFailure ss err log') =
    case err of
      InitFailureRead _err     -> MockReadFailure ss     (fromInitLog log')
      InitFailureTooRecent tip -> MockTooRecent   ss tip (fromInitLog log')

mockInitLog :: forall t. LUT t => Mock t -> MockInitLog t MockSnap
mockInitLog Mock{..} = go (Map.toDescList mockSnaps)
  where
    go :: [(MockSnap, (Tip' t, SnapState))] -> MockInitLog t MockSnap
    go []                         = MockFromGenesis
    go ((snap, (mr, state)):snaps) =
        case (state, mr) of
          (SnapCorrupted Delete, _) ->
            -- The real DB won't even see deleted snapshots
            go snaps
          (SnapCorrupted Truncate, _) ->
            -- If it's truncated, it will skip it
            MockReadFailure snap $ go snaps
          (SnapOk, TipGen) ->
            -- Took Snapshot at genesis: definitely useable
            MockFromSnapshot snap mr
          (SnapOk, Tip r) ->
            if onChain r
              then MockFromSnapshot snap mr
              else MockTooRecent    snap mr $ go snaps

    onChain :: BlockRef t -> Bool
    onChain r = any (\(b, _l) -> blockRef b == r) mockLedger

applyMockLog :: forall t. MockInitLog t MockSnap -> Mock t -> Mock t
applyMockLog = go
  where
    go :: MockInitLog t MockSnap -> Mock t -> Mock t
    go  MockFromGenesis             mock = mock { mockRestore = TipGen }
    go (MockFromSnapshot _  tip)    mock = mock { mockRestore = tip    }
    go (MockReadFailure  ss   log') mock = go log' $ deleteSnap ss mock
    go (MockTooRecent    ss _ log') mock = go log' $ deleteSnap ss mock

    deleteSnap :: MockSnap -> Mock t -> Mock t
    deleteSnap ss mock = mock {
          mockSnaps = Map.alter setIsDeleted ss (mockSnaps mock)
        }

    setIsDeleted :: Maybe (Tip (BlockRef t), SnapState)
                 -> Maybe (Tip (BlockRef t), SnapState)
    setIsDeleted Nothing         = error "setIsDeleted: impossible"
    setIsDeleted (Just (tip, _)) = Just (tip, SnapCorrupted Delete)


-- | Compute theretical maximum rollback
--
-- The actual maximum rollback will be restricted by the memory policy.
mockMaxRollback :: forall t. LUT t => Mock t -> Word64
mockMaxRollback Mock{..} = go mockLedger
  where
    go :: MockLedger t -> Word64
    go ((b, _l):bs)
      | Tip (blockRef b) == mockRestore = 0
      | otherwise                       = 1 + go bs
    go []                               = 0

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

runMock :: forall t. LUT t
        => Cmd t MockSnap -> Mock t -> (Resp t MockSnap, Mock t)
runMock = first Resp .: go
  where
    go :: Cmd t MockSnap -> Mock t -> (Success t MockSnap, Mock t)
    go Current       mock = (Ledger (cur (mockLedger mock)), mock)
    go (Push b)      mock = first MaybeErr $ mockUpdateLedger (push b)      mock
    go (Switch n bs) mock = first MaybeErr $ mockUpdateLedger (switch n bs) mock
    go Restore       mock = (Restored (initLog, cur (mockLedger mock')), mock')
      where
        initLog = mockInitLog mock
        mock'   = applyMockLog initLog mock
    go Snap          mock = (
          Snapped (MockSnap (mockNext mock), snapped)
        , mock { mockNext  = mockNext mock + 1
               , mockSnaps = Map.insert (MockSnap (mockNext mock))
                                        (snapped, SnapOk)
                                        (mockSnaps mock)
               }
        )
      where
        -- The snapshot that the real implementation will write to disk
        --
        -- When we write a snapshot, we write the ledger state @k@ blocks back,
        -- since we only want to write immutable ledger states
        snapped :: Tip' t
        snapped =
            case drop (fromIntegral k) (mockLedger mock) of
              []       -> TipGen
              (b, _):_ -> Tip (blockRef b)
          where
            k = min (memPolicyMaxRollback (mockPolicy mock))
                    (mockMaxRollback mock)
    go (Corrupt c ss) mock = (
          Unit ()
        , mock { mockSnaps = Map.alter corrupt ss (mockSnaps mock) }
        )
      where
        corrupt :: Maybe (Tip' t, SnapState)
                -> Maybe (Tip' t, SnapState)
        corrupt Nothing         = error "corrupt: impossible"
        corrupt (Just (ref, _)) = Just $ (ref, SnapCorrupted c)
    go (Drop n) mock =
        go Restore $ mock {
            mockLedger = drop (fromIntegral n) (mockLedger mock)
          }

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
    ldbConfGenesis = return ledgerGenesis
    ldbConfApply   = ledgerApply
    ldbConfReapply = \b l -> case ledgerApply b l of
                               Left err -> error $ unexpectedLedgerError err
                               Right l' -> l'
    ldbConfResolve = \r -> atomically $ getBlock r <$> readTVar dbBlocks

    getBlock :: BlockRef t -> Map (BlockRef t) (BlockVal t) -> BlockVal t
    getBlock = Map.findWithDefault (error blockNotFound)

    blockNotFound :: String
    blockNotFound = concat [
          "dbConf: "
        , "invariant violation: "
        , "block in dbChain not in dbBlocks, "
        , "or LedgerDB not re-initialized after chain truncation"
        ]

    unexpectedLedgerError :: Show e => e -> String
    unexpectedLedgerError err = concat [
          "dbConf: "
        , "unexpected ledger state error in ldbConfReapply: "
        , show err
        ]

dbStreamAPI :: forall m t. (MonadSTM m, LUT t)
            => StandaloneDB m t -> StreamAPI' m t
dbStreamAPI DB{..} = StreamAPI {..}
  where
    streamAfter :: Tip' t -> (Maybe (m (NextBlock' t)) -> m a) -> m a
    streamAfter tip k = do
        rs <- atomically $ reverse . fst <$> readTVar dbState
        if unknownBlock tip rs
          then k Nothing
          else do
            toStream <- atomically $ newTVar (blocksToStream tip rs)
            k (Just (getNext toStream))

    -- Ignore requests to start streaming from blocks not on the current chain
    unknownBlock :: Tip' t -> [BlockRef t] -> Bool
    unknownBlock TipGen  _  = False
    unknownBlock (Tip r) rs = r `L.notElem` rs

    -- Blocks to stream
    --
    -- Precondition: tip must be on the current chain
    blocksToStream :: Tip' t -> [BlockRef t] -> [BlockRef t]
    blocksToStream TipGen  = id
    blocksToStream (Tip r) = tail . dropWhile (/= r)

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
        (initLog, db) <- initLedgerDB
                           hasFS
                           S.decode
                           S.decode
                           (dbMemPolicy dbEnv)
                           conf
                           streamAPI
        atomically $ modifyTVar dbState (\(rs, _) -> (rs, db))
        return $ Restored (fromInitLog initLog, ledgerDbCurrent db)
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
    new b = (Apply, uncurry Val (refValPair b))

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
        withFile hasFS (snapshotToPath ss) (AppendMode AllowExisting) $ \h ->
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

initModel :: MemPolicy -> Model t r
initModel memPolicy = Model (mockInit memPolicy) []

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
         => MemPolicy
         -> QSM.Commands (At (Cmd t)) (At (Resp t))
         -> [Event t Symbolic]
execCmds memPolicy = \(QSM.Commands cs) -> go (initModel memPolicy) cs
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
    , if null possibleCorruptions
        then []
        else [(At . uncurry Corrupt) <$> QC.elements possibleCorruptions]
    ]
  where
    withoutRef :: [Gen (Cmd t :@ Symbolic)]
    withoutRef = [
          fmap At $ return Current
        , fmap At $ Push <$> genBlock (mockCurrent mock)
        , fmap At $ do
            let maxRollback = minimum [
                    mockMaxRollback mock
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

    possibleCorruptions :: [(Corruption, Reference DiskSnapshot Symbolic)]
    possibleCorruptions = concatMap aux hs
      where
        aux :: (Reference DiskSnapshot Symbolic, MockSnap)
            -> [(Corruption, Reference DiskSnapshot Symbolic)]
        aux (diskSnap, mockSnap) =
            case Map.lookup mockSnap (mockSnaps mock) of
              Just (_tip, state) ->
                map (, diskSnap) $ possibleCorruptionsInState state
              Nothing ->
                error "possibleCorruptions: impossible"

    possibleCorruptionsInState :: SnapState -> [Corruption]
    possibleCorruptionsInState SnapOk                   = [Delete, Truncate]
    possibleCorruptionsInState (SnapCorrupted Truncate) = [Delete]
    possibleCorruptionsInState (SnapCorrupted Delete)   = []

shrinker :: Model t Symbolic -> Cmd t :@ Symbolic -> [Cmd t :@ Symbolic]
shrinker _ (At cmd) =
    case cmd of
      Current      -> []
      Push _b      -> []
      Snap         -> []
      Restore      -> []
      Switch 0 [b] -> [At $ Push b]
      Switch n bs  -> if length bs > fromIntegral n
                        then [At $ Switch n (init bs)]
                        else []
      -- an absent snapshot is easier than a corrupted one
      Corrupt c ss -> case c of
                        Truncate -> [At $ Corrupt Delete ss]
                        Delete   -> []
      Drop n       -> At . Drop <$> QC.shrink n

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
instance ToExpr a => ToExpr (Tip a)
instance ToExpr MemPolicy

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

precondition :: LUT t => Model t Symbolic -> Cmd t :@ Symbolic -> Logic
precondition (Model mock hs) (At c) =
        forall (toList c) (`elem` map fst hs)
    .&& validCmd c
  where
    -- Maximum rollback might decrease if shrinking removed blocks
    validCmd :: Cmd t ss -> Logic
    validCmd (Switch n _) = n .<= mockMaxRollback mock
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
      initModel     = initModel memPolicy
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

prop_sequential :: LUT t => Proxy t -> MemPolicy -> QC.Property
prop_sequential p memPolicy =
    QC.collect (tagMemPolicy memPolicy) $
      forAllCommands (sm memPolicy (dbUnused p)) Nothing $ \cmds ->
        QC.monadicIO (propCmds memPolicy cmds)

-- Ideally we'd like to use @SimM s@ instead of IO, but unfortunately
-- QSM requires monads that implement MonadIO.
propCmds :: LUT t
         => MemPolicy
         -> QSM.Commands (At (Cmd t)) (At (Resp t))
         -> QC.PropertyM IO ()
propCmds memPolicy cmds = do
    fs <- QC.run $ atomically $ newTVar MockFS.empty
    let dbEnv :: DbEnv IO
        dbEnv = DbEnv (simHasFS EH.exceptions fs) memPolicy
    db <- QC.run $ initStandaloneDB dbEnv
    let sm' = sm memPolicy db
    (hist, _model, res) <- runCommands sm' cmds
    prettyCommands sm' hist
      $ QC.tabulate "Tags" (map show $ tagEvents k (execCmds memPolicy cmds))
      $ res QC.=== Ok
  where
    k = SecurityParam $ memPolicyMaxRollback memPolicy

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
    go os = MemPolicyMaxOffset $ range (maximum os)

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
    TagRestore (Maybe SnapState) RangeK

    -- | Tag rollback
    --
    -- We record the rollback length
  | TagMaxRollback RangeK

    -- | Tag chain truncation
    --
    -- We record how many blocks were dropped
  | TagMaxDrop RangeK
  deriving (Show, Eq)

type EventPred t = C.Predicate (Event t Symbolic) Tag

tagEvents :: forall t. SecurityParam -> [Event t Symbolic] -> [Tag]
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
        fmap (TagMaxRollback . rangeK k) $ C.maximum $ \ev ->
          case eventCmd ev of
            At (Switch n _) -> Just n
            _otherwise      -> Nothing

    tagMaxDrop :: EventPred t
    tagMaxDrop =
        fmap (TagMaxDrop . rangeK k) $ C.maximum $ \ev ->
          case eventCmd ev of
            At (Drop n) -> Just n
            _otherwise  -> Nothing

    tagRestore :: Maybe SnapState -> EventPred t
    tagRestore mST =
        fmap (TagRestore mST . rangeK k) $ C.maximum $ \ev ->
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
        repeatedly QC.collect (run cmds) $
          QC.property True
  where
    k = SecurityParam $ memPolicyMaxRollback memPolicy
    p = Proxy @'LedgerSimple

    run :: LUT t => QSM.Commands (At (Cmd t)) (At (Resp t)) -> [Tag]
    run = filter relevant . tagEvents k . execCmds memPolicy
