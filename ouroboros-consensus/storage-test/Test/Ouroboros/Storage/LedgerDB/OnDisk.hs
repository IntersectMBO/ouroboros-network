{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- | On-disk ledger DB tests.
--
-- This is a model based test. The commands here are
--
-- * Get the current ledger state
-- * Push a block, or switch to a fork
-- * Write a snapshot to disk
-- * Restore the ledger DB from the snapshots on disk
-- * Model disk corruption
--
-- The model here is satisfyingly simple: just a map from blocks to their
-- corresponding ledger state.
--
module Test.Ouroboros.Storage.LedgerDB.OnDisk (
    showLabelledExamples
  , tests
  ) where

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S
import           Control.Monad.Except (Except, runExcept)
import           Control.Monad.State (StateT (..))
import qualified Control.Monad.State as State
import           Control.Tracer (nullTracer)
import           Data.Bifunctor
import           Data.Foldable (toList)
import           Data.Functor.Classes
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.LedgerDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Prelude hiding (elem)
import           System.FS.API
import           System.FS.API.Types
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.STM
import           System.Random (getStdRandom, randomR)
import           Test.Ouroboros.Storage.LedgerDB.InMemory ()
import           Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary ()
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Monadic as QC
import qualified Test.QuickCheck.Random as QC
import           Test.StateMachine hiding (showLabelledExamples)
import qualified Test.StateMachine.Labelling as C
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.Range
import           Test.Util.TestBlock hiding (TestBlock, TestBlockCodecConfig,
                     TestBlockStorageConfig)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "OnDisk" [
      testProperty "LedgerSimple" prop_sequential
    ]

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}

type TestBlock = TestBlockWith Tx

-- | Mock of a UTxO transaction where exactly one (transaction) input is
-- consumed and exactly one output is produced.
--
data Tx = Tx {
    -- | Input that the transaction consumes.
    consumed :: Token
    -- | Ouptupt that the transaction produces.
  , produced :: (Token, TValue)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

-- | A token is an identifier for the values produced and consumed by the
-- 'TestBlock' transactions.
--
-- This is analogous to @TxId@: it's how we identify what's in the table. It's
-- also analogous to @TxIn@, since we trivially only have one output per 'Tx'.
newtype Token = Token { unToken :: Point TestBlock }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr)

-- | Unit of value associated with the output produced by a transaction.
--
-- This is analogous to @TxOut@: it's what the table maps 'Token's to.
newtype TValue = TValue (WithOrigin SlotNo)
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr)

{-------------------------------------------------------------------------------
  A ledger semantics for TestBlock
-------------------------------------------------------------------------------}

data UTxTok = UTxTok { utxtok  :: Map Token TValue
                     , -- | All the tokens that ever existed. We use this to
                       -- make sure a token is not created more than once. See
                       -- the definition of 'applyPayload' in the
                       -- 'PayloadSemantics' of 'Tx'.
                       utxhist :: Set Token
                     }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, Serialise, ToExpr)

data TxErr
  = TokenWasAlreadyCreated Token
  | TokenDoesNotExist      Token
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, Serialise, ToExpr)

instance PayloadSemantics Tx where
  type PayloadDependentState Tx = UTxTok

  type PayloadDependentError Tx = TxErr

  -- We need to exercise the HD backend. This requires that we store key-values
  -- ledger tables and the block application semantics satisfy:
  --
  -- * a key is deleted at most once
  -- * a key is inserted at most once
  --
  applyPayload st Tx{consumed, produced} =
      delete consumed st >>= uncurry insert produced
    where
      insert :: Token -> TValue -> UTxTok -> Either TxErr UTxTok
      insert tok val UTxTok{utxtok, utxhist} =
        if tok `Set.member` utxhist
        then Left  $ TokenWasAlreadyCreated tok
        else Right $ UTxTok { utxtok  = Map.insert tok val utxtok
                            , utxhist = Set.insert tok utxhist
                            }

      delete :: Token -> UTxTok -> Either TxErr UTxTok
      delete tok st'@UTxTok{utxtok} =
        if tok `Map.member` utxtok
        then Right $ st' { utxtok = Map.delete tok utxtok }
        else Left  $ TokenDoesNotExist tok

{-------------------------------------------------------------------------------
  TestBlock generation

  When we added support for storing parts of the ledger state on disk we needed
  to exercise this new functionality. Therefore, we modified this test so that
  the ledger state associated to the test block contained tables (key-value
  maps) to be stored on disk. This ledger state needs to follow an evolution
  pattern similar to the UTxO one (see the 'PayloadSemantics' instance for more
  details). As a result, block application might fail on a given payload.

  The tests in this module assume that no invalid blocks are generated. Thus we
  have to satisfy this assumption in the block generators. To keep the
  generators simple, eg independent on the ledger state, we follow this strategy
  to block generation:

  - The block payload consist of a single transaction:
      - input: Point
      - output: (Point, SlotNo)
  - The ledger state is a map from Point to SlotNo.
  - We start always in an initial state in which 'GenesisPoint' maps to slot 0.
  - When we generate a block for point p, the payload of the block will be:
      - input: point p - 1
      - ouptput: (point p, slot of point p)


  A consequence of adopting the strategy above is that the initial state is
  coupled to the generator's semantics.
 -------------------------------------------------------------------------------}

initialTestLedgerState :: UTxTok
initialTestLedgerState = UTxTok {
    utxtok = Map.singleton initialToken (pointTValue initialToken)
  , utxhist = Set.singleton initialToken
  }
  where
    initialToken = Token GenesisPoint

-- | Get the token value associated to a given token. This is coupled to the
-- generators semantics.
pointTValue :: Token -> TValue
pointTValue = TValue . pointSlot . unToken

genBlocks ::
     Word64
  -> Point TestBlock
  -> [TestBlock]
genBlocks n pt0 = take (fromIntegral n) (go pt0)
  where
    go pt = let b = genBlock pt in b : go (blockPoint b)

genBlock ::
     Point TestBlock -> TestBlock
genBlock pt =
  mkBlockFrom pt Tx { consumed = Token pt
                    , produced = ( Token pt', TValue (pointSlot pt'))
                    }
  where
    mkBlockFrom :: Point (TestBlockWith ptype) -> ptype -> (TestBlockWith ptype)
    mkBlockFrom GenesisPoint           = firstBlockWithPayload 0
    mkBlockFrom (BlockPoint slot hash) = successorBlockWithPayload hash slot

    pt' :: Point (TestBlockWith Tx)
    pt' = castPoint (blockPoint dummyBlk)
      where
        -- This could be the new block itself; we merely wanted to avoid the loop.
        dummyBlk :: TestBlockWith ()
        dummyBlk = mkBlockFrom (castPoint pt) ()

genBlockFromLedgerState :: ExtLedgerState TestBlock -> Gen TestBlock
genBlockFromLedgerState = pure . genBlock . lastAppliedPoint . ledgerState

extLedgerDbConfig :: SecurityParam -> LedgerDbCfg (ExtLedgerState TestBlock)
extLedgerDbConfig secParam = LedgerDbCfg {
      ledgerDbCfgSecParam = secParam
    , ledgerDbCfg         = ExtLedgerCfg $ singleNodeTestConfigWith TestBlockCodecConfig TestBlockStorageConfig secParam
    }


-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

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

data Cmd ss =
    -- | Get the current ledger state
    Current

    -- | Push a block
  | Push TestBlock

    -- | Switch to a fork
  | Switch Word64 [TestBlock]

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
    -- responsibility of the 'ChainDB' to /notice/ disk corruption (by
    -- catching the appropriate exceptions), we assume that the ledger state
    -- will immediately be re-initialized after a 'Truncate' (which is precisely
    -- what the 'ChainDB' would do, after first doing recovery on the
    -- underlying 'LedgerDB'). This is important because otherwise the model
    -- would diverge from the real thing.
    --
    -- Since 'Drop' therefore implies a 'Restore', we return the new ledger.
  | Drop Word64
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Success ss =
    Unit ()
  | MaybeErr (Either (ExtValidationError TestBlock) ())
  | Ledger (ExtLedgerState TestBlock)
  | Snapped (Maybe (ss, RealPoint TestBlock))
  | Restored (MockInitLog ss, ExtLedgerState TestBlock)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Currently we don't have any error responses
newtype Resp ss = Resp (Success ss)
  deriving (Show, Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Pure model
-------------------------------------------------------------------------------}

-- | The mock ledger records the blocks and ledger values (new to old)
type MockLedger = [(TestBlock, ExtLedgerState TestBlock)]

-- | We use the slot number of the ledger state as the snapshot number
--
-- We only keep track of this to be able to give more meaningful statistics
-- about generated tests. The mock implementation doesn't actually " take "
-- any snapshots (instead it stores the ledger state at each point).
newtype MockSnap = MockSnap Word64
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToExpr)

-- | State of all snapshots on disk
--
-- In addition to the state of the snapshot we also record the tip of the chain
-- at the time we took the snapshot; this is important for 'mockMaxRollback'.
type MockSnaps = Map MockSnap (RealPoint TestBlock, SnapState)

-- | Mock implementation
--
-- The mock implementation simply records the ledger at every point.
-- We store the chain most recent first.
data Mock = Mock {
      -- | Current ledger
      mockLedger   :: MockLedger

      -- | Current state the snapshots
    , mockSnaps    :: MockSnaps

      -- | The oldest (tail) block in the real DB at the most recent restore
      --
      -- This puts a limit on how far we can roll back.
      -- See also 'applyMockLog', 'mockMaxRollback'.
    , mockRestore  :: Point TestBlock

      -- | Security parameter
      --
      -- We need the security parameter only to compute which snapshots the real
      -- implementation would take, so that we can accurately predict how far
      -- the real implementation can roll back.
    , mockSecParam :: SecurityParam
    }
  deriving (Show, Generic, ToExpr)

data SnapState = SnapOk | SnapCorrupted
  deriving (Show, Eq, Generic, ToExpr)

mockInit :: SecurityParam -> Mock
mockInit = Mock [] Map.empty GenesisPoint

mockCurrent :: Mock -> ExtLedgerState TestBlock
mockCurrent Mock{..} =
    case mockLedger of
      []       -> testInitExtLedgerWithState initialTestLedgerState
      (_, l):_ -> l

mockChainLength :: Mock -> Word64
mockChainLength Mock{..} = fromIntegral (length mockLedger)

mockRollback :: Word64 -> Mock -> Mock
mockRollback n mock@Mock{..} = mock {
      mockLedger = drop (fromIntegral n) mockLedger
    }

mockUpdateLedger :: StateT MockLedger (Except (ExtValidationError TestBlock)) a
                 -> Mock -> (Either (ExtValidationError TestBlock) a, Mock)
mockUpdateLedger f mock =
    case runExcept (runStateT f (mockLedger mock)) of
      Left  err          -> (Left err, mock)
      Right (a, ledger') -> (Right a, mock { mockLedger = ledger' })

mockRecentSnap :: Mock -> Maybe SnapState
mockRecentSnap Mock{..} = snd . snd <$> Map.lookupMax mockSnaps

{-------------------------------------------------------------------------------
  Modelling restoration

  Although the mock implementation itself is not affected by disk failures
  (in fact, the concept makes no sense, since we don't store anything on disk),
  we /do/ need to be able to accurately predict how the real DB will be
  initialized (from which snapshot); this is important, because this dictates
  how far the real DB can roll back.
-------------------------------------------------------------------------------}

data MockInitLog ss =
    MockFromGenesis
  | MockFromSnapshot    ss (RealPoint TestBlock)
  | MockReadFailure     ss                       (MockInitLog ss)
  | MockTooRecent       ss (RealPoint TestBlock) (MockInitLog ss)
  | MockGenesisSnapshot ss                       (MockInitLog ss)
  deriving (Show, Eq, Functor, Foldable, Traversable)

fromInitLog :: InitLog TestBlock -> MockInitLog DiskSnapshot
fromInitLog  InitFromGenesis          = MockFromGenesis
fromInitLog (InitFromSnapshot ss tip) = MockFromSnapshot ss tip
fromInitLog (InitFailure ss err log') =
    case err of
      InitFailureRead _err     -> MockReadFailure     ss     (fromInitLog log')
      InitFailureTooRecent tip -> MockTooRecent       ss tip (fromInitLog log')
      InitFailureGenesis       -> MockGenesisSnapshot ss     (fromInitLog log')

mockInitLog :: Mock -> MockInitLog MockSnap
mockInitLog Mock{..} = go (Map.toDescList mockSnaps)
  where
    go :: [(MockSnap, (RealPoint TestBlock, SnapState))] -> MockInitLog MockSnap
    go []                          = MockFromGenesis
    go ((snap, (pt, state)):snaps) =
        case state of
          SnapCorrupted ->
            -- If it's truncated, it will skip it
            MockReadFailure snap $ go snaps
          SnapOk ->
            if onChain pt
              then MockFromSnapshot snap pt
              else MockTooRecent    snap pt $ go snaps

    onChain :: RealPoint TestBlock -> Bool
    onChain pt = any (\(b, _) -> blockRealPoint b == pt) mockLedger

applyMockLog :: MockInitLog MockSnap -> Mock -> Mock
applyMockLog = go
  where
    go :: MockInitLog MockSnap -> Mock -> Mock
    go  MockFromGenesis                mock = mock { mockRestore = GenesisPoint         }
    go (MockFromSnapshot    _  tip)    mock = mock { mockRestore = realPointToPoint tip }
    go (MockReadFailure     ss   log') mock = go log' $ deleteSnap ss mock
    go (MockTooRecent       ss _ log') mock = go log' $ deleteSnap ss mock
    go (MockGenesisSnapshot ss   log') mock = go log' $ deleteSnap ss mock

    deleteSnap :: MockSnap -> Mock -> Mock
    deleteSnap ss mock = mock {
          mockSnaps = Map.alter delete ss (mockSnaps mock)
        }

    delete :: Maybe (RealPoint TestBlock, SnapState)
           -> Maybe (RealPoint TestBlock, SnapState)
    delete Nothing  = error "setIsDeleted: impossible"
    delete (Just _) = Nothing

-- | Compute theoretical maximum rollback
--
-- The actual maximum rollback will be restricted by the ledger DB params.
mockMaxRollback :: Mock -> Word64
mockMaxRollback Mock{..} = go mockLedger
  where
    go :: MockLedger -> Word64
    go ((b, _l):bs)
      | blockPoint b == mockRestore = 0
      | otherwise                   = 1 + go bs
    go []                           = 0

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

runMock :: Cmd MockSnap -> Mock -> (Resp MockSnap, Mock)
runMock cmd initMock =
    first Resp $ go cmd initMock
  where
    cfg :: LedgerDbCfg (ExtLedgerState TestBlock)
    cfg = extLedgerDbConfig (mockSecParam initMock)

    go :: Cmd MockSnap -> Mock -> (Success MockSnap, Mock)
    go Current       mock = (Ledger (cur (mockLedger mock)), mock)
    go (Push b)      mock = first MaybeErr $ mockUpdateLedger (push b)      mock
    go (Switch n bs) mock = first MaybeErr $ mockUpdateLedger (switch n bs) mock
    go Restore       mock = (Restored (initLog, cur (mockLedger mock')), mock')
      where
        initLog = mockInitLog mock
        mock'   = applyMockLog initLog mock
    go Snap          mock = case mbSnapshot of
        Just pt
          | let mockSnap = MockSnap (unSlotNo (realPointSlot pt))
          , Map.notMember mockSnap (mockSnaps mock)
          -> ( Snapped (Just (mockSnap, pt))
             , mock {
                   mockSnaps =
                     Map.insert mockSnap (pt, SnapOk) (mockSnaps mock)
                 }
             )
        _otherwise
          -- No snapshot to take or one already exists
          -> (Snapped Nothing, mock)
      where
        -- | The snapshot that the real implementation will possibly write to
        -- disk.
        --
        -- 1. We will write the snapshot of the ledger state @k@ blocks back
        --    from the tip to disk.
        --
        --    For example, with @k = 2@:
        --
        --    > A -> B -> C -> D -> E
        --
        --    We will write C to disk.
        --
        -- 2. In case we don't have enough snapshots for (1), i.e., @<= k@, we
        --    look at the snapshot from which we restored ('mockRestore').
        --
        --    a. When that corresponds to the genesis ledger state, we don't
        --       write a snapshot to disk.
        --
        --    b. Otherwise, we write 'mockRestore' to disk. Note that we later
        --       check whether that snapshots still exists on disk, in which
        --       case we wouldn't write it to disk again.
        mbSnapshot :: Maybe (RealPoint TestBlock)
        mbSnapshot = case drop k untilRestore of
            (blk, _):_ -> Just (blockRealPoint blk)  -- 1
            []         -> case pointToWithOriginRealPoint (mockRestore mock) of
                            Origin       -> Nothing  -- 2a
                            NotOrigin pt -> Just pt  -- 2b
          where
            k :: Int
            k = fromIntegral $ maxRollbacks $ mockSecParam mock

            -- The snapshots from new to old until 'mockRestore' (inclusive)
            untilRestore :: [(TestBlock, ExtLedgerState TestBlock)]
            untilRestore =
              takeWhile
                ((/= (mockRestore mock)) . blockPoint . fst)
                (mockLedger mock)

    go (Corrupt c ss) mock = (
          Unit ()
        , mock { mockSnaps = Map.alter corrupt ss (mockSnaps mock) }
        )
      where
        corrupt :: Maybe (RealPoint TestBlock, SnapState)
                -> Maybe (RealPoint TestBlock, SnapState)
        corrupt Nothing         = error "corrupt: impossible"
        corrupt (Just (ref, _)) = case c of
          Delete   -> Nothing
          Truncate -> Just (ref, SnapCorrupted)
    go (Drop n) mock =
        go Restore $ mock {
            mockLedger = drop (fromIntegral n) (mockLedger mock)
          }

    push :: TestBlock -> StateT MockLedger (Except (ExtValidationError TestBlock)) ()
    push b = do
        ls <- State.get
        l' <- State.lift $ tickThenApply (ledgerDbCfg cfg) b (cur ls)
        State.put ((b, l'):ls)

    switch :: Word64
           -> [TestBlock]
           -> StateT MockLedger (Except (ExtValidationError TestBlock)) ()
    switch n bs = do
        State.modify $ drop (fromIntegral n)
        mapM_ push bs

    cur :: MockLedger -> ExtLedgerState TestBlock
    cur []         = testInitExtLedgerWithState initialTestLedgerState
    cur ((_, l):_) = l

{-------------------------------------------------------------------------------
  Standalone instantiation of the ledger DB
-------------------------------------------------------------------------------}

-- | Arguments required by 'StandaloneDB'
data DbEnv m = DbEnv {
      dbHasFS    :: SomeHasFS m
    , dbSecParam :: SecurityParam
    }

-- | Standalone ledger DB
--
-- Under normal circumstances the ledger DB is maintained by the 'ChainDB',
-- and supported by the 'ChainDB'. In order to test it stand-alone we need to
-- mock these components.
data StandaloneDB m = DB {
      -- | Arguments
      dbEnv         :: DbEnv m

      -- | Block storage
      --
      -- We can think of this as mocking the volatile DB. Blocks can be
      -- added to this without updating the rest of the state.
    , dbBlocks      :: StrictTVar m (Map (RealPoint TestBlock) TestBlock)

      -- | Current chain and corresponding ledger state
      --
      -- We can think of this as mocking the ChainDB, which must keep
      -- track of a current chain and keep the ledger DB in sync with it.
      --
      -- Invariant: all references @r@ here must be present in 'dbBlocks'.
    , dbState       :: StrictTVar m ([RealPoint TestBlock], LedgerDB' TestBlock)

      -- | Resolve blocks
    , dbResolve     :: ResolveBlock m TestBlock

      -- | LedgerDB config
    , dbLedgerDbCfg :: LedgerDbCfg (ExtLedgerState TestBlock)
    }

initStandaloneDB :: forall m. IOLike m => DbEnv m -> m (StandaloneDB m)
initStandaloneDB dbEnv@DbEnv{..} = do
    dbBlocks <- uncheckedNewTVarM Map.empty
    dbState  <- uncheckedNewTVarM (initChain, initDB)

    let dbResolve :: ResolveBlock m TestBlock
        dbResolve r = atomically $ getBlock r <$> readTVar dbBlocks

        dbLedgerDbCfg :: LedgerDbCfg (ExtLedgerState TestBlock)
        dbLedgerDbCfg = extLedgerDbConfig dbSecParam

    return DB{..}
  where
    initChain :: [RealPoint TestBlock]
    initChain = []

    initDB :: LedgerDB' TestBlock
    initDB = ledgerDbWithAnchor (testInitExtLedgerWithState initialTestLedgerState)

    getBlock ::
         RealPoint TestBlock
      -> Map (RealPoint TestBlock) TestBlock
      -> TestBlock
    getBlock = Map.findWithDefault (error blockNotFound)

    blockNotFound :: String
    blockNotFound = concat [
          "dbConf: "
        , "invariant violation: "
        , "block in dbChain not in dbBlocks, "
        , "or LedgerDB not re-initialized after chain truncation"
        ]

dbStreamAPI :: forall m. IOLike m => StandaloneDB m -> StreamAPI m TestBlock
dbStreamAPI DB{..} = StreamAPI {..}
  where
    streamAfter ::
         Point TestBlock
      -> (Either (RealPoint TestBlock) (m (NextBlock TestBlock)) -> m a)
      -> m a
    streamAfter tip k = do
        pts <- atomically $ reverse . fst <$> readTVar dbState
        case tip' of
          NotOrigin pt
            | pt `L.notElem` pts
            -> k $ Left pt
          _otherwise
            -> do toStream <- uncheckedNewTVarM (blocksToStream tip' pts)
                  k (Right (getNext toStream))
     where
       tip' = pointToWithOriginRealPoint tip

    -- Blocks to stream
    --
    -- Precondition: tip must be on the current chain
    blocksToStream ::
         WithOrigin (RealPoint TestBlock)
      -> [RealPoint TestBlock] -> [RealPoint TestBlock]
    blocksToStream Origin        = id
    blocksToStream (NotOrigin r) = tail . dropWhile (/= r)

    getNext :: StrictTVar m [RealPoint TestBlock] -> m (NextBlock TestBlock)
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
                          Just b  -> return $ NextBlock b
                          Nothing -> error blockNotFound

    blockNotFound :: String
    blockNotFound = concat [
          "dbStreamAPI: "
        , "invariant violation: "
        , "block in dbChain not present in dbBlocks"
        ]

runDB ::
     forall m. IOLike m
  => StandaloneDB m -> Cmd DiskSnapshot -> m (Resp DiskSnapshot)
runDB standalone@DB{..} cmd =
    case dbEnv of
      DbEnv{dbHasFS} -> Resp <$> go dbHasFS cmd
  where
    streamAPI = dbStreamAPI standalone

    annLedgerErr' ::
         AnnLedgerError (ExtLedgerState TestBlock) TestBlock
      -> ExtValidationError TestBlock
    annLedgerErr' = annLedgerErr

    go :: SomeHasFS m -> Cmd DiskSnapshot -> m (Success DiskSnapshot)
    go _ Current =
        atomically $ (Ledger . ledgerDbCurrent . snd) <$> readTVar dbState
    go _ (Push b) = do
        atomically $ modifyTVar dbBlocks $
          uncurry Map.insert (refValPair b)
        upd (push b) $ \db ->
          fmap (first annLedgerErr') $
            defaultThrowLedgerErrors $
              ledgerDbPush
                dbLedgerDbCfg
                (ApplyVal b)
                db
    go _ (Switch n bs) = do
        atomically $ modifyTVar dbBlocks $
          repeatedly (uncurry Map.insert) (map refValPair bs)
        upd (switch n bs) $ \db ->
          fmap (bimap annLedgerErr' ignoreExceedRollback) $
            defaultResolveWithErrors dbResolve $
              ledgerDbSwitch
                dbLedgerDbCfg
                n
                (const $ pure ())
                (map ApplyVal bs)
                db
    go hasFS Snap = do
        (_, db) <- atomically (readTVar dbState)
        Snapped <$>
          takeSnapshot
            nullTracer
            hasFS
            S.encode
            (ledgerDbAnchor db)
    go hasFS Restore = do
        (initLog, db, _replayed) <-
          initLedgerDB
            nullTracer
            nullTracer
            hasFS
            S.decode
            S.decode
            dbLedgerDbCfg
            (return (testInitExtLedgerWithState initialTestLedgerState))
            streamAPI
        atomically $ modifyTVar dbState (\(rs, _) -> (rs, db))
        return $ Restored (fromInitLog initLog, ledgerDbCurrent db)
    go hasFS (Corrupt c ss) =
        catch
          (case c of
             Delete   -> Unit <$> deleteSnapshot   hasFS ss
             Truncate -> Unit <$> truncateSnapshot hasFS ss)
          (\(_ :: FsError) -> return $ Unit()) -- ignore any errors during corruption
    go hasFS (Drop n) = do
        -- During recovery the ChainDB would ask the ChainDB to recover
        -- and pick a new current chain; only once that is done would it
        -- compute a new ledger state. During this process the ChainDB
        -- would effectively be closed.
        atomically $ do
            (rs, _db) <- readTVar dbState
            writeTVar dbState (drop (fromIntegral n) rs, error "ledger DB not initialized")
        go hasFS Restore

    push ::
         TestBlock
      -> [RealPoint TestBlock] -> [RealPoint TestBlock]
    push b = (blockRealPoint b:)

    switch ::
         Word64
      -> [TestBlock]
      -> [RealPoint TestBlock] -> [RealPoint TestBlock]
    switch 0 bs = (reverse (map blockRealPoint bs) ++)
    switch n bs = switch 0 bs . drop (fromIntegral n)

    -- We don't currently test the case where the LedgerDB cannot support
    -- the full rollback range. See also
    -- <https://github.com/input-output-hk/ouroboros-network/issues/1025>
    ignoreExceedRollback :: Either ExceededRollback a -> a
    ignoreExceedRollback (Left  _) = error "unexpected ExceededRollback"
    ignoreExceedRollback (Right a) = a

    upd :: ( [RealPoint TestBlock] -> [RealPoint TestBlock] )
        -> (   LedgerDB' TestBlock
            -> m (Either (ExtValidationError TestBlock) (LedgerDB' TestBlock))
           )
        -> m (Success DiskSnapshot)
    upd f g = do
        -- We cannot run the whole thing in a transaction, since computing the
        -- new value of the ledger DB may require reading from the chain DB
        (rs, db) <- atomically $ readTVar dbState
        mDB'     <- g db
        case mDB' of
          Left  e   -> return $ MaybeErr (Left e)
          Right db' -> do atomically $ writeTVar dbState (f rs, db')
                          return $ MaybeErr (Right ())

    truncateSnapshot :: SomeHasFS m -> DiskSnapshot -> m ()
    truncateSnapshot (SomeHasFS hasFS@HasFS{..}) ss =
        withFile hasFS (snapshotToPath ss) (AppendMode AllowExisting) $ \h ->
          hTruncate h 0

    refValPair :: TestBlock -> (RealPoint TestBlock, TestBlock)
    refValPair b = (blockRealPoint b, b)

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

data Model r = Model {
    modelMock  :: Mock
  , modelSnaps :: SnapRefs r
  }
  deriving (Generic)

deriving instance Show1 r => Show (Model r)

initModel :: SecurityParam -> Model r
initModel secParam = Model (mockInit secParam) []

toMock :: (Functor f, Eq1 r) => Model r -> f :@ r -> f MockSnap
toMock m (At fr) = (modelSnaps m !) <$> fr

step :: Eq1 r => Model r -> Cmd :@ r -> (Resp MockSnap, Mock)
step m cmd = runMock (toMock m cmd) (modelMock m)

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event r = Event {
      eventBefore   :: Model    r
    , eventCmd      :: Cmd   :@ r
    , eventResp     :: Resp  :@ r
    , eventAfter    :: Model    r
    , eventMockResp :: Resp  MockSnap
    }

deriving instance Show1 r => Show (Event r)

lockstep :: Eq1 r
         => Model    r
         -> Cmd   :@ r
         -> Resp  :@ r
         -> Event    r
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

execCmd :: Model Symbolic
        -> QSM.Command (At Cmd) (At Resp)
        -> Event Symbolic
execCmd model (QSM.Command cmd resp _vars) = lockstep model cmd resp

execCmds :: SecurityParam
         -> QSM.Commands (At Cmd) (At Resp)
         -> [Event Symbolic]
execCmds secParam = \(QSM.Commands cs) -> go (initModel secParam) cs
  where
    go :: Model Symbolic
       -> [QSM.Command (At Cmd) (At Resp)]
       -> [Event Symbolic]
    go _ []     = []
    go m (c:cs) = e : go (eventAfter e) cs
      where
        e = execCmd m c

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

generator :: SecurityParam -> Model Symbolic -> Maybe (Gen (Cmd :@ Symbolic))
generator secParam (Model mock hs) = Just $ QC.oneof $ concat [
      withoutRef
    , if null possibleCorruptions
        then []
        else [(At . uncurry Corrupt) <$> QC.elements possibleCorruptions]
    ]
  where
    withoutRef :: [Gen (Cmd :@ Symbolic)]
    withoutRef = [
          fmap At $ return Current
        , fmap (At . Push) $ genBlockFromLedgerState (mockCurrent mock)
        , fmap At $ do
            let maxRollback = minimum [
                    mockMaxRollback mock
                  , maxRollbacks secParam
                  ]
            numRollback  <- QC.choose (0, maxRollback)
            numNewBlocks <- QC.choose (numRollback, numRollback + 2)
            let
              afterRollback = mockRollback numRollback mock
              blocks        = genBlocks
                                numNewBlocks
                                (lastAppliedPoint . ledgerState . mockCurrent $ afterRollback)
            return $ Switch numRollback blocks
        , fmap At $ return Snap
        , fmap At $ return Restore
        , fmap At $ Drop <$> QC.choose (0, mockChainLength mock)
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
                -- The snapshot has already been deleted
                []

    possibleCorruptionsInState :: SnapState -> [Corruption]
    possibleCorruptionsInState SnapOk        = [Delete, Truncate]
    possibleCorruptionsInState SnapCorrupted = [Delete]

shrinker :: Model Symbolic -> Cmd :@ Symbolic -> [Cmd :@ Symbolic]
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

instance CommandNames (At Cmd) where
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

instance ToExpr (Model Concrete)
instance ToExpr SecurityParam
instance ToExpr DiskSnapshot

{-------------------------------------------------------------------------------
  Final state machine
-------------------------------------------------------------------------------}

semantics :: IOLike m
          => StandaloneDB m
          -> Cmd :@ Concrete -> m (Resp :@ Concrete)
semantics db (At cmd) = (At . fmap reference) <$> runDB db (concrete <$> cmd)

transition :: Eq1 r
           => Model    r
           -> Cmd   :@ r
           -> Resp  :@ r
           -> Model    r
transition m cmd = eventAfter . lockstep m cmd

postcondition :: Model    Concrete
              -> Cmd   :@ Concrete
              -> Resp  :@ Concrete
              -> Logic
postcondition m cmd r = toMock (eventAfter e) r .== eventMockResp e
  where
    e = lockstep m cmd r

precondition :: Model Symbolic -> Cmd :@ Symbolic -> Logic
precondition (Model mock hs) (At c) =
        forall (toList c) (`member` map fst hs)
    .&& validCmd c
  where
    -- Maximum rollback might decrease if shrinking removed blocks
    validCmd :: Cmd ss -> Logic
    validCmd (Switch n _) = n .<= mockMaxRollback mock
    validCmd _otherwise   = Top

symbolicResp :: Model           Symbolic
             -> Cmd          :@ Symbolic
             -> GenSym (Resp :@ Symbolic)
symbolicResp m c = At <$> traverse (const genSym) resp
  where
    (resp, _mock') = step m c

sm :: IOLike m
   => SecurityParam
   -> StandaloneDB m
   -> StateMachine Model (At Cmd) m (At Resp)
sm secParam db = StateMachine {
      initModel     = initModel secParam
    , transition    = transition
    , precondition  = precondition
    , postcondition = postcondition
    , invariant     = Nothing
    , generator     = generator secParam
    , shrinker      = shrinker
    , semantics     = semantics db
    , mock          = symbolicResp
    , cleanup       = noCleanup
    }

prop_sequential :: SecurityParam -> QC.Property
prop_sequential secParam =
    forAllCommands (sm secParam dbUnused) Nothing $ \cmds ->
      QC.monadicIO (propCmds secParam cmds)

-- Ideally we'd like to use @IOSim s@ instead of IO, but unfortunately
-- QSM requires monads that implement MonadIO.
propCmds :: SecurityParam
         -> QSM.Commands (At Cmd) (At Resp)
         -> QC.PropertyM IO ()
propCmds secParam cmds = do
    fs <- QC.run $ uncheckedNewTVarM MockFS.empty
    let dbEnv :: DbEnv IO
        dbEnv = DbEnv (SomeHasFS (simHasFS fs)) secParam
    db <- QC.run $ initStandaloneDB dbEnv
    let sm' = sm secParam db
    (hist, _model, res) <- runCommands sm' cmds
    prettyCommands sm' hist
      $ QC.tabulate
          "Tags"
          (map show $ tagEvents secParam (execCmds secParam cmds))
      $ res QC.=== Ok

dbUnused :: StandaloneDB IO
dbUnused = error "DB unused during command generation"

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

type EventPred = C.Predicate (Event Symbolic) Tag

tagEvents :: SecurityParam -> [Event Symbolic] -> [Tag]
tagEvents k = C.classify [
      tagMaxRollback
    , tagMaxDrop
    , tagRestore Nothing
    , tagRestore (Just SnapOk)
    , tagRestore (Just SnapCorrupted)
    ]
  where
    tagMaxRollback :: EventPred
    tagMaxRollback =
        fmap (TagMaxRollback . rangeK k) $ C.maximum $ \ev ->
          case eventCmd ev of
            At (Switch n _) -> Just n
            _otherwise      -> Nothing

    tagMaxDrop :: EventPred
    tagMaxDrop =
        fmap (TagMaxDrop . rangeK k) $ C.maximum $ \ev ->
          case eventCmd ev of
            At (Drop n) -> Just n
            _otherwise  -> Nothing

    tagRestore :: Maybe SnapState -> EventPred
    tagRestore mST =
        fmap (TagRestore mST . rangeK k) $ C.maximum $ \ev ->
          let mock = modelMock (eventBefore ev) in
          case eventCmd ev of
            At Restore | mockRecentSnap mock == mST -> Just (mockChainLength mock)
            _otherwise                              -> Nothing

{-------------------------------------------------------------------------------
  Inspecting the labelling function
-------------------------------------------------------------------------------}

showLabelledExamples :: SecurityParam
                     -> Maybe Int
                     -> (Tag -> Bool) -- ^ Which tag are we interested in?
                     -> IO ()
showLabelledExamples secParam mReplay relevant = do
    replaySeed <- case mReplay of
                    Nothing   -> getStdRandom $ randomR (1, 999999)
                    Just seed -> return seed

    putStrLn $ "Using replaySeed " ++ show replaySeed

    let args = QC.stdArgs {
            QC.maxSuccess = 10000
          , QC.replay     = Just (QC.mkQCGen replaySeed, 0)
          }

    QC.labelledExamplesWith args $
      forAllCommands (sm secParam dbUnused) Nothing $ \cmds ->
        repeatedly QC.collect (run cmds) $
          QC.property True
  where
    run :: QSM.Commands (At Cmd) (At Resp) -> [Tag]
    run = filter relevant . tagEvents secParam . execCmds secParam
