{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Ouroboros.Storage.ChainDB.Unit (tests) where


import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe (isJust)
import           Ouroboros.Consensus.Block.Abstract (Point, blockPoint,
                     blockSlot)
import           Ouroboros.Consensus.Block.RealPoint
                     (pointToWithOriginRealPoint)
import           Ouroboros.Consensus.Config (TopLevelConfig,
                     configSecurityParam)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as API
import           Ouroboros.Consensus.Storage.ChainDB.Impl (TraceEvent)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args
import           Ouroboros.Consensus.Storage.Common (StreamFrom (..),
                     StreamTo (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (closeRegistry,
                     unsafeNewRegistry)
import           Ouroboros.Network.Block (ChainUpdate (..))
import qualified Ouroboros.Network.Mock.Chain as Mock
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import           Test.Ouroboros.Storage.ChainDB.Model (Model)
import qualified Test.Ouroboros.Storage.ChainDB.StateMachine as SM
import           Test.Ouroboros.Storage.ChainDB.StateMachine (AllComponents,
                     ChainDBEnv (..), ChainDBState (..),
                     ShouldGarbageCollect (..), TestConstraints, allComponents,
                     close, mkTestCfg, open)
import           Test.Ouroboros.Storage.TestBlock
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.ExpectedFailure (expectFailBecause)
import           Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import           Test.Util.ChainDB (MinimalChainDbArgs (..), emptyNodeDBs,
                     fromMinimalChainDbArgs, nodeDBsVol)
import           Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests = testGroup "Unit tests"
  [ testGroup "First follower instruction isJust on empty ChainDB"
    [ testCase "model" $ runModelIO followerInstructionOnEmptyChain
    , testCase "system" $ runSystemIO followerInstructionOnEmptyChain
    ]
  , testGroup (ouroborosNetworkIssue 4183)
    [ testCase "model" $ runModelIO ouroboros_network_4183
    , expectFailBecause "Issue not fixed"
      $ testCase "system" $ runSystemIO ouroboros_network_4183
    ]
  , testGroup (ouroborosNetworkIssue 3999)
    [ testCase "model" $ runModelIO ouroboros_network_3999
    , testCase "system" $ runSystemIO ouroboros_network_3999
    ]
  ]


followerInstructionOnEmptyChain :: (SupportsUnitTest m, MonadError TestFailure m) => m ()
followerInstructionOnEmptyChain = do
  f <- newFollower
  followerInstruction f >>= \case
    Right instr -> isJust instr `orFailWith` "Expecting a follower instruction"
    Left _      -> failWith $ "ChainDbError"


ouroborosNetworkIssue :: Int -> String
ouroborosNetworkIssue n
  | n <= 0 = error "Issue number should be positive"
  | otherwise = "https://github.com/input-output-hk/ouroboros-network/issues/" <> show n

ouroboros_network_4183 :: (
    Block m ~ TestBlock
  , SupportsUnitTest m
  , MonadError TestFailure m
  ) => m ()
ouroboros_network_4183 =
  let fork i = TestBody i True
  in do
    b1 <- addBlock $ firstEBB (const True) $ fork 0
    b2 <- addBlock $ mkNextBlock b1 0 $ fork 0
    b3 <- addBlock $ mkNextBlock b2 1 $ fork 1
    b4 <- addBlock $ mkNextBlock b2 1 $ fork 0
    f <- newFollower
    void $ followerForward f [blockPoint b1]
    void $ addBlock $ mkNextBlock b4 4 $ fork 0
    persistBlks DoNotGarbageCollect
    void $ addBlock $ mkNextBlock b3 3 $ fork 1
    followerInstruction f >>= \case
      Right (Just (RollBack actual))
        -> assertEqual (blockPoint b1) actual "Rollback to wrong point"
      _ -> failWith "Expecting a rollback"


-- | Test that iterators over dead forks that may have been garbage-collected
-- either stream the blocks in the dead fork normally, report that the blocks
-- have been garbage-collected, or return that the iterator is exhausted,
-- depending on when garbage collection happened. The result is
-- non-deterministic, since garbage collection happens in the background, and
-- hence, may not yet have happened when the next item in the iterator is
-- requested.
ouroboros_network_3999 :: (
    Mock.HasHeader (Block m)
  , Block m ~ TestBlock
  , SupportsUnitTest m
  , MonadError TestFailure m
  ) => m ()
ouroboros_network_3999 = do
    b1 <- addBlock $ firstBlock 0 $ fork 1
    b2 <- addBlock $ mkNextBlock b1 1 $ fork 1
    b3 <- addBlock $ mkNextBlock b2 2 $ fork 1
    i <- streamAssertSuccess (inclusiveFrom b1) (inclusiveTo b3)
    b4 <- addBlock $ mkNextBlock b1 3 $ fork 2
    b5 <- addBlock $ mkNextBlock b4 4 $ fork 2
    b6 <- addBlock $ mkNextBlock b5 5 $ fork 2
    void $ addBlock $ mkNextBlock b6 6 $ fork 2
    persistBlks GarbageCollect

    -- The block b1 is part of the current chain, so should always be returned.
    result <- iteratorNextBlock i
    assertEqual (API.IteratorResult b1) result "Streaming first block"

    -- The remainder of the elements in the iterator are part of the dead fork,
    -- and may have been garbage-collected.
    let options = [
            -- The dead fork has been garbage-collected.
            [API.IteratorBlockGCed $ blockRealPoint b2, API.IteratorExhausted]
            -- The dead fork has not been garbage-collected yet.
          , [API.IteratorResult b2, API.IteratorResult b3]]

    actual <- replicateM 2 (iteratorNextBlock i)
    assertOneOf options actual "Streaming over dead fork"

  where
    fork i = TestBody i True

    extractBlock (blk, _, _, _, _, _, _, _, _, _, _) = blk
    iteratorNextBlock it = fmap extractBlock <$> iteratorNext it

    inclusiveFrom      = StreamFromInclusive . blockRealPoint
    inclusiveTo        = StreamToInclusive . blockRealPoint
    -- Do not call this function with `Genesis`
    blockRealPoint blk = case pointToWithOriginRealPoint $ blockPoint blk of
      At realPoint -> realPoint
      _            -> error "Should not happen"


streamAssertSuccess :: (MonadError TestFailure m, SupportsUnitTest m, Mock.HasHeader (Block m))
                    => StreamFrom (Block m) -> StreamTo (Block m) -> m (IteratorId m)
streamAssertSuccess from to = stream from to >>= \case
    Left err -> failWith $ "Should be able to create iterator: " <> show err
    Right (Left err) -> failWith $ "Range should be valid: " <> show err
    Right (Right iteratorId) -> pure iteratorId


-- | Helper function to run the test against the model and translate to something
-- that HUnit likes.
runModelIO :: ModelM TestBlock a -> IO ()
runModelIO expr = toAssertion (runModel newModel topLevelConfig expr)
  where
    chunkInfo      = ImmutableDB.simpleChunkInfo 100
    newModel       = Model.empty testInitExtLedger 0
    topLevelConfig = mkTestCfg chunkInfo


-- | Helper function to run the test against the actual chain database and
-- translate to something that HUnit likes.
runSystemIO :: SystemM TestBlock IO a -> IO ()
runSystemIO expr = runSystem withChainDbEnv expr >>= toAssertion
  where
    chunkInfo      = ImmutableDB.simpleChunkInfo 100
    topLevelConfig = mkTestCfg chunkInfo
    withChainDbEnv = withTestChainDbEnv topLevelConfig chunkInfo $ convertMapKind testInitExtLedger


newtype TestFailure = TestFailure String deriving (Show)


toAssertion :: Either TestFailure a -> Assertion
toAssertion (Left (TestFailure t)) = assertFailure t
toAssertion (Right _)              = pure ()

orFailWith :: (MonadError TestFailure m) => Bool -> String -> m ()
orFailWith b msg = unless b $ failWith msg
infixl 1 `orFailWith`

failWith :: (MonadError TestFailure m) => String -> m a
failWith msg = throwError (TestFailure msg)

assertEqual :: (MonadError TestFailure m, Eq a, Show a)
            => a -> a -> String -> m ()
assertEqual expected actual description = expected == actual `orFailWith` msg
  where
    msg = description <> "\n\t Expected: " <> show expected
                      <> "\n\t Actual: " <> show actual

assertOneOf :: (MonadError TestFailure m, Eq a, Show a)
            => [a] -> a -> String -> m ()
assertOneOf options actual description = actual `elem` options `orFailWith` msg
  where
    msg = description <> "\n\t Options: " <> show options
                      <> "\n\t Actual: " <> show actual

-- | SupportsUnitTests for the test expression need to instantiate this class.
class SupportsUnitTest m where
  type FollowerId m
  type IteratorId m
  type Block m

  addBlock
    :: Block m -> m (Block m)

  newFollower
    :: m (FollowerId m)

  followerInstruction
    :: FollowerId m
    -> m (Either (API.ChainDbError (Block m))
                 (Maybe (ChainUpdate (Block m) (AllComponents (Block m)))))

  followerForward
    :: FollowerId m
    -> [Point (Block m)]
    -> m (Either (API.ChainDbError (Block m))
                 (Maybe (Point (Block m))))

  persistBlks :: ShouldGarbageCollect -> m ()

  stream
    :: StreamFrom (Block m)
    -> StreamTo (Block m)
    -> m (Either (API.ChainDbError (Block m))
                 (Either (API.UnknownRange (Block m)) (IteratorId m)))

  iteratorNext
    :: IteratorId m
    -> m (API.IteratorResult (Block m) (AllComponents (Block m)))


{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

-- | Tests against the model run in this monad.
newtype ModelM blk a = ModelM
  { runModelM :: StateT (Model blk) (ReaderT (TopLevelConfig blk) (Except TestFailure)) a
  } deriving newtype (Functor, Applicative, Monad,
                      MonadReader (TopLevelConfig blk),
                      MonadState (Model blk), MonadError TestFailure)


runModel
  :: Model blk
  -> TopLevelConfig blk
  -> ModelM blk b
  -> Either TestFailure b
runModel model topLevelConfig expr
  = runExcept (runReaderT (evalStateT (runModelM expr) model) topLevelConfig)


withModelContext :: (Model blk -> TopLevelConfig blk -> (a, Model blk)) -> ModelM blk a
withModelContext f = do
  model <- get
  topLevelConfig <- ask
  let (a, model') = f model topLevelConfig
  put model'
  pure a


instance ( Model.ModelSupportsBlock blk
         , LedgerSupportsProtocol blk
         , Eq blk
         , LedgerTablesAreTrivial (LedgerState blk)
         ) => SupportsUnitTest (ModelM blk) where

  type FollowerId (ModelM blk) = Model.FollowerId
  type IteratorId (ModelM blk) = Model.IteratorId
  type Block (ModelM blk) = blk

  newFollower = withModelContext $ \model _ ->
    Model.newFollower model

  followerInstruction followerId = withModelContext $ \model _ ->
    case Model.followerInstruction followerId allComponents model of
      Left err                     -> (Left err, model)
      Right (mChainUpdate, model') -> (Right mChainUpdate, model')

  addBlock blk = do
    -- Ensure that blocks are not characterized as invalid because they are
    -- from the future.
    modify $ \model -> model { Model.currentSlot = blockSlot blk }
    withModelContext $ \model cfg -> ((), Model.addBlock cfg blk model)
    pure blk

  followerForward followerId points = withModelContext $ \model _ ->
    case Model.followerForward followerId points model of
      Left err                     -> (Left err, model)
      Right (mChainUpdate, model') -> (Right mChainUpdate, model')

  persistBlks shouldGarbageCollect = withModelContext $ \model cfg ->
    do
      let k = configSecurityParam cfg
      pure $ Model.copyToImmutableDB k shouldGarbageCollect model

  stream from to = withModelContext $ \model cfg ->
    do
      let k = configSecurityParam cfg
      case Model.stream k from to model of
        Left err               -> (Left err, model)
        Right (result, model') -> (Right result, model')

  iteratorNext iteratorId = withModelContext $ \model _ ->
    Model.iteratorNext iteratorId allComponents model

{-------------------------------------------------------------------------------
  System
-------------------------------------------------------------------------------}

-- | Tests against the actual chain database run in this monad.
newtype SystemM blk m a = SystemM
  { runSystemM :: ReaderT (ChainDBEnv m blk) (ExceptT TestFailure m) a
  } deriving newtype (Functor, Applicative, Monad,
                      MonadReader (ChainDBEnv m blk), MonadError TestFailure)


runSystem
  :: (forall a. (ChainDBEnv m blk -> m [TraceEvent blk] -> m a) -> m a)
  -> SystemM blk m b
  -> m (Either TestFailure b)
runSystem withChainDbEnv expr
  = withChainDbEnv $ \env _getTrace ->
                       runExceptT $ runReaderT (runSystemM expr) env


-- | Provide a standard ChainDbEnv for testing.
withTestChainDbEnv
  :: (IOLike m, TestConstraints blk)
  => TopLevelConfig blk
  -> ImmutableDB.ChunkInfo
  -> ExtLedgerState blk ValuesMK
  -> (ChainDBEnv m blk -> m [TraceEvent blk] -> m a)
  -> m a
withTestChainDbEnv topLevelConfig chunkInfo extLedgerState cont
  = bracket openChainDbEnv closeChainDbEnv (uncurry cont)
  where
    openChainDbEnv = do
      threadRegistry <- unsafeNewRegistry
      iteratorRegistry <- unsafeNewRegistry
      varCurSlot <- uncheckedNewTVarM 0
      varNextId <- uncheckedNewTVarM 0
      nodeDbs <- emptyNodeDBs
      (tracer, getTrace) <- recordingTracerTVar
      let args = chainDbArgs threadRegistry nodeDbs tracer
      varDB <- open args >>= newMVar
      let env = ChainDBEnv
            { varDB
            , registry = iteratorRegistry
            , varCurSlot
            , varNextId
            , varVolatileDbFs = nodeDBsVol nodeDbs
            , args
            }
      pure (env, getTrace)

    closeChainDbEnv (env, _) = do
      readMVar (varDB env) >>= close
      closeRegistry (registry env)
      closeRegistry (cdbRegistry $ args env)

    chainDbArgs registry nodeDbs tracer =
      let args = fromMinimalChainDbArgs MinimalChainDbArgs
            { mcdbTopLevelConfig = topLevelConfig
            , mcdbChunkInfo = chunkInfo
            , mcdbInitLedger = extLedgerState
            , mcdbRegistry = registry
            , mcdbNodeDBs = nodeDbs
            , mcdbBackingStoreSelector = LedgerDB.InMemoryBackingStore
            }
      in args { cdbTracer = tracer }

instance IOLike m => SupportsUnitTest (SystemM blk m) where

  type IteratorId (SystemM blk m) = API.Iterator m blk (AllComponents blk)
  type FollowerId (SystemM blk m) = API.Follower m blk (AllComponents blk)
  type Block (SystemM blk m) = blk

  addBlock blk = do
    env <- ask
    SystemM $ lift $ lift $ do
      api <- chainDB <$> readMVar (varDB env)
      void $ API.addBlock api API.noPunishment blk
      pure blk

  persistBlks shouldGarbageCollect = do
    env <- ask
    SystemM $ lift $ lift $ do
      internal <- internal <$> readMVar (varDB env)
      SM.persistBlks shouldGarbageCollect internal

  newFollower = do
    env <- ask
    SystemM $ lift $ lift $ do
      api <- chainDB <$> readMVar (varDB env)
      API.newFollower api (registry env) API.SelectedChain allComponents

  followerInstruction = SystemM . lift . lift . fmap Right
    <$> API.followerInstruction

  followerForward follower points = SystemM $ lift $ lift $ Right
    <$> API.followerForward follower points

  stream from to = do
    env <- ask
    SystemM $ lift $ lift $ fmap Right $ do
      api <- chainDB <$> readMVar (varDB env)
      API.stream api (registry env) allComponents from to

  iteratorNext iterator = SystemM $ lift $ lift (API.iteratorNext iterator)
