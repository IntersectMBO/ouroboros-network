{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
module Test.Ouroboros.Storage.ChainDB.Unit (tests) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block (ChainUpdate, HasHeader)
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import           Test.Ouroboros.Storage.ChainDB.Model (Model)
import           Test.Ouroboros.Storage.ChainDB.StateMachine (AllComponents,
                     ChainDBEnv (..), ChainDBState (..), TestConstraints,
                     allComponents, close, mkTestCfg, open)
import           Test.Ouroboros.Storage.TestBlock
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertFailure, testCase)

import           Ouroboros.Consensus.Util.ResourceRegistry (closeRegistry,
                     unsafeNewRegistry)
import           Test.Util.ChainDB (MinimalChainDbArgs (..), emptyNodeDBs,
                     fromMinimalChainDbArgs, nodeDBsVol)

import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks as ImmutableDB

tests :: TestTree
tests = testGroup "trivial"
  [ testCase "model" $ runModel' getFollowerInstruction
  , testCase "system" $ runSystem' getFollowerInstruction
  ]

-- | Helper function to run the test against the model and translate to something
-- that HUnit likes.
runModel' :: ModelM TestBlock a -> IO ()
runModel' expr = toAssertion (runModel newModel expr)
  where newModel = Model.empty testInitExtLedger 0

-- | Helper function to run the test against the actual chain database and
-- translate to something that HUnit likes.
runSystem' :: SystemM TestBlock IO a -> IO ()
runSystem' expr = runSystem withChainDbEnv expr >>= toAssertion
  where
    chunkInfo = ImmutableDB.simpleChunkInfo 100
    topLevelConfig = mkTestCfg chunkInfo
    withChainDbEnv = withTestChainDbEnv topLevelConfig chunkInfo testInitExtLedger

newtype TestFailure = TestFailure String deriving (Show)

toAssertion :: Either TestFailure a -> Assertion
toAssertion (Left (TestFailure t)) = assertFailure t
toAssertion (Right _)              = pure ()

assert :: (MonadError TestFailure m) => Bool -> m ()
assert b = unless b $ throwError (TestFailure "boom")

-- | Targets for the test expression need to instantiate this class.
class Target m where
  type FollowerId m
  type Block m

  newFollower
    :: m (FollowerId m)
  followerInstruction
    :: FollowerId m -> m (Maybe (ChainUpdate (Block m) (AllComponents (Block m))))

getFollowerInstruction :: (Target m, MonadError TestFailure m) => m ()
getFollowerInstruction = do
  f <- newFollower
  instr <- followerInstruction f
  assert $ isJust instr

-- | Tests against the model run in this monad.
newtype ModelM blk a = ModelM
  { runModelM :: StateT (Model blk) (Except TestFailure) a
  } deriving newtype (Functor, Applicative, Monad,
                      MonadState (Model blk), MonadError TestFailure)

runModel :: Model blk -> ModelM blk b -> Either TestFailure b
runModel model expr
  = runExcept (evalStateT (runModelM expr) model)

-- | Tests against the actual chain database run in this monad.
newtype SystemM blk m a = SystemM
  { runSystemM :: ReaderT (ChainDBEnv m blk) (ExceptT TestFailure m) a
  } deriving newtype (Functor, Applicative, Monad,
                      MonadReader (ChainDBEnv m blk), MonadError TestFailure)

runSystem
  :: (forall a. (ChainDBEnv m blk -> m a) -> m a)
  -> SystemM blk m b
  -> m (Either TestFailure b)
runSystem withChainDbEnv expr
  = withChainDbEnv $ runExceptT . runReaderT (runSystemM expr)

-- | Provide a standard ChainDbEnv for testing.
withTestChainDbEnv
  :: (IOLike m, TestConstraints blk)
  => TopLevelConfig blk
  -> ImmutableDB.ChunkInfo
  -> ExtLedgerState blk
  -> (ChainDBEnv m blk -> m a)
  -> m a
withTestChainDbEnv topLevelConfig chunkInfo extLedgerState
  = bracket openChainDbEnv closeChainDbEnv
  where
    openChainDbEnv = do
      threadRegistry <- unsafeNewRegistry
      iteratorRegistry <- unsafeNewRegistry
      varCurSlot <- uncheckedNewTVarM 0
      varNextId <- uncheckedNewTVarM 0
      nodeDbs <- emptyNodeDBs
      let args = chainDbArgs threadRegistry nodeDbs
      varDB <- open args >>= newMVar
      pure ChainDBEnv
        { varDB
        , registry = iteratorRegistry
        , varCurSlot
        , varNextId
        , varVolatileDbFs = nodeDBsVol nodeDbs
        , args
        }

    closeChainDbEnv env = do
      readMVar (varDB env) >>= close
      closeRegistry (registry env)
      closeRegistry (cdbRegistry $ args env)

    chainDbArgs registry nodeDbs = fromMinimalChainDbArgs MinimalChainDbArgs
      { mcdbTopLevelConfig = topLevelConfig
      , mcdbChunkInfo = chunkInfo
      , mcdbInitLedger = extLedgerState
      , mcdbRegistry = registry
      , mcdbNodeDBs = nodeDbs
      }

instance IOLike m => Target (SystemM blk m) where

  type FollowerId (SystemM blk m) = API.Follower m blk (AllComponents blk)
  type Block (SystemM blk m) = blk

  newFollower = do
    env <- ask
    SystemM $ lift $ lift $ do
      api <- chainDB <$> readMVar (varDB env)
      API.newFollower api (registry env) API.SelectedChain allComponents

  followerInstruction = SystemM . lift . lift . API.followerInstruction

instance Model.ModelSupportsBlock blk => Target (ModelM blk) where

  type FollowerId (ModelM blk) = Model.FollowerId
  type Block (ModelM blk) = blk

  newFollower = do
    model <- get
    let (followerId, model') = Model.newFollower model
    put model'
    pure $ followerId

  followerInstruction followerId = do
    model <- get
    case Model.followerInstruction followerId allComponents model of
      Left _ -> error "Failure"
      Right (mChainUpdate, model') -> do
        put model'
        pure mChainUpdate
