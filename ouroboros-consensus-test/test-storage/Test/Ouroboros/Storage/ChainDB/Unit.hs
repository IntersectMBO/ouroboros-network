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
module Test.Ouroboros.Storage.ChainDB.Unit (
    run
  , tests
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block (ChainUpdate, Point)
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import           Test.Ouroboros.Storage.ChainDB.Model (Model)
import           Test.Ouroboros.Storage.ChainDB.StateMachine (AllComponents,
                     ChainDBEnv (..), ChainDBState (..), TestConstraints,
                     allComponents, close, mkTestCfg, open)
import           Test.Ouroboros.Storage.TestBlock
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, assertFailure, testCase)

import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Util.ResourceRegistry (closeRegistry,
                     unsafeNewRegistry)
import           Test.Util.ChainDB (MinimalChainDbArgs (..), emptyNodeDBs,
                     fromMinimalChainDbArgs, nodeDBsVol)

import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks as ImmutableDB

run :: IO ()
run = defaultMain tests


tests :: TestTree
tests = testGroup "First follower instruction isJust on empty ChainDB"
  [ testCase "model" $ runModel' getFollowerInstruction
  , testCase "system" $ runSystem' getFollowerInstruction
  ]

getFollowerInstruction :: (SupportsUnitTest m, MonadError TestFailure m) => m ()
getFollowerInstruction = do
  f <- newFollower
  followerInstruction f >>= \case
    Right instr -> isJust instr `orFailWith` "Expecting a follower instruction"
    Left err    -> failWith $ "ChainDbError"

ouroboros_network_4183 :: (Block m ~ TestBlock, SupportsUnitTest m, MonadError TestFailure m)
                       => m ()
ouroboros_network_4183 =
  let fork i = TestBody i True
  in do
    b1 <- addBlock $ firstEBB (const True) $ fork 0
    b2 <- addBlock $ mkNextBlock b1 0 $ fork 0
    b3 <- addBlock $ mkNextBlock b2 1 $ fork 0
    b4 <- addBlock $ mkNextBlock b3 2 $ fork 1
    f <- newFollower

    pure ()

-- | Helper function to run the test against the model and translate to something
-- that HUnit likes.
runModel' :: ModelM TestBlock a -> IO ()
runModel' expr = toAssertion (runModel newModel topLevelConfig expr)
  where
    chunkInfo = ImmutableDB.simpleChunkInfo 100
    newModel = Model.empty testInitExtLedger 0
    topLevelConfig = mkTestCfg chunkInfo

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

orFailWith :: (MonadError TestFailure m) => Bool -> String -> m ()
orFailWith b msg = unless b $ failWith msg

failWith :: (MonadError TestFailure m) => String -> m ()
failWith msg = throwError (TestFailure msg)

-- | SupportsUnitTests for the test expression need to instantiate this class.
class SupportsUnitTest m where
  type FollowerId m
  type Block m

  addBlock
    :: Block m -> m (Block m)

  newFollower
    :: m (FollowerId m)

  followerInstruction
    :: FollowerId m
    -> m (Either (API.ChainDbError (Block m))
                 (Maybe (ChainUpdate (Block m) (AllComponents (Block m)))))
  -- forwardFollower
  --   :: FollowerId m -> [Point blk] -> Point blk


-- ==== Model =====

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


instance (Model.ModelSupportsBlock blk, LedgerSupportsProtocol blk, Eq blk)
      => SupportsUnitTest (ModelM blk) where

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
      Left err -> pure $ Left err
      Right (mChainUpdate, model') -> do
        put model'
        pure $ Right mChainUpdate

  addBlock blk = do
    model <- get
    topLevelConfig <- ask
    let model' = Model.addBlock topLevelConfig blk model
    put model'
    pure blk

  -- followerForward followerId points = do
  --   model <- get

-- ==== System ====

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


instance IOLike m => SupportsUnitTest (SystemM blk m) where

  type FollowerId (SystemM blk m) = API.Follower m blk (AllComponents blk)
  type Block (SystemM blk m) = blk

  newFollower = do
    env <- ask
    SystemM $ lift $ lift $ do
      api <- chainDB <$> readMVar (varDB env)
      API.newFollower api (registry env) API.SelectedChain allComponents

  followerInstruction = SystemM . lift . lift . fmap Right <$> API.followerInstruction
