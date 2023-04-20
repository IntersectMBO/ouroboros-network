{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Bench.Consensus.Mempool.Scenario (
    -- * Scenario
    Scenario
  , fromScenario
    -- * Scenario builder
  , ScBuilder
  , build
    -- ** Compound functions
  , theBackingStoreHas
  , theCandidateTransactionsConsume
  , theCandidateTransactionsHave
  , theCandidateTransactionsHaveLinkedTxs
  , theChangelogConsumes
  , theChangelogHas
  , theChangelogHasLinkedTxs
  ) where

import           Bench.Consensus.Mempool (MempoolCmd, mkSimpleTryAdd)
import           Bench.Consensus.Mempool.Params (InitialMempoolAndModelParams,
                     ledgerStateFromTokens, mkParams, testBlocksFromTxs)
import           Bench.Consensus.Mempool.TestBlock (GenTx (TestBlockGenTx),
                     TestBlock, Token, Tx (..), mkTx)
import           Control.Exception (assert)
import           Control.Monad.State.Strict (MonadState, State, StateT (StateT),
                     execState, gets, modify, replicateM)
import qualified Data.Set as Set
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.Tables (ValuesMK)
import           Ouroboros.Consensus.Storage.LedgerDB.Init
                     (BackingStoreSelector (..))
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB
                     (LedgerDbCfg (..))

{-------------------------------------------------------------------------------
  Scenario
-------------------------------------------------------------------------------}

-- | A benchmark scenario determines the inputs for a mempool benchmark: the
-- mempool parameters ('InitialMempoolAndModelParams') and the workload
-- (['MempoolCmd' 'TestBlock']). See 'fromScenario'.
--
-- INVARIANT: each of the three fields can only be set once in the 'ScBuilder'
-- monad.
data Scenario = Scenario {
    -- | Corresponds to the 'immpBackingState' field of
    -- 'InitialMempoolAndModelParams'.
    scBackingState    :: !(LedgerState TestBlock ValuesMK)
    -- | Corresponds to the 'immpChangelogBlocks' field of
    -- 'InitialMempoolAndModelParams'.
  , scChangelogBlocks :: ![TestBlock]
    -- | Mempool commands to run.
  , scMempoolCommands :: ![MempoolCmd TestBlock]
  }

-- | Convert a 'Scenario' into mempool parameters and a benchmark workload
-- (commands).
fromScenario ::
     LedgerDbCfg (LedgerState TestBlock)
  -> BackingStoreSelector m
  -> Scenario
  -> (InitialMempoolAndModelParams m TestBlock, [MempoolCmd TestBlock])
fromScenario ldbCfg bss sc =
  (mkParams
    (scBackingState sc)
    (scChangelogBlocks sc)
    ldbCfg
    bss
  , scMempoolCommands sc
  )

{-------------------------------------------------------------------------------
  Scenario builder
-------------------------------------------------------------------------------}

-- | A monadic interface to constructing benchmark scenarios.
newtype ScBuilder a = ScBuilder { runScBuilder :: State St a }
  deriving newtype (Functor, Applicative, Monad, MonadState St)

-- | Internal state for the 'ScenarioBuilder' monad.
data St = St {
    -- | A new, unique 'Token'. Reading this field should always be followed by
    -- an increment.
    stNextToken :: !Token
    -- | The scenario being built.
  , stScenario  :: !Scenario
  }

initialSt :: St
initialSt = St 0 (Scenario (ledgerStateFromTokens []) [] [])

execScBuilder :: ScBuilder a -> St -> St
execScBuilder = execState . runScBuilder

build :: ScBuilder () -> Scenario
build m = stScenario (execScBuilder m initialSt)

{-------------------------------------------------------------------------------
  Scenario builder: 'MonadState' extras
-------------------------------------------------------------------------------}

modifyScenario :: MonadState St m => (Scenario -> Scenario) -> m ()
modifyScenario f = modify (\st -> st {
      stScenario = f $ stScenario st
    })

putBackingState :: MonadState St m => LedgerState TestBlock ValuesMK -> m ()
putBackingState lst = modifyScenario $ \sc ->
    precondition sc $ sc {scBackingState = lst }
  where
    precondition sc = assert (putBackingStateOnlyOnce sc)
    putBackingStateOnlyOnce sc = scBackingState sc == ledgerStateFromTokens []

putChangelogBlocks :: MonadState St m => [TestBlock] -> m ()
putChangelogBlocks blks = modifyScenario $ \sc ->
    precondition sc $ sc { scChangelogBlocks = blks }
  where
    precondition sc = assert (putChangelogBlocksOnlyOnce sc)
    putChangelogBlocksOnlyOnce sc = null $ scChangelogBlocks sc

putMempoolCommands :: MonadState St m => [MempoolCmd TestBlock] -> m ()
putMempoolCommands cmds = modifyScenario $ \sc ->
    precondition sc $ sc { scMempoolCommands = cmds }
  where
    precondition sc = assert (putMempoolCommandsOnlyOnce sc)
    putMempoolCommandsOnlyOnce sc = null $ scMempoolCommands sc

{-------------------------------------------------------------------------------
  Scenario builder: compound functions
-------------------------------------------------------------------------------}

-- | Creates @n@ transactions that consume nothing and each produce 1 'Token'.
theBackingStoreHas :: MonadState St m => Int -> m [Tx]
theBackingStoreHas n = do
    bTxs <- replicateM n consumeNoneAndProduceOne
    let fullProduced = Set.toList $ foldMap produced bTxs
    putBackingState $ ledgerStateFromTokens fullProduced
    pure bTxs

-- | Creates @n@ transactions that each consume one of the given 'Tx's, and
-- produce 1 'Token'.
theChangelogConsumes :: MonadState St m => [Tx] -> m [Tx]
theChangelogConsumes txs = do
    cTxs <- mapM consumeOneAndProduceOne txs
    putChangelogBlocks $ testBlocksFromTxs cTxs
    pure cTxs

-- | Creates @n@ transactions that consume nothing and each produce 1 'Token'.
theChangelogHas :: MonadState St m => Int -> m [Tx]
theChangelogHas n = do
  cTxs <- replicateM n consumeNoneAndProduceOne
  putChangelogBlocks $ testBlocksFromTxs cTxs
  pure cTxs

-- | @'theChangelogHasLinkedTxs' n@ creates @n@ transactions, where each
-- transaction consumes the tokens of its predecessor, and produces tokens for
-- its successor.
--
-- All 'Tx's consume and produce 1 'Token'. However, the first 'Tx' does not
-- consume any 'Token's.
theChangelogHasLinkedTxs :: MonadState St m => Int -> m [Tx]
theChangelogHasLinkedTxs n = do
  cTxs <- linkedTxs Nothing n
  putChangelogBlocks $ testBlocksFromTxs cTxs
  pure cTxs

-- | Creates @n@ transactions that each consume one of the given 'Tx's, and
-- produce 1 'Token'.
theCandidateTransactionsConsume :: MonadState St m => [Tx] -> m ()
theCandidateTransactionsConsume txs = do
    mTxs <- mapM consumeOneAndProduceOne txs
    putMempoolCommands $ fmap (mkSimpleTryAdd . TestBlockGenTx) mTxs

-- | Creates @n@ transactions that consume nothing and each produce 1 'Token'.
theCandidateTransactionsHave :: MonadState St m => Int -> m ()
theCandidateTransactionsHave n = do
    mTxs <- replicateM n consumeNoneAndProduceOne
    putMempoolCommands $ fmap (mkSimpleTryAdd . TestBlockGenTx) mTxs

-- | @'theCandidateTransactionsHaveLinkedTxs' prevtx n@ creates @n@
-- transactions, where each transaction consumes the tokens of its predecessor,
-- and produces tokens for its successor.
--
-- All 'Tx's consume and produce 1 'Token'. However, the first 'Tx' consumes
-- @prevtx@'s tokens if @prevtx /= Nothing@.
theCandidateTransactionsHaveLinkedTxs ::
     MonadState St m
  => Maybe Tx -> Int -> m ()
theCandidateTransactionsHaveLinkedTxs prevtx n = do
    mTxs <- linkedTxs prevtx n
    putMempoolCommands $ fmap (mkSimpleTryAdd . TestBlockGenTx) mTxs

{-------------------------------------------------------------------------------
  Scenario builder: utilities
-------------------------------------------------------------------------------}

-- | Monadic equivalent to 'iterate'. The 'Int' parameter ensures termination.
--
-- Note: based on 'iterateM' from @monad-extras-0.6.0@.
iterateM :: forall m a. Monad m => Int -> (a -> m a) -> m a -> m [a]
iterateM n0 f xm0
  | n0 <= 0   = error "iterateM: n <= 0"
  | otherwise = go n0 xm0
  where
    go :: Int -> m a -> m [a]
    go 0 _  = pure []
    go n xm = do
      x <- xm
      (x:) <$> go (n-1) (f x)

{-------------------------------------------------------------------------------
  Scenario builder: primitives
-------------------------------------------------------------------------------}

newToken :: MonadState St m => m Token
newToken = do
  nxtTok <- gets stNextToken
  modify (\st -> st { stNextToken = nxtTok + 1 })
  pure nxtTok

-- | @'consumeAllAndProduceN' n txsToConsume@ creates a 'Tx' that consumes all
-- 'Token's produced by the 'Tx's in @txsToConsume@, and produces @n@ new
-- 'Token's.
consumeAllAndProduceN :: MonadState St m => Int -> [Tx] ->  m Tx
consumeAllAndProduceN n txsToConsume = do
  let toksToConsume = Set.toList $ foldMap produced txsToConsume
  toksToProduce <- replicateM n newToken
  pure $ mkTx toksToConsume toksToProduce

-- | @'consumeOneAndProduceOne' txToConsume@ creates a 'Tx' that consumes all
-- 'Token's produced by @txToConsume@, and produces @1@ new 'Token'.
consumeOneAndProduceOne :: MonadState St m => Tx -> m Tx
consumeOneAndProduceOne txToConsume = consumeAllAndProduceN 1 [txToConsume]

-- | @'consumeOneAndProduceOne'@ creates a 'Tx' that consumes no tokens,
-- and produces @1@ new 'Token'.
consumeNoneAndProduceOne ::  MonadState St m => m Tx
consumeNoneAndProduceOne = consumeAllAndProduceN 1 []

-- | @'linkedTxs' prevMay n@ creates a list of linked 'Tx's, where each 'Tx'
-- consumes tokens that the previous 'Tx' produces.
--
-- >    linkedTxs (Just $ mkTx [0] [1]) 3
-- > =~ [mkTx [1] [2], mkTx [2] [3], mkTx [4] [5]]
--
-- >    linkedTxs Nothing 2
-- > =~ [mkTx [] [1], 'mkTx' [1] [2]]
linkedTxs ::
     forall m. MonadState St m
  => Maybe Tx -- ^ A possible 'Tx' to link the first 'Tx' to.
  -> Int
  -> m [Tx]
linkedTxs _ 0         = pure []
linkedTxs prevtxMay n = iterateM n consumeOneAndProduceOne $
  maybe consumeNoneAndProduceOne consumeOneAndProduceOne prevtxMay
