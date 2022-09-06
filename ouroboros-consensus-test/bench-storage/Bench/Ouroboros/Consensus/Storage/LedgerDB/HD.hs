{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- | Comparative benchmarks for diff sequence implementations.

  The benchmarks measure the time needed to interpret a sequence of commands
  applied to diff sequences (and auxiliary datatypes). A sequence of commands
  should mimick how diff sequences are used in Consensus.

  The commands can be of four types:
  * Push (append a single diff on the right)
  * Flush (split the sequence at a given point and the take right part)
  * Rollback (split the sequence at a given point and take the left part, roll
    forward a number of blocks)
  * Forward (apply the total diff to a number of values)

  We run these benchmarks for two approaches to diff sequences: the /sub-cached/
  and /anti-diff/ implementations. We compare performance across these two
  approaches. The anti-diff implementation should be more performant.

  === Diff sequence implementations

  In the /sub-cached/ implementation, the sequence of diffs, which is internally
  represented as a @StrictFingerTree@, "caches" at each node of the finger tree
  the cumulative diff of the subtree rooted at that node. A cache at a node can
  be constructed from caches of that node's children, which allows for faster
  than linear time reconstruction of the total cumulative diff but it does incur
  a non-negligible cost when we manipulate diff sequences through splits and
  appends. In particular, we could force recomputing a logarithmic number of
  caches when we perform a split or append. In Consensus (post UTxO-HD), we
  often split and append sequences of diffs: we split when we flush diffs or
  roll back blocks, and we append when we push blocks or roll forward blocks.
  The new /anti-diff/ implementation should reduce the overhead of this caching
  since it only caches the total diff, and not the cumulative diffs of subtrees.
  This total diff can be updated when appending by /summing/ diffs, whereas the
  total diff can be updated when splitting by /subtracting/ diffs. As such, the
  anti-diff implementation uses /Group/ properties of its definition of diffs.
  The sub-cached implementation uses /Monoid/ properties of its definition of
  diffs: it only uses summing to reconstruct a cumulative diff from other diffs.

  === What we measure

  Diff sequences are manipulated by client code in Consensus. To mimick how
  Consensus manipulates diff sequences, we must simulate the client code.
  However, the performance of this simulation is not something we should include
  in our measurements: in these benchmarks, we only care about the performance
  of operations on diff sequences. As such, we off-load the simulation to a
  generator, which generates a sequence of commands in a stateful manner. The
  function that we actually measure the performance of is the @'interpret'@
  function, which interprets the sequence of commands by folding it over an
  initial diff sequence.

  === Stateful generators

  The stateful generators simulate how consensus manipulates diff sequences. The
  top-level stateful generator runs in three parts:
  * Generate an initial model.
  * Run the warmup phase using the initial model.
  * Generate a sequence of commands using the warmed up model.

  The warmup phase ensures that we get past the point where the simulation is
  still "warming up", i.e., when the length of the diff sequence is still less
  than @k@: before that, flushes will not have any effect. To be precise, the
  warmup phase corresponds to running the simulation/generator that generates
  Push commands for exactly @k@ steps and throwing away the commands we generate
  in the process.

  We use the commands that we generate after the warmup phase as inputs to the
  benchmarks, as well as the state of the diff sequence right after the warmup
  phase. Convince yourself that interpretation of these commands only make sense
  with respect to the "warmed up" diff sequence.
-}
module Bench.Ouroboros.Consensus.Storage.LedgerDB.HD (benchmarks) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.RWS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as B
import           Data.Foldable
import           Data.Kind (Type)
import           GHC.Generics (Generic)

import           QuickCheck.GenT
import           Test.QuickCheck hiding (Result, choose, frequency, getSize,
                     resize, shuffle, sized, variant)
import           Test.Tasty.Bench
import           Test.Tasty.QuickCheck hiding (choose, frequency, resize, sized,
                     variant)

import           Test.Util.MockDiffSeq



benchmarks :: Benchmark
benchmarks = bgroup "HD" [ bgroup "DiffSeq/Diff operations" [
        env (mkBenchEnv gcDefault) $
          \ ~bEnv@BenchEnv {cmdsMock, ds0Mock } ->
            bgroup "Mocked diffs" [
                bench "Benchmark interpret" $
                  nf (interpretMock ds0Mock) cmdsMock
              , testProperty "Sanity checks" $
                  once (sanityCheck bEnv)
              ]
    ]]
  where
    mkBenchEnv :: GenConfig -> IO (BenchEnv k v)
    mkBenchEnv conf = do
      CmdGenResult{cmds, ds0, finalModel} <- generate $ generator conf
      pure $ BenchEnv {
          cmdsMock = cmds
        , ds0Mock  = ds0
        , model    = finalModel
        }

    interpretMock = interpret @MockDiffSeq @Key @Value

-- | Inputs to a benchmark.
data BenchEnv k v = BenchEnv {
    cmdsMock :: ![Cmd MockDiffSeq k v]
    -- | State of the diff sequence after the warmup phase during command
    -- generation.
  , ds0Mock  :: !(MockDiffSeq k v)
    -- | Final state of the model after the warmup phase /and/
    -- the command generation phase.
  , model    :: !(Model k v)
  }
  deriving stock Generic
  deriving anyclass NFData

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

type Block (ds :: DiffSeqKind) k v = (SlotNo ds, Diff ds k v)

data Cmd (ds :: DiffSeqKind) k v =
    Push     (SlotNo ds)     (Diff ds k v)
  | Flush    Int
  | Rollback Int             [Block ds k v]
  | Forward  (Values ds k v) (Keys ds k v)
  deriving stock Generic

deriving anyclass instance ( NFData (ds k v)
                           , NFData (Diff ds k v)
                           , NFData (Values ds k v)
                           , NFData (Keys ds k v)
                           , NFData (SlotNo ds)
                           ) => NFData (Cmd ds k v)

deriving stock instance ( Show (ds k v)
                        , Show (Diff ds k v)
                        , Show (Values ds k v)
                        , Show (Keys ds k v)
                        , Show (SlotNo ds)
                        ) => Show (Cmd ds k v)

deriving stock instance ( Eq (ds k v)
                        , Eq (Diff ds k v)
                        , Eq (Values ds k v)
                        , Eq (Keys ds k v)
                        , Eq (SlotNo ds)
                        ) => Eq (Cmd ds k v)

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Interpretation of @Cmd@s as a fold over an initial diff sequence.
interpret ::
     forall ds k v.
     ( IsDiffSeq ds k v
     , NFData (Diff ds k v)
     , NFData (Values ds k v)
     )
  => ds k v
  -> [Cmd ds k v]
  -> ds k v
interpret = foldl' go
  where
    go :: ds k v -> Cmd ds k v -> ds k v
    go ds = \case
      Push sl d     -> push ds sl d
      -- We @'deepseq'@ the total diff of the left part of the flush result,
      -- because that is the part that would be written to disk, for which it
      -- must be fully evaluated. In particular, the performance of computing
      -- the total diff can be different for each diff sequence implementation.
      Flush n       -> let (l, r) = flush n ds
                       in  totalDiff l `deepseq` r
      -- We do not @'deepseq'@ the right part of the rollback result, because we
      -- are throwing it away in the case of a rollback.
      Rollback n bs -> let (l, _r) = rollback n ds
                       in  pushMany l bs
      -- We @'deepseq'@ the result of forwarding values through a total diff,
      -- since forwarding can be implementation-specific in (at least) two ways:
      -- 1. How the total diff is computed.
      -- 2. How the change to a specific key-value pair is computed.
      Forward vs ks -> let vs' = forward vs ks ds
                       in  vs' `deepseq` ds

{-------------------------------------------------------------------------------
  Type class for diff sequences
-------------------------------------------------------------------------------}

-- | The kind of a diff sequence type that is parameterised by the type of keys
-- and values respectively.
--
-- Note: Diffs in this context are diffs for key-value stores (a.k.a. @'Map'@),
-- not diffs for arbitrary datatypes.
type DiffSeqKind = Type -> Type -> Type

-- | Common interface to diff sequence implementations.
class IsDiffSeq (ds :: DiffSeqKind) k v where
  type Diff ds k v   = r | r -> ds k v
  type Values ds k v = r | r -> ds k v
  type Keys ds k v   = r | r -> ds k v
  type SlotNo ds     = r | r -> ds

  -- | Mimicks @'Ouroboros.Consensus.Ledger.Basics.extendDbChangelog.ext'@
  push :: ds k v -> SlotNo ds -> Diff ds k v -> ds k v

  -- | Mimicks @'Ouroboros.Consensus.Ledger.Basics.flushDbChangelog.split'@
  flush :: Int -> ds k v -> (ds k v, ds k v)

  -- | Mimicks @'Ouroboros.Consensus.Ledger.Basics.rollbackDbChangelog.trunc'@
  rollback :: Int -> ds k v -> (ds k v, ds k v)

  -- | Mimicks @'Ouroboros.Consensus.Storage.LedgerDB.HD.forwardValuesAndKeys'@.
  forwardValuesAndKeys ::
       Values ds k v
    -> Keys ds k v
    -> Diff ds k v
    -> Values ds k v

  -- | Mimicks @'Ouroboros.Consensus.Storage.LedgerDB.HD.cumulativeDiffSeqUtxoDiff'@.
  totalDiff :: ds k v -> Diff ds k v

-- | Mimicks @'Ouroboros.Consensus.Storage.LedgerDB.InMemory.forwardTableKeySets.forward'@.
forward ::
     IsDiffSeq ds k v
  => Values ds k v
  -> Keys ds k v
  -> ds k v
  -> Values ds k v
forward vs ks ds = forwardValuesAndKeys vs ks (totalDiff ds)

-- | Mimicks @'Ouroboros.Consensus.Storage.LedgerDB.InMemory.ledgerDbPushMany'@.
pushMany :: IsDiffSeq ds k v => ds k v -> [Block ds k v] -> ds k v
pushMany = foldl (uncurry . push)

{-------------------------------------------------------------------------------
  Type class for diff sequences: concrete implementations
-------------------------------------------------------------------------------}

instance Ord k => IsDiffSeq MockDiffSeq k v where
  type Diff   MockDiffSeq k v = MockDiff k v
  type Values MockDiffSeq k v = MockValues k v
  type Keys   MockDiffSeq k v = MockKeys k v
  type SlotNo MockDiffSeq     = MockSlotNo

  push                 = mPush
  flush                = mFlush
  rollback             = mRollback
  forwardValuesAndKeys = mForwardValuesAndKeys
  totalDiff            = mTotalDiff

{-------------------------------------------------------------------------------
  Configuration types
-------------------------------------------------------------------------------}

-- | Configuration parameters for generators.
data GenConfig = GenConfig {
    -- | Desired length of command sequence
    nrCommands        :: Int
    -- | Security parameter @k@
  , securityParameter :: Int
  }

{-------------------------------------------------------------------------------
  Specific configurations
-------------------------------------------------------------------------------}

gcDefault :: GenConfig
gcDefault = GenConfig {
    nrCommands = 10000
  , securityParameter = 2160
  }

{-------------------------------------------------------------------------------
  Command generator: Model
-------------------------------------------------------------------------------}

-- | Model for keeping track of state during generation of the command sequence.
data Model k v = Model {
    -- | The current diff sequence.
    diffs         :: MockDiffSeq k v
    -- | The tip of the diff sequence.
  , tip           :: Int
    -- | Values that have been flushed to a backing store.
  , backingValues :: MockValues k v
    -- | A counter for the number of commands generated up until now.
  , nrGenerated   :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass NFData

genInitialModel :: GenConfig -> Gen (Model k v)
genInitialModel = error "genInitialModel: not implemented"

newtype Key = Key ShortByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype NFData

newtype Value = Value ShortByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype NFData

-- | UTxO keys are expected to be 32 byte cryptographic hashes.
instance Arbitrary Key where
  arbitrary = Key . B.pack <$> replicateM 32 arbitrary

-- | UTxO entries, i.e., key-value pairs, are expected to be around 100-128 bytes
-- in size. (@32 + 64 ~= 100@)
instance Arbitrary Value where
  arbitrary = Value . B.pack <$> replicateM 64 arbitrary

{-------------------------------------------------------------------------------
  Command generator: Monad transformer stack
-------------------------------------------------------------------------------}

-- | A monad transformer stack that can read from a configuration, has state in
-- in the form of a model, and can peform random generation.
newtype CmdGen k v a = CmdGen (RWST GenConfig () (Model k v) Gen a)
  deriving stock Functor
  deriving newtype ( Applicative, Monad
                   , MonadState (Model k v), MonadReader GenConfig, MonadGen
                   )

instance (MonadGen m, Monoid w) => MonadGen (RWST r w s m) where
  liftGen     = lift . liftGen
  variant n g = RWST $ \r s -> variant n (runRWST g r s)
  sized fg    = RWST $ \r s -> sized (\n -> runRWST (fg n) r s)
  resize n g  = RWST $ \r s -> resize n (runRWST g r s)
  choose      = lift . choose

runCmdGen :: CmdGen k v a -> GenConfig -> Model k v -> Gen (a, Model k v)
runCmdGen (CmdGen mt) conf m = (\(x, s, _) -> (x, s)) <$> runRWST mt conf m

-- | The required result of a top-level generator.
data CmdGenResult k v = CmdGenResult {
    -- | The generated command sequence.
    cmds       :: [Cmd MockDiffSeq k v]
    -- | State of the diff sequence after the warmup phase.
  , ds0        :: MockDiffSeq k v
    -- | Final state of the model after the warmup phase /and/
    -- the command generation phase, like @'model'@.
  , finalModel :: Model k v
  }
  deriving Show

{-------------------------------------------------------------------------------
  Command generator: Main generators
-------------------------------------------------------------------------------}

-- | A stateful generator for sequences of commands.
--
-- Note: We return the @'Model'@ because we want to perform invariant and/or
-- sanity checks on it.
generator :: GenConfig -> Gen (CmdGenResult k v)
generator conf = do
  m <- genInitialModel conf
  ((), m') <- runCmdGen warmup conf m
  (cmds, m'') <- runCmdGen genCmds conf m'
  pure $ CmdGenResult {
      cmds
    , ds0 = diffs m'
    , finalModel = m''
    }

-- | Generate a list of commands.
genCmds :: CmdGen k v [Cmd MockDiffSeq k v]
genCmds = error "genCmds: not implemented"

-- | Simulate the warmup phase where there have not yet been at least @k@
-- pushes.
warmup :: CmdGen k v ()
warmup = error "warmup: not implemented"

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

-- | Sanity check benchmark inputs.
sanityCheck :: BenchEnv k v -> Property
sanityCheck _ = property True
