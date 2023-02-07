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
  * Switch (split the sequence at a given point and take the left part, roll
    forward a number of blocks)
  * Forward (apply the total diff to a number of values)

  We run these benchmarks for two approaches to diff sequences: the
  /intermediate-sums/ and /anti-diff/ implementations. We compare performance
  across these two approaches. The anti-diff implementation should be more
  performant.

  === Diff sequence implementations

  A possible way to represent a sequence of monoidal sums of differences (i.e.,
  a /diff sequence/), is by means of a fingertree, where the leaves contain the
  differences in the sequence, and the intermediate nodes contain the cumulative
  sums of the leaves. In this way, the root of the tree contains the cumulative
  sum of all the differences in the sequence. We call this representation
  /intermediate cumulative sum of diffs/, or /intermediate-sums/ for short.
  Because this representation stores intermediate sums in its nodes, we are
  essentially "caching" intermediate sums, which allows for relatively fast
  reconstruction of intermediate sums from cached intermediate sums if the
  fingertree is manipulated.

  However, this representation does incur a non-neglible cost in the context of
  Consensus, where we split and append sequences of diffs just as often as we
  use the total cumulative diff: we split when we flush diffs or roll back
  blocks, and we append when we push blocks or roll forward blocks. What is
  costly is that each split or append requires a logarithmic number of
  intermediate sums to be reconstructed.

  The new /anti-diff/ implementation, where we require the existence of an
  inverse operation on diffs (hence the name anti-diff), should reduce the costs
  of reconstructing the total cumulative diff. In particular, instead of storing
  intermediate sums of diffs, this implementation stores only the total sum.
  This total diff can be updated when appending by /summing/ diffs, whereas the
  total diff can be updated when splitting by /subtracting/ diffs. As such,
  diffs in the anti-diff implementation form a @Group@, whereas diffs in the
  intermediate-sums implementations form a @Monoid@.

  === What we measure

  We want to benchmark only the operations on sequence of differences. However,
  these operations need to be consistent with respect to the diff sequence to
  which they are applied. For instance, we should only delete values that were
  previously inserted in the diff sequence, and we should only insert keys once.
  In other words, there is a dependency between the generated commands: each
  generated command has to account for commands that have been generated before.
  For this reason, commands are generated in a stateful manner, but we do not
  bechmkark the generation process. The function that we actually measure the
  performance of is the @'interpret'@ function, which interprets the sequence of
  commands by folding it over an initial diff sequence.

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
  phase.
-}
module Bench.Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq (benchmarks) where

import           Control.DeepSeq (NFData, deepseq)
import qualified Control.Exception as Exn
import           Control.Monad (replicateM, replicateM_)
import           Control.Monad.RWS (MonadReader (..), MonadState,
                     MonadTrans (lift), RWST (..), gets, modify)

import           Data.Bifunctor (Bifunctor (second))
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as B
import qualified Data.FingerTree.RootMeasured.Strict as RMFT
import qualified Data.FingerTree.Strict as FT
import           Data.Foldable (Foldable (foldl', toList))
import           Data.Kind (Type)
import qualified Data.Map.Diff.Strict as MapDiff
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq (..))
import           Data.Sequence.NonEmpty (NESeq (..))

import           GHC.Generics (Generic)

import           QuickCheck.GenT (Arbitrary (arbitrary), Gen, MonadGen (..),
                     arbitrary', frequency, shuffle)
import           Test.Tasty.Bench (Benchmark, bcompare, bcompareWithin, bench,
                     bgroup, env, nf)
import           Test.Tasty.QuickCheck (Positive (getPositive), Property,
                     Small (getSmall), arbitraryBoundedIntegral, conjoin,
                     counterexample, forAll, generate, once, testProperty,
                     (===))

import           Ouroboros.Consensus.Block.Abstract (SlotNo (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS

import           Test.Util.Orphans.NFData ()

{-------------------------------------------------------------------------------
  Benchmarks
-------------------------------------------------------------------------------}

benchmarks :: Benchmark
benchmarks = bgroup "DiffSeq" [
      benchComparative
        "Configuration: default"
        Nothing
        (mkBenchEnv @Key @Value gcDefault)
    , benchComparative
        "Configuration: no switches"
        Nothing
        (mkBenchEnv @Key @Value gcNoSwitches)
    , benchComparative
        "Configuration: only small switches"
        Nothing
        (mkBenchEnv @Key @Value gcSmallSwitches)
    , benchComparative
        "Configuration: no switches, larger flushes (every 100 pushes)"
        Nothing
        (mkBenchEnv @Key @Value gcNoSwitchesLargerFlush)
    , testProperty "interpret DiffSeq == interpret SeqUtxoDiff" $
        forAll (generator gcDefault TestMode) $
          \CmdGenResult {cmds, ds0} ->
              interpretAntidiff ds0 cmds
            ===
              fromSeqUtxoDiff (
                  interpretIntermediateSum
                    (fromDiffSeq ds0)
                    (fmap fromDiffSeqCmd cmds)
                )
    ]
  where
    mkBenchEnv ::
         forall k v. (Ord k, Eq v, Arbitrary k, Arbitrary v)
      => GenConfig
      -> IO (BenchEnv k v)
    mkBenchEnv conf = do
      CmdGenResult{cmds, ds0, finalModel} <- generate $ generator conf BenchMode
      pure $ BenchEnv {
          cmdsAntidiff        = cmds
        , cmdsIntermediateSum = fmap fromDiffSeqCmd cmds
        , ds0Antidiff         = ds0
        , ds0IntermediateSum  = fromDiffSeq ds0
        , model               = finalModel
        }

    interpretAntidiff        = interpret @DS.DiffSeq     @Key @Value
    interpretIntermediateSum = interpret @HD.SeqUtxoDiff @Key @Value

-- | Inputs to a benchmark.
--
-- As a convention, we use @ds0@ to indicate the state of the diff sequence
-- after the warmup phase during command generation.
data BenchEnv k v = BenchEnv {
    cmdsAntidiff        :: ![Cmd DS.DiffSeq k v]
  , cmdsIntermediateSum :: ![Cmd HD.SeqUtxoDiff k v]
  , ds0Antidiff         :: !(DS.DiffSeq k v)
  , ds0IntermediateSum  :: !(HD.SeqUtxoDiff k v)
    -- | Final state of the model after the warmup phase /and/
    -- the command generation phase.
  , model               :: !(Model k v)
  }
  deriving stock Generic
  deriving anyclass NFData

benchComparative ::
     (Show k, Ord k, Show v, Eq v, NFData k, NFData v)
  => String
  -> Maybe (Double, Double)
  -> IO (BenchEnv k v)
  -> Benchmark
benchComparative name boundsMay benchEnv =
  bgroup name [
    env benchEnv $
      \ ~bEnv@BenchEnv {cmdsAntidiff, cmdsIntermediateSum, ds0Antidiff, ds0IntermediateSum} ->
        bgroup "Comparative performance analysis" [
          maybe bcompare (uncurry bcompareWithin) boundsMay target $
            bench "DiffSeq" $
              nf (interpret ds0Antidiff) cmdsAntidiff
        ,   bench "SeqUtxoDiff" $
              nf (interpret ds0IntermediateSum) cmdsIntermediateSum
        , testProperty "Sanity checks" $
            once (sanityCheck bEnv)
        ]
  ]
  where
    target :: String
    target =
      "    $(NF-2) == \"" <> name <> "\" \
      \ && $NF     == \"SeqUtxoDiff\"     "

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

type Pushes (ds :: DiffSeqKind) k v = [(SlotNo, Diff ds k v)]

data Cmd (ds :: DiffSeqKind) k v =
    Push     SlotNo          (Diff ds k v)
  | Flush    Int
  | Switch Int               (Pushes ds k v)
  | Forward  (Values ds k v) (Keys ds k v)
  deriving stock Generic

deriving anyclass instance ( NFData (ds k v)
                           , NFData (Diff ds k v)
                           , NFData (Values ds k v)
                           , NFData (Keys ds k v)
                           ) => NFData (Cmd ds k v)

deriving stock instance ( Show (ds k v)
                        , Show (Diff ds k v)
                        , Show (Values ds k v)
                        , Show (Keys ds k v)
                        ) => Show (Cmd ds k v)

deriving stock instance ( Eq (ds k v)
                        , Eq (Diff ds k v)
                        , Eq (Values ds k v)
                        , Eq (Keys ds k v)
                        ) => Eq (Cmd ds k v)

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Interpretation of @Cmd@s as a fold over an initial diff sequence.
--
-- We take care to account for all costs related to diff sequences by forcing
-- evaluation of some expressions using @'deepseq'@.
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
      -- are throwing it away.
      Switch n ps   -> let (l, _r) = rollback n ds
                       in  pushMany l ps
      -- We @'deepseq'@ the result of forwarding values, because all the
      -- forwarded key-value pairs would be used in block application, meaning
      -- that they would be fully evaluated at some point.
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

  -- | Mimicks @'Ouroboros.Consensus.Ledger.Basics.extendDbChangelog.ext'@
  push :: ds k v -> SlotNo -> Diff ds k v -> ds k v

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
pushMany :: IsDiffSeq ds k v => ds k v -> Pushes ds k v -> ds k v
pushMany = foldl (uncurry . push)

{-------------------------------------------------------------------------------
  Type class for diff sequences: concrete implementations
-------------------------------------------------------------------------------}

instance (Ord k, Eq v) => IsDiffSeq DS.DiffSeq k v where
  type Diff DS.DiffSeq k v   = DS.Diff k v
  type Values DS.DiffSeq k v = DS.Values k v
  type Keys DS.DiffSeq k v   = DS.Keys k v

  push                 = DS.extend
  flush                = DS.splitAt
  rollback             = DS.splitAtFromEnd
  forwardValuesAndKeys = DS.applyDiffForKeys
  totalDiff            = DS.cumulativeDiff

instance Ord k => IsDiffSeq HD.SeqUtxoDiff k v where
  type Diff HD.SeqUtxoDiff k v    = HD.UtxoDiff k v
  type Values HD.SeqUtxoDiff k v  = HD.UtxoValues k v
  type Keys HD.SeqUtxoDiff k v    = HD.UtxoKeys k v

  push                 = HD.extendSeqUtxoDiff
  flush                = HD.splitAtSeqUtxoDiff
  rollback             = HD.splitAtFromEndSeqUtxoDiff
  forwardValuesAndKeys = HD.forwardValuesAndKeys
  totalDiff            = HD.cumulativeDiffSeqUtxoDiff

{-------------------------------------------------------------------------------
  Configuration types
-------------------------------------------------------------------------------}

-- | Configuration parameters for generators.
data GenConfig = GenConfig {
    -- | Desired length of command sequence
    nrCommands        :: Int
    -- | Security parameter @k@
  , securityParameter :: Int
    -- | The number of initial backing values
  , nrInitialValues   :: Int
  , pushConfig        :: PushConfig
  , forwardConfig     :: ForwardConfig
  , switchConfig      :: SwitchConfig
    -- | An infinite list of directives that determines which commands to
    -- generate.
    --
    -- If we run out of directives without generating @nrCommands@ commands, we
    -- would not know how to proceed generating more commands. Therefore, since
    -- @nrCommands@ can be an arbitrary number, we would want the number of
    -- directives to be /at least/ `nrCommands`.  To ensure that we never run
    -- out of directives, the safest bet is to make the list of directives
    -- infinite, for example by defining it to be an infinitely repeating
    -- series.
    --
    -- @Nothing@ signals that we do not want to use any directives. @Just []@
    -- should result in an error, for the reason we mentioned in the paragraph
    -- above.
  , directives        :: Maybe [Directive]
  }

-- | Configuration parameters for @'Push'@ command generation
data PushConfig = PushConfig {
    -- | How many key-value pairs to delete
    nrToDelete :: Int
    -- | How many key-value pairs to insert
  , nrToInsert :: Int
  }

-- | Configuration parameters for @'Forward'@ command generation.
newtype ForwardConfig = ForwardConfig {
    -- | How many keys to forward
    nrToForward :: Int
  }

-- | Configuration parameters for @'Switch'@ command generation.
newtype SwitchConfig = SwitchConfig {
    -- | Distribution of rollback sizes with respect to the security parameter
    -- @k@.
    --
    -- Each entry @(x, (lb, ub))@ defines a QuickCheck frequency integer @x@,
    -- and inclusive lower and upper bounds @lb@ and @ub@. @(lb, ub)@ is used
    -- to define the range of values from which we can pick values using
    -- @'choose'@. As such, @(lb, ub)@ determines the possible sizes of a
    -- rollback, while @x@ determines the probability of picking from the range
    -- @(lb, ub)@.
    --
    -- PRECONDITION: @0 <= lb <= ub <= k@ should hold.
    rollbackDistribution :: Maybe [(Int, (Int, Int))]
  }

-- | A directive to generate a specific command.
data Directive = GenForward | GenPush | GenFlush | GenSwitch
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Specific configurations
-------------------------------------------------------------------------------}

gcDefault :: GenConfig
gcDefault = GenConfig {
      nrCommands        = 10000
    , securityParameter = 2160
    , nrInitialValues   = 100
    , pushConfig
    , forwardConfig
    , switchConfig
    , directives
    }
  where
    pushConfig = PushConfig {
        nrToDelete = 50
      , nrToInsert = 50
      }

    forwardConfig = ForwardConfig {
        nrToForward = 50
      }

    switchConfig = SwitchConfig {
        rollbackDistribution = Just [(95, (1, 10)), (5, (1000, 2000))]
      }

    directives =
      let
        xs = concat $
          zipWith
            ((. return ) . (:))
            (replicate 10 GenForward)
            (replicate 10 GenPush)
        ys = concat $ replicate 10 (xs ++ [GenFlush])
        zs = ys ++ [GenSwitch]
      in
        Just $ concat $ repeat zs

-- | No switches.
gcNoSwitches :: GenConfig
gcNoSwitches = gcDefault {
    directives =
      let
        xs = concat $
          zipWith
            ((. return ) . (:))
            (replicate 10 GenForward)
            (replicate 10 GenPush)
        ys = xs ++ [GenFlush]
      in
        Just $ concat $ repeat ys
  }

-- | Only small switches.
gcSmallSwitches :: GenConfig
gcSmallSwitches = gcDefault {
    switchConfig = (switchConfig gcDefault) {
        rollbackDistribution = Just [(1, (1, 10))]
      }
  }

-- | Larger flushes, no switches.
gcNoSwitchesLargerFlush :: GenConfig
gcNoSwitchesLargerFlush = gcDefault {
    directives =
      let
        xs = concat $
          zipWith
            ((. return ) . (:))
            (replicate 100 GenForward)
            (replicate 100 GenPush)
        ys = xs ++ [GenFlush]
      in
        Just $ concat $ repeat ys
  }

{-------------------------------------------------------------------------------
  Command generator: Model
-------------------------------------------------------------------------------}

-- | Model for keeping track of state during generation of the command sequence.
data Model k v = Model {
    -- | The current diff sequence.
    diffs         :: DS.DiffSeq k v
    -- | The tip of the diff sequence.
  , tip           :: Int
    -- | Values that have been flushed to a backing store.
  , flushedValues :: Values DS.DiffSeq k v
    -- | A counter for the number of commands generated up until now.
  , nrGenerated   :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass NFData

genInitialModel ::
     (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => GenConfig
  -> Gen (Model k v)
genInitialModel GenConfig{nrInitialValues} = do
    kvs <- replicateM nrInitialValues arbitrary
    pure $ Model DS.empty 0 (DS.valuesFromList kvs) 0

newtype Key = Key ShortByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype NFData

newtype Value = Value ShortByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype NFData

-- | UTxO keys are expected to be 32 byte cryptographic hashes.
--
-- == Note [Use of arbitraryBoundedIntegral]
--
-- We try to simulate a consensus workload as best as possible. In this case, we
-- are simulating operations on a sequence of UTxO (i.e., key-value pairs)
-- differences. UTxO keys are 32-byte cryptographic hashes, where each hash is
-- equally likely to occur, and so if we generate random UTxO-like keys, we
-- should ensure that we pick the keys uniformly random from the whole range of
-- possible 32-byte keys. Previously, we were using
-- @'arbitrarySizedBoundedIntegral'@ to generate the keys: though this picks
-- from the full range of 32-byte keys, it does not pick from a uniform
-- distribution because smaller keys are more likely to be picked.
-- @'arbitraryBoundedIntegral'@ does pick from a uniform distribution, so we use
-- that here.
instance Arbitrary Key where
  arbitrary = Key . B.pack <$> replicateM 32 arbitraryBoundedIntegral

-- | UTxO entries, i.e., key-value pairs, are expected to be around 100-128 bytes
-- in size. (@32 + 64 ~= 100@)
instance Arbitrary Value where
  arbitrary = Value . B.pack <$> replicateM 64 arbitraryBoundedIntegral

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
    cmds       :: [Cmd DS.DiffSeq k v]
    -- | State of the diff sequence after the warmup phase.
  , ds0        :: DS.DiffSeq k v
    -- | Final state of the model after the warmup phase /and/
    -- the command generation phase, like @'model'@.
  , finalModel :: Model k v
  }
  deriving Show

{-------------------------------------------------------------------------------
  Command generator: Main generators
-------------------------------------------------------------------------------}

-- | Mode for the top-level generator.
--
-- * Bench: Generate commands in deterministic order according to the
--   @'directives'@ configuration parameter, and do not modify the passed in
--   configuration parameters.
-- * Test: Generate commands in non-deterministic order, modify the passed in
--   configuration to use sized parameters.
data GenMode = BenchMode | TestMode

-- | A stateful generator for sequences of commands.
--
-- Note: We return the @'Model'@ because we want to perform invariant and/or
-- sanity checks on it.
generator ::
     (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => GenConfig
  -> GenMode
  -> Gen (CmdGenResult k v)
generator preConf gmode = do
  conf <- mkConf
  m <- genInitialModel conf
  ((), m') <- runCmdGen warmup conf m
  (cmds, m'') <- runCmdGen gen conf m'
  pure $ CmdGenResult {
      cmds
    , ds0 = diffs m'
    , finalModel = m''
    }
  where
    gen = case gmode of
      BenchMode -> genCmdsDet
      TestMode  -> genCmdsNonDet

    -- Replace configuration parameters by QuickCheck sized ones if the
    -- generator mode is set to @Test@.
    mkConf = case gmode of
      BenchMode -> pure preConf
      TestMode  -> do
        n <- getPositive <$> arbitrary
        k :: Int <- getSmall . getPositive <$> arbitrary
        pure $ preConf {
            nrCommands        = n
          , securityParameter = k
          , directives        = Nothing
          , switchConfig      = (switchConfig preConf) {
                rollbackDistribution = Nothing
              }
          }

-- | Generate a list of commands in deterministic order.
--
-- Note: The order of commands is deterministic, the inputs for these commands
-- are often semi-randomly generated.
genCmdsDet ::
     forall k v. (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => CmdGen k v [Cmd DS.DiffSeq k v]
genCmdsDet = do
  nrc <- reader nrCommands
  nrg <- gets nrGenerated

  if nrg >= nrc then
    pure []
  else do
    dirsMay <- reader directives

    case dirsMay of
      Nothing       -> error "No directives given."
      Just []       -> error "Unexpectedly ran out of directives."
      Just (dir : dirs) -> do
        c <- genCmdDet dir
        cs <- local (\r -> r { directives = Just dirs }) genCmdsDet
        pure (c : cs)

-- | Generate a specific command according to a directive.
genCmdDet ::
     (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => Directive
  -> CmdGen k v (Cmd DS.DiffSeq k v)
genCmdDet = \case
  GenForward -> genForward
  GenPush    -> genPush
  GenFlush   -> genFlush
  GenSwitch  -> genSwitch

-- | Generate a list of commands in non-deterministic order.
genCmdsNonDet ::
     (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => CmdGen k v [Cmd DS.DiffSeq k v]
genCmdsNonDet = do
  conf <- ask
  replicateM (nrCommands conf) genCmdNonDet

-- | Generate a command based on a probability distribution.
genCmdNonDet ::
     (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => CmdGen k v (Cmd DS.DiffSeq k v)
genCmdNonDet = do
    frequency [
        (100, genPush)
      , (10, genFlush)
      , (1, genSwitch)
      , (100, genForward)
      ]

-- | Simulate the warmup phase where there have not yet been at least @k@
-- pushes.
warmup ::
     forall k v. (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => CmdGen k v ()
warmup = do
  nr <- gets nrGenerated

  k <- reader securityParameter
  replicateM_ k genPush

  nr' <- gets nrGenerated

  modify (\st -> st {
      nrGenerated = 0
    })

  ds   <- gets diffs
  t    <- gets tip
  nr'' <- gets nrGenerated

  let
    invariant = and [
        nr == 0
      , nr' == k
      , nr'' == 0
      , DS.length ds == k
      , t == k
      ]

  Exn.assert invariant $ pure ()

{-------------------------------------------------------------------------------
  Command generator: Individual command generation
-------------------------------------------------------------------------------}

-- | Generate a @'Push'@ command.
--
-- > data Cmd (ds :: DiffSeqKind) k v =
--     Push SlotNo (Diff ds k v)
--     -- ...
--
-- Steps to generate a @'Push'@ command:
-- * Forward flushed values.
-- * "Simulate a transaction" by generating a diff that:
--   1. Deletes one or more values that exist in the forwarded flushed values.
--   2. Inserts one or more values with globally unique keys.
-- * Return a strictly increasing slot number, and the new diff.
-- BOOKKEEPING: Push the new diff onto the model's diff sequence.
genPush ::
     forall k v. (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => CmdGen k v (Cmd DS.DiffSeq k v)
genPush = do
    ds <- gets diffs
    t  <- gets tip
    vs <- gets flushedValues

    let
      vs' = DS.applyDiff vs (totalDiff ds)

    d1 <- valuesToDelete vs'
    d2 <- valuesToInsert
    let
      d = d1 <> d2

    modify (\st -> st {
        diffs       = push ds (fromIntegral t) d
      , tip         = t + 1
      , nrGenerated = nrGenerated st + 1
      })

    pure $ Push (fromIntegral t) d
  where
    valuesToDelete :: Values DS.DiffSeq k v -> CmdGen k v (Diff DS.DiffSeq k v)
    valuesToDelete (DS.Values m) = do
      PushConfig{nrToDelete} <- reader pushConfig
      DS.fromListDeletes . take nrToDelete <$> shuffle (Map.toList m)

    valuesToInsert :: CmdGen k v (Diff DS.DiffSeq k v)
    valuesToInsert = do
      PushConfig{nrToInsert} <- reader pushConfig
      DS.fromListInserts <$> replicateM nrToInsert arbitrary'

-- | Generate a @'Flush'@ command.
--
-- >  data Cmd (ds :: DiffSeqKind) k v =
-- >    -- ...
-- >    | Flush Int
-- >    -- ...
--
-- Steps to generate a @'Flush'@ command:
-- * Compute how many diffs @n@ in the diff sequence have a slot number that
--   exceeds the security parameter @k@.
-- * Return @n@.
-- BOOKKEEPING: Remove the first @n@ diffs from the models' diff sequence and
-- use them to forward the model's flushed values.
genFlush :: (Ord k, Eq v) => CmdGen k v (Cmd DS.DiffSeq k v)
genFlush = do
    k <- reader securityParameter

    ds <- gets diffs
    t  <- gets tip
    vs <- gets flushedValues

    let
      -- Since we never skip slots, we can compute which diffs are immutable
      -- just from the length of the current diff sequence.
      (l, r)    = flush (DS.length ds - k) ds
      -- Before we have pushed at least @k@ diffs, flushing has no effect.
      -- After pushing at least @k@ diffs, flushing should never leave the
      -- resulting diff sequence with less than @k@ elements.
      invariant = if t < k then
                    DS.length l == 0 && DS.length r == DS.length ds
                  else
                    DS.length r == k
      n         = DS.length l

    modify (\st -> st {
        diffs         = r
      , flushedValues = DS.applyDiff vs (totalDiff l)
      , nrGenerated   = nrGenerated st + 1
      })

    Exn.assert invariant $ pure $ Flush n

-- | Generate a @'Switch'@ command.
--
-- >  data Cmd (ds :: DiffSeqKind) k v =
-- >    -- ...
-- >    | Switch Int (Pushes ds k v)
-- >    -- ...
--
-- Steps to generate a @'Switch'@ command:
-- * Pick how much to rollback, i.e. an integer @n@ (depends on the
--   configuration parameter).
-- * Generate @n@ new diffs @ps@ to push, which mimicks a switch to
--   a new fork of length equal to the current fork.
-- * Return @n@ and @ps@.
-- BOOKKEEPING: Replace the last @n@ diffs in the models' diff sequence by
--   @ps@.
--
-- Note: We assume that pushes do not skip any slots.
genSwitch ::
     (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => CmdGen k v (Cmd DS.DiffSeq k v)
genSwitch = do
    k <- reader securityParameter
    distMay <- reader (rollbackDistribution . switchConfig)

    ds <- gets diffs
    t  <- gets tip

    m <- case distMay of
      Just dist -> frequency [(x, choose (lb, ub)) | (x, (lb, ub)) <- dist]
      Nothing   -> choose (1, k)

    let
      n       = min m (DS.length ds)
      (l, _r) = rollback n ds

    modify (\st -> st {
        diffs       = l
      , tip         = t - n
      , nrGenerated = nrGenerated st + 1
      })

    -- To generate @ps@, we re-use the @'genPush'@ function. However, this
    -- function updates @'nrGenerated'@ each time, even though we are only
    -- returning one @'Switch'@ command eventually. So, we subtract the
    -- length of @n@ from @'nrGenerated'@.
    ps <- fmap fromPushCmd <$> replicateM n genPush
    modify (\st -> st {
        nrGenerated = nrGenerated st - n
      })

    let
      invariant = m <= k && n <= DS.length ds

    Exn.assert invariant $ pure $ Switch n ps
  where
    fromPushCmd :: Cmd ds k v -> (SlotNo, Diff ds k v)
    fromPushCmd = \case
      Push sl d -> (sl, d)
      _         -> error "fromPushCmd"

-- | Generate a @'Forward'@ command.
--
-- >  data Cmd (ds :: DiffSeqKind) k v =
-- >    -- ...
-- >    | Forward (Values ds k v) (Keys ds k v)
--
-- Steps to generate a @'Forward'@ command:
-- * Determine which keys to forward.
-- * Retrieve (a subset of) the backing values from the model, i.e.,
--   /read/ the keys (obtained in previous step) from the backing values.
-- * Return the previous two combined.
-- BOOKKEEPING: None, since forwarding doesn't alter any model state.
--
-- Note: The keys to forward should be a superset of the keys of the backing
-- values that we retrieve from the model.
genForward :: forall k v. (Ord k, Eq v) => CmdGen k v (Cmd DS.DiffSeq k v)
genForward = do

    ds  <- gets diffs
    bvs <- gets flushedValues

    ks <- keysToForward (DS.diffKeys $ totalDiff ds)

    let
      vs = DS.restrictValues bvs ks

    modify (\st -> st{
        nrGenerated = nrGenerated st + 1
      })

    pure $ Forward vs ks
  where
    keysToForward :: Keys DS.DiffSeq k v -> CmdGen k v (Keys DS.DiffSeq k v)
    keysToForward (DS.Keys s) = do
      ForwardConfig{nrToForward} <- reader forwardConfig
      DS.keysFromList . take nrToForward <$> shuffle (toList s)

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

-- | Sanity check benchmark inputs.
--
-- These sanity checks ensure that changes to the benchmarking setup do not
-- accidentally invalidate the benchmarks. For example, both the
-- intermediate-sum and anti-diff implementations should have the same (read
-- "isomorphic") commands as inputs.
sanityCheck ::
     forall k v.
     ( Ord k, Eq v, Show k, Show v, NFData k, NFData v
     )
  => BenchEnv k v
  -> Property
sanityCheck bEnv = conjoin [
      interpretationsMatch
    , modelMatchesInterpretation1
    , modelMatchesInterpretation2
    , lengthsMatch1
    , lengthsMatch2
    ]
  where
    BenchEnv {cmdsAntidiff, cmdsIntermediateSum, ds0Antidiff, ds0IntermediateSum, model} = bEnv

    interpretationsMatch = counterexample "interpret matches" $
        interpret ds0Antidiff cmdsAntidiff
      ===
        fromSeqUtxoDiff (interpret ds0IntermediateSum cmdsIntermediateSum)

    modelMatchesInterpretation1 = counterexample "model matches interpretations 1" $
      diffs model === interpret ds0Antidiff cmdsAntidiff

    modelMatchesInterpretation2 = counterexample "model matches interpretations 2" $
        diffs model
       ===
        fromSeqUtxoDiff (interpret ds0IntermediateSum cmdsIntermediateSum)

    lengthsMatch1 = counterexample "Lengths of command/diff sequences match 1" $
      length cmdsAntidiff === length cmdsIntermediateSum

    lengthsMatch2 = counterexample "Lengths of command/diff sequences match 2" $
      DS.length ds0Antidiff === HD.lengthSeqUtxoDiff ds0IntermediateSum

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromDiffSeqCmd :: Cmd DS.DiffSeq k v1 -> Cmd HD.SeqUtxoDiff k v1
fromDiffSeqCmd cmd = case cmd of
  Push sl d     -> Push sl (fromDiff d)
  Flush n       -> Flush n
  Switch n ps   -> Switch n (fmap (second fromDiff) ps)
  Forward vs ks -> Forward (fromValues vs) (fromKeys ks)

fromDiffSeq :: Ord k => DS.DiffSeq k v -> HD.SeqUtxoDiff k v
fromDiffSeq (DS.UnsafeDiffSeq ft) = HD.SeqUtxoDiff . FT.fromList . map fromElement . toList $ ft

fromElement :: DS.Element k v -> HD.SudElement k v
fromElement (DS.Element slot d) = HD.SudElement slot (fromDiff d)

fromDiff :: DS.Diff k v -> HD.UtxoDiff k v
fromDiff (MapDiff.Diff m) = HD.UtxoDiff (fmap fromNEDiffHistory m)

fromNEDiffHistory :: DS.NEDiffHistory v -> HD.UtxoEntryDiff v
fromNEDiffHistory (MapDiff.NEDiffHistory neseq) = case neseq of
  Empty :|> x :||> y -> fromDiffEntry x <> fromDiffEntry y
  Empty :||> x       -> fromDiffEntry x
  _                  -> error "We can only create a UtxoEntryDiff from a \
                          \ DiffHistory if that diff history contains  \
                          \ exactly one insert, exactly one delete or exactly \
                          \ an insert AND a delete."

fromDiffEntry :: DS.DiffEntry v -> HD.UtxoEntryDiff v
fromDiffEntry de = case de of
  MapDiff.Insert v            -> HD.UtxoEntryDiff v HD.UedsIns
  MapDiff.Delete v            -> HD.UtxoEntryDiff v HD.UedsDel
  MapDiff.UnsafeAntiInsert _v -> error "UnsafeAntiInsert found."
  MapDiff.UnsafeAntiDelete _v -> error "UnsafeAntiDelete found."

fromSeqUtxoDiff :: (Ord k, Eq v) => HD.SeqUtxoDiff k v -> DS.DiffSeq k v
fromSeqUtxoDiff (HD.SeqUtxoDiff ft) = DS.UnsafeDiffSeq . RMFT.fromList . map fromSudElement . toList $ ft

fromSudElement :: Eq v => HD.SudElement k v -> DS.Element k v
fromSudElement (HD.SudElement slot d) = DS.Element slot (fromUtxoDiff d)

fromUtxoDiff :: Eq v => HD.UtxoDiff k v -> DS.Diff k v
fromUtxoDiff (HD.UtxoDiff m) = MapDiff.Diff $ fmap fromUtxoEntryDiff m

fromUtxoEntryDiff :: Eq v => HD.UtxoEntryDiff v -> DS.NEDiffHistory v
fromUtxoEntryDiff (HD.UtxoEntryDiff x st) = case st of
  HD.UedsIns       -> MapDiff.singletonInsert x
  HD.UedsDel       -> MapDiff.singletonDelete x
  HD.UedsInsAndDel ->
    MapDiff.unsafeFromDiffHistory (
      MapDiff.toDiffHistory (MapDiff.singletonInsert x) <>
      MapDiff.toDiffHistory (MapDiff.singletonDelete x)
    )

fromValues :: DS.Values k v -> HD.UtxoValues k v
fromValues (MapDiff.Values m) = HD.UtxoValues m

fromKeys :: DS.Keys k v1 -> HD.UtxoKeys k v2
fromKeys (MapDiff.Keys m) = HD.UtxoKeys m
