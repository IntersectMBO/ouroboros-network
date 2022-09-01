{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- | Comparative benchmarks for legacy/anti-diff approaches to diff sequences.

  The benchmarks measure the time needed to evaluate a sequence of operations
  on diff sequences (and auxiliary datatypes). A sequence of operations should
  mimick how diff sequences are used in Consensus.

  We run these benchmarks for two approaches to diff sequences: the legacy and
  anti-diff approaches. We compare performance across these two approaches.
  We suspect that the anti-diff approach should be more performant.

  The operations can be of four types:
  * Push (insert on the right)
  * Flush (split the sequence at a given point/take on the right side)
  * Rollback (split the sequence at a given point/take on the left side)
  * Forward (apply the cumulative diff up to the tip)

  Notes:
  * We use the terms /operation/ and @Cmd@ or /command/ interchangeably.
  * We use the terms /evaluate/ and /interpret/ interchangeably.

  === Benchmarking interesting evaluation steps only

  Intuitively, these operations only make sense with respect to some state:
  the cumulative diff sequence (that we could push to in the future), values
  that have been flushed previously (and could be forwarded in the future),
  the slot of the most recent push (which should montonically increase for each
  subsequent push), etc. However, we aim to only measure the performance of the
  operations, and not /noise/ such as:
  * bookkeeping that has to do with state, or,
  * deciding what operations to perform and what their inputs should be.
  We off-load the responsibility to do these "noisy computations" to a
  generator, which generates a sequence of operations in a stateful way,
  such that we can benchmark just the evaluation of the operations.

  > generator :: Model -> [Cmd]
  > evaluate :: [Cmd] -> Result

  As an example scenario, consider flushing. The part of evaluating a flush
  that is relevant to our benchmarks is just the splitting of the cumulative
  diff sequence. All else is noise and should not affect the benchmark, for
  example:
  * What the cumulative diff sequence /is/ depends on the current state, so
    we have to keep track of state and do bookkeeping.
  * We must compute how much of the diff should be split off/flushed.
-}
module Bench.Ouroboros.Consensus.Storage.LedgerDB.HD (benchmarks) where

import           Control.DeepSeq
import qualified Control.Exception as Exn
import           Control.Monad.RWS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as B
import           Data.Foldable (toList)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           QuickCheck.GenT
import           Test.QuickCheck hiding (Result, choose, frequency, getSize,
                     resize, shuffle, sized, variant)
import           Test.Tasty.Bench
import           Test.Tasty.QuickCheck (testProperty)

import qualified Data.Map.Diff.Strict as MapDiff

import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS

import           Test.Util.Orphans.Isomorphism (Isomorphism (..))
import           Test.Util.Orphans.NFData ()

-- TODO(jdral): Instead of nf on diff sequence, alternative like
-- * read and forward all values
-- * some predicate/function on the resulting diff sequence
-- TODO(jdral): Add monitoring to property tests and benchmarks.
benchmarks :: Benchmark
benchmarks = bgroup "HD" [ bgroup "DiffSeq/Diff operations" [
      benchComparative
        "Configuration: default"
        Nothing
        (mkBenchEnv @Key @Value gcDefault)
    , benchComparative
        "Configuration: no rollbacks"
        Nothing
        (mkBenchEnv @Key @Value gcNoRollbacks)
    , benchComparative
        "Configuration: only small rollbacks"
        Nothing
        (mkBenchEnv @Key @Value gcSmallRollbacks)
    , benchComparative
        "Configuration: no rollbacks, larger flushes (every 100 pushes)"
        Nothing
        (mkBenchEnv @Key @Value gcNoRollbacksLargerFlush)
    , testProperty "interpret AntiDiff == interpret LegacyDiff" $
        forAll (generatorTest gcDefault) $
          \CmdGenResult {cmds, ds0} ->
              interpretNew ds0 cmds
            ===
              to interpretLegacy ds0 cmds
    ]]
  where
    mkBenchEnv ::
         forall k v. (Ord k, Arbitrary k, Eq v, Arbitrary v)
      => GenConfig
      -> IO (BenchEnv k v)
    mkBenchEnv conf = do
      CmdGenResult{cmds, ds0, finalModel} <- generate $ generator' conf
      pure $ BenchEnv {
          cmdsNew    = cmds
        , cmdsLegacy = to cmds
        , ds0New     = ds0
        , ds0Legacy  = to ds0
        , model      = finalModel
        }

    interpretNew    = interpret @New    @Key @Value
    interpretLegacy = interpret @Legacy @Key @Value

-- | Inputs to a benchmark.
data BenchEnv k v = BenchEnv {
    cmdsNew    :: ![Cmd 'New k v]
  , cmdsLegacy :: ![Cmd 'Legacy k v]
    -- | State of the diff sequence after the warmup phase during command
    -- generation.
  , ds0New     :: !(DiffSeq 'New k v)
    -- | State of the diff sequence after the warmup phase during command
    -- generation.
  , ds0Legacy  :: !(DiffSeq 'Legacy k v)
    -- | Final state of the model after the warmup phase /and/
    -- the command generation phase.
  , model      :: !(Model k v)
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
      \ ~bEnv@BenchEnv {cmdsNew, cmdsLegacy, ds0New, ds0Legacy} ->
        bgroup "Comparative performance analysis" [
          maybe bcompare (uncurry bcompareWithin) boundsMay target $
            bench "AntiDiff" $
              nf (interpret ds0New) cmdsNew
        ,   bench "LegacyDiff" $
              nf (interpret ds0Legacy) cmdsLegacy
        , testProperty "Sanity checks" $
            once (sanityCheck bEnv)
        ]
  ]
  where
    target :: String
    target =
      "    $(NF-2) == \"" <> name <> "\" \
      \ && $NF     == \"LegacyDiff\"     "

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

data Imp = Legacy | New

type Block (i :: Imp) k v = (SlotNo i, Diff i k v)

data Cmd (i :: Imp) k v =
    Push     (SlotNo i)      (Diff i k v)
  | Flush    Int
  | Rollback Int             [Block i k v]
  | Forward  (Values i k v)  (Keys i k v)
  deriving stock Generic

deriving anyclass instance ( NFData (DiffSeq i k v)
                           , NFData (Diff i k v)
                           , NFData (Values i k v)
                           , NFData (Keys i k v)
                           , NFData (SlotNo i)
                           ) => NFData (Cmd i k v)

deriving stock instance ( Show (DiffSeq i k v)
                        , Show (Diff i k v)
                        , Show (Values i k v)
                        , Show (Keys i k v)
                        , Show (SlotNo i)
                        ) => Show (Cmd i k v)

deriving stock instance ( Eq (DiffSeq i k v)
                        , Eq (Diff i k v)
                        , Eq (Values i k v)
                        , Eq (Keys i k v)
                        , Eq (SlotNo i)
                        ) => Eq (Cmd i k v)

instance (Ord k, Eq v) => Isomorphism (Cmd New k v) (Cmd Legacy k v) where
  to :: Cmd New k v -> Cmd Legacy k v
  to (Push sl d)     = Push (to sl) (to d)
  to (Flush n)       = Flush n
  to (Rollback n bs) = Rollback n (to bs)
  to (Forward vs ks) = Forward (to vs) (to ks)

instance (Ord k, Eq v) => Isomorphism (Cmd Legacy k v) (Cmd New k v) where
  to :: Cmd Legacy k v -> Cmd New k v
  to (Push sl d)     = Push (to sl) (to d)
  to (Flush n)       = Flush n
  to (Rollback n bs) = Rollback n (to bs)
  to (Forward vs ks) = Forward (to vs) (to ks)

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Interpretation of @Cmd@s as a fold.
interpret ::
     forall i k v.
     ( Ord k
     , Eq v
     , Comparable i
     , NFData (DiffSeq i k v)
     )
  => DiffSeq i k v
  -> [Cmd i k v]
  -> DiffSeq i k v
interpret = foldl' go
  where
    go :: DiffSeq i k v -> Cmd i k v -> DiffSeq i k v
    go ds = \case
      Push sl d     -> push ds sl d
      -- We @'deepseq'@ the left part of the flush result, because that is the
      -- part that would be written to disk, for which it must be fully
      -- evaluated.
      Flush n       -> let (l, r) = flush n ds
                       in  l `deepseq` r
      -- We do not @'deepseq'@ the right part of the rollback result, becase we
      -- can forget about it.
      Rollback n bs -> let (l, _r) = rollback n ds
                       in  foldl (uncurry . push) l bs
      -- FIXME(jdral): Should we @'seq'@ or @'deepseq'@ the forwarding result?
      -- Or, is there something else we should do with the result of @vs'@?
      Forward vs ks -> let vs' = forward vs ks ds
                       in  vs' `seq` ds

-- | Common interface to diff sequence implementations.
class Comparable (i :: Imp) where
  type DiffSeq i k v = r | r -> i k v
  type Diff i k v    = r | r -> i k v
  type Values i k v  = r | r -> i k v
  type Keys i k v    = r | r -> i k v
  type SlotNo i      = r | r -> i

  -- | Mimicks @'Ouroboros.Consensus.Ledger.Basics.extendDbChangelog.ext'@
  push     :: (Ord k, Eq v) => DiffSeq i k v -> SlotNo i -> Diff i k v -> DiffSeq i k v
  -- | Mimicks @'Ouroboros.Consensus.Ledger.Basics.flushDbChangelog.split'@
  flush    :: (Ord k, Eq v) => Int -> DiffSeq i k v -> (DiffSeq i k v, DiffSeq i k v)
  -- | Mimicks @'Ouroboros.Consensus.Ledger.Basics.rollbackDbChangelog.trunc'@
  rollback :: (Ord k, Eq v) => Int -> DiffSeq i k v -> (DiffSeq i k v, DiffSeq i k v)
  -- | Mimicks @'Ouroboros.Consensus.Storage.LedgerDB.InMemory.forward'@.
  forward  :: (Ord k, Eq v) => Values i k v -> Keys i k v -> DiffSeq i k v -> Values i k v

instance Comparable Legacy where
  type DiffSeq Legacy k v = HD.SeqUtxoDiff k v
  type Diff Legacy k v    = HD.UtxoDiff k v
  type Values Legacy k v  = HD.UtxoValues k v
  type Keys Legacy k v    = HD.UtxoKeys k v
  type SlotNo Legacy      = Block.SlotNo

  push             = HD.extendSeqUtxoDiff
  flush            = HD.splitAtSeqUtxoDiff
  rollback         = HD.splitAtFromEndSeqUtxoDiff
  forward vs ks ds = HD.forwardValuesAndKeys vs ks (HD.cumulativeDiffSeqUtxoDiff ds)

instance Comparable New where
  type DiffSeq New k v = DS.DiffSeq k v
  type Diff New k v    = MapDiff.Diff k v
  type Values New k v  = MapDiff.Values k v
  type Keys New k v    = MapDiff.Keys k v
  type SlotNo New      = DS.SlotNo

  push ds slot d   = DS.extend' ds (DS.Element slot d)
  flush            = DS.splitlAt
  rollback         = DS.splitrAtFromEnd
  forward vs ks ds = MapDiff.forwardValuesAndKeys vs ks (DS.cumulativeDiff ds)

{-------------------------------------------------------------------------------
  Configuration types
-------------------------------------------------------------------------------}

-- | Configuration parameters for generators.
data GenConfig = GenConfig {
    -- | Desired length of command sequence
    nrCommands        :: Int
    -- | The number of initial backing values
  , nrInitialValues   :: Int
    -- | Security parameter @k@
  , securityParameter :: Int
  , pushConfig        :: PushConfig
  , forwardConfig     :: ForwardConfig
  , rollbackConfig    :: RollbackConfig
    -- | A series of directives that determines which commands to generate.
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

-- | Configuration parameters for @'Rollback'@ command generation.
newtype RollbackConfig = RollbackConfig {
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
    distribution :: Maybe [(Int, (Int, Int))]
  }

-- | A directive to generate a specific command.
data Directive = GenForward | GenPush | GenFlush | GenRollback
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Specific configurations
-------------------------------------------------------------------------------}

gcDefault :: GenConfig
gcDefault = GenConfig {
    nrCommands = 10000
  , nrInitialValues = 100
  , securityParameter = 2160
  , pushConfig = PushConfig {
        nrToDelete = 50
      , nrToInsert = 50
      }
  , forwardConfig = ForwardConfig {
        nrToForward = 50
      }
  , rollbackConfig = RollbackConfig {
        distribution = Just [(95, (1, 10)), (5, (1000, 2000))]
      }
  , directives =
      let
        xs = concat $
          zipWith
            ((. return ) . (:))
            (replicate 10 GenForward)
            (replicate 10 GenPush)
        ys = concat $ replicate 10 (xs ++ [GenFlush])
        zs = ys ++ [GenRollback]
      in
        Just $ concat $ repeat zs
  }

-- | No rollbacks.
gcNoRollbacks :: GenConfig
gcNoRollbacks = gcDefault {
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

-- | Only small rollbacks.
gcSmallRollbacks :: GenConfig
gcSmallRollbacks = gcDefault {
    rollbackConfig = (rollbackConfig gcDefault) {
        distribution = Just [(1, (1, 10))]
      }
  }

-- | Larger flushes, no rollbacks.
gcNoRollbacksLargerFlush :: GenConfig
gcNoRollbacksLargerFlush = gcDefault {
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
    diffs         :: DS.DiffSeq k v     -- ^ The current diff sequence
  , tip           :: Int                -- ^ The tip of the diff
                                        --   sequence
  , backingValues :: MapDiff.Values k v -- ^ Values that have been
                                        --   flushed to a backing store
  , nrGenerated   :: Int                -- ^ A counter for the number
                                        --   of commands generated
                                        --   up until now
  }
  deriving stock (Show, Generic)
  deriving anyclass NFData

genInitialModel ::
     (Ord k, Arbitrary k, Eq v, Arbitrary v)
  => GenConfig
  -> Gen (Model k v)
genInitialModel GenConfig{nrInitialValues} = do
    kvs <- replicateM nrInitialValues arbitrary
    pure $ Model mempty 0 (MapDiff.valuesFromList kvs) 0

newtype Key = Key ShortByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype NFData

newtype Value = Value ShortByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype NFData

instance Arbitrary Key where
  arbitrary = Key . B.pack <$> replicateM 32 arbitrary

instance Arbitrary Value where
  arbitrary = Value . B.pack <$> replicateM 64 arbitrary

{-------------------------------------------------------------------------------
  Command generator: Monad transformer stack
-------------------------------------------------------------------------------}

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

data CmdGenResult k v = CmdGenResult {
    -- | The generated command sequence.
    cmds       :: [Cmd 'New k v]
    -- | State of the diff sequence after the warmup phase.
  , ds0        :: DiffSeq 'New k v
    -- | Final state of the model after the warmup phase /and/
    -- the command generation phase, like @'model'@.
  , finalModel :: Model k v
  }
  deriving Show

{-------------------------------------------------------------------------------
  Command generator: Main generators (non-deterministic order of commands)
-------------------------------------------------------------------------------}

-- | A stateful generator for sequences of commands.
--
-- Note: We return the @'Model'@ because we might want to check invariants on
-- it.
generator ::
     (Ord k, Arbitrary k, Eq v, Arbitrary v)
  => GenConfig -> Gen (CmdGenResult k v)
generator conf = do
  m <- genInitialModel conf
  ((), m') <- runCmdGen warmup conf m
  (cmds, m'') <- runCmdGen genCmds conf m'
  pure $ CmdGenResult {
      cmds
    , ds0 = diffs m'
    , finalModel = m''
    }

-- | Generate a list of commands in non-deterministic order.
genCmds ::
     (Ord k, Arbitrary k, Eq v, Arbitrary v)
  => CmdGen k v [Cmd 'New k v]
genCmds = do
  conf <- ask
  replicateM (nrCommands conf) genCmd

-- | Generate a command based on a probability distribution.
genCmd ::
     (Ord k, Arbitrary k, Eq v, Arbitrary v)
  => CmdGen k v (Cmd 'New k v)
genCmd = do
    frequency [
        (100, genPush)
      , (10, genFlush)
      , (1, genRollback)
      , (100, genForward)
      ]

{-------------------------------------------------------------------------------
  Command generator: Main generators (deterministic order of commands)
-------------------------------------------------------------------------------}

-- | Like @'generator'@, but the order of operations is deterministic.
generator' ::
     (Ord k, Arbitrary k, Eq v, Arbitrary v)
  => GenConfig -> Gen (CmdGenResult k v)
generator' conf = do
  m <- genInitialModel conf
  ((), m') <- runCmdGen warmup conf m
  (cmds, m'') <- runCmdGen genCmds' conf m'
  pure $ CmdGenResult {
      cmds
    , ds0 = diffs m'
    , finalModel = m''
    }
-- | Generate a list of commands in deterministic order.
--
-- Note: The order of commands is deterministic, the inputs for these commands
-- are often semi-randomly generated.
genCmds' ::
     forall k v. (Ord k, Arbitrary k, Eq v, Arbitrary v)
  => CmdGen k v [Cmd 'New k v]
genCmds' = do
  nrc <- reader nrCommands
  nrg <- gets nrGenerated

  if nrg >= nrc then
    pure []
  else do
    dirsMay <- reader directives

    case dirsMay of
      Nothing       -> error "No directives given."
      Just []       -> error "Directives should be infinitish length"
      Just (dir : dirs) -> do
        c <- genCmd' dir
        cs <- local (\r -> r { directives = Just dirs }) genCmds'
        pure (c : cs)

-- | Generate a specific command.
genCmd' ::
     (Ord k, Arbitrary k, Eq v, Arbitrary v)
  => Directive
  -> CmdGen k v (Cmd 'New k v)
genCmd' = \case
  GenForward  -> genForward
  GenPush     -> genPush
  GenFlush    -> genFlush
  GenRollback -> genRollback

{-------------------------------------------------------------------------------
  Command generator: Main generators (for property tests)
-------------------------------------------------------------------------------}

generatorTest ::
     (Ord k, Arbitrary k, Eq v, Arbitrary v)
  => GenConfig -> Gen (CmdGenResult k v)
generatorTest conf = do
  n <- getPositive <$> arbitrary
  k :: Int <- getSmall . getPositive <$> arbitrary

  let
    conf' = conf {
        nrCommands        = n
      , securityParameter = k
      , directives        = Nothing
      , rollbackConfig    = (rollbackConfig conf) {
            distribution = Nothing
          }
      }

  generator conf'

{-------------------------------------------------------------------------------
  Command generator: Individual command generation
-------------------------------------------------------------------------------}

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

-- | Generate a @'Push'@ command.
--
-- > data Cmd (i :: Imp) k v =
-- >    Push (SlotNo i) (Diff i k v)
-- >    -- ...
--
-- Steps to generate a @'Push'@ command:
-- * Forward backing values.
-- * "Simulate a transaction" by generating a diff that:
--   1. Deletes one or more values that exist in the forwarded backing values.
--   2. Inserts one or more values with globally unique keys.
-- * Return a strictly increasing slot number, and the new diff.
-- BOOKKEEPING: Push the new diff onto the model's diff sequence.
--
-- Note: https://iohk.io/en/blog/posts/2018/07/03/self-organisation-in-coin-selection/
--
-- TODO: Use coin selection other than random choice.
-- TODO: Skip some slots, instead of strictly incrementing @t@.
genPush ::
     forall k v. (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => CmdGen k v (Cmd 'New k v)
genPush = do
    ds <- gets diffs
    t  <- gets tip
    vs <- gets backingValues

    let
      _vs'@(MapDiff.Values m) = MapDiff.forwardValues vs (DS.cumulativeDiff ds)

    d1 <- MapDiff.fromListDeletes <$> valuesToDelete m
    d2 <- MapDiff.fromListInserts <$> valuesToInsert
    let
      d = d1 <> d2

    modify (\st -> st {
        diffs       = DS.extend' ds $ DS.Element (fromIntegral t) d
      , tip         = t + 1
      , nrGenerated = nrGenerated st + 1
      })

    pure $ Push (fromIntegral t) d
  where
    valuesToDelete :: Map k v -> CmdGen k v [(k, v)]
    valuesToDelete m = do
      PushConfig{nrToDelete} <- reader pushConfig
      take nrToDelete <$> shuffle (Map.toList m)

    valuesToInsert :: CmdGen k v [(k, v)]
    valuesToInsert = do
      PushConfig{nrToInsert} <- reader pushConfig
      replicateM nrToInsert arbitrary'

-- | Generate a @'Flush'@ command.
--
-- >  data Cmd (i :: Imp) k v =
-- >    -- ...
-- >    | Flush Int
-- >    -- ...
--
-- Steps to generate a @'Flush'@ command:
-- * Compute how many diffs @n@ in the diff sequence have a slot number that
--   exceeds the security parameter @k@.
-- * Return @n@.
-- BOOKKEEPING: Remove the first @n@ diffs from the models' diff sequence and
-- use them to forward the model's backing values.
genFlush :: (Ord k, Eq v) => CmdGen k v (Cmd 'New k v)
genFlush = do
    k <- reader securityParameter

    ds <- gets diffs
    t  <- gets tip
    vs <- gets backingValues

    let
      -- Since we never skip slots, we can compute which diffs are immutable
      -- just from the length of the current diff sequence.
      (l, r)    = DS.splitlAt (DS.length ds - k) ds
      -- Before we have pushed at least @k@ diffs, flushing has no effect.
      -- After pushing at least @k@ diffs, flushing should never leave the
      -- resulting diff sequence with less than @k@ elements.
      invariant = if t < k then
                    DS.length l == 0 && DS.length r == DS.length ds
                  else
                    DS.length r == k
      n         = Exn.assert invariant $ DS.length l

    modify (\st -> st {
        diffs         = r
      , backingValues = MapDiff.forwardValues vs (DS.cumulativeDiff l)
      , nrGenerated   = nrGenerated st + 1
      })

    pure $ Flush n

-- | Generate a @'Rollback'@ command.
--
-- >  data Cmd (i :: Imp) k v =
-- >    -- ...
-- >    | Rollback Int [Block i k v]
-- >    -- ...
--
-- Steps to generate a @'Rollback'@ command:
-- * Pick how much to rollback, i.e. an integer @n@ (depends on the
--   configuration parameter).
-- * Generate @n@ new diffs (blocks) @bs@ to push, which mimicks a switch to
--   a new fork of length equal to the current fork.
-- * Return @n@ and @bs@.
-- BOOKKEEPING: Replace the last @n@ diffs in the models' diff sequence by
--   @bs@.
--
-- Note: We assume that pushes do not skip any slots.
genRollback ::
     (Ord k, Eq v, Arbitrary k, Arbitrary v)
  => CmdGen k v (Cmd 'New k v)
genRollback = do
    k <- reader securityParameter
    distMay <- reader (distribution . rollbackConfig)

    ds <- gets diffs
    t  <- gets tip

    m <- case distMay of
      Just dist -> frequency [(x, choose (lb, ub)) | (x, (lb, ub)) <- dist]
      Nothing   -> choose (1, k)

    let
      n       = min m (DS.length ds)
      (l, _r) = DS.splitrAtFromEnd n ds

    modify (\st -> st {
        diffs       = l
      , tip         = t - n
      , nrGenerated = nrGenerated st + 1
      })

    -- To generate @bs@, we re-use the @'genPush'@ function. However, this
    -- function updates @'nrGenerated'@ each time, even though we are only
    -- returning one @'Rollback'@ command eventually. So, we subtract the
    -- length of @n@ from @'nrGenerated'@.
    bs <- fmap fromPushCmd <$> replicateM n genPush
    modify (\st -> st {
        nrGenerated = nrGenerated st - n
      })

    let
      invariant = m <= k && n <= DS.length ds

    Exn.assert invariant $ pure $ Rollback n bs
  where
    fromPushCmd :: Cmd i k v -> (SlotNo i, Diff i k v)
    fromPushCmd = \case
      Push sl d -> (sl, d)
      _         -> error "fromPushCmd"

-- | Generate a @'Forward'@ command.
--
-- >  data Cmd (i :: Imp) k v =
-- >    -- ...
-- >    | Forward (Values i k v) (Keys i k v)
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
genForward :: (Ord k, Eq v) => CmdGen k v (Cmd 'New k v)
genForward = do

    ds  <- gets diffs
    bvs <- gets backingValues

    ksTF <- keysToForward (MapDiff.diffKeys $ DS.cumulativeDiff ds)

    let
      ks = MapDiff.Keys $ Set.fromList ksTF
      vs = MapDiff.restrictValues bvs ks

    modify (\st -> st{
        nrGenerated = nrGenerated st + 1
      })

    pure $ Forward vs ks
  where
    keysToForward :: Set k -> CmdGen k v [k]
    keysToForward ks = do
      ForwardConfig{nrToForward} <- reader forwardConfig
      take nrToForward <$> shuffle (toList ks)

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

sanityCheck ::
     forall k v.
     ( Ord k, Eq v, Show k, Show v, NFData k, NFData v
     )
  => BenchEnv k v
  -> Property
sanityCheck BenchEnv {cmdsNew, cmdsLegacy, ds0New, ds0Legacy, model} = conjoin [
      interpretationsMatch
    , modelMatchesInterpretation1
    , modelMatchesInterpretation2
    , lengthsMatch1
    , lengthsMatch2
    , isomorphismCheck1
    , isomorphismCheck2
    , isomorphismProperties1
    , isomorphismProperties2
    ]
  where
    interpretationsMatch = counterexample "interpret matches" $
      interpret ds0New cmdsNew === to (interpret ds0Legacy cmdsLegacy)

    modelMatchesInterpretation1 = counterexample "model matches interpretations 1" $
      diffs model === interpret ds0New cmdsNew

    modelMatchesInterpretation2 = counterexample "model matches interpretations 2" $
       diffs model === to (interpret ds0Legacy cmdsLegacy)

    lengthsMatch1 = counterexample "Lengths of command/diff sequences match 1" $
      length cmdsNew === length cmdsLegacy

    lengthsMatch2 = counterexample "Lengths of command/diff sequences match 2" $
      DS.length ds0New === HD.lengthSeqUtxoDiff ds0Legacy

    isomorphismCheck1 = counterexample "Isomorphism check 1" $
      cmdsNew === to cmdsLegacy

    isomorphismCheck2 = counterexample "Isomorphism check 2" $
      to cmdsNew === cmdsLegacy

    isomorphismProperties1 = counterexample "Isomorphism properties 1" $
      to @[Cmd 'Legacy k v] (to cmdsNew) === cmdsNew

    isomorphismProperties2 = counterexample "Isomorphism properties 2" $
      to @[Cmd 'New k v] (to cmdsLegacy) === cmdsLegacy
