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
import qualified Control.Exception as Exn
import           Control.Monad
import           Control.Monad.RWS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as B
import           Data.Foldable
import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           QuickCheck.GenT
import           Test.Tasty.Bench
import           Test.Tasty.QuickCheck hiding (choose, frequency, resize,
                     shuffle, sized, variant)

import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD

import           Test.Util.MockDiffSeq
import           Test.Util.Orphans.Isomorphism (Isomorphism (..))
import           Test.Util.Orphans.NFData ()



benchmarks :: Benchmark
benchmarks = bgroup "HD" [ bgroup "DiffSeq/Diff operations" [
      benchComparative
        "Configuration: default"
        Nothing
        (mkBenchEnv @Key @Value gcDefault)
    , testProperty "interpret MockDiffSeq == interpret SeqUtxoDiff" $
        forAll (generator gcDefault TestMode) $
          \CmdGenResult {cmds, ds0} ->
              interpretMock ds0 cmds
            ===
              to interpretSubcached ds0 cmds
    ]]
  where
    mkBenchEnv ::
         (Ord k, Eq v, Arbitrary k, Arbitrary v)
      => GenConfig
      -> IO (BenchEnv k v)
    mkBenchEnv conf = do
      CmdGenResult{cmds, ds0, finalModel} <- generate $ generator conf BenchMode
      pure $ BenchEnv {
          cmdsMock = cmds
        , cmdsSubcached = to cmds
        , ds0Mock  = ds0
        , ds0Subcached = to ds0
        , model    = finalModel
        }

    interpretMock      = interpret @MockDiffSeq @Key @Value
    interpretSubcached = interpret @HD.SeqUtxoDiff @Key @Value

-- | Inputs to a benchmark.
--
-- As a convention, we use @ds0@ to indicate the state of the diff sequence
-- after the warmup phase during command generation.
data BenchEnv k v = BenchEnv {
    cmdsMock      :: ![Cmd MockDiffSeq k v]
  , cmdsSubcached :: ![Cmd HD.SeqUtxoDiff k v]
  , ds0Mock       :: !(MockDiffSeq k v)
  , ds0Subcached  :: !(HD.SeqUtxoDiff k v)
    -- | Final state of the model after the warmup phase /and/
    -- the command generation phase.
  , model         :: !(Model k v)
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
      \ ~bEnv@BenchEnv {cmdsMock, cmdsSubcached, ds0Mock, ds0Subcached} ->
        bgroup "Comparative performance analysis" [
          maybe bcompare (uncurry bcompareWithin) boundsMay target $
            bench "MockDiffSeq" $
              nf (interpret ds0Mock) cmdsMock
        ,   bench "SeqUtxoDiff" $
              nf (interpret ds0Subcached) cmdsSubcached
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

instance (Ord k, Eq v) => Isomorphism (Cmd MockDiffSeq k v) (Cmd HD.SeqUtxoDiff k v) where
  to (Push sl d)     = Push (to sl) (to d)
  to (Flush n)       = Flush n
  to (Rollback n bs) = Rollback n (to bs)
  to (Forward vs ks) = Forward (to vs) (to ks)

instance (Ord k, Eq v) => Isomorphism (Cmd HD.SeqUtxoDiff k v) (Cmd MockDiffSeq k v) where
  to (Push sl d)     = Push (to sl) (to d)
  to (Flush n)       = Flush n
  to (Rollback n bs) = Rollback n (to bs)
  to (Forward vs ks) = Forward (to vs) (to ks)

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

instance Ord k => IsDiffSeq HD.SeqUtxoDiff k v where
  type Diff HD.SeqUtxoDiff k v    = HD.UtxoDiff k v
  type Values HD.SeqUtxoDiff k v  = HD.UtxoValues k v
  type Keys HD.SeqUtxoDiff k v    = HD.UtxoKeys k v
  type SlotNo HD.SeqUtxoDiff      = Block.SlotNo

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
  , securityParameter = 2160
  , nrInitialValues = 100
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

genInitialModel ::
     (Ord k, Arbitrary k, Arbitrary v)
  => GenConfig
  -> Gen (Model k v)
genInitialModel GenConfig{nrInitialValues} = do
    kvs <- replicateM nrInitialValues arbitrary
    pure $ Model mempty 0 (mFromListValues kvs) 0

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
     (Ord k, Arbitrary k, Arbitrary v)
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
    -- generatore mode is set to @Test@.
    mkConf = case gmode of
      BenchMode -> pure preConf
      TestMode  -> do
        n <- getPositive <$> arbitrary
        k :: Int <- getSmall . getPositive <$> arbitrary
        pure $ preConf {
            nrCommands        = n
          , securityParameter = k
          , directives        = Nothing
          , rollbackConfig    = (rollbackConfig preConf) {
                distribution = Nothing
              }
          }

-- | Generate a list of commands in deterministic order.
--
-- Note: The order of commands is deterministic, the inputs for these commands
-- are often semi-randomly generated.
genCmdsDet ::
     forall k v. (Ord k, Arbitrary k, Arbitrary v)
  => CmdGen k v [Cmd MockDiffSeq k v]
genCmdsDet = do
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
        c <- genCmdDet dir
        cs <- local (\r -> r { directives = Just dirs }) genCmdsDet
        pure (c : cs)

-- | Generate a specific command according to a directive.
genCmdDet ::
     (Ord k, Arbitrary k, Arbitrary v)
  => Directive
  -> CmdGen k v (Cmd MockDiffSeq k v)
genCmdDet = \case
  GenForward  -> genForward
  GenPush     -> genPush
  GenFlush    -> genFlush
  GenRollback -> genRollback

-- | Generate a list of commands in non-deterministic order.
genCmdsNonDet ::
     (Ord k, Arbitrary k, Arbitrary v)
  => CmdGen k v [Cmd MockDiffSeq k v]
genCmdsNonDet = do
  conf <- ask
  replicateM (nrCommands conf) genCmdNonDet

-- | Generate a command based on a probability distribution.
genCmdNonDet ::
     (Ord k, Arbitrary k, Arbitrary v)
  => CmdGen k v (Cmd MockDiffSeq k v)
genCmdNonDet = do
    frequency [
        (100, genPush)
      , (10, genFlush)
      , (1, genRollback)
      , (100, genForward)
      ]

-- | Simulate the warmup phase where there have not yet been at least @k@
-- pushes.
warmup ::
     forall k v. (Ord k, Arbitrary k, Arbitrary v)
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
      , mLength ds == k
      , t == k
      ]

  Exn.assert invariant $ pure ()

{-------------------------------------------------------------------------------
  Command generator: Individual command generation
-------------------------------------------------------------------------------}

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
genPush ::
     forall k v. (Ord k, Arbitrary k, Arbitrary v)
  => CmdGen k v (Cmd MockDiffSeq k v)
genPush = do
    ds <- gets diffs
    t  <- gets tip
    vs <- gets backingValues

    let
      vs' = mForwardValues vs (mTotalDiff ds)

    d1 <- valuesToDelete vs'
    d2 <- valuesToInsert
    let
      d = d1 <> d2

    modify (\st -> st {
        diffs       = mPush ds (fromIntegral t) d
      , tip         = t + 1
      , nrGenerated = nrGenerated st + 1
      })

    pure $ Push (fromIntegral t) d
  where
    valuesToDelete :: MockValues k v -> CmdGen k v (MockDiff k v)
    valuesToDelete (MockValues m) = do
      PushConfig{nrToDelete} <- reader pushConfig
      mFromListDeletes . take nrToDelete <$> shuffle (Map.toList m)

    valuesToInsert :: CmdGen k v (MockDiff k v)
    valuesToInsert = do
      PushConfig{nrToInsert} <- reader pushConfig
      mFromListInserts <$> replicateM nrToInsert arbitrary'

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
genFlush :: Ord k => CmdGen k v (Cmd MockDiffSeq k v)
genFlush = do
    k <- reader securityParameter

    ds <- gets diffs
    t  <- gets tip
    vs <- gets backingValues

    let
      -- Since we never skip slots, we can compute which diffs are immutable
      -- just from the length of the current diff sequence.
      (l, r)    = mFlush (mLength ds - k) ds
      -- Before we have pushed at least @k@ diffs, flushing has no effect.
      -- After pushing at least @k@ diffs, flushing should never leave the
      -- resulting diff sequence with less than @k@ elements.
      invariant = if t < k then
                    mLength l == 0 && mLength r == mLength ds
                  else
                    mLength r == k
      n         = mLength l

    modify (\st -> st {
        diffs         = r
      , backingValues = mForwardValues vs (mTotalDiff l)
      , nrGenerated   = nrGenerated st + 1
      })

    Exn.assert invariant $ pure $ Flush n

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
     (Ord k, Arbitrary k, Arbitrary v)
  => CmdGen k v (Cmd MockDiffSeq k v)
genRollback = do
    k <- reader securityParameter
    distMay <- reader (distribution . rollbackConfig)

    ds <- gets diffs
    t  <- gets tip

    m <- case distMay of
      Just dist -> frequency [(x, choose (lb, ub)) | (x, (lb, ub)) <- dist]
      Nothing   -> choose (1, k)

    let
      n       = min m (mLength ds)
      (l, _r) = mRollback n ds

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
      invariant = m <= k && n <= mLength ds

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
genForward :: forall k v. Ord k => CmdGen k v (Cmd MockDiffSeq k v)
genForward = do

    ds  <- gets diffs
    bvs <- gets backingValues

    ks <- keysToForward (mDiffKeys $ totalDiff ds)

    let
      vs = mRestrictValues bvs ks

    modify (\st -> st{
        nrGenerated = nrGenerated st + 1
      })

    pure $ Forward vs ks
  where
    keysToForward :: MockKeys k v -> CmdGen k v (MockKeys k v)
    keysToForward (MockKeys s) = do
      ForwardConfig{nrToForward} <- reader forwardConfig
      mFromListKeys . take nrToForward <$> shuffle (toList s)

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

-- | Sanity check benchmark inputs.
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
    , isomorphismCheck1
    , isomorphismCheck2
    , isomorphismProperties1
    , isomorphismProperties2
    ]
  where
    BenchEnv {cmdsMock, cmdsSubcached, ds0Mock, ds0Subcached, model} = bEnv

    interpretationsMatch = counterexample "interpret matches" $
      interpret ds0Mock cmdsMock === to (interpret ds0Subcached cmdsSubcached)

    modelMatchesInterpretation1 = counterexample "model matches interpretations 1" $
      diffs model === interpret ds0Mock cmdsMock

    modelMatchesInterpretation2 = counterexample "model matches interpretations 2" $
       diffs model === to (interpret ds0Subcached cmdsSubcached)

    lengthsMatch1 = counterexample "Lengths of command/diff sequences match 1" $
      length cmdsMock === length cmdsSubcached

    lengthsMatch2 = counterexample "Lengths of command/diff sequences match 2" $
      mLength ds0Mock === HD.lengthSeqUtxoDiff ds0Subcached

    isomorphismCheck1 = counterexample "Isomorphism check 1" $
      cmdsMock === to cmdsSubcached

    isomorphismCheck2 = counterexample "Isomorphism check 2" $
      to cmdsMock === cmdsSubcached

    isomorphismProperties1 = counterexample "Isomorphism properties 1" $
      to @[Cmd HD.SeqUtxoDiff k v] (to cmdsMock) === cmdsMock

    isomorphismProperties2 = counterexample "Isomorphism properties 2" $
      to @[Cmd MockDiffSeq k v] (to cmdsSubcached) === cmdsSubcached
