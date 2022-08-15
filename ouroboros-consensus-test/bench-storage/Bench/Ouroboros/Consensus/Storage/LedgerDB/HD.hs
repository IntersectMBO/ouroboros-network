{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# LANGUAGE UndecidableInstances    #-}

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
  > evaluate :: [Cmd] -> [Result]

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
import           Control.Monad (replicateM)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.State (StateT (..), gets, modify)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Test.QuickCheck hiding (Result)
import           Test.Tasty.Bench
import           Test.Tasty.QuickCheck (testProperty)

import qualified Data.Map.Diff.Strict as MapDiff

import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.TableTypes as TT
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.ToStore as TS

import           Test.Util.Orphans.Isomorphism (Isomorphism (..), from, inside)
import           Test.Util.Orphans.NFData ()

-- TODO(jdral): Instead of nf on diff sequence, alternative like
-- * read and forward all values
-- * some predicate/function on the resulting diff sequence
-- TODO(jdral): Add monitoring to property tests and benchmarks.
benchmarks :: Benchmark
benchmarks = bgroup "HD" [ bgroup "DiffSeq/Diff operations" [
      env (setup @Int) $ \ ~(newCmds, legacyCmds, model) ->
        bgroup "Comparative performance analysis" [
          bgroup "AntiDiff" [
              bcompareWithin 0.3 0.6 target $
                bench "Benchmark interpret" $
                  nf interpretNew newCmds
            , testProperty "Sanity check" $
                sanityCheck model newCmds (interpretNew newCmds)
            ]
        , bgroup "LegacyDiff" [
                bench "Benchmark interpret" $
                  nf interpretLegacy legacyCmds
            , testProperty "Sanity check" $
                sanityCheck model legacyCmds (interpretLegacy legacyCmds)
            ]
        ]
    , testProperty "deterministic: interpret AntiDiff == interpret LegacyDiff" $
        forAll (fst <$> makeSized generator' defaultGenConfig) $
          \cmds ->
              interpret @New @Int @Int cmds
            ===
              inside (interpret @Legacy @Int @Int) cmds
    , testProperty "non-deterministic: interpret AntiDiff == interpret LegacyDiff" $
        forAll (fst <$> makeSized generator defaultGenConfig) $
          \cmds ->
              interpret @New @Int @Int cmds
            ===
              inside (interpret @Legacy @Int @Int) cmds
    ]]
  where
    setup :: forall v. (Eq v, Arbitrary v) => IO ([Cmd New Int v], [Cmd Legacy Int v], Model Int v)
    setup = do
      (cmds, m) <- generate $ generator' defaultGenConfig
      pure (cmds, from cmds, m)

    interpretNew    = interpret @New @Int @Int
    interpretLegacy = interpret @Legacy @Int @Int

    target :: String
    target = "$(NF-1) == \"LegacyDiff\" && $NF == \"Benchmark interpret\""

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

data Imp = Legacy | New

data Cmd (i :: Imp) k v =
    Push     (SlotNo i)      (Diff i k v)
  | Flush    Int
  | Rollback Int
  | Forward  (Values i k v)  (Keys i k v)
  deriving stock Generic

-- TODO: Add trace to custom instance that shows when forced, don't keep in benchmark code.
-- TODO: Look at output with -ddump-deriv.
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

instance (Ord k, Eq v) => Isomorphism (Cmd New k v) (Cmd Legacy k v) where
  to :: Cmd New k v -> Cmd Legacy k v
  to (Push sl d)     = Push (to sl) (to d)
  to (Flush n)       = Flush n
  to (Rollback n)    = Rollback n
  to (Forward vs ks) = Forward (to vs) (to ks)

instance (Ord k, Eq v) => Isomorphism (Cmd Legacy k v) (Cmd New k v) where
  to :: Cmd Legacy k v -> Cmd New k v
  to (Push sl d)     = Push (to sl) (to d)
  to (Flush n)       = Flush n
  to (Rollback n)    = Rollback n
  to (Forward vs ks) = Forward (to vs) (to ks)

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Interpretation of @Cmd@s as a fold.
interpret ::
     forall i k v.
     (Ord k, Eq v, Comparable i, Monoid (DiffSeq i k v), NFData (DiffSeq i k v))
  => [Cmd i k v]
  -> DiffSeq i k v
interpret = foldl go mempty
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
      Rollback n    -> let (l, _r) = rollback n ds
                       in l
      -- FIXME(jdral): Should we @'seq'@ or @'deepseq'@ the forwarding result?
      -- Or, is there something else we should do with the result of @vs'@?
      Forward vs ks -> let vs' = forward vs ks ds
                       in  vs' `seq` ds

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
  type DiffSeq New k v = DS.DiffSeq TS.UTxO k v
  type Diff New k v    = TT.TableDiff TS.UTxO k v
  type Values New k v  = TT.TableValues TS.UTxO k v
  type Keys New k v    = TT.TableKeys TS.UTxO k v
  type SlotNo New      = DS.SlotNo

  push ds slot d   = DS.extend' ds (DS.Element slot d)
  flush            = DS.splitAt
  rollback         = DS.splitAtFromEnd
  forward vs ks ds = TT.forwardValuesAndKeys vs ks (DS.cumulativeDiff ds)

{-------------------------------------------------------------------------------
  Command generator: Configuration
-------------------------------------------------------------------------------}

-- | Configuration parameters for @'generator'@.
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
  }

-- | Configuration parameters for @'Push'@ command generation
data PushConfig = PushConfig {
    -- | How many key-value pairs to delete
    nrToDelete :: Int
    -- | How many key-value pairs to insert
  , nrToInsert :: Int
  }

newtype ForwardConfig = ForwardConfig {
    -- | How many keys to forward
    nrToForward :: Int
  }

newtype RollbackConfig = RollbackConfig {
    -- | Distribution of rollback sizes with respect to the security parameter
    -- @k@.
    --
    -- Each entry @(x, y)@ defines a QuickCheck frequency integer @x@, and
    -- floating point number @y@ that represents a percentage of the security
    -- parameter @k@. As such, @y@ determines the size of the rollback with
    -- respect to @k@, while @x@ determines the probability that this rollback
    -- size is picked.
    distribution :: [(Int, Float)]
  }

defaultGenConfig :: GenConfig
defaultGenConfig = GenConfig {
    nrCommands = 3000
  , nrInitialValues = 100
  , securityParameter = 200
  , pushConfig = PushConfig {
        nrToDelete = 50
      , nrToInsert = 50
      }
  , forwardConfig = ForwardConfig {
        nrToForward = 50
      }
  , rollbackConfig = RollbackConfig {
        distribution = [(95, 5), (5, 95)]
      }
  }

{-------------------------------------------------------------------------------
  Command generator: Model
-------------------------------------------------------------------------------}

data Model k v = Model {
    diffs         :: DS.DiffSeq 'TS.UTxO k v     -- ^ The current diff sequence
  , tip           :: Int                         -- ^ The tip of the diff
                                                 --   sequence
  , backingValues :: TT.TableValues 'TS.UTxO k v -- ^ Values that have been
                                                 --   flushed to a backing store
  , keyCounter    :: Int                         -- ^ A counter used to generate
                                                 --   unique keys
  , nrGenerated   :: Int                         -- ^ A counter for the number
                                                 --   of commands generated
                                                 --   up until now
  }
  deriving stock Generic
  deriving anyclass NFData

genInitialModel :: (Eq v, Arbitrary v) => GenConfig -> Gen (Model Int v)
genInitialModel GenConfig{nrInitialValues} = do
    kvs <- mapM genKeyValue [0 .. nrInitialValues - 1]
    pure $ Model DS.emptyDiffSeq 0 (TT.valuesFromList kvs) nrInitialValues 0

genKeyValue :: Arbitrary v => k -> Gen (k, v)
genKeyValue x = (x,) <$> arbitrary

{-------------------------------------------------------------------------------
  Command generator: Main generators
-------------------------------------------------------------------------------}

-- | A stateful generator for sequences of commands.
--
-- Note: We return the @'Model'@ because we might want to check invariants on
-- it.
generator :: (Eq v, Arbitrary v) => GenConfig -> Gen ([Cmd 'New Int v], Model Int v)
generator conf = do
  m <- genInitialModel conf
  runStateT (genCmds conf) m

-- | Like @'generator'@, but the order of operations is deterministic.
generator' :: (Eq v, Arbitrary v) => GenConfig -> Gen ([Cmd 'New Int v], Model Int v)
generator' conf = do
  m <- genInitialModel conf
  runStateT (genCmds' conf) m

-- | Adapt a @GenConfig@ured generator to override the configured @nrCommands@
-- parameter with a value that depends on the QuickCheck size parameter.
makeSized ::
     (GenConfig -> Gen a)
  -> GenConfig
  -> Gen a
makeSized fgen conf = do
  n <- getSize
  k <- choose (0, n)
  fgen (conf {nrCommands = k})

type CmdGen v a = StateT (Model Int v) Gen a

-- | Generate a list of commands in non-deterministic order.
genCmds :: (Eq v, Arbitrary v) => GenConfig -> CmdGen v [Cmd 'New Int v]
genCmds conf = mapM (const $ genCmd conf) [1 .. nrCommands conf]

-- | Generate a list of commands in deterministic order.
genCmds' :: forall v. (Eq v, Arbitrary v) => GenConfig -> CmdGen v [Cmd 'New Int v]
genCmds' conf = do
    cmds <- go (nrCommands conf)
    pure $ Exn.assert (length cmds == nrCommands conf) cmds
  where
    go n = do
      xs <- genN 10 (genForwardPushFlush (genN 10 genForwardPush))
      x  <- guard (singleton <$> genRollback conf)

      let
        xs0 = xs ++ x
        len = length xs0

      if length xs0 >= n then
        pure $ take n xs0
      else do
        xs0' <- go (n - len)
        pure $ xs0 ++ xs0'

    genForwardPushFlush :: CmdGen v [Cmd 'New Int v] -> CmdGen v [Cmd 'New Int v]
    genForwardPushFlush gen = do
      fps <- gen
      f   <- guard (singleton <$> genFlush conf)

      pure $ fps ++ f

    genForwardPush :: CmdGen v [Cmd 'New Int v]
    genForwardPush = do
      f <- guard (singleton <$> genForward conf)
      p <- guard (singleton <$> genPush conf)
      pure $ f ++ p

    guard :: CmdGen v [Cmd 'New Int v] -> CmdGen v [Cmd 'New Int v]
    guard gen = do
      nrg <- gets nrGenerated
      let nrc = nrCommands conf

      if nrg < nrc then
        gen
      else
        pure []

    singleton :: a -> [a]
    singleton x = [x]

    genN :: Int -> CmdGen v [Cmd 'New Int v] -> CmdGen v [Cmd 'New Int v]
    genN n gen = concat <$> replicateM n gen

-- | Variant of 'frequency' that allows for transformers of 'Gen'
frequency' :: (MonadTrans t, Monad (t Gen)) => [(Int, t Gen a)] -> t Gen a
frequency' [] = error "frequency' used with empty list"
frequency' xs0 = lift (choose (1, tot)) >>= (`pick` xs0)
  where
    tot = sum (map fst xs0)

    pick n ((k,x):xs)
      | n <= k    = x
      | otherwise = pick (n-k) xs
    pick _ _  = error "pick used with empty list"

genCmd :: (Eq v, Arbitrary v) => GenConfig -> CmdGen v (Cmd 'New Int v)
genCmd conf = do
    frequency' [
        (100, genPush conf)
      , (10, genFlush conf)
      , (1, genRollback conf)
      , (100, genForward conf)
      ]

{-------------------------------------------------------------------------------
  Command generator: Individual command generation
-------------------------------------------------------------------------------}

-- | Generate a @'Push'@ command.
--
-- > data Cmd (i :: Imp) k v =
-- >    Push (DiffSeq i k v) (SlotNo i) (Diff i k v)
-- >    -- ...
--
-- Steps to generate a @'Push'@ command:
-- * Forward backing values.
-- * "Simulate a transaction" by generating a diff that:
--   1. Deletes one or more values that exist in the forwarded backing values.
--   2. Inserts one or more values with globally unique keys.
-- * Return the previous diff sequence, a strictly increasing slot number, and
--   the new diff.
-- BOOKKEEPING: Push the new diff onto the model's diff sequence.
--
-- Note: https://iohk.io/en/blog/posts/2018/07/03/self-organisation-in-coin-selection/
--
-- TODO: Use coin selection other than random choice.
-- TODO: Generate unique, /random/ keys, instead of just incremental keys.
-- TODO: Skip some slots, instead of strictly incrementing @t@.
genPush :: (Eq v, Arbitrary v) => GenConfig -> CmdGen v (Cmd 'New Int v)
genPush conf = do
  let
    PushConfig{nrToInsert, nrToDelete} = pushConfig conf

  ds <- gets diffs
  t  <- gets tip
  vs <- gets backingValues
  kc <- gets keyCounter

  let
    _vs'@(TT.TableValues m) = TT.forwardValues vs (DS.cumulativeDiff ds)

  taken :: [(Int, v)] <-
    take nrToDelete <$> lift (shuffle (Map.toList m))
  made :: [(Int, v)]  <-
    lift $ mapM genKeyValue [kc .. kc + nrToInsert - 1]

  let
    d = TT.TableDiff . MapDiff.Diff $
         Map.fromList [(k, MapDiff.singletonDelete v) | (k,v) <- taken]
      <> Map.fromList [(k, MapDiff.singletonInsert v) | (k,v) <- made]

  modify (\st -> st {
      diffs = DS.extend' ds $ DS.Element (fromIntegral t) d
    , tip = t + 1
    , keyCounter = kc + nrToInsert
    , nrGenerated = nrGenerated st + 1
    })

  pure $ Push (fromIntegral t) d

-- | Generate a @'Flush'@ command.
--
-- >  data Cmd (i :: Imp) k v =
-- >    -- ...
-- >    | Flush Int (DiffSeq i k v)
-- >    -- ...
--
-- Steps to generate a @'Flush'@ command:
-- * Compute how many diffs @n@ in the diff sequence have a slot number that
--   exceeds the security parameter @k@.
-- * Return the current diff sequence and @n@.
-- BOOKKEEPING: Remove the first @n@ diffs from the models' diff sequence and
-- use them to forward the model's backing values.
genFlush :: Eq v => GenConfig -> CmdGen v (Cmd 'New Int v)
genFlush GenConfig{securityParameter = k} = do
  ds <- gets diffs
  t  <- gets tip
  vs <- gets backingValues

  let
    (l, r) = DS.splitAtSlot (fromIntegral $ t - k) ds
    n      = DS.length l

  modify (\st -> st {
      diffs         = r
    , backingValues = TT.forwardValues vs (DS.cumulativeDiff l)
    , nrGenerated = nrGenerated st + 1
    })

  pure $ Flush n

-- | Generate a @'Rollback'@ command.
--
-- >  data Cmd (i :: Imp) k v =
-- >    -- ...
-- >    | Rollback Int (DiffSeq i k v)
-- >    -- ...
--
-- Steps to generate a @'Rollback'@ command:
-- * Pick how much to rollback, i.e. an integer @n@.
--   * With high probability, we perform small rollbacks of around @0.05 * k@.
--   * With low probability, we perform large rollbacks of around @0.95 * k@.
-- * Return the current diff sequence and @n@.
-- BOOKKEEPING: Remove the last @n@ diffs from the models' diff sequence.
--
-- Note: We assume that pushes do not skip any slots.
genRollback :: Eq v => GenConfig -> CmdGen v (Cmd 'New Int v)
genRollback GenConfig{securityParameter = k, rollbackConfig} = do
    ds <- gets diffs
    t  <- gets tip

    let
      RollbackConfig{distribution = dist} =  rollbackConfig

    m <- frequency' [(x, pure $ ofK y) | (x, y) <- dist]

    let
      n      = min m $ DS.length ds
      (l, _r) = DS.splitAtFromEnd n ds

    modify (\st -> st {
        diffs = l
      , tip = t - n
      , nrGenerated = nrGenerated st + 1
      })

    pure $ Rollback n
  where
    ofK :: Float -> Int
    ofK p =
      let
        x :: Float
        x = p * fromIntegral k
      in
        round x

-- | Generate a @'Forward'@ command.
--
-- >  data Cmd (i :: Imp) k v =
-- >    -- ...
-- >    | Forward (Values i k v) (Keys i k v) (DiffSeq i k v)
--
-- Steps to generate a @'Forward'@ command:
-- * Determine which keys to forward.
-- * Retrieve (a subset of) the backing values from the model, i.e.,
--   /read/ the keys (obtained in previous step) from the backing values.
-- * Retrieve the current diff sequence from the model.
-- * Return the previous three combined.
-- BOOKKEEPING: None, since forwarding doesn't alter any model state.
--
-- Note: The keys to forward should be a superset of the keys of the backing
-- values that we retrieve from the model.
genForward :: Eq v => GenConfig -> CmdGen v (Cmd 'New Int v)
genForward conf = do
  let
    ForwardConfig{nrToForward} = forwardConfig conf

  ds  <- gets diffs
  bvs <- gets backingValues

  let
    d              = DS.cumulativeDiff ds
    TT.TableKeys s = TT.diffKeys d

  ksList <- take nrToForward <$> lift (shuffle (toList s))

  let
    ks = TT.TableKeys . Set.fromList $ ksList
    vs = TT.restrictValues bvs ks

  modify (\st -> st{
      nrGenerated = nrGenerated st + 1
    })

  pure $ Forward vs ks

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

-- | Sanity checks for interpretation as fold.
sanityCheck ::
     ( Eq k, Eq v, Show k, Show v
     , Isomorphism (DiffSeq i k v) (DS.DiffSeq 'TS.UTxO k v)
     )
  => Model k v
  -> [Cmd i k v]
  -> DiffSeq i k v
  -> Property
sanityCheck model _cmds result = diffs model === to result
