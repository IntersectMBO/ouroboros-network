{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# LANGUAGE UndecidableInstances    #-}

module Bench.Ouroboros.Consensus.Storage.LedgerDB.HD (benchmarks) where

import           Control.DeepSeq
import           Control.Monad.Trans.State (StateT (..))
--import           Criterion
import           GHC.Generics (Generic)
import           Test.QuickCheck hiding (Result)
import           Test.Tasty.Bench
import           Test.Tasty.QuickCheck (testProperty)

import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.TableTypes as TT
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.ToStore as TS

import           Test.Util.Orphans.NFData ()

-- FIXME(jdral): Determine whether we should use @'whnf'@ or @'nf'@.
benchmarks :: Benchmark
benchmarks = bgroup "HD" [ bgroup "DiffSeq/Diff operations" [
      env (setup @Int @Int) $ \ ~(newCmds, legacyCmds) ->
        bgroup "Comparative performance analysis" [
            bcompare "Legacy" $
              bench "New"    $ nf (interpret @New @Int @Int) newCmds
          ,   bench "Legacy" $ nf (interpret @Legacy @Int @Int) legacyCmds
          , testProperty "New == Legacy" $
                interpret @New @Int @Int newCmds
              ===
                to (interpret @Legacy @Int @Int legacyCmds)
          ]
    , testProperty "Property test: New == Legacy" $
        forAll (fst <$> generator defaultGenConfig) $
          \cmds ->
              interpret @New @Int @Int cmds
            ===
              inside (interpret @Legacy @Int @Int) cmds
    ]]
  where
    setup :: forall k v. IO ([Cmd New k v], [Cmd Legacy k v])
    setup = do
      (cmds, _m) <- generate $ generator defaultGenConfig
      pure (cmds, from cmds)

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

data Imp = Legacy | New

data Cmd (i :: Imp) k v =
    Push     (DiffSeq i k v) (SlotNo i)      (Diff i k v)
  | Flush    Int             (DiffSeq i k v)
  | Rollback Int             (DiffSeq i k v)
  | Forward  (Values i k v)  (Diff i k v)
  deriving stock Generic

data Result (i :: Imp) k v =
    RPush     !(DiffSeq i k v)
  | RFlush    !(DiffSeq i k v, DiffSeq i k v)
  | RRollback !(DiffSeq i k v, DiffSeq i k v)
  | RForward  !(Values i k v)
  deriving stock Generic

deriving anyclass instance ( NFData (DiffSeq i k v)
                           , NFData (Diff i k v)
                           , NFData (Values i k v)
                           , NFData (SlotNo i)
                           ) => NFData (Cmd i k v)

deriving anyclass instance ( NFData (DiffSeq i k v)
                           , NFData (Diff i k v)
                           , NFData (Values i k v)
                           ) => NFData (Result i k v)

deriving stock instance ( Eq (DiffSeq i k v)
                        , Eq (Diff i k v)
                        , Eq (Values i k v)
                        ) => Eq (Result i k v)

deriving stock instance ( Show (DiffSeq i k v)
                        , Show (Diff i k v)
                        , Show (Values i k v)
                        , Show (SlotNo i)
                        ) => Show (Cmd i k v)

deriving stock instance ( Show (DiffSeq i k v)
                        , Show (Diff i k v)
                        , Show (Values i k v)
                        ) => Show (Result i k v)

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

interpret :: forall i k v. Comparable i => [Cmd i k v] -> [Result i k v]
interpret = map go'
  where
    go' :: Cmd i k v -> Result i k v
    go' (Push ds sl d)  = RPush $ push ds sl d
    go' (Flush n ds)    = RFlush $ flush n ds
    go' (Rollback n ds) = RRollback $ rollback n ds
    go' (Forward vs d)  = RForward $ forward vs d

class Comparable (i :: Imp) where
  type DiffSeq i k v = r | r -> i k v
  type Diff i k v    = r | r -> i k v
  type Values i k v  = r | r -> i k v
  type SlotNo i      = r | r -> i

  push     :: DiffSeq i k v -> SlotNo i -> Diff i k v -> DiffSeq i k v
  flush    :: Int -> DiffSeq i k v -> (DiffSeq i k v, DiffSeq i k v)
  rollback :: Int -> DiffSeq i k v -> (DiffSeq i k v, DiffSeq i k v)
  forward  :: Values i k v -> Diff i k v -> Values i k v

instance Comparable Legacy where
  type DiffSeq Legacy k v = HD.SeqUtxoDiff k v
  type Diff Legacy k v    = HD.UtxoDiff k v
  type Values Legacy k v  = HD.UtxoValues k v
  type SlotNo Legacy      = Block.SlotNo

  push     = error "Not implemented: Legacy.push"
  flush    = error "Not implemented: Legacy.flush"
  rollback = error "Not implemented: Legacy.rollback"
  forward  = error "Not implemented: Legacy.forward"

instance Comparable New where
  type DiffSeq New k v = DS.DiffSeq TS.UTxO k v
  type Diff New k v    = TT.TableDiff TS.UTxO k v
  type Values New k v  = TT.TableValues TS.UTxO k v
  type SlotNo New      = DS.SlotNo

  push     = error "Not implemented: New.push"
  flush    = error "Not implemented: New.flush"
  rollback = error "Not implemented: New.rollback"
  forward  = error "Not implemented: New.forward"

{-------------------------------------------------------------------------------
  Stateful generator for command sequences
-------------------------------------------------------------------------------}

-- | Configuration parameters for @'generator'@.
data GenConfig = GenConfig {
    size   :: Int -- ^ Desired ength of command sequence
  , others :: ()
  }

defaultGenConfig :: GenConfig
defaultGenConfig = GenConfig 100 ()

-- TODO(jdral): The generator should be implemented as a /stateful/ generator.
-- It should generate sequence of commands WRT to some model state, which keeps
-- track of flushed values, the current diff, etc.
--
-- Note: We return the @'Model'@ because we might want to check invariants on
-- it, for example.
generator :: GenConfig -> Gen ([Cmd i k v], Model)
generator conf = do
  m <- genInitialModel conf
  runStateT (genCmds conf) m

-- TODO(jdral): The datatype is not complete. The model should keep track of
-- state like the current diff sequence, values in the backing store, etc.
data Model = Model {
    diffs         :: ()
  , backingValues :: ()
  }

genInitialModel :: GenConfig -> Gen Model
genInitialModel _conf = pure $ Model () ()

genCmds :: GenConfig -> StateT Model Gen [Cmd i k v]
genCmds conf = mapM (const $ genCmd conf) [1 .. size conf]

genCmd :: GenConfig -> StateT Model Gen (Cmd i k v)
genCmd = error "Not implemented: genCmd"


{-------------------------------------------------------------------------------
  @'Legacy'@ and @'New'@ commands/results are isomorphic
-------------------------------------------------------------------------------}

-- FIXME(jdral): This class is based on the @IsomorphicTo@ class from REF. We
-- can not use the package at this time because the cabal update index is too
-- outdated. Once the index has been updated to a more recent one, we should
-- switch to this package.
--
-- REF: https://hackage.haskell.org/package/isomorphism-class-0.1.0.5/docs/IsomorphismClass.html
class Isomorphism b a => Isomorphism a b where
  to :: a -> b

from :: Isomorphism b a => a -> b
from = to

inside :: (Isomorphism a b, Isomorphism c d) => (b -> c) -> a -> d
inside f = from . f . to

instance Isomorphism a b => Isomorphism [a] [b] where
  to :: [a] -> [b]
  to = fmap to

instance (Isomorphism a c, Isomorphism b d) => Isomorphism (a, b) (c, d) where
  to :: (a, b) -> (c, d)
  to (x, y) = (to x, to y)

-- | Given that @'Legacy'@ and
instance Isomorphism (Cmd New k v) (Cmd Legacy k v) where
  to :: Cmd New k v -> Cmd Legacy k v
  to (Push ds sl d)  = Push (to ds) (to sl) (to d)
  to (Flush n ds)    = Flush n (to ds)
  to (Rollback n ds) = Rollback n (to ds)
  to (Forward vs d)  = Forward (to vs) (to d)

instance Isomorphism (Cmd Legacy k v) (Cmd New k v) where
  to :: Cmd Legacy k v -> Cmd New k v
  to = from @(Cmd New k v)

instance Isomorphism (Result New k v) (Result Legacy k v) where
  to :: Result New k v -> Result Legacy k v
  to (RPush ds)      = RPush $ to ds
  to (RFlush dss)    = RFlush $ to dss
  to (RRollback dss) = RRollback $ to dss
  to (RForward vs)   = RForward $ to vs

instance Isomorphism (Result Legacy k v) (Result New k v) where
  to :: Result Legacy k v -> Result New k v
  to = from @(Result New k v)

instance Isomorphism (DS.DiffSeq ts k v) (HD.SeqUtxoDiff k v) where
  to :: DS.DiffSeq ts k v -> HD.SeqUtxoDiff k v
  to = error "Not implemented"

instance Isomorphism (HD.SeqUtxoDiff k v) (DS.DiffSeq ts k v) where
  to :: HD.SeqUtxoDiff k v -> DS.DiffSeq ts k v
  to = from @(DS.DiffSeq ts k v)

instance Isomorphism (TT.TableDiff ts k v) (HD.UtxoDiff k v) where
  to = error "Not implemented"

instance Isomorphism (HD.UtxoDiff k v) (TT.TableDiff ts k v) where
  to :: HD.UtxoDiff k v -> TT.TableDiff ts k v
  to = from @(TT.TableDiff ts k v)

instance Isomorphism (TT.TableValues ts k v) (HD.UtxoValues k v) where
  to :: TT.TableValues ts k v -> HD.UtxoValues k v
  to = error "Not implemented"

instance Isomorphism (HD.UtxoValues k v) (TT.TableValues ts k v) where
  to :: HD.UtxoValues k v -> TT.TableValues ts k v
  to = from @(TT.TableValues ts k v)

instance Isomorphism DS.SlotNo Block.SlotNo where
  to :: DS.SlotNo -> Block.SlotNo
  to = error "Not implemented"

instance Isomorphism Block.SlotNo DS.SlotNo where
  to :: Block.SlotNo -> DS.SlotNo
  to = from @DS.SlotNo

{-------------------------------------------------------------------------------
  Archive: benchmarking single
-------------------------------------------------------------------------------}

{-
benchExtend :: Benchmark
benchExtend = env setup $ \ ~(ds, sl, d) ->
    bgroup "extend" [
        bench "Legacy" $
          whnf
            (uncurry DS.extend')
            (ds, DS.Element sl d)
      , bench "New"    $
          whnf
            (uncurry . uncurry $ HD.extendSeqUtxoDiff)
            (to $ ((ds, sl), d)
      ]
  where
    setup :: (Ord k, Eq v) => IO (DS.DiffSeq dt k v , DS.SlotNo, DS.TableDiff dt k v)
    setup = error "setup not implemented"

-}
