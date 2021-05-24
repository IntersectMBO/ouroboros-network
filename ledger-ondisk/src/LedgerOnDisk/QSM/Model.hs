{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS -fno-warn-unused-imports #-}

-- from quickcheck-state-machine, see very bottom of file
{-# OPTIONS_GHC -Wno-orphans #-}

module LedgerOnDisk.QSM.Model where

import Data.Coerce
import Data.Function ()

import Data.HashSet (HashSet)

import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Kind
import Data.TreeDiff.Expr
import Data.Typeable
import GHC.Generics hiding ((:.:))
import LedgerOnDisk.Class
import LedgerOnDisk.Pure
import LedgerOnDisk.Simple
import Test.QuickCheck
import Test.StateMachine
import qualified Control.Foldl as Foldl
import Control.Foldl (Fold)
import Data.Monoid
import Test.StateMachine.Lockstep.NAry
import Test.StateMachine.Types
import Data.SOP hiding (Fn)
import Test.StateMachine.Lockstep.Auxiliary
import Test.Tasty.QuickCheckStateMachine
import Test.StateMachine.Labelling
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.Functor.Classes
-- import Test.StateMachine.Types.References

data MonadKVStateMachine (m :: Type -> Type)

stateMachineTest ::
  ( Typeable m,
    SimpleMonadKV m,

    Eq (Err m),
    Show (Err m),
    ToExpr (ReadSet m),
    Eq (ReadSet m),
    Show (ReadSet m)
  ) =>
  Int ->
  SimpleMap ->
  (forall a. m a -> IO a) ->
  StateMachineTest (MonadKVStateMachine m)
stateMachineTest tickets initial_map toIO =
  StateMachineTest
    { runMock = kvRunMock,
      runReal = kvRunReal toIO,
      initMock = nonemptyMock initial_map,
      newHandles = kvNewHandles,
      generator = kvGenerator tickets,
      shrinker = kvShrinker,
      cleanup = kvCleanup
    }

type KVModel m = Model (MonadKVStateMachine m)
type KVStateMachineTest m = StateMachineTest (MonadKVStateMachine m)

data instance Resp (MonadKVStateMachine m) f hs where
  KVSuccessLookupAll_ :: HashMap Int Int -> Resp (MonadKVStateMachine m) f '[resultSet]
  KVSuccessHandle :: f resultSet -> Resp (MonadKVStateMachine m) f '[resultSet]
  KVSuccessResult :: Int -> Resp (MonadKVStateMachine m) f '[resultSet]
  KVError :: SimpleMonadKV m => Err m -> Resp (MonadKVStateMachine m) f '[resultSet]
  KVBaseError :: BaseError -> Resp (MonadKVStateMachine m) f '[resultSet]

deriving stock instance (forall a. Eq a => Eq (f a), Eq resultSet, Eq (Err m)) => Eq (Resp (MonadKVStateMachine m) f '[resultSet])
deriving stock instance (forall a. Show a => Show (f a), Show resultSet, Show (Err m)) => Show (Resp (MonadKVStateMachine m) f '[resultSet])

instance NTraversable (KVResp n) where
  -- nctraverse :: forall m c proxy g h resultSet xs xs'. (Applicative m,
  --                                               Data.SOP.All c xs,
  --                                               xs ~ (resultSet ': xs')
  --                                               )
  --   => proxy c -> (forall a. c a => Elem xs a -> g a -> m (h a)) -> KVResp n g xs -> m (KVResp n h xs)
  nctraverse _ f = \case
    (KVSuccessHandle fh :: KVResp n g xs) -> KVSuccessHandle <$> f ElemHead  fh
    -- There has to be a better way
    KVSuccessLookupAll_ r -> pure $ KVSuccessLookupAll_ r
    KVSuccessResult i -> pure $ KVSuccessResult i
    KVError e -> pure $ KVError e
    KVBaseError e -> pure $ KVBaseError e

type KVResp m = Resp (MonadKVStateMachine m)

data OperationFunction k v a where
  OFArb :: Fun (HashMap k (Maybe v)) (KVOperationResult k v, a) -> OperationFunction k v a
  OFSet :: String -> (HashMap k (Maybe v) -> (KVOperationResult k v, a)) -> OperationFunction k v a

type SimpleOperationFunction = OperationFunction SimpleKey SimpleValue Int

instance (Show k, Show v, Show a) => Show (OperationFunction k v a) where
  showsPrec d (OFArb f) = showsPrec d f
  showsPrec d (OFSet n _) = showsPrec d n

instance (Eq k, Hashable k, Function k, Function v, CoArbitrary k, CoArbitrary v, Arbitrary k, Arbitrary v, Arbitrary a) => Arbitrary (OperationFunction k v a) where
  arbitrary = OFArb <$> arbitrary
  shrink = \case
    OFArb f -> OFArb <$> shrink f
    OFSet {} -> []

applyOperationFunction :: OperationFunction k v a -> HashMap k (Maybe v) -> (KVOperationResult k v, a)
applyOperationFunction = \case
  OFArb (Fn f) -> f
  OFSet _ f -> f
  _ -> error "impossible"

pattern OFn :: (HashMap k (Maybe v) -> (KVOperationResult k v, a)) -> OperationFunction k v a
pattern OFn f <- (applyOperationFunction -> f)
{-# COMPLETE OFn #-}

data instance Cmd (MonadKVStateMachine m) f hs where
  KVPrepare :: QueryScope Int -> Cmd (MonadKVStateMachine m) f '[resultSet]
  KVSubmit :: f resultSet -> OperationFunction Int Int Int -> Cmd (MonadKVStateMachine m) f '[resultSet] -- k = v = Int, always returns Int
  -- suffixed with _ means it can be generated, it's for validating the model
  KVLookupAll_ :: f resultSet -> Cmd (MonadKVStateMachine m) f '[resultSet]
  -- deriving stock (Functor, Foldable, Traversable, Show)


deriving stock instance (forall a. Show a => Show (f a), Show resultSet, Show (Err m)) => Show (Cmd (MonadKVStateMachine m) f '[resultSet])
-- deriving stock instance (forall a. Eq a => Eq (f a), Eq resultSet, Eq (Err m)) => Eq (Resp (MonadKVStateMachine m) f '[resultSet])

type KVCmd m = Cmd (MonadKVStateMachine m)

instance NTraversable (KVCmd m) where
  nctraverse _ go = \case
    KVPrepare s -> pure $ KVPrepare s
    KVSubmit fr f -> (`KVSubmit` f) <$> go ElemHead fr
    KVLookupAll_ fr -> KVLookupAll_ <$> go ElemHead fr

type instance RealHandles (MonadKVStateMachine m) = '[ReadSet m]

type KVRealHandles m = RealHandles (MonadKVStateMachine m)

-- deriving stock instance Eq (ReadSet m) => Eq (KVRealHandle m)

-- deriving stock instance Show (ReadSet m) => Show (KVRealHandle m)

-- instance ToExpr (KVRealHandle m) where
--   toExpr _ = App "KVRealHandle" []

data instance MockHandle (MonadKVStateMachine m) a where
  MockReadSet :: Int -> MockHandle (MonadKVStateMachine m) (ReadSet m)

deriving stock instance Eq (KVMockHandle m a)
deriving stock instance Show (KVMockHandle m a)

instance ToExpr (MockHandle (MonadKVStateMachine m) a) where
  toExpr (MockReadSet i) = toExpr i

type KVMockHandle m = MockHandle (MonadKVStateMachine m)

data Mock = Mock
  { modelMap :: !(HashMap Int Int),
    queries :: !(HashMap Int (QueryScope Int)),
    nextQueryId :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToExpr)

type instance MockState (MonadKVStateMachine m) = Mock

type KVState m = MockState (MonadKVStateMachine m)

type instance RealMonad (MonadKVStateMachine m) = IO

instance Arbitrary Mock where
  arbitrary = arbitrary <&> \x -> Mock x mempty 0
  shrink s@Mock{modelMap} = shrink modelMap <&> \m -> s { modelMap = m}

emptyMock :: Mock
emptyMock = Mock mempty mempty 0

nonemptyMock :: SimpleMap -> Mock
nonemptyMock x = emptyMock { modelMap = x }

mockAddQuery :: QueryScope Int -> Mock -> (KVMockHandle m (ReadSet m), Mock)
mockAddQuery qs m@Mock { nextQueryId, queries} =
  (  MockReadSet nextQueryId,
    m {
        queries = HashMap.insert nextQueryId qs queries,
        LedgerOnDisk.QSM.Model.nextQueryId = nextQueryId + 1
      }
  )

-- newtype ArbDiffItem v = ArbDiffItem { unArbDiffItem :: DiffItem v }
--   deriving stock (Eq, Show, Generic)

-- instance Arbitrary v => Arbitrary (ArbDiffItem v) where
--   arbitrary = ArbDiffItem <$> oneof [ pure DIRemove, DIUpdate <$> arbitrary ]
--   shrink (ArbDiffItem di)= case di of
--     DIRemove -> []
--     DIUpdate v -> ArbDiffItem . DIUpdate <$> shrink v

-- instance Semigroup Mock where
--   m1 <> m2 = Mock
--    { modelMap = ((<>) `on` modelMap) m1 m2
--    , queries = ((<>) `on` queries) m1 m2
--    , nextQueryId = (max `on` nextQueryId) m1 m2
--    }

-- instance Monoid Mock where
--   mempty = Mock
--     { modelMap = mempty
--     , queries = mempty
--     , nextQueryId = 0
--     }

kvRunMock :: KVCmd m (KVMockHandle m) (KVRealHandles m) -> KVState m -> (KVResp m (KVMockHandle m) (KVRealHandles m), KVState m)
kvRunMock cmd s@Mock {..} = case cmd of
  KVLookupAll_(MockReadSet i) -> case i `HashMap.lookup` queries of
    Nothing -> (KVBaseError BEBadReadSet, s)
    Just qs -> let
      (a, _new_map) = pureApplyOperation (coerce qs) (\x -> (mempty, HashMap.mapMaybe id x)) modelMap
      in (KVSuccessLookupAll_ a, s) -- no change to state, don't even delete query
  KVPrepare qs -> case mockAddQuery qs s of
    (h, s') -> (KVSuccessHandle h, s')
  KVSubmit (MockReadSet i) (OFn f) -> case i `HashMap.lookup` queries of
    Nothing -> (KVBaseError BEBadReadSet, s)
    Just qs ->
      let (a, new_map) = pureApplyOperation (coerce qs) f modelMap
       in ( KVSuccessResult a,
            s
              { modelMap = new_map,
                queries = HashMap.filterWithKey (\k _ -> k > i) queries
              }
          )

kvRunReal :: forall m. SimpleMonadKV m
  => (forall a. m a -> IO a)
  -> KVCmd m I (KVRealHandles m)
  -> IO (KVResp m I (KVRealHandles m))
kvRunReal toIO = \case
  KVPrepare qs -> toIO $ prepareOperation qs <&> KVSuccessHandle . I
  KVSubmit (I rs) (OFn f) -> toIO $ submitOperation rs f <&> \case
    Left e
      | Just e' <- toKVBaseError (Proxy @ m) e -> KVBaseError e'
      | otherwise -> KVError e
    Right r -> KVSuccessResult r
  _ -> error "impossible"

-- kvInitMock :: KVState m
-- kvInitMock =
--   Mock
--     { modelMap = mempty,
--       queries = mempty,
--       nextQueryId = 0
--     }

kvNewHandles :: forall m f. KVResp m f (KVRealHandles m) -> NP ([] :.: f) (KVRealHandles m)
kvNewHandles = \case
  KVSuccessHandle fh -> Comp [fh] :* Nil
  _ -> mempty

kvGenerator :: Int -> KVModel m Symbolic -> Maybe (Gen (KVCmd m :@ Symbolic))
kvGenerator max_refs Model {modelRefss = Refss (Refs resultSets :* Nil)} = Just $ do
  let prepare = At . KVPrepare <$> arbitrary
  let submit = do
        (ref, _mockhandle) <- elements resultSets
        f <- arbitrary
        pure . At $ KVSubmit (FlipRef ref) f

  oneof $
    [prepare | length resultSets < max_refs] ++
    [submit | not . null $ resultSets]

kvShrinker :: KVModel m Symbolic -> KVCmd m :@ Symbolic -> [KVCmd m :@ Symbolic]
kvShrinker _model (At cmd) = case cmd of
  KVPrepare qs -> At . KVPrepare <$> shrink qs
  KVSubmit ref f -> At . KVSubmit ref <$> shrink f
  _ -> error "invalid KVCmd"

kvCleanup :: KVModel m Concrete -> IO ()
kvCleanup _ = pure ()



-- withArbitraryCmdList :: (SimpleMonadKV m, Testable prop, Show (Err m), Show (ReadSet m)) => Maybe Int -> KVStateMachineTest m -> ([(KVCmd m :@ Symbolic, KVResp m :@ Symbolic, [Var])] -> prop) -> Property
-- withArbitraryCmdList mb_min_cmds smt go = forAllCommands sm mb_min_cmds $ go . unCommands where
--   sm = toStateMachine smt
  -- commands_to_list (Commands cs) =
  --   [ (cmd, resp, vars)
  --   | Command (cmdAtToSimple -> At cmd) (respMockToSimple -> At resp) vars <- cs
  --   ]
  -- do_cmd :: KVCmd m ()

data KVCmdListTag
  = THasSubmit
  | THasOutOfOrderSubmit
  | THasDisallowedConstructors
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

tagKVCmdList :: forall f e m. (Ord (ReadSet m), Foldable f, e ~ Event (KVModel m) (At (KVCmd m) ) (At (KVResp m)) Symbolic)
  => f e -> [KVCmdListTag]
tagKVCmdList = HashSet.toList . Foldl.fold the_fold   where
    the_fold :: Fold e (HashSet KVCmdListTag)
    the_fold = fold -- lol
        [ hasSubmit
        , hasOutOfOrderSubmit
        , hasDisallowedConstructors
        ]

    tagTrue x (Any b) = if b then HashSet.singleton x else mempty

    hasSubmit :: Fold e (HashSet KVCmdListTag)
    hasSubmit = Foldl.foldMap go $ tagTrue THasSubmit where
      go :: e -> Any
      go = coerce . \case
        Event { eventCmd = At KVSubmit {}} -> True
        _ -> False

    hasOutOfOrderSubmit  :: Fold e (HashSet KVCmdListTag)
    hasOutOfOrderSubmit = Foldl.foldMap go finish where
      finish e = case appEndo e mempty of
        (_rs_to_seen_vars_map, _submitted_vars_set, any_ooo_submits) -> tagTrue THasOutOfOrderSubmit any_ooo_submits
      go :: e -> Endo (Map _ (Set _), Set _, Any)
      go Event{eventResp, eventCmd} = Endo $ \x@(rs_to_seen_vars_map, submitted_vars_set, _) -> case () of
        -- when we introduce a new result set, record which other result sets preceeded this one
        _ | At (KVSuccessHandle ref ) <- eventResp
          , let seen_vars = Map.keysSet rs_to_seen_vars_map
          -> x <> (Map.singleton ref seen_vars, mempty, mempty)

        -- when we see a submitted a result set, see if anything preceeding it has been submitted
          | At (KVSubmit ref _) <- eventCmd
          , let is_ooo = Any . not . null $
                  fromMaybe mempty (Map.lookup ref rs_to_seen_vars_map) `Set.difference` submitted_vars_set
            -> x <> (mempty, Set.singleton ref, is_ooo)
        _ -> x

    hasDisallowedConstructors   :: Fold e (HashSet KVCmdListTag)
    hasDisallowedConstructors = Foldl.foldMap go $ tagTrue THasDisallowedConstructors where
      go Event{eventCmd} = case eventCmd of
        At KVLookupAll_ {} -> Any True
        _ -> mempty

-- testLabelStateMachine :: KVStateMachineTest m -> LabellingTest
-- testLabelStateMachine  m = labellingTest (toStateMachine m) $ \xs -> show <$> tagKVCmdList [eventCmd | Event{..} <- xs]

deriving stock instance (Eq h, Eq1 r) => Eq (FlipRef r h)
deriving stock instance (Ord h, Ord1 r) => Ord (FlipRef r h)
