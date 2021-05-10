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

module LedgerOnDisk.QSM.Model where

import Data.Coerce
--import Data.Function

-- import Data.HashSet (HashSet)

import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
-- import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Kind
-- import Data.Monoid
import Data.TreeDiff.Expr
import Data.Typeable
import GHC.Generics
import LedgerOnDisk.Class
import LedgerOnDisk.Pure
import LedgerOnDisk.Simple
import Test.QuickCheck
import Test.StateMachine
import Test.StateMachine.Lockstep.Simple

-- import Test.StateMachine.Types.References

data MonadKVStateMachine (m :: Type -> Type)

stateMachineTest ::
  ( Typeable m,
    SimpleMonadKV m,
    Eq (Err m),
    Show (Err m),
    Show (ResultSet m),
    Eq (ResultSet m) -- this is perhaps unreasonable
  ) =>
  SimpleMap ->
  (forall a. m a -> IO a) ->
  StateMachineTest (MonadKVStateMachine m)
stateMachineTest initial_map toIO =
  StateMachineTest
    { runMock = kvRunMock,
      runReal = kvRunReal toIO,
      initMock = nonemptyMock initial_map,
      newHandles = kvNewHandles,
      generator = kvGenerator,
      shrinker = kvShrinker,
      cleanup = kvCleanup
    }

type KVModel m = Model (MonadKVStateMachine m)

data instance Resp (MonadKVStateMachine m) h where
  KVSuccessLookupAll_ :: HashMap Int Int -> Resp (MonadKVStateMachine m) h
  KVSuccessHandle :: h -> Resp (MonadKVStateMachine m) h
  KVSuccessResult :: Int -> Resp (MonadKVStateMachine m) h
  KVError :: SimpleMonadKV m => Err m -> Resp (MonadKVStateMachine m) h
  KVBaseError :: BaseError -> Resp (MonadKVStateMachine m) h

type KVResp m = Resp (MonadKVStateMachine m)

deriving stock instance Functor (KVResp m)

deriving stock instance Foldable (KVResp m)

deriving stock instance Traversable (KVResp m)

deriving stock instance (SimpleMonadKV m, Eq (Err m), Eq (Err m), Eq h) => Eq (KVResp m h)

deriving stock instance (SimpleMonadKV m, Show (Err m), Show (Err m), Show h) => Show (KVResp m h)

data OperationFunction k v a where
  OFArb :: Fun (HashMap k (Maybe v)) (OperationResult k v, a) -> OperationFunction k v a
  OFSet :: String -> (HashMap k (Maybe v) -> (OperationResult k v, a)) -> OperationFunction k v a

type SimpleOperationFunction = OperationFunction SimpleKey SimpleValue Int

instance (Show k, Show v, Show a) => Show (OperationFunction k v a) where
  showsPrec d (OFArb f) = showsPrec d f
  showsPrec d (OFSet n _) = showsPrec d n

instance (Eq k, Hashable k, Function k, Function v, CoArbitrary k, CoArbitrary v, Arbitrary k, Arbitrary v, Arbitrary a) => Arbitrary (OperationFunction k v a) where
  arbitrary = OFArb <$> arbitrary
  shrink = \case
    OFArb f -> OFArb <$> shrink f
    OFSet {} -> []

applyOperationFunction :: OperationFunction k v a -> HashMap k (Maybe v) -> (OperationResult k v, a)
applyOperationFunction = \case
  OFArb (Fn f) -> f
  OFSet _ f -> f
  _ -> error "impossible"

pattern OFn :: (HashMap k (Maybe v) -> (OperationResult k v, a)) -> OperationFunction k v a
pattern OFn f <- (applyOperationFunction -> f)

data instance Cmd (MonadKVStateMachine m) h where
  KVPrepare :: QueryScope Int -> Cmd (MonadKVStateMachine m) h
  KVSubmit :: h -> OperationFunction Int Int Int -> Cmd (MonadKVStateMachine m) h -- k = v = Int, always returns Int
  -- suffixed with _ means it can be generated, it's for validating the model
  KVLookupAll_ :: h -> Cmd (MonadKVStateMachine m) h
  deriving stock (Functor, Foldable, Traversable, Show)

type KVCmd m = Cmd (MonadKVStateMachine m)

newtype instance RealHandle (MonadKVStateMachine m) = KVRealHandle (ResultSet m)
  deriving stock (Generic)

type KVRealHandle m = RealHandle (MonadKVStateMachine m)

deriving stock instance Eq (ResultSet m) => Eq (KVRealHandle m)

deriving stock instance Show (ResultSet m) => Show (KVRealHandle m)

instance ToExpr (KVRealHandle m) where
  toExpr _ = App "KVRealHandle" []

newtype instance MockHandle (MonadKVStateMachine m) = KVMockHandle Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToExpr)

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

instance Arbitrary Mock where
  arbitrary = arbitrary <&> \x -> Mock x mempty 0
  shrink s@Mock{modelMap} = shrink modelMap <&> \m -> s { modelMap = m}

emptyMock :: Mock
emptyMock = Mock mempty mempty 0

nonemptyMock :: SimpleMap -> Mock
nonemptyMock x = emptyMock { modelMap = x }

mockAddQuery :: QueryScope Int -> Mock -> (KVMockHandle m, Mock)
mockAddQuery qs m@Mock { nextQueryId, queries} =
  ( KVMockHandle nextQueryId,
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

kvRunMock :: KVCmd m (KVMockHandle m) -> KVState m -> (KVResp m (KVMockHandle m), KVState m)
kvRunMock cmd s@Mock {..} = case cmd of
  KVLookupAll_ (KVMockHandle i) -> case i `HashMap.lookup` queries of
    Nothing -> (KVBaseError BEBadResultSet, s)
    Just qs -> let
      (a, _new_map) = pureApplyOperation (coerce qs) (\x -> (mempty, HashMap.mapMaybe id x)) modelMap
      in (KVSuccessLookupAll_ a, s) -- no change to state, don't even delete query
  KVPrepare qs -> case mockAddQuery qs s of
    (h, s') -> (KVSuccessHandle h, s')
  KVSubmit (KVMockHandle i) (OFn f) -> case i `HashMap.lookup` queries of
    Nothing -> (KVBaseError BEBadResultSet, s)
    Just qs ->
      let (a, new_map) = pureApplyOperation (coerce qs) f modelMap
       in ( KVSuccessResult a,
            s
              { modelMap = new_map,
                queries = i `HashMap.delete` queries
              }
          )
  -- Gives incomplete pattern warnings without this. I
  -- believe it's because Fn isn't properly marked as COMPLETE
  _ -> error "impossible"

kvRunReal :: forall m. SimpleMonadKV m => (forall a. m a -> IO a) -> KVCmd m (KVRealHandle m) -> IO (KVResp m (KVRealHandle m))
kvRunReal toIO = \case
  KVPrepare qs -> toIO $ prepareOperation qs <&> KVSuccessHandle . KVRealHandle
  KVSubmit (KVRealHandle rs) (OFn f) -> toIO $ submitOperation rs f <&> \case
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

kvNewHandles :: forall m. forall h. KVResp m h -> [h]
kvNewHandles = toList -- I think?

kvGenerator :: KVModel m Symbolic -> Maybe (Gen (KVCmd m :@ Symbolic))
kvGenerator Model {modelRefs} = Just $ do
  let prepare = At . KVPrepare <$> arbitrary
  let submit = do
        (ref, _mockhandle) <- elements modelRefs
        f <- arbitrary
        pure . At $ KVSubmit ref f

  oneof $ [prepare] ++ [submit | not . null $ modelRefs]

kvShrinker :: KVModel m Symbolic -> KVCmd m :@ Symbolic -> [KVCmd m :@ Symbolic]
kvShrinker _model (At cmd) = case cmd of
  KVPrepare qs -> At . KVPrepare <$> shrink qs
  KVSubmit ref f -> At . KVSubmit ref <$> shrink f
  _ -> error "invalid KVCmd"

kvCleanup :: KVModel m Concrete -> IO ()
kvCleanup _ = pure ()
