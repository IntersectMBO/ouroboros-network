{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
module LedgerOnDisk.QSM.Model where

import GHC.Generics
import Data.Typeable
import Data.Coerce
import Data.Monoid
import Data.Kind
--import Data.Function
import Data.Functor

import Data.HashMap.Strict (HashMap)
-- import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.StateMachine.Lockstep.Simple
import Test.StateMachine
import Test.QuickCheck
import Data.TreeDiff.Expr

import LedgerOnDisk.Simple
import LedgerOnDisk.Class
import Data.Foldable
-- import Test.StateMachine.Types.References

data MonadKVStateMachine (m :: Type -> Type)

stateMachineTest ::
  (Typeable m, SimpleMonadKV m, Eq (Err m), Show (Err m)
  , Show (ResultSet m), Eq (ResultSet m) -- this is perhaps unreasonable
  ) => (forall a. m a -> IO a) -> StateMachineTest (MonadKVStateMachine m)
stateMachineTest toIO = StateMachineTest
  { runMock = kvRunMock
  , runReal = kvRunReal toIO
  , initMock = kvInitMock
  , newHandles = kvNewHandles
  , generator = kvGenerator
  , shrinker = kvShrinker
  , cleanup = kvCleanup
  }

type KVModel m = Model (MonadKVStateMachine m)

data instance Resp (MonadKVStateMachine m) h where
  KVSuccessHandle :: h -> Resp (MonadKVStateMachine m) h
  KVSuccessResult :: Int -> Resp (MonadKVStateMachine m) h
  KVError :: SimpleMonadKV m => Err m -> Resp (MonadKVStateMachine m) h

type KVResp m = Resp (MonadKVStateMachine m)

deriving stock instance Functor (KVResp m)
deriving stock instance Foldable (KVResp m)
deriving stock instance Traversable (KVResp m)
deriving stock instance (SimpleMonadKV m, Eq (Err m), Eq (Err m), Eq h) => Eq (KVResp m h)
deriving stock instance (SimpleMonadKV m, Show (Err m), Show (Err m), Show h) => Show (KVResp m h)


data instance Cmd (MonadKVStateMachine m) h where
  KVPrepare :: QueryScope Int -> Cmd (MonadKVStateMachine m) h
  KVSubmit :: h -> Fun (HashMap Int (Maybe Int)) (OperationResult Int Int, Int) -> Cmd (MonadKVStateMachine m) h -- k = v = Int, always returns Int
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
  { modelMap :: !(HashMap Int Int)
  , queries :: !(HashMap Int (QueryScope Int))
  , nextQueryId :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToExpr)

type instance MockState (MonadKVStateMachine m) = Mock
type KVState m = MockState (MonadKVStateMachine m)

mockAddQuery :: QueryScope Int -> Mock -> (KVMockHandle m, Mock)
mockAddQuery qs Mock{..} = (KVMockHandle nextQueryId, Mock
       { modelMap
       , queries = HashMap.insert nextQueryId qs queries
       , nextQueryId = nextQueryId + 1
       })

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


kvRunMock :: forall m. SimpleMonadKV m => KVCmd m (KVMockHandle m) -> KVState m -> (KVResp m (KVMockHandle m), KVState m)
kvRunMock cmd s = case cmd of
  KVPrepare qs -> case mockAddQuery qs s of
    (h, s') -> (KVSuccessHandle h, s')
  KVSubmit (KVMockHandle i) (Fn f) -> case HashMap.lookup i $ queries s of
    Nothing -> (KVError . KVBaseError $ KVEBadResultSet, s)
    Just qs -> let
        op_map = HashMap.mapWithKey (\k _ -> HashMap.lookup k (modelMap s)) (HashSet.toMap . coerce $ qs)
        apply_diff_item k = Endo . \case
                DIUpdate v -> HashMap.insert k v
                DIRemove -> HashMap.delete k
        (op_res, a) = f op_map
        in (KVSuccessResult a, s
         { modelMap = appEndo (HashMap.foldMapWithKey apply_diff_item op_res) $ modelMap s
         , queries = HashMap.delete i $ queries s
         })
  _ -> error "impossible"
kvRunReal :: SimpleMonadKV m => (forall a. m a -> IO a) -> KVCmd m (KVRealHandle m)-> IO (KVResp m (KVRealHandle m))
kvRunReal toIO = \case
  KVPrepare qs ->  toIO $ prepareOperation qs <&> KVSuccessHandle . KVRealHandle
  KVSubmit (KVRealHandle rs) (Fn f) -> toIO $ submitOperation rs f <&> either KVError KVSuccessResult
  _ -> error "impossible"


kvInitMock :: KVState m
kvInitMock = Mock
    { modelMap = mempty
    , queries = mempty
    , nextQueryId = 0
    }

kvNewHandles :: forall m. forall h. KVResp m h -> [h]
kvNewHandles = toList -- I think?

kvGenerator :: KVModel m Symbolic -> Maybe (Gen (KVCmd m :@ Symbolic))
kvGenerator Model{modelRefs} = Just $ do
  let prepare = At . KVPrepare <$> arbitrary
  let submit = do
        (ref, _mockhandle) <- elements modelRefs
        f <- arbitrary
        pure . At $ KVSubmit ref f


  oneof $ [prepare] ++ [submit | not . null $ modelRefs]


kvShrinker :: KVModel m Symbolic -> KVCmd m :@ Symbolic -> [KVCmd m :@ Symbolic]
kvShrinker _model (At cmd)= case cmd of
  KVPrepare qs -> At . KVPrepare <$> shrink qs
  KVSubmit ref f -> At . KVSubmit ref <$> shrink f

kvCleanup :: KVModel m Concrete -> IO ()
kvCleanup _ = pure ()
