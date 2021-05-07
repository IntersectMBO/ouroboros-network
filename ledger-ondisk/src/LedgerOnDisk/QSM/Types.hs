-- |

module LedgerOnDisk.QSM.Types where

import GHC.Generics
import Data.Typeable
import Data.Coerce
import Data.Monoid
import Data.Kind
--import Data.Function
import Data.Functor

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.StateMachine.Lockstep.Simple
import Test.StateMachine
import Test.QuickCheck
import Data.TreeDiff.Expr

import LedgerOnDisk.Simple
import LedgerOnDisk.Class
import Data.Foldable

data MonadKVStateMachine (m :: Type -> Type)

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

newtype MockedQueryScope = MockedQueryScope { unMockedQueryScope :: QueryScope Int }
  deriving newtype (Semigroup, Monoid, Eq, Show)
  deriving ToExpr via (HashSet Int)

data Mock = Mock
  { modelMap :: !(HashMap Int Int)
  , queries :: !(HashMap Int MockedQueryScope)
  , nextQueryId :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToExpr)

type instance MockState (MonadKVStateMachine m) = Mock

type KVState m = MockState (MonadKVStateMachine m)

mockAddQuery :: QueryScope Int -> Mock -> (KVMockHandle m, Mock)
mockAddQuery qs Mock{..} = (KVMockHandle nextQueryId, Mock
       { modelMap
       , queries = HashMap.insert nextQueryId (coerce qs) queries
       , nextQueryId = nextQueryId + 1
       })

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
