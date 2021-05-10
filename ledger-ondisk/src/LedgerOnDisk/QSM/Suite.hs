{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
module LedgerOnDisk.QSM.Suite where

import           Test.Tasty
import           Test.Tasty.QuickCheck
-- import qualified LedgerOnDisk.QSM.Model
import           LedgerOnDisk.Simple
import Test.StateMachine.Lockstep.Simple
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Coerce
import LedgerOnDisk.Class
import Data.Functor.Identity
import LedgerOnDisk.QSM.Model
-- import Test.StateMachine (Symbolic)
import Data.Functor ((<&>))
import Data.Monoid
-- import Data.Foldable
import Data.Semigroup (Max(Max), getMax)
-- import Data.Maybe
import Control.Monad.State ()
import Control.Monad.State.Strict
-- import Test.QuickCheck.Property
import Data.HashMap.Strict (HashMap)
import Data.Function
import Data.HashSet (HashSet)
import Data.Hashable
import System.IO.Unsafe
import GHC.Base

type MockKVState = KVState Identity -- in the mock case Identity could be anything

prop_stateMachine_SimpleT :: SimpleMap -> Property
prop_stateMachine_SimpleT initial_map = let
  !initial_state = initialState initial_map
  !ref = noinline . unsafePerformIO . newIORef $ initial_state
  smt0 = LedgerOnDisk.QSM.Model.stateMachineTest initial_map  $ \x -> runSimpleTWithIORef x ref
  smt = smt0
    { cleanup = \x -> cleanup smt0 x *> writeIORef ref initial_state
    }
  in prop_sequential smt Nothing

newtype MockM a = MockM { unMockM :: StateT (KVState Identity) (Either String) a }
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance (s ~ MockKVState) => MonadState s MockM

instance MonadFail MockM where
  fail = MockM . lift . Left

mockCmd :: MonadState (KVState n) m => KVCmd n (KVMockHandle n) -> m (KVResp n (KVMockHandle n))
mockCmd cmd = state $ \s -> kvRunMock cmd s

arbSubkeys :: (Eq k, Hashable k) => HashMap k v -> Gen (HashSet k)
arbSubkeys = fmap HashSet.fromList . sublistOf . HashMap.keys

runMockM :: Testable prop => SimpleMap -> MockM (SimpleMap -> prop) -> Property
runMockM s (MockM m) =  runStateT m (nonemptyMock s) & \case
  Left e -> counterexample e False
  Right (f, Mock{modelMap}) -> property $ f modelMap

prop_model_sees_restricted_map :: SimpleMap -> Property
prop_model_sees_restricted_map initial_map = property $ do
  arbSubkeys initial_map <&> \keys -> runMockM initial_map $ do
    KVSuccessHandle h <- mockCmd . KVPrepare . coerce $ keys
    KVSuccessResult r <- mockCmd $ KVSubmit h $ OFSet "measure_provided_map" $ (mempty,) . length
    pure $ \_final_map ->
      let query_provided_correct_size_map = r === length keys
      in query_provided_correct_size_map

prop_model_can_lookup :: SimpleMap -> Property
prop_model_can_lookup initial_map = property $
  arbSubkeys initial_map <&> \keys -> runMockM initial_map $ do
    KVSuccessHandle h <- mockCmd . KVPrepare . coerce $ keys
    KVSuccessLookupAll_ observed_map <- mockCmd $ KVLookupAll_ h
    pure $ \final_map ->
      let saw_everything = observed_map === HashMap.filterWithKey (\k _ -> k `HashSet.member` keys) initial_map
          nothing_added = final_map === initial_map
      in saw_everything .&&. nothing_added

prop_model_can_delete :: SimpleMap -> Property
prop_model_can_delete initial_map = property $ do
  arbSubkeys initial_map <&> \keys -> runMockM initial_map $ do
    KVSuccessHandle h <- mockCmd . KVPrepare . coerce $ keys
    KVSuccessResult _ <- mockCmd . KVSubmit h $ OFSet "deleteAll" $ \m -> (HashMap.map (const DIRemove) m, length m)
    pure $ \final_map -> let
      correct_length = length final_map + length keys === length initial_map
      deleted_are_absent = getAll . foldMap (\k -> All . not $ k `HashMap.member` final_map ) $ keys
      in correct_length .&&. deleted_are_absent

prop_model_can_update :: SimpleMap -> Property
prop_model_can_update initial_map = property $ do
  v <- arbitrary
  arbSubkeys initial_map <&> \keys -> runMockM initial_map $ do
    KVSuccessHandle h <- mockCmd . KVPrepare . coerce $ keys
    KVSuccessResult _ <- mockCmd . KVSubmit h $ OFSet "updateAll" $ \m -> (HashMap.map (const $ DIUpdate v) m, length m)
    pure $ \final_map -> let
      no_change_in_keys = HashMap.keys initial_map === HashMap.keys final_map
      updates_happened = getAll . foldMap (\k -> All $ HashMap.lookup k final_map == Just v) $ keys
      in no_change_in_keys .&&. updates_happened

prop_model_can_insert :: SimpleMap -> Property
prop_model_can_insert initial_map = property $ do
  -- lb is the greatest key + 1, lower bound for what we'll insert
  let lb = maybe 0 (+1) . fmap getMax . foldMap (Just . Max) . HashMap.keys  $ initial_map
  keys <- listOf1 arbitrary <&> HashSet.fromList . fmap ((+lb) . getNonNegative)
  pure . runMockM initial_map $ do
    KVSuccessHandle h <- mockCmd . KVPrepare . coerce $ keys
    KVSuccessResult _ <- mockCmd . KVSubmit h $ OFSet "insertAll" $ \m -> (HashMap.mapWithKey (\k _ -> DIUpdate k) m, length m)
    pure $ \final_map -> let
      correct_length = length final_map === length initial_map + length keys
      -- new_keys_apart must be true if correct_length is, should it be removed?
      new_keys_apart = keys `HashSet.intersection` (HashMap.keysSet initial_map) === HashSet.empty
      correct_values = getAll . foldMap (\k -> All $ HashMap.lookup k final_map == Just k) $ keys
      in correct_length .&&. new_keys_apart .&&. correct_values

prop_model_cmd_generator_valid :: KVState Identity -> Property
prop_model_cmd_generator_valid m = property $ do
  let model = Model m []
  sequenceA (kvGenerator model) <&> \case
    Nothing -> False
    Just (At x) -> case x of
      KVLookupAll_ {} -> False
      KVSubmit _ OFSet {} -> False
      _ -> True

prop_model_disallows_reuse_of_resultsets :: SimpleMap -> SimpleOperationFunction -> Property
prop_model_disallows_reuse_of_resultsets initial_map op@(OFn f) = runMockM initial_map $ do
  KVSuccessHandle h <- mockCmd . KVPrepare $ mempty
  KVSuccessResult r <- mockCmd . KVSubmit h $ op
  KVBaseError BEBadResultSet <-  mockCmd . KVSubmit h $ op
  pure $ \_ -> r === (snd . f $ mempty)



tests :: TestTree
tests = testGroup "quickcheck state machine"
  [ testGroup "model"
    [ testProperty "model can lookup" prop_model_can_lookup
    , testProperty "model can delete" prop_model_can_delete
    , testProperty "model can update" prop_model_can_update
    , testProperty "model can insert" prop_model_can_insert
    , testProperty "model disallows reuse of resultsets" prop_model_disallows_reuse_of_resultsets
    , testProperty "prop_model_sees_restricted_map" prop_model_sees_restricted_map
    , testProperty "generate no LookupAll_ s" prop_model_cmd_generator_valid
    ]
  , testProperty "SimpleT" prop_stateMachine_SimpleT
  ]
