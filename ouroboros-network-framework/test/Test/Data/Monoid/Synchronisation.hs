{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.Monoid.Synchronisation where

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM

import           Data.Monoid.Synchronisation

import           Control.Monad.IOSim

import           Test.QuickCheck
import           Test.Tasty.QuickCheck
import           Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup "Data.Monoid.Synchronisation"
  [ testProperty "last-to-finish (ST)" prop_lastToFinish_ST
  , testProperty "last-to-finish (IO)" prop_lastToFinish_IO
  ]

lastToFinishExperiment
    :: forall m.
       ( MonadFork  m
       , MonadSTM   m
       )
    => Bool -> m Bool
lastToFinishExperiment writeInSingleTransaction = do
    v  <- newEmptyTMVarIO
    v' <- newEmptyTMVarIO
    _ <- forkIO $ do

      if writeInSingleTransaction
        then atomically $ do
          putTMVar v  False
          putTMVar v' True
        else do
          atomically $ putTMVar v  False
          atomically $ putTMVar v' True
    -- one must anotate the type of 'LastToFinish' because 'STM' is
    -- a non-injective type family.
    atomically $ runLastToFinish $
         LastToFinish (readTMVar v)
      <> LastToFinish (readTMVar v')

prop_lastToFinish_ST :: Bool -> Bool
prop_lastToFinish_ST b =
    runSimOrThrow (lastToFinishExperiment b)

prop_lastToFinish_IO :: Bool -> Property
prop_lastToFinish_IO b =
    ioProperty (lastToFinishExperiment b)
