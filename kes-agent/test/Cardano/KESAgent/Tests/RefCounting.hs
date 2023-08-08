module Cardano.KESAgent.Tests.RefCounting
  ( tests
  ) where

import Cardano.KESAgent.RefCounting

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( concurrently_ )
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Test.Tasty
import Test.Tasty.QuickCheck

{-# ANN module "HLint: ignore Use camelCase" #-}

tests :: TestTree
tests = testGroup "RefCounting"
  [ testProperty "counter is 1 on fresh CRef" p_counterIs1OnFresh
  , testProperty "counter is 0 after release" p_counterIs0AfterRelease
  , testProperty "withCRef keeps counter balanced" p_withCRefBalanced
  , testProperty "finalizer fires on only release" p_finalizerOnOnlyRelease
  , testProperty "finalizer fires once on concurrent use" p_finalizerOnConcurrentUse
  , testProperty "finalizer fires on last release" p_finalizerOnLastRelease
  , testProperty "finalizer does not fire when references remain" p_noFinalizerBeforeLastRelease
  , testProperty "throw on excessive release" p_throwOnExcessiveRelease
  , testProperty "throw on read after free" p_throwOnReadAfterFree
  , testProperty "throw on acquire after free" p_throwOnReacquire
  ]

p_counterIs1OnFresh :: Property
p_counterIs1OnFresh = ioProperty $ do
  ref <- newCRef nullFinalizer ()
  count <- getCRefCount ref
  releaseCRef ref
  return (count === 1)

p_counterIs0AfterRelease :: Property
p_counterIs0AfterRelease = ioProperty $ do
  ref <- newCRef nullFinalizer ()
  releaseCRef ref
  count <- getCRefCount ref
  return (count === 0)

-- | Assert that 'withCRef' is balanced, i.e., that the refcount is the same
-- before and after it runs.
p_withCRefBalanced :: Int -> Property
p_withCRefBalanced n = ioProperty $ do
  ref <- newCRef nullFinalizer ()
  replicateM_ n (acquireCRef ref)
  before <- getCRefCount ref
  withCRef ref doNothingWith
  after <- getCRefCount ref
  replicateM_ n (releaseCRef ref)
  releaseCRef ref
  return (before === after)

-- | Assert that when a CRef is created and then released, the finalizer fires.
p_finalizerOnOnlyRelease :: Property
p_finalizerOnOnlyRelease = ioProperty $ do
  firingsVar <- newMVar 0
  ref <- newCRef (\_ -> modifyMVar_ firingsVar (return . succ)) ()
  releaseCRef ref
  firings <- readMVar firingsVar
  return (firings === 1)

-- | Simulated usage pattern:
-- - Thread A creates a CRef
-- - A passes CRef to thread B
-- - B uses 'withCRef' to guard its own use of the CRef
-- - A and B use the CRef concurrently for a while
-- - A explicitly releases the CRef when it's done
-- - B automatically releases the CRef via 'withCRef'
-- As long as A does not release the CRef before B acquires it, the finalizer
-- should run exactly once, regardless of which of A and B releases it first,
-- and no refcount underflow exception should occur.
p_finalizerOnConcurrentUse :: Int -> Int -> Property
p_finalizerOnConcurrentUse d1 d2 =
  abs d1 < 10000 ==>
  abs d2 < 10000 ==>
  ioProperty $ do
    firingsVar <- newMVar 0
    ref <- newCRef (\_ -> modifyMVar_ firingsVar (return . succ)) ()
    concurrently_
        (threadDelay (abs d1 + 10) >> releaseCRef ref)
        (withCRef ref $ const (threadDelay (abs d2 + 10)))
    firings <- readMVar firingsVar
    return (firings === 1)

-- | Simulated usage pattern: a new CRef is created, acquired N times, and then
-- released N+1 times (once for each explicit acquire, and once for the initial
-- creation). Assert that the finalizer runs exactly once, and no underflow
-- occurs.
p_finalizerOnLastRelease :: Int -> Property
p_finalizerOnLastRelease n = ioProperty $ do
  firingsVar <- newMVar 0
  ref <- newCRef (\_ -> modifyMVar_ firingsVar (return . succ)) ()
  replicateM_ n (acquireCRef ref)
  replicateM_ n (releaseCRef ref)
  releaseCRef ref
  firings <- readMVar firingsVar
  return (firings === 1)

-- | Simulated usage pattern: a new CRef is created, acquired N times, and then
-- released N times. Assert that the finalizer does NOT run.
p_noFinalizerBeforeLastRelease :: Int -> Property
p_noFinalizerBeforeLastRelease n = ioProperty $ do
  firingsVar <- newMVar 0
  ref <- newCRef (\_ -> modifyMVar_ firingsVar (return . succ)) ()
  replicateM_ n (acquireCRef ref)
  replicateM_ n (releaseCRef ref)
  firings <- readMVar firingsVar
  releaseCRef ref
  return (firings === 0)

-- | Simulated usage pattern: a new CRef is created, acquired N times, and then
-- released N+2 times. (This is incorrect usage: the number of releases should
-- equal the number of acquisitions.) Assert that this throws an underflow
-- exception.
p_throwOnExcessiveRelease :: Int -> Property
p_throwOnExcessiveRelease n' = ioProperty $ do
  try go >>= \e -> return (e === Left ReferenceCountUnderflow)
  where
    n = abs n'
    go = do
      ref <- newCRef nullFinalizer ()
      replicateM_ n (acquireCRef ref)
      replicateM_ (n + 2) (releaseCRef ref)

-- | Assert that attempting to read a CRef after its reference count has
-- reached zero throws an underflow exception.
p_throwOnReadAfterFree :: Property
p_throwOnReadAfterFree = ioProperty $ do
  try go >>= \e -> return (e === Left ReferenceCountUnderflow)
  where
    go = do
      ref <- newCRef nullFinalizer ()
      releaseCRef ref
      readCRef ref

-- | Assert that attempting to acquire a CRef after its reference count has
-- reached zero throws an underflow exception.
p_throwOnReacquire :: Property
p_throwOnReacquire = ioProperty $ do
  try go >>= \e -> return (e === Left ReferenceCountUnderflow)
  where
    go = do
      ref <- newCRef nullFinalizer ()
      releaseCRef ref
      withCRef ref doNothingWith

nullFinalizer :: a -> IO ()
nullFinalizer _ = return ()

doNothingWith :: a -> IO ()
doNothingWith _ = return ()
