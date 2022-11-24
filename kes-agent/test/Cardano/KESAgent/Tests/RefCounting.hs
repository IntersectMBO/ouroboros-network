module Cardano.KESAgent.Tests.RefCounting
( tests
)
where

import Cardano.KESAgent.RefCounting

import Test.Tasty
import Test.Tasty.QuickCheck
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)

tests :: TestTree
tests = testGroup "RefCounting"
  [ testProperty "counter is 1 on fresh CRef" $ p_counterIs1OnFresh
  , testProperty "counter is 0 after release" $ p_counterIs0AfterRelease
  , testProperty "withCRef keeps counter balanced" $ p_withCRefBalanced
  , testProperty "finalizer fires on only release" $ p_finalizerOnOnlyRelease
  , testProperty "finalizer fires once on concurrent use" $ p_finalizerOnConcurrentUse
  , testProperty "finalizer fires on last release" $ p_finalizerOnLastRelease
  , testProperty "finalizer does not fire when references remain" $ p_noFinalizerBeforeLastRelease
  , testProperty "throw on excessive release" $ p_throwOnExcessiveRelease
  , testProperty "throw on read after free" $ p_throwOnReadAfterFree
  , testProperty "throw on acquire after free" $ p_throwOnReacquire
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

p_finalizerOnOnlyRelease :: Property
p_finalizerOnOnlyRelease = ioProperty $ do
  firingsVar <- newMVar 0
  ref <- newCRef (\_ -> modifyMVar_ firingsVar (return . succ)) ()
  releaseCRef ref
  firings <- readMVar firingsVar
  return (firings === 1)

p_finalizerOnConcurrentUse :: Int -> Int -> Property
p_finalizerOnConcurrentUse d1 d2 =
  d1 < 10000 ==>
  d2 < 10000 ==>
  ioProperty $ do
    firingsVar <- newMVar 0
    ref <- newCRef (\_ -> modifyMVar_ firingsVar (return . succ)) ()
    concurrently_
        (threadDelay (abs d1 + 10) >> releaseCRef ref)
        (withCRef ref $ const (threadDelay (abs d2 + 10)))
    firings <- readMVar firingsVar
    return (firings === 1)

p_finalizerOnLastRelease :: Int -> Property
p_finalizerOnLastRelease n = ioProperty $ do
  firingsVar <- newMVar 0
  ref <- newCRef (\_ -> modifyMVar_ firingsVar (return . succ)) ()
  replicateM_ n (acquireCRef ref)
  replicateM_ n (releaseCRef ref)
  releaseCRef ref
  firings <- readMVar firingsVar
  return (firings === 1)

p_noFinalizerBeforeLastRelease :: Int -> Property
p_noFinalizerBeforeLastRelease n = ioProperty $ do
  firingsVar <- newMVar 0
  ref <- newCRef (\_ -> modifyMVar_ firingsVar (return . succ)) ()
  replicateM_ n (acquireCRef ref)
  replicateM_ n (releaseCRef ref)
  firings <- readMVar firingsVar
  releaseCRef ref
  return (firings === 0)

p_throwOnExcessiveRelease :: Int -> Property
p_throwOnExcessiveRelease n' = ioProperty $ do
  try go >>= \e -> return (e === Left ReferenceCountUnderflow)
  where
    n = abs n'
    go = do
      ref <- newCRef nullFinalizer ()
      replicateM_ n (acquireCRef ref)
      replicateM_ (n + 2) (releaseCRef ref)

p_throwOnReadAfterFree :: Property
p_throwOnReadAfterFree = ioProperty $ do
  try go >>= \e -> return (e === Left ReferenceCountUnderflow)
  where
    go = do
      ref <- newCRef nullFinalizer ()
      releaseCRef ref
      readCRef ref

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
