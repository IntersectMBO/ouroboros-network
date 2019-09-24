module Test.Event where

import           Control.Concurrent
import           Control.Exception
import           Data.Functor (void)

import           System.Win32.Event

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "Event"
  [ testCase "interruptible waitSingleObject" test_interruptible_waitSingleObject
  ]

test_interruptible_waitSingleObject :: IO ()
test_interruptible_waitSingleObject = do
    v <- newEmptyMVar
    eventHandle <- createEvent Nothing False False "eternityEvent"
    tid <- mask $ \unmask -> forkIO $
          (unmask $ void $ interruptibleWaitForSingleObject eventHandle maxBound)
        `finally`
          (putMVar v ())
    threadDelay 100
    killThread tid
    -- Q: this is needed for `tryTakeMVar` to return 'Just ()', otherwise it
    -- returns 'Nothing'. This is surprising! With out both 'threadDelay' (the
    -- above, and the one that just follows), the test rightfully passes.
    threadDelay 1
    res <- tryTakeMVar v
    assertEqual "interruptibleWaitSingleObject" (Just ()) res
