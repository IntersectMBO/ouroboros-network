{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Ouroboros.Storage.FS
  ( tests
  ) where

import qualified Test.Ouroboros.Storage.FS.StateMachine as StateMachine
import           Test.Ouroboros.Storage.Util
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit

import           Ouroboros.Storage.FS.API

tests :: FilePath -> TestTree
tests tmpDir = testGroup "HasFS"
    [ StateMachine.tests tmpDir
    ]

{------------------------------------------------------------------------------
 The tests proper
-------------------------------------------------------------------------------}

-- | A unit test example.
_test_example :: Assertion
_test_example =
  apiEquivalenceFs (expectFsResult (@?= ())) $ \HasFS{..} _err ->
    return ()
