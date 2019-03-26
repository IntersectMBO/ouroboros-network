{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Ouroboros.Storage.FS
  ( tests
  ) where


import qualified System.IO as IO

import qualified Test.Ouroboros.Storage.FS.StateMachine as StateMachine
import           Test.Ouroboros.Storage.Util
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit

import           Ouroboros.Storage.FS.API
import qualified Ouroboros.Storage.FS.API.Example as FS.API.Example
import           Ouroboros.Storage.FS.API.Types

tests :: FilePath -> TestTree
tests tmpDir = testGroup "HasFS"
    [ StateMachine.tests tmpDir
      -- hOpen
    , testCase     "hOpen ReadOnly and hPut"     test_hOpenWriteInvalid
      -- example
    , testCase     "example equivalence" test_example
    ]

{------------------------------------------------------------------------------
 The tests proper
-------------------------------------------------------------------------------}

test_hOpenWriteInvalid :: Assertion
test_hOpenWriteInvalid = apiEquivalenceFs (expectFsError FsInvalidArgument) $ \HasFS{..} _err -> do
    _  <- hOpen ["foo.txt"] IO.WriteMode
    h2 <- hOpen ["foo.txt"] IO.ReadMode
    hPut h2 "haskell-is-nice"

test_example :: Assertion
test_example = apiEquivalenceFs (expectFsResult (@?= ["test", "block"])) $ \hasFS@HasFS{..} _err -> do
    -- The example assumes the presence of some files and dirs.
    createDirectoryIfMissing True ["var", "tmp"]
    withFile hasFS ["var", "tmp", "foo.txt"] IO.WriteMode $ \_ -> return ()
    FS.API.Example.example hasFS
