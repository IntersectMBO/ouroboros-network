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
import           Ouroboros.Storage.FS.API.Types

tests :: FilePath -> TestTree
tests tmpDir = testGroup "HasFS"
    [ StateMachine.tests tmpDir
      -- hOpen
    , testCase "hOpen ReadOnly and hPut" test_hOpenWriteInvalid
    ]

{------------------------------------------------------------------------------
 The tests proper
-------------------------------------------------------------------------------}

test_hOpenWriteInvalid :: Assertion
test_hOpenWriteInvalid = apiEquivalenceFs (expectFsError FsInvalidArgument) $ \hasFS@HasFS{..} _err -> do
    _  <- hOpen (mkFsPath ["foo.txt"]) (WriteMode MustBeNew)
    h2 <- hOpen (mkFsPath ["foo.txt"]) ReadMode
    hPut hasFS h2 "haskell-is-nice"
