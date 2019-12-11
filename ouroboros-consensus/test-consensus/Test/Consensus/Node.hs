{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Consensus.Node (tests) where

import           Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           System.Directory (getTemporaryDirectory)
import           System.FilePath ((</>))
import           System.IO.Temp (withTempDirectory)

import           Control.Monad.Class.MonadThrow (try)
import           Control.Monad.IOSim (runSimOrThrow)

import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Util.ResourceRegistry (withRegistry)

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.IO (ioHasFS)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Util.FS.Sim.FsTree (FsTree (..))
import           Test.Util.FS.Sim.MockFS (Files)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (runSimFS)

tests :: TestTree
tests = testGroup "Node"
    [ testGroup "checkDbMarker"
      [ testCase "match"        test_checkProtocolMagicId_match
      , testCase "mismatch"     test_checkProtocolMagicId_mismatch
      , testCase "empty folder" test_checkProtocolMagicId_empty_folder
      , testCase "missing"      test_checkProtocolMagicId_missing
      , testCase "corrupt"      test_checkProtocolMagicId_corrupt
      , testCase "empty"        test_checkProtocolMagicId_empty
      ]
    , testCase "lockDbMarkerFile" test_lockDbMarkerFile
    ]

{-------------------------------------------------------------------------------
  checkDbMarker
-------------------------------------------------------------------------------}

expectedProtocolMagicId :: ProtocolMagicId
expectedProtocolMagicId = ProtocolMagicId 1910

mountPoint :: MountPoint
mountPoint = MountPoint "root"

fullPath :: FilePath
fullPath = fsToFilePath
    mountPoint (fsPathFromList [dbMarkerFile])

runCheck :: Files -> (Either DbMarkerError (), Files)
runCheck files = runSimOrThrow $ do
    fmap (second Mock.mockFiles) $
      runSimFS EH.monadCatch Mock.empty { Mock.mockFiles = files } $ \hasFS ->
        checkDbMarker hasFS mountPoint expectedProtocolMagicId

test_checkProtocolMagicId_match :: Assertion
test_checkProtocolMagicId_match = res @?= Right ()
  where
    fs = Folder $ Map.fromList
      [ (dbMarkerFile, File $ dbMarkerContents expectedProtocolMagicId)
      , ("immutable",  Folder mempty)
      , ("ledger",     Folder mempty)
      , ("volatile",   Folder mempty)
      ]
    (res, _) = runCheck fs

test_checkProtocolMagicId_mismatch :: Assertion
test_checkProtocolMagicId_mismatch = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ (dbMarkerFile, File $ dbMarkerContents actual)
      , ("immutable",  Folder mempty)
      , ("ledger",     Folder mempty)
      , ("volatile",   Folder mempty)
      ]
    (res, _) = runCheck fs
    actual = ProtocolMagicId 10
    e = ProtocolMagicIdMismatch
      fullPath
      actual
      expectedProtocolMagicId

test_checkProtocolMagicId_empty_folder :: Assertion
test_checkProtocolMagicId_empty_folder = do
    res @?= Right ()
    fs' @?= expectedFs'
  where
    fs = Folder mempty
    (res, fs') = runCheck fs
    expectedFs' = Folder $ Map.fromList
      [ (dbMarkerFile, File $ dbMarkerContents expectedProtocolMagicId) ]

test_checkProtocolMagicId_missing :: Assertion
test_checkProtocolMagicId_missing = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ ("passwords.txt", File "qwerty\n123456\n")
      ]
    (res, _) = runCheck fs
    e = NoDbMarkerAndNotEmpty fullPath

test_checkProtocolMagicId_corrupt :: Assertion
test_checkProtocolMagicId_corrupt = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ (dbMarkerFile, File "garbage")
      , ("immutable",  Folder mempty)
      , ("ledger",     Folder mempty)
      , ("volatile",   Folder mempty)
      ]
    (res, _) = runCheck fs
    e = CorruptDbMarker fullPath

test_checkProtocolMagicId_empty :: Assertion
test_checkProtocolMagicId_empty = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ (dbMarkerFile, File "")
      , ("immutable",  Folder mempty)
      , ("ledger",     Folder mempty)
      , ("volatile",   Folder mempty)
      ]
    (res, _) = runCheck fs
    e = CorruptDbMarker fullPath

{-------------------------------------------------------------------------------
  lockDbMarkerFile
-------------------------------------------------------------------------------}

test_lockDbMarkerFile :: Assertion
test_lockDbMarkerFile = withTempDir $ \dbPath -> do
    let mount = MountPoint dbPath
    -- Create the DB marker file
    checkDbMarker (ioHasFS mount) mountPoint expectedProtocolMagicId >>=
      (@?= Right ())

    -- Lock it once
    withRegistry $ \registry -> do
      tryL (lockDbMarkerFile registry dbPath) >>=
        (@?= (Right ()))

      -- Try to lock it again. This should fail.
      tryL (lockDbMarkerFile registry dbPath) >>=
        (@?= (Left (DbLocked (dbPath </> T.unpack dbMarkerFile))))

    -- Unlock it now by closing the 'ResourceRegistry'.
    -- We must be able to lock it again now.
    withRegistry $ \registry ->
      tryL (lockDbMarkerFile registry dbPath) >>=
        (@?= (Right ()))
    -- And finally unlock it.
  where
    withTempDir k = do
      sysTmpDir <- getTemporaryDirectory
      withTempDirectory sysTmpDir "ouroboros-network-test" k

    tryL :: IO a -> IO (Either DbMarkerError a)
    tryL = try
