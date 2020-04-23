{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Consensus.Node (tests) where

import           Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           System.Directory (getTemporaryDirectory)
import           System.FilePath ((</>))
import           System.IO.Temp (withTempDirectory)

import           Control.Monad.Class.MonadThrow (Exception, throwM, try)
import           Control.Monad.IOSim (runSimOrThrow)

import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Consensus.Node.DbLock
import           Ouroboros.Consensus.Node.DbMarker

import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)

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
    , testCase "lockDb"         test_lockDb
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
      runSimFS Mock.empty { Mock.mockFiles = files } $ \hasFS ->
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
  lockDb
-------------------------------------------------------------------------------}

test_lockDb :: Assertion
test_lockDb = withTempDir $ \dbPath -> do
    let hasFS     = ioHasFS $ MountPoint dbPath
        withLock  = withLockDB hasFS dbPath
        touchLock = withLock $ return ()

    -- Lock and unlock it
    tryL touchLock >>=
        (@?= (Right ()))

    -- Raise an exception. The lock should get released.
    _ <- tryT (withLock $ throwM TestException)
    -- Test that the lock was released by acquiring it again
    tryL touchLock >>=
        (@?= (Right ()))

    -- While holding the lock, try to acquire it again, which should fail
    tryL (withLock $ tryL touchLock) >>=
        -- The outer 'Right' means that the first call to 'withLock'
        -- succeeded, the inner 'Left' means that the second call to
        -- 'touchLock' failed.
        (@?= Right (Left (DbLocked (dbPath </> T.unpack dbLockFile))))

  where
    withTempDir k = do
      sysTmpDir <- getTemporaryDirectory
      withTempDirectory sysTmpDir "ouroboros-network-test" k

    tryL :: IO a -> IO (Either DbLocked a)
    tryL = try

    tryT :: IO a -> IO (Either TestException a)
    tryT = try

data TestException = TestException deriving (Show, Eq, Exception)
