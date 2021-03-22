{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus.Node (
    tests
  ) where

import           Data.Bifunctor (second)
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Time.Clock (secondsToDiffTime)
import           System.Directory (getTemporaryDirectory)
import           System.IO.Temp (withTempDirectory)

import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Node.DbLock
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Util.FileLock (FileLock, ioFileLock)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API.Types

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Test.Util.FS.Sim.FsTree (FsTree (..))
import           Test.Util.FS.Sim.MockFS (Files)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (runSimFS)
import           Test.Util.FileLock
import           Test.Util.QuickCheck (ge)

tests :: TestTree
tests = testGroup "Node"
    [ testGroup "checkDbMarker"
      [ testCase "match"        test_checkNetworkMagic_match
      , testCase "mismatch"     test_checkNetworkMagic_mismatch
      , testCase "empty folder" test_checkNetworkMagic_empty_folder
      , testCase "missing"      test_checkNetworkMagic_missing
      , testCase "corrupt"      test_checkNetworkMagic_corrupt
      , testCase "empty"        test_checkNetworkMagic_empty
      ]
    , testGroup "lockDb"
      [ testProperty "reacquire a released lock"   prop_reacquire_lock
      , testCase     "acquire a held lock"         test_acquire_held_lock
      , testProperty "wait to acquire a held lock" prop_wait_to_acquire_lock
      ]
    ]

{-------------------------------------------------------------------------------
  checkDbMarker
-------------------------------------------------------------------------------}

expectedNetworkMagic :: NetworkMagic
expectedNetworkMagic = NetworkMagic 1910

mountPoint :: MountPoint
mountPoint = MountPoint "root"

fullPath :: FilePath
fullPath = fsToFilePath
    mountPoint (fsPathFromList [dbMarkerFile])

runCheck :: Files -> (Either DbMarkerError (), Files)
runCheck files = runSimOrThrow $ do
    fmap (second Mock.mockFiles) $
      runSimFS Mock.empty { Mock.mockFiles = files } $ \hasFS ->
        checkDbMarker hasFS mountPoint expectedNetworkMagic

test_checkNetworkMagic_match :: Assertion
test_checkNetworkMagic_match = res @?= Right ()
  where
    fs = Folder $ Map.fromList
      [ (dbMarkerFile, File $ dbMarkerContents expectedNetworkMagic)
      , ("immutable",  Folder mempty)
      , ("ledger",     Folder mempty)
      , ("volatile",   Folder mempty)
      ]
    (res, _) = runCheck fs

test_checkNetworkMagic_mismatch :: Assertion
test_checkNetworkMagic_mismatch = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ (dbMarkerFile, File $ dbMarkerContents actual)
      , ("immutable",  Folder mempty)
      , ("ledger",     Folder mempty)
      , ("volatile",   Folder mempty)
      ]
    (res, _) = runCheck fs
    actual = NetworkMagic 10
    e = NetworkMagicMismatch
      fullPath
      actual
      expectedNetworkMagic

test_checkNetworkMagic_empty_folder :: Assertion
test_checkNetworkMagic_empty_folder = do
    res @?= Right ()
    fs' @?= expectedFs'
  where
    fs = Folder mempty
    (res, fs') = runCheck fs
    expectedFs' = Folder $ Map.fromList
      [ (dbMarkerFile, File $ dbMarkerContents expectedNetworkMagic) ]

test_checkNetworkMagic_missing :: Assertion
test_checkNetworkMagic_missing = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ ("passwords.txt", File "qwerty\n123456\n")
      ]
    (res, _) = runCheck fs
    e = NoDbMarkerAndNotEmpty fullPath

test_checkNetworkMagic_corrupt :: Assertion
test_checkNetworkMagic_corrupt = res @?= Left e
  where
    fs = Folder $ Map.fromList
      [ (dbMarkerFile, File "garbage")
      , ("immutable",  Folder mempty)
      , ("ledger",     Folder mempty)
      , ("volatile",   Folder mempty)
      ]
    (res, _) = runCheck fs
    e = CorruptDbMarker fullPath

test_checkNetworkMagic_empty :: Assertion
test_checkNetworkMagic_empty = res @?= Left e
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

-- | We use the mock lock to test whether we can release and reacquire the
-- lock, because the real lock might release lazily, causing the reacquisition
-- to fail.
prop_reacquire_lock :: ReleaseDelay -> Property
prop_reacquire_lock (ReleaseDelay releaseDelay) =
    runSimOrThrow $ do
      fileLock <- mockFileLock (Just releaseDelay)
      -- Lock and unlock it
      touchLock fileLock

      -- Lock and unlock it again, which might fail:
      tryL (touchLock fileLock) <&> \case
        -- If we failed to obtain the lock, it must be because the release
        -- delay we simulate is greater than or equal to the timeout
        Left  _  -> label "timed out" $ releaseDelay `ge` timeout
        Right () -> property True
  where
    timeout = secondsToDiffTime 2

    touchLock :: (IOLike m, MonadTimer m) => FileLock m -> m ()
    touchLock fileLock =
      withLockDB_
        fileLock
        mountPoint
        dbLockFsPath
        timeout
        (return ())

-- | Test with a real lock that while holding the lock, we cannot reacquire
-- it.
test_acquire_held_lock :: Assertion
test_acquire_held_lock = withTempDir $ \dbPath -> do
    let dbMountPoint = MountPoint dbPath

    -- While holding the lock, try to acquire it again, which should fail
    res <-
      tryL $ withLock dbMountPoint (secondsToDiffTime 0) $
               tryL $ withLock dbMountPoint (millisecondsToDiffTime 10) $
                        return ()

    -- The outer 'Right' means that the first call to 'withLock'
    -- succeeded, the inner 'Left' means that the second call to
    -- 'touchLock' failed.
    res @?= (Left (DbLocked (fsToFilePath dbMountPoint dbLockFsPath)))
  where
    withTempDir :: (FilePath -> IO a) -> IO a
    withTempDir k = do
      sysTmpDir <- getTemporaryDirectory
      withTempDirectory sysTmpDir "ouroboros-network-test" k

    withLock :: MountPoint -> DiffTime -> IO a -> IO a
    withLock dbMountPoint lockTimeout =
      withLockDB_
        ioFileLock
        dbMountPoint
        dbLockFsPath
        lockTimeout

tryL :: MonadCatch m => m a -> m (Either DbLocked a)
tryL = try

-- | Test that we can acquire and already held lock by waiting for it.
--
-- Property:
--   A maximum delay of MAX can cope with any hold up of ACTUAL < MAX.
--
--   Note that we exclude ACTUAL == MAX, as it is \"racy\".
--
prop_wait_to_acquire_lock :: ActualAndMaxDelay -> Property
prop_wait_to_acquire_lock ActualAndMaxDelay { actualDelay, maxDelay } =
    runSimOrThrow $ do
      -- We don't simulate delayed releases because the test depends on
      -- precise timing.
      fileLock <- mockFileLock Nothing

      -- Hold the lock for 'actualDelay' and then signal we have released it
      let bgThread =
            -- The lock will not be held, so just use the default parameters
            -- to acquire it
            withLock fileLock dbLockTimeout $
              -- Hold the lock for ACTUAL
              threadDelay actualDelay

      withAsync bgThread $ \asyncBgThread -> do
        link asyncBgThread
        -- Try to obtain the held lock, waiting MAX for it
        --
        -- The test will fail when an exception is thrown below because it
        -- timed out while waiting on the lock.
        withLock fileLock maxDelay $
          return $ property True
  where
    withLock
      :: (IOLike m, MonadTimer m)
      => FileLock m
      -> DiffTime
      -> m a
      -> m a
    withLock fileLock timeout =
      withLockDB_
        fileLock
        mountPoint
        dbLockFsPath
        timeout

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | Simulate lazy releasing of the lock, as done by Linux and Windows.
newtype ReleaseDelay = ReleaseDelay DiffTime
  deriving (Eq, Show)

instance Arbitrary ReleaseDelay where
  arbitrary =
    ReleaseDelay . millisecondsToDiffTime <$> choose (0, 5000)
  shrink (ReleaseDelay t) =
    [ReleaseDelay (fromRational t') | t' <- shrink (toRational t)]

-- | Invariant: @actualDelay < maxDelay@
data ActualAndMaxDelay = ActualAndMaxDelay {
      actualDelay :: DiffTime
    , maxDelay    :: DiffTime
    }
  deriving (Eq, Show)

instance Arbitrary ActualAndMaxDelay where
    arbitrary = do
        maxDelayMs    <- choose (1, 2000)
        actualDelayMs <- choose (0, maxDelayMs - 1)
        return ActualAndMaxDelay {
            actualDelay = millisecondsToDiffTime actualDelayMs
          , maxDelay    = millisecondsToDiffTime maxDelayMs
          }

    shrink (ActualAndMaxDelay actualDelay maxDelay) =
      [ ActualAndMaxDelay actualDelay' maxDelay
      | actualDelay' <- fromRational <$> shrink (toRational actualDelay)
      ] <>
      [ ActualAndMaxDelay actualDelay maxDelay
      | maxDelay' <- fromRational <$> shrink (toRational maxDelay)
      , actualDelay < maxDelay'
      ]

millisecondsToDiffTime :: Integer -> DiffTime
millisecondsToDiffTime = (/ 1000) . secondsToDiffTime
