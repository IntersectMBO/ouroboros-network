{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | 'HasFS' instance using 'MockFS' stored in an STM variable
module Test.Util.FS.Sim.STM (
    runSimFS
  , simHasFS
  ) where

import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Test.Util.FS.Sim.MockFS (HandleMock, MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.Pure (PureSimFS, runPureSimFS)

{------------------------------------------------------------------------------
  The simulation-related types
------------------------------------------------------------------------------}

--- | Runs a computation provided an initial 'MockFS', producing a
--- result, the final state of the filesystem and a sequence of actions occurred
--- in the filesystem.
runSimFS :: (MonadSTM m, MonadThrow m)
         => MockFS
         -> (HasFS m HandleMock -> m a)
         -> m (a, MockFS)
runSimFS fs act = do
    var <- newTVarIO fs
    a   <- act (simHasFS var)
    fs' <- atomically (readTVar var)
    return (a, fs')

-- | Equip @m@ with a @HasFs@ instance using the mock file system
simHasFS :: forall m. (MonadSTM m, MonadThrow m)
         => StrictTVar m MockFS
         -> HasFS m HandleMock
simHasFS var = HasFS {
      dumpState                = sim     Mock.dumpState
    , hOpen                    = sim  .: Mock.hOpen
    , hClose                   = sim  .  Mock.hClose
    , hIsOpen                  = sim  .  Mock.hIsOpen
    , hSeek                    = sim ..: Mock.hSeek
    , hGetSome                 = sim  .: Mock.hGetSome
    , hGetSomeAt               = sim ..: Mock.hGetSomeAt
    , hPutSome                 = sim  .: Mock.hPutSome
    , hTruncate                = sim  .: Mock.hTruncate
    , hGetSize                 = sim  .  Mock.hGetSize
    , createDirectory          = sim  .  Mock.createDirectory
    , createDirectoryIfMissing = sim  .: Mock.createDirectoryIfMissing
    , listDirectory            = sim  .  Mock.listDirectory
    , doesDirectoryExist       = sim  .  Mock.doesDirectoryExist
    , doesFileExist            = sim  .  Mock.doesFileExist
    , removeFile               = sim  .  Mock.removeFile
    , renameFile               = sim  .: Mock.renameFile
    , mkFsErrorPath            = fsToFsErrorPathUnmounted
    }
  where
    sim :: PureSimFS a -> m a
    sim m = do
      eOrA <- atomically $ do
        st <- readTVar var
        case runPureSimFS m st of
          Left e -> return $ Left e
          Right (a, st') -> do
            writeTVar var st'
            return $ Right a
      either throwIO return eOrA
