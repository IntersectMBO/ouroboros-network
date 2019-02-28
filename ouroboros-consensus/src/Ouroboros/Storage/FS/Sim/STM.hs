{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | 'HasFS' instance using 'MockFS' stored in an STM variable
module Ouroboros.Storage.FS.Sim.STM (
      runSimFS
    , simHasFS
    ) where

import           Control.Monad.State
import           Data.Proxy

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Consensus.Util

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  The simulation-related types
------------------------------------------------------------------------------}

--- | Runs a computation provided an initial 'MockFS', producing a
--- result, the final state of the filesystem and a sequence of actions occurred
--- in the filesystem.
runSimFS :: MonadSTM m
         => ErrorHandling FsError m
         -> MockFS
         -> (HasFS m Mock.Handle -> m a)
         -> m (a, MockFS)
runSimFS err fs act = do
    var <- atomically (newTVar fs)
    a   <- act (simHasFS err var)
    fs' <- atomically (readTVar var)
    return (a, fs')

-- | Equip @m@ with a @HasFs@ instance using the mock file system
simHasFS :: forall m. MonadSTM m
         => ErrorHandling FsError m
         -> TVar m MockFS
         -> HasFS m Mock.Handle
simHasFS err var = HasFS {
      dumpState                = sim     Mock.dumpState
    , hOpen                    = sim  .: Mock.hOpen                    err'
    , hClose                   = sim  .  Mock.hClose                   err'
    , hSeek                    = sim ..: Mock.hSeek                    err'
    , hGet                     = sim  .: Mock.hGet                     err'
    , hPut                     = sim  .: Mock.hPut                     err'
    , hTruncate                = sim  .: Mock.hTruncate                err'
    , hGetSize                 = sim  .  Mock.hGetSize                 err'
    , createDirectory          = sim  .  Mock.createDirectory          err'
    , createDirectoryIfMissing = sim  .: Mock.createDirectoryIfMissing err'
    , listDirectory            = sim  .  Mock.listDirectory            err'
    , doesDirectoryExist       = sim  .  Mock.doesDirectoryExist       err'
    , doesFileExist            = sim  .  Mock.doesFileExist            err'
    , removeFile               = sim  .  Mock.removeFile               err'
    , hasFsErr                 = err
    }
  where
    sim :: StateT MockFS m a -> m a
    sim (StateT f) = do
        st       <- atomically $ readTVar var
        (a, st') <- f st
        atomically $ writeTVar var st'
        return a

    err' :: ErrorHandling FsError (StateT MockFS m)
    err' = EH.liftErrState (Proxy @MockFS) err
