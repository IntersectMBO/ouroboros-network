{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Util.FS.Sim.Pure (
    PureSimFS
    -- opaque
  , pureHasFS
  , runPureSimFS
  ) where

import           Control.Monad.Except
import           Control.Monad.State

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock

-- | Monad useful for running 'HasFS' in pure code
newtype PureSimFS a = PureSimFS (StateT MockFS (Except FsError) a)
  deriving (Functor, Applicative, Monad, MonadState MockFS, MonadError FsError)

runPureSimFS :: PureSimFS a -> MockFS -> Either FsError (a, MockFS)
runPureSimFS (PureSimFS act) !st = runExcept $ runStateT act st

pureHasFS :: HasFS PureSimFS Mock.HandleMock
pureHasFS = HasFS {
      dumpState                = Mock.dumpState
    , hOpen                    = Mock.hOpen
    , hClose                   = Mock.hClose
    , hIsOpen                  = Mock.hIsOpen
    , hSeek                    = Mock.hSeek
    , hGetSome                 = Mock.hGetSome
    , hGetSomeAt               = Mock.hGetSomeAt
    , hPutSome                 = Mock.hPutSome
    , hTruncate                = Mock.hTruncate
    , hGetSize                 = Mock.hGetSize
    , createDirectory          = Mock.createDirectory
    , createDirectoryIfMissing = Mock.createDirectoryIfMissing
    , listDirectory            = Mock.listDirectory
    , doesDirectoryExist       = Mock.doesDirectoryExist
    , doesFileExist            = Mock.doesFileExist
    , removeFile               = Mock.removeFile
    , renameFile               = Mock.renameFile
    , mkFsErrorPath            = fsToFsErrorPathUnmounted
    }
