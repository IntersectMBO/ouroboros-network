{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.FS.Sim.Pure (
    PureSimFS
    -- opaque
  , pureHasFS
  , runPureSimFS
  ) where

import           Control.Monad.Except
import           Control.Monad.State

import           System.FS.API
import           System.FS.API.Types

import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.MockFS (MockFS)

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
