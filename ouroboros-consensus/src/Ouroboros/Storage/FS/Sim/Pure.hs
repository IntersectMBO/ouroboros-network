{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Storage.FS.Sim.Pure (
    PureSimFS -- opaque
  , PureSimFSE
  , runPureSimFS
  , runPureSimFSE
  ) where

import           Control.Monad.Except
import           Control.Monad.State

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.FS.Class.Types
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock

-- | Monad useful for running 'HasFS' in pure code
newtype PureSimFS a = PureSimFS { unPureSimFS :: State MockFS a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState MockFS
           )

type PureSimFSE = ExceptT FsError PureSimFS

runPureSimFS :: PureSimFS a -> MockFS -> (a, MockFS)
runPureSimFS = runState . unPureSimFS

runPureSimFSE :: PureSimFSE a -> MockFS -> (Either FsError a, MockFS)
runPureSimFSE = runPureSimFS . runExceptT

instance HasFS PureSimFSE where
  type FsHandle PureSimFSE = Mock.Handle
  data Buffer   PureSimFSE = MockBufferUnused

  dumpState                = Mock.dumpState
  newBuffer                = \_ -> return MockBufferUnused
  hOpen                    = Mock.hOpen
  hClose                   = Mock.hClose
  hSeek                    = Mock.hSeek
  hGet                     = Mock.hGet
  hPut                     = Mock.hPut
  hPutBuffer               = Mock.hPutBuffer
  hTruncate                = Mock.hTruncate
  hGetSize                 = Mock.hGetSize
  createDirectory          = Mock.createDirectory
  createDirectoryIfMissing = Mock.createDirectoryIfMissing
  listDirectory            = Mock.listDirectory
  doesDirectoryExist       = Mock.doesDirectoryExist
  doesFileExist            = Mock.doesFileExist
