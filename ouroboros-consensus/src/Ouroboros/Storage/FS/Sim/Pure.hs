{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Storage.FS.Sim.Pure (
    PureSimFS -- opaque
  , runPureSimFS
  , pureHasFS
  , liftErrPureSimFS
  ) where

import           Control.Monad.State
import           Data.Proxy
import           Data.Type.Coercion

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

-- | Monad useful for running 'HasFS' in pure code
newtype PureSimFS m a = PureSimFS (StateT MockFS m a)
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadState MockFS (PureSimFS m) where
  get     = PureSimFS get
  put !st = PureSimFS $ put st

runPureSimFS :: PureSimFS m a -> MockFS -> m (a, MockFS)
runPureSimFS (PureSimFS act) !st = runStateT act st

pureHasFS :: forall m. Monad m
          => ErrorHandling FsError m -> HasFS (PureSimFS m) Mock.HandleMock
pureHasFS err = HasFS {
      dumpState                = Mock.dumpState
    , hOpen                    = Mock.hOpen                    err'
    , hClose                   = Mock.hClose                   err'
    , hSeek                    = Mock.hSeek                    err'
    , hGetSome                 = Mock.hGetSome                 err'
    , hPutSome                 = Mock.hPutSome                 err'
    , hTruncate                = Mock.hTruncate                err'
    , hGetSize                 = Mock.hGetSize                 err'
    , createDirectory          = Mock.createDirectory          err'
    , createDirectoryIfMissing = Mock.createDirectoryIfMissing err'
    , listDirectory            = Mock.listDirectory            err'
    , doesDirectoryExist       = Mock.doesDirectoryExist       err'
    , doesFileExist            = Mock.doesFileExist            err'
    , removeFile               = Mock.removeFile               err'
    , hasFsErr                 = err'
    }
  where
    err' :: ErrorHandling FsError (PureSimFS m)
    err' = liftErrPureSimFS err

liftErrPureSimFS :: ErrorHandling e m -> ErrorHandling e (PureSimFS m)
liftErrPureSimFS = EH.liftErrNewtype Coercion
                 . EH.liftErrState (Proxy @MockFS)
