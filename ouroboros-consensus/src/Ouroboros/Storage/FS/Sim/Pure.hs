{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

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
newtype PureSimFS m a = PureSimFS { unPureSimFS :: StateT MockFS m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState MockFS
           )

type instance FsHandle (PureSimFS m) = Mock.Handle
data instance Buffer   (PureSimFS m) = MockBufferUnused

runPureSimFS :: PureSimFS m a -> MockFS -> m (a, MockFS)
runPureSimFS = runStateT . unPureSimFS

pureHasFS :: forall m. Monad m
          => ErrorHandling FsError m -> HasFS (PureSimFS m)
pureHasFS err = HasFS {
      dumpState = Mock.dumpState
    , newBuffer                = \_ -> return MockBufferUnused
    , hOpen                    = Mock.hOpen                    err'
    , hClose                   = Mock.hClose                   err'
    , hSeek                    = Mock.hSeek                    err'
    , hGet                     = Mock.hGet                     err'
    , hPut                     = Mock.hPut                     err'
    , hPutBuffer               = Mock.hPutBuffer               err'
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
