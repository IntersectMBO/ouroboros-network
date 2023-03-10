module Test.Util.FS.Sim.Error {-# DEPRECATED "Use System.FS.Sim.Error from fs-sim" #-} (
    -- * Simulate Errors monad
    mkSimErrorHasFS
  , runSimErrorFS
  , withErrors
    -- * Streams
  , ErrorStream
  , ErrorStreamGetSome
  , ErrorStreamPutSome
  , Stream (..)
  , always
  , mkStream
  , mkStreamGen
  , null
  , runStream
    -- * Generating partial reads/writes
  , Partial (..)
  , hGetSomePartial
  , hPutSomePartial
    -- * Generating corruption for 'hPutSome'
  , PutCorruption (..)
  , corrupt
    -- * Error streams for 'HasFS'
  , Errors (..)
  , allNull
  , genErrors
  , simpleErrors
  ) where

import           Prelude hiding (null)

import           System.FS.Sim.Error
