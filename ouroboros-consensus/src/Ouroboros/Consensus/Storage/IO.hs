module Ouroboros.Consensus.Storage.IO {-# DEPRECATED "Use System.IO.FS from fs-api" #-} (
    FHandle
  , close
  , getSize
  , open
  , pread
  , read
  , sameError
  , seek
  , truncate
  , write
  ) where

import           Prelude hiding (read, truncate)

import           System.IO.FS
