module Ouroboros.Consensus.Storage.FS.Handle {-# DEPRECATED "Use System.FS.Handle from fs-api" #-} (
    HandleOS (..)
  , closeHandleOS
  , isHandleClosedException
  , isOpenHandleOS
  , withOpenHandle
  ) where

import           System.FS.Handle
