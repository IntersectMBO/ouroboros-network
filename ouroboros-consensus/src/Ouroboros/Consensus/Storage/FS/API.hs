module Ouroboros.Consensus.Storage.FS.API {-# DEPRECATED "Use System.FS.API from fs-api" #-} (
    Handle (..)
  , HasFS (..)
  , SomeHasFS (..)
  , hClose'
  , hGetAll
  , hGetAllAt
  , hGetExactly
  , hGetExactlyAt
  , hPut
  , hPutAll
  , hPutAllStrict
  , withFile
  ) where

import           System.FS.API
