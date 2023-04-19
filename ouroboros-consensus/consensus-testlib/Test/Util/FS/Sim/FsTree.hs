module Test.Util.FS.Sim.FsTree {-# DEPRECATED "Use System.FS.Sim.FsTree from fs-sim" #-} (
    -- * FsTree type and indexing functions
    FsTree (..)
  , FsTreeError (..)
  , example
    -- * Construction
  , empty
    -- * Indexing
  , getDir
  , getFile
  , index
    -- * File system operations
  , createDirIfMissing
  , createDirWithParents
  , openFile
  , removeFile
  , renameFile
  , replace
    -- * Pretty-printing
  , pretty
  ) where

import           System.FS.Sim.FsTree
