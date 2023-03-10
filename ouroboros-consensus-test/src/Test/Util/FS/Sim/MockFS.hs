module Test.Util.FS.Sim.MockFS {-# DEPRECATED "Use System.FS.Sim.MockFS from fs-sim" #-} (
    empty
  , example
  , handleIsOpen
  , numOpenHandles
  , pretty
    -- * Debugging
  , dumpState
    -- * Operations on files
  , hClose
  , hGetSize
  , hGetSome
  , hGetSomeAt
  , hIsOpen
  , hOpen
  , hPutSome
  , hSeek
  , hTruncate
    -- * Operations on directories
  , createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeFile
  , renameFile
    -- * Exported for the benefit of tests only
  , Files
  , mockFiles
    -- ** opaque
  , ClosedHandleState
  , FilePtr
  , HandleState
  , OpenHandleState
    -- * opaque
  , HandleMock
  , MockFS
  ) where

import           System.FS.Sim.MockFS
