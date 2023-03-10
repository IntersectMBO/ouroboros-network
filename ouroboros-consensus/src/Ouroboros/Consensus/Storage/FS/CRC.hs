module Ouroboros.Consensus.Storage.FS.CRC {-# DEPRECATED "Use System.FS.CRC from fs-api" #-} (
    -- * Wrap digest functionality
    CRC (..)
  , computeCRC
  , initCRC
  , updateCRC
    -- * File system functions with CRC functionality
  , hGetAllAtCRC
  , hGetExactlyAtCRC
  , hPutAllCRC
  ) where

import           System.FS.CRC
