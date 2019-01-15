{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Storage.FS.Class.Example (
    example
  , exampleSimFS
  ) where

import           Control.Monad.Except
import           Data.ByteString (ByteString)
import qualified Data.Set as Set
import           GHC.Stack
import qualified System.IO as IO

import           Ouroboros.Consensus.Util
import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.FS.Class.Types
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM

{-------------------------------------------------------------------------------
  Example program that can run in any monad with file system support
-------------------------------------------------------------------------------}

example :: (HasCallStack, HasFS m) => m [ByteString]
example = do
    h1 <- hOpen ["cardano.txt"] IO.ReadWriteMode
    _  <- hPut h1 "test"
    _  <- hSeek h1 IO.AbsoluteSeek 0
    r1 <- hGet h1 4
    _  <- hPut h1 "ing"
    h2 <- hOpen ["bar.txt"] IO.ReadWriteMode
    _  <- hPut h2 "blockchain"
    _  <- hSeek h2 IO.AbsoluteSeek 0
    r2 <- hGet h2 5
    _  <- listDirectory []
    _  <- listDirectory ["var"]
    createDirectory ["var", "tmp", "my-temp-dir"]
    createDirectoryIfMissing True ["home", "adinapoli", "test", "foo", "bar"]
    f1 <- listDirectory ["var", "tmp"]
    hClose h1
    hClose h2
    checkThat "listDirectory [var, tmp]" ((==) (Set.fromList ["my-temp-dir", "foo.txt"])) f1
    checkThat "hGet h1 4" ((==) "test") r1
    checkThat "hGet h2 5" ((==) "block") r2
    return [r1, r2]

{-------------------------------------------------------------------------------
  Run in various monads
-------------------------------------------------------------------------------}

exampleSimFS :: IO ()
exampleSimFS = do
    (res, fs) <- runSimFS (runExceptT demo) Mock.example
    case res of
      Left  err -> putStrLn (prettyFsError err)
      Right bs  -> putStrLn (Mock.pretty fs) >> print bs
  where
    demo :: ExceptT FsError (SimFS IO) [ByteString]
    demo = example
