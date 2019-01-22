{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ouroboros.Storage.FS.API.Example (
    example
  , exampleSimFS
  ) where

import           Control.Exception (try)
import           Data.ByteString (ByteString)
import qualified Data.Set as Set
import           GHC.Stack
import qualified System.IO as IO

import           Ouroboros.Consensus.Util
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{-------------------------------------------------------------------------------
  Example program that can run in any monad with file system support
-------------------------------------------------------------------------------}

example :: (HasCallStack, Monad m) => HasFS m -> m [ByteString]
example HasFS{..} = do
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
    mRes <- try $ runSimFS demo Mock.example
    case mRes of
      Left  err      -> putStrLn (prettyFsError err)
      Right (bs, fs) -> putStrLn (Mock.pretty fs) >> print bs
  where
    demo :: SimFS IO [ByteString]
    demo = example (simHasFS EH.exceptions)
