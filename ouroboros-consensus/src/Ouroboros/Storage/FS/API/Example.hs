{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ouroboros.Storage.FS.API.Example (
    example
  , exampleSimFS
  ) where

import           Control.Exception (try)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Set as Set
import           GHC.Stack

import           Ouroboros.Consensus.Util
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{-------------------------------------------------------------------------------
  Example program that can run in any monad with file system support
-------------------------------------------------------------------------------}

example :: (HasCallStack, Monad m) => HasFS m h -> m [ByteString]
example hasFS@HasFS{..} = do
    h1 <- hOpen (mkFsPath ["cardano.txt"]) (ReadWriteMode MustBeNew)
    _  <- hPut hasFS h1 "test"
    _  <- hSeek h1 AbsoluteSeek 0
    r1 <- hGetExactly hasFS h1 4
    _  <- hPut hasFS h1 "ing"
    h2 <- hOpen (mkFsPath ["bar.txt"]) (ReadWriteMode MustBeNew)
    _  <- hPut hasFS h2 "blockchain"
    _  <- hSeek h2 AbsoluteSeek 0
    r2 <- hGetExactly hasFS h2 5
    _  <- listDirectory (mkFsPath [])
    _  <- listDirectory (mkFsPath ["var"])
    createDirectory $ mkFsPath ["var", "tmp", "my-temp-dir"]
    createDirectoryIfMissing True $ mkFsPath ["home", "adinapoli", "test", "foo", "bar"]
    f1 <- listDirectory $ mkFsPath ["var", "tmp"]
    hClose h1
    hClose h2
    checkThat "listDirectory [var, tmp]" ((==) (Set.fromList ["my-temp-dir", "foo.txt"])) f1
    checkThat "hGetExactly hasFS h1 4" ((==) "test") r1
    checkThat "hGetExactly hasFS h2 5" ((==) "block") r2
    return [r1, r2]

{-------------------------------------------------------------------------------
  Run in various monads
-------------------------------------------------------------------------------}

exampleSimFS :: IO ()
exampleSimFS = do
    mRes <- try demo
    case mRes of
      Left  err      -> putStrLn (prettyFsError err)
      Right (bs, fs) -> putStrLn (Mock.pretty fs) >> print bs
  where
    demo :: IO ([ByteString], Mock.MockFS)
    demo = runSimFS EH.exceptions Mock.example example
