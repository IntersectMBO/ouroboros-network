{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- TODO: This module consists of
--
-- * Unit tests for the HasFS mock implementation.
--   These should have been made obsolete "Test.Ouroboros.StateMachine".
--   We should verify that this is the case and remove them.
--   NOTE: Many of these tests are comparing the results of calls to seek,
--   but that is now unit, so those tests have become meaningless.
module Test.Ouroboros.Storage.FS
  ( tests
  ) where

import           Data.Either (isRight)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import qualified System.IO as IO

import qualified Test.Ouroboros.Storage.FS.StateMachine as StateMachine
import           Test.Ouroboros.Storage.Util
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.FS.API
import qualified Ouroboros.Storage.FS.API.Example as FS.API.Example
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..))
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock

--
-- The list of all tests
--

tests :: FilePath -> TestTree
tests tmpDir = testGroup "HasFS"
    [ StateMachine.tests tmpDir

    -- , testCase     "touchFile works"       test_touchFile
    , testCase     "createDirectory works" test_createDirectory
    , testCase     "listDirectory works"   test_listDirectory
      -- hOpen
    , testCase     "hOpen on an existent folder succeeds"  test_hOpenDoesExist
    , testCase     "hOpen on an non-existent folder fails" test_hOpenDoesNotExist
    , testProperty "hOpen(Sim) == hOpen(IO)"     prop_hOpen
    , testCase     "hOpen read contention"       test_hOpenReadContention
    , testCase     "hOpen read/write contention" test_hOpenReadWriteContention
    , testCase     "hOpen ReadOnly and hPut"     test_hOpenWriteInvalid
    , testCase     "hOpen WriteOnly and hGet "   test_hOpenReadInvalid
    , testCase     "hOpen and hPut with Append " test_hOpenAppend
    , testCase     "hOpen on directories fails"  test_hOpenDirectory
      -- hClose
    , testCase     "hClose twice no-op " test_hCloseTwice
      -- hPut
    , testCase     "hPut equivalence" test_hPut
      -- hGet
    , testCase     "hGet equivalence"  test_hGet
    , testCase     "hGet moves offset" test_hGetOffset
      -- hSeek
    , testCase     "hSeek from end" test_hSeekFromEnd
      -- hTruncate
    , testCase     "hTruncate equivalence" test_hTruncate
      -- hCreateDirectory
    , testCase     "hCreate directories and parents" test_hCreateDirectory
      -- doesFileExist
    , testCase     "doesFileExist yields True  for an existing file"    test_doesFileExistOK
    , testCase     "doesFileExist yields False for a non-existing file" test_doesFileExistKO
      -- doesDirectoryExist
    , testCase     "doesDirectoryExist yields True  for an existing directory"    test_doesDirectoryExistOK
    , testCase     "doesDirectoryExist yields False for a non-existing directory" test_doesDirectoryExistKO

    , testCase     "removeFile equivalence" test_removeFile
      -- example
    , testCase     "example equivalence" test_example
    ]

{------------------------------------------------------------------------------
 The tests proper
-------------------------------------------------------------------------------}

{-
test_touchFile :: Assertion
test_touchFile = do
    FS.touch ["foo.txt"] (Folder mempty) @?=
        (Folder $ M.fromList [("foo.txt", File BS.empty)])
    let action = FS.touch ["demo", "bar.md"] . FS.touch ["demo", "foo.txt"]
    action (Folder $ M.fromList [("demo", Folder mempty)]) @?=
        (Folder $ M.fromList [
            ("demo", Folder $ M.fromList [
              ("foo.txt", File BS.empty)
            , ("bar.md", File BS.empty)
            ]
            )])
-}

test_createDirectory :: Assertion
test_createDirectory = do
    withMockFS tryAny (\(Right (_, fs1)) -> Mock.mockFiles fs1 @?= result1) $ \HasFS{..} _err ->
      createDirectory ["foo"]
    withMockFS tryAny (\(Right (_, fs2)) -> Mock.mockFiles fs2 @?= result2) $ \HasFS{..} _err ->
      createDirectoryIfMissing True ["demo", "foo"]
  where
     result1 = Folder $ M.fromList [("foo", Folder mempty)]
     result2 = (Folder $ M.fromList [
                 ("demo", Folder $ M.fromList [("foo", Folder mempty)]
               )])

test_listDirectory :: Assertion
test_listDirectory =
    withMockFS tryFS (expectFsResult $ \(r, _) -> r @?= files) $ \hasFS@HasFS{..} _err  -> do
      createDirectory ["foo"]
      withFile hasFS ["foo", "foo.txt"] IO.WriteMode $ \_ -> return ()
      withFile hasFS ["foo", "bar.txt"] IO.WriteMode $ \_ -> return ()
      withFile hasFS ["foo", "quux.md"] IO.WriteMode $ \_ -> return ()
      listDirectory ["foo"]
  where
    files = Set.fromList ["foo.txt", "bar.txt", "quux.md"]

--
-- hOpen tests
--

test_hOpenDoesExist :: Assertion
test_hOpenDoesExist =
    flip (withMockFS tryFS) (\HasFS{..} _err -> hOpen ["foo.txt"] IO.WriteMode) $ \(Right (_r, fs)) -> do
      -- The file must have been created
      assertBool "file not in the mock FS" $
        isRight (FS.index ["foo.txt"] (Mock.mockFiles fs))

test_hOpenDoesNotExist :: Assertion
test_hOpenDoesNotExist =
    withMockFS tryFS (expectFsError FsResourceDoesNotExist) $ \HasFS{..} _err ->
      hOpen ["test", "foo.txt"] IO.WriteMode

prop_hOpen :: Property
prop_hOpen = monadicIO $ do
    ioMode <- pick $ elements [IO.ReadMode, IO.AppendMode, IO.ReadWriteMode]
    let assrt =
          if ioMode == IO.ReadMode
          then expectFsError FsResourceDoesNotExist
          else expectFsResult (@?= ())
    run $ apiEquivalenceFs assrt $ \HasFS{..} _err -> do
      h <- hOpen ["foo.txt"] ioMode
      hClose h

test_hOpenWriteInvalid :: Assertion
test_hOpenWriteInvalid = apiEquivalenceFs (expectFsError FsInvalidArgument) $ \HasFS{..} _err -> do
    _  <- hOpen ["foo.txt"] IO.WriteMode
    h2 <- hOpen ["foo.txt"] IO.ReadMode
    hPut h2 "haskell-is-nice"

test_hOpenReadInvalid :: Assertion
test_hOpenReadInvalid = apiEquivalenceFs (expectFsError FsInvalidArgument) $ \HasFS{..} _err -> do
    h2 <- hOpen ["foo.txt"] IO.AppendMode
    hGet h2 5

test_hOpenAppend :: Assertion
test_hOpenAppend = apiEquivalenceFs (expectFsResult (@?= res)) $ \HasFS{..} _err -> do
    h1 <- hOpen ["foo.txt"] IO.AppendMode
    h2 <- hOpen ["foo.txt"] IO.ReadMode
    ls <- sequence
      [ hPut h1 "12"
      , hPut h1 "34"
      , hPut h1 "56"
      , hPut h1 "78"
      ]
    -- IO.AppendMode moves offset at the end of file before each hPut.
    -- This means the file consists of all 8 bytes written above.
    bs <- hGet h2 8
    hSeek h2 IO.RelativeSeek 0
    return (ls, bs)
  where
    res = ([2, 2, 2, 2], "12345678")

test_hOpenReadContention :: Assertion
test_hOpenReadContention = apiEquivalenceFs (expectFsResult (@?= ())) $ \HasFS{..} _err -> do
    -- First create it
    h0 <- hOpen ["foo.txt"] IO.WriteMode
    hClose h0
    h1 <- hOpen ["foo.txt"] IO.ReadMode
    h2 <- hOpen ["foo.txt"] IO.ReadMode
    hClose h1
    hClose h2

test_hOpenReadWriteContention :: Assertion
test_hOpenReadWriteContention = apiEquivalenceFs (expectFsResult (@?= ())) $ \HasFS{..} _err -> do
    h0 <- hOpen ["foo.txt"] IO.WriteMode
    hClose h0
    h1 <- hOpen ["foo.txt"] IO.ReadMode
    h2 <- hOpen ["foo.txt"] IO.WriteMode
    hClose h1
    hClose h2

test_hOpenDirectory :: Assertion
test_hOpenDirectory = apiEquivalenceFs (expectFsError FsResourceInappropriateType) $ \HasFS{..} _err -> do
    createDirectoryIfMissing True ["foo"]
    h1 <- hOpen ["foo"] IO.WriteMode
    hClose h1

test_example :: Assertion
test_example = apiEquivalenceFs (expectFsResult (@?= ["test", "block"])) $ \hasFS@HasFS{..} _err -> do
    -- The example assumes the presence of some files and dirs.
    createDirectoryIfMissing True ["var", "tmp"]
    withFile hasFS ["var", "tmp", "foo.txt"] IO.WriteMode $ \_ -> return ()
    FS.API.Example.example hasFS

--
-- hClose tests
--

test_hCloseTwice :: Assertion
test_hCloseTwice = apiEquivalenceFs (expectFsResult (@?= ())) $ \HasFS{..} _err -> do
    h1 <- hOpen ["test.txt"] IO.WriteMode
    hClose h1
    hClose h1

test_hPut :: Assertion
test_hPut = apiEquivalenceFs (expectFsResult (@?= 15)) $ \hasFS@HasFS{..} _err -> do
    withFile hasFS ["test.txt"] IO.WriteMode $ \h1 ->
      hPut h1 "haskell-is-nice"

test_hGet :: Assertion
test_hGet = apiEquivalenceFs (expectFsResult (@?= (15, ["hask", "ell", "-nic"]))) $ \hasFS@HasFS{..} _err ->
    withFile hasFS ["test.txt"] IO.WriteMode $ \h1 -> do
      b  <- hPut h1 "haskell-is-nice"
      _  <- hSeek h1 IO.AbsoluteSeek 0
      r1 <- hGet h1 4
      r2 <- hGet h1 3
      _  <- hSeek h1 IO.RelativeSeek 3
      r3 <- hGet h1 4
      return (b, [r1, r2, r3])

test_hSeekFromEnd :: Assertion
test_hSeekFromEnd = apiEquivalenceFs (expectFsResult (@?= 15)) $ \hasFS@HasFS{..} _err ->
    withFile hasFS ["foo.txt"] IO.WriteMode $ \h1 -> do
      b <- hPut  h1 "haskell-is-nice"
      hSeek h1 IO.SeekFromEnd (-4)
      _ <- hPut  h1 "NICE"
      return b

test_hGetOffset :: Assertion
test_hGetOffset = apiEquivalenceFs (expectFsResult (@?= ("01234", "A67"))) $ \hasFS@HasFS{..} _err ->
    withFile hasFS ["foo.txt"] IO.WriteMode $ \h1 -> do
      _  <- hPut h1 "0123456789"
      hSeek h1 IO.AbsoluteSeek 0
      b1 <- hGet h1 5
      _  <- hPut h1 "A"
      hSeek h1 IO.RelativeSeek (-1)
      b2 <- hGet h1 3
      return (b1, b2)

test_hTruncate :: Assertion
test_hTruncate =
    apiEquivalenceFs (expectFsResult (@?= (15, ["haskell-is-nice", "haskell"]))) $ \hasFS@HasFS{..} _err ->
      withFile hasFS ["test.txt"] IO.AppendMode $ \h1 ->
      withFile hasFS ["test.txt"] IO.ReadMode $ \h2 -> do
        b  <- hPut h1 "haskell-is-nice"
        r1 <- hGet h2 15
        hTruncate h1 7
        _  <- hSeek h2 IO.AbsoluteSeek 0
        r2 <- hGet h2 15
        hClose h1
        return (b, [r1,r2])

test_hCreateDirectory :: Assertion
test_hCreateDirectory = apiEquivalenceFs (expectFsResult (@?= ())) $ \HasFS{..} _err ->
    createDirectoryIfMissing True ["xxx", "yyy"]

test_doesFileExistOK :: Assertion
test_doesFileExistOK = withMockFS tryFS (expectFsResult ((@?= True) . fst)) $ \HasFS{..} _err -> do
    h1 <- hOpen ["foo.txt"] IO.WriteMode
    hClose h1
    doesFileExist ["foo.txt"]

test_doesFileExistKO :: Assertion
test_doesFileExistKO = withMockFS tryFS (expectFsResult ((@?= False) . fst)) $ \HasFS{..} _err ->
    doesFileExist ["foo.txt"]

test_doesDirectoryExistOK :: Assertion
test_doesDirectoryExistOK = withMockFS tryFS (expectFsResult ((@?= True) . fst)) $ \HasFS{..} _err -> do
    createDirectoryIfMissing True ["test-dir"]
    doesDirectoryExist ["test-dir"]

test_doesDirectoryExistKO :: Assertion
test_doesDirectoryExistKO = withMockFS tryFS (expectFsResult ((@?= False) . fst )) $ \HasFS{..} _err ->
    doesDirectoryExist ["test-dir"]

test_removeFile :: Assertion
test_removeFile = apiEquivalenceFs (expectFsResult (@?= False)) $ \HasFS{..} _err -> do
    let file = ["foo.txt"]
    h1 <- hOpen file IO.WriteMode
    hClose h1
    removeFile file
    doesFileExist file
