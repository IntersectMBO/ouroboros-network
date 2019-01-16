{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Monad.Except

import qualified Data.ByteString.Builder.Extra as BL
import qualified Data.ByteString.Lazy.Char8 as C8
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

import           Ouroboros.Storage.FS.Class
import qualified Ouroboros.Storage.FS.Class.Example as FS.Class.Example
import           Ouroboros.Storage.FS.Class.Types
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..))
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock

--
-- The list of all tests
--

tests :: HasCallStack => FilePath -> TestTree
tests tmpDir = testGroup "HasFS"
    [ StateMachine.tests tmpDir

      -- testCase     "touchFile works" test_touchFile
    , testCase     "createDirectory works" test_createDirectory
    , testCase     "listDirectory works" test_listDirectory
      -- hOpen
    , testCase     "hOpen on an existent folder succeeds"  test_hOpenDoesExist
    , testCase     "hOpen on an non-existent folder fails" test_hOpenDoesNotExist
    , testProperty "hOpen(Sim) == hOpen(IO)"     $ prop_hOpenEquivalence
    , testProperty "hOpen read contention"       $ prop_hOpenReadContention
    , testProperty "hOpen read/write contention" $ prop_hOpenReadWriteContention
    , testProperty "hOpen ReadOnly and hput"     $ prop_hOpenWriteInvalid
    , testProperty "hOpen WriteOnly and hGet "   $ prop_hOpenReadInvalid
    , testProperty "hOpen and hPut with Append " $ prop_hOpenAppend
    , testProperty "hOpen on directories fails"   $ prop_hOpenDirectory
      -- hClose
    , testCase     "hClose twice no-op "  $ test_hCloseTwice
    , testProperty "hClose twice no-op equivalence"  $ prop_hCloseTwiceEquivalence
      -- hPut
    , testProperty "hPut equivalence"  $ prop_hPutEquivalence
      -- hPutBuffer
    , testProperty "hPutBuffer chunk boundaries"  $ prop_hPutBufferBoundaries
      -- hGet
    , testProperty "hGet equivalence"  $ prop_hGetEquivalence
      -- hSeek
    , testProperty "hSeek from end" $ prop_hSeekFromEnd
    , testProperty "hGet moves offset" $ prop_hGetOffset
      -- hTruncate
    , testProperty "hTruncate equivalence"  $ prop_hTruncateEquivalence
      -- hCreateDirectory
    , testProperty "hCreate directories and parents" $ prop_hCreateDirectory
      -- doesFileExist
    , testCase "doesFileExist yields True  for an existing file" $ test_doesFileExistOK
    , testCase "doesFileExist yields False for a non-existing file" $ test_doesFileExistKO
      -- doesDirectoryExist
    , testCase "doesDirectoryExist yields True  for an existing directory" $ test_doesDirectoryExistOK
    , testCase "doesDirectoryExist yields False for a non-existing directory" $ test_doesDirectoryExistKO
      -- mockDemo
    , testProperty "mockDemo equivalence" $ prop_mockDemo
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
    let action1 = createDirectory ["foo"]
        result1 = Folder $ M.fromList [("foo", Folder mempty)]
        action2 = createDirectoryIfMissing True ["demo", "foo"]
        result2 = (Folder $ M.fromList [
                    ("demo", Folder $ M.fromList [("foo", Folder mempty)]
                  )])
    withMockFSE (runExceptT action1) $ \(_, fs1) -> Mock.mockFiles fs1 @?= result1
    withMockFSE (runExceptT action2) $ \(_, fs2) -> Mock.mockFiles fs2 @?= result2

test_listDirectory :: Assertion
test_listDirectory = do
    let script = runExceptT $ do
             createDirectory ["foo"]
             withFile ["foo", "foo.txt"] IO.WriteMode $ \_ -> return ()
             withFile ["foo", "bar.txt"] IO.WriteMode $ \_ -> return ()
             withFile ["foo", "quux.md"] IO.WriteMode $ \_ -> return ()
             listDirectory ["foo"]
    withMockFSE script $ \(r, _) ->
        case r of
             Left e     -> fail (show e)
             Right dirs -> dirs @?= Set.fromList ["foo.txt", "bar.txt", "quux.md"]

--
-- hOpen tests
--

test_hOpenDoesExist :: Assertion
test_hOpenDoesExist =
    withMockFSE (runExceptT $ hOpen ["foo.txt"] IO.WriteMode) $ \(r, fs) -> do
    assertBool "hOpen failed" (isRight r)
    -- The file must have been created
    assertBool "file not in the mock FS" $
        isRight (FS.index ["foo.txt"] (Mock.mockFiles fs))

test_hOpenDoesNotExist :: Assertion
test_hOpenDoesNotExist =
    withMockFS (runExceptT $ hOpen ["test", "foo.txt"] IO.WriteMode) $ \(r, _) ->
    expectError (isFsErrorType FsResourceDoesNotExist) r "return type was not FsResourceDoesNotExist"

prop_hOpenEquivalence :: Property
prop_hOpenEquivalence = monadicIO $ do
    ioMode   <- pick $ elements [IO.ReadMode, IO.AppendMode, IO.ReadWriteMode]
    run $ apiEquivalence (runExceptT $ do
                             h <- hOpen ["foo.txt"] ioMode
                             hClose h
                         ) sameFsError prettyFsError

prop_hOpenWriteInvalid :: Property
prop_hOpenWriteInvalid = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                             _ <- hOpen ["foo.txt"] IO.WriteMode
                             h2 <- hOpen ["foo.txt"] IO.ReadMode
                             hPut h2 "haskell-is-nice"
                         ) sameFsError prettyFsError

prop_hOpenReadInvalid :: Property
prop_hOpenReadInvalid = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                            h2 <- hOpen ["foo.txt"] IO.AppendMode
                            hGet h2 5
                        ) sameFsError prettyFsError

prop_hOpenAppend :: Property
prop_hOpenAppend = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                            h1 <- hOpen ["foo.txt"] IO.AppendMode
                            h2 <- hOpen ["foo.txt"] IO.ReadMode
                            ls <- sequence
                                [
                                  hPut h1 "12"
                                , hPut h1 "34"
                                , hPut h1 "56"
                                , hPut h1 "78"
                                ]
                            -- IO.AppendMode moves offset at the end of file before each hPut.
                            -- This means the file consists of all 8 bytes written above.
                            bs <- hGet h2 8
                            offset <- hSeek h2 IO.RelativeSeek 0
                            return (ls, bs, offset)
                        ) sameFsError prettyFsError

-- Opening two read handles on the same file should be allowed in both
-- implementations.
prop_hOpenReadContention :: Property
prop_hOpenReadContention = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                             h1 <- hOpen ["foo.txt"] IO.ReadMode
                             h2 <- hOpen ["foo.txt"] IO.ReadMode
                             hClose h1
                             hClose h2
                         ) sameFsError prettyFsError

prop_hOpenReadWriteContention :: Property
prop_hOpenReadWriteContention = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                             h2 <- hOpen ["foo.txt"] IO.WriteMode
                             h1 <- hOpen ["foo.txt"] IO.ReadMode
                             hClose h1
                             hClose h2
                         ) sameFsError prettyFsError

prop_hOpenDirectory :: Property
prop_hOpenDirectory = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                             createDirectoryIfMissing True ["foo"]
                             h1 <- hOpen ["foo"] IO.WriteMode
                             hClose h1
                         ) sameFsError prettyFsError

prop_mockDemo :: Property
prop_mockDemo = monadicIO $ run $
    apiEquivalence (runExceptT $ do
                       -- The mockDemoScript assumes the presence of some
                       -- files and dirs.
                       createDirectoryIfMissing True ["var", "tmp"]
                       withFile ["var", "tmp", "foo.txt"] IO.WriteMode $ \_ ->
                          return ()
                       FS.Class.Example.example
                   ) sameFsError prettyFsError

--
-- hClose tests
--
test_hCloseTwice :: Assertion
test_hCloseTwice =
    withMockFS (runExceptT $ do
                  h1 <- hOpen ["foo.txt"] IO.WriteMode
                  hClose h1
                  hClose h1
               ) $ \(r, _) ->
    case r of
         Left e   -> fail $ prettyFsError e
         Right () -> return ()

prop_hCloseTwiceEquivalence :: Property
prop_hCloseTwiceEquivalence = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                             h1 <- hOpen ["test.txt"] IO.WriteMode
                             hClose h1
                             hClose h1
                         ) sameFsError prettyFsError

prop_hPutEquivalence :: Property
prop_hPutEquivalence = monadicIO $ do
    run $ apiEquivalence (runExceptT $
              withFile ["test.txt"] IO.WriteMode $ \h1 ->
                  hPut h1 "haskell-is-nice"
        ) sameFsError prettyFsError

-- In this test purposefully create a very small buffer and we test that our
-- 'bytesWritten' count is consistent across chunks boundaries.
prop_hPutBufferBoundaries :: Property
prop_hPutBufferBoundaries = monadicIO $ do
    bufferSize <- pick $ choose (1, 64)
    input      <- pick $ C8.pack <$> listOf1 (elements ['a' .. 'z'])
    threshold  <- pick $ choose (1, 512)
    run $ apiEquivalence (runExceptT $ do
                             b  <- newBuffer bufferSize
                             h1 <- hOpen ["test.txt"] IO.WriteMode
                             bytesWritten  <-
                                 hPutBuffer h1 b (BL.lazyByteStringThreshold threshold input)
                             _ <- hSeek h1 IO.AbsoluteSeek 0
                             r <- hGet h1 (fromIntegral $ C8.length input)
                             hClose h1
                             return ( fromIntegral bytesWritten == (C8.length input)
                                    , bytesWritten
                                    , r
                                    , C8.toStrict input == r
                                    )
                         ) sameFsError prettyFsError

prop_hGetEquivalence :: Property
prop_hGetEquivalence = monadicIO $ do
    run $ apiEquivalence (runExceptT $
              withFile ["test.txt"] IO.WriteMode $ \h1 -> do
                  b  <- hPut h1 "haskell-is-nice"
                  _ <- hSeek h1 IO.AbsoluteSeek 0
                  r1 <- hGet h1 4
                  r2 <- hGet h1 3
                  _ <- hSeek h1 IO.RelativeSeek 3
                  r3 <- hGet h1 4
                  return (b, [r1,r2,r3])
        ) sameFsError prettyFsError

prop_hSeekFromEnd :: Property
prop_hSeekFromEnd = monadicIO $ do
    run $ apiEquivalence (runExceptT $
                withFile ["foo.txt"] IO.WriteMode $ \h1 -> do
                    b <- hPut  h1 "haskell-is-nice"
                    r <- hSeek h1 IO.SeekFromEnd (-4)
                    _ <- hPut  h1 "NICE"
                    return (b,r)
        ) sameFsError prettyFsError


prop_hGetOffset :: Property
prop_hGetOffset = monadicIO $ do
    run $ apiEquivalence (runExceptT $
                withFile ["foo.txt"] IO.WriteMode $ \h1 -> do
                    _  <- hPut h1 "0123456789"
                    hSeek h1 IO.AbsoluteSeek 0
                    b1 <- hGet h1 5
                    _  <- hPut h1 "A"
                    hSeek h1 IO.RelativeSeek (-1)
                    b2 <- hGet h1 3
                    return (b1, b2)

        ) sameFsError prettyFsError

prop_hTruncateEquivalence :: Property
prop_hTruncateEquivalence = monadicIO $ do
    run $ apiEquivalence (runExceptT $
             withFile ["test.txt"] IO.AppendMode $ \h1 -> do
               withFile ["test.txt"] IO.ReadMode $ \h2 -> do
                 b  <- hPut h1 "haskell-is-nice"
                 r1 <- hGet h2 15
                 hTruncate h1 7
                 _  <- hSeek h2 IO.AbsoluteSeek 0
                 r2 <- hGet h2 15
                 hClose h1
                 return (b, [r1,r2])
       ) sameFsError prettyFsError

prop_hCreateDirectory :: Property
prop_hCreateDirectory = withMaxSuccess 1 $ monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                            createDirectoryIfMissing True ["xxx", "yyy"]
                        ) sameFsError prettyFsError

test_doesFileExistOK :: Assertion
test_doesFileExistOK =
    withMockFS (runExceptT $ do
                  h1 <- hOpen ["foo.txt"] IO.WriteMode
                  hClose h1
                  doesFileExist ["foo.txt"]
               ) $ \(r, _) ->
    case r of
         Left e  -> fail $ prettyFsError e
         Right b -> b @? "doesFileExist didn't work as expected"

test_doesFileExistKO :: Assertion
test_doesFileExistKO =
    withMockFS (runExceptT $ doesFileExist ["foo.txt"]) $ \(r, _) ->
    case r of
         Left e  -> fail $ prettyFsError e
         Right b -> not b @? "doesFileExist didn't work as expected"

test_doesDirectoryExistOK :: Assertion
test_doesDirectoryExistOK =
    withMockFS (runExceptT $ do
                  createDirectoryIfMissing True ["test-dir"]
                  doesDirectoryExist ["test-dir"]
               ) $ \(r, _) ->
    case r of
         Left e  -> fail $ prettyFsError e
         Right b -> b @? "doesDirectoryExist didn't work as expected"

test_doesDirectoryExistKO :: Assertion
test_doesDirectoryExistKO =
    withMockFS (runExceptT $ doesDirectoryExist ["test-dir"]) $ \(r, _) ->
    case r of
         Left e  -> fail $ prettyFsError e
         Right b -> not b @? "doesDirectoryExist didn't work as expected"

