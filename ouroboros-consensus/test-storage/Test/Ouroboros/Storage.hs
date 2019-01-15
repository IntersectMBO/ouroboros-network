{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- TODO: This module consists of
--
-- * Unit tests for the HasFS mock implementation.
--   These should have been made obsolete "Test.Ouroboros.StateMachine".
--   We should verify that this is the case and remove them.
--   NOTE: Many of these tests are comparing the results of calls to seek,
--   but that is now unit, so those tests have become meaningless.
--
-- * Some unit tests for the immutable DB.
--   These should move to a module of their own, and be similarly be replaced
--   by q-s-m tests.
module Test.Ouroboros.Storage
  ( tests
  ) where

import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Except
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Builder.Extra as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Either (isRight)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified System.IO as IO
import           System.IO.Temp

import           Test.Ouroboros.Storage.Immutable.Sim (demoScript)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Storage.FS.Class
import qualified Ouroboros.Storage.FS.Class.Example as FS.Class.Example
import           Ouroboros.Storage.FS.Class.Types
import           Ouroboros.Storage.FS.IO (runIOFS)
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..))
import qualified Ouroboros.Storage.FS.Sim.FsTree as FS
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM
import           Ouroboros.Storage.Immutable.DB

--
-- The list of all tests
--

tests :: HasCallStack => TestTree
tests = testGroup "Storage"
  [ testGroup "HasFS"
    [ -- testCase     "touchFile works" test_touchFile
      testCase     "createDirectory works" test_createDirectory
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
    , testProperty "mockDemo equivalence"        $ prop_mockDemo
  ]
  , testGroup "Immutable Storage"
    [ testCase "What you store is what you get" test_appendAndGet
    , testProperty "append/get roundtrip" prop_appendAndGetRoundtrip
    , testProperty "Inconsistent slot error equivalence" prop_inconsistentSlotErrorEquivalence
    , testProperty "Epoch is read only error equivalence" prop_epochIsReadOnlyErrorEquivalence
    , testProperty "Read from invalid epoch error equivalence" prop_slotDoesNotExistErrorEquivalence
      -- demoScript
    , testProperty "demoScript equivalence" prop_demoSimEquivalence
    ]
  , testGroup "Volatile Storage"
    [ ]
  ]

{------------------------------------------------------------------------------
 Handy combinators
-------------------------------------------------------------------------------}

withMockFS :: MonadSTM m
           => SimFS m (Either e a)
           -> ((Either e a, MockFS) -> m b)
           -> m b
withMockFS sim assertion = do
    r <- runSimFS sim Mock.empty
    assertion r

withMockFSE :: MonadSTM m
            => SimFS m (Either FsError a)
            -> ((Either FsError a, MockFS) -> m b)
            -> m b
withMockFSE = withMockFS

expectError :: (HasCallStack, Show a)
            => (FsError -> Bool)
            -> Either FsError a
            -> String
            -> Assertion
expectError errPred r lbl =
    case r of
         Right x -> fail ("Return value " <> show x <> " was not an error.")
         Left  e -> assertEqual (prettyMsg e) True (errPred e)
    where
        prettyMsg :: FsError -> String
        prettyMsg err =
            lbl <> ": error type was "
                <> prettyFsError err
                <> ", which is not what I was expecting."

-- | Given a \"script\", runs it over a simulated FS and over IO (using a
-- temporary, throw-away folder) and compare the results.
apiEquivalence :: (HasCallStack, Eq a)
               => (forall m. (MonadMask m, MonadSTM m, HasCallStack, HasFSE m) => m (Either b a))
               -> (b -> b -> Bool)
               -> (b -> String)
               -> Assertion
apiEquivalence m cmpError prettyError = do
    sysTmpDir <- Dir.getTemporaryDirectory
    withTempDirectory sysTmpDir "cardano." $ \tmpDir -> do
        (r1, fs') <- runSimFS m Mock.empty
        r2 <- runIOFS (MountPoint tmpDir) m
        case (r1, r2) of
            (Left e1, Left e2) ->
                assertBool ("SimFS & IO didn't agree on the API. "
                           <> "The implementation differs.\n\n"
                           <> "Sim returned: " <> prettyError e1  <> "\n\n"
                           <> "IO  returned: " <> prettyError e2  <> "\n\n")
                           (e1 `cmpError` e2)
            (Right x1, Right x2) ->
                assertBool "SimFS & IO didn't yield the same result."
                           (x1 == x2)
            (Left e, Right _) ->
                assertFailure $ "SimFS returned "
                             <> prettyError e
                             <> ", but IO succeeded.\n\n"
                             <> "Sim FS: " <> show fs' <> "\n\n"
            (Right _, Left e) ->
                assertFailure $ "IO returned "
                             <> prettyError e
                             <> ", but SimFS succeeded.\n\n"
                             <> "Sim FS: " <> show fs' <> "\n\n"

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

--
-- Tests for the immutable storage
--

test_appendAndGet :: Assertion
test_appendAndGet =
    withMockFS (withDB ["demo"] 0 $ \db -> runExceptT $ do
          ExceptT $ appendBinaryBlob db (0, RelativeSlot 0) (BL.lazyByteString . C8.pack $ "haskell")
          ExceptT $ getBinaryBlob db (0, RelativeSlot 0)
    ) $ \(r, _) ->
      case r of
           Left e  -> fail $ prettyImmutableDBError e
           Right b -> b @?= "haskell"

prop_appendAndGetRoundtrip :: Property
prop_appendAndGetRoundtrip = monadicIO $ do
    input <- C8.pack <$> pick arbitrary
    run $ apiEquivalence (withDB ["demo"] 0 $ \db -> runExceptT $ do
            ExceptT $ appendBinaryBlob db (0, RelativeSlot 0) (BL.lazyByteString input)
            r <- ExceptT $ getBinaryBlob db (0, RelativeSlot 0)
            return (r == C8.toStrict input, r)
        ) sameDBError prettyImmutableDBError

prop_demoSimEquivalence :: HasCallStack => Property
prop_demoSimEquivalence = monadicIO $ do
    run $ apiEquivalence demoScript sameDBError prettyImmutableDBError

-- Trying to append to a slot \"in the past\" should be an error, both in Sim
-- and IO.
prop_inconsistentSlotErrorEquivalence :: HasCallStack => Property
prop_inconsistentSlotErrorEquivalence = monadicIO $ do
    run $ apiEquivalence (withDB ["demo"] 0 $ \db -> runExceptT $ do
                             ExceptT $ appendBinaryBlob db (0, RelativeSlot 3)
                                                           (BL.lazyByteString . C8.pack $ "test")
                             ExceptT $ appendBinaryBlob db (0, RelativeSlot 2)
                                                           (BL.lazyByteString . C8.pack $ "haskell")
                             return ()
                         ) sameDBError prettyImmutableDBError

prop_slotDoesNotExistErrorEquivalence :: HasCallStack => Property
prop_slotDoesNotExistErrorEquivalence = monadicIO $ do
    run $ apiEquivalence (withDB ["demo"] 0 $ \db -> runExceptT $ do
                             _   <- ExceptT $ getBinaryBlob db (0, RelativeSlot 0)
                             return ()
                         ) sameDBError prettyImmutableDBError

-- Trying to re-open the DB not on the most-recent-epoch should trigger an
-- error, both in Sim and IO.
prop_epochIsReadOnlyErrorEquivalence :: HasCallStack => Property
prop_epochIsReadOnlyErrorEquivalence = monadicIO $ do
    run $ apiEquivalence (runExceptT $ do
                             ExceptT $ withDB ["demo"] 0 $ \db -> runExceptT $ do
                               ExceptT $ appendBinaryBlob db (0, RelativeSlot 0)
                                                             (BL.lazyByteString . C8.pack $ "test")
                               ExceptT $ appendBinaryBlob db (1, RelativeSlot 0)
                                                             (BL.lazyByteString . C8.pack $ "haskell")
                             -- The second withDB should fail.
                             ExceptT $ withDB ["demo"] 0 $ \_ -> return $ Right ()
                             return ()
                         ) sameDBError prettyImmutableDBError
