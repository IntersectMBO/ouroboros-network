{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Ouroboros.Storage.VolatileDB (tests) where

import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import qualified Data.Binary as Binary
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List (nub)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Serialize
import qualified Data.Set as Set
import qualified System.IO as IO
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Util (SomePair (..))

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import           Ouroboros.Storage.VolatileDB.Impl (openDB)
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)
import           Ouroboros.Storage.VolatileDB.Util (modifyTMVar)
import           Test.Ouroboros.Storage.Util
import qualified Test.Ouroboros.Storage.VolatileDB.StateMachine as StateMachine

tests :: HasCallStack => TestTree
tests = testGroup "VolatileDB"
  [
    testProperty "list parsing round trips" prop_roundTrips
  , testProperty "put/get roundtrips" prop_VolatileRoundTrips
  , testProperty "reOpen" prop_VolatileReOpenState
  , testProperty "reOpen and Write" prop_VolatileReOpenWrite
  , testProperty "garbage collect" prop_VolatileGarbageCollect
  , testProperty "garbage collect state" prop_VolatileGarbageState
  , testProperty "remove empty file to Write" prop_VolatileReopenRemoveEmptyFile
  , testProperty "no slot returns Nothing" prop_VolatileNoSlot
  , testProperty "duplicated slot" prop_VolatileDuplicatedSlot
  , testProperty "decode failed" prop_VolatileDecodeFail
  , testProperty "undisputable lookup" prop_VolatileUndisputableLookup
  , testProperty "undisputable lookup with closed handle" prop_VolatileUndisputableLookupClose
  , testProperty "Invalid argument" prop_VolatileInvalidArg
  , testProperty "Invalid filename" prop_VolatileParseFileNameError
  , testProperty "lenient on number of blocks" prop_VolatileLessThanN
  , testProperty "first tries to fill files" prop_VolatileFillFilesFirst
  , testProperty "can open with different number of blocks per file" prop_VolatileDifferentNumBlocks
  , StateMachine.tests
  ]

withTestDB :: (HasCallStack, MonadSTM m, MonadCatch m)
           => HasFS m h
           -> ErrorHandling (VolatileDBError MyBlockId) m
           -> Parser m MyBlockId
           -> Int
           -> (VolatileDB MyBlockId m -> m a)
           -> m a
withTestDB hasFS err parser n = withDB (openDB hasFS err parser n toSlot)

prop_roundTrips :: [MyBlockId] -> Property
prop_roundTrips ls = property $ (binarySize * (length ls), ls) === (fromIntegral $ C8.length bs, ls')
    where
        bs = C8.concat $ Binary.encode . toBlock <$> ls
        ls' = go bs
        go bs' =
            if C8.length bs' == 0 then []
            else
                let (bs1, bsRest) = C8.splitAt (fromIntegral binarySize) bs'
                    sl = fromBlock $ Binary.decode bs1
                in sl : go bsRest

-- Write some blocks and then try to get them back.
prop_VolatileRoundTrips :: HasCallStack => [MyBlockId] -> Property
prop_VolatileRoundTrips ls = monadicIO $ do
    let expected :: [Either String MyBlockId] = (Right <$> ls)
    run $ apiEquivalenceVolDB (expectVolDBResult (@?= expected)) (\hasFS err ->
        withTestDB hasFS err myParser 5 $ \db -> do
            putList db ls
            bss <- forM ls $ \slot -> getBlock db slot
            return $ (fmap fromBlock) <$> decode <$> catMaybes bss
        )

-- Check if the db works normaly after reopening.
prop_VolatileReOpenWrite :: HasCallStack => [MyBlockId] -> [MyBlockId] -> Property
prop_VolatileReOpenWrite ls1 ls2 = monadicIO $ do
    let expected :: [Either String MyBlockId] = Right <$> (ls1 <> ls2)
    run $ apiEquivalenceVolDB (expectVolDBResult (@?= expected)) (\hasFS err -> do
                db <- openDB hasFS err myParser 5 toSlot
                putList db ls1
                closeDB db
                reOpenDB db
                putList db ls2
                ret <- forM (ls1 <> ls2) $ \slot -> getBlock db slot
                closeDB db
                return $ (fmap fromBlock) <$> decode <$> catMaybes ret
        )

-- Check state equality after reopening.
prop_VolatileReOpenState :: HasCallStack => [MyBlockId] -> Property
prop_VolatileReOpenState ls = monadicIO $ do
        run $ apiEquivalenceVolDB (expectVolDBResult (@?= True)) (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser 5 toSlot
            putList db ls
            SomePair _stHasFS1 st1 <- Internal.getInternalState env
            let mp1 :: (Index MyBlockId) = Internal._currentMap st1
            closeDB db
            reOpenDB db
            SomePair _stHasFS2 st2 <- Internal.getInternalState env
            let mp2 = Internal._currentMap st2
            closeDB db
            return $ mp1 == mp2
            )

-- Check if db succesfully garbage-collects.
prop_VolatileGarbageCollect :: HasCallStack => MyBlockId -> [MyBlockId] -> [MyBlockId] -> Property
prop_VolatileGarbageCollect special ls1 ls2 = monadicIO $ do
    let blocksPerFile = 5
    let specialEnc = Binary.encode $ toBlock special
    let st1 = Set.filter (/= special) $ Set.fromList ls1
    let ls1' = (\sl -> (sl, Binary.encode $ toBlock sl)) <$> Set.toList st1
    let st2 = Set.filter (\sl -> sl /= special && not (sl `Set.member` st1)) $ Set.fromList ls2
    let ls2' = (\sl -> (sl, Binary.encode $ toBlock sl)) <$> Set.toList st2
    let ls' = ls1' <> [(special, specialEnc)] <> ls2'
    let lss = chunk blocksPerFile ls'
    let maximumM :: [(MyBlockId, C8.ByteString)] -> Bool
        maximumM [] = False
        maximumM ls = fst (maximum ls) >= special
    let lss' = filter maximumM lss
    let ls'' = concat lss'
    let expected :: [Either String MyBlockId] = Right . fst <$> ls''
    run $ apiEquivalenceVolDB (expectVolDBResult (@?= expected)) (\hasFS err ->
            withTestDB hasFS err myParser blocksPerFile $ \db -> do
                forM_ ls' $ \(slot, enc) -> putBlock db slot (BL.lazyByteString enc)
                garbageCollect db (toSlot special)
                mBss <- sequence <$> (forM ls' $ \(slot, _) -> EH.try err $ getBlock db slot)
                case mBss of
                    Left e -> throwError err e
                    Right bss -> return $ (fmap fromBlock) <$> decode <$> catMaybes bss
        )

-- split at regular intervals
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
    where
        (y1, y2) = splitAt n xs

-- Check if the db can close and reopen after garbage collection.
prop_VolatileGarbageState :: HasCallStack => MyBlockId -> [MyBlockId] -> [MyBlockId] -> Property
prop_VolatileGarbageState special ls1 ls2 = withMaxSuccess 20 $ monadicIO $ do
        let blocksPerFile = 5
        let specialEnc = Binary.encode $ toBlock special
        let set1 = Set.filter (/= special) $ Set.fromList ls1
        let ls1' = (\sl -> (sl, Binary.encode $ toBlock sl)) <$> Set.toList set1
        let set2 = Set.filter (\sl -> sl /= special && not (sl `Set.member` set1)) $ Set.fromList ls2
        let ls2' = (\sl -> (sl, Binary.encode $ toBlock sl)) <$> Set.toList set2
        let ls' = ls1' <> [(special, specialEnc)] <> ls2'
        run $ apiEquivalenceVolDB  (expectVolDBResult (@?= True)) (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser blocksPerFile toSlot
            forM_ ls' $ \(slot, enc) -> putBlock db slot (BL.lazyByteString enc)
            garbageCollect db (toSlot special)
            SomePair _stHasFS1 st1 <- Internal.getInternalState env
            closeDB db
            reOpenDB db
            SomePair _stHasFS2 st2 <- Internal.getInternalState env
            closeDB db
            return $ Internal.sameInternalState st1 st2
            )

-- Try to reopen the db after deleting an empty write file.
prop_VolatileReopenRemoveEmptyFile :: HasCallStack => [MyBlockId] -> Property
prop_VolatileReopenRemoveEmptyFile ls = monadicIO $ do
    let blocksPerFile = 5
    let ls' = nub ls
    let len = length ls'
    let ls'' = take (len - mod len blocksPerFile) ls'
    run $ apiEquivalenceVolDB (expectVolDBResult (@?= (Just(Nothing, 0, M.empty), True))) (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser 5 toSlot
            putList db ls''
            SomePair _stHasFS1 st1 <- Internal.getInternalState env
            closeDB db
            let path = Internal._currentWritePath st1
            let fileStats = M.lookup path (Internal._currentMap st1)
            removeFile hasFS [path]
            reOpenDB db
            SomePair _stHasFS2 st2 <- Internal.getInternalState env
            return (fileStats, Internal.sameInternalState st1 st2)
        )

-- Trying to get a block that does not exist should return Nothing
prop_VolatileNoSlot :: HasCallStack => [MyBlockId] -> MyBlockId -> Property
prop_VolatileNoSlot ls special = monadicIO $ do
        let ls' = (\sl -> (sl, Binary.encode $ toBlock sl)) <$> (filter (/= special) ls)
        run $ apiEquivalenceVolDB (expectVolDBResult (@?= Nothing)) (\hasFS err ->
            withTestDB hasFS err myParser 5 $ \db -> do
                forM_ ls' $ \(slot, enc) -> putBlock db slot (BL.lazyByteString enc)
                getBlock db special
            )

-- Intentionally corrupt a file of the db, to check if we get DuplicatedSlot.
-- Trying to parse a file with duplicated blockId, should throw an error.
prop_VolatileDuplicatedSlot :: HasCallStack => [MyBlockId] -> MyBlockId -> Property
prop_VolatileDuplicatedSlot ls special = mod (1 + length (nub ls)) 5 /= 0 ==> monadicIO $ do
    let specialEnc = Binary.encode $ toBlock special
    let fExpected = \case
            Right (False, Left (VParserError (DuplicatedSlot _))) -> return ()
            somethingElse -> fail $ "IO return " <> show somethingElse <> " instead of DuplicatedSlot"
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser 5 toSlot
            putBlock db special (BL.lazyByteString specialEnc)
            putList db ls
            -- here we intentionally corrupt the fs, so that we can get the wanted error.
            SomePair stHasFS st <- Internal.getInternalState env
            let hndl = Internal._currentWriteHandle st
            _ <- hPut stHasFS hndl (BL.lazyByteString specialEnc)
            closeDB db
            expectedErr <- EH.try err $ reOpenDB db
            -- we also check if db closes in case of errors.
            isOpen <- isOpenDB db
            -- try to close, to make sure we clean-up, even if tests fail.
            -- trying to close a closed db is a no-op.
            closeDB db
            return (isOpen, expectedErr)
        )

-- Intentionally corrupt a file of the db, to check if we get DecodeFail.
prop_VolatileDecodeFail :: HasCallStack => [MyBlockId] -> Property
prop_VolatileDecodeFail ls = monadicIO $ do
    let fExpected = \case
            Right (False, Left (VParserError (DecodeFailed _str n))) -> n @?= 3
            somethingElse -> fail $ "IO failed with " <> show somethingElse <> " instead of DecodeFailed"
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser 5 toSlot
            putList db ls
            -- here we intentionally corrupt the fs, so that we can get the wanted error.
            SomePair stHasFS st <- Internal.getInternalState env
            let hndl = Internal._currentWriteHandle st
            _ <- hPut stHasFS hndl (BL.lazyByteString $ C8.pack "123")
            closeDB db
            expectedErr <- EH.try err $ reOpenDB db
            -- we also check if db closes in case of errors.
            isOpen <- isOpenDB db
            closeDB db
            return (isOpen, expectedErr)
        )

-- Intentially corrupt the Internal State of the db, to check if we get UndisputableLookupError.
prop_VolatileUndisputableLookup :: HasCallStack => [MyBlockId] -> Property
prop_VolatileUndisputableLookup ls = mod (length (nub ls)) 5 /= 0 ==> monadicIO $ do
    let ls' = nub ls
    let tl = last ls'
    let fExpected = \case
            Right (False, Left (UndisputableLookupError _path _mp)) -> return ()
            somethingElse -> fail $ "IO failed with " <> show somethingElse <> " instead of UndisputableLookupError"
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser 5 toSlot
            putList db ls'
            -- here we intentionally corrupt the state, so that we can get the wanted error
            case env of
              Internal.VolatileDBEnv{..} ->
                modifyTMVar _dbInternalState  $ \mbSt -> case mbSt of
                    Nothing -> throwError err ClosedDBError
                    Just st -> return $ (Just $ st {
                                      Internal._currentMap = M.delete (Internal._currentWritePath st) (Internal._currentMap st)
                                    , Internal._currentRevMap = M.delete tl (Internal._currentRevMap st)
                                    }, ())
            expectedErr <- EH.try err $ putBlock db tl (BL.lazyByteString $ Binary.encode $ toBlock tl)
            isOpen <- isOpenDB db
            closeDB db
            return (isOpen, expectedErr)
        )

-- Like above but we also close the write handle. On exceptions, the cleanup code closes the
-- write handles, so it is interesting to check what happens if the handle is already closed.
prop_VolatileUndisputableLookupClose :: HasCallStack => [MyBlockId] -> Property
prop_VolatileUndisputableLookupClose ls = mod (length (nub ls)) 5 /= 0 ==> monadicIO $ do
    let ls' = nub ls
    let tl = last ls'
    let fExpected = \case
            Right (False, Left (UndisputableLookupError _path _mp)) -> return ()
            somethingElse -> fail $ "IO returned " <> show somethingElse <> " instead of UndisputableLookupError"
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser 5 toSlot
            putList db ls'
            -- here we intentionally corrupt the state, so that we can get the wanted error
            case env of
              Internal.VolatileDBEnv{..} ->
                modifyTMVar _dbInternalState $ \mbSt -> case mbSt of
                    Nothing -> throwError err ClosedDBError
                    Just st -> do
                        hClose _dbHasFS (Internal._currentWriteHandle st)
                        return $ (Just $ st {
                                      Internal._currentMap = M.delete (Internal._currentWritePath st) (Internal._currentMap st)
                                    , Internal._currentRevMap = M.delete tl (Internal._currentRevMap st)
                                    }, ())
            expectedErr <- EH.try err $ putBlock db tl (BL.lazyByteString $ Binary.encode $ toBlock tl)
            isOpen <- isOpenDB db
            closeDB db
            return (isOpen, expectedErr)
        )

prop_VolatileInvalidArg :: HasCallStack => Property
prop_VolatileInvalidArg = monadicIO $ do
    let fExpected = \case
            Left (InvalidArgumentsError _str) -> return ()
            somethingElse -> fail $ "IO returned " <> show somethingElse <> " instead of InvalidArgumentsError"
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            _ <- Internal.openDBFull hasFS err myParser 0 toSlot
            return ()
        )

-- Create a new file in the db folder with invalid name and check if we get
-- the expected error.
prop_VolatileParseFileNameError :: HasCallStack => Property
prop_VolatileParseFileNameError = monadicIO $ do
    let fExpected = \case
            Right (False, Left (VParserError (InvalidFilename str))) -> str @?= "invalid-file-name"
            somethingElse -> fail $ "IO failed with " <> show somethingElse <> " instead of InvalidFilename"
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            (db, _env) <- Internal.openDBFull hasFS err myParser 5 toSlot
            closeDB db
            withFile hasFS ["invalid-file-name"] IO.AppendMode $ \_hndl -> do
                return ()
            expectedErr <- EH.try err $ reOpenDB db
            isOpen <- isOpenDB db
            closeDB db
            return (isOpen, expectedErr)
        )

-- Intentiolly create a file with less blocks than needed and check if we can
-- succesfully reOpen db.
prop_VolatileLessThanN :: HasCallStack => [MyBlockId] -> Property
prop_VolatileLessThanN ls = monadicIO $ do
    let ls' = (\sl -> (sl, Binary.encode $ toBlock sl)) <$> nub ls
    run $ apiEquivalenceVolDB (expectVolDBResult (@?=(True,Right ()))) (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser 5 toSlot
            forM_ ls' $ \(slot, enc) -> putBlock db slot (BL.lazyByteString enc)
            SomePair _stHasFS st <- Internal.getInternalState env
            let nextFd = Internal._nextNewFileId st
            let path = Internal.filePath nextFd
            closeDB db
            withFile hasFS [path] IO.AppendMode $ \_hndl -> do
                return ()
            expectedErr <- EH.try err $ reOpenDB db
            isOpen <- isOpenDB db
            closeDB db
            return (isOpen, expectedErr)
        )

prop_VolatileFillFilesFirst :: HasCallStack => [MyBlockId] -> Property
prop_VolatileFillFilesFirst ls = (length (nub ls)) > 3 ==> monadicIO $ do
    let nubLs = nub ls
    let (ls1, ls2) = splitAt (length nubLs - 3) $ nubLs
    let fExpected = \case
            Right (Just (Nothing, 0, mp), [], True, path, [(p,s)]) -> (p,s, mp) @?= (path,0, M.empty)
            somethingelse -> fail $ "failed with " <> show somethingelse
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            (db, env) <- Internal.openDBFull hasFS err myParser 3 toSlot
            putList db ls1
            st1 <- Internal.getInternalState err env
            let nextFd = Internal._nextNewFileId st1
            let path = Internal.filePath nextFd
            closeDB db
            withFile hasFS [path] IO.AppendMode $ \_hndl -> do
                return ()
            reOpenDB db
            st2 <- Internal.getInternalState err env
            putList db ls2
            st3 <- Internal.getInternalState err env
            let fileEntry = M.lookup path (Internal._currentMap st2)
            let bl = Internal._nextNewFileId st2 == Internal._nextNewFileId st1 + 1
            return (fileEntry, Internal._nextWriteFiles st3, bl, path, Internal._nextWriteFiles st2)
        )

prop_VolatileDifferentNumBlocks :: HasCallStack
                                => [MyBlockId] -> [MyBlockId] -> [MyBlockId]
                                -> Int -> Int -> Int
                                -> Property
prop_VolatileDifferentNumBlocks ls1 ls2 ls3 a b c =
    a < 500 && b < 500 && c < 500 && a > 0 && b > 0 && c > 0
    ==> monadicIO $ do
    run $ apiEquivalenceVolDB (expectVolDBResult (@?=())) (\hasFS err -> do
            (db1, _env) <- Internal.openDBFull hasFS err myParser a toSlot
            putList db1 ls1
            closeDB db1
            (db2, _env) <- Internal.openDBFull hasFS err myParser a toSlot
            putList db2 ls2
            closeDB db2
            (db3, _env) <- Internal.openDBFull hasFS err myParser a toSlot
            putList db3 ls3
            closeDB db3
        )

putList :: Monad m
        => VolatileDB MyBlockId m
        -> [MyBlockId]
        -> m ()
putList db ls =
    forM_ ls $ \bid -> putBlock db bid (BL.lazyByteString . Binary.encode . toBlock $ bid)
