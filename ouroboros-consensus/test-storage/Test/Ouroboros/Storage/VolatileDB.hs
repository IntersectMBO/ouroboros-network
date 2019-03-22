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
import           Data.Maybe (catMaybes)
import           Data.Serialize
import qualified Data.Set as Set
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import           Ouroboros.Storage.VolatileDB.Impl
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)
import           Test.Ouroboros.Storage.Util
import qualified Test.Ouroboros.Storage.VolatileDB.StateMachine as StateMachine
import           Test.Ouroboros.Storage.VolatileDB.TestBlock

withTestDB :: (HasCallStack, MonadSTM m, MonadMask m)
           => HasFS m h
           -> ErrorHandling (VolatileDBError BlockId) m
           -> Parser (ParserError BlockId) m BlockId
           -> Int
           -> (VolatileDB BlockId m -> m a)
           -> m a
withTestDB hasFS err parser n = withDB (openDB hasFS err parser n toSlot)

tests :: HasCallStack => TestTree
tests = testGroup "VolatileDB"
  [
    testProperty "list parsing round trips" prop_roundTrips
  , testProperty "Invalid argument" prop_VolatileInvalidArg
  , testProperty "garbage collect" prop_VolatileGarbageCollect
  , StateMachine.tests
  ]

prop_roundTrips :: HasCallStack => [BlockId] -> Property
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

prop_VolatileInvalidArg :: HasCallStack => Property
prop_VolatileInvalidArg = monadicIO $ do
    let fExpected = \case
            Left (InvalidArgumentsError _str) -> return ()
            somethingElse -> fail $ "IO returned " <> show somethingElse <> " instead of InvalidArgumentsError"
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            _ <- Internal.openDBFull hasFS err (myParser hasFS err) 0 toSlot
            return ()
        )

-- Check if db succesfully garbage-collects.
-- This can't be replaces by q-s-m because it's a precondition/limitation.
prop_VolatileGarbageCollect :: HasCallStack => BlockId -> [BlockId] -> [BlockId] -> Property
prop_VolatileGarbageCollect special ls1 ls2 = monadicIO $ do
    let blocksPerFile = 5
    let specialEnc = Binary.encode $ toBlock special
    let st1 = Set.filter (/= special) $ Set.fromList ls1
    let ls1' = (\sl -> (sl, Binary.encode $ toBlock sl)) <$> Set.toList st1
    let st2 = Set.filter (\sl -> sl /= special && not (sl `Set.member` st1)) $ Set.fromList ls2
    let ls2' = (\sl -> (sl, Binary.encode $ toBlock sl)) <$> Set.toList st2
    let ls' = ls1' <> [(special, specialEnc)] <> ls2'
    let lss = chunk blocksPerFile ls'
    let maximumM :: [(BlockId, C8.ByteString)] -> Bool
        maximumM [] = False
        maximumM ls = fst (maximum ls) >= special
    let lss' = filter maximumM lss
    let ls'' = concat lss'
    let expected :: [Either String BlockId] = Right . fst <$> ls''
    run $ apiEquivalenceVolDB (expectVolDBResult (@?= expected)) (\hasFS err ->
            withTestDB hasFS err (myParser hasFS err) blocksPerFile $ \db -> do
                forM_ ls' $ \(slot, enc) -> putBlock db slot (BL.lazyByteString enc)
                garbageCollect db (toSlot special)
                mBss <- sequence <$> (forM ls' $ \(slot, _) -> EH.try err $ getBlock db slot)
                case mBss of
                    Left e -> EH.throwError err e
                    Right bss -> return $ (fmap fromBlock) <$> decode <$> catMaybes bss
        )

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
    where
        (y1, y2) = splitAt n xs
