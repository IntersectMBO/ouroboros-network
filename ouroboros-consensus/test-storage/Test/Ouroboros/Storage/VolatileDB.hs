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

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy.Char8 as C8
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)
import           Test.Ouroboros.Storage.Util
import qualified Test.Ouroboros.Storage.VolatileDB.StateMachine as StateMachine
import           Test.Ouroboros.Storage.VolatileDB.TestBlock

tests :: HasCallStack => TestTree
tests = testGroup "VolatileDB"
  [
    testProperty "list parsing round trips" prop_roundTrips
  , testProperty "Invalid argument" prop_VolatileInvalidArg
  , StateMachine.tests
  ]

prop_roundTrips :: HasCallStack => [BlockInfo] -> Property
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
