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

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)
import           Test.Ouroboros.Storage.Util
import qualified Test.Ouroboros.Storage.VolatileDB.StateMachine as StateMachine

tests :: HasCallStack => TestTree
tests = testGroup "VolatileDB"
  [ testProperty "Invalid argument" prop_VolatileInvalidArg
  , StateMachine.tests
  ]

prop_VolatileInvalidArg :: HasCallStack => Property
prop_VolatileInvalidArg = monadicIO $
    run $ apiEquivalenceVolDB fExpected (\hasFS err -> do
            _ <- Internal.openDBFull hasFS err (EH.throwCantCatch EH.monadCatch) dummyParser 0
            return ()
        )
  where
    dummyParser :: Parser String IO Int
    dummyParser = Parser {
        parse = \_ -> return ([], Nothing)
        }
    fExpected = \case
      Left (UserError (InvalidArgumentsError _str)) -> return ()
      somethingElse -> fail $ "IO returned " <> show somethingElse <> " instead of InvalidArgumentsError"
