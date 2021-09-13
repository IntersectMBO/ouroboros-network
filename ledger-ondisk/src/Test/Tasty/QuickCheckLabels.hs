-- |

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Test.Tasty.QuickCheckLabels
  ( module Test.Tasty.QuickCheckLabels
  , module X
  )
  where


import qualified Test.Tasty.QuickCheck as TastyQC
import Test.Tasty.QuickCheck as X hiding (testProperty, testProperties)
import Test.Tasty.Providers
import Data.Tagged
import Test.Tasty.Options
import Data.Proxy
import qualified Test.QuickCheck as QC
import Data.Coerce
import Test.Tasty.Runners
import Data.List (isSuffixOf)
import Test.Tasty

testProperty :: forall a. QC.Testable a => TestName -> a -> TestTree
testProperty name prop = singleTest name . QuickCheckWithLabels . TastyQC.QC $ QC.property prop

testProperties :: TestName -> [(String, QC.Property)] -> TestTree
testProperties name = testGroup name . map (uncurry testProperty)

newtype QuickCheckWithLabels = QuickCheckWithLabels TastyQC.QC

newtype ShowLabels = ShowLabels Bool

instance IsOption ShowLabels where
  defaultValue = coerce False
  parseValue = coerce safeReadBool
  optionName = coerce "quickcheck-show-labels"
  optionHelp = coerce "TODO"


instance IsTest QuickCheckWithLabels where
  testOptions = let
    Tagged qcopts :: Tagged TastyQC.QC [OptionDescription ] = testOptions
    in Tagged $ qcopts <> [Option (Proxy :: Proxy ShowLabels)]

  run opts (QuickCheckWithLabels (TastyQC.QC prop)) yieldProgress = do
    (_replaySeed, args) <- TastyQC.optionSetToArgs opts

    let
      ShowLabels   showLabels = lookupOption opts

    -- Quickcheck already catches exceptions, no need to do it here.
    if showLabels then do
        r <- QC.labelledExamplesWithResult args {QC.chatty = True} prop

        print r
        qcOutput <- formatMessage $ QC.output r
        let qcOutputNl =
                if "\n" `isSuffixOf` qcOutput
                then qcOutput
                else qcOutput ++ "\n"
            testSuccessful = case r of
              QC.Success {} -> True
              _ -> False
        return $ (if testSuccessful then testPassed else testFailed) (qcOutputNl ++ "")
    else run opts (TastyQC.QC prop) yieldProgress
