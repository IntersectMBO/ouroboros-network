{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines a @tasty@ property that has the option to show a trace.
--
-- The @--show-trace@ command-line argument is used to debug tests by
-- showing a trace of properties that are defined using
-- @`traceableProperty`@. Including the @--show-trace@ argument will signal
-- properties to show /all/ traces regardless of whether tests fail,
-- while excluding the argument will show /none/ at all.
--
-- Todo(jdral): A possibility for improvement would be to show the trace
-- only when the test fails, instead of showing all traces or none at all.
module Test.Util.Tasty.Traceable (
    ShowTrace (..)
  , traceableProperty
  ) where

import           Data.Coerce
import           Data.Proxy
import           Data.Tagged

import           Test.Tasty.Options
import           Test.Tasty.Providers

import qualified Test.QuickCheck as QC
import           Test.Tasty
import           Test.Tasty.QuickCheck (QC (QC))

newtype ShowTrace = ShowTrace Bool
  deriving stock (Eq, Show)

instance IsOption ShowTrace where
  defaultValue = coerce False
  parseValue = coerce safeReadBool
  optionName = coerce "show-trace"
  optionHelp = coerce $
    "This option allows the user to specify if a property should show a trace."
    <> " It is up to the property to choose how to act on this flag."
  optionCLParser = mkFlagCLParser mempty (coerce True)

newtype TraceableTest t = TraceableTest t

instance IsTest t => IsTest (TraceableTest t) where
  testOptions = coerce $ coerce (testOptions :: Tagged t [OptionDescription])
    ++ [Option (Proxy :: Proxy ShowTrace)]

  run os (TraceableTest tree) = run os tree

-- | Create a traceable property that can be tested using the @tasty@ package.
--
-- Note: It is up to the underlying property itself (@mk_prop@ in
-- @`traceableProperty name mk_prop`@) to choose how to act on the flag. For
-- example: (i) the underlying property can choose to ignore the flag entirely,
-- and (ii) the underlying property can choose which concrete tracer to use.
traceableProperty :: QC.Testable t => String -> (ShowTrace -> t) -> TestTree
traceableProperty name mkProp = askOption $ \showTrace ->
  singleTest name (TraceableTest (QC $ QC.property $ mkProp showTrace))
