{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Util.Tasty.Traceable where
import qualified Test.Tasty.Options as Tasty
import Test.Tasty.Providers
import Data.Proxy
import Test.Tasty.Options
import Data.Tagged
import Data.Coerce
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.QuickCheck (QC(QC))

newtype ShowTrace = ShowTrace Bool
  deriving stock (Eq, Show)

instance Tasty.IsOption ShowTrace where
  defaultValue = coerce False
  parseValue = coerce safeReadBool
  optionName = coerce "trace"
  optionHelp = coerce "Use a Control.Tracer.stdoutTracer where possible"

newtype TraceableTest t = TraceableTest t

instance IsTest t => IsTest (TraceableTest t) where
  testOptions = coerce $ coerce (testOptions :: Tagged t [OptionDescription])
    ++ [Option (Proxy :: Proxy ShowTrace)]

  run os (TraceableTest tree) = run os tree

traceableProperty :: QC.Testable t => String -> (ShowTrace -> t) -> TestTree
traceableProperty name mk_prop = askOption $ \show_trace ->
  singleTest name (TraceableTest (QC $ QC.property $ mk_prop show_trace))
