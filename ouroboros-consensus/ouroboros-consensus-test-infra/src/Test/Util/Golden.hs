{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Golden
  ( goldenTestCBOR
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..), toFlatTerm)
import           Data.List (intercalate)
import           Data.TreeDiff (ToExpr, ansiWlEditExpr, ediff)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Test.Tasty.HUnit

-- | Test whether the encoding of the given example value matches the expected
-- encoding, given as a 'FlatTerm'.
--
-- Hint: when writing the test, just pass an empty expected 'FlatTerm', a
-- pastable one will be printed.
goldenTestCBOR
  :: HasCallStack
  => (a -> Encoding) -- ^ Encoder
  -> a               -- ^ Example value to use for the golden test
  -> FlatTerm        -- ^ Expected CBOR (flat) encoding
  -> Assertion
goldenTestCBOR enc a expectedFlatTerm =
    assertBool msg (actualFlatTerm == expectedFlatTerm)
  where
    actualFlatTerm = toFlatTerm $ enc a

    msg
      | null expectedFlatTerm
      = "Running with an empty expected value, actual value is:\n" <>
        pprintList actualFlatTerm
      | otherwise
      = "Expected term /= actual term, diff expected actual:\n" <>
        show (ansiWlEditExpr (ediff expectedFlatTerm actualFlatTerm))

-- | Print a list of elements as follows:
--
-- > [ <first>
-- > , <second>
-- > , ..
-- > ]
--
-- unless the list is empty, then print "[]"
pprintList :: Show a => [a] -> String
pprintList [] = "[]"
pprintList (x:ys) = intercalate "\n" $
    [ "[ " <> show x ] <>
    [ ", " <> show y | y <- ys ] <>
    [ "]" ]

deriving instance Generic TermToken
deriving instance ToExpr TermToken
