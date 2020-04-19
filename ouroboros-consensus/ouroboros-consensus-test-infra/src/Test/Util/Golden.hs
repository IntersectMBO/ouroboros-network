{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Golden
  ( goldenTestCBOR
  , goldenTestCBORInCBOR
  ) where

import           Data.List (intercalate)
import           Data.TreeDiff (ToExpr, ansiWlEditExpr, ediff)
import qualified Data.ByteString as BS

import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..))
import qualified Codec.CBOR.FlatTerm as CBOR
import qualified Codec.CBOR.Read     as CBOR

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import qualified Control.Monad.ST.Lazy as ST.Lazy

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
    actualFlatTerm = CBOR.toFlatTerm $ enc a

    msg
      | null expectedFlatTerm
      = "Running with an empty expected value, actual value is:\n" <>
        pprintList actualFlatTerm
      | otherwise
      = "Expected term /= actual term, diff expected actual:\n" <>
        show (ansiWlEditExpr (ediff expectedFlatTerm actualFlatTerm))

-- | A variant on 'goldenTestCBOR' for use when the encoding uses the
-- CBOR-in-CBOR wrapping. If we don't use this, the examples and the
-- diffs become unreadable.
--
goldenTestCBORInCBOR
  :: HasCallStack
  => (a -> Encoding) -- ^ Encoder
  -> a               -- ^ Example value to use for the golden test
  -> FlatTerm        -- ^ Expected CBOR (flat) encoding
  -> Assertion
goldenTestCBORInCBOR enc a expectedFlatTerm =
    assertBool msg (actualFlatTerm == expectedFlatTerm)
  where
    actualFlatTerm =
      case CBOR.toFlatTerm $ enc a of
        [TkTag 24, TkBytes b] -> decodePreEncoded b
        tks                   -> tks

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


decodePreEncoded :: BS.ByteString -> FlatTerm
decodePreEncoded bs0 =
    ST.Lazy.runST (provideInput bs0)
  where
    provideInput :: BS.ByteString -> ST.Lazy.ST s FlatTerm
    provideInput bs
      | BS.null bs = return []
      | otherwise  = do
          next <- ST.Lazy.strictToLazyST $ do
              -- This will always be a 'Partial' here because decodeTermToken
              -- always starts by requesting initial input. Only decoders that
              -- fail or return a value without looking at their input can give
              -- a different initial result.
              CBOR.Partial k <- CBOR.deserialiseIncremental CBOR.decodeTermToken
              k (Just bs)
          collectOutput next

    collectOutput :: CBOR.IDecode s TermToken -> ST.Lazy.ST s FlatTerm
    collectOutput (CBOR.Fail _ _ err) = fail $ "toFlatTerm: encodePreEncoded "
                                            ++ "used with invalid CBOR: "
                                            ++ show err
    collectOutput (CBOR.Partial    k) = ST.Lazy.strictToLazyST (k Nothing)
                                        >>= collectOutput
    collectOutput (CBOR.Done bs' _ x) = do xs <- provideInput bs'
                                           return (x : xs)

