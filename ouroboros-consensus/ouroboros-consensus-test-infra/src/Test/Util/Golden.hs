{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Golden
  ( CanonicalTestable(..)
  , goldenTestCBOR
  , goldenTestCBORInCBOR
  , goldenTestCBORCanonicalAll
  ) where

import qualified Data.ByteString as BS

import           Cardano.Prelude (CanonicalExamples, CanonicalExamplesSized,
                     Args, Proxy (..), getCanonicalExamplesSized,
                     unsafeGetCanonicalExamples)

import           Cardano.Binary (Encoding, ToCBOR(..))
import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..), toFlatTerm)
import           Data.List (intercalate)
import           Data.TreeDiff (ToExpr, ansiWlEditExpr, ediff)

import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..))
import qualified Codec.CBOR.FlatTerm as CBOR
import qualified Codec.CBOR.Read as CBOR

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import qualified Control.Monad.ST.Lazy as ST.Lazy

import qualified System.IO as IO
import           Test.Tasty.HUnit
import           Text.Show.Pretty
import Codec.CBOR.FlatTerm (FlatTerm, toFlatTerm)


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

goldenTestCBORCanonicalAll
  :: HasCallStack
  => Maybe FilePath
  -> [CanonicalTestable]
  -> [TypeTerm]
  -> Assertion
goldenTestCBORCanonicalAll path types expected =
    if termsLen > typesLen
    then assertFailure $ "received" ++ show (termsLen - typesLen) ++ "more terms than types"
    else
      if actual == expected then
        return ()
      else case (expected, path) of
        ([], Nothing) -> assertFailure $ unlines
            ["━━━ Copy Paste ━━━", ppShow actual]
        ([], Just file) -> do
            IO.withFile file IO.ReadWriteMode $ \h ->
              IO.hPutStr h (ppShow actual)
            assertFailure $ Prelude.unlines
              ["failed: see file ", show file]
        _ ->
            assertBool msg (actual == expected)

  where
    typesLen = length types
    termsLen = length expected

    actual   = fmap encodeCanonical types

    msg = "Running with an empty expected value, actual value is:\n" <>
        pprintList actual


-- | TODO(kde) Find a better place for this. Duplicated code with cardano-base.
newtype TypeTerm = TypeTerm [FlatTerm]
    deriving (Show, Eq)

data CanonicalTestable where
  WithCBOR :: forall a. (ToCBOR a, CanonicalExamples a) =>
      Proxy a -> CanonicalTestable
  Explicit :: forall a. CanonicalExamples a =>
      (a -> Encoding) -> CanonicalTestable
  WithCBORSized :: forall a. (ToCBOR a, CanonicalExamplesSized a) =>
      Proxy a -> Args -> CanonicalTestable
  ExplicitSized :: forall a. CanonicalExamplesSized a =>
      (a -> Encoding) -> Args -> CanonicalTestable

encodeCanonical :: CanonicalTestable -> TypeTerm
encodeCanonical (WithCBOR (Proxy :: Proxy a)) = TypeTerm $
    toFlatTerm . toCBOR <$> (unsafeGetCanonicalExamples :: [a])
encodeCanonical (Explicit enc) = TypeTerm $
    toFlatTerm . enc <$> unsafeGetCanonicalExamples
encodeCanonical (WithCBORSized (Proxy :: Proxy a) args) = TypeTerm $
    toFlatTerm . toCBOR <$> (getCanonicalExamplesSized args :: [a])
encodeCanonical (ExplicitSized enc args) = TypeTerm $
    toFlatTerm . enc <$> getCanonicalExamplesSized args
