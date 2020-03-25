module Test.Util.Golden
  ( goldenTestCBOR
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base58 as B58
import           Data.List (intercalate, uncons)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Tuple (swap)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Util (chunks)

import           Test.Tasty.HUnit

-- | Test whether the encoding of the given example value matches the expected
-- binary encoding, given as a bytestring in Base58.
--
-- Hint: when writing the test, just pass an empty expected string.
goldenTestCBOR
  :: HasCallStack
  => (a -> Encoding)    -- ^ Encoder
  -> a                  -- ^ Example value to use for the golden test
  -> Strict.ByteString  -- ^ Expected binary encoding, in Base58
  -> Assertion
goldenTestCBOR enc a expectedBytesB58 =
    assertBool msg (actualBytes == expectedBytes)
  where
    encodeB58 = B58.encodeBase58 B58.bitcoinAlphabet
    decodeB58 = B58.decodeBase58 B58.bitcoinAlphabet

    actualBytes = toStrictByteString (enc a)
    expectedBytes = case decodeB58 expectedBytesB58 of
      Just decoded -> decoded
      Nothing      -> error "expected: invalid base58 bytestring"

    msg = unlines
      [ "Expected bytes:"
      , formatPastable maxLineWidth expectedBytesB58
      , "but got:"
      , formatPastable maxLineWidth (encodeB58 actualBytes)
      ]
    maxLineWidth = 76

-- | Format a bytestring so that it is pastable into a file and that each line
-- is at most the given number of characters.
--
-- PRECONDITION: max line width > 2
--
-- > formatPastable 4 "0123456789"
-- > "\"01\\\n\\23\\\n\\45\\\n\\67\\\n\\89\""
--
-- Call 'putStrLn' the returned string for a better example. Showing that here
-- would require too much escaping to be readable :(
formatPastable :: Int  -- ^ Max line width
               -> Strict.ByteString
               -> String
formatPastable maxLineWidth bytes
    | Strict.null bytes
    = withQuotes
    | otherwise
    = case headBodyTail $ NE.fromList $
             chunks (maxLineWidth - 2) withoutQuotes of
        Left _singleLine -> withQuotes
        Right (firstLine, middleLines, lastLine) -> intercalate "\n" $
          ["\"" <> firstLine <> "\\"] ++
          ["\\" <> middleLine <> "\\" | middleLine <- middleLines] ++
          ["\\" <> lastLine <> "\""]
  where
    withQuotes :: String
    withQuotes = show bytes

    withoutQuotes :: String
    withoutQuotes = init $ tail withQuotes

-- | Split a non-empty list in the head (first element), body (remainder), and
-- tail (last element).
--
-- > headBodyTail (NE.fromList [1,2,3,4]) == Right (1, [2,3], 4)
-- > headBodyTail (NE.fromList [1]) == Left 1
--
-- > \input -> NE.toList input ==
-- >   either
-- >     (\x -> [x])
-- >     (\(x, ys, z) -> [x] ++ ys ++ [z])
-- >     (headBodyTail input)
headBodyTail :: NonEmpty a -> Either a (a, [a], a)
headBodyTail (hd :| bodyTl) = case unsnoc bodyTl of
    Nothing         -> Left hd
    Just (body, tl) -> Right (hd, body, tl)
  where
    unsnoc = fmap swap . fmap (fmap reverse) . uncons . reverse
