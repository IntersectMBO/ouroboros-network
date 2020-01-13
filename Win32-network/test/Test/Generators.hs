{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Generators where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

tests :: TestTree
tests = testGroup "Generators"
    [ testProperty "NonEmptyBS" prop_NonEmptyBS
    , testProperty "NonEmptyBS" prop_shrink_NonEmptyBS
    ]

--
-- Generators
--

-- | Small non-empty 'ByteString's.
--
data NonEmptyBS = NonEmptyBS { getNonEmptyBS :: ByteString }
  deriving (Eq, Show)

instance Arbitrary NonEmptyBS where
    arbitrary = do
      bs <- arbitrary
      if BS.null bs
        then do
          -- generate a non empty string
          NonEmpty s <- arbitrary
          pure (NonEmptyBS $ BSC.pack s)
        else pure (NonEmptyBS bs)

    shrink (NonEmptyBS bs) =
      [ NonEmptyBS bs'
      | bs' <- shrink bs
      , not (BS.null bs')
      ]

prop_NonEmptyBS :: NonEmptyBS -> Bool
prop_NonEmptyBS (NonEmptyBS bs) = not (BS.null bs)

prop_shrink_NonEmptyBS :: NonEmptyBS -> Bool
prop_shrink_NonEmptyBS = all (\(NonEmptyBS bs) -> not (BS.null bs)) . shrink

-- | Large non-empty 'ByteString's, up to 2.5MB.
--
data LargeNonEmptyBS = LargeNonEmptyBS
    { getLargeNonEmptyBS :: ByteString
    , getSize            :: Word
      -- ^ arbitrary size which is less than length of the bytestring; Useful
      -- for setting buffer size of a named pipe
    }
  deriving (Show, Eq)

instance Arbitrary LargeNonEmptyBS where
    arbitrary = do
        bs <- getNonEmptyBS <$> resize 2_500_000 arbitrary
        bufSize <- fromIntegral <$> choose (64::Word, floor . sqrt @Float . fromIntegral $ BS.length bs)
        pure $ LargeNonEmptyBS bs bufSize

    shrink (LargeNonEmptyBS bs bufSize) =
      [ (LargeNonEmptyBS bs' (min bufSize (fromIntegral $ BS.length bs')))
      | NonEmptyBS bs' <- shrink (NonEmptyBS bs)
      ]
