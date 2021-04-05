module Test.Util.Blob (
    Blob (..)
  , blobFromBS
  , blobToBS
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.String (IsString (..))

import           Test.QuickCheck (ASCIIString (..), Arbitrary (..), suchThat)

{------------------------------------------------------------------------------
  Blob
------------------------------------------------------------------------------}

-- For the custom 'Show' and 'Arbitrary' instances
--
-- A builder of a non-empty bytestring.
newtype Blob = MkBlob { getBlob :: ByteString }
    deriving (Show)

instance Arbitrary Blob where
    arbitrary = do
      str <- (getASCIIString <$> arbitrary) `suchThat` (not . null)
      return $ fromString str
    shrink (MkBlob b) =
      [ fromString s'
      | let s = ASCIIString $ LC8.unpack $ BL.fromStrict b
      , s' <- getASCIIString <$> shrink s
      , not (null s') ]

blobToBS :: Blob -> ByteString
blobToBS = getBlob

blobFromBS :: ByteString -> Blob
blobFromBS = MkBlob

instance IsString Blob where
    fromString = blobFromBS . C8.pack
