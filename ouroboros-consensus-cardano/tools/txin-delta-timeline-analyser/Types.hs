module Types (
    TxIn (..)
  , TxOutputIds (..)
  , outputTxIns
  , showTxIn16
  , showTxIn64
  ) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.ByteString.Short.Base16 (encodeBase16')
import           Data.ByteString.Short.Base64 (encodeBase64)
import qualified Data.Text.Short as TextShort
import qualified Data.Vector as V
import           Data.Word (Word32, Word64)


data TxIn        = TxIn        !ShortByteString !Word32   -- index
  deriving (Eq, Ord)

data TxOutputIds = TxOutputIds !ShortByteString !(V.Vector Word64)   -- sizes
  deriving (Eq, Ord)

outputTxIns :: TxOutputIds -> V.Vector (TxIn, Word64)
outputTxIns (TxOutputIds h sizes) =
    V.imap (\i size -> (TxIn h (toEnum i), size)) sizes

showTxIn64 :: TxIn -> String
showTxIn64 (TxIn h i) =
     TextShort.toString (encodeBase64 h)
  <> "@"
  <> show i

showTxIn16 :: TxIn -> String
showTxIn16 (TxIn h i) =
     -- encodeBase16 is bugged, so I need to convert through hoops to use
     -- encodeBase16'
     Char8.unpack (Char8.fromStrict (Short.fromShort (encodeBase16' h)))
  <> "@"
  <> show i

instance Show TxIn where show = showTxIn64
