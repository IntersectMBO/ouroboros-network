module TxIn.Types (
    TxIn (..)
  , TxOutputIds (..)
  , outputTxIns
  ) where

import           Data.ByteString.Short (ShortByteString)
import           Data.Word (Word32)


data TxIn        = TxIn        !ShortByteString !Word32   -- index
  deriving (Eq, Ord)

data TxOutputIds = TxOutputIds !ShortByteString !Word32   -- count
  deriving (Eq, Ord)

outputTxIns :: TxOutputIds -> [TxIn]
outputTxIns (TxOutputIds h n) = [ TxIn h (i - 1) | i <- [1 .. n] ]
