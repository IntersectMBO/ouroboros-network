{-# language DeriveGeneric #-}
module TxIn.Types (
    TxIn (..)
  , TxOutputIds (..)
  , outputTxIns
  , Row (..)
  , filterRowsForEBBs
  ) where

import           Data.ByteString.Short (ShortByteString)
import           Data.Word (Word32, Word64)
import qualified Data.Vector as V
import           GHC.Generics (Generic())
import           Data.Binary (Binary())

import qualified Data.BTree.Primitives.Value as Haskey(Value(..))
import qualified Data.BTree.Primitives.Key as Haskey(Key(..))

data TxIn        = TxIn        !ShortByteString !Word32   -- index
  deriving (Eq, Ord, Show, Generic)

instance Binary TxIn

instance Haskey.Value TxIn where
  fixedSize _ = Nothing -- TODO is it fixed size? we can be more efficient if it is

instance Haskey.Key TxIn



data TxOutputIds = TxOutputIds !ShortByteString !Word32   -- count
  deriving (Eq, Ord)

outputTxIns :: TxOutputIds -> [TxIn]
outputTxIns (TxOutputIds h n) = [ TxIn h (i - 1) | i <- [1 .. n] ]

data Row = Row {
    rBlockNumber :: {-# UNPACK #-} !Word64
  , rSlotNumber  :: {-# UNPACK #-} !Word64
  , rNumTx       :: {-# UNPACK #-} !Int
  , rConsumed    :: {-# UNPACK #-} !(V.Vector TxIn)
  , rCreated     :: {-# UNPACK #-} !(V.Vector TxOutputIds)
  }

{-
for some slots in the Byron era there is an addtional psuedo block demarcates the
beginning of the era epoch, it's called an EBB.
It never contains any transactions
-}
filterRowsForEBBs :: [Row] -> [Row]
filterRowsForEBBs = go Nothing where
  go _ [] = []
  go Nothing (x: xs) = x: go (Just x) xs
  go (Just x) (y: ys)
    | rBlockNumber x == rBlockNumber y = if rNumTx y /= 0
      then error "EBB block has transactions"
      else go (Just x) ys
    | otherwise = y : go (Just y) ys
