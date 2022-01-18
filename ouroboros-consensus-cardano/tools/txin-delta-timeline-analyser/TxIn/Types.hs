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
import           Data.Foldable(toList)

import qualified Data.BTree.Primitives.Value as Haskey(Value(..))
import qualified Data.BTree.Primitives.Key as Haskey(Key(..))

data TxIn        = TxIn        !ShortByteString !Word32   -- index
  deriving (Eq, Ord, Show, Generic)

instance Binary TxIn

instance Haskey.Value TxIn where
  fixedSize _ = Nothing -- TODO is it fixed size? we can be more efficient if it is

instance Haskey.Key TxIn



data TxOutputIds = TxOutputIds !ShortByteString !Word32   -- count
  deriving (Eq, Ord, Show)

outputTxIns :: TxOutputIds -> [TxIn]
outputTxIns (TxOutputIds h n) = [ TxIn h (i - 1) | i <- [1 .. n] ]

data Row = Row {
    rBlockNumber :: {-# UNPACK #-} !Word64
  , rSlotNumber  :: {-# UNPACK #-} !Word64
  , rNumTx       :: {-# UNPACK #-} !Int
  , rConsumed    :: {-# UNPACK #-} !(V.Vector TxIn)
  , rCreated     :: {-# UNPACK #-} !(V.Vector TxOutputIds)
  } deriving (Show)

{-
for some slots in the Byron era there is an addtional psuedo block demarcates the
beginning of the era epoch, it's called an EBB.
It never contains any transactions
-}
filterRowsForEBBs :: [Row] -> [Row]
filterRowsForEBBs = go Nothing where
  go mb_x [] = toList mb_x
  go Nothing (x: xs) = x : go (Just x) xs
  go (Just x) (y: ys)
    | rBlockNumber x == rBlockNumber y = if rNumTx y /= 0
      then error "EBB block has transactions"
      else go Nothing ys
    | rSlotNumber x == rSlotNumber y = if rNumTx y /= 0
      then error "EBB block has transactions"
      else go Nothing ys
    | otherwise = y : go (Just y) ys
