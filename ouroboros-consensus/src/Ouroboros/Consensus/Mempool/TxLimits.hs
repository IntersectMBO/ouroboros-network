{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Ouroboros.Consensus.Mempool.TxLimits (
    ByteSize (..)
  , TxLimits (..)
  ) where

import           Data.Word (Word32)

import           Ouroboros.Consensus.Ledger.Abstract (Validated)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import           Ouroboros.Consensus.Ticked (Ticked (..))

class ( Monoid (Measure blk)
      ) => TxLimits blk where

  type Measure blk

  lessEq      :: Measure blk -> Measure blk -> Bool
  txMeasure   :: Validated (GenTx blk) -> Measure blk
  maxCapacity :: Ticked (LedgerState blk) -> Measure blk

newtype ByteSize = ByteSize { unByteSize :: Word32 }
  deriving stock (Show, Eq, Ord)
  deriving newtype Num

instance Semigroup ByteSize where
  (ByteSize bs1) <> (ByteSize bs2) = ByteSize $ bs1 + bs2

instance Monoid ByteSize where
  mempty = ByteSize 0
