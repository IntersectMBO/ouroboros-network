{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Abstract view over transactions
--
-- The network layer does not make any concrete assumptions about what
-- transactions or transaction identifiers look like.
module Ouroboros.Network.Tx
  ( HasRawTxId (..)
  , RawTxId (..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString.Short (ShortByteString)
import NoThunks.Class (NoThunks)

-- | Raw byte representation of a transaction identifier.
newtype RawTxId = RawTxId ShortByteString
  deriving newtype (Eq, Ord, Show, NFData, NoThunks)

-- | Abstract over transaction identifiers, providing access to their raw byte
-- representation for efficient comparison.
--
-- Laws:
--
-- * If @getRawTxId x == getRawTxId y@ then @x == y@
--   (the raw bytes must uniquely identify the transaction)
class HasRawTxId txid where
  getRawTxId :: txid -> RawTxId
