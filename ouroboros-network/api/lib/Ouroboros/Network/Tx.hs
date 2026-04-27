{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Abstract view over transactions
--
-- The network layer does not make any concrete assumptions about what
-- transactions or transaction identifiers look like.
module Ouroboros.Network.Tx
  ( HasRawTxId (..)
  ) where

import Control.DeepSeq (NFData)
import NoThunks.Class (NoThunks)

-- | Abstract over transaction identifiers, providing access to their raw byte
-- representation for efficient comparison.
--
-- Laws:
--
-- * If @getRawTxId x == getRawTxId y@ then @x == y@
--   (the raw bytes must uniquely identify the transaction)
class ( Eq (RawTxId txid)
      , Ord (RawTxId txid)
      , Show (RawTxId txid)
      , NFData (RawTxId txid)
      , NoThunks (RawTxId txid)
      )
  => HasRawTxId txid where
  type RawTxId txid
  getRawTxId :: txid -> RawTxId txid
