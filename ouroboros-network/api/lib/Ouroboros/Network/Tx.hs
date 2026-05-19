{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Abstract view over transactions
--
-- The network layer does not make any concrete assumptions about what
-- transactions or transaction identifiers look like.
module Ouroboros.Network.Tx (HasRawTxId (..)) where

import Control.DeepSeq (NFData)
import NoThunks.Class (NoThunks)

-- | Abstract over transaction identifiers, providing access to their raw byte
-- representation for efficient comparison.
--
-- 'getRawTxId' is used both as a faster equality proxy and as the key type
-- for shared lookup tables, so it must agree with the @txid@ instances on
-- both equality and ordering.
--
-- Laws:
--
-- * @getRawTxId x == getRawTxId y@ iff @x == y@
--   (raw equality is equivalent to txid equality; in particular, the raw
--   bytes must uniquely identify the transaction so Map keys do not collide)
-- * @compare (getRawTxId x) (getRawTxId y) == compare x y@
--   (ordering is preserved; ordered containers keyed by 'RawTxId' agree with
--   ordering on the underlying @txid@)
class ( Eq (RawTxId txid)
      , Ord (RawTxId txid)
      , Show (RawTxId txid)
      , NFData (RawTxId txid)
      , NoThunks (RawTxId txid)
      )
  => HasRawTxId txid where
  type RawTxId txid
  getRawTxId :: txid -> RawTxId txid
