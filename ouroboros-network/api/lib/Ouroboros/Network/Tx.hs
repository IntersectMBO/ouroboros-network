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
-- for shared lookup tables.
--
-- Law:
--
-- * @getRawTxId x == getRawTxId y@ iff @x == y@
--   (raw equality is equivalent to txid equality; in particular, the raw
--   bytes must uniquely identify the transaction so Map keys do not collide)
--
-- The 'Ord' instance is only used to key ordered containers, so it does
-- /not/ need to agree with the ordering of the underlying @txid@ (e.g.
-- a serialised-bytes ordering is fine even when it sorts differently from
-- the @txid@'s own 'Ord').
class ( Eq (RawTxId txid)
      , Ord (RawTxId txid)
      , Show (RawTxId txid)
      , NFData (RawTxId txid)
      , NoThunks (RawTxId txid)
      )
  => HasRawTxId txid where
  type RawTxId txid
  getRawTxId :: txid -> RawTxId txid
