{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A stripped-down version of the @OCert@ and @Crypto@ types used in
-- @cardano-ledger@. We only replicate what we need here, so as to avoid
-- depending on @cardano-ledger@.
--
-- In order to keep things simple, we do /not/ use the same CBOR serialization
-- format for the 'OCert' type; this would require using or replicating the
-- @CBORGroup@ functionality from @cardano-ledger@, which I feel would be too
-- much to replicate here, and since the KES Agent protocols are not
-- performance critical, the small overhead introduced by using the default
-- CBOR serialization seems like an acceptable tradeoff.
module Cardano.KESAgent.KES.Crypto
where

import Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.KES.Class

import Data.Kind

-- | Convenience class that bundles associated KES and DSIGN algorithms into a
-- single typeclass. This is a subset of the @Crypto@ class defined in
-- @ouroboros-consensus@; we redefine it here for two reasons:
-- 1. To avoid a dependency on @ouroboros-consensus@
-- 2. Because we only need these two associated types, not the full @Crypto@
--    typeclass.
class
  ( KESAlgorithm (KES c)
  , DSIGNAlgorithm (DSIGN c)
  ) =>
  Crypto c
  where
  type KES c :: Type
  type DSIGN c :: Type
