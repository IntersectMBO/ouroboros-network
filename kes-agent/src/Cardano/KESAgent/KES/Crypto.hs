{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Cardano.Binary
import Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Util ( SignableRepresentation (..) )

import Control.Monad ( when )
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Builder.Extra qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Typeable ( Typeable )
import Data.Word
import GHC.Generics ( Generic )
import NoThunks.Class ( NoThunks (..) )
import Quiet ( Quiet (..) )

-- | Convenience class that bundles associated KES and DSIGN algorithms into a
-- single typeclass
class ( KESAlgorithm (KES c)
      , DSIGNAlgorithm (DSIGN c)
      ) => Crypto c where
  type KES c :: *
  type DSIGN c :: *
