{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module defining the crypto primitives used throughout Shelley based eras.
module Ouroboros.Consensus.Shelley.Crypto
  (StandardCrypto, VRF10Crypto) where

import Cardano.Ledger.Crypto (StandardCrypto, Crypto(..))
import Cardano.Crypto.KES (CompactSum7KES)
import Cardano.Crypto.DSIGN (Ed25519DSIGN)
import Cardano.Crypto.Hash (Blake2b_256)
import Cardano.Crypto.VRF.PraosBatchCompat (PraosBatchCompatVRF)

-- | Crypto to be used from the Babbage era onwards. This incorporates a few
-- changes from 'StandardCrypto':
--
-- - Update to IETF VRF draft 10
-- - Use a smaller KES proof

data VRF10Crypto

instance Crypto VRF10Crypto where
  type ADDRHASH _ = ADDRHASH StandardCrypto
  type HASH _ = HASH StandardCrypto
  type DSIGN _ = DSIGN StandardCrypto
  type VRF _ = PraosBatchCompatVRF
  type KES _ = CompactSum7KES Ed25519DSIGN Blake2b_256
