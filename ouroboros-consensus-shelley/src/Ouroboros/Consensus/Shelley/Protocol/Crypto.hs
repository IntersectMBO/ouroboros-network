{-# LANGUAGE TypeFamilies #-}
module Ouroboros.Consensus.Shelley.Protocol.Crypto (
    PraosCrypto
  , StandardCrypto
  ) where

import           Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Crypto.KES.Sum
import           Cardano.Crypto.VRF.Praos (PraosVRF)

import           Cardano.Ledger.Crypto (Crypto (..))
import           Shelley.Spec.Ledger.API (PraosCrypto)

data StandardCrypto

instance Crypto StandardCrypto where
  type DSIGN    StandardCrypto = Ed25519DSIGN
  type KES      StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF      StandardCrypto = PraosVRF
  type HASH     StandardCrypto = Blake2b_256
  type ADDRHASH StandardCrypto = Blake2b_224

instance PraosCrypto StandardCrypto
