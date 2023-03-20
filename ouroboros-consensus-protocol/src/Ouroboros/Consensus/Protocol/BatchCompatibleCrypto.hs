{-# LANGUAGE TypeFamilies #-}
module Ouroboros.Consensus.Protocol.BatchCompatibleCrypto (BatchCompatibleCrypto) where

import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash (Blake2b_224, Blake2b_256)
import           Cardano.Crypto.KES (Sum6KES)
import           Cardano.Crypto.VRF.PraosBatchCompat (PraosBatchCompatVRF)
import           Cardano.Ledger.Crypto (Crypto (..))
import Ouroboros.Consensus.Protocol.Praos (PraosCrypto)
import qualified Cardano.Protocol.TPraos.API as LedgerPraos


-- | Denotes a `Crypto` instance that implements Batch-compatible VRF.
data BatchCompatibleCrypto

instance Crypto BatchCompatibleCrypto where
  type DSIGN BatchCompatibleCrypto = Ed25519DSIGN
  type KES BatchCompatibleCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF BatchCompatibleCrypto = PraosBatchCompatVRF
  type HASH BatchCompatibleCrypto = Blake2b_256
  type ADDRHASH BatchCompatibleCrypto = Blake2b_224

instance LedgerPraos.PraosCrypto BatchCompatibleCrypto

instance PraosCrypto BatchCompatibleCrypto
