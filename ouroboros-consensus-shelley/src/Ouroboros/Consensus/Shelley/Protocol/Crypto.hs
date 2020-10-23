{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Shelley.Protocol.Crypto (
    TPraosCrypto
  , StandardCrypto
  ) where

import           Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Crypto.KES.Sum
import           Cardano.Crypto.VRF.Praos (PraosVRF)

import           Cardano.Ledger.Crypto (Crypto (..))
import           Shelley.Spec.Ledger.API (BHBody, Hash)
import           Shelley.Spec.Ledger.BaseTypes (Seed)
import qualified Shelley.Spec.Ledger.Keys as SL (DSignable, KESignable,
                     VRFSignable)
import           Shelley.Spec.Ledger.OCert (OCertSignable)
import           Shelley.Spec.Ledger.TxBody (EraIndependentTxBody)

class ( Crypto c
      , SL.DSignable    c (OCertSignable c)
      , SL.DSignable    c (Hash c EraIndependentTxBody)
      , SL.KESignable   c (BHBody c)
      , SL.VRFSignable  c Seed
      ) => TPraosCrypto c

data StandardCrypto

instance Crypto StandardCrypto where
  type DSIGN    StandardCrypto = Ed25519DSIGN
  type KES      StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF      StandardCrypto = PraosVRF
  type HASH     StandardCrypto = Blake2b_256
  type ADDRHASH StandardCrypto = Blake2b_224

instance TPraosCrypto StandardCrypto
