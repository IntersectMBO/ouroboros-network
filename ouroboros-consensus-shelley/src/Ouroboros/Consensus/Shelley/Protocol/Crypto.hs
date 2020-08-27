{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Shelley.Protocol.Crypto (
    module Shelley.Spec.Ledger.Crypto
  , TPraosCrypto
  , TPraosStandardCrypto
  ) where

import           Cardano.Binary (ToCBOR)
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import           Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Crypto.Hash.Class (Hash)
import qualified Cardano.Crypto.KES.Class as KES
import           Cardano.Crypto.KES.Sum
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Crypto.VRF.Praos (PraosVRF)

import           Ouroboros.Consensus.Util.Condense (Condense)

import           Shelley.Spec.Ledger.BaseTypes (Seed)
import           Shelley.Spec.Ledger.BlockChain (BHBody)
import           Shelley.Spec.Ledger.Crypto (Crypto (..))
import           Shelley.Spec.Ledger.OCert (OCertSignable)
import           Shelley.Spec.Ledger.TxData (TxBody)

class ( Crypto c
      , Condense (SigKES (KES c))
      , DSIGN.Signable
          (DSIGN c)
          (OCertSignable c)
      , DSIGN.Signable
          (DSIGN c)
          (Hash (HASH c) (TxBody c))
      , KES.Signable
          (KES c)
          (BHBody c)
      , VRF.Signable (VRF c) Seed
      , ToCBOR (DSIGN.VerKeyDSIGN (DSIGN c))
      ) => TPraosCrypto c

data TPraosStandardCrypto

instance Crypto TPraosStandardCrypto where
  type DSIGN    TPraosStandardCrypto = Ed25519DSIGN
  type KES      TPraosStandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF      TPraosStandardCrypto = PraosVRF
  type HASH     TPraosStandardCrypto = Blake2b_256
  type ADDRHASH TPraosStandardCrypto = Blake2b_224

instance TPraosCrypto TPraosStandardCrypto
