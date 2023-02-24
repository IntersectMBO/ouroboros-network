{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Protocol.Praos.Crypto (
    HASH
  , KES
  , PraosCrypto
  , VRF
  ) where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.Core (EraIndependentTxBody)
import           Cardano.Ledger.Crypto (Crypto, DSIGN, HASH, KES,
                     StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.API as Ledger
import           Cardano.Protocol.TPraos.OCert (OCertSignable)
import           Ouroboros.Consensus.Protocol.Praos.Header (HeaderBody)
import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)

class
    ( KES.KESAlgorithm (KES c)
    , KES.ContextKES (KES c) ~ ()
    , KES.Signable (KES c) (HeaderBody c)
    , VRF.VRFAlgorithm (VRF c)
    , VRF.Signable (VRF c) InputVRF
    , DSIGN.Signable (DSIGN c) (OCertSignable c)
    , DSIGN.Signable (DSIGN c) (Hash.Hash (HASH c) EraIndependentTxBody)
    , VRF.ContextVRF (VRF c) ~ ()
    , Crypto c
    , Ledger.PraosCrypto c
    ) =>
    PraosCrypto c

instance PraosCrypto StandardCrypto
