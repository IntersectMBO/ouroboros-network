{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Consensus.Protocol.Praos.Crypto (
    HASH
  , KES
  , PraosCrypto
  , VRF
  , CanConvertVRF(..)
  , translateOCert
  , translateColdKey
  ) where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.Core (EraIndependentTxBody)
import           Cardano.Ledger.Crypto (Crypto, DSIGN, HASH, KES,
                     StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.API as Ledger
import           Cardano.Protocol.TPraos.OCert (OCertSignable, OCert(..))
import           Ouroboros.Consensus.Protocol.Praos.Header (HeaderBody)
import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)
import Cardano.Crypto.VRF (VerKeyVRF, SignKeyVRF, OutputVRF)
import qualified Cardano.Ledger.Shelley.API as SL

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

class (VRF.VRFAlgorithm vrf1, VRF.VRFAlgorithm vrf2) => CanConvertVRF vrf1 vrf2 where
  convertVKey :: VerKeyVRF vrf1 -> VerKeyVRF vrf2
  convertSKey :: SignKeyVRF vrf1 -> SignKeyVRF vrf2
  convertOutput :: OutputVRF vrf1 -> OutputVRF vrf2


translateOCert ::
  (DSIGN c1 ~ DSIGN c2, KES c1 ~ KES c2)
  => OCert c1 -> OCert c2
translateOCert
  OCert {ocertVkHot, ocertN, ocertKESPeriod, ocertSigma}
  = OCert {ocertVkHot, ocertN, ocertKESPeriod, ocertSigma = translateSigma ocertSigma }

translateSigma ::
  (DSIGN c1 ~ DSIGN c2)
  => DSIGN.SignedDSIGN (DSIGN c1) (OCertSignable c1)
  -> DSIGN.SignedDSIGN (DSIGN c2) (OCertSignable c2)
translateSigma (DSIGN.SignedDSIGN sd) = DSIGN.SignedDSIGN sd

translateColdKey ::
  (DSIGN c1 ~ DSIGN c2)
  => SL.VKey r c1
  -> SL.VKey r c2
translateColdKey (SL.VKey vk) = SL.VKey vk
