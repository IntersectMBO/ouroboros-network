{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Module defining the crypto primitives used throughout Shelley based eras.
module Ouroboros.Consensus.Shelley.Crypto (
    BatchCompatibleCrypto
  , StandardCrypto
  ) where

import           Cardano.Crypto.VRF (OutputVRF, VRFAlgorithm (..))
import           Cardano.Crypto.VRF.Praos (PraosVRF, outputToBatchCompat,
                     skToBatchCompat, vkToBatchCompat)
import           Cardano.Crypto.VRF.PraosBatchCompat (PraosBatchCompatVRF)
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Ouroboros.Consensus.Protocol.Praos.Crypto (CanConvertVRF (..))

import           Ouroboros.Consensus.Protocol.BatchCompatibleCrypto
                     (BatchCompatibleCrypto)

instance CanConvertVRF PraosVRF PraosBatchCompatVRF where
  convertVKey :: VerKeyVRF PraosVRF -> VerKeyVRF PraosBatchCompatVRF
  convertVKey = vkToBatchCompat

  convertSKey :: SignKeyVRF PraosVRF -> SignKeyVRF PraosBatchCompatVRF
  convertSKey = skToBatchCompat

  convertOutput :: OutputVRF PraosVRF -> OutputVRF PraosBatchCompatVRF
  convertOutput = outputToBatchCompat
