{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hard fork eras.
--
--   Compare this to 'Ouroboros.Consensus.Shelley.Eras', which defines ledger
--   eras. This module defines hard fork eras, which are a combination of a
--   ledger era and a protocol.
module Ouroboros.Consensus.Shelley.HFEras (
    StandardAllegraBlock
  , StandardAlonzoBlock
  , StandardMaryBlock
  , StandardShelleyBlock
  ) where

import           Cardano.Crypto.DSIGN (Signable)
import           Cardano.Crypto.Hash (Hash)
import           Cardano.Ledger.Crypto (DSIGN, HASH)
import           Cardano.Ledger.Hashes (EraIndependentTxBody)
import           Ouroboros.Consensus.Protocol.TPraos (StandardCrypto, TPraos)
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.Eras (AllegraEra, AlonzoEra,
                     MaryEra, ShelleyEra, StandardAllegra, StandardAlonzo,
                     StandardMary, StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock,
                     ShelleyCompatible)
import           Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import           Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC ()

{-------------------------------------------------------------------------------
  Hard fork eras
-------------------------------------------------------------------------------}

type StandardShelleyBlock = ShelleyBlock (TPraos StandardCrypto) StandardShelley

type StandardAllegraBlock = ShelleyBlock (TPraos StandardCrypto) StandardAllegra

type StandardMaryBlock = ShelleyBlock (TPraos StandardCrypto) StandardMary

type StandardAlonzoBlock = ShelleyBlock (TPraos StandardCrypto) StandardAlonzo

{-------------------------------------------------------------------------------
  ShelleyCompatible
-------------------------------------------------------------------------------}

instance
  (TPraos.PraosCrypto c, Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)) =>
  ShelleyCompatible (TPraos c) (ShelleyEra c)

instance
  (TPraos.PraosCrypto c, Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)) =>
  ShelleyCompatible (TPraos c) (AllegraEra c)

instance
  (TPraos.PraosCrypto c, Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)) =>
  ShelleyCompatible (TPraos c) (MaryEra c)

instance
  (TPraos.PraosCrypto c, Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)) =>
  ShelleyCompatible (TPraos c) (AlonzoEra c)
