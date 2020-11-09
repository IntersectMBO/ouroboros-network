module Ouroboros.Consensus.Shelley.Eras (
    -- * Eras based on the Shelley ledger
    ShelleyEra
  , AllegraEra
  , MaryEra
    -- * Eras instantiated with standard crypto
  , StandardShelley
  , StandardAllegra
  , StandardMary
    -- * Shelley-based era
  , ShelleyBasedEra
    -- * Type synonyms for convenience
  , EraCrypto
    -- * Re-exports
  , StandardCrypto
  ) where

import           Cardano.Ledger.Allegra (AllegraEra)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Shelley (ShelleyEra)

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Shelley.Spec.Ledger.API (ShelleyBasedEra)

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The Shelley era with standard crypto
type StandardShelley = ShelleyEra StandardCrypto

-- | The Allegra era with standard crypto
type StandardAllegra = AllegraEra StandardCrypto

-- | The Mary era with standard crypto
type StandardMary = MaryEra StandardCrypto

{-------------------------------------------------------------------------------
  Type synonyms for convenience
-------------------------------------------------------------------------------}

-- | The 'Cardano.Ledger.Era.Crypto' type family conflicts with the
-- 'Cardano.Ledger.Crypto.Crypto' class. To avoid having to import one or both
-- of them qualified, define 'EraCrypto' as an alias of the former: /return the
-- crypto used by this era/.
type EraCrypto era = Crypto era
