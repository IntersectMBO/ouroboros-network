module Ouroboros.Consensus.Shelley.Eras (
    -- * Eras based on the Shelley ledger
    ShelleyEra
  , AllegraEra
  , MaryEra
    -- * Eras instantiated with standard crypto
  , StandardShelley
  , StandardAllegra
  , StandardMary
  ) where

import qualified Cardano.Ledger.Shelley as Era (Shelley)

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

{-------------------------------------------------------------------------------
  Eras based on the Shelley ledger
-------------------------------------------------------------------------------}

-- | The era after Byron is the Shelley era.
--
-- The Shelley ledger and block type itself is parameterised by an era
-- parameter, which is in its turn parameterised by the crypto used.
type ShelleyEra c = Era.Shelley c

-- | The era after Shelley is Allegra, the illegitimate daughter of Byron.
--
-- In this era, we introduce time locks and miscellaneous fixes for the Shelley
-- era.
--
-- TODO #2668 Change this to the proper Allegra era
type AllegraEra c = Era.Shelley c

-- | The era after Allegra is Mary (Shelley), the wife of Percy Shelley.
--
-- In this era, we introduce multi-asset (hence MA-ry).
--
-- TODO #2668 Change this to the proper Mary era
type MaryEra c = Era.Shelley c

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The Shelley era with standard crypto
type StandardShelley = ShelleyEra StandardCrypto

-- | The Allegra era with standard crypto
type StandardAllegra = AllegraEra StandardCrypto

-- | The Mary era with standard crypto
type StandardMary = MaryEra StandardCrypto
