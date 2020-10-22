{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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

import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)

import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto,
                     TPraosCrypto)

{-------------------------------------------------------------------------------
  Eras based on the Shelley ledger
-------------------------------------------------------------------------------}

-- | The era after Shelley is Allegra, the illegitimate daughter of Byron.
--
-- In this era, we introduce time locks and miscellaneous fixes for the Shelley
-- era.
--
-- TODO #2668 Change this to the proper Allegra era
type AllegraEra c = ShelleyEra c

-- | The era after Allegra is Mary (Shelley), the wife of Percy Shelley.
--
-- In this era, we introduce multi-asset (hence MA-ry).
--
-- TODO #2668 Change this to the proper Mary era
type MaryEra c = ShelleyEra c

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
  Shelley-based era
-------------------------------------------------------------------------------}

-- | Constraints needed by a Shelley-based era
class ( TPraosCrypto (EraCrypto era)
      , ShelleyBased            era
      , SL.ApplyBlock           era
      , SL.GetLedgerView        era
      , SL.ApplyTx              era
      ) => ShelleyBasedEra era

instance TPraosCrypto c => ShelleyBasedEra (ShelleyEra c)

{-------------------------------------------------------------------------------
  Type synonyms for convenience
-------------------------------------------------------------------------------}

-- | The 'Cardano.Ledger.Era.Crypto' type family conflicts with the
-- 'Cardano.Ledger.Crypto.Crypto' class. To avoid having to import one or both
-- of them qualified, define 'EraCrypto' as an alias of the former: /return the
-- crypto used by this era/.
type EraCrypto era = Crypto era
