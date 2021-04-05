{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Shelley.Eras (
    -- * Eras based on the Shelley ledger
    AllegraEra
  , MaryEra
  , ShelleyEra
    -- * Eras instantiated with standard crypto
  , StandardAllegra
  , StandardMary
  , StandardShelley
    -- * Shelley-based era
  , ShelleyBasedEra (..)
    -- * Type synonyms for convenience
  , EraCrypto
    -- * Re-exports
  , StandardCrypto
  ) where

import           Data.Default.Class (Default)
import           Data.Text (Text)
import           GHC.Records
import           Numeric.Natural (Natural)

import           Cardano.Ledger.Allegra (AllegraEra)
import qualified Cardano.Ledger.Core as LC
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Control.State.Transition (State)

import           Cardano.Binary (FromCBOR, ToCBOR)
import qualified Cardano.Ledger.Shelley.Constraints as SL
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL

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

{-------------------------------------------------------------------------------
  Era polymorphism
-------------------------------------------------------------------------------}

-- | The ledger already defines 'SL.ShelleyBasedEra' as /the/ top-level
-- constraint on an era, however, consensus often needs some more functionality
-- than the ledger currently provides.
--
-- Either the functionality shouldn't or can't live in the ledger, in which case
-- it can be part and remain part of 'ShelleyBasedEra'. Or, the functionality
-- /should/ live in the ledger, but hasn't yet been added to the ledger, or it
-- hasn't yet been propagated to this repository, in which case it can be added
-- to this class until that is the case.
--
-- By having the same name as the class defined in ledger, we can, if this class
-- becomes redundant, switch to the ledger-defined one without having to update
-- all the code using it. We can just export the right one from this module.
--
-- TODO Currently we include some constraints on the update state which are
-- needed to determine the hard fork point. In the future this should be
-- replaced with an appropriate API - see
-- https://github.com/input-output-hk/ouroboros-network/issues/2890
--
-- TODO Currently we include the constraint @SL.AdditionalGenesisConfig era ~
-- ()@. When we fork to Alonzo we will need additional genesis config
-- information.
class ( SL.ShelleyBasedEra era
      , State (LC.EraRule "PPUP" era) ~ SL.PPUPState era
      , Default (State (LC.EraRule "PPUP" era))
      , HasField "_maxBHSize" (LC.PParams era) Natural
      , HasField "_maxTxSize" (LC.PParams era) Natural
      , HasField "_a0" (LC.PParams era) Rational
      , HasField "_nOpt" (LC.PParams era) Natural
      , HasField "_rho" (LC.PParams era) SL.UnitInterval
      , HasField "_tau" (LC.PParams era) SL.UnitInterval
      , HasField "_protocolVersion" (SL.PParamsDelta era) (SL.StrictMaybe SL.ProtVer)
      , SL.AdditionalGenesisConfig era ~ ()
      , FromCBOR (LC.PParams era)
      , FromCBOR (SL.PParamsDelta era)
      , ToCBOR (LC.PParams era)
      ) => ShelleyBasedEra era where
  -- | Return the name of the Shelley-based era, e.g., @"Shelley"@, @"Allegra"@,
  -- etc.
  shelleyBasedEraName :: proxy era -> Text

instance SL.PraosCrypto c => ShelleyBasedEra (ShelleyEra c) where
  shelleyBasedEraName _ = "Shelley"

instance SL.PraosCrypto c => ShelleyBasedEra (AllegraEra c) where
  shelleyBasedEraName _ = "Allegra"

instance SL.PraosCrypto c => ShelleyBasedEra (MaryEra c) where
  shelleyBasedEraName _ = "Mary"
