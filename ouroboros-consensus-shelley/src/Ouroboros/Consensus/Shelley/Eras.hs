{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
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
  , WrapTxInBlock (..)
    -- * Type synonyms for convenience
  , EraCrypto
    -- * Re-exports
  , StandardCrypto
  ) where

import           Data.Default.Class (Default)
import           Data.Text (Text)
import           GHC.Records
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)

import           Cardano.Ledger.Allegra (AllegraEra)
import qualified Cardano.Ledger.Core as LC
import           Cardano.Ledger.Era (Crypto, SupportsSegWit (..))
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.ShelleyMA ()
import           Control.State.Transition (State)

import           Cardano.Binary (FromCBOR, ToCBOR)
import           Cardano.Ledger.Allegra.Translation ()
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as SL (TranslateEra (..), TxInBlock)
import           Cardano.Ledger.Mary.Translation ()
import qualified Cardano.Ledger.Shelley.Constraints as SL
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Serialization as SL

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
--
-- TODO Core.Witnesses is type family that represents the set of witnesses
-- in a Tx which may vary from one Era to another.
-- Currently, for all existing Shelley based eras (Shelley, Alegra,
-- and Mary) this type is set to SL.WitnessSet. This will eventually change,
-- most likely with Alonzo, thus this equivalence will no longer be valid.
class ( SL.ShelleyBasedEra era

      , State (LC.EraRule "PPUP" era) ~ SL.PPUPState era
      , Default (State (LC.EraRule "PPUP" era))

      , HasField "_maxBHSize" (LC.PParams era) Natural
      , HasField "_maxTxSize" (LC.PParams era) Natural
      , HasField "_a0" (LC.PParams era) Rational
      , HasField "_nOpt" (LC.PParams era) Natural
      , HasField "_rho" (LC.PParams era) SL.UnitInterval
      , HasField "_tau" (LC.PParams era) SL.UnitInterval
      , FromCBOR (LC.PParams era)
      , ToCBOR (LC.PParams era)

      , HasField "_protocolVersion" (SL.PParamsDelta era) (SL.StrictMaybe SL.ProtVer)
      , FromCBOR (SL.PParamsDelta era)

      , SL.AdditionalGenesisConfig era ~ ()

      , Core.Witnesses era ~ SL.WitnessSet era

      , SL.ToCBORGroup (TxSeq era)

      , Eq (SL.TxInBlock era)
      , NoThunks (SL.TxInBlock era)
      , Show (SL.TxInBlock era)

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

{-------------------------------------------------------------------------------
  TxInBlock wrapper
-------------------------------------------------------------------------------}

-- | Wrapper for partially applying 'SL.TxInBlock'
--
-- For generality, Consensus uses that type family as eg the index of
-- 'SL.TranslateEra'. We thus need to partially apply it.
newtype WrapTxInBlock era = WrapTxInBlock {unwrapTxInBlock :: SL.TxInBlock era}

instance ShelleyBasedEra (AllegraEra c) => SL.TranslateEra (AllegraEra c) WrapTxInBlock where
  type TranslationError (AllegraEra c) WrapTxInBlock = SL.TranslationError (AllegraEra c) SL.Tx
  translateEra ctxt = fmap WrapTxInBlock . SL.translateEra ctxt . unwrapTxInBlock

instance ShelleyBasedEra (MaryEra c) => SL.TranslateEra (MaryEra c) WrapTxInBlock where
  type TranslationError (MaryEra c) WrapTxInBlock = SL.TranslationError (MaryEra c) SL.Tx
  translateEra ctxt = fmap WrapTxInBlock . SL.translateEra ctxt . unwrapTxInBlock
