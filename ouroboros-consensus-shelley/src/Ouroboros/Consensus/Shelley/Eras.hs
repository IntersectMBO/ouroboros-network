{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Shelley.Eras (
    -- * Eras based on the Shelley ledger
    AllegraEra
  , AlonzoEra
  , MaryEra
  , ShelleyEra
    -- * Eras instantiated with standard crypto
  , StandardAllegra
  , StandardAlonzo
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

import           Cardano.Binary (FromCBOR, ToCBOR)

import           Cardano.Ledger.Allegra (AllegraEra)
import           Cardano.Ledger.Allegra.Translation ()
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Era (Crypto, SupportsSegWit (..))
import qualified Cardano.Ledger.Era as Core
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Mary.Translation ()
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.ShelleyMA ()
import           Control.State.Transition (State)
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Serialization as SL

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The Shelley era with standard crypto
type StandardShelley = ShelleyEra StandardCrypto

-- | The Allegra era with standard crypto
type StandardAllegra = AllegraEra StandardCrypto

-- | The Mary era with standard crypto
type StandardMary = MaryEra StandardCrypto

-- | The Alonzo era with standard crypto
type StandardAlonzo = AlonzoEra StandardCrypto

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
class ( SL.ShelleyBasedEra era

      , State (Core.EraRule "PPUP" era) ~ SL.PPUPState era
      , Default (State (Core.EraRule "PPUP" era))

      , HasField "_maxBHSize" (Core.PParams era) Natural
      , HasField "_maxTxSize" (Core.PParams era) Natural
      , HasField "_a0" (Core.PParams era) Rational
      , HasField "_nOpt" (Core.PParams era) Natural
      , HasField "_rho" (Core.PParams era) SL.UnitInterval
      , HasField "_tau" (Core.PParams era) SL.UnitInterval

      , FromCBOR (Core.PParams era)
      , ToCBOR (Core.PParams era)

      , HasField "_protocolVersion" (Core.PParamsDelta era) (SL.StrictMaybe SL.ProtVer)
      , FromCBOR (Core.PParamsDelta era)

      , SL.AdditionalGenesisConfig era ~ ()

      , SL.ToCBORGroup (TxSeq era)

      , Eq (Core.TxInBlock era)
      , NoThunks (Core.TxInBlock era)
      , Show (Core.TxInBlock era)

      , NoThunks (Core.TranslationContext era)

      , ToCBOR (Core.Witnesses era)

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

instance SL.PraosCrypto c => ShelleyBasedEra (AlonzoEra c) where
  shelleyBasedEraName _ = "Alonzo"

{-------------------------------------------------------------------------------
  TxInBlock wrapper
-------------------------------------------------------------------------------}

-- | Wrapper for partially applying 'Core.TxInBlock'
--
-- For generality, Consensus uses that type family as eg the index of
-- 'Core.TranslateEra'. We thus need to partially apply it.
--
-- @cardano-ledger-specs@ also declares such a newtype, but currently it's only
-- defined in the Alonzo translation module, which seems somewhat inappropriate
-- to use for previous eras. Also, we use a @Wrap@ prefix in Consensus. Hence
-- this minor mediating definition. TODO I'm not even fully persuading myself
-- with this justification.
newtype WrapTxInBlock era = WrapTxInBlock {unwrapTxInBlock :: Core.TxInBlock era}

instance ShelleyBasedEra (AllegraEra c) => Core.TranslateEra (AllegraEra c) WrapTxInBlock where
  type TranslationError (AllegraEra c) WrapTxInBlock = Core.TranslationError (AllegraEra c) SL.Tx
  translateEra ctxt = fmap WrapTxInBlock . Core.translateEra ctxt . unwrapTxInBlock

instance ShelleyBasedEra (MaryEra c) => Core.TranslateEra (MaryEra c) WrapTxInBlock where
  type TranslationError (MaryEra c) WrapTxInBlock = Core.TranslationError (MaryEra c) SL.Tx
  translateEra ctxt = fmap WrapTxInBlock . Core.translateEra ctxt . unwrapTxInBlock

instance ShelleyBasedEra (AlonzoEra c) => Core.TranslateEra (AlonzoEra c) WrapTxInBlock where
  type TranslationError (AlonzoEra c) WrapTxInBlock = Core.TranslationError (AlonzoEra c) Alonzo.TxInBlock
  translateEra ctxt =
        fmap (\(Alonzo.TxInBlock tx) -> WrapTxInBlock tx)
      . Core.translateEra @(AlonzoEra c) ctxt
      . Alonzo.TxInBlock . unwrapTxInBlock
