{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE PatternSynonyms         #-}
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
  , WrapTx (..)
    -- * Type synonyms for convenience
  , EraCrypto
    -- * Re-exports
  , StandardCrypto
    -- * Exceptions
  , UnexpectedAlonzoLedgerErrors
  ) where

import           Control.Exception (Exception, throw)
import           Control.Monad.Except
import           Data.Default.Class (Default)
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Records
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)

import           Cardano.Binary (FromCBOR, ToCBOR)

import           Cardano.Ledger.Allegra (AllegraEra)
import           Cardano.Ledger.Allegra.Translation ()
import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.Alonzo.PParams
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era (Crypto, SupportsSegWit (..))
import qualified Cardano.Ledger.Era as Core
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Mary.Translation ()
import           Cardano.Ledger.Serialization
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.ShelleyMA ()
import           Control.State.Transition (State)
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.STS.Ledger as SL
import qualified Shelley.Spec.Ledger.STS.Utxow as SL

import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (WhetherToIntervene (..))

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
      , HasField "_a0" (Core.PParams era) NonNegativeInterval
      , HasField "_nOpt" (Core.PParams era) Natural
      , HasField "_rho" (Core.PParams era) UnitInterval
      , HasField "_tau" (Core.PParams era) UnitInterval

      , FromCBOR (Core.PParams era)
      , ToCBOR (Core.PParams era)

      , HasField "_protocolVersion" (Core.PParamsDelta era) (SL.StrictMaybe SL.ProtVer)
      , FromCBOR (Core.PParamsDelta era)

      , SL.AdditionalGenesisConfig era ~ Core.TranslationContext era
      , ToCBORGroup (TxSeq era)

      , NoThunks (Core.TranslationContext era)

      , ToCBOR (Core.Witnesses era)

      ) => ShelleyBasedEra era where

  -- | Return the name of the Shelley-based era, e.g., @"Shelley"@, @"Allegra"@,
  -- etc.
  shelleyBasedEraName :: proxy era -> Text

  applyShelleyBasedTx ::
       SL.Globals
    -> SL.LedgerEnv    era
    -> SL.MempoolState era
    -> WhetherToIntervene
    -> Core.Tx           era
    -> Except
         (SL.ApplyTxError era)
         ( SL.MempoolState era
         , SL.Validated (Core.Tx era)
         )

-- | The default implementation of 'applyShelleyBasedTx', a thin wrapper around
-- 'SL.applyTx'
defaultApplyShelleyBasedTx ::
     ShelleyBasedEra era
  => SL.Globals
  -> SL.LedgerEnv    era
  -> SL.MempoolState era
  -> WhetherToIntervene
  -> Core.Tx         era
  -> Except
       (SL.ApplyTxError era)
       ( SL.MempoolState era
       , SL.Validated (Core.Tx era)
       )
defaultApplyShelleyBasedTx globals ledgerEnv mempoolState _wti tx =
    SL.applyTx
      globals
      ledgerEnv
      mempoolState
      tx

instance SL.PraosCrypto c => ShelleyBasedEra (ShelleyEra c) where
  shelleyBasedEraName _ = "Shelley"

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance SL.PraosCrypto c => ShelleyBasedEra (AllegraEra c) where
  shelleyBasedEraName _ = "Allegra"

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance SL.PraosCrypto c => ShelleyBasedEra (MaryEra c) where
  shelleyBasedEraName _ = "Mary"

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance SL.PraosCrypto c => ShelleyBasedEra (AlonzoEra c) where
  shelleyBasedEraName _ = "Alonzo"

  applyShelleyBasedTx globals ledgerEnv mempoolState wti tx = do
      (mempoolState', vtx) <-
          (`catchError` handler)
        $ defaultApplyShelleyBasedTx
            globals
            ledgerEnv
            mempoolState
            wti
            tx

      pure (mempoolState', vtx)
    where
      nubOrd = Set.toList . Set.fromList

      handler e = case (wti, e) of
        (DoNotIntervene, SL.ApplyTxError errs)
          | flag:flags <- nubOrd [b | IncorrectClaimedFlag b <- errs] ->
            if not (null flags)
            then throw $ UnexpectedAlonzoLedgerErrors (flag:flags)
            else
            -- rectify the flag and include the transaction
            --
            -- This either lets the ledger punish the script author for sending
            -- a bad script or else prevents our peer's buggy script validator
            -- from preventing inclusion of a valid script.
            --
            -- TODO 'applyTx' et al needs to include a return value indicating
            -- whether we took this branch; it's a reason to disconnect from
            -- the peer who sent us the incorrect flag (ie Issue #3276)
            defaultApplyShelleyBasedTx
              globals
              ledgerEnv
              mempoolState
              wti
              tx{Alonzo.isValidating = Alonzo.IsValidating (not flag)}
        _ -> throwError e
               -- reject the transaction, protecting the local wallet

-- not exported
--
-- The ledger failure we see when the transaction's claimed 'IsValidating'
-- flag was incorrect
pattern IncorrectClaimedFlag ::
     Bool
  -> SL.PredicateFailure (Core.EraRule "LEDGER" (AlonzoEra c))
pattern IncorrectClaimedFlag claimedFlag <-
    SL.UtxowFailure
      (Alonzo.WrappedShelleyEraFailure
        (SL.UtxoFailure
          (Alonzo.UtxosFailure
            (Alonzo.ValidationTagMismatch
              (Alonzo.IsValidating claimedFlag
      )))))

-- | The ledger responded with Alonzo errors we were not expecting
data UnexpectedAlonzoLedgerErrors =
    -- | We received more than one 'Alonzo.ValidationTagMismatch'
    --
    -- The exception lists the 'Alonzo.IsValidating' flags we saw.
    UnexpectedAlonzoLedgerErrors [Bool]
  deriving (Show, Exception)

{-------------------------------------------------------------------------------
  Tx family wrapper
-------------------------------------------------------------------------------}

-- | Wrapper for partially applying the 'Tx' type family
--
-- For generality, Consensus uses that type family as eg the index of
-- 'Core.TranslateEra'. We thus need to partially apply it.
--
-- @cardano-ledger-specs@ also declares such a newtype, but currently it's only
-- defined in the Alonzo translation module, which seems somewhat inappropriate
-- to use for previous eras. Also, we use a @Wrap@ prefix in Consensus. Hence
-- this minor mediating definition. TODO I'm not even fully persuading myself
-- with this justification.
newtype WrapTx era = WrapTx {unwrapTx :: Core.Tx era}

instance ShelleyBasedEra (AllegraEra c) => Core.TranslateEra (AllegraEra c) WrapTx where
  type TranslationError (AllegraEra c) WrapTx = Core.TranslationError (AllegraEra c) SL.Tx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance ShelleyBasedEra (MaryEra c) => Core.TranslateEra (MaryEra c) WrapTx where
  type TranslationError (MaryEra c) WrapTx = Core.TranslationError (MaryEra c) SL.Tx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance ShelleyBasedEra (AlonzoEra c) => Core.TranslateEra (AlonzoEra c) WrapTx where
  type TranslationError (AlonzoEra c) WrapTx = Core.TranslationError (AlonzoEra c) Alonzo.Tx
  translateEra ctxt =
        fmap (WrapTx . Alonzo.unTx)
      . Core.translateEra @(AlonzoEra c) ctxt
      . Alonzo.Tx . unwrapTx
