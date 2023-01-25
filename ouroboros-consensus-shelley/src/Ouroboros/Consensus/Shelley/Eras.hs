{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
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
  , BabbageEra
  , MaryEra
  , ShelleyEra
    -- * Eras instantiated with standard crypto
  , StandardAllegra
  , StandardAlonzo
  , StandardBabbage
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
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import           Cardano.Ledger.Babbage (BabbageEra)
import           Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import qualified Cardano.Ledger.Babbage.Translation as Babbage
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import           Cardano.Ledger.Keys (DSignable, Hash)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Mary.Translation ()
import           Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.Shelley.Rules.Ledger as SL
import qualified Cardano.Ledger.Shelley.Rules.Utxow as SL
import           Cardano.Ledger.ShelleyMA ()
import qualified Cardano.Protocol.TPraos.API as SL
import           Control.State.Transition (PredicateFailure, State)
import           Data.Data (Proxy (Proxy))
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

-- | The Babbage era with standard crypto
type StandardBabbage = BabbageEra StandardCrypto

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

-- | Consensus often needs some more functionality than the ledger currently
-- provides.
--
-- Either the functionality shouldn't or can't live in the ledger, in which case
-- it can be part and remain part of 'ShelleyBasedEra'. Or, the functionality
-- /should/ live in the ledger, but hasn't yet been added to the ledger, or it
-- hasn't yet been propagated to this repository, in which case it can be added
-- to this class until that is the case.
--
-- If this class becomes redundant, We can move it to ledger and re-export it
-- from here.
--
-- TODO Currently we include some constraints on the update state which are
-- needed to determine the hard fork point. In the future this should be
-- replaced with an appropriate API - see
-- https://github.com/input-output-hk/ouroboros-network/issues/2890
class ( Core.EraSegWits era
      , SL.ApplyTx era
      , SL.ApplyBlock era
      , SL.CanStartFromGenesis era

        -- TODO This constraint is quite tight, since it fixes things to the
        -- original TPraos ledger view. We would like to ultimately remove it.
      , SL.GetLedgerView era

      , State (Core.EraRule "PPUP" era) ~ SL.PPUPState era
      , Default (State (Core.EraRule "PPUP" era))

      , HasField "_maxBBSize" (Core.PParams era) Natural
      , HasField "_maxBHSize" (Core.PParams era) Natural
      , HasField "_maxTxSize" (Core.PParams era) Natural
      , HasField "_a0" (Core.PParams era) NonNegativeInterval
      , HasField "_nOpt" (Core.PParams era) Natural
      , HasField "_rho" (Core.PParams era) UnitInterval
      , HasField "_tau" (Core.PParams era) UnitInterval

      , HasField "_protocolVersion" (Core.PParamsUpdate era) (SL.StrictMaybe SL.ProtVer)

        -- TODO This constraint is a little weird. The translation context
        -- reflects things needed in comparison to the previous era, whereas the
        -- 'AdditionalGenesisConfig' is from Shelley. Ultimately we should drop
        -- this and potentially add a new API for dealing with the relationship
        -- between `GenesisConfig` and `TranslationContext`.
      , SL.AdditionalGenesisConfig era ~ Core.TranslationContext era

      , NoThunks (SL.StashedAVVMAddresses era)
      , FromCBOR (SL.StashedAVVMAddresses era)
      , ToCBOR (SL.StashedAVVMAddresses era)
      , Show (SL.StashedAVVMAddresses era)
      , Eq (SL.StashedAVVMAddresses era)

      , FromCBOR (PredicateFailure (EraRule "DELEGS" era))
      , ToCBOR (PredicateFailure (EraRule "DELEGS" era))
      , FromCBOR (PredicateFailure (EraRule "UTXOW" era))
      , ToCBOR (PredicateFailure (EraRule "UTXOW" era))

      , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
      , NoThunks (PredicateFailure (Core.EraRule "BBODY" era))
      , NoThunks (Core.TranslationContext era)
      , NoThunks (Core.Value era)

      ) => ShelleyBasedEra era where

  -- | Return the name of the Shelley-based era, e.g., @"Shelley"@, @"Allegra"@,
  -- etc.
  shelleyBasedEraName :: proxy era -> Text

  applyShelleyBasedTx ::
       SL.Globals
    -> SL.LedgerEnv era
    -> SL.LedgerState era
    -> WhetherToIntervene
    -> Core.Tx era
    -> Except
         (SL.ApplyTxError era)
         ( SL.LedgerState era
         , SL.Validated (Core.Tx era)
         )

-- | The default implementation of 'applyShelleyBasedTx', a thin wrapper around
-- 'SL.applyTx'
defaultApplyShelleyBasedTx ::
     ShelleyBasedEra era
  => SL.Globals
  -> SL.LedgerEnv era
  -> SL.LedgerState era
  -> WhetherToIntervene
  -> Core.Tx era
  -> Except
       (SL.ApplyTxError era)
       ( SL.LedgerState era
       , SL.Validated (Core.Tx era)
       )
defaultApplyShelleyBasedTx globals ledgerEnv mempoolState _wti tx =
    SL.applyTx
      globals
      ledgerEnv
      mempoolState
      tx

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (ShelleyEra c) where
  shelleyBasedEraName _ = "Shelley"

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (AllegraEra c) where
  shelleyBasedEraName _ = "Allegra"

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (MaryEra c) where
  shelleyBasedEraName _ = "Mary"

  applyShelleyBasedTx = defaultApplyShelleyBasedTx

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (AlonzoEra c) where
  shelleyBasedEraName _ = "Alonzo"

  applyShelleyBasedTx = applyAlonzoBasedTx

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody))
  => ShelleyBasedEra (BabbageEra c) where
  shelleyBasedEraName _ = "Babbage"
  applyShelleyBasedTx = applyAlonzoBasedTx

applyAlonzoBasedTx :: forall era.
  ( ShelleyBasedEra era,
    SupportsTwoPhaseValidation era,
    Core.Tx era ~ Alonzo.AlonzoTx era
  ) =>
  Globals ->
  SL.LedgerEnv era ->
  SL.LedgerState era ->
  WhetherToIntervene ->
  Alonzo.AlonzoTx era ->
  Except
    (SL.ApplyTxError era)
    ( SL.LedgerState era,
      SL.Validated (Alonzo.AlonzoTx era)
    )
applyAlonzoBasedTx globals ledgerEnv mempoolState wti tx = do
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
          | flag:flags <- nubOrd [b | Just b <- incorrectClaimedFlag (Proxy @era) <$> errs] ->
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
              tx{Alonzo.isValid = Alonzo.IsValid (not flag)}
        _ -> throwError e
               -- reject the transaction, protecting the local wallet
-- not exported
--
-- The ledger failure we see when the transaction's claimed 'IsValid'
-- flag was incorrect

class SupportsTwoPhaseValidation era where
  incorrectClaimedFlag :: proxy era -> SL.PredicateFailure (Core.EraRule "LEDGER" era) -> Maybe Bool

instance SupportsTwoPhaseValidation (AlonzoEra c) where
  incorrectClaimedFlag _ pf = case pf of
    SL.UtxowFailure
      ( Alonzo.ShelleyInAlonzoUtxowPredFailure
          ( SL.UtxoFailure
              ( Alonzo.UtxosFailure
                  ( Alonzo.ValidationTagMismatch
                      (Alonzo.IsValid claimedFlag)
                      _validationErrs
                    )
                )
            )
        ) ->
        Just claimedFlag
    _ -> Nothing

instance SupportsTwoPhaseValidation (BabbageEra c) where
  incorrectClaimedFlag _ pf = case pf of
    SL.UtxowFailure
      ( Babbage.AlonzoInBabbageUtxowPredFailure
          ( Alonzo.ShelleyInAlonzoUtxowPredFailure
              ( SL.UtxoFailure
                  ( Babbage.AlonzoInBabbageUtxoPredFailure
                      ( Alonzo.UtxosFailure
                          ( Alonzo.ValidationTagMismatch
                              (Alonzo.IsValid claimedFlag)
                              _validationErrs
                            )
                        )
                    )
                )
            )
        ) -> Just claimedFlag
    _ -> Nothing


-- | The ledger responded with Alonzo errors we were not expecting
data UnexpectedAlonzoLedgerErrors =
    -- | We received more than one 'Alonzo.ValidationTagMismatch'
    --
    -- The exception lists the 'Alonzo.IsValid' flags we saw.
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
  type TranslationError (AllegraEra c) WrapTx = Core.TranslationError (AllegraEra c) SL.ShelleyTx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance ShelleyBasedEra (MaryEra c) => Core.TranslateEra (MaryEra c) WrapTx where
  type TranslationError (MaryEra c) WrapTx = Core.TranslationError (MaryEra c) SL.ShelleyTx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance ShelleyBasedEra (AlonzoEra c) => Core.TranslateEra (AlonzoEra c) WrapTx where
  type TranslationError (AlonzoEra c) WrapTx = Core.TranslationError (AlonzoEra c) Alonzo.Tx
  translateEra ctxt =
        fmap (WrapTx . Alonzo.unTx)
      . Core.translateEra @(AlonzoEra c) ctxt
      . Alonzo.Tx . unwrapTx

instance ShelleyBasedEra (BabbageEra c) => Core.TranslateEra (BabbageEra c) WrapTx where
  type TranslationError (BabbageEra c) WrapTx = Core.TranslationError (BabbageEra c) Babbage.Tx
  translateEra ctxt =
        fmap (WrapTx . Babbage.unTx)
      . Core.translateEra @(BabbageEra c) ctxt
      . Babbage.Tx . unwrapTx
