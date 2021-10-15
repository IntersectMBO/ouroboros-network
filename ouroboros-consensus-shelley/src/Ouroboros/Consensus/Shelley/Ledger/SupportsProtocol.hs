{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains 'SupportsProtocol' instances tying the ledger and
-- protocol together. Since these instances reference both ledger concerns and
-- protocol concerns, it is the one class where we cannot provide a generic
-- instance for 'ShelleyBlock'.
module Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol () where

import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.API as SL
import           Control.Monad.Except (MonadError (throwError))
import           Data.Coerce (coerce)
import           GHC.Records (getField)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.History.Util
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol (..))
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Praos
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Protocol.Translate (TranslateProto,
                     translateTickedLedgerView)
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import           Ouroboros.Consensus.Shelley.Protocol.Abstract ()
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Shelley.Protocol.TPraos ()

instance
  (ShelleyCompatible (TPraos crypto) era, crypto ~ EraCrypto era) =>
  LedgerSupportsProtocol (ShelleyBlock (TPraos crypto) era)
  where
  protocolLedgerView _cfg =
    TPraos.TickedPraosLedgerView
      . SL.currentLedgerView
      . tickedShelleyLedgerState

  ledgerViewForecastAt cfg ledgerState = Forecast at $ \for ->
    if
        | NotOrigin for == at ->
          return $ TPraos.TickedPraosLedgerView $ SL.currentLedgerView shelleyLedgerState
        | for < maxFor ->
          return $ futureLedgerView for
        | otherwise ->
          throwError $
            OutsideForecastRange
              { outsideForecastAt = at,
                outsideForecastMaxFor = maxFor,
                outsideForecastFor = for
              }
    where
      ShelleyLedgerState {shelleyLedgerState} = ledgerState
      globals = shelleyLedgerGlobals cfg
      swindow = SL.stabilityWindow globals
      at = ledgerTipSlot ledgerState

      futureLedgerView :: SlotNo -> Ticked (SL.LedgerView (EraCrypto era))
      futureLedgerView =
        either
          (\e -> error ("futureLedgerView failed: " <> show e))
          TPraos.TickedPraosLedgerView
          . SL.futureLedgerView globals shelleyLedgerState

      -- Exclusive upper bound
      maxFor :: SlotNo
      maxFor = addSlots swindow $ succWithOrigin at

instance
  ( ShelleyCompatible (Praos crypto) era,
    ShelleyCompatible (TPraos crypto) era,
    crypto ~ EraCrypto era,
    TranslateProto (TPraos crypto) (Praos crypto)
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (Praos crypto) era)
  where
  protocolLedgerView _cfg st =
    let SL.NewEpochState {nesPd, nesEs} = tickedShelleyLedgerState st
     in Praos.TickedPraosLedgerView $
          Praos.LedgerView
            { Praos.lvPoolDistr = nesPd,
              Praos.lvMaxBodySize = getField @"_maxBBSize" . SL.esPp $ nesEs,
              Praos.lvMaxHeaderSize = getField @"_maxBHSize" . SL.esPp $ nesEs,
              Praos.lvProtocolVersion = getField @"_protocolVersion" . SL.esPp $ nesEs
            }

  -- | Currently the Shelley+ ledger is hard-coded to produce a TPraos ledger
  -- view. Since we can convert them, we piggy-back on this to get a Praos
  -- ledger view. Ultimately, we will want to liberalise the ledger code
  -- slightly.
  ledgerViewForecastAt cfg st =
    mapForecast (translateTickedLedgerView @(TPraos crypto) @(Praos crypto)) $
      ledgerViewForecastAt @(ShelleyBlock (TPraos crypto) era) cfg st'
    where
      st' :: LedgerState (ShelleyBlock (TPraos crypto) era) EmptyMK
      st' =
        ShelleyLedgerState
          { shelleyLedgerTip = coerceTip <$> shelleyLedgerTip st,
            shelleyLedgerState = shelleyLedgerState st,
            shelleyLedgerTransition = shelleyLedgerTransition st,
            shelleyLedgerTables = polyEmptyLedgerTables
          }
      coerceTip (ShelleyTip slot block hash) = ShelleyTip slot block (coerce hash)
