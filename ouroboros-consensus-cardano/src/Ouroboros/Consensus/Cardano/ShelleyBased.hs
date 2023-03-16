{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Cardano.ShelleyBased (overShelleyBasedLedgerState) where

import           Data.SOP.Strict hiding (All2)
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible)
import Ouroboros.Consensus.Protocol.Praos (PraosCrypto)

-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c c'.
     (PraosCrypto c, PraosCrypto c')
  => (   forall era proto. (ShelleyCompatible proto era)
      => LedgerState (ShelleyBlock proto era)
      -> LedgerState (ShelleyBlock proto era)
     )
  -> LedgerState (CardanoBlock c c')
  -> LedgerState (CardanoBlock c c')
overShelleyBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (LedgerState -.-> LedgerState)
             (CardanoEras c c')
    fs = fn id
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEraCurrent
        :* Nil

    injectSingleEra ::
         (ShelleyCompatible proto era)
      => (LedgerState -.-> LedgerState) (ShelleyBlock proto era)
    injectSingleEra = fn f

    injectSingleEraCurrent ::
      ( ShelleyCompatible proto era, EraCrypto era ~ c'
      , shelleyEra ~ ShelleyBlock proto era
      )
      => (LedgerState -.-> LedgerState) shelleyEra
    injectSingleEraCurrent = fn f
