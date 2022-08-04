{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.Cardano.ShelleyBased (overShelleyBasedLedgerState) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors

import           Ouroboros.Consensus.Cardano.Block
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible)

-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c mk wt.
     (TPraos.PraosCrypto c, Praos.PraosCrypto c)
  => (   forall era proto. (EraCrypto era ~ c, ShelleyCompatible proto era)
      => Flip2 LedgerState wt mk (ShelleyBlock proto era)
      -> Flip2 LedgerState wt mk (ShelleyBlock proto era)
     )
  -> LedgerState (CardanoBlock c) wt mk
  -> LedgerState (CardanoBlock c) wt mk
overShelleyBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (Flip2 LedgerState wt mk -.-> Flip2 LedgerState wt mk)
             (CardanoEras c)
    fs = fn id
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* injectSingleEra
        :* Nil

    injectSingleEra ::
      ( ShelleyCompatible proto era, EraCrypto era ~ c
      , shelleyEra ~ ShelleyBlock proto era
      )
      => (Flip2 LedgerState wt mk -.-> Flip2 LedgerState wt mk) shelleyEra
    injectSingleEra = fn f
