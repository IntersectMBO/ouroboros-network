{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Ouroboros.Consensus.Cardano.ShelleyBased (overShelleyBasedLedgerState) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors (Flip (..))

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Protocol.TPraos (PraosCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.ShelleyBased

-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c mk. PraosCrypto c
  => (   forall era. (EraCrypto era ~ c, ShelleyBasedEra era)
      => LedgerState (ShelleyBlock era) mk
      -> LedgerState (ShelleyBlock era) mk
     )
  -> LedgerState (CardanoBlock c) mk
  -> LedgerState (CardanoBlock c) mk
overShelleyBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (Flip LedgerState mk -.-> Flip LedgerState mk)
             (CardanoEras c)
    fs = fn id
        :* injectShelleyNP
             reassoc
             (hcpure
               (Proxy @(And (HasCrypto c) ShelleyBasedEra))
               (fn (Comp . Flip . f . unFlip . unComp)))

    reassoc ::
         (     Flip LedgerState mk :.: ShelleyBlock
          -.-> Flip LedgerState mk :.: ShelleyBlock
         ) shelleyEra
      -> (     Flip LedgerState mk
          -.-> Flip LedgerState mk
         ) (ShelleyBlock shelleyEra)
    reassoc g = fn $ unComp . apFn g . Comp
