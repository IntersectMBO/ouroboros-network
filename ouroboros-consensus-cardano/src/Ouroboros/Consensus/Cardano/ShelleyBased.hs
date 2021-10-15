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
import           Data.SOP.Strict

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Flip (..))

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible)

-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c mk.
     (TPraos.PraosCrypto c, Praos.PraosCrypto c)
  => (   forall era proto. (EraCrypto era ~ c, ShelleyCompatible proto era)
      => LedgerState (ShelleyBlock proto era) mk
      -> LedgerState (ShelleyBlock proto era) mk
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
