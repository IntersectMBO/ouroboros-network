{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Ouroboros.Consensus.Cardano.ShelleyBased (
    -- * Injection from Shelley-based eras into the Cardano eras
    InjectShelley
  , injectShelleyNP
  , injectShelleyOptNP
    -- * Transform Shelley-based types
  , HasCrypto
  , overShelleyBasedLedgerState
  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Util.OptNP (OptNP (..))

import           Ouroboros.Consensus.HardFork.Combinator

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol (PraosCrypto)

import           Ouroboros.Consensus.Cardano.Block

{-------------------------------------------------------------------------------
  Injection from Shelley-based eras into the Cardano eras
-------------------------------------------------------------------------------}

-- | Witness the relation between the Cardano eras and the Shelley-based eras.
class    cardanoEra ~ ShelleyBlock shelleyEra => InjectShelley shelleyEra cardanoEra
instance cardanoEra ~ ShelleyBlock shelleyEra => InjectShelley shelleyEra cardanoEra

injectShelleyNP ::
     AllZip InjectShelley shelleyEras cardanoEras
  => (   forall shelleyEra cardanoEra.
         InjectShelley shelleyEra cardanoEra
      => f shelleyEra -> g cardanoEra
     )
  -> NP f shelleyEras -> NP g cardanoEras
injectShelleyNP _ Nil       = Nil
injectShelleyNP f (x :* xs) = f x :* injectShelleyNP f xs

injectShelleyOptNP ::
     AllZip InjectShelley shelleyEras cardanoEras
  => (   forall shelleyEra cardanoEra.
         InjectShelley shelleyEra cardanoEra
      => f shelleyEra -> g cardanoEra
     )
  -> OptNP empty f shelleyEras -> OptNP empty g cardanoEras
injectShelleyOptNP _ OptNil         = OptNil
injectShelleyOptNP f (OptSkip   xs) = OptSkip (injectShelleyOptNP f xs)
injectShelleyOptNP f (OptCons x xs) = OptCons (f x) (injectShelleyOptNP f xs)

{-------------------------------------------------------------------------------
  Transform Shelley-based types
-------------------------------------------------------------------------------}

-- | Witness the relation between the crypto used by a Shelley-based era.
--
-- Can be partially applied while an equality constraint cannot.
class EraCrypto era ~ c => HasCrypto c era
instance EraCrypto era ~ c => HasCrypto c era

-- | When the given ledger state corresponds to a Shelley-based era, apply the
-- given function to it.
overShelleyBasedLedgerState ::
     forall c. PraosCrypto c
  => (   forall era. (EraCrypto era ~ c, ShelleyBasedEra era)
      => LedgerState (ShelleyBlock era)
      -> LedgerState (ShelleyBlock era)
     )
  -> LedgerState (CardanoBlock c)
  -> LedgerState (CardanoBlock c)
overShelleyBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (LedgerState -.-> LedgerState)
             (CardanoEras c)
    fs = fn id
        :* injectShelleyNP
             reassoc
             (hcpure
               (Proxy @(And (HasCrypto c) ShelleyBasedEra))
               (fn (Comp . f . unComp)))

    reassoc ::
         (     LedgerState :.: ShelleyBlock
          -.-> LedgerState :.: ShelleyBlock
         ) shelleyEra
      -> (     LedgerState
          -.-> LedgerState
         ) (ShelleyBlock shelleyEra)
    reassoc g = fn $ unComp . apFn g . Comp
