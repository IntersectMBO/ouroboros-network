{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Ouroboros.Consensus.Example.ShelleyBased (
    -- * Injection from Shelley-based eras into the Example eras
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

import           Ouroboros.Consensus.Example.Block

{-------------------------------------------------------------------------------
  Injection from Shelley-based eras into the Example eras
-------------------------------------------------------------------------------}

-- | Witness the relation between the Example eras and the Shelley-based eras.
class    exampleEra ~ ShelleyBlock shelleyEra => InjectShelley shelleyEra exampleEra
instance exampleEra ~ ShelleyBlock shelleyEra => InjectShelley shelleyEra exampleEra

injectShelleyNP ::
     AllZip InjectShelley shelleyEras exampleEras
  => (   forall shelleyEra exampleEra.
         InjectShelley shelleyEra exampleEra
      => f shelleyEra -> g exampleEra
     )
  -> NP f shelleyEras -> NP g exampleEras
injectShelleyNP _ Nil       = Nil
injectShelleyNP f (x :* xs) = f x :* injectShelleyNP f xs

injectShelleyOptNP ::
     AllZip InjectShelley shelleyEras exampleEras
  => (   forall shelleyEra exampleEra.
         InjectShelley shelleyEra exampleEra
      => f shelleyEra -> g exampleEra
     )
  -> OptNP empty f shelleyEras -> OptNP empty g exampleEras
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
  -> LedgerState (ExampleBlock c)
  -> LedgerState (ExampleBlock c)
overShelleyBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (LedgerState -.-> LedgerState)
             (ExampleEras c)
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
