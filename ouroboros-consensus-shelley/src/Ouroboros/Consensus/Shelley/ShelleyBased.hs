{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Ouroboros.Consensus.Shelley.ShelleyBased (
    -- * Injection from Shelley-based eras into the Cardano eras
    InjectShelley
  , injectShelleyNP
  , injectShelleyOptNP
    -- * Transform Shelley-based types
  , EraCrypto
  , HasCrypto
  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Util.OptNP (OptNP (..))

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

{-------------------------------------------------------------------------------
  Injection from Shelley-based eras into consensus mode eras
-------------------------------------------------------------------------------}

-- | Witness the relation between consensus mode (e.g. Cardano) eras and the Shelley-based eras.
class    consensusModeEra ~ ShelleyBlock shelleyEra => InjectShelley shelleyEra consensusModeEra
instance consensusModeEra ~ ShelleyBlock shelleyEra => InjectShelley shelleyEra consensusModeEra

injectShelleyNP ::
     AllZip InjectShelley shelleyEras consensusModeEras
  => (   forall shelleyEra consensusModeEra.
         InjectShelley shelleyEra consensusModeEra
      => f shelleyEra -> g consensusModeEra
     )
  -> NP f shelleyEras -> NP g consensusModeEras
injectShelleyNP _ Nil       = Nil
injectShelleyNP f (x :* xs) = f x :* injectShelleyNP f xs

injectShelleyOptNP ::
     AllZip InjectShelley shelleyEras consensusModeEras
  => (   forall shelleyEra consensusModeEra.
         InjectShelley shelleyEra consensusModeEra
      => f shelleyEra -> g consensusModeEra
     )
  -> OptNP empty f shelleyEras -> OptNP empty g consensusModeEras
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
