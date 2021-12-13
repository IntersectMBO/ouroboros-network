{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

import           Ouroboros.Consensus.Util.NP2
import           Ouroboros.Consensus.Util.OptNP (OptNP (..))

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)

{-------------------------------------------------------------------------------
  Injection from Shelley-based eras into consensus mode eras
-------------------------------------------------------------------------------}

-- | Witness the relation between consensus mode (e.g. Cardano) eras and the Shelley-based eras.
class    (consensusModeEra ~ ShelleyBlock (Fst protoAndEra) (Snd protoAndEra))
  => InjectShelley protoAndEra consensusModeEra
instance (consensusModeEra ~ ShelleyBlock (Fst protoAndEra) (Snd protoAndEra))
  => InjectShelley protoAndEra consensusModeEra

injectShelleyNP ::
     AllZip InjectShelley shelleyEras consensusModeEras
  => (   forall era proto protoAndEra consensusModeEra.
         (InjectShelley protoAndEra consensusModeEra
         , protoAndEra ~ '(proto, era)
         )
      => f proto era -> g consensusModeEra
     )
  -> NP2 f shelleyEras -> NP g consensusModeEras
injectShelleyNP _ Nil2         = Nil
injectShelleyNP f (Cons2 x xs) = f x :* injectShelleyNP f xs

injectShelleyOptNP ::
     AllZip InjectShelley shelleyEras consensusModeEras
  => (   forall era proto protoAndEra consensusModeEra.
         ( InjectShelley protoAndEra consensusModeEra
         , protoAndEra ~ '(proto, era)
         )
      => f (Fst protoAndEra) (Snd protoAndEra) -> g consensusModeEra
     )
  -> OptNP2 empty f shelleyEras -> OptNP empty g consensusModeEras
injectShelleyOptNP _ OptNil2         = OptNil
injectShelleyOptNP f (OptSkip2   xs) = OptSkip (injectShelleyOptNP f xs)
injectShelleyOptNP f (OptCons2 x xs) = OptCons (f x) (injectShelleyOptNP f xs)

{-------------------------------------------------------------------------------
  Transform Shelley-based types
-------------------------------------------------------------------------------}

-- | Witness the relation between the crypto used by a Shelley-based era.
--
-- Can be partially applied while an equality constraint cannot.
class (EraCrypto era ~ c, ProtoCrypto proto ~ c) => HasCrypto c proto era
instance (EraCrypto era ~ c, ProtoCrypto proto ~ c) => HasCrypto c proto era
