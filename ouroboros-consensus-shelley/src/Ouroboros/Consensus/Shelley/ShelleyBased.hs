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
  , InjectShelley2
  , injectShelleyNP
  , injectShelleyNP2
  , injectShelleyOptNP
    -- * Transform Shelley-based types
  , EraCrypto
  , HasCrypto
  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Util.OptNP (OptNP (..))

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import Ouroboros.Consensus.Util.NP2

{-------------------------------------------------------------------------------
  Injection from Shelley-based eras into consensus mode eras
-------------------------------------------------------------------------------}

-- TODO Rename this
-- | Witness the relation between consensus mode (e.g. Cardano) eras and the Shelley-based eras.
class    (consensusModeEra ~ ShelleyBlock (Fst protoAndEra) (Snd protoAndEra))
  => InjectShelley2 protoAndEra consensusModeEra
instance (consensusModeEra ~ ShelleyBlock (Fst protoAndEra) (Snd protoAndEra))
  => InjectShelley2 protoAndEra consensusModeEra

injectShelleyNP2 ::
     AllZip InjectShelley2 shelleyEras consensusModeEras
  => (   forall era proto protoAndEra consensusModeEra.
         (InjectShelley2 protoAndEra consensusModeEra
         , protoAndEra ~ '(proto, era)
         )
      => f proto era -> g consensusModeEra
     )
  -> NP2 f shelleyEras -> NP g consensusModeEras
injectShelleyNP2 _ Nil2         = Nil
injectShelleyNP2 f (Cons2 x xs) = f x :* injectShelleyNP2 f xs

-- injectShelleyOptNP ::
--      AllZip InjectShelley shelleyEras consensusModeEras
--   => (   forall era proto protoAndEra consensusModeEra.
--          ( InjectShelley protoAndEra consensusModeEra
--          , protoAndEra ~ '(proto, era)
--          )
--       => f (Fst protoAndEra) (Snd protoAndEra) -> g consensusModeEra
--      )
--   -> OptNP2 empty f shelleyEras -> OptNP empty g consensusModeEras
-- injectShelleyOptNP _ OptNil2         = OptNil
-- injectShelleyOptNP f (OptSkip2   xs) = OptSkip (injectShelleyOptNP f xs)
-- injectShelleyOptNP f (OptCons2 x xs) = OptCons (f x) (injectShelleyOptNP f xs)

class    consensusModeEra ~ ShelleyBlock proto shelleyEra => InjectShelley proto shelleyEra consensusModeEra
instance consensusModeEra ~ ShelleyBlock proto shelleyEra => InjectShelley proto shelleyEra consensusModeEra

injectShelleyNP ::
     AllZip (InjectShelley proto) shelleyEras consensusModeEras
  => (   forall shelleyEra consensusModeEra.
         InjectShelley proto shelleyEra consensusModeEra
      => f shelleyEra -> g consensusModeEra
     )
  -> NP f shelleyEras -> NP g consensusModeEras
injectShelleyNP _ Nil       = Nil
injectShelleyNP f (x :* xs) = f x :* injectShelleyNP f xs

injectShelleyOptNP ::
     AllZip (InjectShelley proto) shelleyEras consensusModeEras
  => (   forall shelleyEra consensusModeEra.
         InjectShelley proto shelleyEra consensusModeEra
      => f shelleyEra -> g consensusModeEra
     )
  -> OptNP empty f shelleyEras -> OptNP empty g consensusModeEras
injectShelleyOptNP _ OptNil         = OptNil
injectShelleyOptNP f (OptSkip   xs) = OptSkip (injectShelleyOptNP f xs)
injectShelleyOptNP f (OptCons x xs) = OptCons (f x) (injectShelleyOptNP f xs)

{-------------------------------------------------------------------------------
  Transform Shelley-based types
-------------------------------------------------------------------------------}

-- | Witness the relation between the crypto used by a Shelley-based era and
-- protocol.
--
-- Can be partially applied while an equality constraint cannot.
class (EraCrypto era ~ c, ProtoCrypto proto ~ c) => HasCrypto c proto era
instance (EraCrypto era ~ c, ProtoCrypto proto ~ c) => HasCrypto c proto era
