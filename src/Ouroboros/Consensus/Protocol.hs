{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Ouroboros.Consensus.Protocol (
    -- * Generalize over the Ouroboros protocols
    OuroborosProtocol(..)
  , Sing(..)
  , KnownOuroborosProtocol
  , dictKnownOuroborosProtocol
  ) where

import           Test.QuickCheck

import           Ouroboros.Consensus.Infra.Singletons

{-------------------------------------------------------------------------------
  Ouroboros protocol and its lifted version
-------------------------------------------------------------------------------}

data OuroborosProtocol =
    OuroborosBFT
  | OuroborosPraos

instance Arbitrary OuroborosProtocol where
  arbitrary = elements [OuroborosBFT] -- only BFT implemented right now

data instance Sing (p :: OuroborosProtocol) where
  SingBFT   :: Sing 'OuroborosBFT
  SingPraos :: Sing 'OuroborosPraos

instance SingI 'OuroborosBFT   where sing = SingBFT
instance SingI 'OuroborosPraos where sing = SingPraos

instance SingKind OuroborosProtocol where
  type Demote OuroborosProtocol = OuroborosProtocol

  fromSing SingBFT   = OuroborosBFT
  fromSing SingPraos = OuroborosPraos

  toSing OuroborosBFT   = SomeSing SingBFT
  toSing OuroborosPraos = SomeSing SingPraos

{-------------------------------------------------------------------------------
  Generalize over the various Ouroboros protocols
-------------------------------------------------------------------------------}

class KnownOuroborosProtocol (p :: OuroborosProtocol) where

dictKnownOuroborosProtocol :: Sing p -> Dict (KnownOuroborosProtocol p)
dictKnownOuroborosProtocol SingBFT   = Dict
dictKnownOuroborosProtocol SingPraos = Dict

{-------------------------------------------------------------------------------
  BFT
-------------------------------------------------------------------------------}

instance KnownOuroborosProtocol 'OuroborosBFT where

{-------------------------------------------------------------------------------
  Praos
-------------------------------------------------------------------------------}

instance KnownOuroborosProtocol 'OuroborosPraos where
