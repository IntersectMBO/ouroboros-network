{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros (
    -- * Typed used across all protocols
    NodeId(..)
    -- * Generalize over the Ouroboros protocols
  , OuroborosProtocol(..)
  , Sing(..)
  , KnownOuroborosProtocol
  , dictKnownOuroborosProtocol
  ) where

import           Data.Hashable
import           GHC.Generics
import           Test.QuickCheck

import           Util.Singletons

{-------------------------------------------------------------------------------
  Types used across all protocols
-------------------------------------------------------------------------------}

data NodeId = CoreId Int
            | RelayId Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable NodeId -- let generic instance do the job

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
