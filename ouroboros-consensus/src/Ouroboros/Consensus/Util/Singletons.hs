{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

-- | Singletons
--
-- This provides the core of the 'singletons' package, using the same name,
-- but without pulling in all the dependencies and craziness.
module Ouroboros.Consensus.Util.Singletons (
    Sing (..)
  , SingI (..)
  , SingKind (..)
  , SomeSing (..)
  , withSomeSing
  ) where

import           Data.Kind (Type)

-- | Data family of singletons
data family Sing (a :: k)

-- | Singletons for lists
--
-- NOTE: Unlike the singletons library, we do /not/ require instances for the
-- elements of the list.
data instance Sing (xs :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing xs -> Sing (x ': xs)

class SingI (a :: k) where
  sing :: Sing a

instance             SingI '[]       where sing = SNil
instance SingI xs => SingI (x ': xs) where sing = SCons sing

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing :: forall k r. SingKind k
             => Demote k
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing d k = case toSing @k d :: SomeSing k of
                     SomeSing s -> k s

class SingKind k where
  type Demote k = (r :: Type) | r -> k

  fromSing :: Sing (a :: k) -> Demote k
  toSing :: Demote k -> SomeSing k
