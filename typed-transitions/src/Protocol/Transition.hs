{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

module Protocol.Transition
  ( SomeTransition (..)
  , withSomeTransition
  , TransitionFrom (..)
  , castTransition
  , showTransitionFrom
  ) where

-- Definitions in this module are useful for working with unityped channels
-- to transport transitions between threads or processes.
-- 'SomeTransition' is a suitable unitype for encoding/decoding, and
-- 'TransitionFrom tr st' is the image of a type-safe cast from
-- 'SomeTransition tr' given a state 'st'.

import Data.Kind (Type)
import Data.Typeable

data SomeTransition tr where
  -- | Must ensure Typeable here, because we can't do it at 'castTransition'.
  -- That would require stating that every type in the state kind is
  -- typeable. Quantified constraints could help, although a short attempt at
  -- that did not work.
  SomeTransition :: ( Typeable from ) => tr from to -> SomeTransition tr

withSomeTransition :: (forall from to . tr from to -> t) -> SomeTransition tr -> t
withSomeTransition k (SomeTransition it) = k it

data TransitionFrom tr from where
  Expected   :: tr from to -> TransitionFrom tr from
  Unexpected :: TransitionFrom tr from

showTransitionFrom :: TransitionFrom tr from -> String
showTransitionFrom (Expected _) = "Expected"
showTransitionFrom (Unexpected) = "Unexpected"

castTransition
  :: forall st (tr :: st -> st -> Type) (from :: st) .
     ( Typeable from )
  => Proxy from
  -> SomeTransition tr
  -> TransitionFrom tr from
castTransition _ (SomeTransition (it :: tr from' to)) = case eqT of
  Just (Refl :: from :~: from') -> Expected it
  Nothing -> Unexpected
