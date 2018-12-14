{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Protocol.Path where

import Data.Kind (Type)

-- | A rather general type which controls applications of some `cons`
-- type constructor by way of a list of states.
--
-- 'TransitionPath' is a simple first example. It gives some sequence of
-- transitions through the states listed in the type.
data Path (cons :: Type -> [st] -> Type) (states :: [st]) where
  PathCons :: cons (Path cons (b ': rest)) (a ': b ': rest)
           -> Path cons (a ': b ': rest)
  PathNil  :: Path cons '[a]

data Transition (tr :: st -> st -> Type) (next :: Type) (states :: [st]) where
  Transition :: tr a b -> next -> Transition tr next (a ': b ': rest)

type TransitionPath tr = Path (Transition tr)

data From (begin :: st) (path :: [st] -> Type) where
  From :: path (begin ': rest) -> From begin path

foldPath
  :: forall cons states b .
     (forall k states . (k -> b) -> cons k states -> b)
  -> b
  -> Path cons states
  -> b
foldPath comb b path = case path of
  PathNil -> b
  PathCons cons -> comb (foldPath comb b) cons
