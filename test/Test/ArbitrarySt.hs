{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module Test.ArbitrarySt where

import Control.Monad.State

import Test.GlobalState
import Test.QuickCheck

import Ouroboros

class ArbitrarySt p a | a -> p where
    arbitrarySt :: GenSt p a

    shrinkSt :: a -> [a]
    shrinkSt _ = []


data WithSt p a = WithSt (GlobalState p) a

withBft :: prop -> WithSt 'OuroborosBFT prop
withBft prop = WithSt initialBftState prop

instance (ArbitrarySt p a, Show a, Testable prop)
    => Testable (WithSt p (a -> prop)) where
    property (WithSt s f) = property $ do
      a <- evalStateT arbitrarySt s
      return $ f a

