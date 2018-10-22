{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module Test.ArbitrarySt where

import Control.Monad.State

import Test.GlobalState
import Test.QuickCheck
import Test.QuickCheck.Property (Property(..))

import Ouroboros

class ArbitrarySt p a | a -> p where
    arbitrarySt :: GenSt p a

    shrinkSt :: a -> [a]
    shrinkSt _ = []


data WithSt p a = WithSt (GlobalState p) a

withBft :: prop -> WithSt 'OuroborosBFT prop
withBft prop = WithSt initialBftState prop

instance (  ArbitrarySt p (a p), Show (a p), Testable prop)
    => Testable (WithSt p ((a p) -> prop)) where
    property (WithSt s f) = property $ do
      a <- evalStateT arbitrarySt s
      return $ f a

instance Testable (WithSt p Property) where
    property (WithSt s p) = property $ do
      a <- evalStateT (lift . unProperty $ p) s
      return (MkProperty (return a))

