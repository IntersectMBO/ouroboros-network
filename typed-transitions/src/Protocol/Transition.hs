{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Protocol.Transition
  ( SomeTransition (..)
  , withSomeTransition
  ) where

data SomeTransition tr where
  SomeTransition :: tr from to -> SomeTransition tr

withSomeTransition :: (forall from to . tr from to -> t) -> SomeTransition tr -> t
withSomeTransition k (SomeTransition it) = k it
