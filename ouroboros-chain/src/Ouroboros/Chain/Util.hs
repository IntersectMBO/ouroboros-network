{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
module Ouroboros.Chain.Util (
    Some (..)
  ) where

import           Data.Kind (Type)

data Some (f :: k -> Type) where
    Some :: f a -> Some f
