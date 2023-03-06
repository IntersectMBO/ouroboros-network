{-# LANGUAGE DeriveGeneric #-}

module Data.SOP.Functors (Product2 (..)) where

import           GHC.Generics (Generic)

data Product2 f g x y = Pair2 (f x y) (g x y)
  deriving (Eq, Generic, Show)
