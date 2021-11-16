{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

module Ouroboros.Consensus.HardFork.Combinator.Util.Functors (
    Flip (..)
  , Product2 (..)
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

data Product2 f g x y = Pair2 (f x y) (g x y)
  deriving (Eq, Generic, Show)

newtype Flip f x y = Flip {unFlip :: f y x}
  deriving (Eq, Generic, NoThunks, Show)
